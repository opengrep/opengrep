import copy
from collections import defaultdict
from functools import lru_cache
from pathlib import Path
from typing import Callable
from typing import Dict
from typing import Iterator
from typing import List
from typing import Tuple

from attr import evolve

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.external.packaging.specifiers import InvalidSpecifier  # type: ignore
from semdep.external.packaging.specifiers import SpecifierSet  # type: ignore
from semdep.package_restrictions import dependencies_range_match_any
from semgrep.error import SemgrepError
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyMatch
from semgrep.semgrep_interfaces.semgrep_output_v1 import Direct
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.subproject import find_closest_subproject
from semgrep.subproject import ResolvedSubproject
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


SCA_FINDING_SCHEMA = 20220913


def parse_depends_on_yaml(entries: List[Dict[str, str]]) -> Iterator[out.ScaPattern]:
    """
    Convert the entries in the Yaml to ProjectDependsOnEntry objects that specify
    namespace, package name, and semver ranges
    """
    for entry in entries:
        # schema checks should guarantee we have these fields, but we'll code defensively
        namespace = entry.get("namespace")
        if namespace is None:
            raise SemgrepError(f"project-depends-on is missing `namespace`")
        try:
            ecosystem = Ecosystem.from_json(namespace.lower())
        except ValueError:
            raise SemgrepError(f"unknown package ecosystem: {namespace}")
        package = entry.get("package")
        if package is None:
            raise SemgrepError(f"project-depends-on is missing `package`")
        semver_range = entry.get("version")
        if semver_range is None:
            raise SemgrepError(f"project-depends-on is missing `version`")
        try:
            SpecifierSet(semver_range)
        except InvalidSpecifier:
            raise SemgrepError(f"invalid semver range {semver_range}")

        # Pypi package names are case insensitive
        if ecosystem == Ecosystem(Pypi()):
            package = package.lower()

        yield out.ScaPattern(
            ecosystem=ecosystem, package=package, semver_range=semver_range
        )


def generate_unreachable_sca_findings(
    rule: Rule,
    already_reachable: Callable[[Path, FoundDependency], bool],
    resolved_deps: Dict[Ecosystem, List[ResolvedSubproject]],
) -> Tuple[List[RuleMatch], List[SemgrepError]]:
    """
    Returns matches to a only a rule's sca-depends-on patterns; ignoring any reachabiliy patterns it has
    """
    depends_on_keys = rule.project_depends_on
    dep_rule_errors: List[SemgrepError] = []

    depends_on_entries = list(parse_depends_on_yaml(depends_on_keys))
    ecosystems = list(rule.ecosystems)

    non_reachable_matches = []
    match_based_keys: Dict[tuple[str, Path, str], int] = defaultdict(int)
    for ecosystem in ecosystems:
        for subproject in resolved_deps.get(ecosystem, []):
            deps = list(subproject.found_dependencies.iter_found_dependencies())

            dependency_matches = list(
                dependencies_range_match_any(depends_on_entries, list(deps))
            )
            for dep_pat, found_dep in dependency_matches:
                if found_dep.lockfile_path is None:
                    # In rare cases, it's possible for a dependency to not have a lockfile
                    # path. This indicates a dev error and usually means that the parser
                    # did not associate the dep with a lockfile. So we'll just skip this dependency.
                    logger.warning(
                        f"Found a dependency ({found_dep.package}) without a lockfile path. Skipping..."
                    )
                    continue

                lockfile_path = Path(found_dep.lockfile_path.value)

                if already_reachable(lockfile_path, found_dep):
                    continue

                dep_match = DependencyMatch(
                    dependency_pattern=dep_pat,
                    found_dependency=found_dep,
                    lockfile=out.Fpath(str(lockfile_path)),
                )
                match = RuleMatch(
                    message=rule.message,
                    metadata=rule.metadata,
                    severity=rule.severity,
                    fix=None,
                    match=out.CoreMatch(
                        check_id=out.RuleId(rule.id),
                        path=out.Fpath(str(lockfile_path)),
                        start=out.Position(found_dep.line_number or 1, 1, 1),
                        end=out.Position(
                            (found_dep.line_number if found_dep.line_number else 1),
                            1,
                            1,
                        ),
                        # TODO: we need to define the fields below in
                        # Output_from_core.atd so we can reuse out.MatchExtra
                        extra=out.CoreMatchExtra(
                            metavars=out.Metavars({}),
                            engine_kind=out.EngineOfFinding(out.OSS()),
                            is_ignored=False,
                        ),
                    ),
                    extra={
                        "sca_info": out.ScaMatch(
                            sca_finding_schema=SCA_FINDING_SCHEMA,
                            reachable=False,
                            reachability_rule=rule.should_run_on_semgrep_core,
                            dependency_match=dep_match,
                        )
                    },
                )
                match = evolve(
                    match, match_based_index=match_based_keys[match.match_based_key]
                )
                match_based_keys[match.match_based_key] += 1
                non_reachable_matches.append(match)

    return non_reachable_matches, dep_rule_errors


@lru_cache(maxsize=100_000)
def transitive_dep_is_also_direct(
    package: str, deps: Tuple[Tuple[str, Transitivity], ...]
) -> bool:
    """
    Assumes that [dep] is transitive
    Checks if there is a direct version of the transitive dependency [dep]
    """
    return (package, Transitivity(Direct())) in deps


def generate_reachable_sca_findings(
    matches: List[RuleMatch],
    rule: Rule,
    resolved_deps: Dict[Ecosystem, List[ResolvedSubproject]],
) -> Tuple[
    List[RuleMatch], List[SemgrepError], Callable[[Path, FoundDependency], bool]
]:
    depends_on_keys = rule.project_depends_on
    dep_rule_errors: List[SemgrepError] = []

    depends_on_entries = list(parse_depends_on_yaml(depends_on_keys))
    ecosystems = list(rule.ecosystems)

    # Reachability rule
    reachable_matches = []
    reachable_deps = set()
    for ecosystem in ecosystems:
        for match in matches:
            try:
                subproject = find_closest_subproject(
                    match.path, ecosystem, resolved_deps.get(ecosystem, [])
                )

                if subproject is None:
                    continue

                deps = list(subproject.found_dependencies.iter_found_dependencies())
                frozen_deps = tuple((dep.package, dep.transitivity) for dep in deps)

                dependency_matches = list(
                    dependencies_range_match_any(depends_on_entries, deps)
                )
                for dep_pat, found_dep in dependency_matches:
                    if found_dep.lockfile_path is None:
                        # In rare cases, it's possible for a dependency to not have a lockfile
                        # path. This indicates a dev error and usually means that the parser
                        # did not associate the dep with a lockfile. So we'll just skip this dependency.
                        logger.warning(
                            f"Found a dependency ({found_dep.package}) without a lockfile path. Skipping..."
                        )
                        continue

                    lockfile_path = Path(found_dep.lockfile_path.value)

                    if found_dep.transitivity == Transitivity(
                        Transitive()
                    ) and transitive_dep_is_also_direct(found_dep.package, frozen_deps):
                        continue

                    reachable_deps.add(
                        (
                            lockfile_path,
                            found_dep.package,
                            found_dep.version,
                            found_dep.transitivity,
                        )
                    )
                    dep_match = DependencyMatch(
                        dependency_pattern=dep_pat,
                        found_dependency=found_dep,
                        lockfile=out.Fpath(str(lockfile_path)),
                    )
                    # ! deepcopy is necessary here since we might iterate over the
                    # ! same match for multiple dependencies
                    new_match = copy.deepcopy(match)
                    new_match.extra["sca_info"] = out.ScaMatch(
                        sca_finding_schema=SCA_FINDING_SCHEMA,
                        reachable=True,
                        reachability_rule=rule.should_run_on_semgrep_core,
                        dependency_match=dep_match,
                    )
                    reachable_matches.append(new_match)
            except SemgrepError as e:
                dep_rule_errors.append(e)

    return (
        reachable_matches,
        dep_rule_errors,
        (lambda p, d: (p, d.package, d.version, d.transitivity) in reachable_deps),
    )
