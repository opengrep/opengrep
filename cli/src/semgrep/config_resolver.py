import json
import os
import re
import time
from collections import OrderedDict
from enum import auto
from enum import Enum
from functools import lru_cache
from pathlib import Path
from tempfile import mkstemp
from typing import Any
from typing import Dict
from typing import List
from typing import Mapping
from typing import NamedTuple
from typing import Optional
from typing import Sequence
from typing import Tuple
from urllib.parse import urlencode
from urllib.parse import urlparse
from urllib.parse import urlsplit

import requests
from ruamel.yaml import YAMLError

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep import __VERSION__
from semgrep import tracing
from semgrep.constants import CLI_RULE_ID
from semgrep.constants import Colors
from semgrep.constants import DEFAULT_SEMGREP_APP_CONFIG_URL
from semgrep.constants import ID_KEY
from semgrep.constants import MISSED_KEY
from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.constants import RULES_KEY
from semgrep.error import INVALID_API_KEY_EXIT_CODE
from semgrep.error import InvalidRuleSchemaError
from semgrep.error import SemgrepError
from semgrep.error import UNPARSEABLE_YAML_EXIT_CODE
from semgrep.error_location import Span
from semgrep.rule import Rule
from semgrep.rule import rule_without_metadata
from semgrep.rule_lang import EmptySpan
from semgrep.rule_lang import EmptyYamlException
from semgrep.rule_lang import parse_config_preserve_spans
from semgrep.rule_lang import prepend_rule_path
from semgrep.rule_lang import validate_yaml
from semgrep.rule_lang import YamlMap
from semgrep.rule_lang import YamlTree
from semgrep.state import get_state
from semgrep.util import is_config_suffix
from semgrep.util import is_rules
from semgrep.util import is_url
from semgrep.util import with_color
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

AUTO_CONFIG_KEY = "auto"
AUTO_CONFIG_LOCATION = "c/auto"

DEFAULT_CONFIG = {
    "rules": [
        {
            "id": "eqeq-is-bad",
            "pattern": "$X == $X",
            "message": "$X == $X is a useless equality check",
            "languages": ["python"],
            "severity": out.Error().to_json(),
        },
    ],
}

CLOUD_PLATFORM_CONFIG_ID = "semgrep-app-rules"
REGISTRY_CONFIG_ID = "remote-registry"
NON_REGISTRY_REMOTE_CONFIG_ID = "remote-url"


class ConfigFile(NamedTuple):
    config_id: Optional[str]  # None for remote files
    contents: str
    config_path: str


class ConfigType(Enum):
    REGISTRY = auto()
    LOCAL = auto()


class ConfigLoader:
    _origin = ConfigType.LOCAL
    _config_path = ""
    _project_url = None

    def __init__(
        self,
        config_str: str,
        project_url: Optional[str] = None,
    ) -> None:
        """
        Takes a user's inputted config_str and transforms it into the appropriate
        path, checking whether the config string is a registry url or not
        """
        state = get_state()
        self._project_url = project_url
        self._origin = ConfigType.REGISTRY
        if config_str == "r2c":
            self._config_path = "https://semgrep.dev/c/p/r2c"
        elif is_url(config_str):
            self._config_path = config_str
        elif is_registry_id(config_str):
            self._config_path = registry_id_to_url(config_str)
        elif config_str == AUTO_CONFIG_KEY:
            self._config_path = f"{state.env.semgrep_url}/{AUTO_CONFIG_LOCATION}"
        else:
            self._origin = ConfigType.LOCAL
            self._config_path = str(Path(config_str).expanduser())

    @classmethod
    def includes_remote_config(cls, configs: Sequence[str]) -> bool:
        """
        Returns True if any of the configs are remote
        """
        return any(
            config == AUTO_CONFIG_KEY
            or config == "r2c"
            or is_registry_id(config)
            or is_url(config)
            for config in configs
        )

    def load_config(self) -> List[ConfigFile]:
        """
        Loads a config based on self's state.
        A config path produces a list of ConfigFiles because
        it may be a path to a folders of configs, each of
        which produces a file
        """
        if self._origin == ConfigType.REGISTRY:
            return [self._download_config()]
        else:
            return self._load_config_from_local_path()

    def _nice_semgrep_url(self, url: str) -> str:
        """
        Alters semgrep.dev urls to let user
        click through to the nice display page instead
        of raw YAML.
        Replaces '/c/' in semgrep urls with '/'.
        """
        parsed = urlparse(url)
        if "semgrep.dev" in parsed.netloc and parsed.path.startswith("/c"):
            return url.replace("/c/", "/")
        return url

    def _download_config(self) -> ConfigFile:
        """
        Download a configuration from the registry
        """
        return self._download_config_from_url(self._config_path)

    def _download_config_from_url(self, url: str) -> ConfigFile:
        app_session = get_state().app_session
        logger.debug("Downloading config from %s", url)
        error = f"Failed to download configuration from {url}"
        try:
            resp = app_session.get(url, headers={"Accept": "application/json"})
            if resp.status_code == requests.codes.ok:
                try:
                    rule_config = resp.json()["rule_config"]

                    # The backend wants to return native json, but we support a json string here too
                    config_str = (
                        rule_config
                        if isinstance(rule_config, str)
                        else json.dumps(rule_config)
                    )

                    return ConfigFile(None, config_str, url)
                except Exception as ex:
                    # catch JSONDecodeError, AssertionError, etc. is this needed?
                    logger.debug("Failed to decode JSON: %s", repr(ex))
                    return ConfigFile(
                        None, resp.content.decode("utf-8", errors="replace"), url
                    )
                finally:
                    logger.debug(f"Downloaded config from %s", url)

            error += f" HTTP {resp.status_code}."
        except requests.exceptions.RetryError as ex:
            error += f" Failed after multiple attempts ({ex.args[0].reason})"

        logger.debug(error)  # since the raised exception may be caught and suppressed
        raise SemgrepError(error)

    def _load_config_from_local_path(self) -> List[ConfigFile]:
        """
        Return config file(s) as dictionary object
        """
        loc = Path(self._config_path)

        logger.debug(f"Loading local config from {loc}")
        if loc.exists():
            if loc.is_file():
                config = [read_config_at_path(loc)]
            elif loc.is_dir():
                config = read_config_folder(loc)
            else:
                raise SemgrepError(f"config location `{loc}` is not a file or folder!")
        else:
            env = get_state().env
            addendum = ""
            if env.in_docker:
                addendum = " (since you are running in docker, you cannot specify arbitrary paths on the host; they must be mounted into the container)"
            raise SemgrepError(
                f"WARNING: unable to find a config; path `{loc}` does not exist{addendum}"
            )
        logger.debug(f"Done loading local config from {loc}")
        return config

    def is_registry_url(self) -> bool:
        return self._origin == ConfigType.REGISTRY


def read_config_at_path(loc: Path, base_path: Optional[Path] = None) -> ConfigFile:
    """
    Assumes file at loc exists
    """
    config_id = str(loc)
    if base_path:
        config_id = str(loc).replace(str(base_path), "")

    return ConfigFile(config_id, loc.read_text(), str(loc))


def read_config_folder(loc: Path, relative: bool = False) -> List[ConfigFile]:
    configs = []
    for l in loc.rglob("*"):
        if is_config_suffix(l) and l.is_file():
            configs.append(read_config_at_path(l, loc if relative else None))
    return configs


@tracing.trace()
def parse_config_files(
    loaded_config_infos: List[ConfigFile],
    force_jsonschema: bool = False,
) -> Tuple[Dict[str, YamlTree], List[SemgrepError]]:
    """
    Parse a list of config files into rules
    This assumes that config_id is set for local rules
    but is None for registry rules
    """
    config = {}
    errors: List[SemgrepError] = []
    # NOTE: This was pretty slow for directories with many rules. Now for 220+ rules
    # a moderate repo scan took ~45s instead of ~55s, so a 20% improvement.
    # XXX: One test fails, because we don't capture the output from the executor
    # pool. But I manually tested that the expected error was printed, so it's a
    # test configuration issue.
    #
    # for config_id, contents, config_path in loaded_config_infos:
    from concurrent.futures import ThreadPoolExecutor
    @tracing.trace()
    def process_config_item(config_item):
        config_id, contents, config_path = config_item
        try:
            if not config_id:  # registry rules don't have config ids
                # Note: we must disambiguate registry sourced remote rules from
                # non-registry sourced ones for security purposes. Namely, we
                # want to avoid running postprocessors from untrusted remote
                # sources (unless a local flag disabiling the relevant check is
                # used).
                try:
                    remote_rule_netloc = urlsplit(config_path).netloc
                except ValueError:
                    remote_rule_netloc = "invalid-url"
                config_id = (
                    REGISTRY_CONFIG_ID
                    if is_url(config_path)
                    and (
                        remote_rule_netloc.endswith(".semgrep.dev")
                        or remote_rule_netloc == "semgrep.dev"
                    )
                    else NON_REGISTRY_REMOTE_CONFIG_ID
                )
                filename = f"{config_path[:20]}..."
            else:
                filename = config_path
            config_data, config_errors = parse_config_string(
                config_id, contents, filename, force_jsonschema=force_jsonschema
            )
            return 'Ok', config_data, config_errors

        except InvalidRuleSchemaError as e:
            if (
                config_id == REGISTRY_CONFIG_ID
                or config_id == NON_REGISTRY_REMOTE_CONFIG_ID
            ):
                notice = f"\nRules downloaded from {config_path} failed to parse.\nThis is likely because rules have been added that use functionality introduced in later versions of semgrep.\nPlease upgrade to latest version of semgrep (see https://semgrep.dev/docs/upgrading/) and try again.\n"
                notice_color = with_color(Colors.red, notice, bold=True)
                logger.error(notice_color)
                return 'Error', e
            else:
                return 'Error', e

    with ThreadPoolExecutor() as executor:
        results = list(executor.map(process_config_item, loaded_config_infos))

    # Merge results
    for res in results:
        if res[0] == 'Ok':
            config_data, config_errors = res[1:]
            config.update(config_data)
            errors.extend(config_errors)
        else:
            # Raise the first error:
            raise res[1]

    return config, errors


@tracing.trace()
def resolve_config(
    config_str: str,
    project_url: Optional[str] = None,
    force_jsonschema: bool = False,
) -> Tuple[Dict[str, YamlTree], List[SemgrepError]]:
    """resolves if config arg is a registry entry, a url, or a file, folder, or loads from defaults if None"""
    start_t = time.time()
    config_loader = ConfigLoader(config_str, project_url)
    config, errors = parse_config_files(
        config_loader.load_config(), force_jsonschema=force_jsonschema
    )
    if config:
        logger.debug(f"loaded {len(config)} configs in {time.time() - start_t}")
    return config, errors


class Config:
    def __init__(
        self,
        valid_configs: Mapping[str, Sequence[Rule]],
        *,
        # TODO: Use an array of semgrep_output_v1.Product instead of booleans flags for secrets, code, and supply chain
        with_code_rules: bool = False,
        with_supply_chain: bool = False,
        with_secrets: bool = False,
        missed_rule_count: int = 0,
    ) -> None:
        """
        Handles parsing and validating of config files
        and exposes ability to get all rules in parsed config files
        """
        self.valid = valid_configs
        self.with_code_rules = with_code_rules
        self.with_supply_chain = with_supply_chain
        self.with_secrets = with_secrets
        self.missed_rule_count = missed_rule_count

    @classmethod
    @tracing.trace()
    def from_pattern_lang(
        cls, pattern: str, lang: str, replacement: Optional[str] = None
    ) -> Tuple["Config", List[SemgrepError]]:
        config_dict = manual_config(pattern, lang, replacement)
        valid, errors, _ = cls._validate(config_dict)
        return cls(valid), errors

    @classmethod
    @tracing.trace()
    @lru_cache(maxsize=None)
    def from_rules_yaml(
        cls,
        config: str,
        no_rewrite_rule_ids: bool = False,
        force_jsonschema: bool = False,
    ) -> Tuple["Config", List[SemgrepError]]:
        config_dict: Dict[str, YamlTree] = {}
        errors: List[SemgrepError] = []

        try:
            resolved_config_key = CLOUD_PLATFORM_CONFIG_ID
            config_data, config_errors = parse_config_string(
                resolved_config_key,
                config,
                filename=None,
                no_rewrite_rule_ids=no_rewrite_rule_ids,
                force_jsonschema=force_jsonschema,
            )
            config_dict.update(config_data)
            errors.extend(config_errors)
        except SemgrepError as e:
            errors.append(e)

        valid, parse_errors, _ = cls._validate(config_dict)
        errors.extend(parse_errors)
        return cls(valid), errors

    @classmethod
    @tracing.trace()
    def from_config_list(
        cls,
        configs: Sequence[str],
        project_url: Optional[str],
        force_jsonschema: bool = False,
    ) -> Tuple["Config", List[SemgrepError]]:
        """
        Takes in list of files/directories and returns Config object as well as
        list of errors parsing said config files

        If empty list is passed, tries to read config file at default locations
        """
        config_dict: Dict[str, YamlTree] = {}
        errors: List[SemgrepError] = []
        with_supply_chain = False
        with_code_rules = False
        with_secrets = False

        for i, config in enumerate(configs):
            try:
                # Patch config_id to fix
                # https://github.com/semgrep/semgrep/issues/1912
                resolved_config, config_errors = resolve_config(
                    config, project_url, force_jsonschema=force_jsonschema
                )
                errors.extend(config_errors)
                if not resolved_config:
                    logger.verbose(f"Could not resolve config for {config}. Skipping.")
                    continue

                with_code_rules = with_code_rules or not is_supply_chain(config)
                with_supply_chain = with_supply_chain or is_supply_chain(config)
                with_secrets = with_secrets or is_secrets(config)

                for (
                    resolved_config_key,
                    resolved_config_yaml_tree,
                ) in resolved_config.items():
                    patched_resolved_config: Dict[str, YamlTree] = {}
                    patched_resolved_config[
                        f"{resolved_config_key}_{i}"
                    ] = resolved_config_yaml_tree

                    config_dict.update(patched_resolved_config)
            except SemgrepError as e:
                errors.append(e)

        valid, parse_errors, missed_rule_count = cls._validate(config_dict)
        errors.extend(parse_errors)
        return (
            cls(
                valid,
                with_code_rules=with_code_rules,
                with_supply_chain=with_supply_chain,
                with_secrets=with_secrets,
                missed_rule_count=missed_rule_count,
            ),
            errors,
        )

    def get_rules(self, no_rewrite_rule_ids: bool) -> List[Rule]:
        """
        Return list of rules

        If no_rewrite_rule_ids is True will not add
        path to config file to start of rule_ids
        """
        configs = self.valid
        if not no_rewrite_rule_ids:
            # re-write the configs to have the hierarchical rule ids
            self._rename_rule_ids(configs)

        # Deduplicate rules, ignoring metadata, which is not displayed
        # in the result.
        # Deduplication occurs from left to right so as to have the same
        # behavior as osemgrep i.e. the first occurrence of each rule
        # if preserved and subsequent occurrences are discarded.
        return list(
            reversed(
                list(
                    OrderedDict(
                        (rule_without_metadata(rule), rule)
                        for rules in reversed(configs.values())
                        for rule in reversed(rules)
                    ).values()
                )
            )
        )

    @staticmethod
    def _rename_rule_ids(valid_configs: Mapping[str, Sequence[Rule]]) -> None:
        for config_id, rules in valid_configs.items():
            for rule in rules:
                rule.rename_id(prepend_rule_path(config_id, rule.id))

    @staticmethod
    def _validate(
        config_dict: Mapping[str, YamlTree]
    ) -> Tuple[Mapping[str, Sequence[Rule]], List[SemgrepError], int]:
        """
        Take configs and separate into valid and list of errors parsing the invalid ones
        """
        errors: List[SemgrepError] = []
        valid: Dict[str, Any] = {}
        missed_rule_count = 0
        for config_id, config_yaml_tree in config_dict.items():
            config: YamlMap = config_yaml_tree.value
            if not isinstance(config, YamlMap):
                errors.append(SemgrepError(f"{config_id} was not a mapping"))
                continue
            # Increment the count of missed rules
            missed_rule_container = config.get(MISSED_KEY)
            missed_rule_count += (
                int(missed_rule_container.value) if missed_rule_container else 0
            )
            rules = config.get(RULES_KEY)
            if rules is None:
                errors.append(
                    InvalidRuleSchemaError(
                        short_msg="missing keys",
                        long_msg=f"{config_id} is missing `{RULES_KEY}` as top-level key",
                        spans=[config_yaml_tree.span.truncate(lines=5)],
                    )
                )
                continue
            valid_rules = []
            for rule_dict in rules.value:
                try:
                    rule = validate_single_rule(config_id, rule_dict)
                except InvalidRuleSchemaError as ex:
                    errors.append(ex)
                else:
                    if (
                        isinstance(rule.product.value, out.Secrets)
                        # In some instances we might append config_id with `_{i}` where
                        # i is an integer
                        and not config_id.startswith(REGISTRY_CONFIG_ID)
                        and not config_id.startswith(CLOUD_PLATFORM_CONFIG_ID)
                    ):
                        # SECURITY: Set metadata from non-registry secrets
                        # rules so that postprocessors are not run. The default
                        # requirement is that the rule be served from the pro
                        # origin. Without this, local rules could use
                        # postprocessors which may exfiltrate data from source
                        # code.
                        rule.metadata.get("semgrep.dev", {}).get("rule", {})[
                            "origin"
                        ] = "local"
                    valid_rules.append(rule)

            if valid_rules:
                valid[config_id] = valid_rules
        return valid, errors, missed_rule_count


def validate_single_rule(config_id: str, rule_yaml: YamlTree[YamlMap]) -> Rule:
    """
    Validate that a rule dictionary contains all necessary keys
    and can be correctly parsed.
    """
    rule: YamlMap = rule_yaml.value

    # Defaults to search mode if mode is not specified
    return Rule.from_yamltree(rule_yaml)


def manual_config(
    pattern: str, lang: str, replacement: Optional[str]
) -> Dict[str, YamlTree]:
    """Create a fake rule when we only have a pattern and language

    This is used when someone calls `semgrep scan -e print -l py`
    """
    pattern_span = Span.from_string(pattern, filename="CLI Input")
    pattern_tree = YamlTree[str](value=pattern, span=pattern_span)
    error_span = Span.from_string(
        f"Semgrep bug generating manual config {PLEASE_FILE_ISSUE_TEXT}", filename=None
    )
    rules_key = {
        ID_KEY: CLI_RULE_ID,
        "pattern": pattern_tree,
        "message": pattern,
        "languages": [lang],
        "severity": out.Error().to_json(),
    }

    if replacement:
        rules_key["fix"] = replacement

    return {
        "manual": YamlTree.wrap(
            {RULES_KEY: [rules_key]},
            span=error_span,
        )
    }


def adjust_for_docker() -> None:
    """change into this folder so that all paths are relative to it"""
    env = get_state().env
    if env.in_docker and not env.in_gh_action:
        try:
            # check if there's at least one file in /src
            next(env.src_directory.iterdir())
        except (NotADirectoryError, StopIteration):
            raise SemgrepError(
                f"Detected Docker environment without a code volume, please include '-v \"${{PWD}}:{env.src_directory}\"'"
            )
        else:
            os.chdir(env.src_directory)


def indent(msg: str) -> str:
    return "\n".join(["\t" + line for line in msg.splitlines()])


@tracing.trace()
def parse_config_string(
    config_id: str,
    contents: str,
    filename: Optional[str],
    no_rewrite_rule_ids: bool = False,
    force_jsonschema: bool = False,
) -> Tuple[Dict[str, YamlTree], List[SemgrepError]]:
    if not contents:
        raise SemgrepError(
            f"Empty configuration file {filename}", code=UNPARSEABLE_YAML_EXIT_CODE
        )

    # Should we guard this code and checks whether filename ends with .json?
    errors: List[SemgrepError] = []

    rules_tmp_path: Optional[str] = None
    try:
        fd, rules_tmp_path = mkstemp(suffix=".rules", prefix="semgrep-", text=True)
        with os.fdopen(fd, "w") as fp:
            fp.write(contents)
        logger.debug(f"Saving rules to {rules_tmp_path}")
    except Exception as e:
        logger.debug(f"Failed to write {rules_tmp_path=} to disk: {e}")

    try:
        # we pretend it came from YAML so we can keep later code simple
        data = YamlTree.wrap(json.loads(contents), EmptySpan)
        errors.extend(
            validate_yaml(
                data,
                filename,
                no_rewrite_rule_ids=no_rewrite_rule_ids,
                force_jsonschema=force_jsonschema,
                rules_tmp_path=rules_tmp_path,
            )
        )
        return ({config_id: data}, errors)
    except json.decoder.JSONDecodeError:
        pass

    try:
        data, config_errors = parse_config_preserve_spans(
            contents,
            filename,
            force_jsonschema=force_jsonschema,
            rules_tmp_path=rules_tmp_path,
        )
        errors.extend(config_errors)
    except EmptyYamlException:
        raise SemgrepError(
            f"Empty configuration file {filename}", code=UNPARSEABLE_YAML_EXIT_CODE
        )
    except YAMLError as se:
        raise SemgrepError(
            f"Invalid YAML file {config_id}:\n{indent(str(se))}",
            code=UNPARSEABLE_YAML_EXIT_CODE,
        )
    return {config_id: data}, errors


def is_registry_id(config_str: str) -> bool:
    """
    Starts with r/, p/, s/ for registry, pack, and snippet respectively
    """
    return config_str[:2] in {"r/", "p/", "s/"}


def registry_id_to_url(registry_id: str) -> str:
    """
    Convert from registry_id to semgrep.dev url
    """
    env = get_state().env
    return f"{env.semgrep_url}/{registry_id}"


def is_code(config_str: str) -> bool:
    return config_str == "code"


def is_supply_chain(config_str: str) -> bool:
    return config_str == "supply-chain"


def is_secrets(config_str: str) -> bool:
    return config_str == "secrets"


def is_pack_id(config_str: str) -> bool:
    return config_str[:2] == "p/"


@tracing.trace()
def get_config(
    pattern: Optional[str],
    lang: Optional[str],
    config_strs: Sequence[str],
    *,
    project_url: Optional[str],
    replacement: Optional[str] = None,
    no_rewrite_rule_ids: bool = False,
    force_jsonschema: bool = False,
) -> Tuple[Config, List[SemgrepError]]:
    if pattern:
        if not lang:
            raise SemgrepError("language must be specified when a pattern is passed")
        config, errors = Config.from_pattern_lang(pattern, lang, replacement)
    elif len(config_strs) == 1 and is_rules(config_strs[0]):
        config, errors = Config.from_rules_yaml(
            config_strs[0],
            no_rewrite_rule_ids=no_rewrite_rule_ids,
            force_jsonschema=force_jsonschema,
        )
    elif replacement:
        raise SemgrepError(
            "command-line replacement flag can only be used with command-line pattern; when using a config file add the fix: key instead"
        )
    else:
        config, errors = Config.from_config_list(
            config_strs, project_url, force_jsonschema=force_jsonschema
        )

    return config, errors
