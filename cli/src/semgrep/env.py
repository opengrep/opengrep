import os
from pathlib import Path
from typing import Iterable
from typing import Optional
from typing import overload
from typing import Union

from attr import Factory
from attr import field
from attr import frozen

from semgrep.constants import SETTINGS_FILENAME


def url(value: str) -> str:
    return value.rstrip("/")


@overload
def EnvFactory(envvars: Union[str, Iterable[str]], default: str) -> str:
    ...


@overload
def EnvFactory(envvars: Union[str, Iterable[str]]) -> Optional[str]:
    ...


def EnvFactory(
    envvars: Union[str, Iterable[str]], default: Optional[str] = None
) -> Optional[str]:
    if isinstance(envvars, str):
        envvars = [envvars]

    def env_getter() -> Optional[str]:
        for envvar in envvars:
            if os.getenv(envvar):
                return os.getenv(envvar)
        return default

    return Factory(env_getter)


@frozen
class Env:
    """Returns the value of an environment variable at the time of command invocation.

    This is better than just keeping these values as constants on the module level,
    because tests and other non-CLI based invocations might change env variables
    between multiple invocations.
    """

    semgrep_url: str = field(
        # TODO: This needs to be changed to https://opengrep.dev, but tests fail
        # without it.
        default=EnvFactory(["SEMGREP_URL", "SEMGREP_APP_URL"], "https://semgrep.dev"),
        converter=url,
    )

    git_command_timeout: int = field()

    src_directory: Path = field()
    user_data_folder: Path = field()
    user_log_file: Path = field()
    user_settings_file: Path = field()

    in_docker: bool = field()
    in_gh_action: bool = field()
    with_new_cli_ux: bool = field()
    mock_using_registry: bool = field()
    min_fetch_depth: int = field()

    @git_command_timeout.default
    def git_command_timeout_default(self) -> int:
        value = os.getenv("SEMGREP_GIT_COMMAND_TIMEOUT", "300")
        return int(value)

    @src_directory.default
    def src_directory_default(self) -> Path:
        value = os.getenv("SEMGREP_SRC_DIRECTORY")
        if value:
            return Path(value)
        return Path("/src")

    @user_data_folder.default
    def user_data_folder_default(self) -> Path:
        config_home = os.getenv("XDG_CONFIG_HOME")
        if config_home is None or not Path(config_home).is_dir():
            parent_dir = Path.home()
        else:
            parent_dir = Path(config_home)
        return parent_dir / ".opengrep"

    @user_log_file.default
    def user_log_file_default(self) -> Path:
        path = os.getenv("SEMGREP_LOG_FILE", str(self.user_data_folder / "semgrep.log"))
        return Path(path)

    @user_settings_file.default
    def user_settings_file_default(self) -> Path:
        path = os.getenv(
            "SEMGREP_SETTINGS_FILE", str(self.user_data_folder / SETTINGS_FILENAME)
        )
        return Path(path)

    @in_docker.default
    def in_docker_default(self) -> bool:
        return "SEMGREP_IN_DOCKER" in os.environ

    @in_gh_action.default
    def in_gh_action_default(self) -> bool:
        return "GITHUB_WORKSPACE" in os.environ

    @with_new_cli_ux.default
    def with_new_cli_default(self) -> bool:
        return os.environ.get("SEMGREP_NEW_CLI_UX", "0") == "1"

    @mock_using_registry.default
    def with_mock_using_registry(self) -> bool:
        return "MOCK_USING_REGISTRY" in os.environ

    @min_fetch_depth.default
    def min_fetch_depth_default(self) -> int:
        value = os.getenv("SEMGREP_GHA_MIN_FETCH_DEPTH", "0")
        return int(value)
