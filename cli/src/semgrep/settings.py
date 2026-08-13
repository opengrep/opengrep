"""
Manages system settings for Semgrep

These can change the way Semgrep operates within the environment. They are distinct from config,
which describes which rules Semgrep should run.

To retrieve settings use `get_state().settings.get("key")`, to set them use `get_state().settings.set("key", "value").value = ...`.

If no settings have been configured on the system, DEFAULT_SETTINGS will be written.

If the process does not have permission to the settings path, a PermissionError will be raised;
callers should handle this gracefully.
"""
import os
import uuid
from pathlib import Path
from tempfile import mkstemp
from typing import Any
from typing import cast
from typing import Mapping
from typing import Optional

from attr import define
from attr import field
from ruamel.yaml import YAML
from typing_extensions import Literal
from typing_extensions import TypedDict

from semgrep.env import Env
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)
yaml = YAML()
yaml.default_flow_style = False


class SettingsSchema(TypedDict, total=False):
    anonymous_user_id: str


SettingsKeys = Literal["anonymous_user_id"]


def generate_anonymous_user_id() -> str:
    return str(uuid.uuid4())


def generate_default_settings() -> SettingsSchema:
    return {"anonymous_user_id": generate_anonymous_user_id()}


@define
class Settings:
    path: Path = field()
    _contents: SettingsSchema = field()

    @path.default
    def get_default_path(self) -> Path:
        """
        Uses {$XDG_CONFIG_HOME/semgrep || ~/.semgrep}/settings.yaml unless SEMGREP_SETTINGS_FILE is set
        """
        # state.env and state.settings get initialized together, but settings depends on env
        # so we just read the env a second time here ¯\_(ツ)_/¯
        env = Env()
        return env.user_settings_file

    @_contents.default
    def get_default_contents(self) -> SettingsSchema:
        """If file exists, read file. Otherwise use default"""
        return generate_default_settings()

    def __attrs_post_init__(self) -> None:
        return

    # coupling: src/osemgrep/configuring/Semgrep_setting.ml save
    def save(self) -> None:
        return

    def get(self, key: SettingsKeys, default: Any = None) -> Any:
        return self._contents.get(key, default)

    def set(self, key: SettingsKeys, value: Any) -> None:
        """
        Update and save this system's settings object

        :param value: The settings object
        """
        self._contents[key] = value

    def delete(self, key: SettingsKeys) -> None:
        """
        Deletes key KEY from settings file if it exists

        Noop otherwise
        """
        if key in self._contents:
            del self._contents[key]
