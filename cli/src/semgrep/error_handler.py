from typing import Optional

from attr import define
from attr import field

from semgrep.error import FINDINGS_EXIT_CODE
from semgrep.error import OK_EXIT_CODE
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


@define
class ErrorHandler:
    """
    Decide whether a scan that hit errors should still exit successfully
    """

    suppress_errors: bool = field(default=False)

    def configure(self, suppress_errors: Optional[bool] = False) -> None:
        """
        Configures whether to always or never suppress errors.

        :param suppress_errors: The value of the --suppress-errors option
        """
        if suppress_errors:
            self.suppress_errors = suppress_errors

    def suppress(self, exit_code: int) -> int:
        """
        Return the exit code the scan should end with.

        Errors are only suppressed if --suppress-errors was passed
        """
        if (
            not self.suppress_errors
            or exit_code == OK_EXIT_CODE
            or exit_code == FINDINGS_EXIT_CODE
        ):
            return exit_code

        logger.error(
            "There were errors during analysis but the scan will succeed because there were no blocking findings, use --no-suppress-errors if you want it to fail when there are errors."
        )

        return 0
