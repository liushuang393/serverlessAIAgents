"""Validators for CLI tool commands.

Provides validation logic to ensure CLI commands are safe
before execution.
"""

from agentflow.tools.cli.base import CLIToolConfig


class CLIValidator:
    """Validator for CLI commands.

    Validates commands against the tool configuration,
    ensuring forbidden flags are not used.

    Example:
        config = CLIToolConfig(
            name="git",
            executable="/usr/bin/git",
            forbidden_flags={"push", "force"},
        )
        validator = CLIValidator(config)
        validator.validate_command(["git", "status"])  # OK
        validator.validate_command(["git", "push"])    # Raises ValueError
    """

    def __init__(self, config: CLIToolConfig) -> None:
        """Initialize the validator.

        Args:
            config: The CLI tool configuration
        """
        self._config = config

    def validate_command(self, cmd: list[str]) -> None:
        """Validate a command against the configuration.

        Args:
            cmd: The command as a list of strings

        Raises:
            ValueError: If the command contains forbidden flags or subcommands
        """
        for arg in cmd[1:]:  # Skip the executable name
            # Check if this is a flag (starts with -)
            if arg.startswith("-"):
                clean_arg = arg.lstrip("-")
                if self._config.is_flag_forbidden(clean_arg):
                    raise ValueError(
                        f"Forbidden flag '{arg}' in command. "
                        f"Forbidden flags: {self._config.forbidden_flags}"
                    )
            else:
                # Also check positional arguments (subcommands like 'push', 'reset')
                if arg in self._config.forbidden_flags:
                    raise ValueError(
                        f"Forbidden subcommand '{arg}' in command. "
                        f"Forbidden: {self._config.forbidden_flags}"
                    )

    def get_config(self) -> CLIToolConfig:
        """Get the configuration for this validator.

        Returns:
            The CLI tool configuration
        """
        return self._config
