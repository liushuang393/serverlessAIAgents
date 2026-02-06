"""Base configuration for CLI tools.

Defines the structure for CLI tool configurations,
including security constraints and resource limits.
"""

from pydantic import BaseModel, Field


class CLIToolConfig(BaseModel):
    """Configuration for a CLI tool.

    Defines what a CLI tool can do, including:
    - Which flags are allowed/forbidden
    - Resource limits (timeout, memory)
    - Whether evidence collection is required

    Attributes:
        name: Name of the tool
        executable: Path to the executable
        allowed_flags: Flags that are explicitly allowed
        forbidden_flags: Flags that are forbidden (security)
        max_timeout_seconds: Maximum execution time
        max_memory_mb: Maximum memory usage in MB
        evidence_required: Whether to collect execution evidence
    """

    name: str
    executable: str
    allowed_flags: set[str] = Field(default_factory=set)
    forbidden_flags: set[str] = Field(default_factory=set)
    max_timeout_seconds: float = 60.0
    max_memory_mb: int = 1024
    evidence_required: bool = True

    model_config = {"frozen": False}

    def is_flag_allowed(self, flag: str) -> bool:
        """Check if a flag is allowed.

        If allowed_flags is empty, all non-forbidden flags are allowed.
        If allowed_flags is non-empty, only those flags are allowed.

        Args:
            flag: The flag to check (without leading dashes)

        Returns:
            True if the flag is allowed
        """
        clean_flag = flag.lstrip("-")

        # Check forbidden first
        if clean_flag in self.forbidden_flags:
            return False

        # If allowed_flags is empty, allow all non-forbidden
        if not self.allowed_flags:
            return True

        # Otherwise, must be in allowed list
        return clean_flag in self.allowed_flags

    def is_flag_forbidden(self, flag: str) -> bool:
        """Check if a flag is forbidden.

        Args:
            flag: The flag to check (without leading dashes)

        Returns:
            True if the flag is forbidden
        """
        clean_flag = flag.lstrip("-")
        return clean_flag in self.forbidden_flags
