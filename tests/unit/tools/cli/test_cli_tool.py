import pytest

from agentflow.tools.cli.base import CLIToolConfig
from agentflow.tools.cli.validators import CLIValidator


def test_cli_tool_config_creation():
    config = CLIToolConfig(
        name="git",
        executable="/usr/bin/git",
        allowed_flags={"status", "log", "diff"},
        forbidden_flags={"push", "reset"},
    )
    assert config.name == "git"
    assert config.executable == "/usr/bin/git"
    assert "status" in config.allowed_flags
    assert "push" in config.forbidden_flags


def test_cli_tool_config_defaults():
    config = CLIToolConfig(
        name="echo",
        executable="/bin/echo",
    )
    assert config.allowed_flags == set()
    assert config.forbidden_flags == set()
    assert config.max_timeout_seconds == 60.0
    assert config.max_memory_mb == 1024
    assert config.evidence_required is True


def test_command_whitelist_validation():
    config = CLIToolConfig(
        name="git",
        executable="/usr/bin/git",
        allowed_flags={"status", "log", "diff"},
        forbidden_flags={"push", "reset"},
    )
    validator = CLIValidator(config)

    # Should pass
    validator.validate_command(["git", "status"])
    validator.validate_command(["git", "log", "--oneline"])

    # Should fail
    with pytest.raises(ValueError, match=r"(?i)forbidden"):
        validator.validate_command(["git", "push"])


def test_command_forbidden_with_dashes():
    config = CLIToolConfig(
        name="git",
        executable="/usr/bin/git",
        forbidden_flags={"force", "hard"},
    )
    validator = CLIValidator(config)

    with pytest.raises(ValueError, match=r"(?i)forbidden"):
        validator.validate_command(["git", "push", "--force"])

    with pytest.raises(ValueError, match=r"(?i)forbidden"):
        validator.validate_command(["git", "reset", "--hard"])


def test_validator_allows_unforbidden():
    config = CLIToolConfig(
        name="ls",
        executable="/bin/ls",
        forbidden_flags={"recursive"},
    )
    validator = CLIValidator(config)

    # Should pass - not in forbidden list
    validator.validate_command(["ls", "-la"])
    validator.validate_command(["ls", "/tmp"])


def test_config_with_custom_limits():
    config = CLIToolConfig(
        name="heavy_task",
        executable="/usr/bin/heavy",
        max_timeout_seconds=300.0,
        max_memory_mb=4096,
    )
    assert config.max_timeout_seconds == 300.0
    assert config.max_memory_mb == 4096


def test_config_evidence_not_required():
    config = CLIToolConfig(
        name="trivial",
        executable="/bin/true",
        evidence_required=False,
    )
    assert config.evidence_required is False
