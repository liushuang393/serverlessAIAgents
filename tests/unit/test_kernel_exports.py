"""Kernel public export tests."""

from __future__ import annotations


def test_kernel_exports_get_llm() -> None:
    """`from kernel import get_llm` should stay valid."""
    from kernel import get_llm

    assert get_llm.__name__ == "get_llm"


def test_websocket_hub_runtime_import() -> None:
    """`WebSocketHub` should be available from kernel.runtime."""
    from kernel.runtime import WebSocketHub

    assert WebSocketHub.__name__ == "WebSocketHub"


def test_websocket_hub_control_plane_compat_import() -> None:
    """control_plane compatibility import should remain valid."""
    from control_plane.api.websocket_hub import WebSocketHub

    assert WebSocketHub.__name__ == "WebSocketHub"


def test_kernel_skills_exports_skill_runtime() -> None:
    """`from kernel.skills import SkillRuntime` should stay valid."""
    from kernel.skills import ScriptResult, Skill, SkillRouter, SkillRuntime

    assert SkillRuntime.__name__ == "SkillRuntime"
    assert SkillRouter.__name__ == "SkillRouter"
    assert Skill.__name__ == "Skill"
    assert ScriptResult.__name__ == "ScriptResult"


def test_kernel_skills_exports_messaging_hub_symbols() -> None:
    """Messaging Hub が依存する skills 互換導出を維持する."""
    from kernel.skills import (
        CalendarSkill,
        ChatBotSkill,
        ConversationExportSkill,
        ExportFormat,
        RiskLevel,
        create_skill_gateway,
    )

    assert CalendarSkill.__name__ == "CalendarSkill"
    assert ChatBotSkill.__name__ == "ChatBotSkill"
    assert ConversationExportSkill.__name__ == "ConversationExportSkill"
    assert ExportFormat.__name__ == "ExportFormat"
    assert RiskLevel.__name__ == "RiskLevel"
    assert create_skill_gateway.__name__ == "create_skill_gateway"
