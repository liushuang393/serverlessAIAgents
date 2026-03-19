"""Kernel public export tests."""

from __future__ import annotations


def test_kernel_exports_websocket_hub() -> None:
    """`from kernel import WebSocketHub` should stay valid."""
    from kernel import WebSocketHub
    from kernel import get_llm

    assert WebSocketHub.__name__ == "WebSocketHub"
    assert get_llm.__name__ == "get_llm"


def test_kernel_skills_exports_skill_runtime() -> None:
    """`from kernel.skills import SkillRuntime` should stay valid."""
    from kernel.skills import ScriptResult
    from kernel.skills import Skill
    from kernel.skills import SkillRouter
    from kernel.skills import SkillRuntime

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
