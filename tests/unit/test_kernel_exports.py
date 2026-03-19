"""Kernel public export tests."""

from __future__ import annotations


def test_kernel_exports_get_llm() -> None:
    """`from kernel import get_llm` should stay valid."""
    from kernel import get_llm

    assert get_llm.__name__ == "get_llm"


def test_websocket_hub_direct_import() -> None:
    """`WebSocketHub` は control_plane から直接 import する."""
    from control_plane.api.websocket_hub import WebSocketHub

    assert WebSocketHub.__name__ == "WebSocketHub"


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
