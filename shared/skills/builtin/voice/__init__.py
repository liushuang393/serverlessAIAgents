"""後方互換ブリッジ — 実装は kernel/skills/builtin/ に移動済み."""

from __future__ import annotations

import warnings

from kernel.skills.builtin.voice.voice import VoiceConfig, VoiceSkill


warnings.warn(
    "shared.skills.builtin.voice は非推奨です。kernel.skills.builtin.voice を使用してください。",
    DeprecationWarning,
    stacklevel=2,
)

__all__ = ["VoiceConfig", "VoiceSkill"]
