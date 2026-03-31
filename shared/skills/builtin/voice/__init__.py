"""後方互換ブリッジ — 実装は kernel/skills/builtin/ に移動済み."""

from __future__ import annotations

import warnings
from importlib import import_module


_voice = import_module("kernel.skills.builtin.voice.voice")
VoiceConfig = _voice.VoiceConfig
VoiceSkill = _voice.VoiceSkill


warnings.warn(
    "shared.skills.builtin.voice は非推奨です。kernel.skills.builtin.voice を使用してください。",
    DeprecationWarning,
    stacklevel=2,
)

__all__ = ["VoiceConfig", "VoiceSkill"]
