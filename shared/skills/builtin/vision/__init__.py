"""後方互換ブリッジ — 実装は kernel/skills/builtin/ に移動済み."""

from __future__ import annotations

import warnings
from importlib import import_module


_vision = import_module("kernel.skills.builtin.vision.vision")
VisionConfig = _vision.VisionConfig
VisionResult = _vision.VisionResult
VisionSkill = _vision.VisionSkill


warnings.warn(
    "shared.skills.builtin.vision は非推奨です。kernel.skills.builtin.vision を使用してください。",
    DeprecationWarning,
    stacklevel=2,
)

__all__ = ["VisionConfig", "VisionResult", "VisionSkill"]
