"""Vision Skill - 画像認識・分析機能.

GPT-4V / Claude Vision / Gemini Vision を使用した画像認識スキル。

Example:
    >>> from agentflow.skills.builtin.vision import VisionSkill
    >>> vision = VisionSkill()
    >>> result = await vision.analyze_image(
    ...     image_url="https://example.com/image.jpg",
    ...     prompt="この画像に何が写っていますか？"
    ... )
"""

from agentflow.skills.builtin.vision.vision import (
    VisionConfig,
    VisionProvider,
    VisionResult,
    VisionSkill,
)


__all__ = [
    "VisionConfig",
    "VisionProvider",
    "VisionResult",
    "VisionSkill",
]

