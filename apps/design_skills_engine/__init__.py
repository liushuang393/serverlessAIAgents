"""Design Skills Engine - Lovart風マルチ画像生成エンジン.

コア実装は kernel / shared / infrastructure の共通基盤へ移動済み。
このモジュールは後方互換性のための再エクスポートを提供する。

クイックスタート:
    >>> from kernel.skills.builtin.design_skills import DesignSkillsEngine
    >>>
    >>> engine = DesignSkillsEngine()
    >>> result = await engine.run({
    ...     "brief": "Bluetooth スピーカーの商品画像を生成",
    ...     "num_images": 8,
    ... })

アーキテクチャ:
    IntentAnalyzerAgent → PromptPlannerAgent → WorkflowExecutorAgent
"""

from kernel.skills.builtin.design_skills import DesignSkillsEngine


__all__ = ["DesignSkillsEngine"]
