# -*- coding: utf-8 -*-
"""AgentFlow Analysis - 認知分析・品質評価モジュール.

提供機能:
- CognitiveBiasDetector: 認知バイアス検出
- ClarityAnalyzer: 質問/意図の明確性分析
- QualityEvaluator: 多次元品質評価

これらはDGEから抽出した汎用コンポーネントです。

使用例:
    >>> from agentflow.analysis import CognitiveBiasDetector, ClarityAnalyzer
    >>>
    >>> detector = CognitiveBiasDetector()
    >>> biases = detector.detect("この投資は絶対に成功する")
    >>> print(biases)  # [CognitiveBias(bias="確証バイアス", ...)]
    >>>
    >>> analyzer = ClarityAnalyzer()
    >>> result = analyzer.analyze("新システムを構築すべきか")
    >>> print(result.is_clear, result.missing_elements)
"""

from agentflow.analysis.cognitive_analyzer import (
    CognitiveBias,
    CognitiveBiasDetector,
    ClarityAnalyzer,
    ClarityResult,
    HiddenAssumption,
    AmbiguityPoint,
)
from agentflow.analysis.quality_evaluator import (
    QualityEvaluator,
    QualityResult,
    QualityDimension,
)

__all__ = [
    # 認知バイアス検出
    "CognitiveBiasDetector",
    "CognitiveBias",
    "HiddenAssumption",
    "AmbiguityPoint",
    # 明確性分析
    "ClarityAnalyzer",
    "ClarityResult",
    # 品質評価
    "QualityEvaluator",
    "QualityResult",
    "QualityDimension",
]

