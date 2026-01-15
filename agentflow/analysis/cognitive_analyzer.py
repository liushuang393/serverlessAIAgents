# -*- coding: utf-8 -*-
"""認知分析コンポーネント - 認知バイアス検出・明確性分析.

DGEのClarificationAgentから抽出した汎用コンポーネント。
LLMなしでも動作するルールベース実装を提供。

使用例:
    >>> detector = CognitiveBiasDetector()
    >>> biases = detector.detect("この方法が最善だ")
    >>> analyzer = ClarityAnalyzer()
    >>> result = analyzer.analyze("新システムを構築すべきか", ["予算1000万"])
"""

import logging
from dataclasses import dataclass, field
from typing import Any

_logger = logging.getLogger(__name__)


@dataclass
class CognitiveBias:
    """認知バイアス."""
    bias: str
    manifestation: str
    severity: str = "medium"  # low/medium/high


@dataclass
class HiddenAssumption:
    """暗黙の仮定."""
    assumption: str
    validity_question: str


@dataclass
class AmbiguityPoint:
    """曖昧な点."""
    point: str
    clarification_question: str


@dataclass
class ClarityResult:
    """明確性分析結果."""
    is_clear: bool
    confidence: float
    missing_elements: list[str] = field(default_factory=list)
    ambiguities: list[AmbiguityPoint] = field(default_factory=list)
    hidden_assumptions: list[HiddenAssumption] = field(default_factory=list)
    cognitive_biases: list[CognitiveBias] = field(default_factory=list)
    refined_question: str = ""


class CognitiveBiasDetector:
    """認知バイアス検出器.
    
    質問やテキストから認知バイアスを検出。
    """
    
    # バイアスパターン定義
    BIAS_PATTERNS: dict[str, dict[str, Any]] = {
        "confirmation": {
            "name": "確証バイアス",
            "keywords": ["絶対", "必ず", "間違いなく", "確実に"],
            "description": "自分の信念を裏付ける情報のみを重視",
        },
        "sunk_cost": {
            "name": "サンクコスト",
            "keywords": ["既に投資", "これまで", "せっかく", "今更"],
            "description": "過去の投資に引きずられて判断",
        },
        "anchoring": {
            "name": "アンカリング",
            "keywords": ["最初の", "当初", "元々", "本来"],
            "description": "最初の情報に過度に依存",
        },
        "availability": {
            "name": "利用可能性ヒューリスティック",
            "keywords": ["最近", "よく聞く", "話題の", "流行り"],
            "description": "思い出しやすい情報を過大評価",
        },
        "overconfidence": {
            "name": "過信バイアス",
            "keywords": ["簡単", "すぐに", "問題ない", "大丈夫"],
            "description": "自分の能力や判断を過信",
        },
    }
    
    def detect(self, text: str) -> list[CognitiveBias]:
        """テキストから認知バイアスを検出.
        
        Args:
            text: 分析対象テキスト
            
        Returns:
            検出されたバイアスリスト
        """
        biases = []
        for bias_id, info in self.BIAS_PATTERNS.items():
            matched_keywords = [kw for kw in info["keywords"] if kw in text]
            if matched_keywords:
                biases.append(CognitiveBias(
                    bias=info["name"],
                    manifestation=f"「{'」「'.join(matched_keywords)}」を含み、{info['description']}の可能性",
                    severity="medium" if len(matched_keywords) == 1 else "high",
                ))
        return biases[:3]  # 最大3件


class ClarityAnalyzer:
    """明確性分析器.
    
    質問や意図の明確性を分析し、不足要素を特定。
    """
    
    def __init__(self, bias_detector: CognitiveBiasDetector | None = None):
        """初期化."""
        self._bias_detector = bias_detector or CognitiveBiasDetector()
    
    def analyze(
        self,
        question: str,
        constraints: list[str] | None = None,
    ) -> ClarityResult:
        """質問の明確性を分析.
        
        Args:
            question: 分析対象の質問
            constraints: 制約条件リスト
            
        Returns:
            明確性分析結果
        """
        constraints = constraints or []
        
        # 曖昧な点を検出
        ambiguities = self._detect_ambiguities(question)
        
        # 暗黙の仮定を検出
        assumptions = self._detect_assumptions(question, constraints)
        
        # 認知バイアスを検出
        biases = self._bias_detector.detect(question)
        
        # 不足要素を特定
        missing = self._identify_missing(question, constraints)
        
        # 明確性を判定
        is_clear = len(ambiguities) == 0 and len(missing) <= 1
        confidence = self._calculate_confidence(ambiguities, assumptions, biases)
        
        # 精緻化された質問を生成
        refined = self._refine_question(question, ambiguities, assumptions)
        
        return ClarityResult(
            is_clear=is_clear,
            confidence=confidence,
            missing_elements=missing,
            ambiguities=ambiguities,
            hidden_assumptions=assumptions,
            cognitive_biases=biases,
            refined_question=refined,
        )
    
    def _detect_ambiguities(self, question: str) -> list[AmbiguityPoint]:
        """曖昧な点を検出."""
        ambiguities = []
        
        # 主語の曖昧さ
        if not any(w in question for w in ["私", "我々", "自社", "当社", "チーム"]):
            ambiguities.append(AmbiguityPoint(
                point="判断主体が不明確",
                clarification_question="誰が（どの組織が）この判断を行いますか？",
            ))
        
        # 時間軸の曖昧さ
        if not any(w in question for w in ["いつ", "期限", "までに", "年", "月", "週"]):
            ambiguities.append(AmbiguityPoint(
                point="時間軸が不明確",
                clarification_question="いつまでに判断・実行する必要がありますか？",
            ))
        
        return ambiguities[:3]
    
    def _detect_assumptions(
        self, question: str, constraints: list[str]
    ) -> list[HiddenAssumption]:
        """暗黙の仮定を検出."""
        assumptions = []
        
        # リソース無限仮定
        if "構築" in question or "開発" in question:
            if not any("予算" in c or "人員" in c for c in constraints):
                assumptions.append(HiddenAssumption(
                    assumption="必要なリソース（予算・人員）が確保できると仮定",
                    validity_question="リソース制約は考慮していますか？",
                ))
        
        # 市場環境固定仮定
        assumptions.append(HiddenAssumption(
            assumption="現在の環境が継続すると仮定",
            validity_question="環境が急変した場合のシナリオは検討しましたか？",
        ))
        
        return assumptions[:3]
    
    def _identify_missing(
        self, question: str, constraints: list[str]
    ) -> list[str]:
        """不足要素を特定."""
        missing = []
        
        if len(question) < 20:
            missing.append("詳細な背景情報")
        if not constraints:
            missing.append("制約条件")
        if "目的" not in question and "ため" not in question:
            missing.append("判断の目的")
            
        return missing
    
    def _calculate_confidence(
        self,
        ambiguities: list[AmbiguityPoint],
        assumptions: list[HiddenAssumption],
        biases: list[CognitiveBias],
    ) -> float:
        """分析確信度を計算."""
        base = 0.7
        if len(ambiguities) >= 2:
            base += 0.1
        if len(assumptions) >= 2:
            base += 0.1
        if len(biases) >= 1:
            base += 0.05
        return min(base, 0.95)
    
    def _refine_question(
        self,
        question: str,
        ambiguities: list[AmbiguityPoint],
        assumptions: list[HiddenAssumption],
    ) -> str:
        """質問を精緻化."""
        if not ambiguities and not assumptions:
            return question
        
        additions = []
        for amb in ambiguities[:2]:
            additions.append(f"（{amb.point}を明確化）")
        
        return question + " " + " ".join(additions)

