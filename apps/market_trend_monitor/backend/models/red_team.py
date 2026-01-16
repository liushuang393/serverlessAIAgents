# -*- coding: utf-8 -*-
"""Red Team Analysis モデル定義.

反証分析システムのデータモデル。
各トレンド判断に対する反対論点を生成し、バイアスを軽減します。
"""

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class ChallengeType(str, Enum):
    """チャレンジタイプ."""

    COUNTER_EVIDENCE = "counter_evidence"
    ALTERNATIVE_EXPLANATION = "alternative_explanation"
    BIAS_CHECK = "bias_check"
    ASSUMPTION_CHALLENGE = "assumption_challenge"


# ============================================================
# Dataclass モデル（内部データ表現）
# ============================================================


@dataclass
class Challenge:
    """チャレンジデータモデル.

    主張に対する反証チャレンジを表現します。
    """

    id: str
    claim_id: str
    challenge_type: ChallengeType
    argument: str
    counter_evidence_ids: list[str] = field(default_factory=list)
    created_at: datetime = field(default_factory=datetime.now)
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "id": self.id,
            "claim_id": self.claim_id,
            "challenge_type": self.challenge_type.value,
            "argument": self.argument,
            "counter_evidence_ids": self.counter_evidence_ids,
            "created_at": self.created_at.isoformat(),
            "metadata": self.metadata,
        }


@dataclass
class ChallengeResult:
    """チャレンジ評価結果データモデル.

    チャレンジの評価結果を表現します。
    """

    id: str
    challenge_id: str
    is_valid: bool
    impact_score: float
    confidence_adjustment: float
    evaluator_notes: str = ""
    evaluated_at: datetime = field(default_factory=datetime.now)
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "id": self.id,
            "challenge_id": self.challenge_id,
            "is_valid": self.is_valid,
            "impact_score": self.impact_score,
            "confidence_adjustment": self.confidence_adjustment,
            "evaluator_notes": self.evaluator_notes,
            "evaluated_at": self.evaluated_at.isoformat(),
            "metadata": self.metadata,
        }


@dataclass
class CounterArgument:
    """反論データモデル.
    
    主張に対する反対意見を表現します。
    """

    argument: str                            # 反論内容
    strength: float = 0.5                    # 反論の強さ (0-1)
    evidence_ids: list[str] = field(default_factory=list)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "argument": self.argument,
            "strength": self.strength,
            "evidence_ids": self.evidence_ids,
        }


@dataclass
class InvalidationCondition:
    """失効条件データモデル.
    
    主張が無効になる条件を表現します。
    """

    condition: str                           # 失効条件の説明
    probability: float = 0.0                 # 発生確率 (0-1)
    trigger_indicators: list[str] = field(default_factory=list)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "condition": self.condition,
            "probability": self.probability,
            "trigger_indicators": self.trigger_indicators,
        }


@dataclass
class RedTeamAnalysis:
    """Red Team 分析結果データモデル.
    
    主張に対する反証分析結果を表現します。
    """

    id: str
    claim_id: str
    counter_arguments: list[CounterArgument] = field(default_factory=list)
    invalidation_conditions: list[InvalidationCondition] = field(
        default_factory=list
    )
    overall_uncertainty: float = 0.0         # 総合不確実性 (0-1)
    recommendation: str = ""                 # 推奨アクション
    analyzed_at: datetime = field(default_factory=datetime.now)
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "id": self.id,
            "claim_id": self.claim_id,
            "counter_arguments": [ca.to_dict() for ca in self.counter_arguments],
            "invalidation_conditions": [
                ic.to_dict() for ic in self.invalidation_conditions
            ],
            "overall_uncertainty": self.overall_uncertainty,
            "recommendation": self.recommendation,
            "analyzed_at": self.analyzed_at.isoformat(),
            "metadata": self.metadata,
        }

    def calculate_uncertainty(self) -> float:
        """反論と失効条件から不確実性を計算."""
        if not self.counter_arguments and not self.invalidation_conditions:
            return 0.0
        
        # 反論の強さの加重平均
        counter_weight = 0.0
        if self.counter_arguments:
            counter_weight = sum(
                ca.strength for ca in self.counter_arguments
            ) / len(self.counter_arguments)
        
        # 失効条件の確率加重
        invalidation_weight = 0.0
        if self.invalidation_conditions:
            invalidation_weight = sum(
                ic.probability for ic in self.invalidation_conditions
            ) / len(self.invalidation_conditions)
        
        # 総合不確実性の計算（50:50の重み付け）
        self.overall_uncertainty = (counter_weight + invalidation_weight) / 2
        return self.overall_uncertainty


# ============================================================
# Pydantic スキーマ（API I/O用）
# ============================================================


class CounterArgumentSchema(BaseModel):
    """反論スキーマ."""

    argument: str
    strength: float = Field(ge=0.0, le=1.0, default=0.5)
    evidence_ids: list[str] = Field(default_factory=list)


class InvalidationConditionSchema(BaseModel):
    """失効条件スキーマ."""

    condition: str
    probability: float = Field(ge=0.0, le=1.0, default=0.0)
    trigger_indicators: list[str] = Field(default_factory=list)


class RedTeamAnalysisSchema(BaseModel):
    """Red Team 分析スキーマ."""

    id: str
    claim_id: str
    counter_arguments: list[CounterArgumentSchema] = Field(default_factory=list)
    invalidation_conditions: list[InvalidationConditionSchema] = Field(
        default_factory=list
    )
    overall_uncertainty: float = Field(ge=0.0, le=1.0, default=0.0)
    recommendation: str = ""
    analyzed_at: str
    metadata: dict = Field(default_factory=dict)


class ChallengeSchema(BaseModel):
    """チャレンジスキーマ."""

    id: str
    claim_id: str
    challenge_type: str
    argument: str
    counter_evidence_ids: list[str] = Field(default_factory=list)
    created_at: str
    metadata: dict = Field(default_factory=dict)


class ChallengeResultSchema(BaseModel):
    """チャレンジ評価結果スキーマ."""

    id: str
    challenge_id: str
    is_valid: bool
    impact_score: float = Field(ge=0.0, le=1.0)
    confidence_adjustment: float
    evaluator_notes: str = ""
    evaluated_at: str
    metadata: dict = Field(default_factory=dict)
