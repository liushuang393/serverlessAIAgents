# -*- coding: utf-8 -*-
"""Signal Scoring モデル定義.

信号評価システムのデータモデル。
5軸評価体系に基づく統一的な信号強度評価を提供します。
"""

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class SignalGrade(str, Enum):
    """信号グレード.
    
    スコアに基づく信号強度の分類:
    - A (強信号): ≥4.0 - 高確度で行動可能
    - B (中信号): 3.0-3.9 - 注視すべき兆候
    - C (弱信号): 2.0-2.9 - 参考レベル
    - D (雑音): <2.0 - ノイズ
    """

    A = "A"  # 強信号
    B = "B"  # 中信号
    C = "C"  # 弱信号
    D = "D"  # 雑音


# ============================================================
# Dataclass モデル（内部データ表現）
# ============================================================


@dataclass
class SignalScore:
    """5軸信号スコア.
    
    各軸は0-1の範囲で評価されます。
    """

    reliability: float = 0.0      # 信頼性：情報源の信頼度
    leading: float = 0.0          # 先行性：市場先行度
    relevance: float = 0.0        # 関連性：ドメイン関連度
    actionability: float = 0.0    # 実行可能性：行動可能性
    convergence: float = 0.0      # 収束性：多ソース一致度

    @property
    def total(self) -> float:
        """合計スコア (0-5)."""
        return (
            self.reliability
            + self.leading
            + self.relevance
            + self.actionability
            + self.convergence
        )

    @property
    def grade(self) -> SignalGrade:
        """スコアに基づくグレード判定."""
        total = self.total
        if total >= 4.0:
            return SignalGrade.A
        elif total >= 3.0:
            return SignalGrade.B
        elif total >= 2.0:
            return SignalGrade.C
        return SignalGrade.D

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "reliability": self.reliability,
            "leading": self.leading,
            "relevance": self.relevance,
            "actionability": self.actionability,
            "convergence": self.convergence,
            "total": self.total,
            "grade": self.grade.value,
        }


@dataclass
class Signal:
    """信号データモデル.
    
    トレンドに対する信号評価結果を表現します。
    """

    id: str
    trend_id: str
    score: SignalScore = field(default_factory=SignalScore)
    evaluated_at: datetime = field(default_factory=datetime.now)
    metadata: dict[str, Any] = field(default_factory=dict)

    @property
    def grade(self) -> SignalGrade:
        """信号グレードを取得."""
        return self.score.grade

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "id": self.id,
            "trend_id": self.trend_id,
            "score": self.score.to_dict(),
            "grade": self.grade.value,
            "evaluated_at": self.evaluated_at.isoformat(),
            "metadata": self.metadata,
        }


# ============================================================
# Pydantic スキーマ（API I/O用）
# ============================================================


class SignalScoreSchema(BaseModel):
    """5軸信号スコアスキーマ."""

    reliability: float = Field(ge=0.0, le=1.0, default=0.0)
    leading: float = Field(ge=0.0, le=1.0, default=0.0)
    relevance: float = Field(ge=0.0, le=1.0, default=0.0)
    actionability: float = Field(ge=0.0, le=1.0, default=0.0)
    convergence: float = Field(ge=0.0, le=1.0, default=0.0)
    total: float = Field(ge=0.0, le=5.0, default=0.0)
    grade: str = "D"


class SignalSchema(BaseModel):
    """信号スキーマ（API用）."""

    id: str
    trend_id: str
    score: SignalScoreSchema
    grade: str
    evaluated_at: str
    metadata: dict = Field(default_factory=dict)

