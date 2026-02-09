"""Prediction Review モデル定義.

予測復盤システムのデータモデル。
予測精度評価と学習フィードバックを提供します。
"""

from dataclasses import dataclass, field
from datetime import date, datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class PredictionStatus(str, Enum):
    """予測ステータス.

    予測の結果状態を表現します。
    - PENDING: 期限未到来
    - CORRECT: 的中
    - PARTIAL: 部分的中
    - INCORRECT: 外れ
    """

    PENDING = "pending"
    CORRECT = "correct"
    PARTIAL = "partial"
    INCORRECT = "incorrect"


class PredictionOutcome(str, Enum):
    """予測結果判定.

    復盤時の結果判定を表現します。
    """

    CORRECT = "correct"
    PARTIAL = "partial"
    INCORRECT = "incorrect"
    UNKNOWN = "unknown"


# ============================================================
# Dataclass モデル（内部データ表現）
# ============================================================


@dataclass
class Prediction:
    """予測データモデル.

    市場動向に対する予測とその結果を表現します。
    """

    id: str
    statement: str                           # 予測内容
    target_date: date                        # 予測対象日
    confidence: float                        # 予測時信頼度 (0-1)
    claim_id: str | None = None              # 関連主張ID
    created_at: datetime = field(default_factory=datetime.now)
    status: PredictionStatus = PredictionStatus.PENDING
    actual_outcome: str | None = None        # 実際の結果
    review_note: str | None = None           # レビューメモ
    reviewed_at: datetime | None = None      # レビュー日時
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "id": self.id,
            "statement": self.statement,
            "target_date": self.target_date.isoformat(),
            "confidence": self.confidence,
            "claim_id": self.claim_id,
            "created_at": self.created_at.isoformat(),
            "status": self.status.value,
            "actual_outcome": self.actual_outcome,
            "review_note": self.review_note,
            "reviewed_at": (
                self.reviewed_at.isoformat() if self.reviewed_at else None
            ),
            "metadata": self.metadata,
        }

    def is_due(self) -> bool:
        """レビュー期限が到来しているか確認."""
        return date.today() >= self.target_date

    def review(
        self,
        status: PredictionStatus,
        actual_outcome: str,
        review_note: str | None = None,
    ) -> None:
        """予測をレビューして結果を記録."""
        self.status = status
        self.actual_outcome = actual_outcome
        self.review_note = review_note
        self.reviewed_at = datetime.now()


@dataclass
class PredictionReview:
    """予測復盤結果データモデル.

    予測と実際の結果の比較結果を表現します。
    """

    id: str
    prediction_id: str
    actual_outcome: str
    outcome: PredictionOutcome
    accuracy_score: float
    reviewed_at: datetime = field(default_factory=datetime.now)
    notes: str = ""

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "id": self.id,
            "prediction_id": self.prediction_id,
            "actual_outcome": self.actual_outcome,
            "outcome": self.outcome.value,
            "accuracy_score": self.accuracy_score,
            "reviewed_at": self.reviewed_at.isoformat(),
            "notes": self.notes,
        }


@dataclass
class PredictionAccuracy:
    """予測精度統計データモデル.

    一定期間の予測精度を集計します。
    """

    total_count: int = 0
    correct_count: int = 0
    partial_count: int = 0
    incorrect_count: int = 0
    pending_count: int = 0
    period_start: date | None = None
    period_end: date | None = None

    @property
    def accuracy_rate(self) -> float:
        """正解率を計算（PENDINGを除く）."""
        reviewed = self.total_count - self.pending_count
        if reviewed == 0:
            return 0.0
        # 部分的中は0.5としてカウント
        correct_weighted = self.correct_count + (self.partial_count * 0.5)
        return correct_weighted / reviewed

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "total_count": self.total_count,
            "correct_count": self.correct_count,
            "partial_count": self.partial_count,
            "incorrect_count": self.incorrect_count,
            "pending_count": self.pending_count,
            "accuracy_rate": self.accuracy_rate,
            "period_start": (
                self.period_start.isoformat() if self.period_start else None
            ),
            "period_end": (
                self.period_end.isoformat() if self.period_end else None
            ),
        }


# ============================================================
# Pydantic スキーマ（API I/O用）
# ============================================================


class PredictionSchema(BaseModel):
    """予測スキーマ（API用）."""

    id: str
    statement: str
    target_date: str
    confidence: float = Field(ge=0.0, le=1.0)
    claim_id: str | None = None
    created_at: str
    status: str
    actual_outcome: str | None = None
    review_note: str | None = None
    reviewed_at: str | None = None
    metadata: dict = Field(default_factory=dict)


class PredictionReviewInput(BaseModel):
    """予測レビュー入力スキーマ."""

    status: str
    actual_outcome: str
    review_note: str | None = None


class PredictionAccuracySchema(BaseModel):
    """予測精度スキーマ."""

    total_count: int = 0
    correct_count: int = 0
    partial_count: int = 0
    incorrect_count: int = 0
    pending_count: int = 0
    accuracy_rate: float = 0.0
    period_start: str | None = None
    period_end: str | None = None


class PredictionReviewSchema(BaseModel):
    """予測復盤結果スキーマ."""

    id: str
    prediction_id: str
    actual_outcome: str
    outcome: str
    accuracy_score: float = Field(ge=0.0, le=1.0)
    reviewed_at: str
    notes: str = ""
