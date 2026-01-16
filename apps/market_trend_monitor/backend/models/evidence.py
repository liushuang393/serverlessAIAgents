# -*- coding: utf-8 -*-
"""Evidence Ledger モデル定義.

証拠台帳システムのデータモデル。
全ての結論に対する証拠の追跡可能性を確保します。
"""

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field

from apps.market_trend_monitor.backend.models.schemas import SourceType


class ClaimLevel(str, Enum):
    """主張レベル.
    
    証拠数と信頼度に基づいて自動昇格します。
    - LEAD: 初期手がかり（証拠数1以上、信頼度<0.5）
    - HYPOTHESIS: 仮説（証拠数2以上、信頼度0.5-0.7）
    - FINDING: 発見（証拠数3以上、信頼度0.7-0.85）
    - CONCLUSION: 結論（証拠数5以上、信頼度>0.85）
    """

    LEAD = "lead"
    HYPOTHESIS = "hypothesis"
    FINDING = "finding"
    CONCLUSION = "conclusion"


# ============================================================
# Dataclass モデル（内部データ表現）
# ============================================================


@dataclass
class Evidence:
    """証拠データモデル.
    
    情報源から収集した証拠情報を表現します。
    """

    id: str
    source_id: str                           # 情報源ID
    source_type: SourceType                  # 情報源タイプ
    url: str
    title: str
    content_hash: str                        # 重複検出用ハッシュ
    extracted_data: dict[str, Any] = field(default_factory=dict)
    collected_at: datetime = field(default_factory=datetime.now)
    reliability_score: float = 0.5           # 信頼度スコア (0-1)
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "id": self.id,
            "source_id": self.source_id,
            "source_type": self.source_type.value,
            "url": self.url,
            "title": self.title,
            "content_hash": self.content_hash,
            "extracted_data": self.extracted_data,
            "collected_at": self.collected_at.isoformat(),
            "reliability_score": self.reliability_score,
            "metadata": self.metadata,
        }


@dataclass
class Claim:
    """主張データモデル.
    
    証拠に基づく主張を表現します。
    """

    id: str
    statement: str                           # 主張内容
    level: ClaimLevel = ClaimLevel.LEAD      # 主張レベル
    confidence: float = 0.0                  # 信頼度 (0-1)
    evidence_ids: list[str] = field(default_factory=list)
    counter_evidence_ids: list[str] = field(default_factory=list)
    created_at: datetime = field(default_factory=datetime.now)
    updated_at: datetime = field(default_factory=datetime.now)
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "id": self.id,
            "statement": self.statement,
            "level": self.level.value,
            "confidence": self.confidence,
            "evidence_ids": self.evidence_ids,
            "counter_evidence_ids": self.counter_evidence_ids,
            "created_at": self.created_at.isoformat(),
            "updated_at": self.updated_at.isoformat(),
            "metadata": self.metadata,
        }

    def update_level(self) -> None:
        """証拠数と信頼度に基づいて主張レベルを更新."""
        evidence_count = len(self.evidence_ids)
        
        if evidence_count >= 5 and self.confidence > 0.85:
            self.level = ClaimLevel.CONCLUSION
        elif evidence_count >= 3 and self.confidence >= 0.7:
            self.level = ClaimLevel.FINDING
        elif evidence_count >= 2 and self.confidence >= 0.5:
            self.level = ClaimLevel.HYPOTHESIS
        else:
            self.level = ClaimLevel.LEAD
        
        self.updated_at = datetime.now()


# ============================================================
# Pydantic スキーマ（API I/O用）
# ============================================================


class EvidenceSchema(BaseModel):
    """証拠スキーマ（API用）."""

    id: str
    source_id: str
    source_type: str
    url: str
    title: str
    content_hash: str
    extracted_data: dict = Field(default_factory=dict)
    collected_at: str
    reliability_score: float = 0.5
    metadata: dict = Field(default_factory=dict)


class ClaimSchema(BaseModel):
    """主張スキーマ（API用）."""

    id: str
    statement: str
    level: str
    confidence: float
    evidence_ids: list[str] = Field(default_factory=list)
    counter_evidence_ids: list[str] = Field(default_factory=list)
    created_at: str
    updated_at: str
    metadata: dict = Field(default_factory=dict)

