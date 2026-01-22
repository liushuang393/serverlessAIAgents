# -*- coding: utf-8 -*-
"""Decision Governance Engine - 字段级契约（Contract Schemas）.

目的:
    API連携向けに、意思決定結果を「固定フィールド + 追跡可能な証拠（Evidence）/主張（Claim）」として
    安定して返却するためのスキーマを提供する。

入出力:
    - 入力: DecisionReport 等から組み立てた dict / Pydantic オブジェクト
    - 出力: DecisionGovResponseV1（Pydantic Model）

注意点:
    - 既存の DecisionReport(v3.x) と後方互換を保つため、本契約は /api/decision の
      `format=v1` 指定時に利用する。
    - evidence/claims は段階的導入（外部情报抓取の接続前は空配列を許容）。
"""

from __future__ import annotations

from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class DecisionRole(str, Enum):
    """决策结论角色（固定 4 类）."""

    GO = "GO"
    NO_GO = "NO_GO"
    DELAY = "DELAY"
    PILOT = "PILOT"


class DecisionMode(str, Enum):
    """コスト/速度モード（将来拡張用）."""

    FAST = "FAST"
    STANDARD = "STANDARD"
    AUDIT = "AUDIT"


class EvidenceReliability(str, Enum):
    """外部情報ソースの信頼度（簡易）."""

    LOW = "LOW"
    MEDIUM = "MEDIUM"
    HIGH = "HIGH"


class ClaimType(str, Enum):
    """主張タイプ.

    - FACT: 外部証拠で裏取り可能な事実
    - INFERENCE: 複数事実/前提からの推論
    - ASSUMPTION: 証拠不足で仮定として扱うもの
    """

    FACT = "FACT"
    INFERENCE = "INFERENCE"
    ASSUMPTION = "ASSUMPTION"


class EvidenceItem(BaseModel):
    """Evidence（外部情报）.

    目的:
        レポート内の主張（Claim）が参照可能な根拠を、URL/抜粋/要約として保持する。
    """

    evidence_id: str = Field(..., description="証拠ID（参照用）")
    url: str = Field(..., description="参照URL")
    title: str = Field(default="", description="タイトル")
    publisher: str = Field(default="", description="発行元")
    retrieved_at: datetime = Field(..., description="取得日時")
    published_at: datetime | None = Field(default=None, description="公開日時（不明ならNone）")
    snippet: str = Field(default="", description="原文抜粋（短文推奨）")
    summary: str = Field(default="", description="要約")
    reliability: EvidenceReliability = Field(
        default=EvidenceReliability.MEDIUM, description="信頼度"
    )
    tags: list[str] = Field(default_factory=list, description="タグ")
    metadata: dict[str, Any] = Field(default_factory=dict, description="追加メタ情報")


class Claim(BaseModel):
    """Claim（结论/断言）.

    目的:
        レポートの重要な断言を、証拠ID（evidence_refs）で追跡可能にする。
    """

    claim_id: str = Field(..., description="主張ID")
    type: ClaimType = Field(..., description="主張タイプ")
    text: str = Field(..., description="主張本文")
    confidence: float = Field(default=0.5, ge=0.0, le=1.0, description="確信度")
    evidence_refs: list[str] = Field(default_factory=list, description="参照証拠ID一覧")
    related_section: str | None = Field(default=None, description="関連セクション（dao/fa/shu/qi等）")


class DecisionGovResponseV1(BaseModel):
    """字段级契约 v1（API 返却用）.

    目的:
        既存 DecisionReport の情報を、固定トップフィールド + Evidence/Claim に正規化して返却する。
    """

    schema_version: str = Field(default="dgov.response.v1", description="契約バージョン")
    report_id: str = Field(..., description="レポートID")
    request_id: str = Field(default="", description="リクエストID（未提供時は空）")
    mode: DecisionMode = Field(default=DecisionMode.STANDARD, description="処理モード")
    question: str = Field(default="", description="原質問")
    decision_role: DecisionRole = Field(..., description="决策结论角色")
    created_at: datetime = Field(..., description="生成日時")

    summary_bullets: list[str] = Field(default_factory=list, description="要点（最大10推奨）")

    # 既存の構造を保持しつつ、契約側では section payload を dict で返す（後方互換）
    dao: dict[str, Any] = Field(default_factory=dict, description="道（本質分析）")
    fa: dict[str, Any] = Field(default_factory=dict, description="法（戦略選定）")
    shu: dict[str, Any] = Field(default_factory=dict, description="術（実行計画）")
    qi: dict[str, Any] = Field(default_factory=dict, description="器（技術実装）")
    review: dict[str, Any] = Field(default_factory=dict, description="検証")

    evidence: list[EvidenceItem] = Field(default_factory=list, description="証拠プール")
    claims: list[Claim] = Field(default_factory=list, description="主張一覧")
    warnings: list[str] = Field(default_factory=list, description="警告")


__all__ = [
    "DecisionRole",
    "DecisionMode",
    "EvidenceReliability",
    "ClaimType",
    "EvidenceItem",
    "Claim",
    "DecisionGovResponseV1",
]
