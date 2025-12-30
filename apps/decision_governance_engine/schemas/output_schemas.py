# -*- coding: utf-8 -*-
"""出力スキーマ定義.

最終レポートとエグゼクティブサマリーを定義する。
"""

from datetime import datetime

from pydantic import BaseModel, Field

from apps.decision_governance_engine.schemas.agent_schemas import (
    DaoOutput,
    FaOutput,
    QiOutput,
    ReviewOutput,
    ShuOutput,
)


class ExecutiveSummary(BaseModel):
    """エグゼクティブサマリー."""

    one_line_decision: str = Field(
        ...,
        max_length=30,
        description="一文結論（30字以内）",
    )
    recommended_action: str = Field(
        ...,
        description="推奨アクション",
    )
    key_risks: list[str] = Field(
        ...,
        max_length=3,
        description="主要リスク（max 3）",
    )
    first_step: str = Field(
        ...,
        description="最初の一歩",
    )
    estimated_impact: str = Field(
        ...,
        description="期待効果",
    )


class DecisionReport(BaseModel):
    """最終出力レポート.

    全てのAgentの結果を統合した署名可能なレポート。
    """

    # メタ情報
    report_id: str = Field(..., description="レポートID")
    created_at: datetime = Field(default_factory=datetime.now, description="作成日時")
    version: str = Field(default="1.0", description="バージョン")

    # 各層の結果
    dao: DaoOutput = Field(..., description="DaoAgent結果")
    fa: FaOutput = Field(..., description="FaAgent結果")
    shu: ShuOutput = Field(..., description="ShuAgent結果")
    qi: QiOutput = Field(..., description="QiAgent結果")
    review: ReviewOutput = Field(..., description="ReviewAgent結果")

    # エグゼクティブサマリー
    executive_summary: ExecutiveSummary = Field(..., description="エグゼクティブサマリー")

    # 署名欄
    signature_required: bool = Field(default=True, description="署名必須フラグ")
    signed_by: str | None = Field(default=None, description="署名者")
    signed_at: datetime | None = Field(default=None, description="署名日時")

    model_config = {
        "json_schema_extra": {
            "examples": [
                {
                    "report_id": "DGE-2024-001",
                    "version": "1.0",
                    "executive_summary": {
                        "one_line_decision": "B案（SaaS開発）を選択すべき",
                        "recommended_action": "6ヶ月以内にMVP検証を実施",
                        "key_risks": ["キャッシュフロー圧迫", "開発リソース競合"],
                        "first_step": "プロダクトチームとのキックオフMTG設定",
                        "estimated_impact": "3年後の企業価値2倍",
                    },
                    "signature_required": True,
                }
            ]
        }
    }

