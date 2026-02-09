"""入力スキーマ定義.

システム入力の最上位Schemaを定義する。
全てのフィールドは明示的に型定義し、バリデーションを行う。
"""

from typing import Literal

from pydantic import BaseModel, Field


class BudgetConstraint(BaseModel):
    """予算制約."""

    amount: float = Field(..., ge=0, description="予算金額（万円単位）")
    currency: str = Field(default="JPY", description="通貨コード")
    flexibility: Literal["FIXED", "FLEXIBLE", "NEGOTIABLE"] = Field(
        default="FLEXIBLE",
        description="柔軟性レベル",
    )


class TimelineConstraint(BaseModel):
    """期限制約."""

    months: int = Field(..., ge=1, le=120, description="期間（月数）")
    deadline_type: Literal["HARD", "SOFT", "PREFERRED"] = Field(
        default="SOFT",
        description="期限タイプ",
    )


class ConstraintSet(BaseModel):
    """制約条件セット."""

    budget: BudgetConstraint | None = Field(default=None, description="予算制約")
    timeline: TimelineConstraint | None = Field(default=None, description="期限制約")
    human_resources: list[str] = Field(default_factory=list, description="人的リソース")
    technical: list[str] = Field(default_factory=list, description="技術制約")
    regulatory: list[str] = Field(default_factory=list, description="規制・コンプライアンス")


class StakeholderInfo(BaseModel):
    """ステークホルダー（責任者）情報.

    提案書の責任分界を明確にするためのオプション情報。
    入力画面から任意で設定し、提案書の署名欄・責任者欄に反映される。
    """

    product_owner: str = Field(
        default="",
        max_length=100,
        description="プロダクトオーナー（事業価値・撤退判断の最終責任者）",
    )
    tech_lead: str = Field(
        default="",
        max_length=100,
        description="技術責任者（計測・実装の責任者）",
    )
    business_owner: str = Field(
        default="",
        max_length=100,
        description="事業責任者（予算・ROIの責任者）",
    )
    legal_reviewer: str = Field(
        default="",
        max_length=100,
        description="法務・コンプライアンス担当（規制・契約審査の責任者）",
    )


class RequesterInfo(BaseModel):
    """依頼者情報."""

    role: Literal["FOUNDER", "CEO", "EXECUTIVE", "MANAGER"] = Field(
        ...,
        description="役職",
    )
    decision_authority: bool = Field(
        default=True,
        description="最終決定権の有無",
    )
    organization_size: Literal["STARTUP", "SMB", "ENTERPRISE"] = Field(
        default="SMB",
        description="組織規模",
    )


class DecisionRequest(BaseModel):
    """システム入力の最上位Schema.

    このスキーマは意思決定依頼の全ての情報を構造化する。
    """

    question: str = Field(
        ...,
        min_length=10,
        max_length=2000,
        description="解決したい問題・意思決定事項",
    )
    constraints: ConstraintSet = Field(
        default_factory=ConstraintSet,
        description="制約条件",
    )
    requester: RequesterInfo | None = Field(
        default=None,
        description="依頼者情報",
    )
    stakeholders: StakeholderInfo | None = Field(
        default=None,
        description="ステークホルダー（責任者）情報（任意）",
    )
    additional_context: str | None = Field(
        default=None,
        max_length=5000,
        description="追加コンテキスト",
    )
    attachments: list[str] | None = Field(
        default=None,
        description="添付ファイルURL",
    )

    model_config = {
        "json_schema_extra": {
            "examples": [
                {
                    "question": "新規事業としてSaaSプロダクトを立ち上げるべきか、受託開発を強化すべきか判断したい",
                    "constraints": {
                        "budget": {"amount": 500, "currency": "JPY", "flexibility": "FLEXIBLE"},
                        "timeline": {"months": 6, "deadline_type": "SOFT"},
                        "technical": ["Python", "AWS"],
                        "regulatory": ["GDPR"],
                    },
                    "requester": {
                        "role": "CEO",
                        "decision_authority": True,
                        "organization_size": "STARTUP",
                    },
                    "stakeholders": {
                        "product_owner": "田中太郎",
                        "tech_lead": "鈴木一郎",
                        "business_owner": "佐藤花子",
                        "legal_reviewer": "",
                    },
                }
            ]
        }
    }

