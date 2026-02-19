"""出力スキーマ定義 - 提案書.

日本式ビジネス提案書の最終出力フォーマットを定義。
v2.0: ClarificationOutput追加
v3.0: 本質導出・戦略的禁止事項・撤退基準のサマリー追加
v3.1: 提案書フォーマット対応（タイトル自動生成、署名欄強化）
"""

import hashlib
from datetime import datetime
from typing import Any

from apps.decision_governance_engine.schemas.agent_schemas import (
    ClarificationOutput,
    DaoOutput,
    FaOutput,
    QiOutput,
    ReviewOutput,
    ShuOutput,
)
from pydantic import BaseModel, Field


class HumanReview(BaseModel):
    """人間による最終確認情報."""

    requires_review: bool = Field(
        default=True,
        description="人間による最終確認が必要か",
    )
    approved: bool | None = Field(
        default=None,
        description="承認状態（None=未確認, True=承認, False=却下）",
    )
    reviewer_name: str | None = Field(
        default=None,
        description="確認者名",
    )
    reviewer_email: str | None = Field(
        default=None,
        description="確認者メールアドレス",
    )
    review_notes: str | None = Field(
        default=None,
        max_length=500,
        description="確認時のコメント",
    )
    reviewed_at: str | None = Field(
        default=None,
        description="確認日時（ISO 8601形式）",
    )


class ProposalTitle(BaseModel):
    """提案書タイトル.

    ユーザーの質問から自動生成される提案タイトル。
    日本語と英語の両方を含む。
    """

    title_ja: str = Field(
        ...,
        description="日本語タイトル（正式名称）",
    )
    title_en: str = Field(
        ...,
        description="英語タイトル（システム用案件名）",
    )
    case_id: str = Field(
        ...,
        description="案件ID（英数字）",
    )
    subtitle: str = Field(
        default="",
        description="サブタイトル（補足説明）",
    )


class SignatureBlock(BaseModel):
    """署名欄情報.

    提案書の承認フロー用署名欄。
    """

    # 作成者情報
    author_name: str = Field(default="", description="作成者名")
    author_department: str = Field(default="", description="作成者部署")
    author_position: str = Field(default="", description="作成者役職")
    created_date: str = Field(default="", description="作成日")

    # 承認者情報
    approver_name: str | None = Field(default=None, description="承認者名")
    approver_department: str | None = Field(default=None, description="承認者部署")
    approver_position: str | None = Field(default=None, description="承認者役職")
    approved_date: str | None = Field(default=None, description="承認日")

    # 署名状態
    is_signed: bool = Field(default=False, description="署名済みフラグ")
    signature_timestamp: datetime | None = Field(default=None, description="署名タイムスタンプ")


class ExecutiveSummary(BaseModel):
    """エグゼクティブサマリー v3.0."""

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

    # v3.0: 本質の一文（道層から）
    essence_statement: str = Field(
        default="",
        max_length=50,
        description="問題の本質（道層の結論）",
    )

    # v3.0: 戦略的禁止事項サマリー（法層から）
    strategic_prohibition_summary: str = Field(
        default="",
        max_length=50,
        description="最も重要な禁止事項（法層の結論）",
    )

    # v3.0: 撤退基準サマリー（術層から）
    exit_criteria_summary: str = Field(
        default="",
        max_length=50,
        description="撤退基準（術層の結論）",
    )


def generate_proposal_title(
    question: str,
    problem_type: str = "",
    now: datetime | None = None,
) -> ProposalTitle:
    """ユーザーの質問から提案書タイトルを生成.

    Args:
        question: ユーザーの質問文
        problem_type: 問題タイプ（オプション）
        now: 日時（省略時は現在時刻）

    Returns:
        ProposalTitle: 日本語/英語タイトルと案件ID
    """
    now = now or datetime.now()
    # キーワード抽出用パターン
    keywords = {
        # 技術系
        "システム": "System",
        "開発": "Development",
        "構築": "Construction",
        "導入": "Implementation",
        "移行": "Migration",
        "刷新": "Modernization",
        "クラウド": "Cloud",
        "AI": "AI",
        "DX": "DX",
        "自動化": "Automation",
        "セキュリティ": "Security",
        "インフラ": "Infrastructure",
        "ネットワーク": "Network",
        "データ": "Data",
        "分析": "Analytics",
        "プラットフォーム": "Platform",
        # ビジネス系
        "事業": "Business",
        "戦略": "Strategy",
        "改革": "Reform",
        "効率化": "Optimization",
        "コスト削減": "Cost Reduction",
        "売上": "Revenue",
        "拡大": "Expansion",
        "新規": "New",
        "グローバル": "Global",
        "国際": "International",
        "海外": "Overseas",
        # 音声・会議系
        "音声": "Voice",
        "会議": "Conference",
        "リアルタイム": "Real-time",
        "通信": "Communication",
        "コミュニケーション": "Communication",
    }

    # 質問からキーワードを抽出
    found_ja = []
    found_en = []
    for ja, en in keywords.items():
        if ja in question:
            found_ja.append(ja)
            found_en.append(en)

    # タイトル生成
    if found_ja:
        title_ja = f"{found_ja[0]}{'・' + found_ja[1] if len(found_ja) > 1 else ''}に関する提案"
        title_en = f"{found_en[0]}{'_' + found_en[1] if len(found_en) > 1 else ''}_Proposal"
    else:
        # デフォルト
        title_ja = "課題解決に関する提案"
        title_en = "Solution_Proposal"

    # より具体的なタイトル生成
    if "国際" in question or "グローバル" in question:
        if "音声" in question or "会議" in question:
            title_ja = "グローバル音声会議システム構築提案"
            title_en = "Global_Voice_Conference_System_Proposal"
        else:
            title_ja = "グローバル展開戦略提案"
            title_en = "Global_Expansion_Strategy_Proposal"

    if "リアルタイム" in question and "音声" in question:
        title_ja = "リアルタイム音声通信基盤構築提案"
        title_en = "Realtime_Voice_Communication_Platform_Proposal"

    # 案件ID生成（質問のハッシュから）
    hash_input = question + now.strftime("%Y%m%d")
    hash_value = hashlib.md5(hash_input.encode()).hexdigest()[:6].upper()
    case_id = f"PROP-{now.strftime('%Y%m')}-{hash_value}"

    # サブタイトル（問題タイプから）
    subtitle_map = {
        "TRADE_OFF": "選択肢比較・最適解の導出",
        "TIMING": "実施時期の最適化検討",
        "RESOURCE": "リソース配分の最適化",
        "RISK": "リスク軽減策の提示",
        "STRATEGY_DIRECTION": "戦略方向性の提言",
        "CONSTRAINT_DRIVEN": "制約条件下での解決策",
    }
    subtitle = subtitle_map.get(problem_type, "戦略的意思決定支援")

    return ProposalTitle(
        title_ja=title_ja,
        title_en=title_en,
        case_id=case_id,
        subtitle=subtitle,
    )


def generate_signature_block(
    user_info: dict[str, Any] | None = None,
    now: datetime | None = None,
) -> SignatureBlock:
    """署名欄を自動生成.

    Args:
        user_info: ユーザー情報（オプション）
        now: 日時（省略時は現在時刻）

    Returns:
        SignatureBlock: 署名欄情報
    """
    now = now or datetime.now()
    today = now.strftime("%Y年%m月%d日")

    if user_info:
        return SignatureBlock(
            author_name=user_info.get("display_name", ""),
            author_department=user_info.get("department", ""),
            author_position=user_info.get("position", ""),
            created_date=today,
        )

    return SignatureBlock(
        author_name="Decision Agent",
        author_department="AI Decision Support",
        author_position="AI Assistant",
        created_date=today,
    )


class DecisionReport(BaseModel):
    """提案書 v3.1 (旧: 決策レポート).

    全てのAgentの結果を統合した署名可能な提案書。
    日本のビジネス提案書フォーマットに準拠。

    v2.0: ClarificationAgent結果を追加
    v3.0: 本質導出・戦略的禁止事項・撤退基準の強化
    v3.1: 提案書フォーマット対応（タイトル自動生成、署名欄強化）
    """

    # メタ情報
    report_id: str = Field(..., description="提案書ID")
    created_at: datetime = Field(default_factory=datetime.now, description="作成日時")
    version: str = Field(default="3.1", description="バージョン")

    # v3.1: 提案書タイトル（自動生成）
    proposal_title: ProposalTitle | None = Field(
        default=None, description="提案書タイトル（日本語/英語/案件ID）"
    )

    # 元の質問（タイトル生成用）
    original_question: str = Field(default="", description="元の質問")

    # 提案先情報
    client_name: str = Field(default="御中", description="提案先（宛先）")

    # 各層の結果（v2.0: clarification追加）
    clarification: dict[str, Any] | ClarificationOutput | None = Field(
        default=None, description="ClarificationAgent結果（問題診断）"
    )
    dao: dict[str, Any] | DaoOutput = Field(..., description="DaoAgent結果（道 - 本質分析）")
    fa: dict[str, Any] | FaOutput = Field(..., description="FaAgent結果（法 - 戦略選定）")
    shu: dict[str, Any] | ShuOutput = Field(..., description="ShuAgent結果（術 - 実行計画）")
    qi: dict[str, Any] | QiOutput = Field(..., description="QiAgent結果（器 - 技術実装）")
    review: dict[str, Any] | ReviewOutput = Field(..., description="ReviewAgent結果（検証）")
    scoring: dict[str, Any] | None = Field(
        default=None,
        description="定量スコアリング結果（8次元評価）",
    )

    # エグゼクティブサマリー
    executive_summary: ExecutiveSummary = Field(..., description="エグゼクティブサマリー")

    # v3.1: 署名欄（強化版）
    signature_block: SignatureBlock | None = Field(default=None, description="署名欄情報")

    # v3.2: 人間確認フロー
    human_review: HumanReview = Field(
        default_factory=lambda: HumanReview(requires_review=True),
        description="人間による最終確認情報",
    )

    # 旧署名欄（後方互換性）
    signature_required: bool = Field(default=True, description="署名必須フラグ")
    signed_by: str | None = Field(default=None, description="署名者")
    signed_at: datetime | None = Field(default=None, description="署名日時")

    model_config = {
        "json_schema_extra": {
            "examples": [
                {
                    "report_id": "PROP-202601-ABC123",
                    "version": "3.1",
                    "proposal_title": {
                        "title_ja": "グローバル音声会議システム構築提案",
                        "title_en": "Global_Voice_Conference_System_Proposal",
                        "case_id": "PROP-202601-ABC123",
                        "subtitle": "制約条件下での解決策",
                    },
                    "executive_summary": {
                        "one_line_decision": "WebRTC標準準拠で段階構築",
                        "recommended_action": "3ヶ月でPoC実施",
                        "key_risks": ["規制対応遅延", "レイテンシ問題"],
                        "first_step": "対象国の通信規制マッピング",
                        "estimated_impact": "グローバル展開の基盤確立",
                    },
                    "signature_required": True,
                }
            ]
        }
    }
