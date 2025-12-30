# -*- coding: utf-8 -*-
"""Agent入出力スキーマ定義.

各Agentの入出力を厳密に型定義する。
全ての出力は構造化され、自由文テキストは禁止。
"""

from enum import Enum
from typing import Literal

from pydantic import BaseModel, Field


# =============================================================================
# 共通列挙型
# =============================================================================

class QuestionCategory(str, Enum):
    """問題カテゴリ分類."""

    # 受理可能（決策系）
    STRATEGIC_DECISION = "strategic_decision"
    RESOURCE_ALLOCATION = "resource_allocation"
    TRADE_OFF_CHOICE = "trade_off_choice"
    TIMING_JUDGMENT = "timing_judgment"
    RISK_EVALUATION = "risk_evaluation"
    PRIORITY_SETTING = "priority_setting"
    GO_NOGO_DECISION = "go_nogo_decision"

    # 拒否（非決策系）
    GENERAL_KNOWLEDGE = "general_knowledge"
    TECHNICAL_HOWTO = "technical_howto"
    SYSTEM_INQUIRY = "system_inquiry"
    CASUAL_CHAT = "casual_chat"
    FACTUAL_LOOKUP = "factual_lookup"
    OPINION_REQUEST = "opinion_request"
    CREATIVE_REQUEST = "creative_request"


class ProblemType(str, Enum):
    """問題タイプ分類."""

    RESOURCE_ALLOCATION = "RESOURCE_ALLOCATION"
    TIMING_DECISION = "TIMING_DECISION"
    TRADE_OFF = "TRADE_OFF"
    RISK_ASSESSMENT = "RISK_ASSESSMENT"
    STRATEGY_DIRECTION = "STRATEGY_DIRECTION"


class ReviewVerdict(str, Enum):
    """検証判定結果."""

    PASS = "PASS"
    REVISE = "REVISE"
    REJECT = "REJECT"


class FindingSeverity(str, Enum):
    """検証所見の重大度."""

    CRITICAL = "CRITICAL"
    WARNING = "WARNING"
    INFO = "INFO"


class FindingCategory(str, Enum):
    """検証所見のカテゴリ."""

    LOGIC_FLAW = "LOGIC_FLAW"
    OVER_OPTIMISM = "OVER_OPTIMISM"
    RESPONSIBILITY_GAP = "RESPONSIBILITY_GAP"
    RESOURCE_MISMATCH = "RESOURCE_MISMATCH"
    TIMELINE_UNREALISTIC = "TIMELINE_UNREALISTIC"


# =============================================================================
# GatekeeperAgent スキーマ
# =============================================================================

class GatekeeperInput(BaseModel):
    """GatekeeperAgent入力."""

    raw_question: str = Field(..., description="生の質問文")


class GatekeeperOutput(BaseModel):
    """GatekeeperAgent出力."""

    is_acceptable: bool = Field(..., description="受理可否")
    category: QuestionCategory = Field(..., description="分類結果")
    confidence: float = Field(..., ge=0.0, le=1.0, description="判定確信度")
    rejection_reason: str | None = Field(default=None, description="拒否理由")
    rejection_message: str | None = Field(default=None, description="ユーザー向けメッセージ")
    suggested_rephrase: str | None = Field(default=None, description="言い換え提案")


# =============================================================================
# DaoAgent スキーマ
# =============================================================================

class DaoInput(BaseModel):
    """DaoAgent入力."""

    question: str = Field(..., description="原始質問（Gatekeeper通過済み）")
    constraints: list[str] = Field(default_factory=list, description="現実制約")
    stakeholders: list[str] = Field(default_factory=list, description="関係者")
    gatekeeper_result: GatekeeperOutput | None = Field(default=None, description="入口検証結果")


class DaoOutput(BaseModel):
    """DaoAgent出力."""

    problem_type: ProblemType = Field(..., description="問題タイプ")
    essence: str = Field(..., max_length=50, description="一文での本質（50字以内）")
    immutable_constraints: list[str] = Field(..., max_length=5, description="不可変制約（max 5）")
    hidden_assumptions: list[str] = Field(..., max_length=3, description="隠れた前提（max 3）")


# =============================================================================
# FaAgent スキーマ
# =============================================================================

class PathOption(BaseModel):
    """戦略パスオプション."""

    path_id: str = Field(..., description="パスID")
    name: str = Field(..., max_length=10, description="パス名（10字以内）")
    description: str = Field(..., max_length=100, description="説明（100字以内）")
    pros: list[str] = Field(..., max_length=3, description="メリット（max 3）")
    cons: list[str] = Field(..., max_length=3, description="デメリット（max 3）")
    success_probability: float = Field(..., ge=0.0, le=1.0, description="成功確率")


class FaInput(BaseModel):
    """FaAgent入力."""

    dao_result: DaoOutput = Field(..., description="DaoAgent結果")
    available_resources: dict = Field(default_factory=dict, description="利用可能リソース")
    time_horizon: str = Field(default="", description="時間軸")


class FaOutput(BaseModel):
    """FaAgent出力."""

    recommended_paths: list[PathOption] = Field(..., max_length=2, description="推奨パス（1-2個）")
    rejected_paths: list[PathOption] = Field(default_factory=list, description="明示的に不推奨")
    decision_criteria: list[str] = Field(..., description="判断基準")


# =============================================================================
# ShuAgent スキーマ
# =============================================================================

class ActionPhase(BaseModel):
    """実行フェーズ."""

    phase_number: int = Field(..., ge=1, description="フェーズ番号")
    name: str = Field(..., description="フェーズ名")
    duration: str = Field(..., description="期間（例：2週間、1ヶ月）")
    actions: list[str] = Field(..., max_length=5, description="具体的行動（max 5）")
    deliverables: list[str] = Field(default_factory=list, description="成果物")
    success_criteria: list[str] = Field(default_factory=list, description="完了条件")


class ShuInput(BaseModel):
    """ShuAgent入力."""

    fa_result: FaOutput = Field(..., description="FaAgent結果")
    selected_path_id: str = Field(..., description="選択されたパスID")


class ShuOutput(BaseModel):
    """ShuAgent出力."""

    phases: list[ActionPhase] = Field(..., min_length=3, max_length=5, description="3-5フェーズ")
    first_action: str = Field(..., description="最初の一歩（明日できること）")
    dependencies: list[str] = Field(default_factory=list, description="前提条件")


# =============================================================================
# QiAgent スキーマ
# =============================================================================

class Implementation(BaseModel):
    """技術実装要素."""

    component: str = Field(..., description="コンポーネント名")
    technology: str = Field(..., description="使用技術")
    estimated_effort: str = Field(..., description="見積もり工数")
    risks: list[str] = Field(default_factory=list, description="技術リスク")


class QiInput(BaseModel):
    """QiAgent入力."""

    shu_result: ShuOutput = Field(..., description="ShuAgent結果")
    tech_constraints: list[str] = Field(default_factory=list, description="技術制約")


class QiOutput(BaseModel):
    """QiAgent出力."""

    implementations: list[Implementation] = Field(..., description="実装要素")
    tool_recommendations: list[str] = Field(default_factory=list, description="ツール推奨")
    integration_points: list[str] = Field(default_factory=list, description="統合ポイント")
    technical_debt_warnings: list[str] = Field(default_factory=list, description="技術負債警告")


# =============================================================================
# ReviewAgent スキーマ
# =============================================================================

class ReviewFinding(BaseModel):
    """検証所見."""

    severity: FindingSeverity = Field(..., description="重大度")
    category: FindingCategory = Field(..., description="カテゴリ")
    description: str = Field(..., description="説明")
    affected_agent: str = Field(..., description="影響を受けるAgent")
    suggested_revision: str = Field(..., description="修正提案")


class ReviewInput(BaseModel):
    """ReviewAgent入力."""

    dao_result: DaoOutput = Field(..., description="DaoAgent結果")
    fa_result: FaOutput = Field(..., description="FaAgent結果")
    shu_result: ShuOutput = Field(..., description="ShuAgent結果")
    qi_result: QiOutput = Field(..., description="QiAgent結果")


class ReviewOutput(BaseModel):
    """ReviewAgent出力."""

    overall_verdict: ReviewVerdict = Field(..., description="総合判定")
    findings: list[ReviewFinding] = Field(default_factory=list, description="検証所見")
    confidence_score: float = Field(..., ge=0.0, le=1.0, description="信頼度スコア")
    final_warnings: list[str] = Field(default_factory=list, description="最終警告")

