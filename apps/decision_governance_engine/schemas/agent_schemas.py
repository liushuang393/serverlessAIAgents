"""Agent入出力スキーマ定義.

各Agentの入出力を厳密に型定義する。
全ての出力は構造化され、自由文テキストは禁止。
"""

from enum import Enum

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
# CognitiveGateAgent スキーマ（認知前処理）
# =============================================================================


class IrreversibilityLevel(str, Enum):
    """不可逆性レベル."""

    HIGH = "HIGH"      # 一度決めたら取り消し困難
    MEDIUM = "MEDIUM"  # 取り消し可能だがコスト大
    LOW = "LOW"        # 容易に取り消し可能


class Irreversibility(BaseModel):
    """不可逆性評価."""

    level: IrreversibilityLevel = Field(..., description="不可逆性レベル")
    description: str = Field(..., max_length=50, description="不可逆性の説明")


class CognitiveGateInput(BaseModel):
    """CognitiveGateAgent入力."""

    raw_question: str = Field(..., description="ユーザーの質問")
    constraints: list[str] = Field(default_factory=list, description="制約条件")
    clarification_result: "ClarificationOutput | None" = Field(
        default=None, description="Clarification結果（あれば）"
    )


class CognitiveGateOutput(BaseModel):
    """CognitiveGateAgent出力."""

    evaluation_object: str = Field(
        ..., max_length=50, description="評価対象（何を判断するか）"
    )
    intent: str = Field(
        ..., max_length=100, description="判断目的・動機"
    )
    criteria: list[str] = Field(
        ..., min_length=1, max_length=5, description="評価軸"
    )
    irreversibility: Irreversibility = Field(..., description="不可逆性評価")
    proceed: bool = Field(..., description="分析進行可否（True=GO, False=STOP）")
    missing_info: list[str] = Field(
        default_factory=list, max_length=3, description="不足情報"
    )
    clarification_questions: list[str] = Field(
        default_factory=list, max_length=3, description="追加質問"
    )


# =============================================================================
# ClarificationAgent スキーマ（問題診断/澄清）
# =============================================================================

class Ambiguity(BaseModel):
    """曖昧な点."""

    point: str = Field(..., description="曖昧な点")
    clarification_needed: str = Field(..., description="明確化が必要な内容")


class HiddenAssumption(BaseModel):
    """暗黙の仮定."""

    assumption: str = Field(..., description="暗黙の仮定")
    validity_question: str = Field(..., description="この仮定は成り立つか？")


class CognitiveBias(BaseModel):
    """認知バイアス."""

    bias: str = Field(..., description="バイアス名")
    manifestation: str = Field(..., description="この質問でどう現れているか")


class ClarificationInput(BaseModel):
    """ClarificationAgent入力."""

    raw_question: str = Field(..., description="ユーザーの原始質問（Gatekeeper通過済み）")
    constraints: list[str] = Field(default_factory=list, description="現実制約（あれば）")
    gatekeeper_result: "GatekeeperOutput | None" = Field(default=None, description="入口検証結果")


class ClarificationOutput(BaseModel):
    """ClarificationAgent出力."""

    restated_question: str = Field(
        ..., max_length=100, description="一文で正確に復唱した質問"
    )
    ambiguities: list[Ambiguity] = Field(
        default_factory=list, max_length=3, description="質問の曖昧な点（最大3つ）"
    )
    hidden_assumptions: list[HiddenAssumption] = Field(
        default_factory=list, max_length=3, description="暗黙の仮定（最大3つ）"
    )
    cognitive_biases: list[CognitiveBias] = Field(
        default_factory=list, max_length=2, description="認知バイアス（最大2つ）"
    )
    refined_question: str = Field(..., description="診断後の精緻化された質問")
    diagnosis_confidence: float = Field(
        ..., ge=0.0, le=1.0, description="診断の確信度"
    )


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
# DaoAgent スキーマ（本質分析 + 因果齿轮 + 死穴分析 + 制約主導型分析）
# =============================================================================


class ProblemNatureType(str, Enum):
    """問題の本質的性質（v3.0: 制約主導型分析）.

    単なるカテゴリ分類ではなく、問題の「存在理由」を分類する。
    """

    TECHNICAL_LIMITATION = "TECHNICAL_LIMITATION"  # 技術的に解決可能な問題
    INVESTMENT_DECISION = "INVESTMENT_DECISION"    # ROI/リソース配分の判断
    CONSTRAINT_DRIVEN = "CONSTRAINT_DRIVEN"        # 既存解が使えない制約主導型
    STRATEGIC_CHOICE = "STRATEGIC_CHOICE"          # 方向性・ビジョンの選択
    REGULATORY_COMPLIANCE = "REGULATORY_COMPLIANCE"  # 規制対応が主因の問題
    MARKET_TIMING = "MARKET_TIMING"                # タイミングが本質の問題


class ExistingAlternative(BaseModel):
    """既存代替手段の分析（なぜ使えないか）."""

    name: str = Field(..., max_length=30, description="既存解の名称（例: Zoom, Teams）")
    why_not_viable: str = Field(..., max_length=100, description="なぜこの企業では使えないか")
    specific_constraint: str = Field(..., max_length=50, description="具体的な制約（法規制/技術/コスト）")


class EssenceDerivation(BaseModel):
    """本質導出プロセス（思考の可視化）.

    問題の本質を「一言で表現する」前に、なぜその結論に至ったかを構造化。
    """

    surface_problem: str = Field(..., max_length=50, description="表面的な問題（ユーザーが言ったこと）")
    underlying_why: str = Field(..., max_length=100, description="なぜそれが問題なのか（一段深い理由）")
    root_constraint: str = Field(..., max_length=100, description="根本的な制約（これがなければ問題ではない）")
    essence_statement: str = Field(..., max_length=50, description="本質の一文（非これ不可）")


class LeverageLevel(str, Enum):
    """レバレッジ効果レベル."""

    HIGH = "HIGH"
    MEDIUM = "MEDIUM"
    LOW = "LOW"


class DeathTrapSeverity(str, Enum):
    """死穴の深刻度."""

    FATAL = "FATAL"      # 致命的 - これをやったら終わり
    SEVERE = "SEVERE"    # 重大 - 大きなダメージ
    MODERATE = "MODERATE"  # 中程度 - 回復可能だが痛い


class CausalGear(BaseModel):
    """因果齿轮（構造モジュール）."""

    gear_id: int = Field(..., ge=1, le=5, description="齿轮ID（1-5）")
    name: str = Field(..., max_length=20, description="齿轮名（20字以内）")
    description: str = Field(..., max_length=100, description="内容描述（100字以内）")
    drives: list[int] = Field(default_factory=list, description="駆動する齿轮ID（因→果）")
    driven_by: list[int] = Field(default_factory=list, description="駆動される齿轮ID（果←因）")
    leverage: LeverageLevel = Field(default=LeverageLevel.MEDIUM, description="レバレッジ効果")


class DeathTrap(BaseModel):
    """死穴（絶対にやってはいけないこと）."""

    action: str = Field(..., description="禁止行動")
    reason: str = Field(..., description="なぜ致命的か")
    severity: DeathTrapSeverity = Field(..., description="深刻度")


class DaoInput(BaseModel):
    """DaoAgent入力."""

    question: str = Field(..., description="原始質問（Gatekeeper通過済み）")
    constraints: list[str] = Field(default_factory=list, description="現実制約")
    stakeholders: list[str] = Field(default_factory=list, description="関係者")
    gatekeeper_result: GatekeeperOutput | None = Field(default=None, description="入口検証結果")
    clarification_result: ClarificationOutput | None = Field(default=None, description="問題診断結果")


class DaoOutput(BaseModel):
    """DaoAgent出力（v3.0 制約主導型分析対応）."""

    # 基本分析
    problem_type: ProblemType = Field(..., description="問題タイプ")
    essence: str = Field(..., max_length=50, description="一文での本質（50字以内）")
    immutable_constraints: list[str] = Field(..., max_length=5, description="不可変制約（max 5）")
    hidden_assumptions: list[str] = Field(..., max_length=3, description="隠れた前提（max 3）")

    # v3.0: 本質導出プロセス（思考の可視化）
    essence_derivation: EssenceDerivation | None = Field(
        default=None,
        description="本質導出プロセス（なぜこの結論に至ったか）",
    )

    # v3.0: 問題の本質的性質（制約主導型分析）
    problem_nature: ProblemNatureType | None = Field(
        default=None,
        description="問題の本質的性質（技術/投資/制約主導/戦略）",
    )

    # v3.0: 既存代替手段の分析
    existing_alternatives: list[ExistingAlternative] = Field(
        default_factory=list,
        max_length=3,
        description="既存の代替手段とそれが使えない理由（max 3）",
    )

    # 因果齿轮（構造拆解）
    causal_gears: list[CausalGear] = Field(
        default_factory=list,
        min_length=0,
        max_length=5,
        description="因果齿轮（3-5个互相咬合的結構模块）",
    )
    bottleneck_gear: int | None = Field(
        default=None, ge=1, le=5, description="关键瓶颈齿轮ID"
    )

    # 死穴分析（核心判断）
    death_traps: list[DeathTrap] = Field(
        default_factory=list,
        max_length=3,
        description="死穴（現阶段绝对不能做的事，max 3）",
    )


# =============================================================================
# FaAgent スキーマ（v2.0: 稳健型 vs 激进型対比）
# =============================================================================

class StrategyType(str, Enum):
    """戦略タイプ（稳健型 vs 激进型）."""

    CONSERVATIVE = "CONSERVATIVE"  # 稳健型：低リスク、慢回報、可控性高
    AGGRESSIVE = "AGGRESSIVE"      # 激进型：高リスク、快回報、不确定性大
    BALANCED = "BALANCED"          # バランス型：中間


class ReversibilityLevel(str, Enum):
    """可逆性レベル."""

    HIGH = "HIGH"      # 高：やり直し可能、ピボットしやすい
    MEDIUM = "MEDIUM"  # 中：部分的に可逆
    LOW = "LOW"        # 低：一度決めたら変更困難


class PathOption(BaseModel):
    """戦略パスオプション v2.0."""

    # 基本情報
    path_id: str = Field(..., description="パスID")
    name: str = Field(..., max_length=10, description="パス名（10字以内）")
    description: str = Field(..., max_length=100, description="説明（100字以内）")

    # v2.0: 戦略タイプ分類
    strategy_type: StrategyType = Field(
        default=StrategyType.BALANCED,
        description="戦略タイプ（CONSERVATIVE/AGGRESSIVE/BALANCED）",
    )

    # メリット・デメリット
    pros: list[str] = Field(..., max_length=3, description="メリット（max 3）")
    cons: list[str] = Field(..., max_length=3, description="デメリット（max 3）")

    # v2.0: 詳細評価
    suitable_conditions: list[str] = Field(
        default_factory=list,
        max_length=3,
        description="適用条件（このパスが有効な条件、max 3）",
    )
    risks: list[str] = Field(
        default_factory=list,
        max_length=3,
        description="主要リスク（max 3）",
    )
    costs: list[str] = Field(
        default_factory=list,
        max_length=3,
        description="コスト（金銭・時間・機会、max 3）",
    )
    time_to_value: str = Field(
        default="",
        description="価値実現までの時間（例：3ヶ月、6ヶ月）",
    )
    reversibility: ReversibilityLevel = Field(
        default=ReversibilityLevel.MEDIUM,
        description="可逆性（HIGH/MEDIUM/LOW）",
    )

    # 成功確率
    success_probability: float = Field(..., ge=0.0, le=1.0, description="成功確率")


class PathComparisonMatrix(BaseModel):
    """パス比較マトリックス（稳健 vs 激进の可視化）."""

    dimensions: list[str] = Field(
        default_factory=lambda: ["ROI", "リスク", "時間", "可逆性", "リソース効率"],
        description="比較軸（5項目推奨）",
    )
    scores: dict[str, list[int]] = Field(
        default_factory=dict,
        description="パスIDごとのスコア（各軸1-5点）",
    )
    recommendation_summary: str = Field(
        default="",
        max_length=200,
        description="比較サマリー（200字以内）",
    )


class StrategicProhibition(BaseModel):
    """戦略的禁止事項（v3.0: 法層の核心）.

    「何をすべきか」ではなく「何を絶対にしてはいけないか」を定義。
    """

    prohibition: str = Field(..., max_length=50, description="禁止事項（例: 独自プロトコル開発禁止）")
    rationale: str = Field(..., max_length=100, description="なぜ禁止するか（戦略的理由）")
    violation_consequence: str = Field(..., max_length=50, description="違反した場合の結果")


class DifferentiationAxis(BaseModel):
    """差別化軸（v3.0: 真の競争優位）.

    「音質」ではなく「法規制対応能力」のように、
    本当の差別化ポイントを明確化。
    """

    axis_name: str = Field(..., max_length=30, description="差別化軸名")
    why_this_axis: str = Field(..., max_length=100, description="なぜこの軸で差別化するか")
    not_this_axis: str = Field(..., max_length=50, description="差別化しない軸（例: 音質では勝負しない）")


class FaInput(BaseModel):
    """FaAgent入力."""

    dao_result: DaoOutput = Field(..., description="DaoAgent結果")
    available_resources: dict = Field(default_factory=dict, description="利用可能リソース")
    time_horizon: str = Field(default="", description="時間軸")


class FaOutput(BaseModel):
    """FaAgent出力 v3.0（戦略的禁止事項・差別化軸対応）."""

    # 推奨・不推奨パス
    recommended_paths: list[PathOption] = Field(..., max_length=2, description="推奨パス（1-2個）")
    rejected_paths: list[PathOption] = Field(default_factory=list, description="明示的に不推奨")

    # 判断基準
    decision_criteria: list[str] = Field(..., description="判断基準")

    # v2.0: 比較マトリックス
    path_comparison: PathComparisonMatrix | None = Field(
        default=None,
        description="パス比較マトリックス（稳健 vs 激进の対比）",
    )

    # v3.0: 戦略的禁止事項（法の核心）
    strategic_prohibitions: list[StrategicProhibition] = Field(
        default_factory=list,
        max_length=3,
        description="戦略的禁止事項（絶対にやってはいけないこと、max 3）",
    )

    # v3.0: 差別化軸
    differentiation_axis: DifferentiationAxis | None = Field(
        default=None,
        description="差別化軸（どこで勝負するか）",
    )

    # v3.0: 既存解が使えない理由のサマリー
    why_existing_fails: str = Field(
        default="",
        max_length=100,
        description="既存の標準解が使えない理由（一文）",
    )


# =============================================================================
# ShuAgent スキーマ
# =============================================================================


class RhythmPeriod(str, Enum):
    """節奏周期."""

    WEEK_1 = "WEEK_1"       # 1週間
    WEEK_2 = "WEEK_2"       # 2週間
    MONTH_1 = "MONTH_1"     # 1ヶ月（30日）
    MONTH_3 = "MONTH_3"     # 3ヶ月（90日）


class FocusArea(BaseModel):
    """聚焦领域 - 30天内只做这一件事."""

    name: str = Field(..., max_length=20, description="聚焦名称（20字以内）")
    description: str = Field(..., max_length=100, description="具体说明（100字以内）")
    success_metric: str = Field(..., description="成功指标（可量化）")
    avoid_list: list[str] = Field(
        default_factory=list,
        max_length=3,
        description="这段时间要避免的事（max 3）",
    )


class RhythmControl(BaseModel):
    """行动节奏控制 - 精准的行动节奏.

    原则: "接下来30天，只做这一件事"
    """

    period: RhythmPeriod = Field(
        default=RhythmPeriod.MONTH_1,
        description="节奏周期（默认30天）",
    )
    focus: FocusArea = Field(..., description="当前阶段的唯一聚焦点")
    checkpoint_date: str = Field(
        default="",
        description="检查点日期（例: '30天后'、'2025-02-01'）",
    )
    checkpoint_criteria: list[str] = Field(
        default_factory=list,
        max_length=3,
        description="检查点评估标准（max 3）",
    )
    next_decision_point: str = Field(
        default="",
        max_length=100,
        description="下一个决策点描述（100字以内）",
    )


class ActionPhase(BaseModel):
    """実行フェーズ."""

    phase_number: int = Field(..., ge=1, description="フェーズ番号")
    name: str = Field(..., description="フェーズ名")
    duration: str = Field(..., description="期間（例：2週間、1ヶ月）")
    actions: list[str] = Field(..., max_length=5, description="具体的行動（max 5）")
    deliverables: list[str] = Field(default_factory=list, description="成果物")
    success_criteria: list[str] = Field(default_factory=list, description="完了条件")


class ContextSpecificAction(BaseModel):
    """文脈特化行動（v3.0: 術層の核心）.

    「チーム編成」ではなく「WebRTC SFUベンダー3社比較」のように、
    この問題固有の行動を定義。
    """

    action: str = Field(..., max_length=50, description="具体的行動")
    why_this_context: str = Field(..., max_length=50, description="なぜこの文脈で必要か")
    expected_output: str = Field(..., max_length=30, description="期待するアウトプット")


class SingleValidationPoint(BaseModel):
    """単一検証ポイント（v3.0: PoCで絶対に検証すべき1点）.

    「検証」ではなく「中国からのレイテンシが200ms以下か」のように、
    Go/No-Goを決定づける唯一の検証ポイント。
    """

    validation_target: str = Field(..., max_length=50, description="検証対象（1つだけ）")
    success_criteria: str = Field(..., max_length=50, description="成功基準（数値で）")
    failure_action: str = Field(..., max_length=50, description="失敗した場合の行動")


class ExitCriteria(BaseModel):
    """撤退基準（v3.0: どこで止めるか）.

    失敗した場合に「どの時点で」「何を基準に」撤退するかを定義。
    """

    checkpoint: str = Field(..., max_length=30, description="チェックポイント（例: 30日後）")
    exit_trigger: str = Field(..., max_length=50, description="撤退トリガー（数値で）")
    exit_action: str = Field(..., max_length=50, description="撤退時の行動")


class ShuInput(BaseModel):
    """ShuAgent入力."""

    fa_result: FaOutput = Field(..., description="FaAgent結果")
    selected_path_id: str = Field(..., description="選択されたパスID")


class ShuOutput(BaseModel):
    """ShuAgent出力 v3.0（文脈特化・撤退基準対応）."""

    # 既存フィールド
    phases: list[ActionPhase] = Field(..., min_length=3, max_length=5, description="3-5フェーズ")
    first_action: str = Field(..., description="最初の一歩（明日できること）")
    dependencies: list[str] = Field(default_factory=list, description="前提条件")

    # v2.0: 30天节奏控制
    rhythm_control: RhythmControl | None = Field(
        default=None,
        description="30天行动节奏控制 - 接下来30天，只做这一件事",
    )

    # v3.0: 最初の30日間で「捨てる」こと
    cut_list: list[str] = Field(
        default_factory=list,
        max_length=3,
        description="最初の30日間で明示的に「やらない」こと（max 3）",
    )

    # v3.0: 文脈特化行動（最初の2週間）
    context_specific_actions: list[ContextSpecificAction] = Field(
        default_factory=list,
        max_length=5,
        description="この問題固有の行動（教科書にない行動、max 5）",
    )

    # v3.0: 単一検証ポイント
    single_validation_point: SingleValidationPoint | None = Field(
        default=None,
        description="PoCで絶対に検証すべき1点（これがダメなら撤退）",
    )

    # v3.0: 撤退基準
    exit_criteria: ExitCriteria | None = Field(
        default=None,
        description="撤退基準（どこで止めるか）",
    )


# =============================================================================
# QiAgent スキーマ（v3.0: ドメイン固有技術・規制対応）
# =============================================================================


class DomainSpecificTechnology(BaseModel):
    """ドメイン固有技術（v3.0: 器層の核心）.

    「Python/FastAPI」ではなく「WebRTC / mediasoup SFU」のように、
    この問題領域に特化した技術を列挙。
    """

    technology_name: str = Field(..., max_length=80, description="技術名（例: WebRTC, SFU）")
    category: str = Field(..., max_length=30, description="カテゴリ（例: プロトコル, インフラ）")
    why_required: str = Field(..., max_length=100, description="なぜこの技術が必要か")
    alternatives: list[str] = Field(default_factory=list, max_length=3, description="代替技術（max 3）")


class RegulatoryConsideration(BaseModel):
    """規制対応事項（v3.0: コンプライアンス）.

    対象地域の法規制・データ主権を明示。
    """

    region: str = Field(..., max_length=20, description="地域（例: EU, 中国, 米国）")
    regulation: str = Field(..., max_length=30, description="規制名（例: GDPR, 中国サイバーセキュリティ法）")
    requirement: str = Field(..., max_length=50, description="具体的要件（例: データ滞留必須）")
    implementation_impact: str = Field(..., max_length=50, description="実装への影響")


class GeographicConsideration(BaseModel):
    """地理的考慮事項（v3.0: レイテンシ・PoP）.

    国際的なシステムにおける地理的要因を明示。
    """

    region: str = Field(..., max_length=20, description="地域")
    latency_requirement: str = Field(..., max_length=30, description="レイテンシ要件（例: 200ms以下）")
    infrastructure_need: str = Field(..., max_length=50, description="必要なインフラ（例: 東京リージョンPoP）")


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
    """QiAgent出力 v3.0（ドメイン固有技術・規制対応）."""

    implementations: list[Implementation] = Field(..., description="実装要素")
    tool_recommendations: list[str] = Field(default_factory=list, description="ツール推奨")
    integration_points: list[str] = Field(default_factory=list, description="統合ポイント")
    technical_debt_warnings: list[str] = Field(default_factory=list, description="技術負債警告")

    # v3.0: ドメイン固有技術（具体名詞必須）
    domain_technologies: list[DomainSpecificTechnology] = Field(
        default_factory=list,
        max_length=5,
        description="ドメイン固有技術（抽象化禁止、具体名詞必須、max 5）",
    )

    # v3.0: 規制対応事項
    regulatory_considerations: list[RegulatoryConsideration] = Field(
        default_factory=list,
        max_length=5,
        description="規制対応事項（地域別、max 5）",
    )

    # v3.0: 地理的考慮事項
    geographic_considerations: list[GeographicConsideration] = Field(
        default_factory=list,
        max_length=5,
        description="地理的考慮事項（レイテンシ・PoP、max 5）",
    )


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

