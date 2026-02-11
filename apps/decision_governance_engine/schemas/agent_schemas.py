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


class ConstraintBoundary(BaseModel):
    """制約の境界条件定義（v3.1: 違反判定条件・判定方法・例外のセット）."""

    constraint_name: str = Field(..., max_length=30, description="制約名")
    definition: str = Field(..., max_length=100, description="判定条件（何を超えたら違反か）")
    violation_example: str = Field(..., max_length=100, description="違反判定の具体例")
    exceptions: str = Field(default="", max_length=100, description="例外条件（許容される逸脱）")


class SolutionRoute(BaseModel):
    """成立ルート（v3.1: 解空間の探索、最低3案を比較）."""

    route_type: str = Field(..., max_length=20, description="ルート種別（置換/旁路/アプライアンス等）")
    description: str = Field(..., max_length=100, description="ルートの説明")
    viability: str = Field(..., max_length=50, description="実現可能性の評価")
    tradeoffs: list[str] = Field(default_factory=list, max_length=3, description="トレードオフ（max 3）")


class QuantifiedMetric(BaseModel):
    """定量指標（v3.1: 要請を数値or段階＋優先順位に変換）."""

    metric_name: str = Field(..., max_length=30, description="指標名（E2E遅延、品質等）")
    target_value: str = Field(..., max_length=50, description="目標値（数値 or 段階）")
    priority: int = Field(..., ge=1, le=10, description="優先順位（1=最優先）")
    tradeoff_note: str = Field(default="", max_length=100, description="トレードオフ注記")


class AuditEvidenceItem(BaseModel):
    """監査証拠チェックリスト項目（v3.1: 証拠として提出するもの）."""

    category: str = Field(..., max_length=30, description="カテゴリ（データフロー/ログ保持/削除証跡等）")
    required_evidence: str = Field(..., max_length=100, description="必要な証拠")
    verification_method: str = Field(default="", max_length=100, description="確認方法")


class SelfCheckStatus(str, Enum):
    """セルフチェック総合ステータス（v3.1）."""

    PASS = "PASS"          # 全項目クリア
    WARNING = "WARNING"    # 軽微な不足あり
    FATAL = "FATAL"        # 致命的な不足あり（トレードオフ未解決等）


class SelfCheckResult(BaseModel):
    """セルフチェック結果（v3.1: 分析の自己検証）."""

    boundary_undefined: list[str] = Field(
        default_factory=list, description="境界未定義の制約",
    )
    missing_alternatives: list[str] = Field(
        default_factory=list, description="漏れた選択肢",
    )
    ambiguous_metrics: list[str] = Field(
        default_factory=list, description="曖昧な指標",
    )
    constraint_conflicts: list[str] = Field(
        default_factory=list, description="制約衝突",
    )
    evidence_gaps: list[str] = Field(
        default_factory=list, description="証拠不足",
    )
    overall_status: SelfCheckStatus = Field(
        default=SelfCheckStatus.WARNING, description="総合ステータス",
    )


class DaoInput(BaseModel):
    """DaoAgent入力."""

    question: str = Field(..., description="原始質問（Gatekeeper通過済み）")
    constraints: list[str] = Field(default_factory=list, description="現実制約")
    stakeholders: list[str] = Field(default_factory=list, description="関係者")
    gatekeeper_result: GatekeeperOutput | None = Field(default=None, description="入口検証結果")
    clarification_result: ClarificationOutput | None = Field(default=None, description="問題診断結果")


class DaoOutput(BaseModel):
    """DaoAgent出力（v3.1 制約境界・解空間・指標・証拠・セルフチェック対応）."""

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

    # v3.1: 制約境界条件定義（単語列挙→判定条件セット）
    constraint_boundaries: list[ConstraintBoundary] = Field(
        default_factory=list,
        max_length=5,
        description="制約の境界条件定義（違反判定条件・判定方法・例外のセット、max 5）",
    )

    # v3.1: 成立ルート（解空間の探索、最低3案）
    solution_routes: list[SolutionRoute] = Field(
        default_factory=list,
        max_length=5,
        description="成立ルート（結論前に最低3案を比較、max 5）",
    )

    # v3.1: 定量指標（要請→数値/段階＋優先順位）
    quantified_metrics: list[QuantifiedMetric] = Field(
        default_factory=list,
        max_length=5,
        description="定量指標（数値or段階＋優先順位、max 5）",
    )

    # v3.1: 監査証拠チェックリスト
    audit_evidence_checklist: list[AuditEvidenceItem] = Field(
        default_factory=list,
        max_length=8,
        description="監査証拠チェックリスト（証拠として提出するもの、max 8）",
    )

    # v3.1: セルフチェック結果（分析の自己検証、必須）
    self_check: SelfCheckResult | None = Field(
        default=None,
        description="セルフチェック結果（境界/選択肢/指標/衝突/証拠の自己検証）",
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


class ConditionalEvaluation(BaseModel):
    """条件付き評価（v3.1: 成功確率%の代替）."""

    success_conditions: list[str] = Field(
        default_factory=list, max_length=3, description="成立条件（満たせば成功確率↑）"
    )
    risk_factors: list[str] = Field(
        default_factory=list, max_length=3, description="主要リスク要因（失敗確率↑）"
    )
    failure_modes: list[str] = Field(
        default_factory=list, max_length=3, description="代表的失敗モード（どう壊れるか）"
    )
    probability_basis: str = Field(
        default="", max_length=200,
        description="確率算定根拠（算定式/前提/根拠、無ければ空）",
    )


class PathOption(BaseModel):
    """戦略パスオプション v3.1."""

    # 基本情報
    path_id: str = Field(..., description="パスID")
    name: str = Field(..., max_length=20, description="パス名（20字以内）")
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

    # 成功確率（v3.0互換、v3.1では条件付き評価を推奨）
    success_probability: float = Field(default=0.0, ge=0.0, le=1.0, description="成功確率（v3.0互換）")

    # v3.1: 条件付き評価（確率%の代替）
    conditional_evaluation: ConditionalEvaluation | None = Field(
        default=None, description="条件付き評価（v3.1: 成功確率%の代替）",
    )
    # v3.1: リスク集中点
    risk_concentration: str = Field(
        default="", max_length=100, description="リスク集中点（何が一番壊れやすいか）",
    )


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
    """戦略的禁止事項（v3.1: 仕組み化対応）.

    「何を絶対にしてはいけないか」＋防止策・検知・責任者を定義。
    """

    prohibition: str = Field(..., max_length=50, description="禁止事項")
    rationale: str = Field(..., max_length=100, description="なぜ禁止するか（戦略的理由）")
    violation_consequence: str = Field(..., max_length=50, description="違反した場合の結果")
    # v3.1: 仕組み化フィールド
    prevention_measure: str = Field(
        default="", max_length=100, description="防止策（レビュー/手順/チェックリスト）",
    )
    detection_metric: str = Field(
        default="", max_length=100, description="検知指標（証跡/ログ/監査項目）",
    )
    responsible_role: str = Field(
        default="", max_length=30, description="責任者（Roleで可）",
    )


class DifferentiationAxis(BaseModel):
    """差別化軸（v3.0互換: 真の競争優位）."""

    axis_name: str = Field(..., max_length=30, description="差別化軸名")
    why_this_axis: str = Field(..., max_length=100, description="なぜこの軸で差別化するか")
    not_this_axis: str = Field(..., max_length=50, description="差別化しない軸")


class CompetitiveHypothesis(BaseModel):
    """競争優位仮説（v3.1: 差別化軸の検証可能版）."""

    axis_name: str = Field(..., max_length=30, description="差別化軸名")
    target_customer: str = Field(..., max_length=100, description="具体的な対象顧客/利用シーン")
    substitution_barrier: str = Field(..., max_length=100, description="代替が難しい理由（何が障壁か）")
    winning_metric: str = Field(..., max_length=100, description="勝ち筋指標（何で勝ったと判定するか）")
    minimum_verification: str = Field(
        ..., max_length=150, description="最小検証（誰に何を当ててどう測るか）",
    )


class MustGate(BaseModel):
    """Mustゲート（v3.1: 不可変判断基準）."""

    criterion: str = Field(..., max_length=50, description="判断基準名")
    threshold: str = Field(..., max_length=100, description="閾値（満たさなければ即却下）")


class ShouldCriterion(BaseModel):
    """Should評価基準（v3.1: 比較評価用）."""

    criterion: str = Field(..., max_length=50, description="評価基準名")
    weight: str = Field(..., description="重み（High/Med/Low）")
    scoring_method: str = Field(..., max_length=100, description="採点方法")


class JudgmentFramework(BaseModel):
    """判断フレームワーク（v3.1: Must/Should分離）."""

    must_gates: list[MustGate] = Field(default_factory=list, description="Mustゲート（不可変）")
    should_criteria: list[ShouldCriterion] = Field(default_factory=list, description="Should評価基準")
    gate_results: dict[str, list[bool]] = Field(
        default_factory=dict, description="各案のゲート通過結果（パスID→[True/False...]）",
    )
    should_scores: dict[str, list[int]] = Field(
        default_factory=dict, description="各案のShould採点（パスID→[1-5...]）",
    )


class FaSelfCheckResult(BaseModel):
    """FaAgent セルフチェック結果（v3.1）."""

    baseless_numbers: list[str] = Field(default_factory=list, description="根拠なき数値表現")
    missing_intermediate: list[str] = Field(default_factory=list, description="中間案漏れ")
    missing_gates: list[str] = Field(default_factory=list, description="Mustゲート不在")
    appearance_precision: list[str] = Field(default_factory=list, description="見せかけの精度")
    overall_status: SelfCheckStatus = Field(..., description="総合ステータス")


class FaInput(BaseModel):
    """FaAgent入力."""

    dao_result: DaoOutput = Field(..., description="DaoAgent結果")
    available_resources: dict = Field(default_factory=dict, description="利用可能リソース")
    time_horizon: str = Field(default="", description="時間軸")


class FaOutput(BaseModel):
    """FaAgent出力 v3.1（根拠駆動・可実行）."""

    # 戦略オプション（v3.1: 最低4案、中間案含む）
    recommended_paths: list[PathOption] = Field(
        default_factory=list, description="戦略オプション（v3.1: 最低4案）",
    )
    rejected_paths: list[PathOption] = Field(default_factory=list, description="明示的に不推奨")

    # v3.0互換: 判断基準（文字列リスト）
    decision_criteria: list[str] = Field(default_factory=list, description="判断基準（v3.0互換）")

    # v2.0: 比較マトリックス
    path_comparison: PathComparisonMatrix | None = Field(
        default=None, description="パス比較マトリックス",
    )

    # v3.1: 戦略的禁止事項（仕組み化付き）
    strategic_prohibitions: list[StrategicProhibition] = Field(
        default_factory=list,
        description="戦略的禁止事項（防止策・検知・責任者付き）",
    )

    # v3.0互換: 差別化軸
    differentiation_axis: DifferentiationAxis | None = Field(
        default=None, description="差別化軸（v3.0互換）",
    )

    # v3.1: 競争優位仮説（差別化軸の検証可能版）
    competitive_hypothesis: CompetitiveHypothesis | None = Field(
        default=None, description="競争優位仮説（v3.1: 検証計画付き）",
    )

    # v3.1: 判断フレームワーク（Must/Should分離）
    judgment_framework: JudgmentFramework | None = Field(
        default=None, description="判断フレームワーク（v3.1: Must/Should分離）",
    )

    # v3.1: セルフチェック結果
    fa_self_check: FaSelfCheckResult | None = Field(
        default=None, description="セルフチェック結果（v3.1）",
    )

    # v3.0互換: 既存解が使えない理由
    why_existing_fails: str = Field(
        default="", max_length=100, description="既存の標準解が使えない理由（一文）",
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


# --- ShuAgent v3.1 新モデル（提案モード） ---


class PoCSuccessMetric(BaseModel):
    """PoC成功指標（v3.1: 定量的な成功判定）."""

    metric_name: str = Field(..., max_length=50, description="指標名（例: p95遅延, 翻訳安定率）")
    target_value: str = Field(..., max_length=50, description="目標値（例: 500ms以下, 95%以上）")
    measurement_method: str = Field(..., max_length=80, description="計測方法（例: Datadogでp95集計）")


class PoCDefinitionOfDone(BaseModel):
    """PoC完成定義（v3.1: 体験条件＋成功指標＋フォールバック）.

    PoCで「何が体験として成立するか」を先に固定する。
    """

    experience_conditions: list[str] = Field(
        ..., min_length=1, max_length=5,
        description="体験条件（例: 字幕リアルタイム表示, 翻訳2言語対応）",
    )
    success_metrics: list[PoCSuccessMetric] = Field(
        ..., min_length=3, max_length=5,
        description="成功指標（3〜5個: 遅延, 安定性, 運用手間等）",
    )
    fallback_strategy: str = Field(
        ..., max_length=200,
        description="フォールバック（失敗時に何を落として成立させるか）",
    )


class PhaseBranch(BaseModel):
    """フェーズ分岐（v3.1: 詰まった場合の代替手段）.

    各フェーズに最低2つの代替パスを用意する。
    """

    branch_name: str = Field(..., max_length=50, description="分岐名（例: 字幕先行, 準リアルタイム化）")
    trigger_condition: str = Field(..., max_length=80, description="この分岐に入る条件")
    description: str = Field(..., max_length=150, description="具体的な内容")


class ProposalPhase(BaseModel):
    """提案フェーズ（v3.1: 目的/作業/成果物/計測/注意点/分岐）.

    v3.0のActionPhaseを拡張し、提案モードに必要な全フィールドを含む。
    """

    phase_number: int = Field(..., ge=1, description="フェーズ番号")
    name: str = Field(..., max_length=50, description="フェーズ名")
    duration: str = Field(..., max_length=30, description="期間")
    purpose: str = Field(..., max_length=100, description="目的（なぜこのフェーズを行うか）")
    tasks: list[str] = Field(..., max_length=5, description="作業（max 5）")
    deliverables: list[str] = Field(default_factory=list, max_length=5, description="成果物")
    measurement: str = Field(default="", max_length=100, description="計測（どう測るか）")
    notes: list[str] = Field(default_factory=list, max_length=3, description="注意点")
    branches: list[PhaseBranch] = Field(
        default_factory=list, min_length=0, max_length=5,
        description="分岐（詰まった場合の代替、最低2つ推奨）",
    )


class StagePlan(BaseModel):
    """ステージ計画（v3.1: 2段ロケットの各ステージ）."""

    stage_name: str = Field(..., max_length=50, description="ステージ名")
    objective: str = Field(..., max_length=150, description="ステージ目標")
    phases: list[ProposalPhase] = Field(..., min_length=1, max_length=5, description="フェーズ群")
    gate_criteria: list[str] = Field(
        default_factory=list, max_length=3,
        description="次ステージへのゲート基準",
    )


class TwoStageRocket(BaseModel):
    """2段ロケット（v3.1: 学び最短順の実行計画）.

    Stage1: 最小パイプライン成立（ASR→翻訳→表示）を最短で確認。
    Stage2: 統制（監査ログ/保持削除/権限）を段階的に厚くする。
    """

    stage1_minimal_pipeline: StagePlan = Field(
        ..., description="Stage1: 最小パイプライン成立を最短で確認",
    )
    stage2_governance: StagePlan = Field(
        ..., description="Stage2: 統制を段階的に厚くする",
    )


class ShuOutput(BaseModel):
    """ShuAgent出力 v3.1（提案モード: DoD→2段ロケット→分岐付きフェーズ）."""

    # v3.0 既存フィールド（後方互換性）
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

    # v3.1: PoC完成定義（まず体験条件と成功指標を固定）
    poc_definition_of_done: PoCDefinitionOfDone | None = Field(
        default=None,
        description="PoC完成定義（体験条件＋成功指標＋フォールバック）",
    )

    # v3.1: 2段ロケット（学び最短順の実行計画）
    two_stage_rocket: TwoStageRocket | None = Field(
        default=None,
        description="2段ロケット（Stage1: 最小パイプライン → Stage2: 統制強化）",
    )

    # v3.1: 提案フェーズ（分岐付き）
    proposal_phases: list[ProposalPhase] = Field(
        default_factory=list,
        max_length=10,
        description="提案フェーズ（目的/作業/成果物/計測/注意点/分岐）",
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


# --- QiAgent v3.1 新モデル（提案モード） ---


class ArchitectureComponent(BaseModel):
    """アーキテクチャコンポーネント（v3.1: 箱と矢印の箱）."""

    name: str = Field(..., max_length=50, description="コンポーネント名（例: ストリーミングASR）")
    purpose: str = Field(..., max_length=80, description="役割（例: 音声→テキスト変換）")
    technology_choice: str = Field(..., max_length=80, description="PoCで使う技術（例: Google Speech-to-Text）")
    notes: str = Field(default="", max_length=100, description="注意点")


class MinimalLogging(BaseModel):
    """最小ログ/計測設定（v3.1: 相関ID＋タイムスタンプ）."""

    correlation_id_strategy: str = Field(
        ..., max_length=100, description="相関ID戦略（例: リクエスト毎にUUID発行）",
    )
    timestamp_points: list[str] = Field(
        default_factory=list, max_length=5,
        description="タイムスタンプ計測ポイント（例: 音声取得時, ASR完了時）",
    )
    storage: str = Field(default="", max_length=80, description="ログ保存先（例: CloudWatch Logs）")


class PoCMinimalArchitecture(BaseModel):
    """PoC最小アーキテクチャ（v3.1: 1つだけ提案する箱と矢印）.

    音声取得 → VAD → ストリーミングASR → 翻訳 → 字幕配信
    + 最小ログ/計測を組み込む。
    """

    components: list[ArchitectureComponent] = Field(
        ..., min_length=1, max_length=10,
        description="コンポーネント一覧（箱）",
    )
    data_flow_description: str = Field(
        ..., max_length=300,
        description="データフロー説明（矢印: コンポーネント間の流れ）",
    )
    minimal_logging: MinimalLogging | None = Field(
        default=None,
        description="最小ログ/計測設定",
    )
    deferred_components: list[str] = Field(
        default_factory=list, max_length=10,
        description="後回しにするコンポーネント（例: WebRTC SFU, WORM）",
    )


class ExpansionStage(BaseModel):
    """拡張アーキテクチャ段階（v3.1: 導入条件付き拡張）."""

    stage_name: str = Field(..., max_length=50, description="段階名（例: 多人数/多拠点対応）")
    introduction_condition: str = Field(
        ..., max_length=100,
        description="導入条件/閾値（例: 同時接続10人超, 監査要件ISO27001準拠必須時）",
    )
    added_components: list[str] = Field(
        ..., min_length=1, max_length=5,
        description="追加コンポーネント（例: SFU, WORM, 話者分離）",
    )
    rationale: str = Field(..., max_length=150, description="追加理由")


class ImplementationStep(BaseModel):
    """実装手順Step（v3.1: Step1〜StepN）."""

    step_number: int = Field(..., ge=1, description="ステップ番号")
    objective: str = Field(..., max_length=80, description="目標（例: 最短で動く字幕表示）")
    tasks: list[str] = Field(..., min_length=1, max_length=5, description="作業一覧")
    notes: list[str] = Field(default_factory=list, max_length=3, description="注意点")
    common_pitfalls: list[str] = Field(
        default_factory=list, max_length=3,
        description="よくある詰まりポイントと回避策",
    )


class QiInput(BaseModel):
    """QiAgent入力."""

    shu_result: ShuOutput = Field(..., description="ShuAgent結果")
    tech_constraints: list[str] = Field(default_factory=list, description="技術制約")


class QiOutput(BaseModel):
    """QiAgent出力 v3.1（提案モード: 最小構成→拡張構成→手順）."""

    # v3.0 既存フィールド（後方互換性）
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

    # v3.1: PoC最小アーキテクチャ（箱と矢印）
    poc_minimal_architecture: PoCMinimalArchitecture | None = Field(
        default=None,
        description="PoC最小アーキテクチャ（1つだけ提案）",
    )

    # v3.1: 拡張アーキテクチャ段階（導入条件付き）
    expansion_stages: list[ExpansionStage] = Field(
        default_factory=list,
        max_length=5,
        description="拡張アーキテクチャ段階（導入条件/閾値付き）",
    )

    # v3.1: 実装手順（Step1〜StepN）
    implementation_steps: list[ImplementationStep] = Field(
        default_factory=list,
        max_length=10,
        description="実装手順（最短で動くもの→計測→安定化→統制強化）",
    )

    # v3.1: 将来スケール要件（規制/地理をPoCから隔離）
    future_scale_requirements: list[str] = Field(
        default_factory=list,
        max_length=10,
        description="将来スケール時の追加要件（PoC外の規制/地理/性能要件）",
    )


# =============================================================================
# ReviewAgent スキーマ
# =============================================================================

# v3.1: 修正アクション分類
class ActionType(str, Enum):
    """修正アクション分類（v3.1）."""

    PATCH = "PATCH"      # 追記だけでOK（再走不要）
    RECALC = "RECALC"    # 追記→自動再計算（再走不要）
    RERUN = "RERUN"      # 全体再走が必要（原則40点未満のみ）


class ScoreImprovement(BaseModel):
    """パッチ適用後のスコア改善見込み（v3.1）."""

    target_score: str = Field(..., max_length=50, description="改善対象スコア名")
    current_estimate: float = Field(..., ge=0.0, le=100.0, description="現在推定値")
    improved_estimate: float = Field(..., ge=0.0, le=100.0, description="パッチ後推定値")
    delta: float = Field(..., description="改善幅（+何点）")


class MinimalPatch(BaseModel):
    """最小パッチ（チェックボックス＋注釈）（v3.1）."""

    checkbox_label: str = Field(..., max_length=80, description="チェックボックスのラベル")
    annotation_hint: str = Field(default="", max_length=30, description="注釈のヒント（10〜20字）")
    default_value: str = Field(default="", max_length=50, description="デフォルト案（暫定ロール等）")


class ReviewFinding(BaseModel):
    """検証所見 v3.1（差分パッチ型）."""

    severity: FindingSeverity = Field(..., description="重大度")
    category: FindingCategory = Field(..., description="カテゴリ")
    description: str = Field(..., description="説明")
    affected_agent: str = Field(..., description="影響を受けるAgent")
    suggested_revision: str = Field(..., description="修正提案")
    requires_human_review: bool = Field(
        default=False, description="人間確認が必須か"
    )
    human_review_hint: str | None = Field(
        default=None, description="人間確認時の補足メッセージ"
    )
    # v3.1 差分パッチ型フィールド
    failure_point: str = Field(default="", max_length=200, description="破綻点：このままだとどこで失敗するか")
    impact_scope: str = Field(default="", max_length=200, description="影響範囲：どのAgent/成果物が無効になるか")
    minimal_patch: MinimalPatch | None = Field(default=None, description="最小パッチ（checkbox＋注釈）")
    score_improvements: list[ScoreImprovement] = Field(default_factory=list, description="パッチ適用後のスコア改善見込み")
    action_type: ActionType = Field(default=ActionType.RECALC, description="修正アクション分類")


class ConfidenceComponent(BaseModel):
    """信頼度分解の各項目（v3.1）."""

    name: str = Field(..., max_length=30, description="項目名")
    score: float = Field(..., ge=0.0, le=100.0, description="現在スコア（0-100）")
    max_score: float = Field(default=100.0, ge=0.0, le=100.0, description="最大スコア")
    checkbox_boost: float = Field(default=0.0, ge=0.0, le=30.0, description="チェックで+何点")
    description: str = Field(default="", max_length=100, description="評価説明")


class ConfidenceBreakdown(BaseModel):
    """信頼度分解表示（v3.1）."""

    input_sufficiency: ConfidenceComponent = Field(..., description="入力充足度")
    logic_consistency: ConfidenceComponent = Field(..., description="論理整合")
    implementation_feasibility: ConfidenceComponent = Field(..., description="実装可能性")
    risk_coverage: ConfidenceComponent = Field(..., description="リスク網羅")


class CheckpointItem(BaseModel):
    """チェックボックス項目（v3.1）."""

    item_id: str = Field(..., max_length=50, description="項目ID")
    label: str = Field(..., max_length=100, description="チェックボックスラベル")
    checked: bool = Field(default=False, description="チェック済みか")
    annotation: str = Field(default="", max_length=50, description="ユーザー注釈（任意）")
    score_boost: float = Field(default=0.0, ge=0.0, le=20.0, description="チェック時のスコア上昇幅")
    target_component: str = Field(default="", max_length=30, description="影響する信頼度コンポーネント")
    default_suggestion: str = Field(default="", max_length=100, description="デフォルト案（暫定責任者ロール等）")


class ReviewInput(BaseModel):
    """ReviewAgent入力."""

    dao_result: DaoOutput = Field(..., description="DaoAgent結果")
    fa_result: FaOutput = Field(..., description="FaAgent結果")
    shu_result: ShuOutput = Field(..., description="ShuAgent結果")
    qi_result: QiOutput = Field(..., description="QiAgent結果")


class ReviewOutput(BaseModel):
    """ReviewAgent出力 v3.1（差分パッチ型）."""

    overall_verdict: ReviewVerdict = Field(..., description="総合判定")
    findings: list[ReviewFinding] = Field(default_factory=list, max_length=3, description="検証所見（最大3件、重複除去済み）")
    confidence_score: float = Field(..., ge=0.0, le=1.0, description="信頼度スコア（総合）")
    final_warnings: list[str] = Field(default_factory=list, description="最終警告")
    # v3.1 差分パッチ型フィールド
    confidence_breakdown: ConfidenceBreakdown | None = Field(default=None, description="信頼度分解（4項目）")
    checkpoint_items: list[CheckpointItem] = Field(default_factory=list, description="チェックボックス項目")
    auto_recalc_enabled: bool = Field(default=True, description="自動再計算が可能か")
