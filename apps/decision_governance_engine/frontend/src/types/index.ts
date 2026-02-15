/**
 * Decision Governance Engine - 型定義.
 *
 * 目的: APIリクエスト・レスポンスの型安全性を確保
 */

// ========================================
// 認証関連型
// ========================================

/** ユーザー情報 */
export interface UserInfo {
  user_id: string;
  username: string;
  display_name: string;
  department: string;
  position: string;
  created_at: string;
}

/** 認証レスポンス */
export interface AuthResponse {
  success: boolean;
  message: string;
  user?: UserInfo | null;
}

/** 署名情報 */
export interface SignatureData {
  report_id: string;
  signed_by: string;
  signer_id: string;
  department: string;
  position: string;
  signed_at: string;
  signed_at_display: string;
}

/** 署名レスポンス */
export interface SignatureResponse {
  success: boolean;
  message: string;
  signature?: SignatureData | null;
}

// ========================================
// 履歴関連型
// ========================================

/** サーバー側履歴アイテム */
export interface ServerHistoryItem {
  id: string;
  request_id: string;
  question: string;
  decision_role: 'GO' | 'NO_GO' | 'DELAY' | 'PILOT';
  confidence: number | null;
  mode: 'FAST' | 'STANDARD' | 'AUDIT';
  created_at: string;
}

/** 履歴一覧レスポンス */
export interface HistoryListResponse {
  status: string;
  total: number;
  items: ServerHistoryItem[];
}

/** 履歴詳細レスポンス */
export interface HistoryDetailResponse {
  status: string;
  data: {
    id: string;
    request_id: string;
    question: string;
    decision_role: string;
    confidence: number | null;
    mode: string;
    fa_result: Record<string, unknown> | null;
    shu_result: Record<string, unknown> | null;
    qi_result: Record<string, unknown> | null;
    summary_bullets: string[] | null;
    warnings: string[] | null;
    processing_time_ms: number | null;
    created_at: string;
  };
}

// ========================================
// API リクエスト/レスポンス型
// ========================================

/** 制約条件 */
export interface ConstraintSet {
  budget?: number;
  timeline_months?: number;
  technical_constraints: string[];
  regulatory_constraints: string[];
  human_resources: string[];
}

/** ステークホルダー（責任者）情報 */
export interface StakeholderInfo {
  product_owner: string;
  tech_lead: string;
  business_owner: string;
  legal_reviewer: string;
}

/** 決策リクエスト */
export interface DecisionRequest {
  question: string;
  budget?: number;
  timeline_months?: number;
  technical_constraints: string[];
  regulatory_constraints: string[];
  human_resources: string[];
  stakeholder_product_owner?: string;
  stakeholder_tech_lead?: string;
  stakeholder_business_owner?: string;
  stakeholder_legal_reviewer?: string;
}

/** 拒否レスポンス */
export interface RejectionResponse {
  status: 'rejected';
  reason?: string;
  message?: string;
  suggested_rephrase?: string;
}

/** 成功レスポンス */
export interface DecisionResponse {
  status: 'success';
  /** 履歴照会・PDF出力用のリクエストID（UUID） */
  request_id?: string;
  report_id: string;
  data: DecisionReport;
}

/** API レスポンス（統合型） */
export type DecisionAPIResponse = DecisionResponse | RejectionResponse;

// ========================================
// レポート関連型
// ========================================

/** 問題タイプ */
export type ProblemType = 'TRADE_OFF' | 'TIMING' | 'RESOURCE' | 'RISK' | 'STRATEGY_DIRECTION' | 'CONSTRAINT_DRIVEN';

/** 問題の本質的性質 (v3.0) */
export type ProblemNatureType =
  | 'CONSTRAINT_DRIVEN'       // 制約主導型
  | 'OPPORTUNITY_DRIVEN'      // 機会主導型
  | 'RISK_MITIGATION'         // リスク軽減型
  | 'OPTIMIZATION'            // 最適化型
  | 'EXPLORATION'             // 探索型
  | 'UNKNOWN';

/** 本質導出プロセス (v3.0) */
export interface EssenceDerivation {
  surface_problem: string;      // 表面的問題
  underlying_why: string;       // 一段深い理由
  root_constraint: string;      // 根本制約
  essence_statement: string;    // 本質の一文
}

/** 既存代替手段 (v3.0) */
export interface ExistingAlternative {
  name: string;                 // 代替手段名
  why_not_viable: string;       // 使えない理由
  specific_constraint: string;  // 具体的制約
}

/** 因果齿轮 (v3.0) */
export interface CausalGear {
  name: string;                 // Gear名
  description: string;          // 説明
  leverage: 'HIGH' | 'MEDIUM' | 'LOW';  // テコ効果
}

/** 死穴 (v3.0) */
export interface DeathTrap {
  action: string;               // 禁忌行動
  reason: string;               // 理由
  severity: 'FATAL' | 'SEVERE' | 'MODERATE' | 'CRITICAL';  // 深刻度
}

/** 制約境界条件 (v3.1) */
export interface ConstraintBoundary {
  constraint_name: string;
  definition: string;
  violation_example: string;
  exceptions?: string;
}

/** 成立ルート (v3.1) */
export interface SolutionRoute {
  route_type: string;
  description: string;
  viability: string;
  tradeoffs?: string[];
}

/** 定量指標 (v3.1) */
export interface QuantifiedMetric {
  metric_name: string;
  target_value: string;
  priority: number;
  tradeoff_note?: string;
}

/** 監査証拠項目 (v3.1) */
export interface AuditEvidenceItem {
  category: string;
  required_evidence: string;
  verification_method?: string;
}

/** Dao セルフチェック (v3.1) */
export interface DaoSelfCheckResult {
  boundary_undefined: string[];
  missing_alternatives: string[];
  ambiguous_metrics: string[];
  constraint_conflicts: string[];
  evidence_gaps: string[];
  overall_status: 'PASS' | 'WARNING' | 'FATAL';
}

/** 道（Dao）出力 v3.1 */
export interface DaoOutput {
  problem_type: ProblemType;
  problem_nature?: ProblemNatureType;  // v3.0
  essence: string;
  essence_derivation?: EssenceDerivation;  // v3.0
  existing_alternatives?: ExistingAlternative[];  // v3.0
  immutable_constraints: string[];
  hidden_assumptions: string[];
  causal_gears?: CausalGear[];  // v3.0
  bottleneck_gear?: string | number;  // v3.0
  death_traps?: DeathTrap[];    // v3.0
  // v3.1
  constraint_boundaries?: ConstraintBoundary[];
  solution_routes?: SolutionRoute[];
  quantified_metrics?: QuantifiedMetric[];
  audit_evidence_checklist?: AuditEvidenceItem[];
  self_check?: DaoSelfCheckResult;
}

/** 戦略タイプ (v3.0) */
export type StrategyType =
  | 'PHASED_INVESTMENT'
  | 'ALL_IN'
  | 'PARTNERSHIP'
  | 'WAIT_AND_SEE'
  | 'PIVOT'
  | 'BUILD'
  | 'BUY'
  | 'HYBRID';

/** 条件付き評価 (v3.1) */
export interface ConditionalEvaluation {
  success_conditions: string[];
  risk_factors: string[];
  failure_modes: string[];
  probability_basis?: string;
}

/** 推奨パス */
export interface RecommendedPath {
  path_id: string;
  name: string;
  description: string;
  strategy_type?: StrategyType;  // v3.0
  pros: string[];
  cons: string[];
  success_probability: number;
  time_to_value?: string;      // v3.0
  reversibility?: string;      // v3.0
  // v3.1
  conditional_evaluation?: ConditionalEvaluation;
  risk_concentration?: string;
}

/** 戦略的禁止事項 (v3.1) */
export interface StrategicProhibition {
  prohibition: string;
  rationale: string;
  violation_consequence: string;
  // v3.1: 仕組み化
  prevention_measure?: string;
  detection_metric?: string;
  responsible_role?: string;
}

/** 差別化軸 (v3.0互換) */
export interface DifferentiationAxis {
  axis_name: string;
  why_this_axis: string;
  not_this_axis: string;
}

/** 競争優位仮説 (v3.1) */
export interface CompetitiveHypothesis {
  axis_name: string;
  target_customer: string;
  substitution_barrier: string;
  winning_metric: string;
  minimum_verification: string;
}

/** Must判断基準 (v3.1) */
export interface MustGate {
  criterion: string;
  threshold: string;
}

/** Should判断基準 (v3.1) */
export interface ShouldCriterion {
  criterion: string;
  weight: string;
  scoring_method: string;
}

/** 判断フレームワーク (v3.1) */
export interface JudgmentFramework {
  must_gates: MustGate[];
  should_criteria: ShouldCriterion[];
  gate_results: Record<string, boolean[]>;
  should_scores: Record<string, number[]>;
}

/** 法セルフチェック結果 (v3.1) */
export interface FaSelfCheckResult {
  baseless_numbers: string[];
  missing_intermediate: string[];
  missing_gates: string[];
  appearance_precision: string[];
  overall_status: 'PASS' | 'WARNING' | 'FAIL';
}

/** 法（Fa）出力 v3.1 */
export interface FaOutput {
  recommended_paths: RecommendedPath[];
  rejected_paths: RecommendedPath[];
  decision_criteria: string[];
  strategic_prohibitions?: StrategicProhibition[];
  differentiation_axis?: DifferentiationAxis;
  why_existing_fails?: string;
  // v3.1
  competitive_hypothesis?: CompetitiveHypothesis;
  judgment_framework?: JudgmentFramework;
  fa_self_check?: FaSelfCheckResult;
}

/** フェーズ */
export interface Phase {
  phase_number: number;
  name: string;
  duration: string;
  actions: string[];
  deliverables: string[];
  success_criteria: string[];
}

/** 文脈特化行動 (v3.0) */
export interface ContextSpecificAction {
  action: string;              // 行動
  why_this_context: string;    // この文脈での理由
  expected_output: string;     // 期待出力
}

/** 単一検証ポイント (v3.0) */
export interface SingleValidationPoint {
  validation_target: string;   // 検証対象
  success_criteria: string;    // 成功基準
  failure_action: string;      // 失敗時行動
}

/** 撤退基準 (v3.0) */
export interface ExitCriteria {
  checkpoint: string;          // チェックポイント
  exit_trigger: string;        // 撤退トリガー
  exit_action: string;         // 撤退時行動
}

/** フォーカス (v3.0) */
export interface RhythmFocus {
  name: string;
  description: string;
  success_metric: string;
  avoid_list: string[];
}

/** 30天行動節奏 (v3.0) */
export interface RhythmControl {
  focus: RhythmFocus;
  checkpoint_date: string;
  next_decision_point: string;
}

/** 術（Shu）出力 v3.0 */
/** PoC成功指標 (v3.1) */
export interface PoCSuccessMetric {
  metric_name: string;
  target_value: string;
  measurement_method: string;
}

/** PoC完成定義 (v3.1) */
export interface PoCDefinitionOfDone {
  experience_conditions: string[];
  success_metrics: PoCSuccessMetric[];
  fallback_strategy: string;
}

/** フェーズ分岐 (v3.1) */
export interface PhaseBranch {
  branch_name: string;
  trigger_condition: string;
  description: string;
}

/** 提案フェーズ (v3.1) */
export interface ProposalPhase {
  phase_number: number;
  name: string;
  duration: string;
  purpose: string;
  tasks: string[];
  deliverables?: string[];
  measurement?: string;
  notes?: string[];
  branches?: PhaseBranch[];
}

/** ステージ計画 (v3.1) */
export interface StagePlan {
  stage_name: string;
  objective: string;
  phases: ProposalPhase[];
  gate_criteria?: string[];
}

/** 2段ロケット (v3.1) */
export interface TwoStageRocket {
  stage1_minimal_pipeline: StagePlan;
  stage2_governance: StagePlan;
}

export interface ShuOutput {
  phases: Phase[];
  first_action: string;
  dependencies: string[];
  cut_list?: string[];                              // v3.0 切り捨てリスト
  context_specific_actions?: ContextSpecificAction[];  // v3.0
  single_validation_point?: SingleValidationPoint;  // v3.0
  exit_criteria?: ExitCriteria;                     // v3.0
  rhythm_control?: RhythmControl;                   // v3.0
  // v3.1 新フィールド
  poc_definition_of_done?: PoCDefinitionOfDone;
  two_stage_rocket?: TwoStageRocket;
  proposal_phases?: ProposalPhase[];
}

/** 実装項目 */
export interface Implementation {
  component: string;
  technology: string;
  estimated_effort: string;
  risks: string[];
}

/** ドメイン固有技術 (v3.0) */
export interface DomainSpecificTechnology {
  technology_name: string;     // 技術名（具体名詞）
  category: string;            // カテゴリ
  why_required: string;        // 必要理由
  alternatives: string[];      // 代替技術
}

/** 規制対応事項 (v3.0) */
export interface RegulatoryConsideration {
  region: string;              // 地域
  regulation: string;          // 規制名
  requirement: string;         // 要件
  implementation_impact: string;  // 実装影響
}

/** 地理的考慮事項 (v3.0) */
export interface GeographicConsideration {
  region: string;              // 地域
  latency_requirement: string; // レイテンシ要件
  infrastructure_need: string; // インフラ要件
}

/** アーキテクチャコンポーネント (v3.1) */
export interface ArchitectureComponent {
  name: string;
  purpose: string;
  technology_choice: string;
  notes?: string;
}

/** 最小ログ設定 (v3.1) */
export interface MinimalLogging {
  correlation_id_strategy: string;
  timestamp_points?: string[];
  storage?: string;
}

/** PoC最小アーキテクチャ (v3.1) */
export interface PoCMinimalArchitecture {
  components: ArchitectureComponent[];
  data_flow_description: string;
  minimal_logging?: MinimalLogging;
  deferred_components?: string[];
}

/** 拡張アーキテクチャ段階 (v3.1) */
export interface ExpansionStage {
  stage_name: string;
  introduction_condition: string;
  added_components: string[];
  rationale: string;
}

/** 実装手順Step (v3.1) */
export interface ImplementationStep {
  step_number: number;
  objective: string;
  tasks: string[];
  notes?: string[];
  common_pitfalls?: string[];
}

/** 器（Qi）出力 v3.1 */
export interface QiOutput {
  implementations: Implementation[];
  tool_recommendations: string[];
  integration_points: string[];
  technical_debt_warnings: string[];
  domain_technologies?: DomainSpecificTechnology[];     // v3.0
  regulatory_considerations?: RegulatoryConsideration[];  // v3.0
  geographic_considerations?: GeographicConsideration[];  // v3.0
  // v3.1 新フィールド
  poc_minimal_architecture?: PoCMinimalArchitecture;
  expansion_stages?: ExpansionStage[];
  implementation_steps?: ImplementationStep[];
  future_scale_requirements?: string[];
}

/** 修正アクション分類 (v3.1) */
export type ActionType = 'PATCH' | 'RECALC' | 'RERUN';

/** スコア改善見込み (v3.1) */
export interface ScoreImprovement {
  target_score: string;
  current_estimate: number;
  improved_estimate: number;
  delta: number;
}

/** 最小パッチ (v3.1) */
export interface MinimalPatch {
  checkbox_label: string;
  annotation_hint?: string;
  default_value?: string;
}

/** 検証所見 v3.1（差分パッチ型） */
export interface ReviewFinding {
  severity: 'CRITICAL' | 'WARNING' | 'INFO';
  category: string;
  description: string;
  affected_agent?: string;
  suggested_revision?: string;
  requires_human_review?: boolean;
  human_review_hint?: string;
  // v3.1 差分パッチ型フィールド
  failure_point?: string;
  impact_scope?: string;
  minimal_patch?: MinimalPatch;
  score_improvements?: ScoreImprovement[];
  action_type?: ActionType;
}

/** 信頼度コンポーネント (v3.1) */
export interface ConfidenceComponent {
  name: string;
  score: number;
  max_score?: number;
  checkbox_boost: number;
  description?: string;
}

/** 信頼度分解 (v3.1) */
export interface ConfidenceBreakdown {
  input_sufficiency: ConfidenceComponent;
  logic_consistency: ConfidenceComponent;
  implementation_feasibility: ConfidenceComponent;
  risk_coverage: ConfidenceComponent;
}

/** チェックポイント項目 (v3.1) */
export interface CheckpointItem {
  item_id: string;
  label: string;
  checked: boolean;
  annotation?: string;
  score_boost: number;
  target_component?: string;
  default_suggestion?: string;
}

/** 検証出力 v3.1（差分パッチ型） */
export interface ReviewOutput {
  overall_verdict: 'PASS' | 'REVISE' | 'COACH';
  confidence_score: number;
  findings: ReviewFinding[];
  final_warnings: string[];
  // v3.1 差分パッチ型フィールド
  confidence_breakdown?: ConfidenceBreakdown;
  checkpoint_items?: CheckpointItem[];
  auto_recalc_enabled?: boolean;
}

/** 人間確認による重要指摘の再チェック要求 */
export interface FindingRecheckRequest {
  report_id: string;
  request_id?: string;
  finding_index: number;
  confirmation_note: string;
  acknowledged: boolean;
  reviewer_name?: string;
}

/** 人間確認による重要指摘の再チェック応答 */
export interface FindingRecheckResponse {
  success: boolean;
  resolved: boolean;
  message: string;
  issues: string[];
  updated_review?: ReviewOutput;
}

/** 人間確認メモ（任意）保存リクエスト */
export interface FindingNoteRequest {
  report_id: string;
  request_id?: string;
  finding_index: number;
  acknowledged?: boolean;
  memo?: string;
  reviewer_name?: string;
}

/** 人間確認メモ（任意）保存レスポンス */
export interface FindingNoteResponse {
  success: boolean;
  message: string;
}

/** エグゼクティブサマリー v3.0 */
export interface ExecutiveSummary {
  one_line_decision: string;
  recommended_action: string;
  key_risks: string[];
  first_step: string;
  estimated_impact: string;
  essence_statement?: string;                // v3.0 本質の一文
  strategic_prohibition_summary?: string;    // v3.0 戦略的禁止サマリー
  exit_criteria_summary?: string;            // v3.0 撤退基準サマリー
}

/** 提案書タイトル (v3.1) */
export interface ProposalTitle {
  title_ja: string;        // 日本語タイトル
  title_en: string;        // 英語タイトル（システム用）
  case_id: string;         // 案件ID
  subtitle: string;        // サブタイトル
}

/** 署名欄情報 (v3.1) */
export interface SignatureBlock {
  author_name: string;           // 作成者名
  author_department: string;     // 作成者部署
  author_position: string;       // 作成者役職
  created_date: string;          // 作成日
  approver_name?: string;        // 承認者名
  approver_department?: string;  // 承認者部署
  approver_position?: string;    // 承認者役職
  approved_date?: string;        // 承認日
  is_signed: boolean;            // 署名済みフラグ
  signature_timestamp?: string;  // 署名タイムスタンプ
}

/** 提案書 v3.1 (旧: 決策レポート) */
export interface DecisionReport {
  report_id: string;
  created_at: string;
  version: string;
  proposal_title?: ProposalTitle;  // v3.1 提案書タイトル
  original_question?: string;      // v3.1 元の質問
  signature_block?: SignatureBlock;  // v3.1 署名欄情報
  dao: DaoOutput;
  fa: FaOutput;
  shu: ShuOutput;
  qi: QiOutput;
  review?: ReviewOutput;  // Optional: 最終ステージが未完了の場合 undefined
  executive_summary: ExecutiveSummary;
}

// ========================================
// AG-UI イベント型
// ========================================

/** AG-UI イベントタイプ */
export type AGUIEventType =
  | 'connection.established'
  | 'resume.context'
  | 'flow.start'
  | 'flow.complete'
  | 'flow.error'
  | 'node.start'
  | 'node.complete'
  | 'node.error'
  | 'progress'
  | 'log'
  | 'clarification.required'
  | 'clarification.received';

/** 補足質問 */
export interface ClarificationQuestion {
  id: string;
  text: string;
  type: 'text' | 'number' | 'select';
  required: boolean;
  options?: string[];
  placeholder?: string;
}

/** AG-UI イベント */
export interface AGUIEvent {
  event_type: AGUIEventType;
  timestamp: number;
  flow_id: string;
  node_id?: string;
  node_name?: string;
  percentage?: number;
  message?: string;
  error_message?: string;  // flow.error 用エラーメッセージ
  data?: Record<string, unknown>;
  // flow.complete 用（AgentFlow 框架標準）
  result_id?: string;  // 結果ID（ResultStore参照用）
  result?: Record<string, unknown>;  // 完整結果（include_result=true時）
  // clarification.required 用
  original_question?: string;
  questions?: ClarificationQuestion[];
  timeout_seconds?: number;
}

