/**
 * Decision Governance Engine - 型定義.
 *
 * 目的: APIリクエスト・レスポンスの型安全性を確保
 */

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

/** 決策リクエスト */
export interface DecisionRequest {
  question: string;
  budget?: number;
  timeline_months?: number;
  technical_constraints: string[];
  regulatory_constraints: string[];
  human_resources: string[];
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
  report_id: string;
  data: DecisionReport;
}

/** API レスポンス（統合型） */
export type DecisionAPIResponse = DecisionResponse | RejectionResponse;

// ========================================
// レポート関連型
// ========================================

/** 問題タイプ */
export type ProblemType = 'TRADE_OFF' | 'TIMING' | 'RESOURCE' | 'RISK';

/** 道（Dao）出力 */
export interface DaoOutput {
  problem_type: ProblemType;
  essence: string;
  immutable_constraints: string[];
  hidden_assumptions: string[];
}

/** 推奨パス */
export interface RecommendedPath {
  path_id: string;
  name: string;
  description: string;
  pros: string[];
  cons: string[];
  success_probability: number;
}

/** 法（Fa）出力 */
export interface FaOutput {
  recommended_paths: RecommendedPath[];
  rejected_paths: RecommendedPath[];
  decision_criteria: string[];
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

/** 術（Shu）出力 */
export interface ShuOutput {
  phases: Phase[];
  first_action: string;
  dependencies: string[];
}

/** 実装項目 */
export interface Implementation {
  component: string;
  technology: string;
  estimated_effort: string;
  risks: string[];
}

/** 器（Qi）出力 */
export interface QiOutput {
  implementations: Implementation[];
  tool_recommendations: string[];
  integration_points: string[];
  technical_debt_warnings: string[];
}

/** 検証所見 */
export interface ReviewFinding {
  severity: 'CRITICAL' | 'WARNING' | 'INFO';
  category: string;
  description: string;
  affected_agent?: string;
  suggested_revision?: string;
}

/** 検証出力 */
export interface ReviewOutput {
  overall_verdict: 'PASS' | 'REVISE' | 'REJECT';
  confidence_score: number;
  findings: ReviewFinding[];
  final_warnings: string[];
}

/** エグゼクティブサマリー */
export interface ExecutiveSummary {
  one_line_decision: string;
  recommended_action: string;
  key_risks: string[];
  first_step: string;
  estimated_impact: string;
}

/** 決策レポート */
export interface DecisionReport {
  report_id: string;
  created_at: string;
  dao: DaoOutput;
  fa: FaOutput;
  shu: ShuOutput;
  qi: QiOutput;
  review: ReviewOutput;
  executive_summary: ExecutiveSummary;
}

// ========================================
// AG-UI イベント型
// ========================================

/** AG-UI イベントタイプ */
export type AGUIEventType =
  | 'flow.start'
  | 'flow.complete'
  | 'flow.error'
  | 'node.start'
  | 'node.complete'
  | 'node.error'
  | 'progress'
  | 'log';

/** AG-UI イベント */
export interface AGUIEvent {
  event_type: AGUIEventType;
  timestamp: number;
  flow_id: string;
  node_id?: string;
  node_name?: string;
  percentage?: number;
  message?: string;
  data?: Record<string, unknown>;
}

