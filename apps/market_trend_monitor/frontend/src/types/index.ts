/**
 * フロントエンド型定義.
 * 
 * 目的: バックエンドAPIとの型安全な通信を保証
 * 注意: バックエンドのスキーマと同期を保つこと
 */

export enum SourceType {
  NEWS = 'news',
  GITHUB = 'github',
  ARXIV = 'arxiv',
  RSS = 'rss',
  STACKOVERFLOW = 'stackoverflow',
  DEVTO = 'devto',
}

export enum SentimentType {
  POSITIVE = 'positive',
  NEUTRAL = 'neutral',
  NEGATIVE = 'negative',
}

export enum NotificationPriority {
  LOW = 'low',
  MEDIUM = 'medium',
  HIGH = 'high',
  CRITICAL = 'critical',
}

export interface Article {
  id: string;
  title: string;
  url: string;
  source: SourceType;
  published_at: string;
  content: string;
  keywords: string[];
}

export interface Trend {
  id: string;
  topic: string;
  score: number;
  articles_count: number;
  keywords: string[];
  sentiment: SentimentType;
  growth_rate: number;
  created_at: string;
  metadata: Record<string, unknown>;
}

export interface ReportSection {
  title: string;
  content: string;
  chart_data?: Record<string, unknown>;
}

export interface Report {
  id: string;
  title: string;
  summary: string;
  sections: ReportSection[];
  trends: Trend[];
  period_start: string;
  period_end: string;
  created_at: string;
  metadata: Record<string, unknown>;
}

export interface Notification {
  id: string;
  title: string;
  message: string;
  priority: NotificationPriority;
  trend_id: string;
  created_at: string;
  read: boolean;
  metadata: Record<string, unknown>;
}

// API リクエスト型
export interface CollectRequest {
  keywords: string[];
  sources: SourceType[];
}

export interface CollectResponse {
  status: string;
  articles_count: number;
  trends_count: number;
  message: string;
}

export interface TrendsResponse {
  trends: Trend[];
  total: number;
}

export interface ReportsResponse {
  reports: Report[];
  total: number;
}

// Phase 13: Signal types
export interface SignalScore {
  reliability: number;
  leading: number;
  relevance: number;
  actionability: number;
  convergence: number;
  total: number;
  grade: string;
}

export interface Signal {
  id: string;
  trend_id: string;
  score: SignalScore;
  grade: string;
  evaluated_at: string;
  metadata: Record<string, unknown>;
}

// Phase 13: Prediction types
export interface Prediction {
  id: string;
  statement: string;
  target_date: string;
  confidence: number;
  claim_id?: string;
  status: string;
  created_at: string;
  metadata: Record<string, unknown>;
}

export interface PredictionReview {
  id: string;
  prediction_id: string;
  actual_outcome: string;
  outcome: string;
  accuracy_score: number;
  reviewed_at: string;
  notes: string;
}

// Phase 13: Evidence types
export interface Evidence {
  id: string;
  source_type: string;
  url: string;
  title: string;
  collected_at: string;
  reliability_score: number;
  extracted_data: Record<string, unknown>;
}

export interface Claim {
  id: string;
  statement: string;
  level: string;
  confidence: number;
  evidence_ids: string[];
  created_at: string;
}

// Phase 13: Recommendation
export interface Recommendation {
  topic: string;
  grade: string;
  score: number;
  priority: string;
  action_type: string;
  recommendation: string;
  growth_note: string;
}

// チャートデータ型
export interface ChartDataPoint {
  date: string;
  value: number;
  label?: string;
}

export interface TrendChartData {
  topic: string;
  data: ChartDataPoint[];
}

