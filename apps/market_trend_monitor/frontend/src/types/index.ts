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

