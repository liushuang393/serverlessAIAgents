import type React from "react";

/**
 * RichContent 型定義
 *
 * @description バックエンド A2UI RichResponse に対応する TypeScript 型
 */

/** コンポーネントタイプ */
export type RichComponentType =
  | "markdown"
  | "code_block"
  | "data_table"
  | "chart"
  | "citation"
  | "collapsible"
  | "link"
  | "progress"
  | "alert"
  | "tabs"
  | "timeline";

/** チャートタイプ */
export type ChartType = "bar" | "line" | "pie" | "scatter" | "area" | "radar";

/** アラートタイプ */
export type AlertType = "info" | "success" | "warning" | "error";

/** Agent ステータス */
export type AgentStatus = "healthy" | "unhealthy" | "degraded" | "unknown";

/**
 * コンポーネント Props 型
 */
export type RichComponentProps =
  | MarkdownProps
  | CodeBlockProps
  | DataTableProps
  | ChartProps
  | AlertProps
  | TabsProps
  | CollapsibleProps
  | CitationProps;

/**
 * 基底 RichComponent
 */
export interface RichComponent {
  /** コンポーネントタイプ */
  type: RichComponentType;
  /** 一意な ID */
  id?: string;
  /** タイプ固有のプロパティ */
  props: Partial<
    MarkdownProps &
      CodeBlockProps &
      DataTableProps &
      ChartProps &
      AlertProps &
      TabsProps &
      CollapsibleProps &
      CitationProps
  >;
  /** メタデータ */
  metadata?: Record<string, unknown>;
}

/**
 * Markdown コンポーネント Props
 */
export interface MarkdownProps {
  /** Markdown テキスト */
  content: string;
  /** 数式を有効化 */
  enableMath?: boolean;
  /** GFM を有効化 */
  enableGfm?: boolean;
}

/**
 * CodeBlock コンポーネント Props
 */
export interface CodeBlockProps {
  /** ソースコード */
  code: string;
  /** 言語 */
  language: string;
  /** ファイル名 */
  filename?: string;
  /** 行番号を表示 */
  showLineNumbers?: boolean;
  /** ハイライト行 */
  highlightLines?: number[];
}

/**
 * DataTable コンポーネント Props
 */
export interface DataTableProps {
  /** 列定義 */
  columns: Array<{
    key: string;
    header: string;
    sortable?: boolean;
    width?: number;
  }>;
  /** データ行 */
  data: Array<Record<string, unknown>>;
  /** ページネーション */
  pageSize?: number;
}

/**
 * Chart コンポーネント Props
 */
export interface ChartProps {
  /** チャートタイプ */
  chartType: ChartType;
  /** データ */
  data: unknown;
  /** タイトル */
  title?: string;
  /** 幅 */
  width?: number;
  /** 高さ */
  height?: number;
}

/**
 * Alert コンポーネント Props
 */
export interface AlertProps {
  /** アラートタイプ */
  alertType: AlertType;
  /** タイトル */
  title?: string;
  /** メッセージ */
  message: string;
}

/**
 * Tabs コンポーネント Props
 */
export interface TabsProps {
  /** タブ項目 */
  tabs: Array<{
    label: string;
    value: string;
    content: RichComponent[];
  }>;
  /** デフォルト選択 */
  defaultValue?: string;
}

/**
 * Collapsible コンポーネント Props
 */
export interface CollapsibleProps {
  /** タイトル */
  title: string;
  /** コンテンツ */
  content: RichComponent[];
  /** デフォルトで開く */
  defaultOpen?: boolean;
}

/**
 * Citation コンポーネント Props
 */
export interface CitationProps {
  /** 引用元 */
  sources: Array<{
    title: string;
    url?: string;
    author?: string;
    date?: string;
  }>;
}

/**
 * RichResponse - バックエンドからの応答
 */
export interface RichResponse {
  /** コンポーネント配列 */
  components: RichComponent[];
  /** メタデータ */
  metadata?: Record<string, unknown>;
  /** 作成日時 */
  createdAt?: string;
}

/**
 * RichContentRenderer Props
 */
export interface RichContentRendererProps {
  /** レンダリングするレスポンス */
  response: RichResponse;
  /** カスタムレンダラー */
  customRenderers?: Record<string, React.ComponentType<RichComponent>>;
  /** エラーフォールバック */
  fallback?: React.ReactNode;
  /** テーマ */
  theme?: "light" | "dark";
  /** CSS クラス */
  className?: string;
}
