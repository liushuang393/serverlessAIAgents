/**
 * RichContent コンポーネントエクスポート
 *
 * @description 富文本レンダリング機能の公開 API
 */

// メインレンダラー
export { RichContentRenderer, default } from './RichContentRenderer';

// 型定義
export type {
  RichComponentType,
  ChartType,
  AlertType,
  AgentStatus,
  RichComponent,
  MarkdownProps,
  CodeBlockProps,
  DataTableProps,
  ChartProps,
  AlertProps,
  TabsProps,
  CollapsibleProps,
  CitationProps,
  RichResponse,
  RichContentRendererProps,
} from './types';

// 個別レンダラー（カスタマイズ用）
export {
  AlertRenderer,
  CodeBlockRenderer,
  MarkdownRenderer,
  DataTableRenderer,
  CollapsibleRenderer,
  CitationRenderer,
  TabsRenderer,
} from './renderers';

