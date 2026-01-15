# RichContentRenderer 設計書

> **バージョン**: 1.0.0
> **更新日**: 2026-01-15

---

## 📋 概要

フロントエンド富文本レンダリングコンポーネントの設計。
バックエンドの A2UI `RichResponse` を React コンポーネントとして表示。

### 対応コンテンツタイプ

| タイプ | コンポーネント | 依存ライブラリ |
|--------|---------------|---------------|
| Markdown | `MarkdownRenderer` | `react-markdown`, `remark-gfm` |
| コード | `CodeBlockRenderer` | `prism-react-renderer` |
| テーブル | `DataTableRenderer` | `@tanstack/react-table` |
| チャート | `ChartRenderer` | `echarts-for-react` |
| 数式 | `MathRenderer` | `katex`, `rehype-katex` |
| 引用 | `CitationRenderer` | (内蔵) |
| アラート | `AlertRenderer` | (内蔵) |
| タブ | `TabsRenderer` | `@radix-ui/react-tabs` |

---

## 🏗️ アーキテクチャ

```
┌─────────────────────────────────────────────────────────────┐
│                    RichContentRenderer                       │
│  (メインエントリーポイント - コンポーネント配列を処理)         │
├─────────────────────────────────────────────────────────────┤
│  ┌─────────────┐ ┌─────────────┐ ┌─────────────┐           │
│  │ Markdown    │ │ CodeBlock   │ │ DataTable   │           │
│  │ Renderer    │ │ Renderer    │ │ Renderer    │           │
│  └─────────────┘ └─────────────┘ └─────────────┘           │
│  ┌─────────────┐ ┌─────────────┐ ┌─────────────┐           │
│  │ Chart       │ │ Math        │ │ Citation    │           │
│  │ Renderer    │ │ Renderer    │ │ Renderer    │           │
│  └─────────────┘ └─────────────┘ └─────────────┘           │
│  ┌─────────────┐ ┌─────────────┐ ┌─────────────┐           │
│  │ Alert       │ │ Tabs        │ │ Collapsible │           │
│  │ Renderer    │ │ Renderer    │ │ Renderer    │           │
│  └─────────────┘ └─────────────┘ └─────────────┘           │
└─────────────────────────────────────────────────────────────┘
```

---

## 📦 型定義

```typescript
// types/rich-content.ts

/** コンポーネントタイプ */
export type RichComponentType =
  | 'markdown'
  | 'code_block'
  | 'data_table'
  | 'chart'
  | 'citation'
  | 'collapsible'
  | 'link'
  | 'progress'
  | 'alert'
  | 'tabs'
  | 'timeline';

/** チャートタイプ */
export type ChartType = 'bar' | 'line' | 'pie' | 'scatter' | 'area' | 'radar';

/** アラートタイプ */
export type AlertType = 'info' | 'success' | 'warning' | 'error';

/** 基底コンポーネント */
export interface RichComponent {
  type: RichComponentType;
  id?: string;
  props: Record<string, unknown>;
  metadata?: Record<string, unknown>;
}

/** RichResponse (バックエンドからの応答) */
export interface RichResponse {
  components: RichComponent[];
  metadata?: Record<string, unknown>;
  createdAt?: string;
}
```

---

## 🔧 コンポーネント API

### メインレンダラー

```tsx
interface RichContentRendererProps {
  /** レンダリングするコンポーネント配列 */
  response: RichResponse;
  /** カスタムレンダラー（拡張用） */
  customRenderers?: Record<string, React.ComponentType<RichComponent>>;
  /** エラー時のフォールバック */
  fallback?: React.ReactNode;
  /** テーマ ('light' | 'dark') */
  theme?: 'light' | 'dark';
  /** クラス名 */
  className?: string;
}

export function RichContentRenderer(props: RichContentRendererProps): JSX.Element;
```

### 使用例

```tsx
import { RichContentRenderer } from '@/components/rich-content';

function ResultPanel({ data }: { data: RichResponse }) {
  return (
    <RichContentRenderer
      response={data}
      theme="dark"
      className="p-4"
    />
  );
}
```

---

## 📁 ファイル構成

```
studio/src/components/rich-content/
├── index.ts                    # エクスポート
├── RichContentRenderer.tsx     # メインレンダラー
├── types.ts                    # 型定義
├── renderers/
│   ├── MarkdownRenderer.tsx    # Markdown
│   ├── CodeBlockRenderer.tsx   # コード
│   ├── DataTableRenderer.tsx   # テーブル
│   ├── ChartRenderer.tsx       # チャート
│   ├── MathRenderer.tsx        # 数式
│   ├── CitationRenderer.tsx    # 引用
│   ├── AlertRenderer.tsx       # アラート
│   ├── TabsRenderer.tsx        # タブ
│   └── CollapsibleRenderer.tsx # 折りたたみ
└── hooks/
    └── useRichContent.ts       # カスタム Hook
```

---

## 🎯 実装優先度

1. **Phase 1**: `MarkdownRenderer`, `CodeBlockRenderer`, `AlertRenderer`
2. **Phase 2**: `DataTableRenderer`, `ChartRenderer`
3. **Phase 3**: `MathRenderer`, `CitationRenderer`, `TabsRenderer`

