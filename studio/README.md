# AgentFlow Studio

AgentFlow のビジュアルワークフローエディタ。React + TypeScript + React Flow で構築されたモダンな Web アプリケーション。

## 機能

- 🎨 **ビジュアルエディタ**: ドラッグ&ドロップでワークフローを作成
- 🔌 **エージェント統合**: インストール済みエージェントを簡単に配置
- ⚙️ **プロパティ編集**: ノードの設定をリアルタイムで編集
- 💾 **保存/読み込み**: ワークフローの永続化
- ↩️ **Undo/Redo**: 変更履歴の管理
- 🎯 **ミニマップ**: ワークフロー全体を俯瞰

## 技術スタック

- **React 18** - UI フレームワーク
- **TypeScript** - 型安全な開発
- **React Flow** - ビジュアルワークフローエディタ
- **Zustand** - 状態管理
- **Tailwind CSS** - スタイリング
- **Vite** - ビルドツール
- **shadcn/ui** - UI コンポーネント

## セットアップ

### 前提条件

- Node.js 18.0.0 以上
- npm 9.0.0 以上

### インストール

```bash
# 依存関係をインストール
npm install

# 開発サーバーを起動
npm run dev
```

開発サーバーは `http://localhost:3000` で起動します。

### ビルド

```bash
# プロダクションビルド
npm run build

# ビルドをプレビュー
npm run preview
```

## プロジェクト構造

```
studio/
├── src/
│   ├── components/          # React コンポーネント
│   │   ├── Canvas.tsx       # メインキャンバス
│   │   ├── AgentNode.tsx    # エージェントノード
│   │   ├── Sidebar.tsx      # サイドバー
│   │   └── PropertiesPanel.tsx  # プロパティパネル
│   ├── stores/              # Zustand ストア
│   │   └── workflowStore.ts # ワークフロー状態管理
│   ├── App.tsx              # メインアプリ
│   ├── main.tsx             # エントリーポイント
│   └── index.css            # グローバルスタイル
├── package.json
├── tsconfig.json
├── vite.config.ts
└── tailwind.config.js
```

## 開発

### コンポーネント

#### Canvas

メインのワークフローキャンバス。React Flow を使用してノードとエッジを管理。

#### AgentNode

カスタムノードコンポーネント。エージェントを表示し、設定ボタンを提供。

#### Sidebar

インストール済みエージェントの一覧を表示。ドラッグ&ドロップでキャンバスに配置可能。

#### PropertiesPanel

選択されたノードの設定を表示・編集。

### 状態管理

Zustand を使用してグローバル状態を管理：

```typescript
const { workflow, updateWorkflow, selectedNode, setSelectedNode } =
  useWorkflowStore();
```

### API 連携

バックエンド API との連携：

- `GET /api/agents` - エージェント一覧取得
- `GET /api/workflows/{id}` - ワークフロー取得
- `PUT /api/workflows/{id}` - ワークフロー保存

Vite の proxy 設定により、`/api` と `/ws` のリクエストは `http://localhost:8000` にプロキシされます。

## キーボードショートカット

- `Ctrl/Cmd + Z` - 元に戻す
- `Ctrl/Cmd + Shift + Z` - やり直す
- `Delete` - 選択ノード/エッジを削除
- `Ctrl/Cmd + S` - ワークフローを保存

## スタイリング

Tailwind CSS を使用したユーティリティファーストのスタイリング。

カスタムテーマは `tailwind.config.js` で定義：

```javascript
theme: {
  extend: {
    colors: {
      primary: 'hsl(var(--primary))',
      // ...
    }
  }
}
```

## テスト

```bash
# 型チェック
npm run type-check

# Lint
npm run lint
```

## デプロイ

```bash
# プロダクションビルド
npm run build

# dist/ ディレクトリを静的ホスティングサービスにデプロイ
```

## ライセンス

MIT License

## 関連リンク

- [React Flow Documentation](https://reactflow.dev/)
- [Zustand Documentation](https://zustand-demo.pmnd.rs/)
- [Tailwind CSS Documentation](https://tailwindcss.com/)
- [shadcn/ui Documentation](https://ui.shadcn.com/)
