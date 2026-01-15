# 変更履歴

AgentFlow フレームワークの変更履歴。

---

## [2026-01-15] - 新機能追加

### ✨ 新機能

#### 1. フロントエンド富文本レンダリングコンポーネント (`RichContentRenderer`)

バックエンド A2UI `RichResponse` をフロントエンドで美しくレンダリングするコンポーネント群。

**対応コンポーネント:**

| タイプ | コンポーネント | 説明 |
|--------|---------------|------|
| `markdown` | `MarkdownRenderer` | Markdown テキストを HTML に変換 |
| `code_block` | `CodeBlockRenderer` | シンタックスハイライト付きコード表示 |
| `alert` | `AlertRenderer` | 情報・警告・エラーアラート |
| `data_table` | `DataTableRenderer` | ソート・ページネーション付きテーブル |
| `citation` | `CitationRenderer` | 引用元情報の表示 |
| `collapsible` | `CollapsibleRenderer` | 折りたたみセクション |
| `tabs` | `TabsRenderer` | タブ付きコンテンツ |

**使用例:**

```tsx
import { RichContentRenderer } from '@/components/rich-content';

function ResultPanel({ data }) {
  return (
    <RichContentRenderer
      response={data}
      theme="dark"
      className="p-4"
    />
  );
}
```

**ファイル構成:**

```
studio/src/components/rich-content/
├── index.ts                    # エクスポート
├── RichContentRenderer.tsx     # メインレンダラー
├── types.ts                    # 型定義
└── renderers/
    ├── MarkdownRenderer.tsx
    ├── CodeBlockRenderer.tsx
    ├── DataTableRenderer.tsx
    ├── AlertRenderer.tsx
    ├── CitationRenderer.tsx
    ├── CollapsibleRenderer.tsx
    └── TabsRenderer.tsx
```

---

#### 2. Agent 発見機構 (`AgentDiscovery`)

大規模デプロイメント向けの Agent 自動発見・登録・負荷分散機構。

**主な機能:**

- **動的登録/解除**: Agent の自動登録・発見
- **ヘルスチェック**: ハートビートによる生存確認
- **能力検索**: 特定能力を持つ Agent の検索
- **負荷分散**: 複数戦略対応（Round Robin / Random / Weighted）

**使用例:**

```python
from agentflow.discovery import AgentDiscovery, AgentEntry, AgentStatus

# 初期化
discovery = AgentDiscovery()

# Agent 登録
await discovery.register(AgentEntry(
    agent_id="agent-001",
    name="ResearchAgent",
    endpoint="http://localhost:8001",
    capabilities=["research", "summarize"],
    status=AgentStatus.HEALTHY,
))

# 能力による検索
agents = await discovery.discover(capability="research")

# 負荷分散で選択
agent = await discovery.select("research")

# ハートビート送信
await discovery.heartbeat("agent-001")
```

**負荷分散戦略:**

| 戦略 | 説明 |
|------|------|
| `ROUND_ROBIN` | 順番に選択（デフォルト） |
| `RANDOM` | ランダム選択 |
| `WEIGHTED` | 重み付き選択 |
| `LEAST_CONNECTIONS` | 最小接続数（将来実装） |

**ファイル構成:**

```
agentflow/discovery/
├── __init__.py     # エクスポート
├── base.py         # 基底クラス・型定義
├── registry.py     # InMemoryRegistry 実装
└── health.py       # ヘルスチェッカー
```

---

### 📝 ドキュメント

- `docs/design/RICH_CONTENT_RENDERER_DESIGN.md`: フロントエンド設計書
- `docs/design/AGENT_DISCOVERY_DESIGN.md`: Agent 発見機構設計書
- `docs/CHANGELOG_JA.md`: このファイル

---

### 🧪 テスト

- `tests/unit/test_agent_discovery.py`: Agent Discovery 単体テスト (10 件)

---

### 🔧 修正

- `datetime.utcnow()` の非推奨警告を修正（`datetime.now(UTC)` に変更）

---

## 今後の予定

1. **Chart レンダラー**: ECharts を使用したグラフ表示
2. **Math レンダラー**: KaTeX を使用した数式表示
3. **Redis ベース Registry**: 本番環境向け分散レジストリ
4. **Consul/etcd 連携**: 外部サービスディスカバリとの統合

