# app_config.json スキーマ仕様

> **最終更新**: 2026-02-14
> **対象**: 全 `apps/*/app_config.json`

---

## 1. 概要

`app_config.json` は各 App のメタデータを宣言するマニフェストファイル。
Platform がこのファイルをスキャンして App を自動発見・管理する。

### 設計原則

- **必須フィールドは最小限** — 既存 App への導入コストを下げる
- **Pydantic v2 でバリデーション** — 型安全を保証
- **後方互換** — `market_trend_monitor` の既存形式を包含する

---

## 2. スキーマ定義

```json
{
  "name": "faq_system",
  "display_name": "FAQ システム",
  "description": "社内FAQ/SQL分析/営業資料画像生成",
  "version": "1.0.0",
  "icon": "💬",

  "ports": {
    "api": 8001,
    "frontend": null,
    "db": 5433,
    "redis": null
  },

  "entry_points": {
    "api_module": "apps.faq_system.main:app",
    "health": "/health"
  },

  "agents": [
    {
      "name": "FAQAgent",
      "module": "agentflow.agents.faq_agent",
      "capabilities": ["faq", "rag", "sql"]
    }
  ],

  "services": {
    "rag": { "collections": ["faq_knowledge"] },
    "sql": { "dialect": "postgresql" }
  },

  "dependencies": {
    "database": "postgresql",
    "redis": false,
    "external": ["comfyui"]
  },

  "tags": ["faq", "rag", "sql"]
}
```

---

## 3. フィールド定義

### 必須フィールド

| フィールド | 型 | 説明 |
|-----------|-----|------|
| `name` | `str` | App 識別子（snake_case、ディレクトリ名と一致） |
| `display_name` | `str` | UI 表示用の名前 |
| `version` | `str` | セマンティックバージョニング |

### 任意フィールド

| フィールド | 型 | デフォルト | 説明 |
|-----------|-----|----------|------|
| `description` | `str` | `""` | App の説明文 |
| `icon` | `str` | `"📦"` | 絵文字アイコン |
| `ports.api` | `int \| null` | `null` | API ポート番号 |
| `ports.frontend` | `int \| null` | `null` | フロントエンドポート |
| `ports.db` | `int \| null` | `null` | DB ポート |
| `ports.redis` | `int \| null` | `null` | Redis ポート |
| `entry_points.api_module` | `str \| null` | `null` | FastAPI モジュールパス |
| `entry_points.health` | `str` | `"/health"` | ヘルスチェックパス |
| `agents` | `list[AgentInfo]` | `[]` | Agent 一覧 |
| `services` | `dict` | `{}` | 利用サービス情報 |
| `dependencies.database` | `str \| null` | `null` | DB 種別 |
| `dependencies.redis` | `bool` | `false` | Redis 使用有無 |
| `dependencies.external` | `list[str]` | `[]` | 外部依存 |
| `tags` | `list[str]` | `[]` | 検索用タグ |

### AgentInfo

| フィールド | 型 | 説明 |
|-----------|-----|------|
| `name` | `str` | Agent 名 |
| `module` | `str \| null` | Python モジュールパス |
| `capabilities` | `list[str]` | 能力タグ |

---

## 4. 後方互換性

`market_trend_monitor` の既存形式:

```json
{ "api_host": "0.0.0.0", "api_port": 8002, "frontend_port": 3002 }
```

新スキーマでは `api_host` → 不要（`entry_points` で管理）、
`api_port` → `ports.api`、`frontend_port` → `ports.frontend` に移行。
既存の `vite.config.ts` は `ports.api` / `ports.frontend` を読むよう更新する。

---

## 5. バリデーションルール

1. `name` は `^[a-z][a-z0-9_]*$` に一致すること
2. `ports.*` は 1024〜65535 の範囲
3. `version` はセマンティックバージョニング形式
4. `agents[].name` は App 内で一意
5. ファイルが存在しない App は Platform に表示されない（エラーではない）

