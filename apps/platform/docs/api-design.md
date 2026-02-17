# Platform API 設計書（P0 安定化）

> 最終更新: 2026-02-15  
> 対象: `apps/platform/routers/*.py`  
> 互換方針: **破壊的変更を直接適用（双轨なし）**

---

## 1. 目的

`platform` を単なる App 一覧 UI から、全 App を横断する統一コントロールプレーンへ移行する。  
P0 では `rag / skill / agent` の分類・契約・出力形を統一し、前後端の型ドリフトを止める。

---

## 2. P0 で確定した API 変更

1. `GET /api/studios/framework/agents/by-app` は map 返却を廃止し、`groups` 配列を返す。
2. `GET /api/studios/framework/agents` と `GET /api/studios/framework/agents/search` は、`capabilities`（標準能力オブジェクト）と `capabilities_legacy`（旧タグ）を返す。
3. `GET /api/studios/framework/agents/capabilities` は標準能力集約 `{id, domain, task, qualifier, label, aliases, count, apps}` を返す。
4. `GET /api/studios/framework/skills` と `GET /api/studios/framework/skills/{name}` は `label` を必須返却する。
5. `GET /api/studios/framework/apps/summary` の件数字段は `agent_count` に統一し、`has_api` を返す。
6. `POST /api/studios/framework/apps/migrate-manifests` を新設し、全 `apps/*/app_config.json` の標準化を実行する。

---

## 3. エンドポイント契約（P0）

### 3.1 Apps

`GET /api/studios/framework/apps/summary`

```json
{
  "total_apps": 7,
  "total_agents": 18,
  "apps": [
    {
      "name": "faq_system",
      "display_name": "FAQ システム",
      "icon": "💬",
      "agent_count": 3,
      "has_api": true,
      "port": 8001,
      "config_path": "apps/faq_system/app_config.json",
      "contracts": {
        "auth": false,
        "rag": true,
        "skills": false,
        "release_targets": 0
      }
    }
  ],
  "errors": {}
}
```

`POST /api/studios/framework/apps/migrate-manifests`

Request:

```json
{ "dry_run": true }
```

Response:

```json
{
  "total": 7,
  "changed": 0,
  "unchanged": 7,
  "apps": [],
  "dry_run": true
}
```

### 3.2 Agents

`GET /api/studios/framework/agents`

```json
{
  "agents": [
    {
      "name": "RAGAgent",
      "app_name": "faq_system",
      "app_display_name": "FAQ システム",
      "app_icon": "💬",
      "module": "apps.faq_system.agent:RAGAgent",
      "capabilities": [
        {
          "id": "knowledge.retrieval.rag",
          "domain": "knowledge",
          "task": "retrieval",
          "qualifier": "rag",
          "label": "Retrieval Rag",
          "aliases": ["rag"]
        }
      ],
      "capabilities_legacy": ["rag", "search"]
    }
  ],
  "total": 1
}
```

`GET /api/studios/framework/agents/by-app`

```json
{
  "groups": [
    {
      "app_name": "faq_system",
      "display_name": "FAQ システム",
      "icon": "💬",
      "agents": []
    }
  ],
  "total_apps": 1
}
```

`GET /api/studios/framework/agents/capabilities`

```json
{
  "capabilities": [
    {
      "id": "knowledge.retrieval.rag",
      "domain": "knowledge",
      "task": "retrieval",
      "qualifier": "rag",
      "label": "Retrieval Rag",
      "aliases": ["rag"],
      "count": 3,
      "apps": ["faq_system", "market_trend_monitor"]
    }
  ],
  "total": 1
}
```

### 3.3 Skills

`GET /api/studios/framework/skills`

```json
{
  "skills": [
    {
      "name": "chatbot",
      "label": "Chatbot",
      "description": "汎用チャットボットスキル",
      "version": "1.0.0",
      "author": "AgentFlow Team",
      "tags": ["interaction.conversation.chat"],
      "tags_legacy": ["chat", "conversation"],
      "triggers": ["こんにちは"],
      "requirements": ["openai"],
      "examples": ["こんにちは、今日の天気は？"],
      "path": "agentflow/skills/builtin/chatbot/SKILL.md"
    }
  ],
  "total": 1
}
```

---

## 4. エラー契約

HTTP エラーは `detail` に構造化情報を返す。

```json
{
  "detail": {
    "message": "App not found: unknown_app",
    "error_code": "APP_NOT_FOUND"
  }
}
```

代表コード:

1. `APP_NOT_FOUND`（404）
2. `APP_CONFIG_NOT_FOUND`（404）
3. `APP_CONFIG_INVALID`（400）
4. `SKILL_NOT_FOUND`（404）

---

## 5. 直接置換ポリシー

1. 本 P0 は旧レスポンスとの二重提供を行わない。
2. フロント型は常に現行 API 契約を唯一正とする。
3. 破壊的変更は `docs/api-design.md` と `apps/platform/frontend/src/types/index.ts` を同時更新する。
