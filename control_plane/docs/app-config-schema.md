# app_config.json スキーマ仕様（P0 統一契約）

> 最終更新: 2026-02-23  
> 対象: 全 `apps/*/app_config.json`  
> 実装: `control_plane/schemas/app_config_schemas.py`

---

## 1. 目的

全 App の manifest 契約を統一し、`platform` が推論ではなく明示契約を優先して管理できる状態を作る。  
P0 では `contracts`、`blueprint`、`visibility` を全 App に補完する。

---

## 2. ルート構造

```json
{
  "name": "faq_system",
  "display_name": "FAQ システム",
  "description": "社内FAQ/SQL分析/営業資料画像生成",
  "business_base": "knowledge",
  "version": "1.0.0",
  "icon": "💬",
  "ports": { "api": 8001, "frontend": null, "db": 5433, "redis": null },
  "entry_points": {
    "api_module": "apps.faq_system.main:app",
    "health": "/health"
  },
  "agents": [
    {
      "name": "FAQAgent",
      "module": null,
      "capabilities": ["rag", "faq"],
      "business_base": "knowledge",
      "pattern": "specialist"
    }
  ],
  "services": {},
  "dependencies": { "database": "postgresql", "redis": false, "external": [] },
  "runtime": {
    "urls": {
      "backend": null,
      "frontend": null,
      "health": null,
      "database": null
    },
    "hosts": { "backend": "0.0.0.0", "frontend": null },
    "database": {
      "kind": null,
      "url": null,
      "host": null,
      "port": null,
      "name": null,
      "user": null,
      "password": null,
      "password_env": null,
      "note": null
    },
    "commands": {
      "backend_dev": null,
      "frontend_dev": null,
      "publish": null,
      "start": null,
      "stop": null
    }
  },
  "contracts": {
    "auth": {
      "enabled": false,
      "providers": [],
      "allow_anonymous": true,
      "required_scopes": [],
      "mode": "tenant_sso",
      "tenant_claim_key": "tenant_id",
      "allow_same_tenant_sso": false,
      "token_policy": {},
      "session_ttl_minutes": 60
    },
    "rag": {
      "enabled": true,
      "pattern": null,
      "provider": null,
      "collections": ["faq_system_knowledge"],
      "data_sources": [],
      "chunk_strategy": "recursive",
      "chunk_size": 800,
      "chunk_overlap": 120,
      "retrieval_method": "hybrid",
      "embedding_model": null,
      "rerank_model": null,
      "default_top_k": 5,
      "score_threshold": null,
      "indexing_schedule": null
    },
    "skills": {
      "auto_install": false,
      "hot_reload": true,
      "allowed_sources": [],
      "default_skills": []
    },
    "release": {
      "strategy": "manual",
      "targets": [],
      "environments": ["dev"],
      "require_approval": true
    }
  },
  "product_line": "faq",
  "surface_profile": "business",
  "audit_profile": "business",
  "evolution": {
    "enabled": true,
    "strategy_service_url": null,
    "validator_queue": {
      "backend": "redis_stream",
      "redis_url": "redis://localhost:6379/0",
      "stream_key": "evolution:validate:stream",
      "consumer_group": "evolution-validator-v1",
      "max_retries": 5
    },
    "scope_policy": ["tenant_app", "tenant_product_line", "global_verified"],
    "retrieval": {
      "high_confidence_skip_threshold": 0.82,
      "high_complexity_threshold": 0.7,
      "low_confidence_threshold": 0.55
    },
    "suspicion": {
      "max_age_days": 30,
      "failure_streak_threshold": 2,
      "performance_drop_ratio": 0.2
    }
  },
  "plugin_bindings": [
    {
      "id": "official.enterprise-connector-pack",
      "version": "1.0.0",
      "config": {}
    }
  ],
  "security_mode": null,
  "blueprint": {
    "engine_pattern": "simple",
    "flow_pattern": null,
    "system_prompt": "",
    "llm_provider": null,
    "llm_base_url": null,
    "llm_api_key_env": null,
    "default_model": null,
    "default_skills": [],
    "vector_db_provider": null,
    "vector_db_url": null,
    "vector_db_collection": null,
    "vector_db_api_key_env": null,
    "mcp_servers": [],
    "agents": []
  },
  "visibility": { "mode": "private", "tenants": [] },
  "tags": ["faq", "rag"]
}
```

---

## 3. P0 マイグレーション規則

`POST /api/studios/framework/apps/migrate-manifests` および `AppDiscoveryService.migrate_manifests()` は以下を保証する。

1. `contracts` が欠落時は `auth / rag / skills / release` を補完。
2. `contracts.rag` は `services.rag + services.vector_db + tags + agents.capabilities` から推論して補完。
3. `blueprint` が欠落時は補完し、`engine_pattern` は `services.engine.pattern` 優先、`services.workflow / services.pipeline` は `pipeline` として推論。
4. `business_base` が欠落時は `tags / contracts.rag / agents.capabilities` から推論して補完。
5. `agents[].business_base` と `agents[].pattern` が欠落時は推論して補完。
6. `visibility` が欠落時は `{ "mode": "private", "tenants": [] }` を補完。
7. 既存の業務独自フィールドは削除しない。
8. 再実行しても差分が増えない（幂等）。

---

## 4. RAG 設定の優先順位

RAG 概要サービスの抽出優先度は以下。

1. `contracts.rag`
2. `services.rag`
3. 推論値（タグ/Agent/デフォルト）

この順序により、明示契約が常に最優先される。

---

## 5. バリデーション要点

1. `name`: `^[a-z][a-z0-9_]*$`
2. `version`: SemVer
3. `ports.*`: 1024-65535
4. `agents[].name`: App 内で重複不可
5. `visibility.mode`: `private | public | tenant_allowlist`
6. `product_line`: `migration | faq | assistant | framework`
7. `surface_profile`: `business | developer | operator`
8. `audit_profile`: `business | developer`
9. `security_mode`: `read_only | approval_required | autonomous`（assistant 向け）
10. `evolution.scope_policy`: `tenant_app -> tenant_product_line -> global_verified`
11. `evolution.validator_queue.backend`: `redis_stream | none`
12. `contracts.auth.mode`: `tenant_sso | enterprise_isolated`（既定は `tenant_sso`。tenant SSO 運用時は `tenant_claim_key` / `required_scopes` を併用）

---

## 6. Plugin Manifest

プラグインは `plugins/<plugin_id>/plugin_manifest.json` で管理する。
必須項目は `id/version/type/capabilities/risk_tier/side_effects/required_permissions/signature/compatibility/tests_required`。

---

## 7. 運用フロー

1. 開発者が `app_config.json` を更新。
2. `POST /api/studios/framework/apps/migrate-manifests` を `dry_run=true` で確認。
3. 問題なければ `dry_run=false` で適用。
4. `GET /api/studios/framework/apps/summary` で `agent_count` / `has_api` / `contracts` を確認。

---

## 8. `runtime.cli` 契約（CLI 自動準備/診断）

`runtime` 配下に `cli` を追加し、App ごとに CLI 実行契約を上書きできる。

```json
{
  "runtime": {
    "commands": {
      "backend_dev": "python -m apps.example.main",
      "frontend_dev": "npm run dev",
      "publish": "docker compose up -d --build",
      "start": "docker compose up -d",
      "stop": "docker compose down"
    },
    "cli": {
      "preferred": ["codex", "claude"],
      "codex": {
        "executable": "codex",
        "install_commands": [["npm", "install", "-g", "@openai/codex"]],
        "auth": {
          "status": ["codex", "login", "status"],
          "api_key_env": "OPENAI_API_KEY",
          "api_key_login": ["codex", "login", "--with-api-key"],
          "interactive_login": ["codex", "login"]
        },
        "diagnostic_mode": "read_only",
        "diagnostic_command": [
          "codex",
          "exec",
          "--skip-git-repo-check",
          "--sandbox",
          "read-only"
        ],
        "repair_mode": "workspace_write",
        "repair_command": [
          "codex",
          "exec",
          "--skip-git-repo-check",
          "--sandbox",
          "workspace-write"
        ]
      },
      "claude": {
        "executable": "claude",
        "install_commands": [
          ["npm", "install", "-g", "@anthropic-ai/claude-code"]
        ],
        "auth": {
          "status": ["claude", "auth", "status", "--json"],
          "api_key_env": "ANTHROPIC_API_KEY",
          "api_key_login": null,
          "interactive_login": ["claude", "auth", "login"]
        },
        "diagnostic_mode": "plan",
        "diagnostic_command": ["claude", "-p", "--permission-mode", "plan"],
        "repair_mode": "accept_edits",
        "repair_command": ["claude", "-p", "--permission-mode", "acceptEdits"]
      }
    }
  }
}
```

### 認証順序（既定）

1. 既存ログイン状態 (`auth.status`) を確認
2. API Key で認証（`api_key_env` + `api_key_login`）
3. 対話ログイン手順（`interactive_login`）を提示

### 安全制約

- 診断モードは `read_only` / `plan` のみ
- 危険フラグ（`--dangerously-*`）は実行拒否
- 診断失敗は構造化エラーとして返し、主処理の応答に同梱

---

## 9. 起動コマンド解決優先度

`README.md` 優先は **開発起動コマンドのみ**（`backend_dev` / `frontend_dev`）に適用される。

1. `backend_dev` / `frontend_dev`: `README.md` → `runtime.commands` → fallback
2. `start` / `publish` / `stop`: `runtime.commands` → compose/process fallback
3. `start` / `stop` は実行前に `execution_mode`（`docker` / `local`）を判定し、選択モードのみ実行する

補足:

- `README.md` の前景 Python コマンドは `start` / `publish` の override に使わない
- API action response には `execution_mode` と `repair`（CLI 修復試行履歴）が同梱される
