# AgentFlow Platform Runtime ガイド（日本語）

このドキュメントは、**マルチテナントSaaS** を前提とした AgentFlow の運用ガイドです。
実行時の副作用を抑え、**テナント隔離・可観測性・拡張性**を高めるための標準的な手順をまとめます。

---

## 1. 明示的初期化（P0）

従来の import 副作用による `.env` 読み込みは廃止し、**`init_agentflow()` による明示的初期化**を推奨します。

```python
from agentflow import init_agentflow

# .env 読み込みを有効にする（必要に応じて）
init_agentflow(load_env=True)
```

---

## 2. RuntimeContext（P0）

マルチテナントでは **設定・プロバイダー・ログ** をテナント単位で分離する必要があります。
`RuntimeContext` を使って、**設定・テナント情報・トレース情報** を束ねます。

```python
from agentflow import RuntimeContext, use_runtime_context, get_llm
from agentflow.config import AgentFlowSettings

settings = AgentFlowSettings(openai_api_key="sk-...", openai_model="gpt-4o")
ctx = RuntimeContext(tenant_id="tenant-001", request_id="req-123", settings=settings)

with use_runtime_context(ctx):
    llm = get_llm(context=ctx)
```

### 付随効果
- ログに `tenant_id`, `request_id`, `trace_id` が自動的に付与されます。
- Provider は `context.settings` に基づき分離されます。

---

## 3. イベント標準化（P0）

フロー実行イベントは AG-UI 標準イベントに統一されました。
従来の `type` フィールド互換は **legacy 互換層** で維持します。

期待される形式:
- `event_type`: `flow.start` / `node.complete` など
- `type`: legacy 互換名（必要に応じて）

---

## 4. ストレージとテナント情報（P1）

`ResultStore` は `tenant_id` を保持できます。  
`RuntimeContext` が設定されている場合、自動的に埋め込まれます。

```python
from agentflow.core.result_store import ResultStoreManager

await ResultStoreManager.save("result-1", {"ok": True}, flow_id="flow-1")
```

---

## 5. Provider 設定の分離（P1）

`get_llm / get_db / get_vectordb / get_embedding` は  
`context` 引数を受け取り、**テナント単位の設定**を適用します。

```python
from agentflow import RuntimeContext
from agentflow.providers import get_db

ctx = RuntimeContext(tenant_id="tenant-002", settings=AgentFlowSettings(...))
db = get_db(context=ctx)
```

---

## 6. 段階的ロードマップ（P0 / P1 / P2）

### P0（基盤）
- 明示的初期化
- RuntimeContext 導入
- AG-UI 事件標準化
- Provider 単位のコンテキスト分離

### P1（拡張）
- ResultStore の非同期 I/O
- テナント情報の永続化
- 監査/観測の強化

### P2（拡張性）
- `agentflow-core` / `agentflow-extras` の分離
- プロトコルゲートウェイの統一

---

## 7. 注意事項

- 既存 API は可能な限り互換を維持しています。
- 互換のための legacy 形式は、将来的に段階的に廃止予定です。
