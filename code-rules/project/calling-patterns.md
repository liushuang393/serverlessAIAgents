# 呼び出しパターン規約

> **バージョン**: 1.0.0
> **適用範囲**: 全 App のフロントエンド→バックエンド→ビジネスロジック間の呼び出し規約
> **最終更新**: 2026-04-04

---

## 📋 目次

1. [概要](#概要)
2. [バックエンド呼び出しパターン](#バックエンド呼び出しパターン)
3. [フロントエンド通信パターン](#フロントエンド通信パターン)
4. [認証パターン](#認証パターン)
5. [各 App の適用パターン](#各-app-の適用パターン)
6. [判定フローチャート](#判定フローチャート)

---

## 概要

Router（FastAPI エンドポイント）からビジネスロジックへの接続方式を 3 パターンに規範化する。
全 App はこの規約に従い、独自のオーケストレーション層を作らない。

### 全体像

```
Frontend (Browser)
  │
  │  REST / SSE / WebSocket
  ▼
FastAPI Router + Depends() 認証・DI
  │
  │  ← 以下の 3 パターンのいずれかで接続 →
  ▼
┌──────────────────────────────────────────────┐
│  パターン A: 単発処理                          │
│  Router → Service / A2AHub.call(agent)        │
├──────────────────────────────────────────────┤
│  パターン B: パイプライン処理                    │
│  Router → Engine.run() → Flow → Agent[]       │
├──────────────────────────────────────────────┤
│  パターン C: リアルタイム対話                    │
│  Router → Agent.run_stream() → SSE/WS         │
└──────────────────────────────────────────────┘
  │
  ▼
Infrastructure (LLM / VectorDB / Storage)
```

---

## バックエンド呼び出しパターン

### パターン A: 単発処理

**経路**: `Router → Service.execute()` or `Router → A2AHub.call(agent_name, inputs)`

**適用場面**:
- CRUD 操作（コレクション管理、ドキュメント操作、設定変更）
- 単一の検索・クエリ（RAG 検索、SQL クエリ、ヘルスチェック）
- 単一 Agent への質問応答（FAQ、分析依頼など 1 つの Agent で完結）
- 認証・認可処理

**必須規則**:
- Agent を呼ぶ場合は **必ず A2AHub 経由** (`hub.call(agent_name, inputs)`)。直接 `agent.run()` は禁止。
  - 理由: Agent 発見・ルーティング・監査が一元化される
- Service を呼ぶ場合は **Depends() で DI** し、lazy init + cache パターンを使用
- 戻り値は同期的に HTTP レスポンスとして返却

**コード例**:
```python
# Agent 呼び出し（A2AHub 経由必須）
@router.post("/api/chat")
async def chat(
    request: ChatRequest,
    user: UserInfo = Depends(require_auth),
) -> ChatResponse:
    agent = get_faq_agent()          # Depends + lazy init
    hub = get_hub()
    result = await hub.call(agent.name, {
        "question": request.message,
        "context": {"user": user.dict()},
    })
    return ChatResponse(**result)

# Service 呼び出し
@router.post("/api/rag/query")
async def rag_query(
    request: RAGQueryRequest,
    user: UserInfo = Depends(require_auth),
) -> RAGQueryResponse:
    service = get_rag_service(collection=request.collection)
    result = await service.execute(action="query", question=request.question)
    return RAGQueryResponse(**result)
```

---

### パターン B: パイプライン処理

**経路**: `Router → Engine.run() → Flow → Agent[]`

**適用場面**:
- 多段 Agent 協調（意思決定分析、コード移行、市場分析、コンテンツ生成）
- ステージ間にデータ依存関係がある処理
- 承認ゲート (HITL) が必要な処理
- レビュー + リトライループがある処理

**必須規則**:
- **必ず kernel Engine（BaseEngine / PipelineEngine）を継承** する
- Agent の生成は **AgentFactory** を使用
- ステージ構成は **app_config.json** に定義
- 独自オーケストレーション層（asyncio.gather 手動管理等）は禁止。kernel の `FlowBuilder.then_parallel()` を使用する

#### サブパターン B-1: 同期パイプライン

**経路**: `Router → Engine.run(inputs)` → 結果を待って HTTP レスポンス返却

**適用場面**:
- 処理時間が短い（数十秒以内）
- フロントエンドが結果全体を一括で必要とする

```python
@router.post("/api/decision")
async def process_decision(
    request: DecisionRequest,
    engine: DecisionEngine = Depends(get_engine),
) -> DecisionResponse:
    result = await engine.run(request.to_inputs())
    return DecisionResponse.from_result(result)
```

#### サブパターン B-2: 非同期パイプライン + SSE 進捗通知

**経路**: `Router → task_id 返却` + `Background Task → Engine.run_stream()` + `SSE endpoint でイベント配信`

**適用場面**:
- 処理時間が長い（数分〜数十分）
- フロントエンドに段階的な進捗通知が必要
- 中断・再開 (resume) が必要

**必須規則**:
- 起動エンドポイントは即座に `task_id` を返却
- 進捗は SSE エンドポイント (`GET /api/{resource}/{task_id}/stream`) で配信
- Engine は `run_stream()` を実装し、`AsyncIterator[dict]` でイベントを yield
- イベントフォーマットは AG-UI プロトコルに準拠

```python
# 起動エンドポイント
@router.post("/api/migrate/upload")
async def upload(request: UploadRequest) -> UploadResponse:
    task_id = create_task(request)
    asyncio.create_task(_run_pipeline(task_id, request))
    return UploadResponse(task_id=task_id, stream_url=f"/api/migrate/{task_id}/stream")

# SSE 配信エンドポイント
@router.get("/api/migrate/{task_id}/stream")
async def stream_events(task_id: str) -> EventSourceResponse:
    async def generate() -> AsyncIterator[str]:
        async for event in engine.run_stream(inputs):
            yield f"data: {json.dumps(event)}\n\n"
    return EventSourceResponse(generate(), media_type="text/event-stream")
```

#### サブパターン B-Coordinator: Coordinator Engine

**経路**: `Gateway → CoordinatorEngine.run() → IntentRouter → Specialist Agent`

**適用場面**:
- メッセージ受信 → 意図分類 → 専門 Agent ルーティングが必要なケース
- マルチチャネルゲートウェイ（Telegram/Slack/Discord 等）からの入力処理

**必須規則**:
- **kernel Engine（CoordinatorEngine or BaseEngine）を継承** する
  - 独自の Coordinator クラスで全ロジックを抱えない
- IntentRouter、ActionRouter は Engine 内部のコンポーネントとして実装
- Specialist Agent の呼び出しは **A2AHub 経由**

```python
class CoordinatorEngine(BaseEngine):
    """意図分類 → ルーティング → 専門 Agent 実行の Engine"""

    async def _execute(self, inputs: dict[str, Any]) -> dict[str, Any]:
        # 1. 意図分類
        intent = await self.intent_router.classify(inputs["message"])
        # 2. 専門 Agent 選択
        agent_name = self.action_router.resolve(intent)
        # 3. A2AHub 経由で実行
        result = await self.call_agent_by_name(agent_name, inputs)
        return result
```

---

### パターン C: リアルタイム対話

**経路**: `Frontend → SSE/WS → Router → Agent.run_stream() → イベント配信`

**適用場面**:
- チャット形式のストリーミング応答
- トークン単位の逐次表示

**必須規則**:
- Agent は `run_stream()` を実装し、`AsyncIterator[dict]` でイベントを yield
- Agent の取得は **A2AHub 経由**（パターン A と同様）

```python
# SSE チャットストリーム
@router.post("/api/chat/stream")
async def chat_stream(
    request: ChatRequest,
    user: UserInfo = Depends(require_auth),
) -> StreamingResponse:
    agent = get_faq_agent()
    async def generate() -> AsyncIterator[str]:
        async for event in agent.run_stream(request.to_inputs()):
            yield f"data: {json.dumps(event)}\n\n"
        yield "data: [DONE]\n\n"
    return StreamingResponse(generate(), media_type="text/event-stream")
```

---

## フロントエンド通信パターン

### 通信方式の選択基準

| 方式 | 実装 | 適用場面 | 不採用の場面 |
|------|------|----------|-------------|
| **REST** | `fetch()` | 全ての単発リクエスト（CRUD、検索、設定） | ストリーミングが必要な場合 |
| **SSE (EventSource)** | `new EventSource(url)` (GET) | パイプライン進捗通知（B-2）。リクエストボディ不要の場合 | POST が必要な場合 |
| **SSE (ReadableStream)** | `fetch(url, {method:"POST"})` + `response.body.getReader()` | チャットストリーム（C）。リクエストボディ（質問文、コンテキスト）の送信が必要な場合 | 自動再接続が必要な場合（EventSource を使う） |
| **WebSocket** | `new WebSocket(url)` | クライアント→サーバー送信が頻繁に必要な場合（リアルタイムチャット入力、コマンド送信） | 単方向ストリームで十分な場合 |

### 判定フロー

```
リクエストにボディが必要？
  ├── YES → レスポンスがストリーム？
  │         ├── YES → POST + ReadableStream (SSE)
  │         └── NO  → REST (fetch POST)
  └── NO  → レスポンスがストリーム？
            ├── YES → EventSource (GET SSE)
            │         ※ 自動再接続・Last-Event-ID 対応あり
            └── NO  → REST (fetch GET)

双方向通信が必要？（チャット入力の頻繁な送受信）
  └── YES → WebSocket
```

### SSE イベントフォーマット（標準）

```typescript
// パイプライン進捗イベント (B-2: EventSource)
type PipelineEvent =
  | { event_type: "node.start"; agent: string; stage: string }
  | { event_type: "progress"; progress: number; message: string }
  | { event_type: "node.complete"; agent: string; result: unknown }
  | { event_type: "flow.complete"; result: unknown }
  | { event_type: "flow.error"; error: string }

// チャットストリームイベント (C: ReadableStream)
type ChatStreamEvent =
  | { type: "token"; data: string }
  | { type: "progress"; progress: number; message: string; phase?: string }
  | { type: "result"; data: ChatResponse }
  | { type: "error"; message: string }
```

### フロントエンド実装標準

| 観点 | 標準 | 備考 |
|------|------|------|
| **HTTP Client** | `fetch()` ベース | axios は使用しない（バンドルサイズ削減） |
| **状態管理** | Zustand | persist middleware で localStorage 永続化 |
| **型安全** | TypeScript + interfaces | discriminated unions でイベント型を区別 |
| **認証** | Bearer token (Authorization header) | EventSource/WS は query param にフォールバック |
| **エラー処理** | カスタム `AppApiError` クラス | `isRetryable` フラグ、HTTP ステータスマッピング |
| **リトライ** | 指数バックオフ + jitter (max 3) | 5xx / 429 のみリトライ対象 |
| **SSE パーサー** | ステートフルバッファリング | TCP チャンク断片化に対応 |

---

## 認証パターン

| 通信方式 | 認証方式 | 実装 |
|----------|----------|------|
| REST (fetch) | Bearer token | `Authorization: Bearer ${token}` ヘッダー |
| SSE (EventSource GET) | Bearer token (query param) | `?access_token=${token}`（EventSource はヘッダー非対応） |
| SSE (ReadableStream POST) | Bearer token | `Authorization: Bearer ${token}` ヘッダー |
| WebSocket | Bearer token (query param) | `?access_token=${token}`（WS はヘッダー非対応） |

**共通規則**:
- トークンは `localStorage` に保存
- 401 レスポンス → トークン削除 + ログインページへリダイレクト
- Cookie 認証（`credentials: "include"`）は CSRF 保護が必要な特殊ケースのみ許可

---

## 各 App の適用パターン

### 各 App の準拠状況

| App | バックエンド | フロントエンド | 準拠状態 |
|-----|-------------|--------------|:---:|
| **faq_system** | A (A2AHub) + C (SSE stream) | fetch + Zustand + Bearer token | ✅ |
| **decision_governance** | B-1 + B-2 (PipelineEngine) | fetch + Zustand + Cookie 認証 (※1) | ✅ |
| **code_migration** | B-2 (BaseEngine + BackgroundTasks) | vanilla JS + SSE + API key | ✅ (※2) |
| **market_trend** | B-1 (kernel Flow) | fetch + Zustand | ✅ |
| **messaging_hub** | B-Coordinator (ResilientAgent + A2AHub) | fetch + WS + API key | ✅ |
| **legacy_modernization** | B-2 (BaseEngine + asyncio.gather) | fetch + SSE (再接続付き) | ✅ |
| **design_skills_engine** | B-1 (PipelineEngine) | FE なし | ✅ |
| **dev_studio** | A (Service 直接) | FE なし | ✅ |
| **orchestration_guardian** | A (A2AHub 経由) | FE なし | ✅ |

**※1**: decision_governance はバックエンドがセッション Cookie ベースで設計されているため、Cookie 認証は「CSRF 保護が必要な特殊ケース」として許容。
**※2**: code_migration のフロントエンドは vanilla JS (TypeScript 未使用)。将来的な TS 化は別途計画。

---

## 判定フローチャート

新しいエンドポイントを作る際の判定フロー:

```
このエンドポイントは何をする？
│
├── 単一の操作（CRUD/検索/質問応答）
│   └── → パターン A
│       Agent を使う？ → A2AHub.call() 経由
│       Service のみ？ → Depends() で DI
│
├── 複数 Agent の協調処理
│   ├── 処理時間が短い（〜30秒）
│   │   └── → パターン B-1（同期パイプライン）
│   ├── 処理時間が長い（数分〜）
│   │   └── → パターン B-2（非同期 + SSE）
│   └── 意図分類 → ルーティングが必要
│       └── → パターン B-Coordinator
│
└── ストリーミング応答（トークン逐次表示）
    └── → パターン C
        リクエストボディが必要？
        ├── YES → POST + ReadableStream
        └── NO  → GET + EventSource
```

---

## 禁止パターン

| ❌ 禁止 | ✅ 代替 | 理由 |
|---------|---------|------|
| Router → `agent.run()` 直接 | Router → `A2AHub.call(agent_name, inputs)` | Agent 発見・監査の一元化 |
| App 層で `asyncio.gather()` 手動並列管理 | `FlowBuilder.then_parallel([agents])` | kernel Flow で統一管理 |
| App 独自の Coordinator/Orchestrator クラス | `BaseEngine` / `PipelineEngine` を継承 | Engine 層で統一管理 |
| App 独自のイベントキュー（asyncio.Queue） | `Engine.run_stream()` の AsyncIterator | kernel の標準ストリーミング |
| axios の新規導入 | `fetch()` ベースの共通クライアント | バンドルサイズ・統一性 |
| 認証方式の App 独自実装 | Bearer token 標準方式 | セキュリティ統一 |
