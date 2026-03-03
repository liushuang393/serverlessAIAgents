# FAQ RAG 運用再設計 v3 — App 自己管理 + auth_service リソース標準化

> **作成日**: 2026-03-03
> **前身**: PLAN.md (v2) + unstaged 変更の Gap 分析結果
> **スコープ**: auth_service リソース標準化、FAQ App 自己管理、agentflow SDK 共通化、3段ルーティング、confidential 分離

---

## 1. 背景

### 1.1 PLAN.md (v2) との統合

PLAN.md は Platform 一元管理を前提としていたが、以下の方針転換を確定した：

| 項目 | PLAN.md (v2) | 本設計 (v3) |
|------|-------------|-------------|
| RAG 設定管理 | Platform が PATCH で変更 | 各 App が自分の UI/API で管理 |
| ファイル/DB 追加 | Platform UI → FAQ proxy | FAQ UI から直接追加 |
| access_control 配置 | app_config.json に埋込 | auth_service のリソース定義として一元管理 |
| 共通化 | contracts.rag | agentflow SDK に抽出 |

### 1.2 PLAN.md から継承する設計要素

- role_scope_map（role → 許可 scope）の概念
- scope_bindings（scope → backend + collection 命名規則）の概念
- 3段ルーティング（enterprise/general/unclear）
- confidential 専用 backend 分離
- Feature flag ロールアウト
- テスト計画 15 項目
- sqlglot ガード（DB 危険 SQL 拒否）

### 1.3 実装済み機能（変更不要）

| 機能 | 場所 |
|------|------|
| Platform RAG 読み取り専用（PATCH 405） | `apps/platform/routers/rag.py` |
| auth_client SDK + `require_permission()` | `agentflow/security/auth_client/` |
| JWT に permissions 埋込 | `apps/auth_service/core/jwt.py`, `service.py` |
| auth_service RBAC 全体 | roles, permissions, user_roles, role_permissions, resource_permissions |
| auth_service 管理 API（17+5） | router_authorization.py, router_admin.py |
| Docker Compose auth+FAQ 統合 | `docker-compose.auth-faq.yml` |
| FAQ auth モード簡素化 | `apps/faq_system/backend/auth/dependencies.py` |
| FAQ ingestion tracking | `migrations/versions/20260301_0002_ingestion_tracking.py` |

---

## 2. アーキテクチャ

```
┌──────────────────────────────────────────────────┐
│               auth_service (8010)                 │
│  ログイン / JWT 発行 / RBAC                       │
│  resource_definitions（リソース標準定義）[NEW]     │
│  resolve-scopes API（role→scope 解決）[NEW]       │
│  resource_permissions（role→resource→level）       │
└──────────┬──────────────────────────┬─────────────┘
           │ JWT (permissions 埋込)    │
     ┌─────┴──────┐           ┌───────┴───────┐
     │ Platform    │ ←閲覧→   │  FAQ App       │
     │ (8001)      │           │  (8005)        │
     │             │           │                │
     │ RAG設定閲覧  │           │ リソース管理UI  │ [NEW]
     │ App一覧表示  │           │ RAG設定CRUD    │ [NEW]
     │ ingest状況   │           │ ファイル投入    │
     │ (全て読取)   │           │ ingest実行     │
     │             │           │ チャット検索    │
     └─────────────┘           └───────┬────────┘
                                       │ 呼び出し
                                ┌──────┴────────┐
                                │ agentflow SDK  │
                                │                │
                                │ auth_client    │ ← 既存
                                │ resource_mgr   │ [NEW]
                                │ rag_access_ctl │ [NEW]
                                │ scope_resolver │ [NEW]
                                └────────────────┘
```

**原則:**
- auth_service = 認証・認可・リソース定義の唯一の権威
- Platform = 読み取り専用の可視化レイヤー（405 維持）
- 各 App = 自分のリソースを自分で管理（UI + API）
- agentflow SDK = 共通ロジック抽出（他 App 再利用可能）
- 各 App は agentflow SDK を呼ぶだけ。直接 auth_service HTTP を叩かない

---

## 3. auth_service リソース標準定義

### 3.1 resource_definitions テーブル [NEW]

既存の `resource_permissions` はロール×リソースのアクセスレベルを管理するが、
「どんなリソースが存在するか」のマスタ定義がない。これを追加する。

```sql
CREATE TABLE resource_definitions (
    id VARCHAR(64) PRIMARY KEY,
    resource_type VARCHAR(100) NOT NULL,    -- "vector_db", "business_db", "kb"
    resource_id VARCHAR(200) NOT NULL,      -- "faq__default__manager"
    display_name VARCHAR(200) NOT NULL,     -- "FAQ マネージャー用 KB"
    app_name VARCHAR(100),                  -- "faq_system"（所属 App）
    scope VARCHAR(100),                     -- "manager", "common", etc.
    backend_key VARCHAR(100),               -- "shared" or "confidential"
    metadata JSONB,                         -- collection_tpl, vector_url 等
    is_active BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW(),
    UNIQUE(resource_type, resource_id)
);
```

### 3.2 シードデータ（FAQ 用）

```python
FAQ_RESOURCE_DEFINITIONS = [
    {
        "resource_type": "vector_db",
        "resource_id": "faq__default__common",
        "display_name": "FAQ 共通ナレッジ",
        "app_name": "faq_system",
        "scope": "common",
        "backend_key": "shared",
        "metadata": {
            "collection_tpl": "{app}__{tenant}__{scope}",
            "vector_provider": "qdrant",
        },
    },
    {
        "resource_type": "vector_db",
        "resource_id": "faq__default__manager",
        "display_name": "FAQ マネージャー専用",
        "app_name": "faq_system",
        "scope": "manager",
        "backend_key": "shared",
        "metadata": {"collection_tpl": "{app}__{tenant}__{scope}"},
    },
    {
        "resource_type": "vector_db",
        "resource_id": "faq__default__sales",
        "display_name": "FAQ 営業専用",
        "app_name": "faq_system",
        "scope": "sales",
        "backend_key": "shared",
        "metadata": {"collection_tpl": "{app}__{tenant}__{scope}"},
    },
    {
        "resource_type": "vector_db",
        "resource_id": "faq__default__employee",
        "display_name": "FAQ 一般社員",
        "app_name": "faq_system",
        "scope": "employee",
        "backend_key": "shared",
        "metadata": {"collection_tpl": "{app}__{tenant}__{scope}"},
    },
    {
        "resource_type": "vector_db",
        "resource_id": "faq__default__confidential",
        "display_name": "FAQ 機密",
        "app_name": "faq_system",
        "scope": "confidential",
        "backend_key": "confidential",
        "metadata": {
            "collection_tpl": "{app}__{tenant}__{scope}",
            "vector_provider": "qdrant",
            "vector_url": "http://faq-qdrant-confidential:6333",
        },
    },
]
```

### 3.3 role → resource_permissions シード（PLAN.md role_scope_map 相当）

既存の `resource_permissions` テーブルを活用。

| role | resource_type | resource_id パターン | permission_level |
|------|--------------|---------------------|-----------------|
| admin | vector_db | `faq__*` | admin |
| manager | vector_db | `faq__*__manager` | read |
| manager | vector_db | `faq__*__common` | read |
| sales | vector_db | `faq__*__sales` | read |
| sales | vector_db | `faq__*__common` | read |
| employee | vector_db | `faq__*__employee` | read |
| employee | vector_db | `faq__*__common` | read |

未定義ロールは `AUTHZ_DEFAULT_OPEN=false` にした上で common のみ明示許可。

### 3.4 resolve-scopes API [NEW]

```
GET /auth/authorization/resolve-scopes?role={role}&app_name={app}&resource_type=vector_db
```

レスポンス:
```json
{
  "role": "manager",
  "app_name": "faq_system",
  "scopes": [
    {
      "scope": "manager",
      "resource_id": "faq__default__manager",
      "backend_key": "shared",
      "collection_tpl": "{app}__{tenant}__{scope}",
      "permission_level": "read"
    },
    {
      "scope": "common",
      "resource_id": "faq__default__common",
      "backend_key": "shared",
      "collection_tpl": "{app}__{tenant}__{scope}",
      "permission_level": "read"
    }
  ]
}
```

ロジック:
1. `resource_permissions` から role の許可リソース取得
2. `resource_definitions` で各リソースの scope, backend_key, metadata を結合
3. app_name でフィルタ

### 3.5 resource-definitions CRUD API [NEW]

```
GET    /auth/authorization/resource-definitions              → 一覧
GET    /auth/authorization/resource-definitions?app_name=faq_system → App 別
POST   /auth/authorization/resource-definitions              → 登録
PUT    /auth/authorization/resource-definitions/{id}         → 更新
DELETE /auth/authorization/resource-definitions/{id}         → 削除
```

---

## 4. agentflow SDK 共通コンポーネント

### 4.1 resource_manager.py [NEW]

```python
# agentflow/knowledge/resource_manager.py
"""auth_service リソース定義の CRUD ラッパー."""

class ResourceManager:
    """auth_service の resource_definitions API を呼び出す共通クライアント.

    各 App はこのクラスを使ってリソース登録・一覧・削除を行う。
    auth_service への HTTP 呼び出しを隠蔽し、他 App でも再利用可能。
    """

    def __init__(self, auth_client: AuthClient) -> None: ...
    async def list_resources(self, app_name: str, user: RemoteUser) -> list[ResourceDefinition]: ...
    async def register_resource(self, resource: ResourceDefinition) -> ResourceDefinition: ...
    async def update_resource(self, resource_id: str, updates: dict) -> ResourceDefinition: ...
    async def delete_resource(self, resource_id: str) -> None: ...
```

### 4.2 scope_resolver.py [NEW]

```python
# agentflow/knowledge/scope_resolver.py
"""JWT の role/tenant → 検索対象 collection 一覧を解決."""

@dataclass
class CollectionTarget:
    collection: str       # 実際の collection 名（例: "faq__default__manager"）
    backend_key: str      # "shared" or "confidential"
    vector_url: str | None  # confidential の場合は専用 URL
    scope: str            # "manager", "common", etc.

class ScopeResolver:
    """auth_service resolve-scopes API を呼び出し、collection 一覧を解決."""

    def __init__(self, auth_client: AuthClient) -> None: ...

    async def resolve_collections(
        self, user: RemoteUser, app_name: str, tenant_id: str | None = None
    ) -> list[CollectionTarget]:
        """role → scope → collection を解決.

        1. auth_service /auth/authorization/resolve-scopes を呼ぶ
        2. collection_tpl を tenant で展開
        3. CollectionTarget のリストを返す
        """
        ...
```

### 4.3 rag_access_control.py [NEW]

```python
# agentflow/knowledge/rag_access_control.py
"""role/scope 制限付き並列 RAG 検索."""

class RAGAccessControl:
    """許可された collection のみ検索し、結果を merge/rerank する共通エンジン."""

    def __init__(
        self, scope_resolver: ScopeResolver, rag_pipeline: RAGPipeline
    ) -> None: ...

    async def search(
        self,
        user: RemoteUser,
        query: str,
        app_name: str,
        top_k: int = 8,
    ) -> list[SearchResult]:
        """1. resolve_collections で許可 collection 取得
        2. shared / confidential を分けて並列検索
        3. score 正規化 → merge/dedup → rerank
        """
        ...
```

---

## 5. FAQ App 自己管理 API

### 5.1 リソース管理 API [NEW]

```python
# apps/faq_system/routers/resources.py [NEW]
# agentflow SDK を呼ぶだけの薄いルーター

@router.get("/api/resources")
async def list_resources(user=Depends(require_auth)):
    mgr = ResourceManager(auth_client=get_auth_client())
    return await mgr.list_resources(app_name="faq_system", user=user)

@router.post("/api/resources")
async def create_resource(req: ResourceCreateRequest, user=Depends(require_permission("faq:admin"))):
    mgr = ResourceManager(auth_client=get_auth_client())
    return await mgr.register_resource(req.to_definition(app_name="faq_system"))

@router.put("/api/resources/{resource_id}")
async def update_resource(resource_id: str, req: ResourceUpdateRequest, user=Depends(require_permission("faq:admin"))):
    ...

@router.delete("/api/resources/{resource_id}")
async def delete_resource(resource_id: str, user=Depends(require_permission("faq:admin"))):
    ...
```

### 5.2 RAG 設定管理 API [NEW]

```python
# apps/faq_system/routers/rag.py に追加

@router.get("/api/rag/config")
async def get_rag_config(user=Depends(require_auth)):
    """現在の RAG 設定取得（app_config.json から読み出し）."""
    ...

@router.patch("/api/rag/config")
async def patch_rag_config(req: RAGConfigPatchRequest, user=Depends(require_permission("faq:admin"))):
    """RAG 設定変更（chunk, retrieval, reranker 等）.
    app_config.json を更新し、RAGEngine をホットリロード。
    """
    ...

@router.get("/api/rag/config/access-control")
async def get_access_control(user=Depends(require_auth)):
    """アクセス制御設定（auth_service から取得）."""
    resolver = ScopeResolver(auth_client=get_auth_client())
    return await resolver.resolve_collections(user, app_name="faq_system")
```

### 5.3 既存 RAG query の改修

```python
# apps/faq_system/routers/rag.py の rag_query を改修

@router.post("/api/rag/query")
async def rag_query(request: RAGQueryRequest, user=Depends(require_auth)):
    # 変更前: 全 collection を検索
    # 変更後: role/tenant に応じた collection のみ検索
    if settings.FAQ_RAG_ACL_ENABLED:
        rac = RAGAccessControl(scope_resolver, rag_pipeline)
        results = await rac.search(user, request.question, app_name="faq_system")
    else:
        results = await rag_pipeline.query(request.question)
    return {"results": results}
```

### 5.4 upload/add の scope 対応

```python
@router.post("/api/rag/upload")
async def upload_file(file: UploadFile, scope: str = "common", user=Depends(require_permission("faq:write"))):
    """ファイルアップロード（scope 指定可）."""
    # scope → collection 解決
    collection = f"faq_system__{user.tenant_id}__{scope}"
    ...

@router.post("/api/rag/add")
async def add_document(req: RAGAddRequest, user=Depends(require_permission("faq:write"))):
    """ドキュメント追加（scope 指定可）."""
    scope = req.scope or "common"
    collection = f"faq_system__{user.tenant_id}__{scope}"
    ...
```

---

## 6. 3段ルーティング

PLAN.md §4 をそのまま採用。QueryClassifier は既に `apps/faq_system/backend/services/query_classifier.py` として計画済み（2026-02-28 FAQ改善計画）。

```
質問 → QueryClassifier
         │
         ├─ score ≥ 2 → enterprise → RAGAccessControl（scope 制限付き検索）
         ├─ score == 1 → general   → LLM 直接回答
         └─ score == 0 → unclear   → "お答えできません"
```

policy 補正:
- `who are you`, `今天天气` → `general` へ強制
- DB SQL → `sqlglot` ガードで DDL/DML 拒否、SELECT + LIMIT 強制

---

## 7. confidential 専用 backend

### 7.1 Docker サービス追加

```yaml
# docker-compose.auth-faq.yml に追加
faq-qdrant-confidential:
  image: qdrant/qdrant:latest
  ports:
    - "6334:6333"
  volumes:
    - faq-qdrant-conf-data:/qdrant/storage
  healthcheck:
    test: ["CMD", "curl", "-f", "http://localhost:6333/healthz"]
    interval: 10s
    timeout: 5s
    retries: 3
```

### 7.2 分離ロジック

`RAGAccessControl.search()` 内で `backend_key` に応じて接続先を切り替え:
- `shared` → `QDRANT_URL`（既定 `http://faq-qdrant:6333`）
- `confidential` → resource_definitions の metadata.vector_url

---

## 8. Feature Flag

```env
# auth_service 統合（既に実装済みベース）
FAQ_AUTH_SOURCE=auth_service          # auth_service | local_db

# RAG アクセス制御 [NEW]
FAQ_RAG_ACL_ENABLED=false             # true で role/scope 制限有効化

# confidential 分離 [NEW]
FAQ_CONFIDENTIAL_BACKEND_ENABLED=false # true で専用 backend 使用
```

段階投入: `FAQ_RAG_ACL_ENABLED=false` で既存動作を維持しつつ、有効化時のみ新ロジックを適用。

---

## 9. FAQ フロントエンド新画面

### 9.1 リソース管理タブ

- auth_service に登録されたリソース一覧表示
- リソース追加/編集/削除（admin のみ）
- リソースタイプ別フィルタ（vector_db, business_db, kb）

### 9.2 RAG 設定タブ

- 現在の RAG 設定表示（chunk, retrieval, reranker 等）
- 設定変更フォーム（admin のみ）
- パターンプリセット適用

### 9.3 データソース管理タブ

- ファイルアップロード（scope 指定）
- DB ソース設定（接続テスト + dry_run）
- ingest 実行 + 進捗表示

---

## 10. テスト計画（PLAN.md 継承 + 追加）

1. auth_service: resource_definitions CRUD が正しく動作する
2. auth_service: resolve-scopes が role→scope→backend を正しく解決する
3. auth_service: JWT の role/tenant で許可 scope が正しく解決される
4. 未定義ロールが `common` のみ検索可能である
5. confidential scope が専用 backend を使い、shared backend に書き込まれない
6. DB 危険 SQL（DDL/DML/multi statement）が拒否される
7. DB SELECT/CTE で LIMIT 強制と checkpoint 増分が動く
8. file upload と file URI の両導線で scope 付き ingest 成功する
9. `dry_run` で副作用ゼロ
10. async ingest 状態遷移が整合
11. `/api/chat` で企業質問は enterprise ルート、一般質問は chat ルート
12. 範囲外質問が `unclear` を返す
13. Platform の ingest runs 取得で非対応 app が 404 にならない
14. 既存回帰テストが通る
15. E2E: manager/sales/employee で検索結果集合が異なることを検証
16. [追加] agentflow SDK の ResourceManager/ScopeResolver が他 App からも使用可能
17. [追加] FAQ リソース管理 API が auth_service と正しく連携する
18. [追加] Feature flag OFF 時に既存動作が維持される

---

## 11. マイルストーン

| M | 内容 | DoD |
|---|------|-----|
| M1 | auth_service: resource_definitions + resolve-scopes + シード | テーブル作成、API 動作、FAQ 用シードデータ投入。テスト 1-3 green |
| M2 | agentflow SDK: resource_manager + scope_resolver + rag_access_control | 他 App から import 可能。テスト 16 green |
| M3 | FAQ: リソース管理 API + RAG 設定 API + query 改修 | scope 制限付き検索動作。テスト 4, 17, 18 green |
| M4 | FAQ: 3段ルーティング + confidential 分離 | enterprise/general/unclear 安定分岐。テスト 5-6, 11-12 green |
| M5 | FAQ: フロントエンド UI（リソース/RAG設定/データソース管理） | 画面から設定→投入→検索まで完結 |
| M6 | テスト + ドキュメント + 乖離修正 | 全テスト green。README がコードと一致。PLAN.md は本設計で置換 |

---

## 12. ドキュメント乖離修正リスト

| ファイル | 問題 | 修正 |
|---------|------|------|
| `apps/faq_system/README.md` | リソースパーミッション連携を実装済みと記載 | M3 完了まで「計画中」に修正、M3 後に実装済みへ更新 |
| `apps/platform/README.md` | RAG 管理を各 App に委譲と記載だが FAQ 側に管理 API がない | M3 完了後に FAQ 側 API/UI へのリンクを追加 |
| `apps/auth_service/README.md` | resource_definitions の概念がない | M1 完了後に追加 |

---

## 13. ロールアウト手順

1. M1-M2 完了後: Feature flag OFF のままデプロイ
2. dev 環境: `FAQ_RAG_ACL_ENABLED=true` で 3ロール × 2テナント検証
3. staging: ingest 再構築 + run 監視
4. 本番: read-only dry_run 1日運用後に write 有効化
5. `FAQ_CONFIDENTIAL_BACKEND_ENABLED=true` は機密データ投入前に有効化
