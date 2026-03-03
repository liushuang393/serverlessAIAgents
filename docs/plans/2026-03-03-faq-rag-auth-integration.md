# FAQ RAG Auth 統合 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** auth_service にリソース標準定義を追加し、agentflow SDK に共通 RAG アクセス制御を抽出し、FAQ App が自己管理 API/UI で DB・ベクトルDB・RAG 設定を管理できるようにする。

**Architecture:** 設計書 `docs/plans/2026-03-03-faq-rag-auth-integration-design.md` に準拠。auth_service が resource_definitions + resolve-scopes を提供。agentflow SDK が scope_resolver + rag_access_control を共通化。FAQ App は薄いルーターで SDK を呼ぶ。Platform は読み取り専用を維持。

**Tech Stack:** Python 3.13+, FastAPI, SQLAlchemy (async), Pydantic v2, pytest, mypy, Ruff

---

### Task 1: auth_service — ResourceDefinition モデル追加

**Files:**
- Modify: `apps/auth_service/models/authorization.py` (末尾に追加)
- Modify: `apps/auth_service/models/__init__.py` (エクスポート追加)

**Step 1: テストを書く**

```python
# tests/apps/auth_service/test_models_resource_definition.py
"""ResourceDefinition モデルのユニットテスト."""
from __future__ import annotations

import pytest

from apps.auth_service.models.authorization import ResourceDefinition


class TestResourceDefinitionModel:
    """ResourceDefinition モデルの基本検証."""

    def test_tablename(self) -> None:
        assert ResourceDefinition.__tablename__ == "resource_definitions"

    def test_required_fields(self) -> None:
        """必須カラムが定義されている."""
        columns = {c.name for c in ResourceDefinition.__table__.columns}
        required = {
            "id", "resource_type", "resource_id", "display_name",
            "app_name", "scope", "backend_key", "metadata",
            "is_active", "created_at", "updated_at",
        }
        assert required.issubset(columns)

    def test_unique_constraint(self) -> None:
        """resource_type + resource_id のユニーク制約がある."""
        constraints = [
            c.name for c in ResourceDefinition.__table__.constraints
            if hasattr(c, "name") and c.name and "resource_def" in c.name
        ]
        assert len(constraints) >= 1
```

**Step 2: テストが失敗することを確認**

```bash
conda activate agentflow
pytest tests/apps/auth_service/test_models_resource_definition.py -v
```

期待: `ImportError: cannot import name 'ResourceDefinition'`

**Step 3: 実装**

`apps/auth_service/models/authorization.py` の末尾に追加:

```python
class ResourceDefinition(Base):
    """リソース定義マスタ.

    auth_service が管理するリソースの標準定義。
    各 App はこのテーブルに自分のリソース（vector_db, business_db, kb 等）を登録し、
    resource_permissions と組み合わせて role→scope→backend の解決を実現する。
    """

    __tablename__ = "resource_definitions"
    __table_args__ = (
        UniqueConstraint("resource_type", "resource_id", name="uq_resource_def"),
    )

    id: Mapped[str] = mapped_column(String(64), primary_key=True)
    resource_type: Mapped[str] = mapped_column(
        String(100), nullable=False, index=True
    )
    resource_id: Mapped[str] = mapped_column(String(200), nullable=False)
    display_name: Mapped[str] = mapped_column(String(200), nullable=False)
    app_name: Mapped[str] = mapped_column(String(100), default="", nullable=False)
    scope: Mapped[str] = mapped_column(String(100), default="", nullable=False)
    backend_key: Mapped[str] = mapped_column(
        String(100), default="shared", nullable=False
    )
    metadata_json: Mapped[str | None] = mapped_column(
        "metadata", Text, nullable=True
    )  # JSON 文字列（SQLite 互換）
    is_active: Mapped[bool] = mapped_column(
        Boolean, default=True, nullable=False
    )
    created_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True), default=_utcnow, nullable=False
    )
    updated_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True), default=_utcnow, onupdate=_utcnow, nullable=False
    )
```

注意: `metadata` は SQLAlchemy の予約語に近いため、カラム名は `metadata` だが属性名は `metadata_json` にする。

**Step 4: テストが通ることを確認**

```bash
pytest tests/apps/auth_service/test_models_resource_definition.py -v
```

---

### Task 2: auth_service — ResourceDefinition スキーマ + API

**Files:**
- Modify: `apps/auth_service/api/schemas_authorization.py` (末尾に追加)
- Modify: `apps/auth_service/api/router_authorization.py` (末尾にエンドポイント追加)

**Step 1: テストを書く**

```python
# tests/apps/auth_service/test_resource_definitions_api.py
"""resource-definitions API のテスト."""
from __future__ import annotations

import pytest
from httpx import ASGITransport, AsyncClient

from apps.auth_service.main import create_app


@pytest.fixture
def app():
    return create_app()


@pytest.fixture
async def client(app):
    transport = ASGITransport(app=app)
    async with AsyncClient(transport=transport, base_url="http://test") as c:
        yield c


@pytest.fixture
async def admin_token(client: AsyncClient) -> str:
    """管理者トークンを取得（seed 済み前提）."""
    resp = await client.post("/auth/login", json={
        "username": "admin", "password": "admin123",
    })
    return resp.json()["access_token"]


class TestResourceDefinitionsAPI:
    """resource-definitions CRUD テスト."""

    @pytest.mark.asyncio
    async def test_list_empty(self, client: AsyncClient, admin_token: str) -> None:
        resp = await client.get(
            "/auth/authorization/resource-definitions",
            headers={"Authorization": f"Bearer {admin_token}"},
        )
        assert resp.status_code == 200
        assert isinstance(resp.json(), list)

    @pytest.mark.asyncio
    async def test_create_and_get(self, client: AsyncClient, admin_token: str) -> None:
        headers = {"Authorization": f"Bearer {admin_token}"}
        payload = {
            "resource_type": "vector_db",
            "resource_id": "faq__default__common",
            "display_name": "FAQ 共通ナレッジ",
            "app_name": "faq_system",
            "scope": "common",
            "backend_key": "shared",
            "metadata": {"collection_tpl": "{app}__{tenant}__{scope}"},
        }
        resp = await client.post(
            "/auth/authorization/resource-definitions",
            json=payload,
            headers=headers,
        )
        assert resp.status_code == 201
        data = resp.json()
        assert data["resource_type"] == "vector_db"
        assert data["scope"] == "common"

        # 一覧取得で存在確認
        resp2 = await client.get(
            "/auth/authorization/resource-definitions",
            params={"app_name": "faq_system"},
            headers=headers,
        )
        assert resp2.status_code == 200
        assert len(resp2.json()) >= 1

    @pytest.mark.asyncio
    async def test_delete(self, client: AsyncClient, admin_token: str) -> None:
        headers = {"Authorization": f"Bearer {admin_token}"}
        # 作成
        resp = await client.post(
            "/auth/authorization/resource-definitions",
            json={
                "resource_type": "vector_db",
                "resource_id": "test__delete__target",
                "display_name": "削除テスト",
                "app_name": "test",
                "scope": "common",
            },
            headers=headers,
        )
        rd_id = resp.json()["id"]

        # 削除
        resp2 = await client.delete(
            f"/auth/authorization/resource-definitions/{rd_id}",
            headers=headers,
        )
        assert resp2.status_code == 200
```

**Step 2: テストが失敗することを確認**

```bash
pytest tests/apps/auth_service/test_resource_definitions_api.py -v
```

期待: 404 Not Found（エンドポイント未実装）

**Step 3: スキーマ追加**

`apps/auth_service/api/schemas_authorization.py` の末尾に追加:

```python
# ---------------------------------------------------------------------------
# リソース定義
# ---------------------------------------------------------------------------


class ResourceDefinitionRequest(BaseModel):
    """リソース定義作成/更新リクエスト."""

    resource_type: str = Field(..., min_length=1, max_length=100, description="リソース種別")
    resource_id: str = Field(..., min_length=1, max_length=200, description="リソースID")
    display_name: str = Field(..., min_length=1, max_length=200, description="表示名")
    app_name: str = Field("", max_length=100, description="所属 App 名")
    scope: str = Field("", max_length=100, description="スコープ名")
    backend_key: str = Field("shared", max_length=100, description="バックエンドキー")
    metadata: dict[str, Any] | None = Field(None, description="追加メタデータ（JSON）")


class ResourceDefinitionResponse(BaseModel):
    """リソース定義レスポンス."""

    id: str = Field(..., description="ID")
    resource_type: str = Field(..., description="リソース種別")
    resource_id: str = Field(..., description="リソースID")
    display_name: str = Field(..., description="表示名")
    app_name: str = Field("", description="所属 App 名")
    scope: str = Field("", description="スコープ名")
    backend_key: str = Field("shared", description="バックエンドキー")
    metadata: dict[str, Any] | None = Field(None, description="追加メタデータ")
    is_active: bool = Field(True, description="有効フラグ")
```

注意: `schemas_authorization.py` の `from __future__ import annotations` の後に `from typing import Any` を追加する必要あり。

**Step 4: API エンドポイント追加**

`apps/auth_service/api/router_authorization.py` の末尾に追加:

```python
# ---------------------------------------------------------------------------
# リソース定義 CRUD
# ---------------------------------------------------------------------------


@router.get("/resource-definitions", response_model=list[ResourceDefinitionResponse])
async def list_resource_definitions(
    app_name: str | None = None,
    resource_type: str | None = None,
    _user: UserInfo = Depends(require_auth_dependency),
) -> list[ResourceDefinitionResponse]:
    """リソース定義一覧を取得."""
    async with get_db_session() as session:
        stmt = select(ResourceDefinition).where(ResourceDefinition.is_active.is_(True))
        if app_name:
            stmt = stmt.where(ResourceDefinition.app_name == app_name)
        if resource_type:
            stmt = stmt.where(ResourceDefinition.resource_type == resource_type)
        stmt = stmt.order_by(ResourceDefinition.resource_type, ResourceDefinition.resource_id)
        result = await session.execute(stmt)
        definitions = result.scalars().all()
        return [_rd_to_response(rd) for rd in definitions]


@router.post(
    "/resource-definitions",
    response_model=ResourceDefinitionResponse,
    status_code=status.HTTP_201_CREATED,
)
async def create_resource_definition(
    req: ResourceDefinitionRequest,
    _user: UserInfo = Depends(require_permission("*")),
) -> ResourceDefinitionResponse:
    """リソース定義を作成."""
    import json as _json

    async with get_db_session() as session:
        existing = await session.scalar(
            select(ResourceDefinition).where(
                ResourceDefinition.resource_type == req.resource_type,
                ResourceDefinition.resource_id == req.resource_id,
            )
        )
        if existing is not None:
            raise HTTPException(status_code=409, detail="同一リソース定義が既に存在します")

        rd = ResourceDefinition(
            id=_gen_id("rd"),
            resource_type=req.resource_type,
            resource_id=req.resource_id,
            display_name=req.display_name,
            app_name=req.app_name,
            scope=req.scope,
            backend_key=req.backend_key,
            metadata_json=_json.dumps(req.metadata) if req.metadata else None,
        )
        session.add(rd)
        await session.commit()
        return _rd_to_response(rd)


@router.delete("/resource-definitions/{rd_id}")
async def delete_resource_definition(
    rd_id: str,
    _user: UserInfo = Depends(require_permission("*")),
) -> dict[str, str]:
    """リソース定義を削除."""
    async with get_db_session() as session:
        rd = await session.get(ResourceDefinition, rd_id)
        if rd is None:
            raise HTTPException(status_code=404, detail="リソース定義が見つかりません")
        await session.delete(rd)
        await session.commit()
        return {"message": "リソース定義を削除しました"}


def _rd_to_response(rd: ResourceDefinition) -> ResourceDefinitionResponse:
    """ResourceDefinition → レスポンス変換."""
    import json as _json

    metadata = None
    if rd.metadata_json:
        try:
            metadata = _json.loads(rd.metadata_json)
        except (ValueError, TypeError):
            metadata = None
    return ResourceDefinitionResponse(
        id=rd.id,
        resource_type=rd.resource_type,
        resource_id=rd.resource_id,
        display_name=rd.display_name,
        app_name=rd.app_name,
        scope=rd.scope,
        backend_key=rd.backend_key,
        metadata=metadata,
        is_active=rd.is_active,
    )
```

import 追加: ファイル先頭の import ブロックに `ResourceDefinition` と `ResourceDefinitionRequest`, `ResourceDefinitionResponse` を追加。

**Step 5: テストが通ることを確認**

```bash
pytest tests/apps/auth_service/test_resource_definitions_api.py -v
```

---

### Task 3: auth_service — resolve-scopes API

**Files:**
- Modify: `apps/auth_service/core/authorization.py` (メソッド追加)
- Modify: `apps/auth_service/api/schemas_authorization.py` (スキーマ追加)
- Modify: `apps/auth_service/api/router_authorization.py` (エンドポイント追加)

**Step 1: テストを書く**

```python
# tests/apps/auth_service/test_resolve_scopes.py
"""resolve-scopes API のテスト."""
from __future__ import annotations

import pytest
from httpx import ASGITransport, AsyncClient

from apps.auth_service.main import create_app


@pytest.fixture
def app():
    return create_app()


@pytest.fixture
async def client(app):
    transport = ASGITransport(app=app)
    async with AsyncClient(transport=transport, base_url="http://test") as c:
        yield c


@pytest.fixture
async def admin_token(client: AsyncClient) -> str:
    resp = await client.post("/auth/login", json={
        "username": "admin", "password": "admin123",
    })
    return resp.json()["access_token"]


@pytest.fixture
async def seeded_resources(client: AsyncClient, admin_token: str) -> None:
    """テスト用リソース定義 + resource_permissions をシード."""
    headers = {"Authorization": f"Bearer {admin_token}"}

    # リソース定義を登録
    for scope, bk in [("common", "shared"), ("manager", "shared"), ("confidential", "confidential")]:
        await client.post(
            "/auth/authorization/resource-definitions",
            json={
                "resource_type": "vector_db",
                "resource_id": f"faq__default__{scope}",
                "display_name": f"FAQ {scope}",
                "app_name": "faq_system",
                "scope": scope,
                "backend_key": bk,
                "metadata": {"collection_tpl": "{app}__{tenant}__{scope}"},
            },
            headers=headers,
        )

    # resource_permissions: manager → common(read), manager(read)
    for rid in ["faq__default__common", "faq__default__manager"]:
        await client.post(
            "/auth/authorization/resource-permissions",
            json={
                "role_name": "manager",
                "resource_type": "vector_db",
                "resource_id": rid,
                "permission_level": "read",
            },
            headers=headers,
        )


class TestResolveScopes:
    """resolve-scopes エンドポイントのテスト."""

    @pytest.mark.asyncio
    async def test_resolve_manager_scopes(
        self,
        client: AsyncClient,
        admin_token: str,
        seeded_resources: None,
    ) -> None:
        resp = await client.get(
            "/auth/authorization/resolve-scopes",
            params={"role": "manager", "app_name": "faq_system", "resource_type": "vector_db"},
            headers={"Authorization": f"Bearer {admin_token}"},
        )
        assert resp.status_code == 200
        data = resp.json()
        scopes = {s["scope"] for s in data["scopes"]}
        assert "common" in scopes
        assert "manager" in scopes
        assert "confidential" not in scopes  # manager に confidential 権限なし

    @pytest.mark.asyncio
    async def test_resolve_unknown_role_empty(
        self,
        client: AsyncClient,
        admin_token: str,
        seeded_resources: None,
    ) -> None:
        resp = await client.get(
            "/auth/authorization/resolve-scopes",
            params={"role": "unknown_role", "app_name": "faq_system", "resource_type": "vector_db"},
            headers={"Authorization": f"Bearer {admin_token}"},
        )
        assert resp.status_code == 200
        assert len(resp.json()["scopes"]) == 0
```

**Step 2: テストが失敗することを確認**

```bash
pytest tests/apps/auth_service/test_resolve_scopes.py -v
```

**Step 3: AuthorizationService にメソッド追加**

`apps/auth_service/core/authorization.py` の `invalidate_cache` メソッドの前に追加:

```python
    async def resolve_scopes(
        self,
        role_name: str,
        app_name: str,
        resource_type: str = "vector_db",
    ) -> list[dict[str, Any]]:
        """ロールが許可されたスコープ一覧を解決.

        resource_permissions と resource_definitions を結合し、
        ロールがアクセス可能なスコープ + backend 情報を返す。

        Args:
            role_name: ロール名
            app_name: App 名（フィルタ）
            resource_type: リソース種別（既定: vector_db）

        Returns:
            [{"scope": str, "resource_id": str, "backend_key": str,
              "collection_tpl": str, "permission_level": str}, ...]
        """
        from apps.auth_service.models.authorization import ResourceDefinition
        import json as _json

        async with get_db_session() as session:
            # ロール ID を取得
            role = await session.scalar(
                select(Role.id).where(Role.name == role_name)
            )
            if role is None:
                return []

            # resource_permissions から許可されたリソースを取得
            rp_result = await session.execute(
                select(ResourcePermission).where(
                    ResourcePermission.role_id == role,
                    ResourcePermission.resource_type == resource_type,
                    ResourcePermission.permission_level != "none",
                )
            )
            resource_perms = rp_result.scalars().all()

            if not resource_perms:
                return []

            # 許可された resource_id 一覧
            permitted_ids = {rp.resource_id for rp in resource_perms}
            perm_map = {rp.resource_id: rp.permission_level for rp in resource_perms}

            # resource_definitions から詳細情報を取得
            rd_result = await session.execute(
                select(ResourceDefinition).where(
                    ResourceDefinition.resource_type == resource_type,
                    ResourceDefinition.app_name == app_name,
                    ResourceDefinition.resource_id.in_(permitted_ids),
                    ResourceDefinition.is_active.is_(True),
                )
            )
            definitions = rd_result.scalars().all()

            scopes: list[dict[str, Any]] = []
            for rd in definitions:
                metadata = {}
                if rd.metadata_json:
                    try:
                        metadata = _json.loads(rd.metadata_json)
                    except (ValueError, TypeError):
                        pass
                scopes.append({
                    "scope": rd.scope,
                    "resource_id": rd.resource_id,
                    "backend_key": rd.backend_key,
                    "collection_tpl": metadata.get("collection_tpl", ""),
                    "permission_level": perm_map.get(rd.resource_id, "read"),
                    "metadata": metadata,
                })

            return scopes
```

**Step 4: スキーマ追加**

`apps/auth_service/api/schemas_authorization.py` 末尾:

```python
class ResolveScopesResponse(BaseModel):
    """スコープ解決レスポンス."""

    role: str = Field(..., description="ロール名")
    app_name: str = Field(..., description="App 名")
    resource_type: str = Field("vector_db", description="リソース種別")
    scopes: list[dict[str, Any]] = Field(default_factory=list, description="許可スコープ一覧")
```

**Step 5: API エンドポイント追加**

`apps/auth_service/api/router_authorization.py` の末尾に追加:

```python
@router.get("/resolve-scopes", response_model=ResolveScopesResponse)
async def resolve_scopes(
    role: str,
    app_name: str,
    resource_type: str = "vector_db",
    _user: UserInfo = Depends(require_auth_dependency),
) -> ResolveScopesResponse:
    """ロールの許可スコープを解決.

    resource_permissions × resource_definitions を結合し、
    指定ロールがアクセス可能なスコープ + backend 情報を返す。
    """
    authz = get_authorization_service()
    scopes = await authz.resolve_scopes(
        role_name=role, app_name=app_name, resource_type=resource_type
    )
    return ResolveScopesResponse(
        role=role, app_name=app_name, resource_type=resource_type, scopes=scopes
    )
```

**Step 6: テストが通ることを確認**

```bash
pytest tests/apps/auth_service/test_resolve_scopes.py -v
```

---

### Task 4: auth_service — FAQ 用リソース定義シード

**Files:**
- Modify: `apps/auth_service/db/seed_authorization.py` (末尾に関数追加)
- Modify: `apps/auth_service/main.py` (lifespan でシード呼び出し)

**Step 1: テストを書く**

```python
# tests/apps/auth_service/test_seed_resource_definitions.py
"""リソース定義シードのテスト."""
from __future__ import annotations

import pytest

from apps.auth_service.db.seed_authorization import seed_faq_resource_definitions


class TestSeedFAQResourceDefinitions:
    """FAQ リソース定義シードの検証."""

    @pytest.mark.asyncio
    async def test_seed_creates_5_definitions(self) -> None:
        """5 つのスコープ定義が作成される."""
        await seed_faq_resource_definitions()
        # 2回目は冪等
        await seed_faq_resource_definitions()

    @pytest.mark.asyncio
    async def test_seed_creates_resource_permissions(self) -> None:
        """各ロールの resource_permissions が作成される."""
        await seed_faq_resource_definitions()
```

**Step 2: 実装**

`apps/auth_service/db/seed_authorization.py` の末尾に追加:

```python
# デフォルト FAQ リソース定義
FAQ_RESOURCE_DEFINITIONS: list[dict[str, Any]] = [
    {
        "resource_type": "vector_db",
        "resource_id": "faq__default__common",
        "display_name": "FAQ 共通ナレッジ",
        "app_name": "faq_system",
        "scope": "common",
        "backend_key": "shared",
        "metadata": {"collection_tpl": "{app}__{tenant}__{scope}", "vector_provider": "qdrant"},
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

# ロール → 許可リソース マッピング（resource_permissions 用）
FAQ_ROLE_RESOURCE_MAP: dict[str, list[str]] = {
    "admin": [
        "faq__default__common", "faq__default__manager",
        "faq__default__sales", "faq__default__employee",
        "faq__default__confidential",
    ],
    "manager": ["faq__default__common", "faq__default__manager"],
    "sales": ["faq__default__common", "faq__default__sales"],  # sales ロールを別途追加する前提
    "employee": ["faq__default__common", "faq__default__employee"],
}


async def seed_faq_resource_definitions() -> None:
    """FAQ 用リソース定義と resource_permissions をシード（冪等）."""
    import json as _json

    from apps.auth_service.models.authorization import ResourceDefinition, ResourcePermission

    async with get_db_session() as session:
        # 既存チェック
        existing = await session.scalar(
            select(ResourceDefinition.id).where(
                ResourceDefinition.app_name == "faq_system"
            ).limit(1)
        )
        if existing is not None:
            logger.debug("FAQ リソース定義が既に存在するためスキップ")
            return

        # リソース定義作成
        for rd_def in FAQ_RESOURCE_DEFINITIONS:
            rd = ResourceDefinition(
                id=_gen_id("rd"),
                resource_type=rd_def["resource_type"],
                resource_id=rd_def["resource_id"],
                display_name=rd_def["display_name"],
                app_name=rd_def["app_name"],
                scope=rd_def["scope"],
                backend_key=rd_def["backend_key"],
                metadata_json=_json.dumps(rd_def.get("metadata")),
            )
            session.add(rd)

        # resource_permissions 作成
        for role_name, resource_ids in FAQ_ROLE_RESOURCE_MAP.items():
            role = await session.scalar(
                select(Role).where(Role.name == role_name)
            )
            if role is None:
                continue
            for resource_id in resource_ids:
                level = "admin" if role_name == "admin" else "read"
                session.add(ResourcePermission(
                    id=_gen_id("rsp"),
                    role_id=role.id,
                    resource_type="vector_db",
                    resource_id=resource_id,
                    permission_level=level,
                ))

        await session.commit()
        logger.info("FAQ リソース定義をシードしました: %d 件", len(FAQ_RESOURCE_DEFINITIONS))
```

import 追加: ファイル先頭に `from typing import Any` と `ResourceDefinition` の import を追加。

**Step 3: main.py のリスパンでシード呼び出し**

`apps/auth_service/main.py` の lifespan 内、`seed_authorization()` の後に追加:

```python
from apps.auth_service.db.seed_authorization import seed_faq_resource_definitions
await seed_faq_resource_definitions()
```

**Step 4: テストが通ることを確認**

```bash
pytest tests/apps/auth_service/test_seed_resource_definitions.py -v
```

---

### Task 5: agentflow SDK — ScopeResolver

**Files:**
- Create: `agentflow/knowledge/scope_resolver.py`
- Create: `tests/unit/knowledge/test_scope_resolver.py`

**Step 1: テストを書く**

```python
# tests/unit/knowledge/test_scope_resolver.py
"""ScopeResolver のユニットテスト."""
from __future__ import annotations

from unittest.mock import AsyncMock, MagicMock

import pytest

from agentflow.knowledge.scope_resolver import CollectionTarget, ScopeResolver


class TestScopeResolver:
    """ScopeResolver のテスト."""

    @pytest.fixture
    def mock_auth_client(self) -> MagicMock:
        client = MagicMock()
        client.base_url = "http://auth-service:8010"
        return client

    @pytest.fixture
    def resolver(self, mock_auth_client: MagicMock) -> ScopeResolver:
        return ScopeResolver(auth_client=mock_auth_client)

    def test_build_collection_name(self, resolver: ScopeResolver) -> None:
        """collection_tpl を展開して collection 名を生成."""
        name = resolver.build_collection_name(
            tpl="{app}__{tenant}__{scope}",
            app_name="faq_system",
            tenant_id="default",
            scope="manager",
        )
        assert name == "faq_system__default__manager"

    def test_build_collection_name_missing_tenant(self, resolver: ScopeResolver) -> None:
        """tenant_id が None の場合 'default' にフォールバック."""
        name = resolver.build_collection_name(
            tpl="{app}__{tenant}__{scope}",
            app_name="faq_system",
            tenant_id=None,
            scope="common",
        )
        assert name == "faq_system__default__common"
```

**Step 2: 実装**

```python
# agentflow/knowledge/scope_resolver.py
"""JWT の role/tenant → 検索対象 collection 一覧を解決する共通モジュール.

auth_service の resolve-scopes API を呼び出し、
ユーザーの role + tenant から検索対象の collection 一覧を返す。
他 App からも再利用可能。
"""
from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import Any

import httpx

logger = logging.getLogger(__name__)


@dataclass
class CollectionTarget:
    """検索対象 collection の情報."""

    collection: str
    scope: str
    backend_key: str = "shared"
    vector_url: str | None = None
    permission_level: str = "read"
    metadata: dict[str, Any] = field(default_factory=dict)


class ScopeResolver:
    """auth_service resolve-scopes API を呼び出し、collection 一覧を解決.

    各 App はこのクラスを使って、ユーザーの role + tenant から
    検索可能な collection 一覧を取得する。

    Args:
        auth_client: agentflow.security.auth_client.AuthClient インスタンス
    """

    def __init__(self, auth_client: Any) -> None:
        self._auth_client = auth_client

    async def resolve_collections(
        self,
        role: str,
        app_name: str,
        tenant_id: str | None = None,
        resource_type: str = "vector_db",
        token: str | None = None,
    ) -> list[CollectionTarget]:
        """role → scope → collection を解決.

        1. auth_service /auth/authorization/resolve-scopes を呼ぶ
        2. collection_tpl を tenant で展開
        3. CollectionTarget のリストを返す

        Args:
            role: ユーザーのロール名
            app_name: App 名
            tenant_id: テナントID（None の場合 "default"）
            resource_type: リソース種別
            token: 認証トークン（auth_service 呼び出し用）

        Returns:
            検索対象 CollectionTarget のリスト
        """
        effective_tenant = tenant_id or "default"
        base_url = getattr(self._auth_client, "base_url", "http://localhost:8010")

        url = f"{base_url}/auth/authorization/resolve-scopes"
        params = {
            "role": role,
            "app_name": app_name,
            "resource_type": resource_type,
        }
        headers: dict[str, str] = {}
        if token:
            headers["Authorization"] = f"Bearer {token}"

        try:
            async with httpx.AsyncClient(timeout=10.0) as client:
                resp = await client.get(url, params=params, headers=headers)
                resp.raise_for_status()
                data = resp.json()
        except Exception:
            logger.warning("resolve-scopes 呼び出しに失敗。空リストを返却します。")
            return []

        targets: list[CollectionTarget] = []
        for scope_info in data.get("scopes", []):
            tpl = scope_info.get("collection_tpl", "")
            collection = self.build_collection_name(
                tpl=tpl,
                app_name=app_name,
                tenant_id=effective_tenant,
                scope=scope_info.get("scope", ""),
            )
            metadata = scope_info.get("metadata", {})
            targets.append(CollectionTarget(
                collection=collection,
                scope=scope_info.get("scope", ""),
                backend_key=scope_info.get("backend_key", "shared"),
                vector_url=metadata.get("vector_url"),
                permission_level=scope_info.get("permission_level", "read"),
                metadata=metadata,
            ))

        return targets

    @staticmethod
    def build_collection_name(
        tpl: str,
        app_name: str,
        tenant_id: str | None,
        scope: str,
    ) -> str:
        """collection_tpl を展開して collection 名を生成.

        Args:
            tpl: テンプレート（例: "{app}__{tenant}__{scope}"）
            app_name: App 名
            tenant_id: テナントID（None → "default"）
            scope: スコープ名

        Returns:
            展開された collection 名
        """
        return tpl.format(
            app=app_name,
            tenant=tenant_id or "default",
            scope=scope,
        )
```

**Step 3: テストが通ることを確認**

```bash
pytest tests/unit/knowledge/test_scope_resolver.py -v
```

---

### Task 6: agentflow SDK — RAGAccessControl

**Files:**
- Create: `agentflow/knowledge/rag_access_control.py`
- Create: `tests/unit/knowledge/test_rag_access_control.py`

**Step 1: テストを書く**

```python
# tests/unit/knowledge/test_rag_access_control.py
"""RAGAccessControl のユニットテスト."""
from __future__ import annotations

from unittest.mock import AsyncMock

import pytest

from agentflow.knowledge.rag_access_control import RAGAccessControl
from agentflow.knowledge.scope_resolver import CollectionTarget, ScopeResolver


class TestRAGAccessControl:
    """RAGAccessControl のテスト."""

    @pytest.fixture
    def mock_resolver(self) -> AsyncMock:
        resolver = AsyncMock(spec=ScopeResolver)
        resolver.resolve_collections.return_value = [
            CollectionTarget(
                collection="faq__default__common",
                scope="common",
                backend_key="shared",
            ),
            CollectionTarget(
                collection="faq__default__manager",
                scope="manager",
                backend_key="shared",
            ),
        ]
        return resolver

    @pytest.fixture
    def rac(self, mock_resolver: AsyncMock) -> RAGAccessControl:
        return RAGAccessControl(scope_resolver=mock_resolver)

    @pytest.mark.asyncio
    async def test_get_search_targets(self, rac: RAGAccessControl, mock_resolver: AsyncMock) -> None:
        """role に応じた collection 一覧を返す."""
        targets = await rac.get_search_targets(
            role="manager", app_name="faq_system", tenant_id="default",
        )
        assert len(targets) == 2
        assert targets[0].collection == "faq__default__common"

    @pytest.mark.asyncio
    async def test_separate_by_backend(self, rac: RAGAccessControl) -> None:
        """shared と confidential を分離."""
        targets = [
            CollectionTarget(collection="a", scope="common", backend_key="shared"),
            CollectionTarget(collection="b", scope="conf", backend_key="confidential"),
        ]
        shared, confidential = rac.separate_by_backend(targets)
        assert len(shared) == 1
        assert len(confidential) == 1
        assert confidential[0].collection == "b"
```

**Step 2: 実装**

```python
# agentflow/knowledge/rag_access_control.py
"""role/scope 制限付き RAG 検索の共通モジュール.

ScopeResolver で許可された collection のみ検索し、
結果を merge する共通エンジン。他 App からも再利用可能。
"""
from __future__ import annotations

import logging
from typing import Any

from agentflow.knowledge.scope_resolver import CollectionTarget, ScopeResolver

logger = logging.getLogger(__name__)


class RAGAccessControl:
    """許可された collection のみ検索し、結果を merge する共通エンジン.

    Args:
        scope_resolver: ScopeResolver インスタンス
    """

    def __init__(self, scope_resolver: ScopeResolver) -> None:
        self._resolver = scope_resolver

    async def get_search_targets(
        self,
        role: str,
        app_name: str,
        tenant_id: str | None = None,
        token: str | None = None,
    ) -> list[CollectionTarget]:
        """ユーザーの role/tenant から検索対象 collection を取得.

        Args:
            role: ユーザーのロール名
            app_name: App 名
            tenant_id: テナントID
            token: 認証トークン

        Returns:
            検索対象 CollectionTarget のリスト
        """
        return await self._resolver.resolve_collections(
            role=role,
            app_name=app_name,
            tenant_id=tenant_id,
            token=token,
        )

    @staticmethod
    def separate_by_backend(
        targets: list[CollectionTarget],
    ) -> tuple[list[CollectionTarget], list[CollectionTarget]]:
        """shared と confidential を分離.

        Args:
            targets: CollectionTarget リスト

        Returns:
            (shared, confidential) のタプル
        """
        shared: list[CollectionTarget] = []
        confidential: list[CollectionTarget] = []
        for t in targets:
            if t.backend_key == "confidential":
                confidential.append(t)
            else:
                shared.append(t)
        return shared, confidential
```

**Step 3: テストが通ることを確認**

```bash
pytest tests/unit/knowledge/test_rag_access_control.py -v
```

---

### Task 7: agentflow SDK — ResourceManager

**Files:**
- Create: `agentflow/knowledge/resource_manager.py`
- Create: `tests/unit/knowledge/test_resource_manager.py`

**Step 1: テストを書く**

```python
# tests/unit/knowledge/test_resource_manager.py
"""ResourceManager のユニットテスト."""
from __future__ import annotations

from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from agentflow.knowledge.resource_manager import ResourceDefinition, ResourceManager


class TestResourceManager:
    """ResourceManager のテスト."""

    @pytest.fixture
    def mock_auth_client(self) -> MagicMock:
        client = MagicMock()
        client.base_url = "http://auth-service:8010"
        return client

    @pytest.fixture
    def manager(self, mock_auth_client: MagicMock) -> ResourceManager:
        return ResourceManager(auth_client=mock_auth_client)

    def test_resource_definition_dataclass(self) -> None:
        """ResourceDefinition データクラスの基本検証."""
        rd = ResourceDefinition(
            id="rd-1",
            resource_type="vector_db",
            resource_id="faq__default__common",
            display_name="FAQ 共通",
            app_name="faq_system",
            scope="common",
            backend_key="shared",
        )
        assert rd.resource_type == "vector_db"
        assert rd.scope == "common"
```

**Step 2: 実装**

```python
# agentflow/knowledge/resource_manager.py
"""auth_service リソース定義の CRUD ラッパー.

各 App はこのクラスを使ってリソース登録・一覧・削除を行う。
auth_service への HTTP 呼び出しを隠蔽し、他 App でも再利用可能。
"""
from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import Any

import httpx

logger = logging.getLogger(__name__)


@dataclass
class ResourceDefinition:
    """リソース定義データクラス."""

    id: str = ""
    resource_type: str = ""
    resource_id: str = ""
    display_name: str = ""
    app_name: str = ""
    scope: str = ""
    backend_key: str = "shared"
    metadata: dict[str, Any] = field(default_factory=dict)
    is_active: bool = True


class ResourceManager:
    """auth_service のリソース定義 API を呼び出す共通クライアント.

    Args:
        auth_client: agentflow.security.auth_client.AuthClient インスタンス
    """

    def __init__(self, auth_client: Any) -> None:
        self._base_url = getattr(auth_client, "base_url", "http://localhost:8010")

    async def list_resources(
        self,
        app_name: str,
        token: str | None = None,
        resource_type: str | None = None,
    ) -> list[ResourceDefinition]:
        """リソース定義一覧を取得.

        Args:
            app_name: App 名でフィルタ
            token: 認証トークン
            resource_type: リソース種別でフィルタ

        Returns:
            ResourceDefinition のリスト
        """
        url = f"{self._base_url}/auth/authorization/resource-definitions"
        params: dict[str, str] = {"app_name": app_name}
        if resource_type:
            params["resource_type"] = resource_type
        headers = self._auth_headers(token)

        try:
            async with httpx.AsyncClient(timeout=10.0) as client:
                resp = await client.get(url, params=params, headers=headers)
                resp.raise_for_status()
                return [self._to_definition(d) for d in resp.json()]
        except Exception:
            logger.warning("リソース定義一覧の取得に失敗")
            return []

    async def register_resource(
        self,
        resource: ResourceDefinition,
        token: str | None = None,
    ) -> ResourceDefinition:
        """リソース定義を登録.

        Args:
            resource: 登録するリソース定義
            token: 認証トークン

        Returns:
            登録された ResourceDefinition
        """
        url = f"{self._base_url}/auth/authorization/resource-definitions"
        payload = {
            "resource_type": resource.resource_type,
            "resource_id": resource.resource_id,
            "display_name": resource.display_name,
            "app_name": resource.app_name,
            "scope": resource.scope,
            "backend_key": resource.backend_key,
            "metadata": resource.metadata or None,
        }
        headers = self._auth_headers(token)

        async with httpx.AsyncClient(timeout=10.0) as client:
            resp = await client.post(url, json=payload, headers=headers)
            resp.raise_for_status()
            return self._to_definition(resp.json())

    async def delete_resource(
        self,
        resource_id: str,
        token: str | None = None,
    ) -> None:
        """リソース定義を削除.

        Args:
            resource_id: リソース定義の ID
            token: 認証トークン
        """
        url = f"{self._base_url}/auth/authorization/resource-definitions/{resource_id}"
        headers = self._auth_headers(token)

        async with httpx.AsyncClient(timeout=10.0) as client:
            resp = await client.delete(url, headers=headers)
            resp.raise_for_status()

    @staticmethod
    def _auth_headers(token: str | None) -> dict[str, str]:
        if token:
            return {"Authorization": f"Bearer {token}"}
        return {}

    @staticmethod
    def _to_definition(data: dict[str, Any]) -> ResourceDefinition:
        return ResourceDefinition(
            id=data.get("id", ""),
            resource_type=data.get("resource_type", ""),
            resource_id=data.get("resource_id", ""),
            display_name=data.get("display_name", ""),
            app_name=data.get("app_name", ""),
            scope=data.get("scope", ""),
            backend_key=data.get("backend_key", "shared"),
            metadata=data.get("metadata") or {},
            is_active=data.get("is_active", True),
        )
```

**Step 3: テストが通ることを確認**

```bash
pytest tests/unit/knowledge/test_resource_manager.py -v
```

---

### Task 8: agentflow SDK — __init__.py エクスポート

**Files:**
- Modify: `agentflow/knowledge/__init__.py`

**Step 1: エクスポート追加**

`agentflow/knowledge/__init__.py` に以下を追加:

```python
from agentflow.knowledge.rag_access_control import RAGAccessControl
from agentflow.knowledge.resource_manager import ResourceDefinition, ResourceManager
from agentflow.knowledge.scope_resolver import CollectionTarget, ScopeResolver

__all__ = [
    "CollectionTarget",
    "RAGAccessControl",
    "ResourceDefinition",
    "ResourceManager",
    "ScopeResolver",
]
```

既存の内容がある場合はマージする。

**Step 2: import テスト**

```bash
conda activate agentflow
python -c "from agentflow.knowledge import ScopeResolver, RAGAccessControl, ResourceManager; print('OK')"
```

---

### Task 9: FAQ — .env.example に Feature Flag 追加

**Files:**
- Modify: `apps/faq_system/.env.example`

**Step 1: 追加**

```env
# --- RAG アクセス制御（v3: App 自己管理） ---
# true にすると role/scope 制限が有効（auth_service resolve-scopes 連携）
FAQ_RAG_ACL_ENABLED=false
# confidential 専用 backend を使用
FAQ_CONFIDENTIAL_BACKEND_ENABLED=false
```

---

### Task 10: FAQ README — ドキュメント乖離修正

**Files:**
- Modify: `apps/faq_system/README.md`

**Step 1: 乖離修正**

README 内の「auth_service リソースパーミッション連携」セクションで、
現在「実装済み」のように書かれている箇所を以下のように修正:

- リソースパーミッション連携 → 「auth_service RBAC は実装済み。リソースレベルのアクセス制御（`FAQ_RAG_ACL_ENABLED=true` で有効化）は M3 で実装予定。」
- `check-resource` の使用例 → そのまま残すが「FAQ_RAG_ACL_ENABLED=true 時のみ有効」と注記

**Step 2: auth_service README にリソース定義セクション追加**

`apps/auth_service/README.md` に以下を追加:

- リソース定義 API（`/auth/authorization/resource-definitions`）の説明
- resolve-scopes API（`/auth/authorization/resolve-scopes`）の説明
- FAQ 用シードデータの一覧

---

### Task 11: Docker — confidential backend 追加

**Files:**
- Modify: `docker-compose.auth-faq.yml`

**Step 1: サービス追加**

`docker-compose.auth-faq.yml` の services セクションに追加:

```yaml
  faq-qdrant-confidential:
    image: qdrant/qdrant:latest
    ports:
      - "${FAQ_QDRANT_CONF_PORT:-6334}:6333"
    volumes:
      - faq-qdrant-conf-data:/qdrant/storage
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:6333/healthz"]
      interval: 10s
      timeout: 5s
      retries: 3
```

volumes セクションに追加:

```yaml
  faq-qdrant-conf-data:
```

---

### Task 12: 全テスト実行 + 型チェック

**Step 1: M1 テスト**

```bash
conda activate agentflow
pytest tests/apps/auth_service/ -v
```

**Step 2: M2 テスト**

```bash
pytest tests/unit/knowledge/test_scope_resolver.py tests/unit/knowledge/test_rag_access_control.py tests/unit/knowledge/test_resource_manager.py -v
```

**Step 3: 既存回帰テスト**

```bash
pytest tests/apps/platform/test_rag_router.py -v
```

**Step 4: 型チェック**

```bash
mypy agentflow/knowledge/scope_resolver.py agentflow/knowledge/rag_access_control.py agentflow/knowledge/resource_manager.py --strict
```

**Step 5: リント**

```bash
ruff check apps/auth_service/ agentflow/knowledge/
ruff format apps/auth_service/ agentflow/knowledge/
```

---

## 次フェーズ（M3-M6）概要

M3 以降は本計画の M1-M2 完了後に詳細化する。概要のみ記載:

### M3: FAQ App リソース管理 API + RAG query 改修

- `apps/faq_system/routers/resources.py` 新規作成（agentflow SDK ResourceManager を呼ぶ薄いルーター）
- `apps/faq_system/routers/rag.py` の `rag_query` を改修（`FAQ_RAG_ACL_ENABLED=true` 時に RAGAccessControl 経由で検索）
- `rag_upload` / `rag_add` に `scope` パラメータ追加

### M4: 3段ルーティング + confidential 分離

- `apps/faq_system/backend/services/query_classifier.py` は既に実装済み（2026-02-28 計画で作成）
- FAQ チャットルーターに 3段ルーティング統合
- RAGAccessControl の confidential 分離ロジック活用

### M5: FAQ フロントエンド UI

- リソース管理タブ、RAG設定タブ、データソース管理タブ

### M6: テスト + ドキュメント最終調整

- 設計書テスト計画 18 項目の全 green
- README 最終更新
