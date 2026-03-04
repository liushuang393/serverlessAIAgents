"""JWT の role/tenant → 検索対象 collection 一覧を解決する共通モジュール.

auth_service の resolve-scopes API を呼び出し、
ユーザーの role + tenant から検索対象の collection 一覧を返す。
他 App からも再利用可能。

auth_service が利用不可の場合は、フォールバック RBAC マッピングにより
ロールに応じた KB タイプ（internal / external / confidential）で
アクセス制御を行う。
"""
from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import Any

import httpx


logger = logging.getLogger(__name__)

# auth_service 未接続時のフォールバック RBAC マッピング
FALLBACK_ROLE_KB_MAP: dict[str, list[str]] = {
    "admin": ["internal", "external", "confidential"],
    "manager": ["internal", "external"],
    "employee": ["internal", "external"],
    "guest": ["external"],
}


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

    @staticmethod
    def check_kb_type_access(role: str, kb_type: str) -> bool:
        """ロールが指定された KB タイプにアクセス可能か判定.

        auth_service 未接続時のフォールバック判定として使用。

        Args:
            role: ユーザーのロール名
            kb_type: KB 種別 (internal / external / confidential)

        Returns:
            アクセス可能なら True
        """
        allowed = FALLBACK_ROLE_KB_MAP.get(role, ["external"])
        return kb_type in allowed

    @staticmethod
    def get_allowed_kb_types(role: str) -> list[str]:
        """ロールに応じた許可 KB タイプ一覧を返す.

        Args:
            role: ユーザーのロール名

        Returns:
            許可された KB タイプのリスト
        """
        return FALLBACK_ROLE_KB_MAP.get(role, ["external"])
