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
DEFAULT_AUTH_SERVICE_URL = "http://localhost:8010"


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
        auth_client: auth_service 互換クライアントインスタンス
    """

    def __init__(self, auth_client: Any) -> None:
        self._base_url = _resolve_auth_base_url(auth_client)

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


def _resolve_auth_base_url(auth_client: Any) -> str:
    """auth_client から auth_service のベース URL を解決."""
    base_url = getattr(auth_client, "base_url", None)
    if isinstance(base_url, str):
        normalized = base_url.strip().rstrip("/")
        if normalized:
            return normalized

    config = getattr(auth_client, "config", None)
    config_base_url = getattr(config, "base_url", None)
    if isinstance(config_base_url, str):
        normalized = config_base_url.strip().rstrip("/")
        if normalized:
            return normalized

    return DEFAULT_AUTH_SERVICE_URL
