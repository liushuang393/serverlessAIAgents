"""app_config 契約駆動の HTTP/WS 認証ガード."""

from __future__ import annotations

import os
import re
from collections.abc import Awaitable, Callable, Mapping
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any, TypeVar

from fastapi import HTTPException, Request, WebSocket
from fastapi.responses import JSONResponse

from shared.config.manifest import load_app_manifest_dict


if TYPE_CHECKING:
    from pathlib import Path


_T = TypeVar("_T")
_TENANT_PATH_PATTERN = re.compile(r"/tenants/([^/]+)", re.IGNORECASE)
_AUTH_SERVICE_PROVIDER = "auth_service"
_API_KEY_PROVIDER = "api_key"
_DEFAULT_AUTH_SERVICE_URL_ENV = "AUTH_SERVICE_URL"
_DEFAULT_AUTH_SERVICE_SECRET_ENV = "AUTH_SERVICE_JWT_SECRET"
_DEFAULT_AUTH_SERVICE_ALGORITHM_ENV = "AUTH_SERVICE_JWT_ALGORITHM"
_DEFAULT_AUTH_SERVICE_ISSUER_ENV = "AUTH_SERVICE_JWT_ISSUER"
_DEFAULT_AUTH_SERVICE_AUDIENCE_ENV = "AUTH_SERVICE_JWT_AUDIENCE"

HTTPAuthBackend = Callable[[Request], Awaitable[_T | None] | _T | None]
WSAuthBackend = Callable[[WebSocket], Awaitable[_T | None] | _T | None]


@dataclass(slots=True)
class AuthPrincipal:
    """auth_service から解決した標準 principal."""

    user_id: str
    username: str
    tenant_id: str | None = None
    role: str = "employee"
    roles: list[str] = field(default_factory=list)
    scopes: list[str] = field(default_factory=list)
    permissions: list[str] = field(default_factory=list)
    azp: str | None = None
    email: str | None = None
    display_name: str = ""
    department: str = ""
    position: str = ""
    claims: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """principal を辞書へ変換."""
        return {
            "user_id": self.user_id,
            "username": self.username,
            "tenant_id": self.tenant_id,
            "role": self.role,
            "roles": list(self.roles),
            "scopes": list(self.scopes),
            "permissions": list(self.permissions),
            "azp": self.azp,
            "email": self.email,
            "display_name": self.display_name,
            "department": self.department,
            "position": self.position,
            "claims": dict(self.claims),
        }


@dataclass(slots=True)
class ContractAuthGuardConfig:
    """契約駆動認証ガード設定."""

    app_config_path: Path
    public_http_paths: set[str] = field(default_factory=set)
    http_path_prefixes: tuple[str, ...] = ("/api/",)
    auth_header_name: str = "x-api-key"
    ws_query_key: str = "api_key"
    browser_token_query_key: str = "access_token"
    api_key_env_selector_var: str | None = None
    default_api_key_env_var: str = "APP_API_KEY"
    tenant_header_name: str = "x-tenant-id"


class ContractAuthGuard:
    """contracts.auth を基準に HTTP/WS 認証を統一する."""

    def __init__(
        self,
        config: ContractAuthGuardConfig,
        *,
        http_backend: HTTPAuthBackend[Any] | None = None,
        ws_backend: WSAuthBackend[Any] | None = None,
        auth_client_factory: Callable[..., Any] | None = None,
    ) -> None:
        self._config = config
        self._http_backend = http_backend
        self._ws_backend = ws_backend
        if auth_client_factory is not None:
            self._auth_client_factory = auth_client_factory
        else:
            # 遅延 import: infrastructure 層への直接依存を回避
            from infrastructure.security.auth_client import AuthClient
            self._auth_client_factory = AuthClient
        self._cached_app_config: dict[str, Any] | None = None

    def load_app_config(self) -> dict[str, Any]:
        """app_config.json を読み込む."""
        if self._cached_app_config is not None:
            return self._cached_app_config
        path = self._config.app_config_path
        if not path.is_file():
            self._cached_app_config = {}
            return self._cached_app_config
        try:
            self._cached_app_config = load_app_manifest_dict(path)
        except (OSError, ValueError):
            self._cached_app_config = {}
        return self._cached_app_config

    def auth_contract(self) -> dict[str, Any]:
        """contracts.auth セクションを返す."""
        raw = self.load_app_config()
        contracts = raw.get("contracts", {})
        if not isinstance(contracts, dict):
            return {}
        auth = contracts.get("auth", {})
        if not isinstance(auth, dict):
            return {}
        return auth

    def auth_providers(self) -> tuple[str, ...]:
        """正規化済み provider 順序を返す."""
        auth = self.auth_contract()
        raw_providers = auth.get("providers")
        providers = _normalize_string_list(raw_providers)
        if not providers:
            return (_API_KEY_PROVIDER,)
        return tuple(providers)

    def is_auth_required(self) -> bool:
        """匿名禁止 + auth enabled かを判定."""
        auth = self.auth_contract()
        enabled = bool(auth.get("enabled", False))
        allow_anonymous = bool(auth.get("allow_anonymous", True))
        return enabled and not allow_anonymous

    def should_protect_http_path(self, path: str) -> bool:
        """HTTP 保護対象かを判定."""
        if path in self._config.public_http_paths:
            return False
        return any(path.startswith(prefix) for prefix in self._config.http_path_prefixes)

    def _api_key_env_name(self) -> str:
        selector = self._config.api_key_env_selector_var
        if selector:
            selected = os.getenv(selector, "").strip()
            if selected:
                return selected
        return self._config.default_api_key_env_var

    def verify_api_key(self, incoming_key: str | None) -> None:
        """API key provider を検証."""
        if not self.is_auth_required():
            return

        env_name = self._api_key_env_name()
        expected_key = os.getenv(env_name)
        if not expected_key:
            raise HTTPException(
                status_code=503,
                detail=f"Auth required but env '{env_name}' is not configured",
            )
        if incoming_key != expected_key:
            raise HTTPException(status_code=401, detail="Invalid API key")

    async def require_http(self, request: Request) -> Any | None:
        """HTTP リクエストの認証を強制する."""
        if not self.should_protect_http_path(request.url.path):
            return None
        if not self.is_auth_required():
            return None

        resolved = await self._resolve_http_principal(request)
        if resolved is not None:
            request.state.auth_principal = resolved
        return resolved

    async def require_ws(self, websocket: WebSocket) -> tuple[bool, Any | None]:
        """WS/SSE ハンドシェイク認証を強制する."""
        if not self.is_auth_required():
            return True, None

        try:
            resolved = await self._resolve_ws_principal(websocket)
            return True, resolved
        except HTTPException as exc:
            close_code = 1011
            if exc.status_code == 401:
                close_code = 4401
            elif exc.status_code == 403:
                close_code = 4403
            await websocket.close(code=close_code, reason=str(exc.detail))
            return False, None

    async def http_middleware(self, request: Request, call_next: Any) -> Any:
        """FastAPI middleware 連携."""
        try:
            await self.require_http(request)
        except HTTPException as exc:
            return JSONResponse({"detail": exc.detail}, status_code=exc.status_code)
        return await call_next(request)

    def reset_cache(self) -> None:
        """app_config キャッシュを破棄."""
        self._cached_app_config = None

    async def _resolve_http_principal(self, request: Request) -> Any | None:
        if self._http_backend is not None:
            user = await _maybe_await(self._http_backend(request))
            if user is None:
                raise HTTPException(status_code=401, detail="Unauthorized")
            return user

        auth = self.auth_contract()
        for provider in self.auth_providers():
            if provider == _AUTH_SERVICE_PROVIDER:
                return await self._resolve_auth_service_http(request, auth)
            if provider == _API_KEY_PROVIDER:
                incoming_key = request.headers.get(self._config.auth_header_name)
                self.verify_api_key(incoming_key)
                return None

        raise HTTPException(status_code=503, detail="No supported auth provider configured")

    async def _resolve_ws_principal(self, websocket: WebSocket) -> Any | None:
        if self._ws_backend is not None:
            user = await _maybe_await(self._ws_backend(websocket))
            if user is None:
                raise HTTPException(status_code=401, detail="Unauthorized")
            return user

        auth = self.auth_contract()
        for provider in self.auth_providers():
            if provider == _AUTH_SERVICE_PROVIDER:
                return await self._resolve_auth_service_ws(websocket, auth)
            if provider == _API_KEY_PROVIDER:
                incoming_key = websocket.headers.get(
                    self._config.auth_header_name,
                ) or websocket.query_params.get(self._config.ws_query_key)
                self.verify_api_key(incoming_key)
                return None

        raise HTTPException(status_code=503, detail="No supported auth provider configured")

    async def _resolve_auth_service_http(
        self,
        request: Request,
        auth: Mapping[str, Any],
    ) -> AuthPrincipal:
        client = self._build_auth_client(auth)
        token = self._extract_http_token(request)
        if token is None:
            raise HTTPException(status_code=401, detail="Missing bearer token")
        remote_user = await client.verify_token(token)
        if remote_user is None:
            raise HTTPException(status_code=401, detail="Invalid bearer token")
        principal = self._build_principal(remote_user)
        self._enforce_contract_policy(
            principal,
            auth=auth,
            request_tenant=self._resolve_request_tenant(
                path=request.url.path,
                headers=request.headers,
                query_params=request.query_params,
            ),
        )
        return principal

    async def _resolve_auth_service_ws(
        self,
        websocket: WebSocket,
        auth: Mapping[str, Any],
    ) -> AuthPrincipal:
        client = self._build_auth_client(auth)
        token = self._extract_websocket_token(websocket)
        if token is None:
            raise HTTPException(status_code=401, detail="Missing bearer token")
        remote_user = await client.verify_token(token)
        if remote_user is None:
            raise HTTPException(status_code=401, detail="Invalid bearer token")
        principal = self._build_principal(remote_user)
        self._enforce_contract_policy(
            principal,
            auth=auth,
            request_tenant=self._resolve_request_tenant(
                path=websocket.url.path,
                headers=websocket.headers,
                query_params=websocket.query_params,
            ),
        )
        return principal

    def _build_auth_client(self, auth: Mapping[str, Any]) -> Any:
        token_policy = _mapping_or_empty(auth.get("token_policy"))
        base_url = (
            _clean_text(token_policy.get("auth_service_url")) or os.getenv(_DEFAULT_AUTH_SERVICE_URL_ENV, "").strip()
        )
        if not base_url:
            raise HTTPException(
                status_code=503,
                detail="Auth required but auth_service base URL is not configured",
            )

        jwt_secret_env = _clean_text(token_policy.get("jwt_secret_env")) or _DEFAULT_AUTH_SERVICE_SECRET_ENV
        jwt_algorithm_env = _clean_text(token_policy.get("jwt_algorithm_env")) or _DEFAULT_AUTH_SERVICE_ALGORITHM_ENV
        jwt_issuer_env = _clean_text(token_policy.get("jwt_issuer_env")) or _DEFAULT_AUTH_SERVICE_ISSUER_ENV
        jwt_audience_env = _clean_text(token_policy.get("jwt_audience_env")) or _DEFAULT_AUTH_SERVICE_AUDIENCE_ENV

        jwt_secret = os.getenv(jwt_secret_env, "")
        jwt_algorithm = _clean_text(token_policy.get("jwt_algorithm")) or os.getenv(jwt_algorithm_env, "HS256")
        jwt_issuer = _clean_text(token_policy.get("jwt_issuer")) or os.getenv(jwt_issuer_env, "auth-service")
        jwt_audience = _clean_text(token_policy.get("jwt_audience")) or os.getenv(jwt_audience_env, "auth-service")

        return self._auth_client_factory(
            base_url=base_url,
            jwt_secret=jwt_secret,
            jwt_algorithm=jwt_algorithm,
            jwt_issuer=jwt_issuer,
            jwt_audience=jwt_audience,
        )

    def _build_principal(self, remote_user: Any) -> AuthPrincipal:
        roles = list(remote_user.roles or [remote_user.role])
        claims = dict(remote_user.extra)
        return AuthPrincipal(
            user_id=remote_user.user_id,
            username=remote_user.username,
            tenant_id=remote_user.tenant_id,
            role=remote_user.role or (roles[0] if roles else "employee"),
            roles=roles,
            scopes=list(remote_user.scopes),
            permissions=list(remote_user.permissions),
            azp=remote_user.azp,
            email=remote_user.email,
            display_name=remote_user.display_name,
            department=remote_user.department,
            position=remote_user.position,
            claims=claims,
        )

    def _enforce_contract_policy(
        self,
        principal: AuthPrincipal,
        *,
        auth: Mapping[str, Any],
        request_tenant: str | None,
    ) -> None:
        required_scopes = _normalize_string_list(auth.get("required_scopes"))
        if required_scopes:
            granted = set(principal.scopes)
            if not granted.issuperset(required_scopes):
                raise HTTPException(
                    status_code=403,
                    detail={
                        "message": "required scope is missing",
                        "error_code": "insufficient_scope",
                        "required_scopes": required_scopes,
                    },
                )

        tenant_claim_key = _clean_text(auth.get("tenant_claim_key")) or "tenant_id"
        token_tenant = self._resolve_principal_claim(principal, tenant_claim_key)
        if request_tenant and not token_tenant:
            raise HTTPException(
                status_code=403,
                detail={"message": "tenant_id is required in token", "error_code": "tenant_missing"},
            )
        if request_tenant and token_tenant and request_tenant != token_tenant:
            raise HTTPException(
                status_code=403,
                detail={"message": "tenant mismatch", "error_code": "tenant_mismatch"},
            )

    def _resolve_principal_claim(self, principal: AuthPrincipal, claim_key: str) -> str | None:
        if claim_key == "tenant_id":
            return _clean_text(principal.tenant_id)
        if claim_key == "azp":
            return _clean_text(principal.azp)
        return _clean_text(principal.claims.get(claim_key))

    def _extract_http_token(self, request: Request) -> str | None:
        authorization = request.headers.get("authorization")
        if authorization:
            token = _parse_bearer_token(authorization)
            if token is not None:
                return token
        query_token = request.query_params.get(self._config.browser_token_query_key)
        return _clean_text(query_token)

    def _extract_websocket_token(self, websocket: WebSocket) -> str | None:
        authorization = websocket.headers.get("authorization")
        if authorization:
            token = _parse_bearer_token(authorization)
            if token is not None:
                return token
        query_token = websocket.query_params.get(self._config.browser_token_query_key)
        return _clean_text(query_token)

    def _resolve_request_tenant(
        self,
        *,
        path: str,
        headers: Mapping[str, str],
        query_params: Mapping[str, str],
    ) -> str | None:
        tenant_from_header = _clean_text(headers.get(self._config.tenant_header_name))
        if tenant_from_header:
            return tenant_from_header
        tenant_from_query = _clean_text(query_params.get("tenant_id"))
        if tenant_from_query:
            return tenant_from_query
        match = _TENANT_PATH_PATTERN.search(path)
        if match:
            return _clean_text(match.group(1))
        return None


async def _maybe_await(value: _T | Awaitable[_T]) -> _T:
    if hasattr(value, "__await__"):
        return await value
    return value


def _mapping_or_empty(value: Any) -> dict[str, Any]:
    if isinstance(value, dict):
        return value
    return {}


def _clean_text(value: Any) -> str | None:
    if value is None:
        return None
    text = str(value).strip()
    return text or None


def _normalize_string_list(value: Any) -> list[str]:
    if isinstance(value, list):
        return [item.lower() for item in (_clean_text(entry) for entry in value) if item]
    if isinstance(value, str):
        return [item.lower() for item in (_clean_text(entry) for entry in value.split(",")) if item]
    return []


def _parse_bearer_token(raw_header: str) -> str | None:
    parts = raw_header.strip().split(" ", 1)
    if len(parts) != 2 or parts[0].lower() != "bearer":
        return None
    return _clean_text(parts[1])


__all__ = [
    "AuthPrincipal",
    "ContractAuthGuard",
    "ContractAuthGuardConfig",
    "HTTPAuthBackend",
    "WSAuthBackend",
]
