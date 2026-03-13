"""FAQ システム認証依存関係.

Tenant SSO を前提に auth_service（auth_client）で JWT を検証し、
tenant/scope を必須ポリシーとして適用する。
"""

from __future__ import annotations

import json
import logging
import os
import re
from pathlib import Path
from typing import TYPE_CHECKING, Any

from apps.faq_system.backend.auth.models import UserInfo
from apps.faq_system.backend.auth.service import AuthService, get_auth_service
from apps.faq_system.backend.security.proxy_auth import proxy_auth_verifier
from fastapi import Cookie, Depends, Header, HTTPException, Request, WebSocket

from agentflow.security.auth_client import AuthClient
from agentflow.security.contract_auth_guard import ContractAuthGuard, ContractAuthGuardConfig


if TYPE_CHECKING:
    from collections.abc import Callable, Mapping

    from agentflow.security.auth_client.client import RemoteUser


logger = logging.getLogger(__name__)
_APP_CONFIG_PATH = Path(__file__).resolve().parents[2] / "app_config.json"
_FAQ_PUBLIC_HTTP_PATHS = {
    "/api/auth/login",
    "/api/auth/register",
    "/api/auth/password/forgot",
    "/api/auth/password/reset",
    "/api/health",
    "/docs",
    "/redoc",
    "/openapi.json",
}
_faq_auth_guard: ContractAuthGuard | None = None
_faq_auth_client: AuthClient | None = None
_auth_contract_cache: tuple[tuple[int, int] | None, dict[str, Any]] | None = None
_TENANT_PATH_PATTERN = re.compile(r"/tenants/([^/]+)", re.IGNORECASE)
_AUTH_MODE_TENANT_SSO = "tenant_sso"
_AUTH_MODE_ENTERPRISE_ISOLATED = "enterprise_isolated"


def _env_bool(name: str) -> bool | None:
    raw = os.getenv(name)
    if raw is None:
        return None
    return raw.lower() in {"1", "true", "yes", "on"}


def _trust_proxy_auth_enabled() -> bool:
    return os.getenv("FAQ_TRUST_PROXY_AUTH", "false").lower() in {"1", "true", "yes", "on"}


def _legacy_fallback_enabled() -> bool:
    # 明示的に設定されている場合はモードに関わらず優先する。
    # auth_service 未起動のローカル開発で FAQ_AUTH_LEGACY_FALLBACK=true にすれば
    # FAQ ローカル認証へフォールバックできる。
    configured = _env_bool("FAQ_AUTH_LEGACY_FALLBACK")
    if configured is not None:
        return configured

    mode = _resolve_auth_mode()
    if mode == _AUTH_MODE_TENANT_SSO and not os.getenv("PYTEST_CURRENT_TEST"):
        return False

    # pytest 実行時は auth_service 未起動のため、互換 fallback を既定有効にする。
    return bool(os.getenv("PYTEST_CURRENT_TEST"))


def _get_auth_service() -> AuthService:
    return get_auth_service()


def _get_auth_client() -> AuthClient:
    global _faq_auth_client
    if _faq_auth_client is None:
        _faq_auth_client = AuthClient()
    return _faq_auth_client


def _get_header_candidates(env_name: str, default: str) -> tuple[str, ...]:
    raw = os.getenv(env_name, default).strip().lower()
    return tuple(item.strip() for item in raw.split(",") if item.strip())


def _pick_header(headers: Mapping[str, str], candidates: tuple[str, ...]) -> str | None:
    for key in candidates:
        value = headers.get(key)
        if value is not None and value.strip():
            return value.strip()
    return None


def _clean_text(value: Any) -> str | None:
    if value is None:
        return None
    text = str(value).strip()
    return text or None


def _normalize_list(value: Any) -> list[str]:
    if isinstance(value, list):
        return [str(item).strip() for item in value if str(item).strip()]
    if isinstance(value, str):
        return [item.strip() for item in value.split(",") if item.strip()]
    return []


def _auth_contract() -> dict[str, Any]:
    global _auth_contract_cache
    try:
        stat = _APP_CONFIG_PATH.stat()
        signature = (stat.st_mtime_ns, stat.st_size)
    except OSError:
        signature = None

    if _auth_contract_cache is not None and _auth_contract_cache[0] == signature:
        return _auth_contract_cache[1]

    payload: dict[str, Any] = {}
    if _APP_CONFIG_PATH.is_file():
        try:
            payload = json.loads(_APP_CONFIG_PATH.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError):
            payload = {}

    contracts = payload.get("contracts")
    contract_map = contracts if isinstance(contracts, dict) else {}
    auth = contract_map.get("auth")
    auth_map = auth if isinstance(auth, dict) else {}
    _auth_contract_cache = (signature, auth_map)
    return auth_map


def _resolve_required_scopes() -> list[str]:
    contract = _auth_contract()
    required_scopes = _normalize_list(contract.get("required_scopes"))
    default_scope = _clean_text(os.getenv("FAQ_REQUIRED_SCOPE", "faq.access"))
    if default_scope and default_scope not in required_scopes:
        required_scopes.append(default_scope)
    return required_scopes


def _normalize_auth_mode(value: str | None) -> str:
    text = _clean_text(value)
    if text is None:
        return _AUTH_MODE_TENANT_SSO
    normalized = text.lower()
    if normalized in {"tenant_sso", "tenant", "sso"}:
        return _AUTH_MODE_TENANT_SSO
    if normalized in {
        "enterprise_isolated",
        "enterprise",
        "legacy",
        "single_app",
        "app_isolated",
    }:
        return _AUTH_MODE_ENTERPRISE_ISOLATED
    return _AUTH_MODE_TENANT_SSO


def _resolve_auth_mode() -> str:
    env_mode = _clean_text(os.getenv("FAQ_AUTH_MODE"))
    if env_mode:
        return _normalize_auth_mode(env_mode)
    contract_mode = _clean_text(_auth_contract().get("mode"))
    return _normalize_auth_mode(contract_mode)


def _resolve_allow_same_tenant_sso() -> bool:
    if _resolve_auth_mode() != _AUTH_MODE_TENANT_SSO:
        return False

    contract = _auth_contract()
    direct_value = contract.get("allow_same_tenant_sso")
    if isinstance(direct_value, bool):
        return direct_value

    token_policy = contract.get("token_policy")
    token_policy_map = token_policy if isinstance(token_policy, dict) else {}
    contract_value = token_policy_map.get("allow_same_tenant_sso")
    if isinstance(contract_value, bool):
        return contract_value
    env_value = _env_bool("FAQ_ALLOW_SAME_TENANT_SSO")
    if env_value is not None:
        return env_value
    return True


def _resolve_require_tenant_context() -> bool:
    """tenant コンテキスト必須判定.

    tenant_sso モードでは常に必須（テスト時のみ省略可）。
    enterprise_isolated モードでは不要。
    """
    if _resolve_auth_mode() != _AUTH_MODE_TENANT_SSO:
        return False
    return not os.getenv("PYTEST_CURRENT_TEST")


def _resolve_request_tenant(headers: Mapping[str, str], request_path: str) -> str | None:
    tenant_header_names = _get_header_candidates("FAQ_TENANT_HEADER_NAMES", "x-tenant-id")
    tenant_from_header = _pick_header(headers, tenant_header_names)
    if tenant_from_header:
        return tenant_from_header

    match = _TENANT_PATH_PATTERN.search(request_path)
    if match:
        return _clean_text(match.group(1))

    host = _clean_text(headers.get("host"))
    if not host:
        return _clean_text(os.getenv("FAQ_DEFAULT_TENANT_ID"))
    host_no_port = host.split(":", 1)[0]
    if host_no_port in {"localhost", "127.0.0.1"}:
        return _clean_text(os.getenv("FAQ_DEFAULT_TENANT_ID"))
    segments = [part for part in host_no_port.split(".") if part]
    if len(segments) >= 3:
        return _clean_text(segments[0])
    return _clean_text(os.getenv("FAQ_DEFAULT_TENANT_ID"))


def _normalize_proxy_role(raw_role: str | None) -> str | None:
    if raw_role is None:
        return None
    value = raw_role.strip()
    if not value:
        return None

    if "," in value:
        groups = [item.strip() for item in value.split(",") if item.strip()]
        normalized = [item.lower() for item in groups]
        if "admin" in normalized:
            return "admin"
        if "manager" in normalized:
            return "manager"
        return groups[0]
    return value


def _proxy_user_id(username: str) -> str:
    base = f"user-{username}"
    if len(base) <= 64:
        return base
    import hashlib

    digest = hashlib.sha256(username.encode("utf-8")).hexdigest()[:59]
    return f"user-{digest}"


def _collect_proxy_values(headers: Mapping[str, str]) -> dict[str, str | None]:
    return {
        "x_auth_user": _pick_header(
            headers,
            _get_header_candidates(
                "FAQ_PROXY_AUTH_USER_HEADERS",
                "x-forwarded-user,x-auth-user",
            ),
        ),
        "x_auth_name": _pick_header(
            headers,
            _get_header_candidates(
                "FAQ_PROXY_AUTH_NAME_HEADERS",
                "x-forwarded-preferred-username,x-auth-name,x-forwarded-name",
            ),
        ),
        "x_auth_role": _normalize_proxy_role(
            _pick_header(
                headers,
                _get_header_candidates(
                    "FAQ_PROXY_AUTH_ROLE_HEADERS",
                    "x-forwarded-groups,x-auth-role",
                ),
            ),
        ),
        "x_auth_department": _pick_header(
            headers,
            _get_header_candidates(
                "FAQ_PROXY_AUTH_DEPARTMENT_HEADERS",
                "x-auth-department,x-forwarded-department",
            ),
        ),
        "x_auth_position": _pick_header(
            headers,
            _get_header_candidates(
                "FAQ_PROXY_AUTH_POSITION_HEADERS",
                "x-auth-position,x-forwarded-title",
            ),
        ),
        "x_auth_timestamp": _pick_header(
            headers,
            _get_header_candidates(
                "FAQ_PROXY_AUTH_TIMESTAMP_HEADERS",
                "x-auth-timestamp",
            ),
        ),
        "x_auth_nonce": _pick_header(
            headers,
            _get_header_candidates(
                "FAQ_PROXY_AUTH_NONCE_HEADERS",
                "x-auth-nonce",
            ),
        ),
        "x_auth_signature": _pick_header(
            headers,
            _get_header_candidates(
                "FAQ_PROXY_AUTH_SIGNATURE_HEADERS",
                "x-auth-signature",
            ),
        ),
    }


def _to_user_info(remote: RemoteUser) -> UserInfo:
    effective_roles = remote.roles or [remote.role]
    primary_role = remote.role or (effective_roles[0] if effective_roles else "employee")
    return UserInfo(
        user_id=remote.user_id,
        username=remote.username,
        display_name=remote.display_name or remote.username,
        department=remote.department,
        position=remote.position,
        role=primary_role,
        roles=effective_roles,
        tenant_id=remote.tenant_id,
        scopes=remote.scopes,
        azp=remote.azp,
    )


def _validate_tenant_scope_policy(
    *,
    user: UserInfo,
    request_tenant: str | None,
    required_scopes: list[str],
) -> None:
    mode = _resolve_auth_mode()
    token_tenant = _clean_text(user.tenant_id)
    if mode == _AUTH_MODE_TENANT_SSO and _resolve_require_tenant_context() and not request_tenant:
        raise HTTPException(
            status_code=403,
            detail={"message": "tenant context is required", "error_code": "tenant_missing"},
        )
    if mode == _AUTH_MODE_TENANT_SSO and request_tenant and not token_tenant:
        raise HTTPException(
            status_code=403,
            detail={"message": "tenant_id is required in token", "error_code": "tenant_missing"},
        )
    if mode == _AUTH_MODE_TENANT_SSO and request_tenant and token_tenant and request_tenant != token_tenant:
        raise HTTPException(
            status_code=403,
            detail={"message": "tenant mismatch", "error_code": "tenant_mismatch"},
        )

    current_app = os.getenv("FAQ_APP_NAME", "faq_system")
    token_source = _clean_text(user.azp)
    if mode == _AUTH_MODE_ENTERPRISE_ISOLATED:
        if token_source and token_source not in {current_app, "proxy_auth"}:
            raise HTTPException(
                status_code=403,
                detail={"message": "token source app is not allowed", "error_code": "token_invalid"},
            )
    elif not _resolve_allow_same_tenant_sso():
        if token_source and token_source != current_app:
            raise HTTPException(
                status_code=403,
                detail={"message": "token source app is not allowed", "error_code": "token_invalid"},
            )

    if required_scopes:
        granted = set(user.scopes)
        if not granted.issuperset(required_scopes):
            raise HTTPException(
                status_code=403,
                detail={
                    "message": "required scope is missing",
                    "error_code": "insufficient_scope",
                    "required_scopes": required_scopes,
                },
            )


async def _resolve_with_auth_service(
    *,
    authorization: str | None,
    request_tenant: str | None,
) -> UserInfo | None:
    if not authorization or not authorization.lower().startswith("bearer "):
        return None
    token = authorization.split(" ", 1)[1].strip()
    if not token:
        return None

    remote_user = await _get_auth_client().verify_token(token)
    if remote_user is None:
        return None
    user = _to_user_info(remote_user)
    _validate_tenant_scope_policy(
        user=user,
        request_tenant=request_tenant,
        required_scopes=_resolve_required_scopes(),
    )
    return user


async def _resolve_legacy_fallback(
    *,
    authorization: str | None,
    session_token: str | None,
) -> UserInfo | None:
    if not _legacy_fallback_enabled():
        return None
    auth_service = _get_auth_service()
    if authorization:
        auth_user = await auth_service.verify_token(authorization)
        if auth_user:
            metadata = auth_user.metadata or {}
            username = metadata.get("username", auth_user.id)
            current_user = await auth_service.get_user(username)
            if current_user:
                return current_user
            role = auth_user.roles[0] if auth_user.roles else "employee"
            return UserInfo(
                user_id=auth_user.id,
                username=username,
                display_name=metadata.get("display_name", auth_user.id),
                department=metadata.get("department", ""),
                position=metadata.get("position", ""),
                role=role,
                roles=auth_user.roles,
                tenant_id=_clean_text(metadata.get("tenant_id")),
                scopes=_normalize_list(metadata.get("scp")),
                azp=_clean_text(metadata.get("azp")),
            )
    if session_token:
        return await auth_service.get_user_by_session_token(session_token)
    return None


async def _resolve_user_from_request_payload(
    *,
    headers: Mapping[str, str],
    authorization: str | None,
    session_token: str | None,
    request_method: str,
    request_path: str,
) -> UserInfo | None:
    return await resolve_user(
        authorization=authorization,
        session_token=session_token,
        request_method=request_method,
        request_path=request_path,
        **_collect_proxy_values(headers),
        _headers=headers,
    )


async def get_current_user(
    request: Request,
    authorization: str | None = Header(None),
    session_token: str | None = Cookie(None, alias="session_token"),
) -> UserInfo | None:
    """現在のユーザーを取得 (任意)."""
    return await _resolve_user_from_request_payload(
        headers=request.headers,
        authorization=authorization,
        session_token=session_token,
        request_method=request.method,
        request_path=request.url.path,
    )


async def resolve_user(
    *,
    authorization: str | None = None,
    session_token: str | None = None,
    x_auth_user: str | None = None,
    x_auth_name: str | None = None,
    x_auth_role: str | None = None,
    x_auth_department: str | None = None,
    x_auth_position: str | None = None,
    x_auth_timestamp: str | None = None,
    x_auth_nonce: str | None = None,
    x_auth_signature: str | None = None,
    request_method: str = "GET",
    request_path: str = "/",
    _headers: Mapping[str, str] | None = None,
) -> UserInfo | None:
    """認証情報からユーザーを解決."""
    headers = _headers or {}
    request_tenant = _resolve_request_tenant(headers, request_path)

    # 1) auth_service トークンを最優先
    auth_user = await _resolve_with_auth_service(
        authorization=authorization,
        request_tenant=request_tenant,
    )
    if auth_user is not None:
        return auth_user

    # 2) 認証プロキシ（明示的に有効化時のみ）
    if _trust_proxy_auth_enabled() and x_auth_user:
        verified = await proxy_auth_verifier.verify(
            user=x_auth_user,
            display_name=x_auth_name,
            role=x_auth_role,
            department=x_auth_department,
            position=x_auth_position,
            timestamp=x_auth_timestamp,
            nonce=x_auth_nonce,
            signature=x_auth_signature,
            request_method=request_method,
            request_path=request_path,
        )
        if verified is None:
            logger.warning("Proxy auth verification failed for user=%s", x_auth_user)
            return None
        proxy_user = UserInfo(
            user_id=_proxy_user_id(verified.username),
            username=verified.username,
            display_name=verified.display_name,
            department=verified.department,
            position=verified.position,
            role=verified.role,
            roles=[verified.role],
            tenant_id=request_tenant,
            scopes=_resolve_required_scopes(),
            azp="proxy_auth",
        )
        _validate_tenant_scope_policy(
            user=proxy_user,
            request_tenant=request_tenant,
            required_scopes=_resolve_required_scopes(),
        )
        return proxy_user

    # 3) 旧実装 fallback（既定無効）
    return await _resolve_legacy_fallback(
        authorization=authorization,
        session_token=session_token,
    )


async def resolve_user_from_http_request(request: Request) -> UserInfo | None:
    """HTTP リクエストからユーザーを解決."""
    return await _resolve_user_from_request_payload(
        headers=request.headers,
        authorization=request.headers.get("authorization"),
        session_token=request.cookies.get("session_token"),
        request_method=request.method,
        request_path=request.url.path,
    )


async def resolve_user_from_websocket(websocket: WebSocket) -> UserInfo | None:
    """WebSocket ハンドシェイク情報からユーザーを解決."""
    access_token = websocket.query_params.get("access_token")
    authorization_header = websocket.headers.get("authorization")
    authorization = authorization_header or (f"Bearer {access_token}" if access_token else None)
    return await _resolve_user_from_request_payload(
        headers=websocket.headers,
        authorization=authorization,
        session_token=websocket.cookies.get("session_token"),
        request_method="GET",
        request_path=websocket.url.path,
    )


def get_faq_contract_auth_guard() -> ContractAuthGuard:
    """FAQ 認証用 ContractAuthGuard を取得."""
    global _faq_auth_guard
    if _faq_auth_guard is None:
        _faq_auth_guard = ContractAuthGuard(
            ContractAuthGuardConfig(
                app_config_path=_APP_CONFIG_PATH,
                public_http_paths=_FAQ_PUBLIC_HTTP_PATHS,
                auth_header_name="x-api-key",
                ws_query_key="api_key",
                api_key_env_selector_var="FAQ_API_KEY_ENV",
                default_api_key_env_var="FAQ_API_KEY",
            ),
            http_backend=resolve_user_from_http_request,
            ws_backend=resolve_user_from_websocket,
        )
    return _faq_auth_guard


async def require_ws_auth(websocket: WebSocket) -> UserInfo | None:
    """WebSocket 認証を共通ガード経由で実行."""
    guard = get_faq_contract_auth_guard()
    ok, user = await guard.require_ws(websocket)
    if not ok:
        return None
    if isinstance(user, UserInfo):
        return user
    fallback = await resolve_user_from_websocket(websocket)
    if fallback is None:
        await websocket.close(code=4401, reason="Unauthorized")
    return fallback


async def get_optional_user(
    user: UserInfo | None = Depends(get_current_user),
) -> UserInfo | None:
    """オプショナルユーザー取得 (未認証でもエラーにならない)."""
    return user


async def require_auth(
    request: Request,
) -> UserInfo:
    """認証必須."""
    guard = get_faq_contract_auth_guard()
    resolved = await guard.require_http(request)
    if isinstance(resolved, UserInfo):
        return resolved

    user = await resolve_user_from_http_request(request)
    if user is None:
        raise HTTPException(
            status_code=401,
            detail={"message": "認証が必要です。ログインしてください。", "error_code": "token_invalid"},
        )
    return user


def require_role(*roles: str) -> Callable[..., Any]:
    """ロール必須依存関係ファクトリ."""

    async def _check_role(
        user: UserInfo = Depends(require_auth),
    ) -> UserInfo:
        effective_roles = set(user.roles or [user.role])
        if not any(role in effective_roles for role in roles):
            raise HTTPException(
                status_code=403,
                detail=f"権限不足です。必要なロール: {', '.join(roles)}",
            )
        return user

    return _check_role
