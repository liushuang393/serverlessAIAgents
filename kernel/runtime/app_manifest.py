"""Helpers for resolving app runtime networking from app_config.json."""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING
from urllib.parse import urlparse, urlunparse


if TYPE_CHECKING:
    from collections.abc import Mapping


@dataclass(frozen=True, slots=True)
class AppRuntimePorts:
    """Canonical runtime ports declared in the manifest."""

    api: int | None
    frontend: int | None
    db: int | None
    redis: int | None


@dataclass(frozen=True, slots=True)
class AppRuntimeHosts:
    """Bind hosts declared in the manifest."""

    backend: str | None
    frontend: str | None


@dataclass(frozen=True, slots=True)
class AppRuntimeUrls:
    """Access URLs declared in the manifest."""

    backend: str | None
    frontend: str | None
    health: str | None
    database: str | None


@dataclass(frozen=True, slots=True)
class ResolvedAppRuntime:
    """Resolved networking contract for one app."""

    app_name: str
    config_path: Path
    ports: AppRuntimePorts
    hosts: AppRuntimeHosts
    urls: AppRuntimeUrls


def resolve_app_runtime(
    config_path: str | Path,
    *,
    env: Mapping[str, str] | None = None,
    backend_host_env: str | None = None,
    backend_port_env: str | None = None,
    frontend_host_env: str | None = None,
    frontend_port_env: str | None = None,
    backend_url_env: str | None = None,
    frontend_url_env: str | None = None,
    health_url_env: str | None = None,
    database_url_env: str | None = None,
) -> ResolvedAppRuntime:
    """Resolve manifest runtime networking with optional env overrides."""
    resolved_env = env if env is not None else {}
    manifest_path = Path(config_path).resolve()
    payload = json.loads(manifest_path.read_text(encoding="utf-8"))

    ports_payload = _as_dict(payload.get("ports"))
    runtime_payload = _as_dict(payload.get("runtime"))
    urls_payload = _as_dict(runtime_payload.get("urls"))
    hosts_payload = _as_dict(runtime_payload.get("hosts"))

    api_port = _resolve_port(ports_payload.get("api"), env=resolved_env, env_key=backend_port_env)
    frontend_port = _resolve_port(ports_payload.get("frontend"), env=resolved_env, env_key=frontend_port_env)
    db_port = _clean_port(ports_payload.get("db"))
    redis_port = _clean_port(ports_payload.get("redis"))

    backend_url = _resolve_url(
        urls_payload.get("backend"),
        env=resolved_env,
        env_key=backend_url_env,
        port_override=api_port,
    )
    frontend_url = _resolve_url(
        urls_payload.get("frontend"),
        env=resolved_env,
        env_key=frontend_url_env,
        port_override=frontend_port,
    )
    health_url = _resolve_url(
        urls_payload.get("health"),
        env=resolved_env,
        env_key=health_url_env,
        port_override=api_port,
    )
    database_url = _resolve_url(
        urls_payload.get("database"),
        env=resolved_env,
        env_key=database_url_env,
        port_override=db_port,
    )

    return ResolvedAppRuntime(
        app_name=_clean_text(payload.get("name")) or manifest_path.parent.name,
        config_path=manifest_path,
        ports=AppRuntimePorts(
            api=api_port,
            frontend=frontend_port,
            db=db_port,
            redis=redis_port,
        ),
        hosts=AppRuntimeHosts(
            backend=_resolve_host(
                hosts_payload.get("backend"),
                env=resolved_env,
                env_key=backend_host_env,
                default="0.0.0.0" if api_port is not None else None,
            ),
            frontend=_resolve_host(
                hosts_payload.get("frontend"),
                env=resolved_env,
                env_key=frontend_host_env,
                default="0.0.0.0" if frontend_port is not None else None,
            ),
        ),
        urls=AppRuntimeUrls(
            backend=backend_url,
            frontend=frontend_url,
            health=health_url,
            database=database_url,
        ),
    )




def _resolve_host(
    value: object,
    *,
    env: Mapping[str, str],
    env_key: str | None,
    default: str | None,
) -> str | None:
    if env_key:
        overridden = _clean_text(env.get(env_key))
        if overridden is not None:
            return overridden
    resolved = _clean_text(value)
    return resolved or default


def _resolve_port(value: object, *, env: Mapping[str, str], env_key: str | None) -> int | None:
    if env_key:
        overridden = _clean_port(env.get(env_key))
        if overridden is not None:
            return overridden
    return _clean_port(value)


def _resolve_url(
    value: object,
    *,
    env: Mapping[str, str],
    env_key: str | None,
    port_override: int | None,
) -> str | None:
    if env_key:
        overridden = _clean_text(env.get(env_key))
        if overridden is not None:
            return overridden.rstrip("/")
    resolved = _clean_text(value)
    if resolved is None:
        return None
    if port_override is None:
        return resolved.rstrip("/")
    return _replace_url_port(resolved, port_override).rstrip("/")


def _replace_url_port(url: str, port: int) -> str:
    parsed = urlparse(url)
    if not parsed.scheme or not parsed.netloc:
        return url
    if parsed.hostname is None:
        return url
    host = parsed.hostname
    if ":" in host and not host.startswith("["):
        host = f"[{host}]"
    netloc = f"{host}:{port}"
    if parsed.username is not None:
        userinfo = parsed.username
        if parsed.password is not None:
            userinfo = f"{userinfo}:{parsed.password}"
        netloc = f"{userinfo}@{netloc}"
    return urlunparse(parsed._replace(netloc=netloc))


def _as_dict(value: object) -> dict[str, object]:
    return value if isinstance(value, dict) else {}


def _clean_text(value: object) -> str | None:
    if not isinstance(value, str):
        return None
    text = value.strip()
    return text or None


def _clean_port(value: object) -> int | None:
    if isinstance(value, int):
        return value
    if isinstance(value, str):
        text = value.strip()
        if not text:
            return None
        try:
            return int(text)
        except ValueError:
            return None
    return None


__all__ = [
    "AppRuntimeHosts",
    "AppRuntimePorts",
    "AppRuntimeUrls",
    "ResolvedAppRuntime",
    "resolve_app_runtime",
]