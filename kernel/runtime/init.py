"""Runtime initialization helpers for BizCore."""

from __future__ import annotations

from pathlib import Path
from typing import Any


def init_bizcore(
    *,
    load_env: bool = True,
    env_path: str | Path | None = None,
    settings_overrides: dict[str, Any] | None = None,
) -> None:
    """Initialize BizCore runtime explicitly.

    Args:
        load_env: Whether to load .env file via python-dotenv (optional).
        env_path: Optional .env path (default: auto-discovery by dotenv).
        settings_overrides: Optional overrides for AgentFlowSettings.
    """
    if load_env:
        try:
            from dotenv import load_dotenv

            if env_path:
                load_dotenv(Path(env_path))
            else:
                load_dotenv()
        except Exception:
            # dotenv is optional; ignore if not installed
            pass

    if settings_overrides:
        from infrastructure.config import AgentFlowSettings

        settings = AgentFlowSettings(**settings_overrides)
        settings.configure_logging()
        return

    # Default: configure logging via standard settings
    from infrastructure.config import get_settings

    get_settings()


def init_agentflow(
    *,
    load_env: bool = True,
    env_path: str | Path | None = None,
    settings_overrides: dict[str, Any] | None = None,
) -> None:
    """Backward-compatible alias for legacy callers."""
    init_bizcore(
        load_env=load_env,
        env_path=env_path,
        settings_overrides=settings_overrides,
    )


__all__ = ["init_agentflow", "init_bizcore"]
