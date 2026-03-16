"""Layer 5 の lifecycle サービス."""

from __future__ import annotations

import time
from typing import TYPE_CHECKING, Any

import httpx


if TYPE_CHECKING:
    from contracts.app import AppManifest


class LifecycleService:
    """manifest 駆動の簡易 lifecycle サービス."""

    async def check_health(self, manifest: AppManifest) -> dict[str, Any]:
        """health URL があれば問い合わせる."""
        runtime = manifest.runtime
        urls = runtime.get("urls", {}) if isinstance(runtime, dict) else {}
        health_url = urls.get("health") or urls.get("backend")
        if not isinstance(health_url, str) or not health_url:
            return {"status": "unknown", "reason": "health url not configured"}

        started = time.perf_counter()
        try:
            async with httpx.AsyncClient(timeout=5.0) as client:
                response = await client.get(health_url)
            payload: dict[str, Any]
            try:
                payload = response.json()
            except ValueError:
                payload = {"text": response.text}
            return {
                "status": "healthy" if response.is_success else "unhealthy",
                "status_code": response.status_code,
                "response_time_ms": (time.perf_counter() - started) * 1000,
                "payload": payload,
            }
        except httpx.HTTPError as exc:
            return {"status": "unhealthy", "error": str(exc)}

    def build_command_plan(self, manifest: AppManifest, action: str) -> list[str]:
        """manifest から実行予定コマンドを返す."""
        runtime = manifest.runtime
        commands = runtime.get("commands", {}) if isinstance(runtime, dict) else {}
        value = commands.get(action)
        if isinstance(value, str) and value.strip():
            return [value.strip()]
        return []
