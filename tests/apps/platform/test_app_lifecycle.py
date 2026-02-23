"""AppLifecycleManager のユニットテスト.

テスト対象: apps/platform/services/app_lifecycle.py
"""

from __future__ import annotations

from unittest.mock import AsyncMock, patch

import httpx
import pytest
from apps.platform.schemas.app_config_schemas import AppConfig
from apps.platform.services.app_lifecycle import (
    AppLifecycleManager,
    AppStatus,
    HealthCheckResult,
)


# ------------------------------------------------------------------
# HealthCheckResult テスト
# ------------------------------------------------------------------


class TestHealthCheckResult:
    """HealthCheckResult データクラステスト."""

    def test_to_dict_healthy(self) -> None:
        """正常結果の辞書変換."""
        r = HealthCheckResult("app1", AppStatus.HEALTHY, response_time_ms=12.5)
        d = r.to_dict()
        assert d["app_name"] == "app1"
        assert d["status"] == "healthy"
        assert d["response_time_ms"] == 12.5
        assert "error" not in d

    def test_to_dict_with_error(self) -> None:
        """エラー結果の辞書変換."""
        r = HealthCheckResult("app2", AppStatus.STOPPED, error="接続拒否")
        d = r.to_dict()
        assert d["status"] == "stopped"
        assert d["error"] == "接続拒否"

    def test_to_dict_with_details(self) -> None:
        """詳細情報付き結果の辞書変換."""
        r = HealthCheckResult(
            "app3",
            AppStatus.HEALTHY,
            response_time_ms=5.0,
            details={"version": "1.0"},
        )
        d = r.to_dict()
        assert d["details"] == {"version": "1.0"}


class TestAppStatus:
    """AppStatus Enum テスト."""

    def test_values(self) -> None:
        """全ステータス値が正しい."""
        assert AppStatus.HEALTHY.value == "healthy"
        assert AppStatus.UNHEALTHY.value == "unhealthy"
        assert AppStatus.UNKNOWN.value == "unknown"
        assert AppStatus.STOPPED.value == "stopped"


# ------------------------------------------------------------------
# AppLifecycleManager テスト
# ------------------------------------------------------------------


class TestAppLifecycleManager:
    """AppLifecycleManager テスト."""

    def _make_config(
        self,
        name: str = "test_app",
        api_port: int | None = 8099,
        health: str | None = "/health",
    ) -> AppConfig:
        """テスト用 AppConfig を生成."""
        return AppConfig(
            name=name,
            display_name="Test",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
            ports={"api": api_port},
            entry_points={"health": health},
        )

    @pytest.mark.asyncio
    async def test_no_api_port_returns_unknown(
        self,
        lifecycle: AppLifecycleManager,
    ) -> None:
        """API ポートなしの App は UNKNOWN を返す."""
        cfg = self._make_config(api_port=None)
        result = await lifecycle.check_health(cfg)
        assert result.status == AppStatus.UNKNOWN

    @pytest.mark.asyncio
    async def test_no_health_path_still_checks_fallback_paths(
        self,
        lifecycle: AppLifecycleManager,
    ) -> None:
        """ヘルスパスなしでもフォールバックURLを試行する."""
        cfg = self._make_config(health=None)
        with patch("apps.platform.services.app_lifecycle.httpx.AsyncClient") as mock_cls:
            mock_client = AsyncMock()
            mock_client.get.side_effect = httpx.ConnectError("refused")
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=False)
            mock_cls.return_value = mock_client

            result = await lifecycle.check_health(cfg)
            assert result.status == AppStatus.STOPPED

    @pytest.mark.asyncio
    async def test_healthy_response(
        self,
        lifecycle: AppLifecycleManager,
    ) -> None:
        """HTTP 200 で HEALTHY を返す."""
        cfg = self._make_config()
        mock_resp = httpx.Response(
            200,
            json={"status": "ok"},
            request=httpx.Request("GET", "http://localhost:8099/health"),
        )
        with patch("apps.platform.services.app_lifecycle.httpx.AsyncClient") as mock_cls:
            mock_client = AsyncMock()
            mock_client.get.return_value = mock_resp
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=False)
            mock_cls.return_value = mock_client

            result = await lifecycle.check_health(cfg)
            assert result.status == AppStatus.HEALTHY
            assert result.response_time_ms > 0

    @pytest.mark.asyncio
    async def test_unhealthy_response(
        self,
        lifecycle: AppLifecycleManager,
    ) -> None:
        """HTTP 500 で UNHEALTHY を返す."""
        cfg = self._make_config()
        mock_resp = httpx.Response(
            500,
            request=httpx.Request("GET", "http://localhost:8099/health"),
        )
        with patch("apps.platform.services.app_lifecycle.httpx.AsyncClient") as mock_cls:
            mock_client = AsyncMock()
            mock_client.get.return_value = mock_resp
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=False)
            mock_cls.return_value = mock_client

            result = await lifecycle.check_health(cfg)
            assert result.status == AppStatus.UNHEALTHY
            assert "HTTP 500" in (result.error or "")

    @pytest.mark.asyncio
    async def test_connect_error_returns_stopped(
        self,
        lifecycle: AppLifecycleManager,
    ) -> None:
        """接続拒否で STOPPED を返す."""
        cfg = self._make_config()
        with patch("apps.platform.services.app_lifecycle.httpx.AsyncClient") as mock_cls:
            mock_client = AsyncMock()
            mock_client.get.side_effect = httpx.ConnectError("refused")
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=False)
            mock_cls.return_value = mock_client

            result = await lifecycle.check_health(cfg)
            assert result.status == AppStatus.STOPPED

    @pytest.mark.asyncio
    async def test_start_uses_runtime_command_override(
        self,
        lifecycle: AppLifecycleManager,
        tmp_path,
    ) -> None:
        """runtime.commands.start がある場合は compose より優先する."""
        cfg = AppConfig(
            name="cmd_app",
            display_name="Command App",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
            ports={"api": None},
            entry_points={"health": None},
            runtime={"commands": {"start": "echo start-from-config"}},
        )
        app_dir = tmp_path / "cmd_app"
        app_dir.mkdir()
        config_path = app_dir / "app_config.json"
        config_path.write_text("{}", encoding="utf-8")

        class _Proc:
            returncode = 0

            async def communicate(self):
                return b"started", b""

        with patch(
            "apps.platform.services.app_lifecycle.asyncio.create_subprocess_exec",
            new=AsyncMock(return_value=_Proc()),
        ) as mocked, patch.object(
            lifecycle,
            "_run_cli_preflight",
            new=AsyncMock(return_value={"ready": True, "final": {"available_tools": [], "authenticated_tools": []}}),
        ):
            result = await lifecycle.start_app(cfg, config_path=config_path)

        assert result.success is True
        assert result.return_code == 0
        assert result.command[:2] == ["bash", "-lc"]
        assert result.cwd == str(app_dir)
        mocked.assert_called_once()

    def test_resolve_action_command_supports_local_dev_commands(self) -> None:
        """backend_dev / frontend_dev を解決できる."""
        cfg = AppConfig(
            name="local_cmd_app",
            display_name="Local Command App",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
            runtime={
                "commands": {
                    "backend_dev": "  python -m apps.local_cmd_app.main  ",
                    "frontend_dev": "  cd apps/local_cmd_app/frontend && npm run dev  ",
                },
            },
        )

        assert AppLifecycleManager._resolve_action_command(cfg, "backend_dev") == "python -m apps.local_cmd_app.main"
        assert (
            AppLifecycleManager._resolve_action_command(cfg, "frontend_dev")
            == "cd apps/local_cmd_app/frontend && npm run dev"
        )

    @pytest.mark.asyncio
    async def test_local_start_runs_backend_only_when_frontend_missing(
        self,
        lifecycle: AppLifecycleManager,
        tmp_path,
    ) -> None:
        """frontend_dev なしでも backend_dev があれば local_start できる."""
        cfg = AppConfig(
            name="local_backend_only",
            display_name="Local Backend Only",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
            ports={"api": None},
            entry_points={"health": None},
            runtime={"commands": {"backend_dev": "python -m apps.local_backend_only.main"}},
        )
        config_path = tmp_path / "apps" / "local_backend_only" / "app_config.json"
        config_path.parent.mkdir(parents=True)
        config_path.write_text("{}", encoding="utf-8")

        class _Proc:
            async def communicate(self):
                return b"", b""

        with patch(
            "apps.platform.services.app_lifecycle.asyncio.create_subprocess_shell",
            new=AsyncMock(return_value=_Proc()),
        ) as mocked, patch.object(
            lifecycle,
            "_run_cli_preflight",
            new=AsyncMock(return_value={"ready": True, "final": {"available_tools": [], "authenticated_tools": []}}),
        ):
            result = await lifecycle.start_local_dev(cfg, config_path=config_path)

        assert result.success is True
        assert result.error is None
        assert "backend: python -m apps.local_backend_only.main" in result.stdout
        mocked.assert_called_once()

    @pytest.mark.asyncio
    async def test_start_prefers_readme_command_over_runtime(
        self,
        lifecycle: AppLifecycleManager,
        tmp_path,
    ) -> None:
        """README の start コマンドを runtime.commands より優先する."""
        cfg = AppConfig(
            name="readme_app",
            display_name="README App",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
            ports={"api": None},
            entry_points={"health": None},
            runtime={"commands": {"start": "echo runtime-start"}},
        )
        app_dir = tmp_path / "readme_app"
        app_dir.mkdir(parents=True)
        (app_dir / "README.md").write_text(
            "```bash\npython -m apps.readme_app.main --reload\n```",
            encoding="utf-8",
        )
        config_path = app_dir / "app_config.json"
        config_path.write_text("{}", encoding="utf-8")

        class _Proc:
            returncode = 0

            async def communicate(self):
                return b"ok", b""

        with patch(
            "apps.platform.services.app_lifecycle.asyncio.create_subprocess_exec",
            new=AsyncMock(return_value=_Proc()),
        ), patch.object(
            lifecycle,
            "_run_cli_preflight",
            new=AsyncMock(return_value={"ready": True, "final": {"available_tools": [], "authenticated_tools": []}}),
        ):
            result = await lifecycle.start_app(cfg, config_path=config_path)

        assert result.success is True
        assert result.command[:2] == ["bash", "-lc"]
        assert "apps.readme_app.main --reload" in result.command[2]
        assert result.command_source == "readme"

    @pytest.mark.asyncio
    async def test_start_failure_includes_diagnostic_payload(
        self,
        lifecycle: AppLifecycleManager,
        tmp_path,
    ) -> None:
        """起動失敗時に diagnostic が付与される."""
        cfg = AppConfig(
            name="diag_app",
            display_name="Diag App",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
            ports={"api": None},
            entry_points={"health": None},
            runtime={"commands": {"start": "echo fail"}},
        )
        app_dir = tmp_path / "diag_app"
        app_dir.mkdir(parents=True)
        (app_dir / "app_config.json").write_text("{}", encoding="utf-8")

        class _Proc:
            returncode = 1

            async def communicate(self):
                return b"", b"boom"

        with patch(
            "apps.platform.services.app_lifecycle.asyncio.create_subprocess_exec",
            new=AsyncMock(return_value=_Proc()),
        ), patch.object(
            lifecycle,
            "_run_cli_preflight",
            new=AsyncMock(return_value={"ready": False, "final": {"available_tools": [], "authenticated_tools": []}}),
        ), patch.object(
            lifecycle._diagnostic_service,
            "diagnose_action_failure",
            new=AsyncMock(
                return_value={
                    "tool": "codex",
                    "summary": "boom",
                    "recommendations": ["check env"],
                    "raw_output": "x",
                    "setup": {"ready": False, "available_tools": [], "authenticated_tools": []},
                    "command_source": "runtime.commands",
                }
            ),
        ):
            result = await lifecycle.start_app(cfg, config_path=app_dir / "app_config.json")

        assert result.success is False
        assert result.diagnostic is not None
        assert result.diagnostic.get("summary") == "boom"
