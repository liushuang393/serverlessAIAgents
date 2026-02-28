"""AppLifecycleManager のユニットテスト.

テスト対象: apps/platform/services/app_lifecycle.py
"""

from __future__ import annotations

import asyncio
from unittest.mock import AsyncMock, patch

import httpx
import pytest
from apps.platform.schemas.app_config_schemas import AppConfig
from apps.platform.services.app_lifecycle import (
    AppActionResult,
    AppLifecycleManager,
    AppStatus,
    HealthCheckResult,
    LocalProcessLaunchResult,
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
    async def test_health_requires_frontend_backend_and_database(
        self,
        lifecycle: AppLifecycleManager,
    ) -> None:
        """frontend/backend/database がすべて OK の時のみ HEALTHY."""
        cfg = AppConfig(
            name="full_stack_app",
            display_name="Full Stack App",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
            ports={"api": 8099, "frontend": 3004, "db": 5433},
            entry_points={"health": "/health"},
            dependencies={"database": "postgresql"},
        )
        mock_resp = httpx.Response(
            200,
            json={"status": "ok"},
            request=httpx.Request("GET", "http://localhost:8099/health"),
        )
        with patch("apps.platform.services.app_lifecycle.httpx.AsyncClient") as mock_cls, patch.object(
            lifecycle,
            "_is_tcp_port_open",
            side_effect=lambda _host, port, _timeout: port in {3004, 5433},
        ):
            mock_client = AsyncMock()
            mock_client.get.return_value = mock_resp
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=False)
            mock_cls.return_value = mock_client

            result = await lifecycle.check_health(cfg)

        assert result.status == AppStatus.HEALTHY
        assert result.details is not None
        components = result.details.get("components", {})
        assert components.get("frontend", {}).get("healthy") is True
        assert components.get("backend", {}).get("healthy") is True
        assert components.get("database", {}).get("healthy") is True

    @pytest.mark.asyncio
    async def test_health_reports_frontend_failure_even_when_backend_is_ok(
        self,
        lifecycle: AppLifecycleManager,
    ) -> None:
        """backend が OK でも frontend が不健康なら healthy にしない."""
        cfg = AppConfig(
            name="frontend_fail_app",
            display_name="Frontend Fail App",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
            ports={"api": 8099, "frontend": 3004, "db": 5433},
            entry_points={"health": "/health"},
            dependencies={"database": "postgresql"},
        )
        mock_resp = httpx.Response(
            200,
            json={"status": "ok"},
            request=httpx.Request("GET", "http://localhost:8099/health"),
        )
        with patch("apps.platform.services.app_lifecycle.httpx.AsyncClient") as mock_cls, patch.object(
            lifecycle,
            "_is_tcp_port_open",
            side_effect=lambda _host, port, _timeout: port == 5433,
        ):
            mock_client = AsyncMock()
            mock_client.get.return_value = mock_resp
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=False)
            mock_cls.return_value = mock_client

            result = await lifecycle.check_health(cfg)

        assert result.status == AppStatus.STOPPED
        assert "frontend" in (result.error or "")

    @pytest.mark.asyncio
    async def test_start_uses_docker_mode_when_compose_exists_and_stopped(
        self,
        lifecycle: AppLifecycleManager,
        tmp_path,
    ) -> None:
        """start は compose-first 規約で docker モードを選択する."""
        cfg = AppConfig(
            name="cmd_app",
            display_name="Command App",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
            ports={"api": None},
            entry_points={"health": None},
            runtime={"commands": {"backend_dev": "python -m apps.cmd_app.main"}},
        )
        app_dir = tmp_path / "cmd_app"
        app_dir.mkdir()
        (app_dir / "docker-compose.yml").write_text("services: {}\n", encoding="utf-8")
        config_path = app_dir / "app_config.json"
        config_path.write_text("{}", encoding="utf-8")

        compose_result = AppActionResult(
            app_name="cmd_app",
            action="start",
            success=True,
            command=["docker", "compose", "up", "-d"],
            cwd=str(app_dir),
            command_source="fallback",
            execution_mode="docker",
        )
        with patch.object(
            lifecycle,
            "_run_cli_preflight",
            new=AsyncMock(return_value={"ready": True, "final": {"available_tools": [], "authenticated_tools": []}}),
        ), patch.object(
            lifecycle,
            "_detect_execution_mode",
            new=AsyncMock(return_value=("docker", {"reason": "compose_first_default"})),
        ), patch.object(
            lifecycle,
            "_run_compose_action",
            new=AsyncMock(return_value=compose_result),
        ) as mocked_compose, patch.object(
            lifecycle,
            "_run_process_action",
            new=AsyncMock(),
        ) as mocked_process:
            result = await lifecycle.start_app(cfg, config_path=config_path)

        assert result.success is True
        assert result.execution_mode == "docker"
        mocked_compose.assert_called_once()
        mocked_process.assert_not_called()

    @pytest.mark.asyncio
    async def test_start_skips_restart_when_local_already_running(
        self,
        lifecycle: AppLifecycleManager,
        tmp_path,
    ) -> None:
        """local 起動中なら start は再起動せず成功応答を返す."""
        cfg = AppConfig(
            name="local_running_app",
            display_name="Local Running App",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
            ports={"api": None},
            entry_points={"health": None},
            runtime={"commands": {"backend_dev": "python -m apps.local_running_app.main"}},
        )
        config_path = tmp_path / "local_running_app" / "app_config.json"
        config_path.parent.mkdir(parents=True)
        config_path.write_text("{}", encoding="utf-8")

        with patch.object(
            lifecycle,
            "_detect_execution_mode",
            new=AsyncMock(
                return_value=(
                    "local",
                    {
                        "reason": "local_running",
                        "local": {"running": True, "running_roles": ["backend"]},
                    },
                )
            ),
        ), patch.object(
            lifecycle,
            "check_health",
            new=AsyncMock(return_value=HealthCheckResult("local_running_app", AppStatus.HEALTHY)),
        ), patch.object(
            lifecycle,
            "_run_process_action",
            new=AsyncMock(),
        ) as mocked_process, patch.object(
            lifecycle,
            "_run_cli_preflight",
            new=AsyncMock(return_value={"ready": False, "final": {"available_tools": [], "authenticated_tools": []}}),
        ):
            result = await lifecycle.start_app(cfg, config_path=config_path)

        assert result.success is True
        assert result.execution_mode == "local"
        assert "already running" in result.stdout
        mocked_process.assert_not_called()

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
    async def test_local_start_fails_when_frontend_dies_early(
        self,
        lifecycle: AppLifecycleManager,
        tmp_path,
    ) -> None:
        """local_start は frontend 異常終了時に失敗を返す."""
        cfg = AppConfig(
            name="local_crash_app",
            display_name="Local Crash App",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
            ports={"api": 8999, "frontend": 3009},
            entry_points={"health": "/health"},
            runtime={
                "commands": {
                    "backend_dev": "python -m apps.local_crash_app.main",
                    "frontend_dev": "cd apps/local_crash_app/frontend && npm run dev",
                }
            },
        )
        config_path = tmp_path / "apps" / "local_crash_app" / "app_config.json"
        config_path.parent.mkdir(parents=True)
        config_path.write_text("{}", encoding="utf-8")

        backend_launch = LocalProcessLaunchResult(
            role="backend",
            command="python -m apps.local_crash_app.main",
            success=True,
            pid=12345,
            log_path="/tmp/local_crash_app_backend.log",
            message="backend started",
        )
        frontend_launch = LocalProcessLaunchResult(
            role="frontend",
            command="cd apps/local_crash_app/frontend && npm run dev",
            success=False,
            pid=23456,
            log_path="/tmp/local_crash_app_frontend.log",
            error="frontend プロセスが直後に終了しました",
        )

        with patch.object(
            lifecycle,
            "_start_local_process",
            new=AsyncMock(side_effect=[backend_launch, frontend_launch]),
        ), patch.object(
            lifecycle,
            "_run_cli_preflight",
            new=AsyncMock(return_value={"ready": True, "final": {"available_tools": [], "authenticated_tools": []}}),
        ), patch.object(
            lifecycle,
            "_preflight_local_dependencies",
            new=AsyncMock(return_value=([], None)),
        ), patch.object(
            lifecycle._diagnostic_service,
            "diagnose_action_failure",
            new=AsyncMock(return_value={"summary": "frontend crash"}),
        ):
            result = await lifecycle.start_local_dev(cfg, config_path=config_path)

        assert result.success is False
        assert result.execution_mode == "local"
        assert "frontend プロセスが直後に終了しました" in (result.error or "")

    @pytest.mark.asyncio
    async def test_preflight_auto_starts_missing_dependency(
        self,
        lifecycle: AppLifecycleManager,
        tmp_path,
    ) -> None:
        """local_start 前提チェックは不足依存の自動起動を試みる."""
        cfg = AppConfig(
            name="dep_autostart_app",
            display_name="Dependency Autostart App",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
            ports={"db": 5433},
            dependencies={"database": "postgresql"},
        )
        config_path = tmp_path / "apps" / "dep_autostart_app" / "app_config.json"
        config_path.parent.mkdir(parents=True)
        config_path.write_text("{}", encoding="utf-8")
        (config_path.parent / "docker-compose.yml").write_text("services: {}\n", encoding="utf-8")

        with patch.object(
            lifecycle,
            "_is_tcp_port_open",
            side_effect=[False, True],
        ), patch.object(
            lifecycle,
            "_auto_start_local_dependencies",
            new=AsyncMock(return_value=(["dependency auto-start: docker compose up -d faq-db"], None)),
        ):
            notes, error = await lifecycle._preflight_local_dependencies(cfg, config_path=config_path)

        assert error is None
        assert any("dependency auto-start" in note for note in notes)

    @pytest.mark.asyncio
    async def test_preflight_fails_when_dependency_stays_unreachable(
        self,
        lifecycle: AppLifecycleManager,
        tmp_path,
    ) -> None:
        """自動起動後も依存が待受しない場合は失敗を返す."""
        cfg = AppConfig(
            name="dep_fail_app",
            display_name="Dependency Fail App",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
            ports={"db": 5433},
            dependencies={"database": "postgresql"},
        )
        config_path = tmp_path / "apps" / "dep_fail_app" / "app_config.json"
        config_path.parent.mkdir(parents=True)
        config_path.write_text("{}", encoding="utf-8")
        (config_path.parent / "docker-compose.yml").write_text("services: {}\n", encoding="utf-8")

        with patch.object(
            lifecycle,
            "_is_tcp_port_open",
            side_effect=[False, False],
        ), patch.object(
            lifecycle,
            "_auto_start_local_dependencies",
            new=AsyncMock(return_value=(["dependency auto-start: docker compose up -d faq-db"], None)),
        ):
            notes, error = await lifecycle._preflight_local_dependencies(cfg, config_path=config_path)

        assert error is not None
        assert "localhost:5433" in error
        assert any("dependency auto-start" in note for note in notes)

    @pytest.mark.asyncio
    async def test_start_ignores_readme_foreground_command(
        self,
        lifecycle: AppLifecycleManager,
        tmp_path,
    ) -> None:
        """README の前景 backend コマンドは start 判定に使わない."""
        cfg = AppConfig(
            name="readme_app",
            display_name="README App",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
            ports={"api": None},
            entry_points={"health": None},
            runtime={
                "commands": {
                    "backend_dev": "python -m apps.readme_app.main --reload",
                    "start": "docker compose -f docker-compose.yml up -d",
                }
            },
        )
        app_dir = tmp_path / "readme_app"
        app_dir.mkdir(parents=True)
        (app_dir / "README.md").write_text(
            "```bash\npython -m apps.readme_app.main --reload\n```",
            encoding="utf-8",
        )
        (app_dir / "docker-compose.yml").write_text("services: {}\n", encoding="utf-8")
        config_path = app_dir / "app_config.json"
        config_path.write_text("{}", encoding="utf-8")

        compose_result = AppActionResult(
            app_name="readme_app",
            action="start",
            success=True,
            command=["docker", "compose", "up", "-d"],
            cwd=str(app_dir),
            command_source="fallback",
            execution_mode="docker",
        )

        with patch.object(
            lifecycle,
            "_run_compose_action",
            new=AsyncMock(return_value=compose_result),
        ) as mocked_compose, patch.object(
            lifecycle,
            "_detect_execution_mode",
            new=AsyncMock(return_value=("docker", {"reason": "compose_first_default"})),
        ), patch.object(
            lifecycle,
            "_run_cli_preflight",
            new=AsyncMock(return_value={"ready": False, "final": {"available_tools": [], "authenticated_tools": []}}),
        ):
            result = await lifecycle.start_app(cfg, config_path=config_path)

        assert result.success is True
        assert result.execution_mode == "docker"
        mocked_compose.assert_called_once()

    @pytest.mark.asyncio
    async def test_start_retry_stops_after_success(
        self,
        lifecycle: AppLifecycleManager,
        tmp_path,
    ) -> None:
        """AI 修復ループは成功時点で停止する."""
        cfg = AppConfig(
            name="retry_success_app",
            display_name="Retry Success App",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
            ports={"api": None},
            entry_points={"health": None},
        )
        config_path = tmp_path / "retry_success_app" / "app_config.json"
        config_path.parent.mkdir(parents=True)
        config_path.write_text("{}", encoding="utf-8")

        failed = AppActionResult(
            app_name=cfg.name,
            action="start",
            success=False,
            command=["docker", "compose", "up", "-d"],
            cwd=str(config_path.parent),
            error="boom",
            diagnostic={"summary": "boom"},
            execution_mode="docker",
        )
        recovered = AppActionResult(
            app_name=cfg.name,
            action="start",
            success=True,
            command=["docker", "compose", "up", "-d"],
            cwd=str(config_path.parent),
            execution_mode="docker",
        )

        with patch.object(
            lifecycle,
            "_run_cli_preflight",
            new=AsyncMock(return_value={"ready": True, "final": {"available_tools": ["codex"], "authenticated_tools": ["codex"]}}),
        ), patch.object(
            lifecycle._diagnostic_service,
            "attach_retry_trace",
            new=lambda diagnostic, _: diagnostic,
        ), patch.object(
            lifecycle,
            "_start_app_once",
            new=AsyncMock(side_effect=[failed, recovered]),
        ), patch.object(
            lifecycle._repair_service,
            "attempt_action_repair",
            new=AsyncMock(return_value={"tool": "codex", "attempt": 1, "success": True}),
        ) as mocked_repair:
            result = await lifecycle.start_app(cfg, config_path=config_path)

        assert result.success is True
        assert result.repair is not None
        assert result.repair.get("total_attempts") == 1
        assert result.repair.get("outcome") == "success_after_repair"
        mocked_repair.assert_called_once()

    @pytest.mark.asyncio
    async def test_start_retry_stops_at_max_attempts(
        self,
        lifecycle: AppLifecycleManager,
        tmp_path,
    ) -> None:
        """AI 修復ループは最大 4 回で停止する."""
        cfg = AppConfig(
            name="retry_fail_app",
            display_name="Retry Fail App",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
            ports={"api": None},
            entry_points={"health": None},
        )
        config_path = tmp_path / "retry_fail_app" / "app_config.json"
        config_path.parent.mkdir(parents=True)
        config_path.write_text("{}", encoding="utf-8")

        failed = AppActionResult(
            app_name=cfg.name,
            action="start",
            success=False,
            command=["docker", "compose", "up", "-d"],
            cwd=str(config_path.parent),
            error="boom",
            diagnostic={"summary": "boom"},
            execution_mode="docker",
        )

        with patch.object(
            lifecycle,
            "_run_cli_preflight",
            new=AsyncMock(return_value={"ready": True, "final": {"available_tools": ["codex", "claude"], "authenticated_tools": ["codex", "claude"]}}),
        ), patch.object(
            lifecycle._diagnostic_service,
            "attach_retry_trace",
            new=lambda diagnostic, _: diagnostic,
        ), patch.object(
            lifecycle,
            "_start_app_once",
            new=AsyncMock(side_effect=[failed, failed, failed, failed, failed]),
        ), patch.object(
            lifecycle._repair_service,
            "attempt_action_repair",
            new=AsyncMock(return_value={"tool": "codex", "attempt": 1, "success": False}),
        ) as mocked_repair:
            result = await lifecycle.start_app(cfg, config_path=config_path)

        assert result.success is False
        assert result.repair is not None
        assert result.repair.get("total_attempts") == 4
        assert result.repair.get("outcome") == "failed"
        assert mocked_repair.await_count == 4

    @pytest.mark.asyncio
    async def test_start_failure_includes_diagnostic_payload(
        self,
        lifecycle: AppLifecycleManager,
        tmp_path,
    ) -> None:
        """起動失敗時に diagnostic が保持される."""
        cfg = AppConfig(
            name="diag_app",
            display_name="Diag App",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
            ports={"api": None},
            entry_points={"health": None},
        )
        config_path = tmp_path / "diag_app" / "app_config.json"
        config_path.parent.mkdir(parents=True)
        config_path.write_text("{}", encoding="utf-8")

        failed = AppActionResult(
            app_name="diag_app",
            action="start",
            success=False,
            command=["docker", "compose", "up", "-d"],
            cwd=str(config_path.parent),
            error="boom",
            diagnostic={"summary": "boom"},
            execution_mode="docker",
        )

        with patch.object(
            lifecycle,
            "_run_cli_preflight",
            new=AsyncMock(return_value={"ready": False, "final": {"available_tools": [], "authenticated_tools": []}}),
        ), patch.object(
            lifecycle,
            "_start_app_once",
            new=AsyncMock(return_value=failed),
        ):
            result = await lifecycle.start_app(cfg, config_path=config_path)

        assert result.success is False
        assert result.diagnostic is not None
        assert result.diagnostic.get("summary") == "boom"

    def test_sanitize_local_process_env_rewrites_invalid_debug(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """無効な DEBUG 値は local 起動時に false へ補正する."""
        monkeypatch.setenv("DEBUG", "release")
        env = AppLifecycleManager._sanitize_local_process_env()
        assert env["DEBUG"] == "false"

    @pytest.mark.asyncio
    async def test_start_local_dev_returns_cancelled_result(
        self,
        lifecycle: AppLifecycleManager,
        tmp_path,
    ) -> None:
        """local_start 中断時は CancelledError を外へ漏らさない."""
        cfg = AppConfig(
            name="cancel_local_start_app",
            display_name="Cancel Local Start App",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
            ports={"api": None},
            entry_points={"health": None},
        )
        config_path = tmp_path / "cancel_local_start_app" / "app_config.json"
        config_path.parent.mkdir(parents=True)
        config_path.write_text("{}", encoding="utf-8")

        with patch.object(
            lifecycle,
            "_run_cli_preflight",
            new=AsyncMock(return_value={"ready": False, "final": {"available_tools": [], "authenticated_tools": []}}),
        ), patch.object(
            lifecycle,
            "_start_local_dev_once",
            new=AsyncMock(side_effect=asyncio.CancelledError),
        ), patch.object(
            lifecycle,
            "_stop_local_processes",
            new=AsyncMock(return_value={"success": True, "killed": [], "errors": []}),
        ) as mocked_stop:
            result = await lifecycle.start_local_dev(cfg, config_path=config_path)

        assert result.success is False
        assert "cancelled" in (result.error or "")
        assert result.repair is not None
        assert result.repair.get("outcome") == "cancelled"
        mocked_stop.assert_awaited_once()

    @pytest.mark.asyncio
    async def test_start_local_process_handles_cancelled_boot_wait(
        self,
        lifecycle: AppLifecycleManager,
        tmp_path,
    ) -> None:
        """起動待機中にキャンセルされても失敗結果で返す."""

        class _Proc:
            returncode = 0

            async def communicate(self):
                return b"12345\n", b""

        with patch(
            "apps.platform.services.app_lifecycle.asyncio.create_subprocess_shell",
            new=AsyncMock(return_value=_Proc()),
        ), patch(
            "apps.platform.services.app_lifecycle.asyncio.sleep",
            new=AsyncMock(side_effect=asyncio.CancelledError),
        ):
            launch = await lifecycle._start_local_process(
                app_name="cancel_wait_app",
                command="python -m apps.cancel_wait_app.main",
                role="backend",
                cwd=tmp_path,
            )

        assert launch.success is False
        assert launch.error is not None
        assert "キャンセル" in launch.error
