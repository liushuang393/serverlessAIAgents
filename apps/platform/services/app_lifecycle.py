"""App Lifecycle Manager — App のヘルスチェック・起動/停止管理.

各 App の稼働状態を HTTP ヘルスチェックで確認し、
起動/停止指示を発行するサービス。

使用例:
    >>> manager = AppLifecycleManager(discovery)
    >>> result = await manager.check_health("faq_system")
"""

from __future__ import annotations

import asyncio
import json
import logging
import os
import re
import shlex
import signal
import socket
import subprocess
import time
from contextlib import suppress
from dataclasses import dataclass
from datetime import UTC, datetime
from enum import Enum
from pathlib import Path
from typing import TYPE_CHECKING, Any, Literal

import httpx
from apps.platform.services.cli_diagnostic_service import CLIDiagnosticService
from apps.platform.services.cli_repair_service import CLIRepairService
from apps.platform.services.runtime_command_resolver import (
    ResolvedRuntimeCommands,
    RuntimeCommandResolver,
)

from agentflow.tools.cli.runtime_manager import CLIRuntimeManager


if TYPE_CHECKING:
    from collections.abc import Awaitable, Callable

    from apps.platform.schemas.app_config_schemas import AppConfig


_logger = logging.getLogger(__name__)

# ヘルスチェックのタイムアウト（秒）
_HEALTH_TIMEOUT = 2.0
_ACTION_TIMEOUT = 900.0
_HEALTH_FALLBACK_PATHS = ("/api/health", "/health", "/healthz")
_HEALTHY_PAYLOAD_STATES = {"ok", "healthy", "up", "ready", "pass"}
_NON_HEALTHY_PAYLOAD_STATES = {
    "error",
    "failed",
    "fail",
    "down",
    "degraded",
    "starting",
    "initializing",
}
_DOCKER_COMPOSE_FILES = ("docker-compose.yml", "compose.yml")
_DOCKER_COMPOSE_DEV_FILES = ("docker-compose.dev.yml", "compose.override.yml")
_BACKEND_SERVICE_HINTS = ("backend", "api", "server")
_PORT_MAPPING_RE = re.compile(r"(?P<published>\d+)->(?P<target>\d+)")
_LOCAL_BOOT_WAIT_SECONDS = 2.0
_LOCAL_LOG_TAIL_LINES = 40
_LOCAL_DEPENDENCY_TIMEOUT = 0.4
_LOCAL_EXTERNAL_DB_KINDS = {"postgresql", "postgres", "mysql", "mariadb", "redis"}
_LOCAL_READINESS_TIMEOUT_SECONDS = 45.0
_LOCAL_READINESS_POLL_SECONDS = 1.0
_LOCAL_READINESS_STABLE_STREAK = 2
_REPAIR_TOOL_ORDER: tuple[Literal["codex", "claude"], ...] = ("codex", "claude")
_REPAIR_MAX_ATTEMPTS_PER_TOOL = 2

# WSL / Linux 環境で conda agentflow を活性化するシェルプレフィックス
_CONDA_ACTIVATE_PREFIX = (
    'eval "$(conda shell.bash hook)" && conda activate agentflow && '
)

ExecutionMode = Literal["local", "docker"]


class AppStatus(str, Enum):
    """App の稼働状態."""

    HEALTHY = "healthy"
    UNHEALTHY = "unhealthy"
    UNKNOWN = "unknown"
    STOPPED = "stopped"


class HealthCheckResult:
    """ヘルスチェック結果.

    Attributes:
        app_name: App 識別子
        status: 稼働状態
        response_time_ms: レスポンス時間（ミリ秒）
        checked_at: チェック日時
        details: 追加情報
        error: エラーメッセージ（異常時のみ）
    """

    __slots__ = (
        "app_name",
        "checked_at",
        "details",
        "error",
        "response_time_ms",
        "status",
    )

    def __init__(
        self,
        app_name: str,
        status: AppStatus,
        *,
        response_time_ms: float = 0.0,
        details: dict[str, Any] | None = None,
        error: str | None = None,
    ) -> None:
        """初期化."""
        self.app_name = app_name
        self.status = status
        self.response_time_ms = response_time_ms
        self.checked_at = datetime.now(UTC)
        self.details = details or {}
        self.error = error

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        result: dict[str, Any] = {
            "app_name": self.app_name,
            "status": self.status.value,
            "response_time_ms": round(self.response_time_ms, 2),
            "checked_at": self.checked_at.isoformat(),
        }
        if self.details:
            result["details"] = self.details
        if self.error:
            result["error"] = self.error
        return result


@dataclass(slots=True)
class LocalProcessLaunchResult:
    """ローカル起動コマンドの実行結果."""

    role: str
    command: str
    success: bool
    pid: int | None
    log_path: str
    message: str | None = None
    error: str | None = None


class AppActionResult:
    """App 操作（publish/start/stop）結果."""

    __slots__ = (
        "action",
        "app_name",
        "checked_health",
        "command",
        "command_source",
        "cwd",
        "diagnostic",
        "error",
        "execution_mode",
        "repair",
        "return_code",
        "stderr",
        "stdout",
        "success",
    )

    def __init__(
        self,
        *,
        app_name: str,
        action: str,
        success: bool,
        command: list[str],
        cwd: str,
        command_source: str = "fallback",
        return_code: int | None = None,
        stdout: str = "",
        stderr: str = "",
        error: str | None = None,
        checked_health: HealthCheckResult | None = None,
        diagnostic: dict[str, Any] | None = None,
        execution_mode: ExecutionMode | None = None,
        repair: dict[str, Any] | None = None,
    ) -> None:
        self.app_name = app_name
        self.action = action
        self.success = success
        self.command = command
        self.cwd = cwd
        self.command_source = command_source
        self.return_code = return_code
        self.stdout = stdout
        self.stderr = stderr
        self.error = error
        self.checked_health = checked_health
        self.diagnostic = diagnostic
        self.execution_mode = execution_mode
        self.repair = repair

    def to_dict(self) -> dict[str, Any]:
        """辞書へ変換."""
        data: dict[str, Any] = {
            "app_name": self.app_name,
            "action": self.action,
            "success": self.success,
            "command": " ".join(shlex.quote(part) for part in self.command),
            "command_source": self.command_source,
            "cwd": self.cwd,
            "return_code": self.return_code,
        }
        if self.execution_mode is not None:
            data["execution_mode"] = self.execution_mode
        if self.stdout:
            data["stdout"] = self.stdout
        if self.stderr:
            data["stderr"] = self.stderr
        if self.error:
            data["error"] = self.error
        if self.checked_health is not None:
            data["health"] = self.checked_health.to_dict()
        if self.diagnostic is not None:
            data["diagnostic"] = self.diagnostic
        if self.repair is not None:
            data["repair"] = self.repair
        return data


class AppLifecycleManager:
    """App ライフサイクル管理サービス.

    ヘルスチェック、起動/停止操作を提供。
    """

    def __init__(self) -> None:
        """初期化."""
        self._health_cache: dict[str, HealthCheckResult] = {}
        self._health_tasks: dict[str, asyncio.Task[HealthCheckResult]] = {}
        self._command_resolver = RuntimeCommandResolver()
        self._cli_runtime = CLIRuntimeManager()
        self._diagnostic_service = CLIDiagnosticService(self._cli_runtime)
        self._repair_service = CLIRepairService(self._cli_runtime)

    async def check_health(
        self,
        config: AppConfig,
        *,
        config_path: Path | None = None,
    ) -> HealthCheckResult:
        """App のヘルスチェックを実行.

        Args:
            config: 対象 App の設定

        Returns:
            ヘルスチェック結果
        """
        existing = self._health_tasks.get(config.name)
        if existing is not None and not existing.done():
            return await asyncio.shield(existing)

        task = asyncio.create_task(
            self._check_health_once(config, config_path=config_path),
            name=f"health-check:{config.name}",
        )
        self._health_tasks[config.name] = task
        try:
            return await asyncio.shield(task)
        finally:
            if self._health_tasks.get(config.name) is task:
                self._health_tasks.pop(config.name, None)

    async def _check_health_once(
        self,
        config: AppConfig,
        *,
        config_path: Path | None = None,
    ) -> HealthCheckResult:
        """App のヘルスチェック本体."""
        docker_info = await self._inspect_docker_runtime(config_path)
        health_paths = self._resolve_health_paths(config.entry_points.health)
        candidate_urls = self._build_candidate_urls(config, health_paths, docker_info)

        # URL 候補が生成できない場合は UNKNOWN
        if not candidate_urls:
            result = HealthCheckResult(
                config.name,
                AppStatus.UNKNOWN,
                error="ヘルスチェックURLを構築できませんでした",
                details={
                    "health_paths": health_paths,
                    "docker": docker_info,
                },
            )
            self._health_cache[config.name] = result
            return result

        attempts: list[dict[str, Any]] = []
        any_http_response = False
        any_timeout = False
        any_connect_error = False
        last_error: str | None = None
        unreachable_ports: set[int] = set()

        try:
            async with httpx.AsyncClient(timeout=_HEALTH_TIMEOUT, trust_env=False) as client:
                for url in candidate_urls:
                    port = httpx.URL(url).port
                    if port is not None and port in unreachable_ports:
                        continue

                    start = time.monotonic()
                    attempt: dict[str, Any] = {"url": url}
                    try:
                        response = await client.get(url)
                        elapsed_ms = (time.monotonic() - start) * 1000
                        payload = self._extract_json_payload(response)
                        payload_state = self._extract_payload_state(payload)

                        attempt["response_time_ms"] = round(elapsed_ms, 2)
                        attempt["http_status"] = response.status_code
                        if payload_state is not None:
                            attempt["payload_state"] = payload_state
                        if payload is not None:
                            attempt["payload"] = payload

                        attempts.append(attempt)
                        any_http_response = True

                        is_2xx = 200 <= response.status_code < 300
                        payload_marked_bad = payload_state is not None and payload_state in _NON_HEALTHY_PAYLOAD_STATES
                        payload_marked_good = payload_state is not None and payload_state in _HEALTHY_PAYLOAD_STATES

                        # 2xx かつ payload が明示的に bad でない場合のみ healthy
                        if is_2xx and not payload_marked_bad:
                            result = HealthCheckResult(
                                config.name,
                                AppStatus.HEALTHY,
                                response_time_ms=elapsed_ms,
                                details={
                                    "checked_url": url,
                                    "http_status": response.status_code,
                                    "payload_state": payload_state,
                                    "payload": payload,
                                    "attempts": attempts,
                                    "docker": docker_info,
                                },
                            )
                            self._health_cache[config.name] = result
                            return result

                        if is_2xx and not payload_marked_good and payload_state is None:
                            attempt["error"] = "2xx response without explicit status field"
                        elif payload_marked_bad:
                            attempt["error"] = f"payload indicates non-healthy state: {payload_state}"
                        else:
                            attempt["error"] = f"HTTP {response.status_code}"
                        last_error = attempt["error"]
                    except httpx.ConnectError as exc:
                        any_connect_error = True
                        last_error = str(exc)
                        if port is not None:
                            unreachable_ports.add(port)
                        attempts.append({"url": url, "error": "connect_error", "message": str(exc)})
                    except httpx.TimeoutException as exc:
                        any_timeout = True
                        last_error = str(exc)
                        if port is not None:
                            unreachable_ports.add(port)
                        attempts.append({"url": url, "error": "timeout", "message": str(exc)})
                    except Exception as exc:
                        last_error = str(exc)
                        attempts.append({"url": url, "error": "exception", "message": str(exc)})
        except Exception as exc:
            last_error = str(exc)
            attempts.append({"url": "client_setup", "error": "exception", "message": str(exc)})

        backend_running = bool(docker_info.get("backend_running"))
        if any_http_response or any_timeout or backend_running:
            status = AppStatus.UNHEALTHY
            error_message = last_error or "ヘルスチェック結果が不正です"
        elif any_connect_error:
            status = AppStatus.STOPPED
            error_message = last_error or "接続拒否"
        else:
            status = AppStatus.UNKNOWN
            error_message = last_error or "ヘルスチェック実行不可"

        result = HealthCheckResult(
            config.name,
            status,
            error=error_message,
            details={
                "attempts": attempts,
                "candidate_urls": candidate_urls,
                "health_paths": health_paths,
                "docker": docker_info,
            },
        )
        self._health_cache[config.name] = result
        return result

    async def publish_app(
        self,
        config: AppConfig,
        *,
        config_path: Path | None = None,
        _repair_enabled: bool = True,
    ) -> AppActionResult:
        """App を publish（docker 優先）する."""
        preflight = await self._run_cli_preflight(config, enabled=True)
        initial = await self._publish_app_once(
            config,
            config_path=config_path,
            cli_preflight=preflight,
        )
        if not _repair_enabled:
            return self._attach_repair_result(initial, attempts=[], outcome="disabled")
        return await self._run_action_with_repair(
            config=config,
            action="publish",
            config_path=config_path,
            cli_preflight=preflight,
            initial_result=initial,
            retry_callback=lambda: self.publish_app(
                config,
                config_path=config_path,
                _repair_enabled=False,
            ),
        )

    async def _publish_app_once(
        self,
        config: AppConfig,
        *,
        config_path: Path | None,
        cli_preflight: dict[str, Any] | None,
    ) -> AppActionResult:
        """publish 1-step: docker compose up -d / up -d --build."""
        if config_path is None:
            return AppActionResult(
                app_name=config.name,
                action="publish",
                success=False,
                command=[],
                cwd="",
                error="app_config.json のパスが取得できません",
                command_source="fallback",
                execution_mode="docker",
            )

        app_dir = config_path.parent
        compose_files = self._resolve_compose_files(app_dir, include_dev=True)
        if not compose_files:
            # compose が無い app はローカルプロセス publish にフォールバック
            result = await self._run_process_action(
                config,
                app_dir=app_dir,
                config_path=config_path,
                action="publish",
                run_health_check=True,
                cli_preflight=cli_preflight,
            )
            result.execution_mode = "local"
            return result

        publish_subcommand, publish_mode = await self._resolve_publish_subcommand(
            app_dir=app_dir,
            compose_files=compose_files,
        )
        result = await self._run_compose_action(
            config,
            config_path=config_path,
            action="publish",
            compose_subcommand=publish_subcommand,
            run_health_check=True,
            cli_preflight=cli_preflight,
            allow_process_fallback=False,
            allow_command_override=False,
        )
        result.execution_mode = "docker"
        mode_note = f"publish_mode={publish_mode}"
        result.stdout = "\n".join(part for part in (mode_note, result.stdout) if part)
        return result

    async def start_app(
        self,
        config: AppConfig,
        *,
        config_path: Path | None = None,
        _repair_enabled: bool = True,
    ) -> AppActionResult:
        """App を起動（実行モード判定後、docker/local いずれかのみ実行）."""
        preflight = await self._run_cli_preflight(config, enabled=True)
        initial = await self._start_app_once(
            config,
            config_path=config_path,
            cli_preflight=preflight,
        )
        if not _repair_enabled:
            return self._attach_repair_result(initial, attempts=[], outcome="disabled")
        return await self._run_action_with_repair(
            config=config,
            action="start",
            config_path=config_path,
            cli_preflight=preflight,
            initial_result=initial,
            retry_callback=lambda: self.start_app(
                config,
                config_path=config_path,
                _repair_enabled=False,
            ),
        )

    async def _start_app_once(
        self,
        config: AppConfig,
        *,
        config_path: Path | None,
        cli_preflight: dict[str, Any] | None,
    ) -> AppActionResult:
        if config_path is None:
            return AppActionResult(
                app_name=config.name,
                action="start",
                success=False,
                command=[],
                cwd="",
                error="app_config.json のパスが取得できません",
                command_source="fallback",
                execution_mode="docker",
            )
        mode, mode_details = await self._detect_execution_mode(
            app_name=config.name,
            config_path=config_path,
        )
        if mode == "docker":
            result = await self._run_compose_action(
                config,
                config_path=config_path,
                action="start",
                compose_subcommand=["up", "-d"],
                run_health_check=True,
                cli_preflight=cli_preflight,
                allow_process_fallback=False,
                allow_command_override=False,
            )
        else:
            local_running = bool(mode_details.get("local", {}).get("running"))
            if local_running:
                running_roles = mode_details.get("local", {}).get("running_roles", [])
                checked_health = await self.check_health(config, config_path=config_path)
                return AppActionResult(
                    app_name=config.name,
                    action="start",
                    success=True,
                    command=["process_mode", "start", config.name],
                    command_source="process_mode",
                    cwd=str(config_path.parent.parent.parent),
                    stdout=f"local process already running: {running_roles}",
                    checked_health=checked_health,
                    execution_mode="local",
                )
            result = await self._run_process_action(
                config,
                app_dir=config_path.parent,
                config_path=config_path,
                action="start",
                run_health_check=True,
                cli_preflight=cli_preflight,
            )
        result.execution_mode = mode
        if not result.success:
            result.stderr = "\n".join(
                part for part in (result.stderr, self._trim_output(json.dumps(mode_details, ensure_ascii=False))) if part
            )
        return result

    async def stop_app(
        self,
        config: AppConfig,
        *,
        config_path: Path | None = None,
        _repair_enabled: bool = True,
    ) -> AppActionResult:
        """App を停止（実行モード判定後、docker/local いずれかのみ実行）."""
        preflight = await self._run_cli_preflight(config, enabled=True)
        initial = await self._stop_app_once(
            config,
            config_path=config_path,
        )
        if not _repair_enabled:
            return self._attach_repair_result(initial, attempts=[], outcome="disabled")
        return await self._run_action_with_repair(
            config=config,
            action="stop",
            config_path=config_path,
            cli_preflight=preflight,
            initial_result=initial,
            retry_callback=lambda: self.stop_app(
                config,
                config_path=config_path,
                _repair_enabled=False,
            ),
        )

    async def _stop_app_once(
        self,
        config: AppConfig,
        *,
        config_path: Path | None,
    ) -> AppActionResult:
        if config_path is None:
            return AppActionResult(
                app_name=config.name,
                action="stop",
                success=False,
                command=[],
                cwd="",
                error="app_config.json のパスが取得できません",
                command_source="fallback",
                execution_mode="docker",
            )
        mode, _ = await self._detect_execution_mode(
            app_name=config.name,
            config_path=config_path,
        )
        if mode == "docker":
            result = await self._run_compose_action(
                config,
                config_path=config_path,
                action="stop",
                compose_subcommand=["down"],
                run_health_check=False,
                cli_preflight=None,
                allow_process_fallback=False,
                allow_command_override=False,
            )
        else:
            result = await self._run_process_action(
                config,
                app_dir=config_path.parent,
                config_path=config_path,
                action="stop",
                run_health_check=False,
                cli_preflight=None,
            )
        result.execution_mode = mode
        return result

    async def cli_status(self, config: AppConfig) -> dict[str, Any]:
        """CLI 現在状態を取得する."""
        runtime_cli = config.runtime.cli.model_dump()
        return await self._cli_runtime.detect(runtime_cli)

    async def cli_setup(self, config: AppConfig) -> dict[str, Any]:
        """CLI の検出・インストール・認証を一括実行する."""
        runtime_cli = config.runtime.cli.model_dump()
        return await self._cli_runtime.preflight(runtime_cli)

    async def start_local_dev(
        self,
        config: AppConfig,
        *,
        config_path: Path | None = None,
        _repair_enabled: bool = True,
    ) -> AppActionResult:
        """App をローカル開発モードで起動（バックエンド・フロントエンドをバックグラウンドで起動）.

        runtime.commands.backend_dev / frontend_dev のコマンドを実行する。
        DBはDockerで起動済みを前提とし、アプリのみをローカルで実行する。

        Args:
            config: App 設定
            config_path: app_config.json のパス

        Returns:
            実行結果
        """
        cli_preflight = await self._run_cli_preflight(config, enabled=True)
        initial = await self._start_local_dev_once(
            config,
            config_path=config_path,
            cli_preflight=cli_preflight,
        )
        if not _repair_enabled:
            return self._attach_repair_result(initial, attempts=[], outcome="disabled")
        return await self._run_action_with_repair(
            config=config,
            action="local_start",
            config_path=config_path,
            cli_preflight=cli_preflight,
            initial_result=initial,
            retry_callback=lambda: self.start_local_dev(
                config,
                config_path=config_path,
                _repair_enabled=False,
            ),
        )

    async def _start_local_dev_once(
        self,
        config: AppConfig,
        *,
        config_path: Path | None,
        cli_preflight: dict[str, Any] | None,
    ) -> AppActionResult:
        _logger.info("[%s] local_start 開始", config.name)

        if config_path is None:
            _logger.warning("[%s] config_path が None — 中断", config.name)
            return AppActionResult(
                app_name=config.name,
                action="local_start",
                success=False,
                command=[],
                cwd="",
                error="app_config.json のパスが取得できません",
                command_source="fallback",
                execution_mode="local",
            )

        # プロジェクトルートを取得（app_config.json の2階層上）
        project_root = config_path.parent.parent.parent
        resolved_commands = self._resolve_runtime_commands(config, config_path)

        backend_cmd = resolved_commands.get("backend_dev")
        frontend_cmd = resolved_commands.get("frontend_dev")

        if backend_cmd is None and frontend_cmd is None:
            diagnostic = await self._diagnostic_service.diagnose_action_failure(
                app_config=config,
                action="local_start",
                config_path=config_path,
                command=[],
                cwd=str(project_root),
                error="backend/frontend command is missing",
                stdout="",
                stderr="No backend_dev/frontend_dev command resolved from README/runtime",
                preflight=cli_preflight,
                command_source="fallback",
            )
            return AppActionResult(
                app_name=config.name,
                action="local_start",
                success=False,
                command=[],
                cwd=str(project_root),
                error="runtime.commands に backend_dev/frontend_dev が設定されていません",
                command_source="fallback",
                diagnostic=diagnostic,
                execution_mode="local",
            )

        results: list[str] = []
        errors: list[str] = []

        preflight_notes, preflight_error = await self._preflight_local_dependencies(
            config,
            config_path=config_path,
        )
        results.extend(preflight_notes)
        if preflight_error is not None:
            diagnostic = await self._diagnostic_service.diagnose_action_failure(
                app_config=config,
                action="local_start",
                config_path=config_path,
                command=["bash", "-lc", backend_cmd or "", frontend_cmd or ""],
                cwd=str(project_root),
                error=preflight_error,
                stdout="\n".join(results),
                stderr="",
                preflight=cli_preflight,
                command_source=self._merge_command_source(
                    resolved_commands.source.get("backend_dev", "fallback"),
                    resolved_commands.source.get("frontend_dev", "fallback"),
                ),
            )
            return AppActionResult(
                app_name=config.name,
                action="local_start",
                success=False,
                command=["bash", "-lc", backend_cmd or "", frontend_cmd or ""],
                cwd=str(project_root),
                stdout="\n".join(results),
                stderr="",
                error=preflight_error,
                command_source=self._merge_command_source(
                    resolved_commands.source.get("backend_dev", "fallback"),
                    resolved_commands.source.get("frontend_dev", "fallback"),
                ),
                diagnostic=diagnostic,
                execution_mode="local",
            )

        started_processes: list[LocalProcessLaunchResult] = []

        # バックエンド起動（バックグラウンド）
        if backend_cmd:
            try:
                launch = await self._start_local_process(
                    app_name=config.name,
                    command=backend_cmd,
                    role="backend",
                    cwd=project_root,
                )
                if launch.success and launch.message:
                    started_processes.append(launch)
                    results.append(launch.message)
                if not launch.success and launch.error:
                    errors.append(launch.error)
            except Exception as exc:
                errors.append(f"backend error: {exc}")
                _logger.exception("[%s] バックエンド起動失敗", config.name)

        # フロントエンド起動（バックグラウンド）
        if frontend_cmd:
            try:
                launch = await self._start_local_process(
                    app_name=config.name,
                    command=frontend_cmd,
                    role="frontend",
                    cwd=project_root,
                )
                if launch.success and launch.message:
                    started_processes.append(launch)
                    results.append(launch.message)
                if not launch.success and launch.error:
                    errors.append(launch.error)
            except Exception as exc:
                errors.append(f"frontend error: {exc}")
                _logger.exception("[%s] フロントエンド起動失敗", config.name)

        if not errors and started_processes:
            readiness_ok, readiness_notes, readiness_error = await self._verify_local_runtime_readiness(
                config=config,
                config_path=config_path,
                launches=started_processes,
            )
            results.extend(readiness_notes)
            if not readiness_ok and readiness_error:
                errors.append(readiness_error)

        success = len(errors) == 0 and len(results) > 0
        stdout_msg = "\n".join(results) if results else ""
        error_msg = "\n".join(errors) if errors else None
        command_source = self._merge_command_source(
            resolved_commands.source.get("backend_dev", "fallback"),
            resolved_commands.source.get("frontend_dev", "fallback"),
        )
        diagnostic: dict[str, Any] | None = None
        if not success:
            diagnostic = await self._diagnostic_service.diagnose_action_failure(
                app_config=config,
                action="local_start",
                config_path=config_path,
                command=["bash", "-lc", backend_cmd or "", frontend_cmd or ""],
                cwd=str(project_root),
                error=error_msg,
                stdout=stdout_msg,
                stderr="",
                preflight=cli_preflight,
                command_source=command_source,
            )

        checked_health: HealthCheckResult | None = None
        if success:
            checked_health = await self.check_health(config, config_path=config_path)

        return AppActionResult(
            app_name=config.name,
            action="local_start",
            success=success,
            command=["bash", "-lc", backend_cmd or "", frontend_cmd or ""],
            cwd=str(project_root),
            stdout=stdout_msg,
            stderr="",
            error=error_msg,
            command_source=command_source,
            diagnostic=diagnostic,
            checked_health=checked_health,
            execution_mode="local",
        )

    async def _preflight_local_dependencies(
        self,
        config: AppConfig,
        *,
        config_path: Path | None,
    ) -> tuple[list[str], str | None]:
        """Local 起動前に依存サービスの待受を確認."""
        notes: list[str] = []
        targets: list[tuple[str, str, int]] = []
        db_kind = (config.dependencies.database or config.runtime.database.kind or "").strip().lower()
        db_port = config.runtime.database.port
        if not isinstance(db_port, int):
            db_port = config.ports.db
        if db_kind in _LOCAL_EXTERNAL_DB_KINDS and isinstance(db_port, int):
            targets.append(("database", db_kind, db_port))

        if config.dependencies.redis and isinstance(config.ports.redis, int):
            targets.append(("redis", "redis", config.ports.redis))

        if not targets:
            return notes, None

        first_failure: tuple[str, str, int] | None = None
        for label, kind, port in targets:
            reachable = self._is_tcp_port_open(
                "127.0.0.1",
                port,
                _LOCAL_DEPENDENCY_TIMEOUT,
            )
            if reachable:
                notes.append(f"dependency check: {label} ({kind}) localhost:{port} reachable")
                continue
            first_failure = (label, kind, port)
            break

        if first_failure is None:
            return notes, None

        label, kind, port = first_failure
        compose_detected = False
        if config_path is not None:
            compose_detected = bool(self._resolve_compose_files(config_path.parent, include_dev=True))
        message = f"local_start 前提チェック失敗: {label} ({kind}) が localhost:{port} で待受していません。"
        if compose_detected:
            notes.append("docker compose file detected for this app")
            message += " docker compose ファイルを検出しました。先に Start/Publish を実行してください。"
        else:
            notes.append("docker compose file not detected for this app")
            message += " docker compose ファイルは未検出です。DB/依存サービスを手動起動してください。"
        message += " 先に Start/Publish で依存コンテナを起動するか、DB を手動起動してください。"
        return notes, message

    async def _start_local_process(
        self,
        *,
        app_name: str,
        command: str,
        role: str,
        cwd: Path,
    ) -> LocalProcessLaunchResult:
        """Local 開発コマンドをバックグラウンド起動し、生存確認する.

        WSL / Linux 環境で conda agentflow を活性化してから実行する。
        """
        log_path = f"/tmp/{app_name}_{role}.log"
        pid_file = f"/tmp/{app_name}_{role}.pid"
        # conda agentflow 環境を活性化してからコマンドを実行する
        activated_command = f"{_CONDA_ACTIVATE_PREFIX}{command}"
        launch_cmd = (
            f"nohup bash -lc {shlex.quote(activated_command)}"
            f" > {shlex.quote(log_path)} 2>&1"
            f" & PID=$!; echo $PID; echo $PID > {shlex.quote(pid_file)}"
        )
        _logger.info("[%s] %s 起動 (conda agentflow): %s", app_name, role, command)
        proc = await asyncio.create_subprocess_shell(
            launch_cmd,
            cwd=str(cwd),
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
        )
        stdout_bytes, stderr_bytes = await proc.communicate()
        stdout = stdout_bytes.decode("utf-8", errors="ignore").strip()
        stderr = stderr_bytes.decode("utf-8", errors="ignore").strip()
        return_code = getattr(proc, "returncode", 0)
        if return_code not in (0, None):
            error = stderr or stdout or f"{role} の起動コマンド実行に失敗"
            return LocalProcessLaunchResult(
                role=role,
                command=command,
                success=False,
                pid=None,
                log_path=log_path,
                error=error,
            )

        pid: int | None = None
        if stdout:
            try:
                pid = int(stdout.splitlines()[-1].strip())
            except ValueError:
                pid = None

        if pid is None:
            return LocalProcessLaunchResult(
                role=role,
                command=command,
                success=True,
                pid=None,
                log_path=log_path,
                message=f"{role}: {command} (log={log_path})",
            )

        await asyncio.sleep(_LOCAL_BOOT_WAIT_SECONDS)
        is_running = self._is_process_running(pid)
        if is_running:
            return LocalProcessLaunchResult(
                role=role,
                command=command,
                success=True,
                pid=pid,
                log_path=log_path,
                message=f"{role}: {command} (pid={pid}, log={log_path})",
            )

        log_tail = self._read_log_tail(log_path, _LOCAL_LOG_TAIL_LINES)
        error = f"{role} プロセスが直後に終了しました (pid={pid}, log={log_path})"
        if log_tail:
            error = f"{error}\n--- {role} log tail ---\n{self._trim_output(log_tail)}"
        return LocalProcessLaunchResult(
            role=role,
            command=command,
            success=False,
            pid=pid,
            log_path=log_path,
            error=error,
        )

    async def _verify_local_runtime_readiness(
        self,
        *,
        config: AppConfig,
        config_path: Path,
        launches: list[LocalProcessLaunchResult],
    ) -> tuple[bool, list[str], str | None]:
        """local_start 後の実稼働確認（PID + backend health + frontend port）。"""
        notes: list[str] = []
        deadline = time.monotonic() + _LOCAL_READINESS_TIMEOUT_SECONDS
        backend_declared = any(item.role == "backend" for item in launches)
        frontend_declared = any(item.role == "frontend" for item in launches)
        backend_started = any(item.role == "backend" and item.pid is not None for item in launches)
        frontend_started = any(item.role == "frontend" and item.pid is not None for item in launches)
        frontend_port = config.ports.frontend if isinstance(config.ports.frontend, int) else None
        ready_streak = 0
        last_backend_status = "unknown"
        last_frontend_status = "unknown"

        if backend_declared and not backend_started:
            notes.append("local readiness backend check skipped (pid unavailable)")
        if frontend_declared and not frontend_started and frontend_port is None:
            notes.append("local readiness frontend check skipped (pid/port unavailable)")
        if not backend_started and not frontend_started and frontend_port is None:
            return True, notes, None

        while time.monotonic() < deadline:
            for launch in launches:
                if launch.pid is None:
                    continue
                if self._is_process_running(launch.pid):
                    continue
                log_tail = self._read_log_tail(launch.log_path, _LOCAL_LOG_TAIL_LINES)
                message = (
                    f"{launch.role} プロセスが起動後に停止しました "
                    f"(pid={launch.pid}, log={launch.log_path})"
                )
                if log_tail:
                    message = f"{message}\n--- {launch.role} log tail ---\n{self._trim_output(log_tail)}"
                return False, notes, message

            backend_ready = True
            if backend_started:
                health = await self.check_health(config, config_path=config_path)
                backend_ready = health.status == AppStatus.HEALTHY
                last_backend_status = health.status.value

            frontend_ready = True
            if frontend_port is not None and (frontend_started or frontend_declared):
                frontend_ready = self._is_tcp_port_open("127.0.0.1", frontend_port, _LOCAL_DEPENDENCY_TIMEOUT)
                last_frontend_status = "ready" if frontend_ready else "not_ready"

            if backend_ready and frontend_ready:
                ready_streak += 1
                if ready_streak >= _LOCAL_READINESS_STABLE_STREAK:
                    notes.append("local readiness check: backend/frontend ready")
                    return True, notes, None
            else:
                ready_streak = 0

            await asyncio.sleep(_LOCAL_READINESS_POLL_SECONDS)

        timeout_message = (
            "local_start readiness timeout: "
            f"backend={last_backend_status}, frontend={last_frontend_status}"
        )
        return False, notes, timeout_message

    async def _stop_local_processes(self, app_name: str) -> dict[str, Any]:
        """PID ファイルを参照してローカルプロセスを停止する."""
        killed: list[str] = []
        errors: list[str] = []
        not_running: list[str] = []

        for role in ("backend", "frontend"):
            pid_file = f"/tmp/{app_name}_{role}.pid"
            try:
                with open(pid_file) as f:
                    pid = int(f.read().strip())
            except FileNotFoundError:
                not_running.append(f"{role}: PID ファイルなし（起動していない可能性）")
                continue
            except (ValueError, OSError) as exc:
                errors.append(f"{role}: PID ファイル読み取りエラー ({exc})")
                continue

            try:
                os.kill(pid, signal.SIGTERM)
                for _ in range(50):
                    await asyncio.sleep(0.1)
                    try:
                        os.kill(pid, 0)
                    except ProcessLookupError:
                        break
                else:
                    os.kill(pid, signal.SIGKILL)
                killed.append(f"{role}(pid={pid})")
            except ProcessLookupError:
                killed.append(f"{role}(pid={pid}, already stopped)")
            except OSError as exc:
                errors.append(f"{role}: kill 失敗 ({exc})")
            finally:
                with suppress(OSError):
                    Path(pid_file).unlink()

        if not killed and not errors:
            return {"success": True, "killed": not_running, "errors": []}
        return {"success": len(errors) == 0, "killed": killed + not_running, "errors": errors}

    async def _run_process_action(
        self,
        config: AppConfig,
        *,
        app_dir: Path,
        config_path: Path | None,
        action: str,
        run_health_check: bool,
        cli_preflight: dict[str, Any] | None,
    ) -> AppActionResult:
        """Tier 3: docker compose なし時にプロセスモードでアクションを実行する."""
        project_root = config_path.parent.parent.parent if config_path else app_dir

        if action == "stop":
            stop_result = await self._stop_local_processes(config.name)
            success = stop_result["success"]
            stdout = "\n".join(stop_result.get("killed", []))
            error_msg: str | None = "; ".join(stop_result.get("errors", [])) or None

            checked_health: HealthCheckResult | None = None
            if success:
                checked_health = HealthCheckResult(
                    config.name,
                    AppStatus.STOPPED,
                    details={"message": "プロセスを停止しました"},
                )
                self._health_cache[config.name] = checked_health

            diagnostic: dict[str, Any] | None = None
            if not success and error_msg:
                diagnostic = await self._diagnostic_service.diagnose_action_failure(
                    app_config=config,
                    action=action,
                    config_path=config_path,
                    command=["process_mode", "stop"],
                    cwd=str(app_dir),
                    error=error_msg,
                    stdout=stdout,
                    stderr="",
                    preflight=cli_preflight,
                    command_source="process_mode",
                )

            return AppActionResult(
                app_name=config.name,
                action=action,
                success=success,
                command=["process_mode", "stop", config.name],
                command_source="process_mode",
                cwd=str(app_dir),
                stdout=stdout,
                error=error_msg,
                checked_health=checked_health,
                diagnostic=diagnostic,
                execution_mode="local",
            )

        # start / publish: backend_dev + frontend_dev を起動
        resolved_commands = self._resolve_runtime_commands(config, config_path)
        backend_cmd = resolved_commands.get("backend_dev")
        frontend_cmd = resolved_commands.get("frontend_dev")

        if backend_cmd is None and frontend_cmd is None:
            diagnostic = await self._diagnostic_service.diagnose_action_failure(
                app_config=config,
                action=action,
                config_path=config_path,
                command=[],
                cwd=str(project_root),
                error="backend/frontend command is missing",
                stdout="",
                stderr="No backend_dev/frontend_dev command resolved",
                preflight=cli_preflight,
                command_source="process_mode",
            )
            return AppActionResult(
                app_name=config.name,
                action=action,
                success=False,
                command=[],
                cwd=str(project_root),
                error="runtime.commands に backend_dev/frontend_dev が設定されていません",
                command_source="process_mode",
                diagnostic=diagnostic,
                execution_mode="local",
            )

        results: list[str] = []
        errors_list: list[str] = []

        for cmd, role in [(backend_cmd, "backend"), (frontend_cmd, "frontend")]:
            if not cmd:
                continue
            launch = await self._start_local_process(
                app_name=config.name,
                command=cmd,
                role=role,
                cwd=project_root,
            )
            if launch.success and launch.message:
                results.append(launch.message)
            if not launch.success and launch.error:
                errors_list.append(launch.error)

        success = len(errors_list) == 0 and len(results) > 0
        stdout_msg = "\n".join(results)
        error_msg = "\n".join(errors_list) if errors_list else None

        process_checked_health: HealthCheckResult | None = None
        if success and run_health_check:
            process_checked_health = await self.check_health(config, config_path=config_path)

        process_diagnostic: dict[str, Any] | None = None
        if not success:
            process_diagnostic = await self._diagnostic_service.diagnose_action_failure(
                app_config=config,
                action=action,
                config_path=config_path,
                command=["process_mode", backend_cmd or "", frontend_cmd or ""],
                cwd=str(project_root),
                error=error_msg,
                stdout=stdout_msg,
                stderr="",
                preflight=cli_preflight,
                command_source="process_mode",
            )

        return AppActionResult(
            app_name=config.name,
            action=action,
            success=success,
            command=["process_mode", config.name],
            command_source="process_mode",
            cwd=str(project_root),
            stdout=stdout_msg,
            error=error_msg,
            checked_health=process_checked_health,
            diagnostic=process_diagnostic,
            execution_mode="local",
        )

    async def _run_compose_action(
        self,
        config: AppConfig,
        *,
        config_path: Path | None,
        action: str,
        compose_subcommand: list[str],
        run_health_check: bool,
        cli_preflight: dict[str, Any] | None,
        allow_process_fallback: bool = True,
        allow_command_override: bool = True,
    ) -> AppActionResult:
        """docker compose を実行して App 操作を行う."""
        _logger.info(
            "[%s] %s 開始 (config_path=%s)",
            config.name,
            action,
            config_path,
        )

        if config_path is None:
            _logger.warning("[%s] config_path が None — 中断", config.name)
            result = HealthCheckResult(
                config.name,
                AppStatus.UNKNOWN,
                error="app_config.json のパスが取得できません",
            )
            return AppActionResult(
                app_name=config.name,
                action=action,
                success=False,
                command=[],
                cwd="",
                error=result.error,
                command_source="fallback",
                execution_mode="docker",
            )

        app_dir = config_path.parent
        if allow_command_override:
            resolved_commands = self._resolve_runtime_commands(config, config_path)
            action_key = action if action in {"publish", "start", "stop"} else "start"
            command_override = resolved_commands.get(action_key)  # type: ignore[arg-type]
            if command_override is not None:
                shell_result = await self._run_shell_action(
                    config,
                    app_dir=app_dir,
                    config_path=config_path,
                    action=action,
                    shell_command=command_override,
                    run_health_check=run_health_check,
                    cli_preflight=cli_preflight,
                    command_source=resolved_commands.source.get(action_key, "runtime.commands"),
                )
                shell_result.execution_mode = "docker"
                return shell_result

        compose_files = self._resolve_compose_files(app_dir, include_dev=True)
        if not compose_files:
            if allow_process_fallback:
                # Tier 3: docker compose なし → プロセスモードにフォールバック
                _logger.info(
                    "[%s] %s: docker-compose なし → Tier 3 プロセスモードで実行",
                    config.name,
                    action,
                )
                process_result = await self._run_process_action(
                    config,
                    app_dir=app_dir,
                    config_path=config_path,
                    action=action,
                    run_health_check=run_health_check,
                    cli_preflight=cli_preflight,
                )
                process_result.execution_mode = "local"
                return process_result
            return AppActionResult(
                app_name=config.name,
                action=action,
                success=False,
                command=["docker", "compose", *compose_subcommand],
                cwd=str(app_dir),
                error="docker compose ファイルが見つかりません",
                command_source="fallback",
                execution_mode="docker",
            )

        command = ["docker", "compose"]
        for compose_file in compose_files:
            command.extend(["-f", compose_file.name])
        command.extend(compose_subcommand)
        _logger.info("[%s] 実行: %s (cwd=%s)", config.name, command, app_dir)

        try:
            proc = await asyncio.create_subprocess_exec(
                *command,
                cwd=str(app_dir),
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
            )
            stdout_bytes, stderr_bytes = await asyncio.wait_for(
                proc.communicate(),
                timeout=_ACTION_TIMEOUT,
            )
            stdout = stdout_bytes.decode("utf-8", errors="ignore").strip()
            stderr = stderr_bytes.decode("utf-8", errors="ignore").strip()
            success = proc.returncode == 0
            error = None if success else (stderr or stdout or "docker compose 実行に失敗")
            if success:
                _logger.info("[%s] %s 成功 (rc=%s)", config.name, action, proc.returncode)
            else:
                _logger.warning(
                    "[%s] %s 失敗 (rc=%s): %s",
                    config.name,
                    action,
                    proc.returncode,
                    error,
                )
        except FileNotFoundError:
            success = False
            stdout = ""
            stderr = ""
            error = "docker コマンドが見つかりません（docker がインストールされていない可能性）"
            proc = None
            _logger.exception("[%s] %s: %s", config.name, action, error)
        except TimeoutError:
            success = False
            stdout = ""
            stderr = ""
            error = f"docker compose がタイムアウトしました（{_ACTION_TIMEOUT:.0f}s）"
            proc = None
            _logger.exception("[%s] %s: %s", config.name, action, error)
        except Exception as exc:
            success = False
            stdout = ""
            stderr = ""
            error = str(exc)
            proc = None
            _logger.exception("[%s] %s: 予期しないエラー", config.name, action)

        checked_health: HealthCheckResult | None = None
        if success and run_health_check:
            checked_health = await self.check_health(config, config_path=config_path)
        elif success and action == "stop":
            checked_health = HealthCheckResult(
                config.name,
                AppStatus.STOPPED,
                details={"message": "停止コマンドを実行しました"},
            )
            self._health_cache[config.name] = checked_health
        diagnostic: dict[str, Any] | None = None
        if not success:
            diagnostic = await self._diagnostic_service.diagnose_action_failure(
                app_config=config,
                action=action,
                config_path=config_path,
                command=command,
                cwd=str(app_dir),
                error=error,
                stdout=stdout,
                stderr=stderr,
                preflight=cli_preflight,
                command_source="fallback",
            )

        return AppActionResult(
            app_name=config.name,
            action=action,
            success=success,
            command=command,
            command_source="fallback",
            cwd=str(app_dir),
            return_code=None if proc is None else proc.returncode,
            stdout=self._trim_output(stdout),
            stderr=self._trim_output(stderr),
            error=error,
            checked_health=checked_health,
            diagnostic=diagnostic,
            execution_mode="docker",
        )

    async def _run_shell_action(
        self,
        config: AppConfig,
        *,
        app_dir: Path,
        config_path: Path | None,
        action: str,
        shell_command: str,
        run_health_check: bool,
        cli_preflight: dict[str, Any] | None,
        command_source: str,
    ) -> AppActionResult:
        """runtime.commands の shell コマンドを実行."""
        command = ["bash", "-lc", shell_command]
        _logger.info("[%s] 実行: %s (cwd=%s)", config.name, shell_command, app_dir)

        try:
            proc = await asyncio.create_subprocess_exec(
                *command,
                cwd=str(app_dir),
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
            )
            stdout_bytes, stderr_bytes = await asyncio.wait_for(
                proc.communicate(),
                timeout=_ACTION_TIMEOUT,
            )
            stdout = stdout_bytes.decode("utf-8", errors="ignore").strip()
            stderr = stderr_bytes.decode("utf-8", errors="ignore").strip()
            success = proc.returncode == 0
            error = None if success else (stderr or stdout or "コマンド実行に失敗")
            if success:
                _logger.info("[%s] %s 成功 (rc=%s)", config.name, action, proc.returncode)
            else:
                _logger.warning(
                    "[%s] %s 失敗 (rc=%s): %s",
                    config.name,
                    action,
                    proc.returncode,
                    error,
                )
        except FileNotFoundError:
            success = False
            stdout = ""
            stderr = ""
            error = "bash コマンドが見つかりません"
            proc = None
            _logger.exception("[%s] %s: %s", config.name, action, error)
        except TimeoutError:
            success = False
            stdout = ""
            stderr = ""
            error = f"コマンドがタイムアウトしました（{_ACTION_TIMEOUT:.0f}s）"
            proc = None
            _logger.exception("[%s] %s: %s", config.name, action, error)
        except Exception as exc:
            success = False
            stdout = ""
            stderr = ""
            error = str(exc)
            proc = None
            _logger.exception("[%s] %s: 予期しないエラー", config.name, action)

        checked_health: HealthCheckResult | None = None
        if success and run_health_check:
            checked_health = await self.check_health(config, config_path=config_path)
        elif success and action == "stop":
            checked_health = HealthCheckResult(
                config.name,
                AppStatus.STOPPED,
                details={"message": "停止コマンドを実行しました"},
            )
            self._health_cache[config.name] = checked_health
        diagnostic: dict[str, Any] | None = None
        if not success:
            diagnostic = await self._diagnostic_service.diagnose_action_failure(
                app_config=config,
                action=action,
                config_path=config_path,
                command=command,
                cwd=str(app_dir),
                error=error,
                stdout=stdout,
                stderr=stderr,
                preflight=cli_preflight,
                command_source=command_source,
            )

        return AppActionResult(
            app_name=config.name,
            action=action,
            success=success,
            command=command,
            command_source=command_source,
            cwd=str(app_dir),
            return_code=None if proc is None else proc.returncode,
            stdout=self._trim_output(stdout),
            stderr=self._trim_output(stderr),
            error=error,
            checked_health=checked_health,
            diagnostic=diagnostic,
        )

    async def _run_action_with_repair(
        self,
        *,
        config: AppConfig,
        action: str,
        config_path: Path | None,
        cli_preflight: dict[str, Any] | None,
        initial_result: AppActionResult,
        retry_callback: Callable[[], Awaitable[AppActionResult]],
    ) -> AppActionResult:
        """失敗時に AI 修復を実行し、最大 4 回まで再試行する."""
        if initial_result.success:
            return self._attach_repair_result(initial_result, attempts=[], outcome="success_without_repair")
        if not self._is_repair_ready(cli_preflight):
            return self._attach_repair_result(initial_result, attempts=[], outcome="skipped_cli_not_ready")

        attempts: list[dict[str, Any]] = []
        latest_result = initial_result
        for tool in _REPAIR_TOOL_ORDER:
            for attempt_index in range(1, _REPAIR_MAX_ATTEMPTS_PER_TOOL + 1):
                repair_attempt = await self._repair_service.attempt_action_repair(
                    app_config=config,
                    action=action,
                    config_path=config_path,
                    tool=tool,
                    attempt=attempt_index,
                    command=latest_result.command,
                    cwd=latest_result.cwd,
                    error=latest_result.error,
                    stdout=latest_result.stdout,
                    stderr=latest_result.stderr,
                )
                attempts.append(repair_attempt)
                latest_result = await retry_callback()
                if latest_result.success:
                    return self._attach_repair_result(
                        latest_result,
                        attempts=attempts,
                        outcome="success_after_repair",
                    )

        failed = self._attach_repair_result(latest_result, attempts=attempts, outcome="failed")
        if failed.diagnostic is not None:
            failed.diagnostic = self._diagnostic_service.attach_retry_trace(
                failed.diagnostic,
                failed.repair or {},
            )
        elif failed.repair is not None:
            failed.diagnostic = {"retry_trace": failed.repair}
        return failed

    def _attach_repair_result(
        self,
        result: AppActionResult,
        *,
        attempts: list[dict[str, Any]],
        outcome: str,
    ) -> AppActionResult:
        """AppActionResult に repair メタデータを付与する."""
        repair: dict[str, Any] = {
            "enabled": outcome != "disabled",
            "tool_order": list(_REPAIR_TOOL_ORDER),
            "max_attempts_per_tool": _REPAIR_MAX_ATTEMPTS_PER_TOOL,
            "total_attempts": len(attempts),
            "attempts": attempts,
            "outcome": outcome,
        }
        result.repair = repair
        if attempts and result.diagnostic is not None:
            result.diagnostic = self._diagnostic_service.attach_retry_trace(result.diagnostic, repair)
        return result

    @staticmethod
    def _is_repair_ready(cli_preflight: dict[str, Any] | None) -> bool:
        if not isinstance(cli_preflight, dict):
            return False
        if cli_preflight.get("ready") is not True:
            return False
        final = cli_preflight.get("final")
        if not isinstance(final, dict):
            return False
        authenticated = final.get("authenticated_tools")
        return isinstance(authenticated, list) and len(authenticated) > 0

    async def _detect_execution_mode(
        self,
        *,
        app_name: str,
        config_path: Path,
    ) -> tuple[ExecutionMode, dict[str, Any]]:
        """docker/local 実行モードを判定する."""
        docker_info = await self._inspect_docker_runtime(config_path)
        local_info = self._inspect_local_process_runtime(app_name)
        compose_present = bool(self._resolve_compose_files(config_path.parent, include_dev=True))
        docker_running = bool(docker_info.get("available")) and bool(docker_info.get("running_services"))
        local_running = bool(local_info.get("running"))

        if docker_running:
            mode: ExecutionMode = "docker"
            reason = "docker_running"
        elif local_running:
            mode = "local"
            reason = "local_running"
        elif compose_present:
            mode = "docker"
            reason = "compose_first_default"
        else:
            mode = "local"
            reason = "no_compose_local_default"

        return mode, {
            "reason": reason,
            "compose_present": compose_present,
            "docker": docker_info,
            "local": local_info,
        }

    def _inspect_local_process_runtime(self, app_name: str) -> dict[str, Any]:
        """PID ファイルから local 実行状態を判定する."""
        roles: dict[str, dict[str, Any]] = {}
        running_roles: list[str] = []
        for role in ("backend", "frontend"):
            pid_file = Path(f"/tmp/{app_name}_{role}.pid")
            pid_value: int | None = None
            running = False
            if pid_file.is_file():
                try:
                    pid_value = int(pid_file.read_text(encoding="utf-8").strip())
                    running = self._is_process_running(pid_value)
                except Exception:
                    pid_value = None
                    running = False
            if running:
                running_roles.append(role)
            roles[role] = {
                "pid_file": str(pid_file),
                "pid": pid_value,
                "running": running,
            }
        return {
            "running": len(running_roles) > 0,
            "running_roles": running_roles,
            "roles": roles,
        }

    async def _resolve_publish_subcommand(
        self,
        *,
        app_dir: Path,
        compose_files: list[Path],
    ) -> tuple[list[str], str]:
        """publish 用 compose サブコマンドを決定する."""
        state = await asyncio.to_thread(self._inspect_compose_asset_state_sync, app_dir, compose_files)
        has_existing_assets = bool(state.get("containers_exist")) or bool(state.get("images_exist"))
        if has_existing_assets:
            return ["up", "-d"], "lightweight_start"
        return ["up", "-d", "--build"], "build_start"

    def _inspect_compose_asset_state_sync(
        self,
        app_dir: Path,
        compose_files: list[Path],
    ) -> dict[str, Any]:
        """compose の既存コンテナ/イメージ有無を確認する."""
        base_command = ["docker", "compose"]
        for compose_file in compose_files:
            base_command.extend(["-f", compose_file.name])

        containers_exist = False
        images_exist = False

        ps_command = [*base_command, "ps", "-a", "--format", "json"]
        try:
            ps_proc = subprocess.run(
                ps_command,
                cwd=str(app_dir),
                capture_output=True,
                text=True,
                check=False,
                timeout=3,
            )
            if ps_proc.returncode == 0:
                containers_exist = len(self._parse_compose_ps_output(ps_proc.stdout)) > 0
        except Exception:
            containers_exist = False

        images_command = [*base_command, "images", "-q"]
        try:
            img_proc = subprocess.run(
                images_command,
                cwd=str(app_dir),
                capture_output=True,
                text=True,
                check=False,
                timeout=3,
            )
            if img_proc.returncode == 0:
                images_exist = len([line for line in img_proc.stdout.splitlines() if line.strip()]) > 0
        except Exception:
            images_exist = False

        return {
            "containers_exist": containers_exist,
            "images_exist": images_exist,
        }

    def get_cached_health(self, app_name: str) -> HealthCheckResult | None:
        """キャッシュ済みヘルスチェック結果を取得.

        Args:
            app_name: App 識別子

        Returns:
            直近のヘルスチェック結果、なければ None
        """
        return self._health_cache.get(app_name)

    async def check_all(
        self,
        configs: list[AppConfig],
        *,
        config_paths: dict[str, Path] | None = None,
    ) -> list[HealthCheckResult]:
        """全 App のヘルスチェックを実行.

        Args:
            configs: チェック対象の AppConfig リスト

        Returns:
            ヘルスチェック結果リスト
        """
        if not configs:
            return []
        return await asyncio.gather(
            *(
                self.check_health(
                    config,
                    config_path=None if config_paths is None else config_paths.get(config.name),
                )
                for config in configs
            ),
        )

    @staticmethod
    def _resolve_health_paths(configured_health_path: str | None) -> list[str]:
        """ヘルスチェック用パス候補を作成."""
        paths: list[str] = []
        if configured_health_path:
            path = configured_health_path if configured_health_path.startswith("/") else f"/{configured_health_path}"
            paths.append(path)
        for fallback in _HEALTH_FALLBACK_PATHS:
            if fallback not in paths:
                paths.append(fallback)
        return paths

    def _build_candidate_urls(
        self,
        config: AppConfig,
        health_paths: list[str],
        docker_info: dict[str, Any],
    ) -> list[str]:
        """ヘルスチェックURL候補を作成."""
        ports: list[int] = []

        for port in docker_info.get("backend_published_ports", []):
            if isinstance(port, int) and port not in ports:
                ports.append(port)

        if config.ports.api is not None and config.ports.api not in ports:
            ports.append(config.ports.api)

        urls: list[str] = []
        hosts = ("127.0.0.1", "localhost")
        for port in ports:
            for path in health_paths:
                for host in hosts:
                    url = f"http://{host}:{port}{path}"
                    if url not in urls:
                        urls.append(url)
        return urls

    async def _inspect_docker_runtime(self, config_path: Path | None) -> dict[str, Any]:
        """docker compose の稼働状態と公開ポートを取得（非同期）."""
        if config_path is None:
            return {
                "detected": False,
                "available": False,
                "backend_running": False,
                "backend_published_ports": [],
                "running_services": [],
                "services": [],
            }

        app_dir = config_path.parent
        compose_files = self._resolve_compose_files(app_dir, include_dev=True)
        if not compose_files:
            return {
                "detected": False,
                "available": False,
                "backend_running": False,
                "backend_published_ports": [],
                "running_services": [],
                "services": [],
            }

        return await asyncio.to_thread(
            self._inspect_docker_runtime_sync,
            app_dir,
            compose_files,
        )

    def _inspect_docker_runtime_sync(
        self,
        app_dir: Path,
        compose_files: list[Path],
    ) -> dict[str, Any]:
        """docker compose の稼働状態と公開ポートを取得."""
        command = ["docker", "compose"]
        for compose_file in compose_files:
            command.extend(["-f", compose_file.name])
        command.extend(["ps", "--format", "json"])

        try:
            proc = subprocess.run(
                command,
                cwd=str(app_dir),
                capture_output=True,
                text=True,
                check=False,
                timeout=1,
            )
        except FileNotFoundError:
            return {
                "detected": True,
                "available": False,
                "error": "docker command not found",
                "command": " ".join(command),
                "backend_running": False,
                "backend_published_ports": [],
                "running_services": [],
                "services": [],
            }
        except Exception as exc:
            return {
                "detected": True,
                "available": False,
                "error": str(exc),
                "command": " ".join(command),
                "backend_running": False,
                "backend_published_ports": [],
                "running_services": [],
                "services": [],
            }

        if proc.returncode != 0:
            return {
                "detected": True,
                "available": False,
                "error": (proc.stderr or proc.stdout).strip() or f"docker compose ps failed ({proc.returncode})",
                "command": " ".join(command),
                "backend_running": False,
                "backend_published_ports": [],
                "running_services": [],
                "services": [],
            }

        services_raw = self._parse_compose_ps_output(proc.stdout)
        services: list[dict[str, Any]] = []
        for item in services_raw:
            service_name = str(item.get("Service") or item.get("Name") or "")
            state = str(item.get("State") or item.get("Status") or "").lower()
            health = str(item.get("Health") or "").lower()
            running = ("running" in state) and ("exit" not in state)
            published_ports = self._extract_published_ports(item)
            services.append(
                {
                    "service": service_name,
                    "state": state,
                    "health": health,
                    "running": running,
                    "published_ports": published_ports,
                },
            )

        backend_service = self._select_backend_service(services)
        backend_ports = [] if backend_service is None else backend_service.get("published_ports", [])
        running_services = [service.get("service") for service in services if service.get("running") is True]
        return {
            "detected": True,
            "available": True,
            "command": " ".join(command),
            "compose_files": [f.name for f in compose_files],
            "services": services,
            "running_services": running_services,
            "backend_service": None if backend_service is None else backend_service.get("service"),
            "backend_running": False if backend_service is None else bool(backend_service.get("running")),
            "backend_published_ports": backend_ports,
        }

    @staticmethod
    def _parse_compose_ps_output(raw: str) -> list[dict[str, Any]]:
        """`docker compose ps --format json` の出力を配列へ変換."""
        text = raw.strip()
        if not text:
            return []

        try:
            parsed = json.loads(text)
            if isinstance(parsed, list):
                return [p for p in parsed if isinstance(p, dict)]
            if isinstance(parsed, dict):
                return [parsed]
        except json.JSONDecodeError:
            pass

        rows: list[dict[str, Any]] = []
        for line in text.splitlines():
            line = line.strip()
            if not line:
                continue
            try:
                row = json.loads(line)
            except json.JSONDecodeError:
                continue
            if isinstance(row, dict):
                rows.append(row)
        return rows

    @staticmethod
    def _extract_published_ports(item: dict[str, Any]) -> list[int]:
        """docker compose ps の1行から公開ポートを抽出."""
        ports: list[int] = []
        publishers = item.get("Publishers")
        if isinstance(publishers, list):
            for pub in publishers:
                if not isinstance(pub, dict):
                    continue
                published = pub.get("PublishedPort")
                if isinstance(published, int) and published not in ports:
                    ports.append(published)

        ports_text = str(item.get("Ports") or "")
        for match in _PORT_MAPPING_RE.finditer(ports_text):
            published = int(match.group("published"))
            if published not in ports:
                ports.append(published)
        return ports

    @staticmethod
    def _select_backend_service(services: list[dict[str, Any]]) -> dict[str, Any] | None:
        """backend 相当のサービスを選択."""
        for hint in _BACKEND_SERVICE_HINTS:
            for service in services:
                name = str(service.get("service", "")).lower()
                if hint == name:
                    return service

        for service in services:
            name = str(service.get("service", "")).lower()
            if any(hint in name for hint in _BACKEND_SERVICE_HINTS):
                return service

        for service in services:
            if service.get("published_ports"):
                return service
        return services[0] if services else None

    @staticmethod
    def _resolve_compose_files(app_dir: Path, *, include_dev: bool) -> list[Path]:
        """利用可能な compose ファイル一覧を返す."""
        files: list[Path] = []
        for filename in _DOCKER_COMPOSE_FILES:
            path = app_dir / filename
            if path.is_file():
                files.append(path)

        if include_dev:
            for filename in _DOCKER_COMPOSE_DEV_FILES:
                path = app_dir / filename
                if path.is_file():
                    files.append(path)
                    break
        return files

    @staticmethod
    def _resolve_action_command(config: AppConfig, action: str) -> str | None:
        """runtime.commands から action 用コマンドを取得."""
        commands = config.runtime.commands
        value = {
            "backend_dev": commands.backend_dev,
            "frontend_dev": commands.frontend_dev,
            "publish": commands.publish,
            "start": commands.start,
            "stop": commands.stop,
        }.get(action)
        if value is None:
            return None
        stripped = value.strip()
        return stripped if stripped else None

    async def _run_cli_preflight(self, config: AppConfig, *, enabled: bool) -> dict[str, Any] | None:
        """CLI preflight を実行する（失敗時も例外を投げない）."""
        if not enabled:
            return None
        try:
            runtime_cli = config.runtime.cli.model_dump()
            return await self._cli_runtime.preflight(runtime_cli)
        except Exception as exc:  # pragma: no cover - defensive fallback
            return {
                "ready": False,
                "error": str(exc),
                "final": {"available_tools": [], "authenticated_tools": []},
            }

    def _resolve_runtime_commands(
        self,
        config: AppConfig,
        config_path: Path | None,
    ) -> ResolvedRuntimeCommands:
        """README 優先で runtime command を解決."""
        if config_path is None:
            return ResolvedRuntimeCommands(
                backend_dev=self._resolve_action_command(config, "backend_dev"),
                frontend_dev=self._resolve_action_command(config, "frontend_dev"),
                publish=self._resolve_action_command(config, "publish"),
                start=self._resolve_action_command(config, "start"),
                stop=self._resolve_action_command(config, "stop"),
                source={
                    "backend_dev": "runtime.commands",
                    "frontend_dev": "runtime.commands",
                    "publish": "runtime.commands",
                    "start": "runtime.commands",
                    "stop": "runtime.commands",
                },
            )
        return self._command_resolver.resolve(
            app_name=config.name,
            app_dir=config_path.parent,
            runtime_commands=config.runtime.commands,
        )

    @staticmethod
    def _merge_command_source(primary: str, secondary: str) -> str:
        if primary == secondary:
            return primary
        return f"{primary}+{secondary}"

    @staticmethod
    def _is_tcp_port_open(host: str, port: int, timeout: float) -> bool:
        """TCP ポート待受を確認."""
        try:
            with socket.create_connection((host, port), timeout=timeout):
                return True
        except OSError:
            return False

    @staticmethod
    def _is_process_running(pid: int) -> bool:
        """PID のプロセス生存確認."""
        try:
            os.kill(pid, 0)
            return True
        except OSError:
            return False

    @staticmethod
    def _read_log_tail(path: str, max_lines: int) -> str:
        """ログ末尾を取得."""
        try:
            with open(path, encoding="utf-8", errors="ignore") as file:
                lines = file.readlines()
        except OSError:
            return ""
        return "".join(lines[-max_lines:]).strip()

    @staticmethod
    def _extract_json_payload(response: httpx.Response) -> dict[str, Any] | list[Any] | None:
        """JSON payload を安全に抽出."""
        content_type = response.headers.get("content-type", "").lower()
        if "application/json" not in content_type:
            return None
        try:
            payload = response.json()
        except Exception:
            return None
        if isinstance(payload, (dict, list)):
            return payload
        return None

    @staticmethod
    def _extract_payload_state(payload: dict[str, Any] | list[Any] | None) -> str | None:
        """payload の状態値（status/state/health）を抽出."""
        if not isinstance(payload, dict):
            return None
        for key in ("status", "state", "health"):
            value = payload.get(key)
            if isinstance(value, str) and value.strip():
                return value.strip().lower()
        return None

    @staticmethod
    def _trim_output(text: str, *, max_lines: int = 80, max_chars: int = 8000) -> str:
        """長すぎる標準出力を短く整形."""
        if not text:
            return ""
        lines = text.splitlines()
        if len(lines) > max_lines:
            lines = lines[-max_lines:]
        joined = "\n".join(lines)
        if len(joined) > max_chars:
            return joined[-max_chars:]
        return joined
