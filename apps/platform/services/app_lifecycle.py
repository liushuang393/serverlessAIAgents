# -*- coding: utf-8 -*-
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
import re
import shlex
import subprocess
import time
from datetime import UTC, datetime
from enum import Enum
from pathlib import Path
from typing import Any

import httpx

from apps.platform.schemas.app_config_schemas import AppConfig


_logger = logging.getLogger(__name__)

# ヘルスチェックのタイムアウト（秒）
_HEALTH_TIMEOUT = 2.0
_ACTION_TIMEOUT = 300.0
_HEALTH_FALLBACK_PATHS = ("/api/health", "/health", "/healthz")
_HEALTHY_PAYLOAD_STATES = {"ok", "healthy", "up", "ready", "pass"}
_NON_HEALTHY_PAYLOAD_STATES = {"error", "failed", "fail", "down", "degraded", "starting", "initializing"}
_DOCKER_COMPOSE_FILES = ("docker-compose.yml", "compose.yml")
_DOCKER_COMPOSE_DEV_FILES = ("docker-compose.dev.yml", "compose.override.yml")
_BACKEND_SERVICE_HINTS = ("backend", "api", "server")
_PORT_MAPPING_RE = re.compile(r"(?P<published>\d+)->(?P<target>\d+)")


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


class AppActionResult:
    """App 操作（publish/start/stop）結果."""

    __slots__ = (
        "action",
        "app_name",
        "checked_health",
        "command",
        "cwd",
        "error",
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
        return_code: int | None = None,
        stdout: str = "",
        stderr: str = "",
        error: str | None = None,
        checked_health: HealthCheckResult | None = None,
    ) -> None:
        self.app_name = app_name
        self.action = action
        self.success = success
        self.command = command
        self.cwd = cwd
        self.return_code = return_code
        self.stdout = stdout
        self.stderr = stderr
        self.error = error
        self.checked_health = checked_health

    def to_dict(self) -> dict[str, Any]:
        """辞書へ変換."""
        data: dict[str, Any] = {
            "app_name": self.app_name,
            "action": self.action,
            "success": self.success,
            "command": " ".join(shlex.quote(part) for part in self.command),
            "cwd": self.cwd,
            "return_code": self.return_code,
        }
        if self.stdout:
            data["stdout"] = self.stdout
        if self.stderr:
            data["stderr"] = self.stderr
        if self.error:
            data["error"] = self.error
        if self.checked_health is not None:
            data["health"] = self.checked_health.to_dict()
        return data


class AppLifecycleManager:
    """App ライフサイクル管理サービス.

    ヘルスチェック、起動/停止操作を提供。
    """

    def __init__(self) -> None:
        """初期化."""
        self._health_cache: dict[str, HealthCheckResult] = {}

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
        docker_info = self._inspect_docker_runtime(config_path)
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
                        payload_marked_bad = (
                            payload_state is not None
                            and payload_state in _NON_HEALTHY_PAYLOAD_STATES
                        )
                        payload_marked_good = (
                            payload_state is not None
                            and payload_state in _HEALTHY_PAYLOAD_STATES
                        )

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
                    except Exception as exc:  # noqa: BLE001
                        last_error = str(exc)
                        attempts.append({"url": url, "error": "exception", "message": str(exc)})
        except Exception as exc:  # noqa: BLE001
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
    ) -> AppActionResult:
        """App を publish（docker compose up -d --build）する."""
        return await self._run_compose_action(
            config,
            config_path=config_path,
            action="publish",
            compose_subcommand=["up", "-d", "--build"],
            run_health_check=True,
        )

    async def start_app(
        self,
        config: AppConfig,
        *,
        config_path: Path | None = None,
    ) -> AppActionResult:
        """App を起動（docker compose up -d）する."""
        return await self._run_compose_action(
            config,
            config_path=config_path,
            action="start",
            compose_subcommand=["up", "-d"],
            run_health_check=True,
        )

    async def stop_app(
        self,
        config: AppConfig,
        *,
        config_path: Path | None = None,
    ) -> AppActionResult:
        """App を停止（docker compose down）する."""
        return await self._run_compose_action(
            config,
            config_path=config_path,
            action="stop",
            compose_subcommand=["down"],
            run_health_check=False,
        )

    async def _run_compose_action(
        self,
        config: AppConfig,
        *,
        config_path: Path | None,
        action: str,
        compose_subcommand: list[str],
        run_health_check: bool,
    ) -> AppActionResult:
        """docker compose を実行して App 操作を行う."""
        _logger.info(
            "[%s] %s 開始 (config_path=%s)",
            config.name, action, config_path,
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
            )

        app_dir = config_path.parent
        compose_files = self._resolve_compose_files(app_dir, include_dev=True)
        if not compose_files:
            _logger.warning(
                "[%s] docker-compose ファイルが見つかりません: dir=%s",
                config.name, app_dir,
            )
            return AppActionResult(
                app_name=config.name,
                action=action,
                success=False,
                command=[],
                cwd=str(app_dir),
                error=f"docker-compose.yml が見つかりません (dir={app_dir})",
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
                    config.name, action, proc.returncode, error,
                )
        except FileNotFoundError:
            success = False
            stdout = ""
            stderr = ""
            error = "docker コマンドが見つかりません（docker がインストールされていない可能性）"
            proc = None
            _logger.error("[%s] %s: %s", config.name, action, error)
        except TimeoutError:
            success = False
            stdout = ""
            stderr = ""
            error = f"docker compose がタイムアウトしました（{_ACTION_TIMEOUT:.0f}s）"
            proc = None
            _logger.error("[%s] %s: %s", config.name, action, error)
        except Exception as exc:  # noqa: BLE001
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

        return AppActionResult(
            app_name=config.name,
            action=action,
            success=success,
            command=command,
            cwd=str(app_dir),
            return_code=None if proc is None else proc.returncode,
            stdout=self._trim_output(stdout),
            stderr=self._trim_output(stderr),
            error=error,
            checked_health=checked_health,
        )

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
        results: list[HealthCheckResult] = []
        for config in configs:
            path = None if config_paths is None else config_paths.get(config.name)
            result = await self.check_health(config, config_path=path)
            results.append(result)
        return results

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

    def _inspect_docker_runtime(self, config_path: Path | None) -> dict[str, Any]:
        """docker compose の稼働状態と公開ポートを取得."""
        if config_path is None:
            return {
                "detected": False,
                "available": False,
                "backend_running": False,
                "backend_published_ports": [],
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
                "services": [],
            }

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
                "services": [],
            }
        except Exception as exc:  # noqa: BLE001
            return {
                "detected": True,
                "available": False,
                "error": str(exc),
                "command": " ".join(command),
                "backend_running": False,
                "backend_published_ports": [],
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
        return {
            "detected": True,
            "available": True,
            "command": " ".join(command),
            "compose_files": [f.name for f in compose_files],
            "services": services,
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
    def _extract_json_payload(response: httpx.Response) -> dict[str, Any] | list[Any] | None:
        """JSON payload を安全に抽出."""
        content_type = response.headers.get("content-type", "").lower()
        if "application/json" not in content_type:
            return None
        try:
            payload = response.json()
        except Exception:  # noqa: BLE001
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
