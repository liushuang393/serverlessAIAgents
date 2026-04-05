"""LLM Management の preflight / 配備セットアップ管理."""

from __future__ import annotations

import asyncio
from datetime import UTC, datetime
from pathlib import Path
from typing import Literal, Protocol

import httpx
import yaml

from control_plane.schemas.llm_management_schemas import (
    LLMBackendKind,
    LLMPreflightReport,
    LLMPreflightRequest,
    LLMPreflightStep,
    LLMProviderKind,
    LLMSetupCommandResult,
)
from control_plane.services.llm_management_validator import (
    LLMConfigValidator,
    provider_default_api_key_env,
)
from infrastructure.llm.gateway import InferenceEngineConfig, LLMGatewayConfig, resolve_secret


_DANGEROUS_TOKENS: set[str] = {
    "--dangerously-bypass-approvals-and-sandbox",
    "--dangerously-skip-permissions",
    "--allow-dangerously-skip-permissions",
}

_ALLOWED_EXECUTABLES: set[str] = {
    "python",
    "python3",
    "pip",
    "docker",
    "ollama",
    "curl",
}


class LLMCommandRunner(Protocol):
    """Protocol for command runner."""

    async def run(
        self,
        command: list[str],
        *,
        cwd: str | None = None,
        timeout_seconds: float = 120.0,
    ) -> LLMSetupCommandResult:
        """Execute command and return structured result."""


class DefaultLLMCommandRunner:
    """Async subprocess based command runner."""

    async def run(
        self,
        command: list[str],
        *,
        cwd: str | None = None,
        timeout_seconds: float = 120.0,
    ) -> LLMSetupCommandResult:
        try:
            proc = await asyncio.create_subprocess_exec(
                *command,
                cwd=cwd,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
            )
        except FileNotFoundError as exc:
            return LLMSetupCommandResult(
                command=command,
                cwd=cwd,
                allowed=True,
                error=str(exc),
            )

        try:
            stdout_b, stderr_b = await asyncio.wait_for(proc.communicate(), timeout=timeout_seconds)
        except TimeoutError:
            proc.kill()
            await asyncio.gather(proc.wait(), return_exceptions=True)
            return LLMSetupCommandResult(
                command=command,
                cwd=cwd,
                timed_out=True,
                allowed=True,
                error="timeout",
            )

        return LLMSetupCommandResult(
            command=command,
            cwd=cwd,
            return_code=proc.returncode,
            stdout=stdout_b.decode("utf-8", errors="ignore").strip(),
            stderr=stderr_b.decode("utf-8", errors="ignore").strip(),
            allowed=True,
        )


class LLMSetupManager:
    """Perform detect/install/start/health preflight with command allowlist."""

    def __init__(
        self,
        *,
        config_path: Path,
        command_runner: LLMCommandRunner | None = None,
    ) -> None:
        self._config_path = config_path
        self._runner = command_runner or DefaultLLMCommandRunner()
        self._validator = LLMConfigValidator()

    async def deploy_engine(
        self,
        engine: InferenceEngineConfig,
    ) -> tuple[LLMSetupCommandResult, Path, str]:
        """Engine 設定から compose を生成して起動する."""
        if engine.deployment_mode == "native":
            return await self._deploy_engine_native(engine)

        compose_path, compose_yaml = self._ensure_engine_compose(engine)
        command = ["docker", "compose", "-f", str(compose_path), "up", "-d"]
        result = await self._run_command(command, dry_run=False, cwd=str(compose_path.parent), timeout_seconds=900.0)
        if result.error is not None or result.return_code != 0:
            return result, compose_path, compose_yaml

        health_result = await self._wait_for_engine_health(engine)
        if health_result is not None:
            return health_result, compose_path, compose_yaml
        return result, compose_path, compose_yaml

    async def _deploy_engine_native(
        self,
        engine: InferenceEngineConfig,
    ) -> tuple[LLMSetupCommandResult, Path, str]:
        """native モードで engine を起動する（Ollama 等）."""
        marker_dir = self._config_path.parent / "llm_backends" / engine.name
        marker_dir.mkdir(parents=True, exist_ok=True)
        marker_path = marker_dir / "native.marker"
        marker_path.write_text(f"deployment_mode=native\nengine={engine.name}\n", encoding="utf-8")

        command = ["ollama", "serve"] if engine.engine_type == "ollama" else []
        if not command:
            return (
                LLMSetupCommandResult(
                    command=command, allowed=True,
                    error=f"native デプロイは engine_type={engine.engine_type} に未対応です。",
                ),
                marker_path,
                "",
            )

        result = await self._run_command(command, dry_run=False, cwd=None, timeout_seconds=5.0)
        # ollama serve はブロッキングなのでタイムアウトしても正常
        # ヘルスチェックで起動確認する

        health_result = await self._wait_for_engine_health(engine, timeout_seconds=30.0, interval_seconds=2.0)
        if health_result is not None:
            # ollama serve が既に起動中の場合はヘルスチェックで確認
            return health_result, marker_path, ""
        return (
            LLMSetupCommandResult(
                command=command, allowed=True, return_code=0,
                stdout="native engine started successfully",
            ),
            marker_path,
            "",
        )

    async def stop_engine(self, engine_name: str) -> LLMSetupCommandResult:
        """既存 compose または native プロセスから engine を停止する."""
        # native モードの場合はマーカーファイルで判定
        native_marker = self._config_path.parent / "llm_backends" / engine_name / "native.marker"
        if native_marker.is_file():
            return LLMSetupCommandResult(
                command=["native", "stop", engine_name],
                allowed=True,
                return_code=0,
                stdout="native engine は手動停止してください（例: systemctl stop ollama）。",
            )

        compose_path = self._config_path.parent / "llm_backends" / engine_name / "docker-compose.yml"
        if not compose_path.is_file():
            return LLMSetupCommandResult(
                command=["docker", "compose", "down"],
                cwd=str(compose_path.parent),
                allowed=True,
                error="compose_file_missing",
            )
        command = ["docker", "compose", "-f", str(compose_path), "down"]
        return await self._run_command(command, dry_run=False, cwd=str(compose_path.parent), timeout_seconds=900.0)

    async def preflight(
        self,
        request: LLMPreflightRequest,
        config: LLMGatewayConfig,
    ) -> LLMPreflightReport:
        """Run preflight and return detailed report."""
        started_at = datetime.now(UTC).isoformat()
        steps: list[LLMPreflightStep] = []

        provider_targets = request.providers or []
        backend_targets = [item for item in request.backends if item != LLMBackendKind.NONE]

        provider_steps = self._run_provider_checks(
            config=config,
            provider_targets=provider_targets,
            dry_run=request.dry_run,
        )
        steps.extend(provider_steps)

        for backend in backend_targets:
            steps.extend(await self._run_backend_checks(config=config, backend=backend, request=request))

        unresolved_failures = [step for step in steps if step.status == "failed"]
        filtered_failures: list[LLMPreflightStep] = []
        for failed_step in unresolved_failures:
            if failed_step.category == "backend" and failed_step.phase == "detect":
                install_step = next(
                    (
                        step
                        for step in steps
                        if step.category == "backend" and step.target == failed_step.target and step.phase == "install"
                    ),
                    None,
                )
                if install_step is not None and install_step.status in {"success", "dry_run"}:
                    continue
            filtered_failures.append(failed_step)

        statuses = {step.status for step in steps}
        status: Literal["success", "failed", "partial", "dry_run"]
        if filtered_failures:
            status = "failed"
        elif "dry_run" in statuses:
            status = "dry_run"
        elif statuses.issubset({"success", "skipped"}) and "skipped" in statuses:
            status = "partial"
        else:
            status = "success"

        summary = self._summary_from_steps(steps, status)
        return LLMPreflightReport(
            status=status,
            started_at=started_at,
            completed_at=datetime.now(UTC).isoformat(),
            request=request,
            steps=steps,
            summary=summary,
        )

    def _run_provider_checks(
        self,
        *,
        config: LLMGatewayConfig,
        provider_targets: list[LLMProviderKind],
        dry_run: bool,
    ) -> list[LLMPreflightStep]:
        by_name = {self._validator.canonical_provider_name(provider.name): provider for provider in config.providers}
        steps: list[LLMPreflightStep] = []
        for provider_kind in provider_targets:
            name = self._validator.canonical_provider_name(provider_kind.value)
            provider = by_name.get(name)
            env_name = provider.api_key_env if provider is not None else provider_default_api_key_env(name)

            if name in {"local", "custom"} and not env_name:
                steps.append(
                    LLMPreflightStep(
                        category="provider",
                        target=name,
                        phase="detect",
                        status="success" if not dry_run else "dry_run",
                        message=f"provider {name} は API Key 不要です。",
                    )
                )
                continue

            value, source = resolve_secret(
                env_name,
                provider_name=name,
                config_path=self._config_path,
            )
            if value:
                steps.append(
                    LLMPreflightStep(
                        category="provider",
                        target=name,
                        phase="detect",
                        status="success",
                        message=f"API Key を {source} から解決しました。",
                    )
                )
            else:
                remediation = [f"{name} へ切り替える前に、{env_name} を環境変数または .env に設定してください。"]
                if name == "google":
                    remediation.append("google / gemini では GEMINI_API_KEY を正規の変数名として使用します。")
                steps.append(
                    LLMPreflightStep(
                        category="provider",
                        target=name,
                        phase="detect",
                        status="failed",
                        message="API Key が未設定です。",
                        remediation=remediation,
                    )
                )
        return steps

    async def _run_backend_checks(
        self,
        *,
        config: LLMGatewayConfig,
        backend: LLMBackendKind,
        request: LLMPreflightRequest,
    ) -> list[LLMPreflightStep]:
        steps: list[LLMPreflightStep] = []
        backend_name = backend.value

        detect_command = self._backend_detect_command(backend)
        detect_result = await self._run_command(detect_command, dry_run=request.dry_run, cwd=None)
        detect_success = bool(detect_result.return_code == 0 and detect_result.error is None)
        steps.append(
            LLMPreflightStep(
                category="backend",
                target=backend_name,
                phase="detect",
                status="dry_run" if request.dry_run else ("success" if detect_success else "failed"),
                message="backend 依存関係を検出しました。" if detect_success else "backend 依存関係が不足しています。",
                command=detect_result,
                remediation=[] if detect_success else [f"{backend_name} の実行依存関係をインストールしてください。"],
            )
        )

        if not detect_success:
            install_status: Literal["success", "failed", "skipped", "dry_run"] = "skipped"
            install_message = "自動インストールは無効です。"
            install_command_result: LLMSetupCommandResult | None = None
            if request.auto_install:
                install_command = self._backend_install_command(backend)
                install_command_result = await self._run_command(
                    install_command,
                    dry_run=request.dry_run,
                    cwd=None,
                    timeout_seconds=900.0,
                )
                install_ok = bool(install_command_result.error is None and install_command_result.return_code == 0)
                install_status = "dry_run" if request.dry_run else ("success" if install_ok else "failed")
                install_message = (
                    "backend 依存関係のインストールに成功しました。"
                    if install_ok
                    else "backend 依存関係のインストールに失敗しました。"
                )
            steps.append(
                LLMPreflightStep(
                    category="backend",
                    target=backend_name,
                    phase="install",
                    status=install_status,
                    message=install_message,
                    command=install_command_result,
                    remediation=[
                        "インターネット接続とパッケージミラー設定を確認してください。",
                        f"{backend_name} のインストールコマンドを手動実行してから preflight を再試行してください。",
                    ]
                    if install_status == "failed"
                    else [],
                )
            )

        if request.auto_start:
            engine_config = self._resolve_backend_engine_config(config, backend)
            if engine_config is not None:
                compose_path, _compose_yaml = self._ensure_engine_compose(engine_config)
            else:
                compose_path = self._ensure_backend_compose(backend)
            start_command = ["docker", "compose", "-f", str(compose_path), "up", "-d"]
            start_result = await self._run_command(start_command, dry_run=request.dry_run, cwd=str(compose_path.parent))
            start_ok = bool(start_result.error is None and start_result.return_code == 0)
            steps.append(
                LLMPreflightStep(
                    category="backend",
                    target=backend_name,
                    phase="start",
                    status="dry_run" if request.dry_run else ("success" if start_ok else "failed"),
                    message="backend 起動コマンドを実行しました。"
                    if start_ok
                    else "backend 起動コマンドに失敗しました。",
                    command=start_result,
                    remediation=[
                        "Docker Engine が起動しており、アクセス可能か確認してください。",
                        f"{compose_path.parent} 配下の compose ログを確認してください。",
                    ]
                    if not start_ok and not request.dry_run
                    else [],
                )
            )
        else:
            steps.append(
                LLMPreflightStep(
                    category="backend",
                    target=backend_name,
                    phase="start",
                    status="skipped",
                    message="自動起動は無効です。",
                )
            )

        if request.health_check:
            url = self._backend_health_url(config, backend)
            health_step = await self._run_health_check(url=url, backend_name=backend_name, dry_run=request.dry_run)
            steps.append(health_step)
        else:
            steps.append(
                LLMPreflightStep(
                    category="backend",
                    target=backend_name,
                    phase="health",
                    status="skipped",
                    message="ヘルスチェックは無効です。",
                )
            )
        return steps

    async def _run_health_check(
        self,
        *,
        url: str,
        backend_name: str,
        dry_run: bool,
    ) -> LLMPreflightStep:
        if dry_run:
            return LLMPreflightStep(
                category="backend",
                target=backend_name,
                phase="health",
                status="dry_run",
                message=f"ヘルスチェックを予定しています: GET {url}",
            )
        try:
            async with httpx.AsyncClient(timeout=10.0) as client:
                response = await client.get(url)
            if response.status_code < 400:
                return LLMPreflightStep(
                    category="backend",
                    target=backend_name,
                    phase="health",
                    status="success",
                    message=f"ヘルスチェック成功: GET {url}",
                )
            return LLMPreflightStep(
                category="backend",
                target=backend_name,
                phase="health",
                status="failed",
                message=f"ヘルスチェック失敗: status={response.status_code}",
            )
        except Exception as exc:
            return LLMPreflightStep(
                category="backend",
                target=backend_name,
                phase="health",
                status="failed",
                message=f"ヘルスチェック失敗: {exc}",
            )

    async def _wait_for_engine_health(
        self,
        engine: InferenceEngineConfig,
        *,
        timeout_seconds: float = 600.0,
        interval_seconds: float = 5.0,
    ) -> LLMSetupCommandResult | None:
        """配備後に engine の health が通るまで待機する."""
        url = f"{engine.base_url.rstrip('/')}/{engine.health_path.lstrip('/')}"
        deadline = asyncio.get_running_loop().time() + timeout_seconds
        attempts = 0
        last_error: str | None = None

        while asyncio.get_running_loop().time() < deadline:
            attempts += 1
            try:
                async with httpx.AsyncClient(timeout=10.0) as client:
                    response = await client.get(url)
                if response.status_code < 400:
                    return None
                last_error = f"health status={response.status_code}"
            except Exception as exc:
                last_error = str(exc)
            await asyncio.sleep(interval_seconds)

        return LLMSetupCommandResult(
            command=["GET", url],
            cwd=None,
            return_code=1,
            allowed=True,
            timed_out=True,
            error=(f"health_check_timeout after {attempts} attempts: {last_error or 'engine is not ready'}"),
        )

    async def _run_command(
        self,
        command: list[str],
        *,
        dry_run: bool,
        cwd: str | None,
        timeout_seconds: float = 120.0,
    ) -> LLMSetupCommandResult:
        if not self._is_command_allowed(command):
            return LLMSetupCommandResult(
                command=command,
                cwd=cwd,
                allowed=False,
                error="blocked_by_allowlist",
            )
        if dry_run:
            return LLMSetupCommandResult(
                command=command,
                cwd=cwd,
                allowed=True,
                return_code=0,
                stdout="dry_run",
            )
        return await self._runner.run(command, cwd=cwd, timeout_seconds=timeout_seconds)

    @staticmethod
    def _backend_detect_command(backend: LLMBackendKind) -> list[str]:
        if backend == LLMBackendKind.VLLM:
            return ["python", "-m", "pip", "show", "vllm"]
        if backend == LLMBackendKind.SGLANG:
            return ["python", "-m", "pip", "show", "sglang"]
        return ["docker", "--version"]

    @staticmethod
    def _backend_install_command(backend: LLMBackendKind) -> list[str]:
        if backend == LLMBackendKind.VLLM:
            return ["python", "-m", "pip", "install", "vllm"]
        if backend == LLMBackendKind.SGLANG:
            return ["python", "-m", "pip", "install", "sglang"]
        return ["docker", "pull", "ghcr.io/huggingface/text-generation-inference:3.3.7"]

    def _backend_health_url(self, config: LLMGatewayConfig, backend: LLMBackendKind) -> str:
        defaults = {
            LLMBackendKind.VLLM: ("http://127.0.0.1:18001", "/health"),
            LLMBackendKind.SGLANG: ("http://127.0.0.1:18002", "/health"),
            LLMBackendKind.TGI: ("http://127.0.0.1:18003", "/health"),
        }
        base_url, health_path = defaults[backend]
        for engine in config.inference_engines:
            normalized = engine.name.strip().lower()
            if normalized == backend.value:
                base_url = engine.base_url
                health_path = engine.health_path
                break
        return f"{base_url.rstrip('/')}/{health_path.lstrip('/')}"

    @staticmethod
    def _resolve_backend_engine_config(
        config: LLMGatewayConfig,
        backend: LLMBackendKind,
    ) -> InferenceEngineConfig | None:
        for engine in config.inference_engines:
            if engine.name.strip().lower() == backend.value:
                return engine
        return None

    def _ensure_backend_compose(self, backend: LLMBackendKind) -> Path:
        target_dir = self._config_path.parent / "llm_backends"
        target_dir.mkdir(parents=True, exist_ok=True)
        target = target_dir / f"{backend.value}.compose.yml"
        if target.exists():
            return target

        compose = self._compose_template(backend)
        target.write_text(compose, encoding="utf-8")
        return target

    def _ensure_engine_compose(self, engine: InferenceEngineConfig) -> tuple[Path, str]:
        target_dir = self._config_path.parent / "llm_backends" / engine.name
        target_dir.mkdir(parents=True, exist_ok=True)
        target = target_dir / "docker-compose.yml"
        compose_payload = self._compose_payload_for_engine(engine)
        compose_yaml = yaml.safe_dump(
            compose_payload,
            sort_keys=False,
            allow_unicode=True,
        )
        target.write_text(compose_yaml, encoding="utf-8")
        return target, compose_yaml

    def _compose_payload_for_engine(self, engine: InferenceEngineConfig) -> dict[str, object]:
        container_port = self._container_port_for_engine(engine.engine_type)
        host_port = engine.host_port or self._host_port_from_base_url(engine.base_url) or container_port
        service_name = f"llm-{engine.name}"
        served_model_name = engine.served_model_name or "Qwen/Qwen2.5-0.5B-Instruct"
        cache_host_dir = self._huggingface_cache_dir()

        service: dict[str, object] = {
            "image": engine.docker_image or self._default_docker_image(engine.engine_type),
            "container_name": engine.container_name or service_name,
            "ports": [f"{host_port}:{container_port}"],
            "restart": "unless-stopped",
        }

        environment = dict(engine.extra_env)
        volumes: list[str] = []
        if engine.gpu_enabled:
            service["gpus"] = "all"
            if engine.gpu_devices:
                environment["CUDA_VISIBLE_DEVICES"] = ",".join(engine.gpu_devices)

        if engine.engine_type in {"vllm", "sglang"}:
            cache_target = "/root/.cache/huggingface"
            environment.setdefault("HF_HOME", cache_target)
            volumes.append(f"{cache_host_dir}:{cache_target}")
            service["ipc"] = "host"
        else:
            cache_target = "/data"
            environment.setdefault("HF_HOME", cache_target)
            volumes.append(f"{cache_host_dir}:{cache_target}")
            service["shm_size"] = "1g"

        if engine.engine_type == "sglang":
            service["shm_size"] = "16g"

        if environment:
            service["environment"] = environment
        if volumes:
            service["volumes"] = volumes

        if engine.engine_type == "vllm":
            command: list[str] = [
                "--model",
                served_model_name,
                "--host",
                "0.0.0.0",
                "--port",
                str(container_port),
            ]
            if engine.gpu_count is not None and engine.gpu_count > 0:
                command.extend(["--tensor-parallel-size", str(engine.gpu_count)])
            service["command"] = command
        elif engine.engine_type == "sglang":
            service["command"] = [
                "python3",
                "-m",
                "sglang.launch_server",
                "--model-path",
                served_model_name,
                "--host",
                "0.0.0.0",
                "--port",
                str(container_port),
            ]
        else:
            service["command"] = [
                "--model-id",
                served_model_name,
                "--port",
                str(container_port),
            ]

        return {"services": {service_name: service}}

    @staticmethod
    def _default_docker_image(engine_type: str) -> str:
        if engine_type == "vllm":
            return "vllm/vllm-openai:v0.8.5"
        if engine_type == "sglang":
            return "lmsysorg/sglang:v0.4.9.post4-cu126"
        return "ghcr.io/huggingface/text-generation-inference:3.3.7"

    @staticmethod
    def _huggingface_cache_dir() -> str:
        cache_dir = Path.home() / ".cache" / "huggingface"
        cache_dir.mkdir(parents=True, exist_ok=True)
        return str(cache_dir)

    @staticmethod
    def _container_port_for_engine(engine_type: str) -> int:
        if engine_type == "vllm":
            return 8000
        if engine_type == "sglang":
            return 30000
        return 80

    @staticmethod
    def _host_port_from_base_url(base_url: str) -> int | None:
        try:
            port = int(base_url.rsplit(":", 1)[1].strip())
        except (IndexError, ValueError):
            return None
        return port

    @staticmethod
    def _compose_template(backend: LLMBackendKind) -> str:
        if backend == LLMBackendKind.VLLM:
            return """services:
  llm-vllm:
    image: vllm/vllm-openai:v0.8.5
    container_name: llm-vllm
    ports:
      - "18001:8000"
    ipc: host
    environment:
      HF_HOME: /root/.cache/huggingface
    volumes:
      - "~/.cache/huggingface:/root/.cache/huggingface"
    command: ["--model", "Qwen/Qwen2.5-0.5B-Instruct"]
"""
        if backend == LLMBackendKind.SGLANG:
            return """services:
  llm-sglang:
    image: lmsysorg/sglang:v0.4.9.post4-cu126
    container_name: llm-sglang
    ports:
      - "18002:30000"
    ipc: host
    shm_size: "16g"
    environment:
      HF_HOME: /root/.cache/huggingface
    volumes:
      - "~/.cache/huggingface:/root/.cache/huggingface"
"""
        return """services:
  llm-tgi:
    image: ghcr.io/huggingface/text-generation-inference:3.3.7
    container_name: llm-tgi
    ports:
      - "18003:80"
    shm_size: "1g"
    environment:
      HF_HOME: /data
    volumes:
      - "~/.cache/huggingface:/data"
"""

    @staticmethod
    def _is_command_allowed(command: list[str]) -> bool:
        if not command:
            return False
        if any(token in _DANGEROUS_TOKENS for token in command):
            return False

        binary = Path(command[0]).name
        if binary not in _ALLOWED_EXECUTABLES:
            return False

        if binary in {"python", "python3"}:
            if len(command) < 4 or command[1] != "-m" or command[2] != "pip":
                return False
            return command[3] in {"show", "install"}

        if binary == "pip":
            return len(command) >= 2 and command[1] in {"show", "install"}

        if binary == "docker":
            if len(command) < 2:
                return False
            return command[1] in {"--version", "compose", "pull"}

        if binary == "ollama":
            return len(command) >= 2 and command[1] in {"--version", "list", "pull", "serve"}

        return binary == "curl"

    @staticmethod
    def _summary_from_steps(steps: list[LLMPreflightStep], status: str) -> str:
        failed = sum(1 for step in steps if step.status == "failed")
        succeeded = sum(1 for step in steps if step.status == "success")
        skipped = sum(1 for step in steps if step.status == "skipped")
        dry_run = sum(1 for step in steps if step.status == "dry_run")
        return f"状態={status}, 成功={succeeded}, 失敗={failed}, skip={skipped}, dry_run={dry_run}"
