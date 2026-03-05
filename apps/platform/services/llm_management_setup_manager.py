"""Setup manager for provider/backend preflight in LLM management."""

from __future__ import annotations

import asyncio
from datetime import datetime, timezone
from pathlib import Path
from typing import Protocol

import httpx
from agentflow.llm.gateway import LLMGatewayConfig, resolve_secret
from apps.platform.schemas.llm_management_schemas import (
    LLMBackendKind,
    LLMPreflightReport,
    LLMPreflightRequest,
    LLMPreflightStep,
    LLMProviderKind,
    LLMSetupCommandResult,
)
from apps.platform.services.llm_management_validator import (
    LLMConfigValidator,
    provider_default_api_key_env,
)


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

    async def preflight(
        self,
        request: LLMPreflightRequest,
        config: LLMGatewayConfig,
    ) -> LLMPreflightReport:
        """Run preflight and return detailed report."""
        started_at = datetime.now(timezone.utc).isoformat()
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
                        if step.category == "backend"
                        and step.target == failed_step.target
                        and step.phase == "install"
                    ),
                    None,
                )
                if install_step is not None and install_step.status in {"success", "dry_run"}:
                    continue
            filtered_failures.append(failed_step)

        statuses = {step.status for step in steps}
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
            completed_at=datetime.now(timezone.utc).isoformat(),
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
        by_name = {
            self._validator.canonical_provider_name(provider.name): provider
            for provider in config.providers
        }
        steps: list[LLMPreflightStep] = []
        for provider_kind in provider_targets:
            name = self._validator.canonical_provider_name(provider_kind.value)
            provider = by_name.get(name)
            env_name = provider.api_key_env if provider is not None else provider_default_api_key_env(name)

            if name in {"local", "ollama", "custom"} and not env_name:
                steps.append(
                    LLMPreflightStep(
                        category="provider",
                        target=name,
                        phase="detect",
                        status="success" if not dry_run else "dry_run",
                        message=f"provider {name} does not require api key",
                    )
                )
                continue

            value, source = resolve_secret(env_name, config_path=self._config_path)
            if value:
                steps.append(
                    LLMPreflightStep(
                        category="provider",
                        target=name,
                        phase="detect",
                        status="success",
                        message=f"api key resolved from {source}",
                    )
                )
            else:
                remediation = [f"Set {env_name} in environment or .env before switching to {name}."]
                if name == "google":
                    remediation.append("GEMINI_API_KEY is the canonical variable for google/gemini.")
                steps.append(
                    LLMPreflightStep(
                        category="provider",
                        target=name,
                        phase="detect",
                        status="failed",
                        message="api key not configured",
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
                message="backend dependency detected" if detect_success else "backend dependency is missing",
                command=detect_result,
                remediation=[] if detect_success else [f"Install runtime dependency for {backend_name}."],
            )
        )

        if not detect_success:
            install_status = "skipped"
            install_message = "auto_install disabled"
            install_command_result: LLMSetupCommandResult | None = None
            if request.auto_install:
                install_command = self._backend_install_command(backend)
                install_command_result = await self._run_command(
                    install_command,
                    dry_run=request.dry_run,
                    cwd=None,
                    timeout_seconds=900.0,
                )
                install_ok = bool(
                    install_command_result.error is None and install_command_result.return_code == 0
                )
                install_status = "dry_run" if request.dry_run else ("success" if install_ok else "failed")
                install_message = (
                    "backend dependency install succeeded"
                    if install_ok
                    else "backend dependency install failed"
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
                        "Verify internet and package mirrors for install command.",
                        f"Run install command manually for {backend_name} and retry preflight.",
                    ]
                    if install_status == "failed"
                    else [],
                )
            )

        if request.auto_start:
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
                    message="backend start command executed" if start_ok else "backend start command failed",
                    command=start_result,
                    remediation=[
                        "Ensure docker engine is running and accessible.",
                        f"Inspect compose logs under {compose_path.parent}.",
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
                    message="auto_start disabled",
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
                    message="health_check disabled",
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
                message=f"health check planned: GET {url}",
            )

        try:
            async with httpx.AsyncClient(timeout=5.0) as client:
                response = await client.get(url)
            if response.status_code < 400:
                return LLMPreflightStep(
                    category="backend",
                    target=backend_name,
                    phase="health",
                    status="success",
                    message=f"health check passed: {url}",
                )
            return LLMPreflightStep(
                category="backend",
                target=backend_name,
                phase="health",
                status="failed",
                message=f"health check failed: status={response.status_code}",
                remediation=[f"Verify backend endpoint and health_path: {url}"],
            )
        except Exception as exc:
            return LLMPreflightStep(
                category="backend",
                target=backend_name,
                phase="health",
                status="failed",
                message=f"health check failed: {exc}",
                remediation=[f"Verify backend endpoint is reachable: {url}"],
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
        return ["docker", "pull", "ghcr.io/huggingface/text-generation-inference:latest"]

    def _backend_health_url(self, config: LLMGatewayConfig, backend: LLMBackendKind) -> str:
        defaults = {
            LLMBackendKind.VLLM: ("http://127.0.0.1:8001", "/health"),
            LLMBackendKind.SGLANG: ("http://127.0.0.1:30000", "/health"),
            LLMBackendKind.TGI: ("http://127.0.0.1:8080", "/health"),
        }
        base_url, health_path = defaults[backend]
        for engine in config.inference_engines:
            normalized = engine.name.strip().lower()
            if normalized == backend.value:
                base_url = engine.base_url
                health_path = engine.health_path
                break
        return f"{base_url.rstrip('/')}/{health_path.lstrip('/')}"

    def _ensure_backend_compose(self, backend: LLMBackendKind) -> Path:
        target_dir = self._config_path.parent / "llm_backends"
        target_dir.mkdir(parents=True, exist_ok=True)
        target = target_dir / f"{backend.value}.compose.yml"
        if target.exists():
            return target

        compose = self._compose_template(backend)
        target.write_text(compose, encoding="utf-8")
        return target

    @staticmethod
    def _compose_template(backend: LLMBackendKind) -> str:
        if backend == LLMBackendKind.VLLM:
            return """services:
  llm-vllm:
    image: vllm/vllm-openai:latest
    container_name: llm-vllm
    ports:
      - "8001:8000"
    command: ["--model", "Qwen/Qwen2.5-0.5B-Instruct"]
"""
        if backend == LLMBackendKind.SGLANG:
            return """services:
  llm-sglang:
    image: lmsysorg/sglang:latest
    container_name: llm-sglang
    ports:
      - "30000:30000"
"""
        return """services:
  llm-tgi:
    image: ghcr.io/huggingface/text-generation-inference:latest
    container_name: llm-tgi
    ports:
      - "8080:80"
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

        if binary == "curl":
            return True

        return False

    @staticmethod
    def _summary_from_steps(steps: list[LLMPreflightStep], status: str) -> str:
        failed = sum(1 for step in steps if step.status == "failed")
        succeeded = sum(1 for step in steps if step.status == "success")
        skipped = sum(1 for step in steps if step.status == "skipped")
        dry_run = sum(1 for step in steps if step.status == "dry_run")
        return (
            f"status={status}, success={succeeded}, failed={failed}, "
            f"skipped={skipped}, dry_run={dry_run}"
        )
