"""Unit tests for LLM setup manager preflight workflow."""

from __future__ import annotations

import os
from pathlib import Path

from infrastructure.llm.gateway import InferenceEngineConfig, LLMGatewayConfig, ProviderConfig
from control_plane.schemas.llm_management_schemas import (
    LLMBackendKind,
    LLMPreflightRequest,
    LLMPreflightStep,
    LLMProviderKind,
    LLMSetupCommandResult,
)
from control_plane.services.llm_management_setup_manager import LLMSetupManager


class StubRunner:
    """Command runner with deterministic return codes by executable prefix."""

    def __init__(self, failures: set[str] | None = None) -> None:
        self.failures = failures or set()

    async def run(
        self,
        command: list[str],
        *,
        cwd: str | None = None,
        timeout_seconds: float = 120.0,
    ) -> LLMSetupCommandResult:
        del cwd, timeout_seconds
        key = " ".join(command[:4])
        failed = key in self.failures
        return LLMSetupCommandResult(
            command=command,
            return_code=1 if failed else 0,
            stdout="" if failed else "ok",
            stderr="failed" if failed else "",
            allowed=True,
        )


def _config() -> LLMGatewayConfig:
    return LLMGatewayConfig(
        providers=[
            ProviderConfig(
                name="openai",
                api_base="https://api.openai.com/v1",
                api_key_env="OPENAI_API_KEY",
                enabled=True,
            ),
        ],
        inference_engines=[
            InferenceEngineConfig(
                name="vllm",
                engine_type="vllm",
                base_url="http://127.0.0.1:8001",
                enabled=True,
            )
        ],
    )


async def _health_success(*, url: str, backend_name: str, dry_run: bool) -> LLMPreflightStep:
    del url, backend_name, dry_run
    return LLMPreflightStep(
        category="backend",
        target="vllm",
        phase="health",
        status="success",
        message="ok",
    )


async def test_preflight_provider_key_missing_fails(tmp_path: Path) -> None:
    previous = os.environ.pop("OPENAI_API_KEY", None)
    try:
        manager = LLMSetupManager(config_path=tmp_path / ".bizcore" / "llm_gateway.yaml")
        request = LLMPreflightRequest(
            providers=[LLMProviderKind.OPENAI],
            backends=[],
            auto_install=True,
            auto_start=True,
            health_check=True,
            dry_run=False,
        )

        report = await manager.preflight(request, _config())

        assert report.status == "failed"
        assert any(step.phase == "detect" and step.status == "failed" for step in report.steps)
    finally:
        if previous is not None:
            os.environ["OPENAI_API_KEY"] = previous


async def test_preflight_backend_detect_fail_install_success_recovers(tmp_path: Path) -> None:
    runner = StubRunner(failures={"python -m pip show"})
    manager = LLMSetupManager(
        config_path=tmp_path / ".bizcore" / "llm_gateway.yaml",
        command_runner=runner,
    )
    manager._run_health_check = _health_success  # type: ignore[method-assign]
    request = LLMPreflightRequest(
        providers=[],
        backends=[LLMBackendKind.VLLM],
        auto_install=True,
        auto_start=True,
        health_check=True,
        dry_run=False,
    )

    report = await manager.preflight(request, _config())

    assert report.status in {"success", "partial"}
    assert any(step.phase == "detect" and step.status == "failed" for step in report.steps)
    assert any(step.phase == "install" and step.status == "success" for step in report.steps)
    assert any(step.phase == "start" and step.status == "success" for step in report.steps)
    assert any(step.phase == "health" and step.status == "success" for step in report.steps)


async def test_preflight_dry_run_returns_dry_run_status(tmp_path: Path) -> None:
    manager = LLMSetupManager(
        config_path=tmp_path / ".bizcore" / "llm_gateway.yaml",
        command_runner=StubRunner(),
    )
    request = LLMPreflightRequest(
        providers=[],
        backends=[LLMBackendKind.TGI],
        auto_install=True,
        auto_start=True,
        health_check=True,
        dry_run=True,
    )

    report = await manager.preflight(request, _config())

    assert report.status == "dry_run"
    assert any(step.status == "dry_run" for step in report.steps)
