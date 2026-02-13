"""Lightning backend 統合テスト."""

from __future__ import annotations

import pytest

from agentflow.run import (
    LightningRuntimeConfig,
    PromptRewardSample,
    resolve_lightning_store,
    train_with_lightning_backend,
)


def test_resolve_lightning_store_disabled() -> None:
    config = LightningRuntimeConfig(enabled=False)
    store = resolve_lightning_store(config)
    assert store is None


def test_resolve_lightning_store_auto_returns_store() -> None:
    config = LightningRuntimeConfig(enabled=True, backend="auto")
    store = resolve_lightning_store(config)
    assert store is not None


def test_resolve_lightning_store_strict_microsoft_raises() -> None:
    config = LightningRuntimeConfig(enabled=True, backend="microsoft", strict_backend=True)
    with pytest.raises(RuntimeError):
        _ = resolve_lightning_store(config)


@pytest.mark.asyncio
async def test_train_with_lightning_backend_disabled() -> None:
    result = await train_with_lightning_backend(
        samples=[],
        runtime=LightningRuntimeConfig(enable_training=False),
    )
    assert result.success is True
    assert result.backend == "disabled"
    assert result.trained is False


@pytest.mark.asyncio
async def test_train_with_lightning_backend_builtin_profile() -> None:
    samples = [
        PromptRewardSample(
            run_id="run-1",
            event_id="evt-1",
            step=0,
            prompt="q1",
            response="a1",
            reward=0.9,
        ),
        PromptRewardSample(
            run_id="run-1",
            event_id="evt-2",
            step=1,
            prompt="q2",
            response="a2",
            reward=0.4,
        ),
    ]
    result = await train_with_lightning_backend(
        samples=samples,
        runtime=LightningRuntimeConfig(
            enable_training=True,
            backend="builtin",
            enable_api_optimization=True,
        ),
    )
    assert result.success is True
    assert result.backend == "builtin"
    assert result.optimized is True
    assert "temperature" in result.optimized_llm_profile


@pytest.mark.asyncio
async def test_train_with_lightning_backend_strict_microsoft_unavailable() -> None:
    samples = [
        PromptRewardSample(
            run_id="run-2",
            event_id="evt-3",
            step=0,
            prompt="q",
            response="a",
            reward=0.1,
        )
    ]
    result = await train_with_lightning_backend(
        samples=samples,
        runtime=LightningRuntimeConfig(
            enable_training=True,
            backend="microsoft",
            strict_backend=True,
        ),
    )
    assert result.success is False
    assert result.backend == "microsoft"
