"""Tests for LiteLLMGateway routing, fallback, and cost tracking."""

from __future__ import annotations

from typing import Any

import pytest

from infrastructure.llm.gateway.config import (
    GatewayRuntimeConfig,
    LLMGatewayConfig,
    ModelConfig,
    ModelCostConfig,
    ProviderConfig,
    RoutingPolicyConfig,
)
from infrastructure.llm.gateway.router import LiteLLMGateway


def _build_config(*, priority: str = "quality", strategy: str = "round_robin") -> LLMGatewayConfig:
    return LLMGatewayConfig(
        gateway=GatewayRuntimeConfig(default_role="reasoning"),
        providers=[ProviderConfig(name="openai", api_key_env=None), ProviderConfig(name="google", api_key_env=None)],
        models=[
            ModelConfig(
                alias="reasoning_primary",
                provider="openai",
                model="gpt-4o",
                quality_score=0.9,
                avg_latency_ms=900,
                cost=ModelCostConfig(input_per_1k=0.004, output_per_1k=0.012),
            ),
            ModelConfig(
                alias="cheap_backup",
                provider="google",
                model="gemini-2.0-flash",
                quality_score=0.7,
                avg_latency_ms=600,
                cost=ModelCostConfig(input_per_1k=0.0002, output_per_1k=0.0008),
            ),
        ],
        registry={"reasoning": "reasoning_primary"},
        routing_policy=RoutingPolicyConfig(
            priority=priority,  # type: ignore[arg-type]
            fallback_chain={"reasoning": ["cheap_backup"]},
            load_balance_strategy=strategy,  # type: ignore[arg-type]
        ),
    )


def test_candidate_aliases_round_robin_rotation() -> None:
    gateway = LiteLLMGateway(config=_build_config(priority="cost", strategy="round_robin"))

    first = gateway._candidate_aliases("reasoning", None)
    second = gateway._candidate_aliases("reasoning", None)

    assert first == ["cheap_backup", "reasoning_primary"]
    assert second == ["reasoning_primary", "cheap_backup"]


@pytest.mark.asyncio
async def test_generate_fallback_and_cost_tracking(monkeypatch: pytest.MonkeyPatch) -> None:
    gateway = LiteLLMGateway(config=_build_config(priority="quality", strategy="round_robin"))

    async def _mock_call_completion(
        model_cfg: ModelConfig,
        *,
        messages: list[dict[str, Any]],
        tools: list[dict[str, Any]] | None,
        tool_choice: str | dict[str, Any] | None,
        temperature: float | None,
        max_tokens: int | None,
        metadata: dict[str, Any] | None,
    ) -> dict[str, Any]:
        if model_cfg.alias == "reasoning_primary":
            msg = "primary failed"
            raise RuntimeError(msg)
        return {
            "choices": [{"message": {"content": "ok"}, "finish_reason": "stop"}],
            "usage": {"prompt_tokens": 1000, "completion_tokens": 500, "total_tokens": 1500},
        }

    monkeypatch.setattr(gateway, "_call_completion", _mock_call_completion)

    response = await gateway.generate(role="reasoning", prompt="hello")
    assert response.content == "ok"
    assert response.model == "google/gemini-2.0-flash"

    summary = gateway.get_cost_summary()
    details = {item["alias"]: item for item in summary["details"]}
    assert details["reasoning_primary"]["failures"] == 1
    assert details["cheap_backup"]["requests"] == 1
    assert details["cheap_backup"]["cost_usd"] > 0
