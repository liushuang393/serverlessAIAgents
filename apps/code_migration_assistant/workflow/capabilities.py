# -*- coding: utf-8 -*-
"""Stage capability contracts and skill-first execution adapter."""

from __future__ import annotations

import inspect
import time
from dataclasses import dataclass
from typing import Any, Awaitable, Callable


StageRunner = Callable[[dict[str, Any]], dict[str, Any] | Awaitable[dict[str, Any]]]
SkillExecutor = Callable[
    [str, dict[str, Any], "StageCapability"],
    dict[str, Any] | Awaitable[dict[str, Any]],
]


@dataclass(slots=True, frozen=True)
class StageCapability:
    """Capability contract for a pipeline stage."""

    stage: str
    capability_id: str
    input_schema: dict[str, Any]
    output_schema: dict[str, Any]
    fallback_policy: str = "native_fallback"
    version: str = "1.0.0"


@dataclass(slots=True)
class CapabilityExecution:
    """Single capability execution result."""

    output: dict[str, Any]
    trace: dict[str, Any]


CAPABILITY_MAP: dict[str, StageCapability] = {
    "analysis": StageCapability(
        stage="analysis",
        capability_id="legacy-ingestion",
        input_schema={"type": "object"},
        output_schema={"type": "object"},
    ),
    "business_semantics": StageCapability(
        stage="business_semantics",
        capability_id="business-semantics",
        input_schema={"type": "object"},
        output_schema={"type": "object"},
    ),
    "transform": StageCapability(
        stage="transform",
        capability_id="modernization-generator",
        input_schema={"type": "object"},
        output_schema={"type": "object"},
    ),
    "report": StageCapability(
        stage="report",
        capability_id="compliance-reporter",
        input_schema={"type": "object"},
        output_schema={"type": "object"},
    ),
}


class SkillStageAdapter:
    """Skill-first stage execution adapter with native fallback."""

    def __init__(
        self,
        *,
        skill_mode: str = "skill_first",
        skill_executor: SkillExecutor | None = None,
    ) -> None:
        self._skill_mode = skill_mode
        self._skill_executor = skill_executor

    async def execute(
        self,
        *,
        capability: StageCapability,
        payload: dict[str, Any],
        native_runner: StageRunner,
    ) -> CapabilityExecution:
        """Execute stage using skill first, then fallback to native."""
        if self._skill_mode == "native_only":
            output = await self._run_native(native_runner, payload)
            return CapabilityExecution(
                output=output,
                trace=self._build_trace(
                    capability=capability,
                    provider="native",
                    status="native_only",
                ),
            )

        if self._skill_executor is None:
            output = await self._run_native(native_runner, payload)
            return CapabilityExecution(
                output=output,
                trace=self._build_trace(
                    capability=capability,
                    provider="native",
                    status="fallback_no_skill_executor",
                ),
            )

        try:
            skill_output = await self._run_skill(capability, payload)
            return CapabilityExecution(
                output=skill_output,
                trace=self._build_trace(
                    capability=capability,
                    provider="skill",
                    status="applied",
                ),
            )
        except Exception as exc:  # noqa: BLE001
            if capability.fallback_policy != "native_fallback":
                raise
            output = await self._run_native(native_runner, payload)
            return CapabilityExecution(
                output=output,
                trace=self._build_trace(
                    capability=capability,
                    provider="native",
                    status="fallback_applied",
                    error=str(exc),
                ),
            )

    async def _run_native(
        self,
        native_runner: StageRunner,
        payload: dict[str, Any],
    ) -> dict[str, Any]:
        result = native_runner(payload)
        if inspect.isawaitable(result):
            result = await result
        if not isinstance(result, dict):
            msg = "native runner must return dict payload"
            raise TypeError(msg)
        return result

    async def _run_skill(
        self,
        capability: StageCapability,
        payload: dict[str, Any],
    ) -> dict[str, Any]:
        if self._skill_executor is None:
            msg = "skill executor is not configured"
            raise RuntimeError(msg)

        try:
            result = self._skill_executor(capability.capability_id, payload, capability)
        except TypeError:
            # 互換性のため 2 引数形式も許容
            result = self._skill_executor(  # type: ignore[call-arg]
                capability.capability_id,
                payload,
            )
        if inspect.isawaitable(result):
            result = await result
        if not isinstance(result, dict):
            msg = "skill executor must return dict payload"
            raise TypeError(msg)

        if result.get("success") is False:
            msg = str(result.get("error", "skill execution failed"))
            raise RuntimeError(msg)

        output = result.get("output", result)
        if not isinstance(output, dict):
            msg = "skill output must be dict payload"
            raise TypeError(msg)
        return output

    def _build_trace(
        self,
        *,
        capability: StageCapability,
        provider: str,
        status: str,
        error: str | None = None,
    ) -> dict[str, Any]:
        trace: dict[str, Any] = {
            "stage": capability.stage,
            "capability_id": capability.capability_id,
            "provider": provider,
            "status": status,
            "fallback_policy": capability.fallback_policy,
            "version": capability.version,
            "timestamp": time.time(),
        }
        if error:
            trace["error"] = error
        return trace


class StageCapabilityRunner:
    """Run stages with capability contract + skill/native adapter."""

    def __init__(
        self,
        *,
        skill_mode: str = "skill_first",
        skill_executor: SkillExecutor | None = None,
        capability_map: dict[str, StageCapability] | None = None,
    ) -> None:
        self._map = capability_map or CAPABILITY_MAP
        self._adapter = SkillStageAdapter(
            skill_mode=skill_mode,
            skill_executor=skill_executor,
        )

    async def run_stage(
        self,
        *,
        stage: str,
        payload: dict[str, Any],
        native_runner: StageRunner,
    ) -> CapabilityExecution:
        """Run a stage and return normalized output + trace."""
        capability = self._map.get(stage)
        if capability is None:
            result = native_runner(payload)
            if inspect.isawaitable(result):
                result = await result
            if not isinstance(result, dict):
                msg = "native runner must return dict payload"
                raise TypeError(msg)
            return CapabilityExecution(
                output=result,
                trace={
                    "stage": stage,
                    "capability_id": "native",
                    "provider": "native",
                    "status": "unmapped_stage",
                    "timestamp": time.time(),
                },
            )

        return await self._adapter.execute(
            capability=capability,
            payload=payload,
            native_runner=native_runner,
        )
