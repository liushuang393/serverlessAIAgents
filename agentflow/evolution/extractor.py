"""Strategy extractor implementation for Evolution V2."""

from __future__ import annotations

import re
from typing import Any

from agentflow.evolution.types import (
    ExecutionEvent,
    OutcomeRecord,
    ScopeLevel,
    StrategyCapsule,
    StrategyScope,
    StrategyValidity,
)


_WORD_RE = re.compile(r"[a-zA-Z0-9_\-]{2,}")


class StrategyExtractor:
    """Extracts reusable strategy capsules from successful runs."""

    async def extract(
        self,
        *,
        task: str,
        context: dict[str, Any],
        events: list[ExecutionEvent],
        outcome: OutcomeRecord,
    ) -> StrategyCapsule | None:
        if not outcome.success:
            return None

        tool_sequence = self._extract_tool_sequence(events)
        guard_conditions = self._extract_guard_conditions(events, context)
        keywords = self._extract_keywords(task, tool_sequence, context)

        scope = StrategyScope(
            scope_level=ScopeLevel.TENANT_APP,
            tenant_id=self._str_or_none(context.get("tenant_id")),
            app_id=self._str_or_none(context.get("app_id")),
            product_line=self._str_or_none(context.get("product_line")),
        )

        return StrategyCapsule(
            intent=task,
            environment_fingerprint=self._extract_environment(context),
            tool_sequence=tool_sequence,
            guard_conditions=guard_conditions,
            expected_output=self._expected_output(context, outcome),
            validation_method="replay+tests+policy_checks",
            keywords=keywords,
            scope=scope,
            validity=StrategyValidity(max_age_days=30),
        )

    def _extract_tool_sequence(self, events: list[ExecutionEvent]) -> list[str]:
        sequence: list[str] = []
        for event in events:
            if event.event_type != "tool_result":
                continue
            tool_name = self._str_or_none(event.payload.get("tool_uri"))
            if tool_name is None:
                tool_name = self._str_or_none(event.payload.get("tool_name"))
            if tool_name:
                sequence.append(tool_name)
        return sequence

    def _extract_guard_conditions(
        self,
        events: list[ExecutionEvent],
        context: dict[str, Any],
    ) -> list[str]:
        conditions: list[str] = []
        if context.get("requires_approval"):
            conditions.append("requires_approval")
        if context.get("read_only"):
            conditions.append("read_only_mode")

        error_count = sum(1 for event in events if event.event_type == "step_error")
        if error_count > 0:
            conditions.append(f"error_recovered:{error_count}")

        if context.get("product_line"):
            conditions.append(f"product_line:{context['product_line']}")

        return conditions

    def _extract_keywords(
        self,
        task: str,
        tool_sequence: list[str],
        context: dict[str, Any],
    ) -> list[str]:
        tokens: set[str] = set()
        for token in _WORD_RE.findall(task.lower()):
            tokens.add(token)

        for item in tool_sequence:
            for token in _WORD_RE.findall(item.lower()):
                tokens.add(token)

        for key in ("product_line", "app_id", "tenant_id", "task_type"):
            value = self._str_or_none(context.get(key))
            if value:
                for token in _WORD_RE.findall(value.lower()):
                    tokens.add(token)

        return sorted(tokens)[:64]

    def _extract_environment(self, context: dict[str, Any]) -> dict[str, Any]:
        return {
            "language": context.get("language") or "python",
            "framework": context.get("framework") or "agentflow",
            "framework_version": context.get("framework_version"),
            "runtime": context.get("runtime"),
            "product_line": context.get("product_line"),
        }

    def _expected_output(self, context: dict[str, Any], outcome: OutcomeRecord) -> str:
        if outcome.failure_reason:
            return f"success_without:{outcome.failure_reason}"
        if context.get("expected_output"):
            return str(context["expected_output"])
        return "task_completed"

    def _str_or_none(self, value: Any) -> str | None:
        if value is None:
            return None
        text = str(value).strip()
        return text or None
