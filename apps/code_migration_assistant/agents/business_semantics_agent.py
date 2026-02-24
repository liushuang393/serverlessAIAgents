# -*- coding: utf-8 -*-
"""Business Semantics Agent - 業務語義抽出."""

from __future__ import annotations

from typing import Any

from agentflow import agent

from apps.code_migration_assistant.agents.prompts import BUSINESS_SEMANTICS_PROMPT
from apps.code_migration_assistant.workflow.models import (
    BusinessSemanticsArtifact,
    UnknownItem,
    build_meta,
)


@agent
class BusinessSemanticsAgent:
    """業務語義 Agent.

    LegacyAnalysis の技術情報を業務視点へ写像する。
    """

    system_prompt = BUSINESS_SEMANTICS_PROMPT

    def process(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """業務語義成果物を生成."""
        legacy_analysis = input_data.get("legacy_analysis")
        if not isinstance(legacy_analysis, dict):
            return {"success": False, "error": "legacy_analysis is required"}

        business_context = input_data.get("business_context", {})
        if not isinstance(business_context, dict):
            business_context = {}

        human_facts = input_data.get("human_facts", [])
        if not isinstance(human_facts, list):
            human_facts = []

        meta = legacy_analysis.get("meta", {})
        task_id = str(meta.get("task_id", "unknown-task"))
        trace_id = str(meta.get("trace_id", task_id))
        module = str(meta.get("module", "UNKNOWN"))

        entry_points = legacy_analysis.get("entry_points", [])
        control_flow = legacy_analysis.get("control_flow", [])
        external_calls = legacy_analysis.get("external_calls", [])
        data_structures = legacy_analysis.get("data_structures", [])

        business_processes = self._build_processes(entry_points, external_calls)
        business_events = self._build_events(control_flow, external_calls, human_facts)
        business_rules = self._build_rules(control_flow, human_facts)
        state_model = self._build_state_model(data_structures, business_context, business_events)

        unknowns: list[UnknownItem] = []
        if not business_processes:
            unknowns.append(UnknownItem(field="business_processes", reason="業務フローを抽出できなかった"))
        if not business_rules:
            unknowns.append(UnknownItem(field="business_rules", reason="業務ルールを抽出できなかった"))

        artifact = BusinessSemanticsArtifact(
            meta=build_meta(
                task_id=task_id,
                trace_id=trace_id,
                stage="business_semantics",
                source_language=meta.get("source_language"),
                target_language=meta.get("target_language"),
                module=module,
            ),
            business_processes=business_processes,
            business_events=business_events,
            state_model=state_model,
            business_rules=business_rules,
            unknowns=unknowns,
            extensions={
                "business_context": business_context,
                "human_fact_count": len(human_facts),
            },
        )
        return artifact.model_dump(mode="json")

    def _build_processes(
        self,
        entry_points: list[dict[str, Any]],
        external_calls: list[dict[str, Any]],
    ) -> list[dict[str, Any]]:
        """業務プロセスを構築."""
        if not entry_points:
            return []

        call_steps = [
            str(call.get("operation") or call.get("target") or call.get("type", "external"))
            for call in external_calls
        ]
        if not call_steps:
            call_steps = ["入力処理", "計算処理", "出力処理"]

        processes: list[dict[str, Any]] = []
        for ep in entry_points:
            name = str(ep.get("name", "UNKNOWN"))
            processes.append(
                {
                    "name": f"{name}業務フロー",
                    "entry_point": name,
                    "steps": call_steps[:6],
                    "complexity": "medium" if len(call_steps) > 3 else "low",
                }
            )
        return processes

    def _build_events(
        self,
        control_flow: list[dict[str, Any]],
        external_calls: list[dict[str, Any]],
        human_facts: list[dict[str, Any]],
    ) -> list[dict[str, Any]]:
        """業務イベントを抽出."""
        events: list[dict[str, Any]] = []

        for item in control_flow:
            statement = str(item.get("statement", ""))
            upper = statement.upper()
            if "DISPLAY" in upper or "WRITE" in upper:
                events.append({"name": "出力イベント", "trigger": statement, "type": "output"})
            elif "READ" in upper:
                events.append({"name": "読込イベント", "trigger": statement, "type": "input"})
            elif "CALL" in upper:
                events.append({"name": "連携イベント", "trigger": statement, "type": "integration"})

        for call in external_calls:
            ctype = str(call.get("type", "external"))
            if ctype == "sql":
                events.append({"name": "DB操作", "trigger": str(call.get("line", "")), "type": "data"})
            elif ctype == "program_call":
                events.append(
                    {
                        "name": "外部プログラム呼出",
                        "trigger": str(call.get("line", "")),
                        "type": "integration",
                    }
                )

        for fact in human_facts:
            if not isinstance(fact, dict):
                continue
            if str(fact.get("kind", "")).lower() != "event":
                continue
            events.append(
                {
                    "name": str(fact.get("name", "業務イベント")),
                    "trigger": str(fact.get("trigger", "human_fact")),
                    "type": str(fact.get("event_type", "business")),
                }
            )

        deduped: list[dict[str, Any]] = []
        seen: set[tuple[str, str]] = set()
        for event in events:
            key = (str(event.get("name")), str(event.get("trigger")))
            if key in seen:
                continue
            seen.add(key)
            deduped.append(event)
        return deduped

    def _build_rules(
        self,
        control_flow: list[dict[str, Any]],
        human_facts: list[dict[str, Any]],
    ) -> list[dict[str, Any]]:
        """業務ルールを抽出."""
        rules: list[dict[str, Any]] = []
        for idx, item in enumerate(control_flow, start=1):
            statement = str(item.get("statement", ""))
            upper = statement.upper()
            if "IF" in upper or "EVALUATE" in upper:
                rules.append(
                    {
                        "name": f"rule_{idx}",
                        "condition": statement,
                        "action": "条件分岐の結果に応じた処理",
                        "source_line": idx,
                    }
                )

        for fact in human_facts:
            if not isinstance(fact, dict):
                continue
            if str(fact.get("kind", "")).lower() != "rule":
                continue
            rules.append(
                {
                    "name": str(fact.get("name", "human_rule")),
                    "condition": str(fact.get("condition", "")),
                    "action": str(fact.get("action", "")),
                    "source_line": int(fact.get("source_line", 0) or 0),
                }
            )

        return rules

    def _build_state_model(
        self,
        data_structures: list[dict[str, Any]],
        business_context: dict[str, Any],
        business_events: list[dict[str, Any]],
    ) -> dict[str, Any]:
        """状態モデルを構築."""
        state_fields = [
            str(v.get("name", ""))
            for v in data_structures
            if "STATUS" in str(v.get("name", "")).upper() or "STATE" in str(v.get("name", "")).upper()
        ]

        initial_states = business_context.get("states")
        states = initial_states if isinstance(initial_states, list) else ["初期", "処理中", "完了"]

        transitions = []
        for event in business_events[:5]:
            transitions.append(
                {
                    "from": states[0],
                    "to": states[min(1, len(states) - 1)],
                    "event": event.get("name", "イベント"),
                }
            )

        return {
            "entity": business_context.get("entity", "業務エンティティ"),
            "state_fields": state_fields,
            "states": states,
            "transitions": transitions,
        }
