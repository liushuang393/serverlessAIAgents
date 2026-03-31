"""Agent runtime canonical contracts.

内部 agent runtime / bus / registry が共有する最小契約を定義する。
外部 A2A/MCP wire model とは分離し、protocol adapter 側で変換する。
"""

from __future__ import annotations

import time
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any, Literal


if TYPE_CHECKING:
    from kernel.core.metadata import AgentMetadata


RuntimeLocality = Literal["local", "remote"]


@dataclass(slots=True)
class AgentDescriptor:
    """内部 agent 記述子.

    外部 protocol 向けの card/tool を直接持たず、runtime 上の最小情報だけを保持する。
    """

    agent_id: str
    name: str
    description: str
    version: str = "1.0.0"
    capabilities: list[str] = field(default_factory=list)
    input_schema: dict[str, Any] = field(default_factory=lambda: {"type": "object"})
    output_schema: dict[str, Any] = field(default_factory=lambda: {"type": "object"})
    runtime_locality: RuntimeLocality = "local"
    supports_streaming: bool = False
    timeout_seconds: int | None = None
    max_retries: int | None = None
    protocol_exposure: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass(slots=True)
class AgentInvocation:
    """内部 agent 呼び出し要求."""

    agent_id: str
    input_data: dict[str, Any]
    context: dict[str, Any] = field(default_factory=dict)
    metadata: dict[str, Any] = field(default_factory=dict)
    requested_at: float = field(default_factory=time.time)


@dataclass(slots=True)
class AgentResult:
    """内部 agent 呼び出し結果."""

    agent_id: str
    success: bool
    output_data: dict[str, Any] = field(default_factory=dict)
    error: str | None = None
    duration_ms: float | None = None
    attempts: int = 1
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass(slots=True)
class AgentEvent:
    """内部 agent 実行イベント."""

    event_type: str
    agent_id: str
    payload: dict[str, Any] = field(default_factory=dict)
    timestamp: float = field(default_factory=time.time)


@dataclass(slots=True)
class AgentFeedback:
    """agent 実行フィードバック."""

    success: bool
    duration_ms: float | None = None
    cost: float | None = None
    score: float | None = None
    fallback_count: int = 0


@dataclass(slots=True)
class AgentFeedbackSummary:
    """agent 実行フィードバック集計."""

    total_runs: int = 0
    success_count: int = 0
    failure_count: int = 0
    fallback_count: int = 0
    total_duration_ms: float = 0.0
    total_cost: float = 0.0
    total_score: float = 0.0
    scored_runs: int = 0
    costed_runs: int = 0

    @property
    def success_rate(self) -> float:
        """成功率."""
        if self.total_runs == 0:
            return 0.0
        return self.success_count / self.total_runs

    @property
    def average_duration_ms(self) -> float:
        """平均実行時間."""
        if self.total_runs == 0:
            return 0.0
        return self.total_duration_ms / self.total_runs

    @property
    def average_cost(self) -> float:
        """平均コスト."""
        if self.costed_runs == 0:
            return 0.0
        return self.total_cost / self.costed_runs

    @property
    def average_score(self) -> float:
        """平均評価点."""
        if self.scored_runs == 0:
            return 0.0
        return self.total_score / self.scored_runs

    def record(self, feedback: AgentFeedback) -> None:
        """フィードバックを集計."""
        self.total_runs += 1
        if feedback.success:
            self.success_count += 1
        else:
            self.failure_count += 1
        self.fallback_count += feedback.fallback_count
        if feedback.duration_ms is not None:
            self.total_duration_ms += feedback.duration_ms
        if feedback.cost is not None:
            self.total_cost += feedback.cost
            self.costed_runs += 1
        if feedback.score is not None:
            self.total_score += feedback.score
            self.scored_runs += 1


def descriptor_from_agent_metadata(metadata: AgentMetadata) -> AgentDescriptor:
    """AgentMetadata から内部 descriptor を生成."""
    capabilities = [metadata.meta.category]
    a2a_skills = list(metadata.protocols.a2a.skills) if metadata.protocols.a2a and metadata.protocols.a2a.skills else []
    if a2a_skills:
        capabilities.extend(a2a_skills)
    protocol_exposure: list[str] = []
    if metadata.protocols.a2a and metadata.protocols.a2a.enabled:
        protocol_exposure.append("a2a")
    if metadata.protocols.mcp is not None:
        protocol_exposure.append("mcp")
    if metadata.protocols.agui and metadata.protocols.agui.enabled:
        protocol_exposure.append("agui")

    input_schema: dict[str, Any] = {
        "type": "object",
        "properties": {},
        "required": [],
    }
    for item in metadata.interfaces.inputs:
        item_schema: dict[str, Any] = {
            "type": item.type,
            "description": item.description,
        }
        if item.default is not None:
            item_schema["default"] = item.default
        if item.options:
            item_schema["enum"] = item.options
        if item.accept:
            item_schema["accept"] = item.accept
        input_schema["properties"][item.name] = item_schema
        if item.required:
            input_schema["required"].append(item.name)

    output_schema: dict[str, Any] = {
        "type": "object",
        "properties": {},
    }
    for item in metadata.interfaces.outputs:
        item_schema: dict[str, Any] = {"type": item.type}
        if item.output_schema:
            item_schema.update(item.output_schema)
        output_schema["properties"][item.name] = item_schema

    return AgentDescriptor(
        agent_id=metadata.meta.id,
        name=metadata.meta.name,
        description=metadata.meta.description,
        version=metadata.meta.version,
        capabilities=_dedupe(capabilities),
        input_schema=input_schema,
        output_schema=output_schema,
        runtime_locality="local",
        supports_streaming=bool(metadata.protocols.agui and metadata.protocols.agui.enabled),
        protocol_exposure=protocol_exposure,
        metadata={
            "author": metadata.meta.author,
            "icon": metadata.meta.icon,
            "category": metadata.meta.category,
            "a2a_skills": a2a_skills,
        },
    )


def descriptor_from_agent_instance(
    agent_instance: Any,
    *,
    agent_id: str | None = None,
    description: str | None = None,
    capabilities: list[str] | None = None,
    protocol_exposure: list[str] | None = None,
    metadata: dict[str, Any] | None = None,
) -> AgentDescriptor:
    """Agent インスタンスから内部 descriptor を推定生成."""
    resolved_name = getattr(agent_instance, "name", None) or type(agent_instance).__name__
    resolved_id = agent_id or resolved_name
    resolved_description = description or (getattr(agent_instance, "__doc__", "") or f"Agent: {resolved_name}").strip()
    input_schema: dict[str, Any] = {"type": "object"}
    output_schema: dict[str, Any] = {"type": "object"}

    orig_bases = getattr(type(agent_instance), "__orig_bases__", ())
    for base in orig_bases:
        args = getattr(base, "__args__", None)
        if args and len(args) >= 2:
            input_type, output_type = args[0], args[1]
            if hasattr(input_type, "model_json_schema"):
                input_schema = input_type.model_json_schema()
            if hasattr(output_type, "model_json_schema"):
                output_schema = output_type.model_json_schema()
            break

    resolved_protocols = list(protocol_exposure or [])
    if hasattr(agent_instance, "get_a2a_card") and "a2a" not in resolved_protocols:
        resolved_protocols.append("a2a")
    if hasattr(agent_instance, "get_mcp_tools") and "mcp" not in resolved_protocols:
        resolved_protocols.append("mcp")
    if hasattr(agent_instance, "run_stream") and "agui" not in resolved_protocols:
        resolved_protocols.append("agui")

    resolved_capabilities = list(capabilities or [])
    agent_type = getattr(agent_instance, "_agent_type", None)
    if isinstance(agent_type, str) and agent_type:
        resolved_capabilities.append(agent_type)

    resolved_metadata = dict(metadata or {})
    resolved_metadata.setdefault("python_class", type(agent_instance).__name__)

    return AgentDescriptor(
        agent_id=resolved_id,
        name=resolved_name,
        description=resolved_description,
        version=str(getattr(agent_instance, "version", "1.0.0")),
        capabilities=_dedupe(resolved_capabilities),
        input_schema=input_schema,
        output_schema=output_schema,
        runtime_locality="local",
        supports_streaming=hasattr(agent_instance, "run_stream")
        and callable(getattr(agent_instance, "run_stream", None)),
        timeout_seconds=_int_or_none(getattr(agent_instance, "timeout_seconds", None)),
        max_retries=_int_or_none(getattr(agent_instance, "max_retries", None)),
        protocol_exposure=_dedupe(resolved_protocols),
        metadata=resolved_metadata,
    )


def _dedupe(items: list[str]) -> list[str]:
    """順序を維持しながら重複を除去."""
    seen: set[str] = set()
    result: list[str] = []
    for item in items:
        normalized = item.strip()
        if not normalized or normalized in seen:
            continue
        seen.add(normalized)
        result.append(normalized)
    return result


def _int_or_none(value: Any) -> int | None:
    """int へ安全変換."""
    if isinstance(value, bool):
        return int(value)
    if isinstance(value, int):
        return value
    return None
