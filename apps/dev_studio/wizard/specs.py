"""Builder 向け spec helpers."""

from __future__ import annotations

import re
import uuid
from typing import Any, Literal

from apps.dev_studio.wizard.models import (
    AgentSpec,
    EngineType,
    SystemSpec,
    ValidationResult,
    ValidationStatus,
)
from contracts.core.types import EdgeDefinition, NodeDefinition, WorkflowDefinition


SpecKind = Literal["agent", "system"]

_DEFAULT_AGENT_TEMPLATES: tuple[dict[str, str], ...] = (
    {
        "id": "simple",
        "kind": "agent",
        "name": "Simple Agent",
        "description": "単一責務の Agent を素早く立ち上げる基本テンプレート",
    },
    {
        "id": "pipeline",
        "kind": "agent",
        "name": "Pipeline Agent",
        "description": "順次ステップで処理を進める Agent テンプレート",
    },
    {
        "id": "rag",
        "kind": "agent",
        "name": "RAG Agent",
        "description": "検索と生成を組み合わせる知識強化型テンプレート",
    },
    {
        "id": "gate",
        "kind": "agent",
        "name": "Gate Agent",
        "description": "ポリシー判定や入口制御を担うテンプレート",
    },
)

_DEFAULT_SYSTEM_TEMPLATES: tuple[dict[str, str], ...] = (
    {
        "id": "multi_agent_system",
        "kind": "system",
        "name": "Multi-Agent System",
        "description": "Coordinator を中心に複数 Agent を組み合わせる標準構成",
    },
    {
        "id": "research_copilot",
        "kind": "system",
        "name": "Research Copilot",
        "description": "調査、要約、レビューを分担するリサーチ向け構成",
    },
    {
        "id": "ops_automation",
        "kind": "system",
        "name": "Ops Automation",
        "description": "受付、実行、検証の 3 層で自動化を組む運用向け構成",
    },
    {
        "id": "knowledge_orchestrator",
        "kind": "system",
        "name": "Knowledge Orchestrator",
        "description": "知識取得、分析、報告を分離するナレッジ運用構成",
    },
)


def list_builder_templates() -> list[dict[str, str]]:
    """Builder 用テンプレート一覧を返す."""
    return [*map(dict, _DEFAULT_AGENT_TEMPLATES), *map(dict, _DEFAULT_SYSTEM_TEMPLATES)]


def normalize_spec_kind(value: str | None) -> SpecKind:
    """spec kind を正規化する."""
    if str(value or "agent").strip().lower() == "system":
        return "system"
    return "agent"


def build_system_spec(
    *,
    description: str,
    name: str | None = None,
    template_id: str | None = None,
) -> SystemSpec:
    """説明文から最小の system spec を組み立てる."""
    normalized_template = str(template_id or "multi_agent_system").strip().lower() or "multi_agent_system"
    system_name = name or _derive_system_name(description, normalized_template)
    goal = description.strip()
    keyword_capabilities = _extract_capabilities(description)

    if normalized_template == "research_copilot":
        agents = [
            _make_agent(
                name="ResearchPlanner",
                description="調査計画を立案し、必要な調査観点を整理する。",
                capabilities=["task_planning", "question_decomposition", *keyword_capabilities[:2]],
                engine_type=EngineType.PIPELINE,
                prompt_focus=goal,
            ),
            _make_agent(
                name="EvidenceResearcher",
                description="必要な情報を収集し、根拠を整理する。",
                capabilities=["evidence_collection", "source_summarization", *keyword_capabilities[:3]],
                engine_type=EngineType.RAG,
                prompt_focus=goal,
            ),
            _make_agent(
                name="SynthesisReviewer",
                description="結果を統合し、品質と一貫性を確認する。",
                capabilities=["answer_synthesis", "quality_review", "risk_check"],
                engine_type=EngineType.GATE,
                prompt_focus=goal,
            ),
        ]
        shared_skills = ["web-search", "document-summary", "review-loop"]
    elif normalized_template == "ops_automation":
        agents = [
            _make_agent(
                name="IntakeRouter",
                description="依頼内容を分類し、適切な実行ルートへ振り分ける。",
                capabilities=["intent_routing", "request_validation"],
                engine_type=EngineType.GATE,
                prompt_focus=goal,
            ),
            _make_agent(
                name="TaskExecutor",
                description="実作業を自動実行し、結果を構造化する。",
                capabilities=["tool_execution", "task_automation", *keyword_capabilities[:2]],
                engine_type=EngineType.PIPELINE,
                prompt_focus=goal,
            ),
            _make_agent(
                name="ChangeReviewer",
                description="実行結果を検証し、差分と失敗要因を確認する。",
                capabilities=["result_validation", "change_review", "reporting"],
                engine_type=EngineType.GATE,
                prompt_focus=goal,
            ),
        ]
        shared_skills = ["command-runner", "policy-check", "change-summary"]
    elif normalized_template == "knowledge_orchestrator":
        agents = [
            _make_agent(
                name="KnowledgeRetriever",
                description="関連知識を取得し、一次情報を整理する。",
                capabilities=["retrieval", "knowledge_lookup", *keyword_capabilities[:2]],
                engine_type=EngineType.RAG,
                prompt_focus=goal,
            ),
            _make_agent(
                name="KnowledgeAnalyst",
                description="知識を分析し、判断や提案を生成する。",
                capabilities=["analysis", "reasoning", *keyword_capabilities[:3]],
                engine_type=EngineType.PIPELINE,
                prompt_focus=goal,
            ),
            _make_agent(
                name="KnowledgeReporter",
                description="意思決定者向けに結果を構造化して報告する。",
                capabilities=["report_generation", "stakeholder_summary"],
                engine_type=EngineType.SIMPLE,
                prompt_focus=goal,
            ),
        ]
        shared_skills = ["knowledge-search", "citation-format", "decision-summary"]
    else:
        agents = [
            _make_agent(
                name="SystemCoordinator",
                description="全体方針を管理し、必要な Agent へタスクを割り当てる。",
                capabilities=["coordination", "planning", *keyword_capabilities[:2]],
                engine_type=EngineType.PIPELINE,
                prompt_focus=goal,
            ),
            _make_agent(
                name="DomainSpecialist",
                description="対象業務の中核処理を担当する。",
                capabilities=["domain_execution", *keyword_capabilities[:3]],
                engine_type=EngineType.SIMPLE,
                prompt_focus=goal,
            ),
            _make_agent(
                name="QualityGuard",
                description="成果物を検証し、再実行や改善を判断する。",
                capabilities=["quality_review", "policy_check", "feedback_loop"],
                engine_type=EngineType.GATE,
                prompt_focus=goal,
            ),
        ]
        shared_skills = ["planner", "review-loop"]

    flows = _sequential_flows(agents)
    metadata = {
        "generated_by": "builder_v1",
        "template_id": normalized_template,
        "keywords": keyword_capabilities,
    }

    return SystemSpec(
        name=system_name,
        description=f"{goal} を達成するための {len(agents)} Agent 構成",
        goal=goal,
        template_id=normalized_template,
        agents=agents,
        flows=flows,
        shared_skills=shared_skills,
        shared_tools=[],
        execution_mode="coordinator",
        confidence=0.76,
        metadata=metadata,
    )


def build_agent_spec(
    *,
    description: str,
    name: str | None = None,
    template_id: str | None = None,
) -> AgentSpec:
    """説明文から最小の agent spec を組み立てる."""
    normalized_template = str(template_id or "simple").strip().lower() or "simple"
    capabilities = _extract_capabilities(description)
    engine_type = {
        "pipeline": EngineType.PIPELINE,
        "rag": EngineType.RAG,
        "gate": EngineType.GATE,
    }.get(normalized_template, EngineType.SIMPLE)
    agent_name = name or _derive_agent_name(description)
    return AgentSpec(
        name=agent_name,
        description=description.strip() or f"{agent_name} generated by builder",
        capabilities=capabilities or ["task_execution"],
        required_skills=[],
        required_tools=[],
        system_prompt=f"You are {agent_name}. Focus on: {description.strip() or agent_name}",
        engine_type=engine_type,
        confidence=0.74,
        metadata={"generated_by": "builder_v1", "template_id": normalized_template},
    )


def validate_builder_spec(spec_kind: SpecKind, spec: dict[str, Any]) -> ValidationResult:
    """builder spec を検証する."""
    errors: list[str] = []
    warnings: list[str] = []
    suggestions: list[str] = []

    name = str(spec.get("name", "")).strip()
    description = str(spec.get("description", "")).strip()

    if not name:
        errors.append("name は必須です。")
    elif not name[0].isupper():
        warnings.append("name は PascalCase または Title Case を推奨します。")

    if not description:
        warnings.append("description が空です。")

    if spec_kind == "agent":
        capabilities = spec.get("capabilities") or []
        engine_type = str(spec.get("engine_type", "simple")).strip().lower()
        if not capabilities:
            warnings.append("capabilities が空です。")
        if engine_type not in {member.value for member in EngineType}:
            errors.append(f"engine_type が不正です: {engine_type}")
    else:
        agents = spec.get("agents") or []
        flows = spec.get("flows") or []
        if len(agents) < 2:
            errors.append("system spec には 2 つ以上の agents が必要です。")
        agent_names = [str(agent.get("name", "")).strip() for agent in agents if isinstance(agent, dict)]
        duplicate_names = sorted({item for item in agent_names if agent_names.count(item) > 1 and item})
        if duplicate_names:
            errors.append(f"agents の name が重複しています: {', '.join(duplicate_names)}")
        if not flows:
            warnings.append("flows が空です。最小連携フローを定義することを推奨します。")
        known_names = {name for name in agent_names if name}
        for flow in flows:
            if not isinstance(flow, dict):
                continue
            source = str(flow.get("source", "")).strip()
            target = str(flow.get("target", "")).strip()
            if source and source not in known_names:
                errors.append(f"flow source が agents に存在しません: {source}")
            if target and target not in known_names:
                errors.append(f"flow target が agents に存在しません: {target}")
        if not spec.get("goal"):
            warnings.append("goal を設定すると codegen の意図が安定します。")
        if len(agents) > 5:
            suggestions.append("v1 builder では 3-5 Agent 程度に絞ると実装しやすくなります。")

    score = max(0.0, 1.0 - len(errors) * 0.25 - len(warnings) * 0.08)
    status = ValidationStatus.PASSED if not errors else ValidationStatus.FAILED
    if not errors and warnings:
        status = ValidationStatus.WARNING

    return ValidationResult(
        valid=not errors,
        status=status,
        errors=errors,
        warnings=warnings,
        suggestions=suggestions,
        score=score,
    )


def workflow_from_spec(
    spec_kind: SpecKind,
    spec: AgentSpec | SystemSpec | dict[str, Any],
) -> WorkflowDefinition:
    """spec を workflow に変換する."""
    if spec_kind == "system":
        if isinstance(spec, SystemSpec):
            system = spec
        elif isinstance(spec, dict):
            system = SystemSpec.from_dict(spec)
        else:
            msg = "system spec は SystemSpec または dict である必要があります。"
            raise TypeError(msg)
        nodes = []
        agent_id_map: dict[str, str] = {}
        for index, agent in enumerate(system.agents):
            node_id = f"agent-{index + 1}"
            agent_id_map[agent.name] = node_id
            nodes.append(
                NodeDefinition(
                    id=node_id,
                    type="agent",
                    agent_type=agent.name,
                    config={
                        "description": agent.description,
                        "capabilities": agent.capabilities,
                        "required_skills": agent.required_skills,
                        "required_tools": agent.required_tools,
                        "system_prompt": agent.system_prompt,
                        "engine_type": agent.engine_type.value,
                    },
                    position={"x": float(index * 240), "y": float((index % 2) * 120)},
                )
            )

        edges = []
        for index, flow in enumerate(system.flows):
            source_name = str(flow.get("source", "")).strip()
            target_name = str(flow.get("target", "")).strip()
            if source_name not in agent_id_map or target_name not in agent_id_map:
                continue
            edges.append(
                EdgeDefinition(
                    id=f"edge-{index + 1}",
                    source=agent_id_map[source_name],
                    target=agent_id_map[target_name],
                    label=str(flow.get("condition", "")).strip(),
                )
            )

        return WorkflowDefinition(
            id=str(uuid.uuid4()),
            name=system.name,
            description=system.goal or system.description,
            nodes=nodes,
            edges=edges,
            metadata={
                "spec_kind": "system",
                "template_id": system.template_id,
                "execution_mode": system.execution_mode,
                "shared_skills": system.shared_skills,
                "shared_tools": system.shared_tools,
                "confidence": system.confidence,
                "agents": [agent.to_dict() for agent in system.agents],
                "flows": system.flows,
            },
        )

    if isinstance(spec, AgentSpec):
        agent = spec
    elif isinstance(spec, dict):
        agent = AgentSpec.from_dict(spec)
    else:
        msg = "agent spec は AgentSpec または dict である必要があります。"
        raise TypeError(msg)
    return WorkflowDefinition(
        id=str(uuid.uuid4()),
        name=agent.name,
        description=agent.description,
        nodes=[
            NodeDefinition(
                id="agent-1",
                type="agent",
                agent_type=agent.name,
                config={
                    "description": agent.description,
                    "capabilities": agent.capabilities,
                    "required_skills": agent.required_skills,
                    "required_tools": agent.required_tools,
                    "system_prompt": agent.system_prompt,
                    "engine_type": agent.engine_type.value,
                },
                position={"x": 0.0, "y": 0.0},
            )
        ],
        edges=[],
        metadata={"spec_kind": "agent", "agent_spec": agent.to_dict(), "confidence": agent.confidence},
    )


def _derive_system_name(description: str, template_id: str) -> str:
    slug = re.sub(r"[^a-zA-Z0-9]+", " ", description).strip()
    words = [word.capitalize() for word in slug.split()[:3]]
    if words:
        return "".join(words) + "System"
    return "".join(part.capitalize() for part in template_id.split("_")) or "GeneratedSystem"


def _derive_agent_name(description: str) -> str:
    slug = re.sub(r"[^a-zA-Z0-9]+", " ", description).strip()
    words = [word.capitalize() for word in slug.split()[:2]]
    if words:
        return "".join(words) + "Agent"
    return "GeneratedAgent"


def _extract_capabilities(description: str) -> list[str]:
    words = [word.lower() for word in re.findall(r"[a-zA-Z][a-zA-Z0-9_-]+", description)]
    stop_words = {
        "the",
        "and",
        "for",
        "with",
        "that",
        "this",
        "from",
        "into",
        "agent",
        "system",
    }
    capabilities: list[str] = []
    for word in words:
        if word in stop_words or len(word) < 4:
            continue
        if word not in capabilities:
            capabilities.append(word)
    return capabilities[:6]


def _make_agent(
    *,
    name: str,
    description: str,
    capabilities: list[str],
    engine_type: EngineType,
    prompt_focus: str,
) -> AgentSpec:
    return AgentSpec(
        name=name,
        description=description,
        capabilities=capabilities,
        required_skills=[],
        required_tools=[],
        system_prompt=f"You are {name}. Focus on: {prompt_focus or description}",
        engine_type=engine_type,
        confidence=0.72,
        metadata={"generated_by": "builder_v1"},
    )


def _sequential_flows(agents: list[AgentSpec]) -> list[dict[str, Any]]:
    flows: list[dict[str, Any]] = []
    for index in range(len(agents) - 1):
        flows.append(
            {
                "source": agents[index].name,
                "target": agents[index + 1].name,
                "condition": "handoff",
            }
        )
    return flows


__all__ = [
    "SpecKind",
    "build_agent_spec",
    "build_system_spec",
    "list_builder_templates",
    "normalize_spec_kind",
    "validate_builder_spec",
    "workflow_from_spec",
]
