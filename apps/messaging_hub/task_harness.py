"""Generic task harness primitives for Messaging Hub.

タスク固有の specialist agent を増やす代わりに、
共通の planning / memory / provider discovery / verification を束ねる。
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import StrEnum
from typing import TYPE_CHECKING, Any
from urllib.parse import urlparse

from pydantic import BaseModel, Field

from kernel.planner.service import ExecutionPlan, PlanStep, StepType
from kernel.reviewer.service import ResultVerifier, VerificationResult, VerificationStrategy


if TYPE_CHECKING:
    from apps.messaging_hub.mcp_manager import MCPManager
    from kernel.skills.gateway import SkillGateway


class ProviderDiscoveryMode(StrEnum):
    """外部 provider 発見モード."""

    NONE = "none"
    DISCOVER_FIRST = "discover_first"
    USER_PROVIDED = "user_provided"


class HarnessWorkerRole(StrEnum):
    """worker 役割."""

    MAIN = "main_agent"
    SPECIALIST = "specialist"
    VERIFIER = "verifier"
    MONITOR = "monitor"


class MemoryLayer(StrEnum):
    """記憶レイヤ."""

    SENSORY = "sensory"
    WORKING = "working"
    EPISODIC = "episodic"
    LONG_TERM = "long_term"


class MemoryOwner(StrEnum):
    """記憶 ownership."""

    MAIN_AGENT = "main_agent"
    SHARED_TASK = "shared_task"
    SUBAGENT_PRIVATE = "subagent_private"


class ProviderCandidate(BaseModel):
    """候補 provider."""

    domain: str = Field(default="")
    url: str = Field(default="")
    title: str = Field(default="")
    snippet: str = Field(default="")
    score: float = Field(default=0.0)
    rationale: str = Field(default="")
    supports_browser: bool = Field(default=True)
    metadata: dict[str, Any] = Field(default_factory=dict)


class HarnessWorkerSpec(BaseModel):
    """worker assignment."""

    worker_id: str
    role: HarnessWorkerRole
    responsibility: str
    required_capability: str | None = None
    allowed_tools: list[str] = Field(default_factory=list)
    allowed_mcp_servers: list[str] = Field(default_factory=list)
    prompt_sources: list[str] = Field(default_factory=list)


class MemoryEntrySpec(BaseModel):
    """記憶項目仕様."""

    layer: MemoryLayer
    owner: MemoryOwner
    key: str
    description: str


class HarnessMemoryPlan(BaseModel):
    """タスク記憶設計."""

    entries: list[MemoryEntrySpec] = Field(default_factory=list)
    main_agent_keys: list[str] = Field(default_factory=list)
    shared_task_keys: list[str] = Field(default_factory=list)
    subagent_private_keys: dict[str, list[str]] = Field(default_factory=dict)


class VerificationConstraint(BaseModel):
    """検証制約."""

    label: str
    constraint_type: str
    field: str
    value: Any | None = None


class HarnessVerificationSpec(BaseModel):
    """検証仕様."""

    strategy: VerificationStrategy = Field(default=VerificationStrategy.CONSTRAINT)
    completion_loop: str = Field(default="execute -> verify -> revise_or_replan")
    success_signals: list[str] = Field(default_factory=list)
    constraints: list[VerificationConstraint] = Field(default_factory=list)
    ownership_notes: list[str] = Field(default_factory=list)


class ProviderStrategySpec(BaseModel):
    """provider 発見戦略."""

    mode: ProviderDiscoveryMode = Field(default=ProviderDiscoveryMode.NONE)
    discovery_query: str = Field(default="")
    selected_domains: list[str] = Field(default_factory=list)
    candidate_limit: int = Field(default=5, ge=1)
    rationale: str = Field(default="")


class ToolContextSnapshot(BaseModel):
    """動的 tool / prompt / mcp スナップショット."""

    available_skills: list[str] = Field(default_factory=list)
    available_mcp_servers: list[str] = Field(default_factory=list)
    prompt_sources: list[str] = Field(default_factory=list)
    dynamic_artifact_policy: str = Field(default="")


class HarnessPlan(BaseModel):
    """タスク harness plan."""

    blueprint_id: str
    task_kind: str
    intent_summary: str
    goal: str
    execution_plan: dict[str, Any]
    workers: list[HarnessWorkerSpec] = Field(default_factory=list)
    memory_plan: HarnessMemoryPlan
    provider_strategy: ProviderStrategySpec
    verification: HarnessVerificationSpec
    tool_context: ToolContextSnapshot
    missing_inputs: list[str] = Field(default_factory=list)
    provider_candidates: list[ProviderCandidate] = Field(default_factory=list)
    memory_context: dict[str, Any] = Field(default_factory=dict)


@dataclass(frozen=True)
class TaskBlueprint:
    """再利用可能な task blueprint."""

    blueprint_id: str
    task_kind: str
    required_capability: str | None
    required_inputs: tuple[str, ...]
    provider_discovery_mode: ProviderDiscoveryMode
    prompt_sources: tuple[str, ...]
    success_signals: tuple[str, ...]
    dynamic_artifact_policy: str


class TaskBlueprintRegistry:
    """task blueprint registry."""

    _FLIGHT_BLUEPRINT = TaskBlueprint(
        blueprint_id="structured_monitoring.flight_watch",
        task_kind="structured_monitoring",
        required_capability="flight_watch",
        required_inputs=("origin", "destination", "depart_window", "return_window"),
        provider_discovery_mode=ProviderDiscoveryMode.DISCOVER_FIRST,
        prompt_sources=("system_policy", "task_blueprint", "runtime_tools", "user_context"),
        success_signals=("offers_found", "ranking_completed", "subscription_created_if_requested"),
        dynamic_artifact_policy="reuse existing agents first; create runtime artifact only when capability gap remains",
    )
    _GENERAL_BLUEPRINT = TaskBlueprint(
        blueprint_id="general.autonomous_assistant",
        task_kind="general_execution",
        required_capability=None,
        required_inputs=(),
        provider_discovery_mode=ProviderDiscoveryMode.NONE,
        prompt_sources=("system_policy", "runtime_tools", "user_context"),
        success_signals=("result_delivered",),
        dynamic_artifact_policy="route to coordinator or specialist; gap-fill only after discovery misses",
    )

    def resolve(
        self,
        *,
        message: str,
        required_capability: str | None,
    ) -> TaskBlueprint:
        """要求に対応する blueprint を返す."""
        lowered = message.lower()
        if required_capability == "flight_watch" or any(
            keyword in lowered for keyword in ("flight", "airfare", "ticket", "机票", "航空券")
        ):
            return self._FLIGHT_BLUEPRINT
        return self._GENERAL_BLUEPRINT


class TaskProviderDiscoveryService:
    """適切な外部 provider / website を先に発見するサービス."""

    def __init__(self, skill_gateway: SkillGateway | None = None) -> None:
        """初期化."""
        self._gateway = skill_gateway

    async def discover(
        self,
        *,
        query: str,
        task_kind: str,
        keywords: list[str] | None = None,
        candidate_limit: int = 5,
    ) -> list[ProviderCandidate]:
        """候補 website を検索し、ドメイン単位で正規化する."""
        if self._gateway is None:
            return []
        try:
            result = await self._gateway.call("web_search", {"query": query})
        except Exception:
            return []
        if not result.success:
            return []

        items = self._extract_search_items(result.result)
        normalized: dict[str, ProviderCandidate] = {}
        for index, item in enumerate(items):
            candidate = self._to_candidate(
                item=item,
                index=index,
                task_kind=task_kind,
                keywords=keywords or [],
            )
            if candidate is None:
                continue
            current = normalized.get(candidate.domain)
            if current is None or candidate.score > current.score:
                normalized[candidate.domain] = candidate

        ranked = sorted(normalized.values(), key=lambda item: item.score, reverse=True)
        return ranked[:candidate_limit]

    @staticmethod
    def _extract_search_items(raw_result: Any) -> list[dict[str, Any]]:
        """検索結果を dict 配列へ正規化する."""
        if isinstance(raw_result, list):
            return [item for item in raw_result if isinstance(item, dict)]
        if isinstance(raw_result, dict):
            results = raw_result.get("results", raw_result.get("items", []))
            if isinstance(results, list):
                return [item for item in results if isinstance(item, dict)]
        return []

    def _to_candidate(
        self,
        *,
        item: dict[str, Any],
        index: int,
        task_kind: str,
        keywords: list[str],
    ) -> ProviderCandidate | None:
        """検索 item を candidate へ変換する."""
        url = str(item.get("url", item.get("link", ""))).strip()
        if not url:
            return None
        parsed = urlparse(url)
        domain = parsed.netloc.lower().strip()
        if not domain:
            return None
        title = str(item.get("title", item.get("name", ""))).strip()
        snippet = " ".join(
            str(item.get(key, "")).strip() for key in ("snippet", "content", "description") if item.get(key)
        ).strip()
        score = self._score_candidate(
            domain=domain,
            title=title,
            snippet=snippet,
            task_kind=task_kind,
            keywords=keywords,
            order=index,
            secure=parsed.scheme == "https",
        )
        rationale_parts = [f"search_rank={index + 1}"]
        if parsed.scheme == "https":
            rationale_parts.append("https")
        if keywords:
            matched = [kw for kw in keywords if kw.lower() in f"{title} {snippet} {domain}".lower()]
            if matched:
                rationale_parts.append(f"keyword_match={','.join(matched[:3])}")
        return ProviderCandidate(
            domain=domain,
            url=url,
            title=title,
            snippet=snippet,
            score=round(score, 4),
            rationale="; ".join(rationale_parts),
            supports_browser=parsed.scheme in {"http", "https"},
            metadata={"task_kind": task_kind},
        )

    @staticmethod
    def _score_candidate(
        *,
        domain: str,
        title: str,
        snippet: str,
        task_kind: str,
        keywords: list[str],
        order: int,
        secure: bool,
    ) -> float:
        """候補をスコアリングする."""
        haystack = f"{domain} {title} {snippet}".lower()
        score = max(0.0, 1.0 - (order * 0.08))
        if secure:
            score += 0.05
        if task_kind == "structured_monitoring":
            for task_keyword in ("flight", "airfare", "travel", "ticket", "fare"):
                if task_keyword in haystack:
                    score += 0.06
        for keyword in keywords:
            if keyword.lower() in haystack:
                score += 0.08
        if domain.count(".") >= 1:
            score += 0.02
        return score


class TaskHarnessVerifier:
    """harness 用の結果検証ラッパー."""

    def __init__(self) -> None:
        """初期化."""
        self._verifier = ResultVerifier(default_strategy=VerificationStrategy.CONSTRAINT)

    async def verify(
        self,
        *,
        goal: str,
        result: dict[str, Any],
        spec: HarnessVerificationSpec,
    ) -> VerificationResult:
        """定義済み制約に基づいて結果を検証する."""
        expected = {
            "constraints": [
                {
                    "type": constraint.constraint_type,
                    "field": constraint.field,
                    "value": constraint.value,
                }
                for constraint in spec.constraints
            ]
        }
        return await self._verifier.verify(
            goal=goal,
            result=result,
            expected=expected,
            strategy=spec.strategy,
        )


class TaskHarnessPlanner:
    """共通 harness planning."""

    def __init__(
        self,
        *,
        skill_gateway: SkillGateway | None = None,
        mcp_manager: MCPManager | None = None,
    ) -> None:
        """初期化."""
        self._gateway = skill_gateway
        self._mcp_manager = mcp_manager
        self._registry = TaskBlueprintRegistry()

    async def build_plan(
        self,
        *,
        message: str,
        required_capability: str | None,
        input_data: dict[str, Any] | None = None,
        partial_request: dict[str, Any] | None = None,
    ) -> HarnessPlan:
        """message を generic harness plan へ変換する."""
        payload = input_data or {}
        blueprint = self._registry.resolve(
            message=message,
            required_capability=required_capability,
        )
        missing_inputs = self._resolve_missing_inputs(blueprint, partial_request)
        create_watch = bool(payload.get("request", {}).get("create_watch")) if isinstance(payload.get("request"), dict) else False
        execution_plan = self._build_execution_plan(
            blueprint=blueprint,
            goal=message,
            missing_inputs=missing_inputs,
            create_watch=create_watch,
        )
        tool_context = ToolContextSnapshot(
            available_skills=self._available_skill_names(),
            available_mcp_servers=self._available_mcp_server_names(),
            prompt_sources=list(blueprint.prompt_sources),
            dynamic_artifact_policy=blueprint.dynamic_artifact_policy,
        )
        return HarnessPlan(
            blueprint_id=blueprint.blueprint_id,
            task_kind=blueprint.task_kind,
            intent_summary=self._summarize_intent(message=message, blueprint=blueprint),
            goal=message,
            execution_plan=execution_plan.model_dump(mode="json"),
            workers=self._build_workers(
                blueprint=blueprint,
                tool_context=tool_context,
                create_watch=create_watch,
            ),
            memory_plan=self._build_memory_plan(blueprint=blueprint, create_watch=create_watch),
            provider_strategy=self._build_provider_strategy(
                blueprint=blueprint,
                partial_request=partial_request or {},
            ),
            verification=self._build_verification_spec(blueprint=blueprint, create_watch=create_watch),
            tool_context=tool_context,
            missing_inputs=missing_inputs,
        )

    def attach_provider_candidates(
        self,
        plan: HarnessPlan,
        candidates: list[ProviderCandidate],
    ) -> HarnessPlan:
        """provider candidates を plan に反映する."""
        plan.provider_candidates = candidates
        plan.provider_strategy.selected_domains = [candidate.domain for candidate in candidates[:3]]
        return plan

    @staticmethod
    def _resolve_missing_inputs(
        blueprint: TaskBlueprint,
        partial_request: dict[str, Any] | None,
    ) -> list[str]:
        """不足入力を推定する."""
        if partial_request is None:
            return list(blueprint.required_inputs)
        return [field_name for field_name in blueprint.required_inputs if field_name not in partial_request]

    def _available_skill_names(self) -> list[str]:
        """利用可能 skill 名一覧."""
        if self._gateway is None:
            return []
        return sorted(skill.name for skill in self._gateway.list_available_skills())

    def _available_mcp_server_names(self) -> list[str]:
        """利用可能 MCP 名一覧."""
        if self._mcp_manager is None:
            return []
        names: list[str] = []
        for server in self._mcp_manager.list_servers():
            name = str(server.get("name", "")).strip()
            if name:
                names.append(name)
        return sorted(names)

    @staticmethod
    def _summarize_intent(message: str, blueprint: TaskBlueprint) -> str:
        """intent summary を返す."""
        if blueprint.blueprint_id == "structured_monitoring.flight_watch":
            return "ユーザー条件を補完し、適切な flight provider を探して比較・監視する"
        return f"ユーザー要求を {blueprint.task_kind} として実行する"

    @staticmethod
    def _build_execution_plan(
        *,
        blueprint: TaskBlueprint,
        goal: str,
        missing_inputs: list[str],
        create_watch: bool,
    ) -> ExecutionPlan:
        """harness 向け実行計画."""
        steps: list[PlanStep] = [
            PlanStep(
                name="意図分析",
                description=f"要求を分析して task blueprint を確定する: {goal}",
                step_type=StepType.LLM_GENERATION,
                order=0,
            ),
            PlanStep(
                name="安全確認",
                description="許可された tool / mcp / scope を確定する",
                step_type=StepType.CONDITIONAL,
                order=1,
            ),
        ]
        steps[1].dependencies = [steps[0].id]

        if missing_inputs:
            clarification_step = PlanStep(
                name="補足要求",
                description="不足入力を補足して同一 task を再開する",
                step_type=StepType.HUMAN_INPUT,
                order=len(steps),
                expected_output={"missing_inputs": missing_inputs},
            )
            clarification_step.dependencies = [steps[-1].id]
            steps.append(clarification_step)
        else:
            provider_step = PlanStep(
                name="provider discovery",
                description="先に適切な website/provider を見つける",
                step_type=StepType.TOOL_CALL,
                order=len(steps),
            )
            provider_step.dependencies = [steps[-1].id]
            steps.append(provider_step)

            route_step = PlanStep(
                name="agent routing",
                description="既存 agent を優先し、不足時は runtime artifact を生成する",
                step_type=StepType.CONDITIONAL,
                order=len(steps),
            )
            route_step.dependencies = [steps[-1].id]
            steps.append(route_step)

            execute_step = PlanStep(
                name="専門実行",
                description="選定した specialist に実処理を委譲する",
                step_type=StepType.SUB_PLAN,
                order=len(steps),
            )
            execute_step.dependencies = [steps[-1].id]
            steps.append(execute_step)

            verify_step = PlanStep(
                name="目標検証",
                description="結果が goal / constraints を満たしたか検証する",
                step_type=StepType.LLM_GENERATION,
                order=len(steps),
            )
            verify_step.dependencies = [steps[-1].id]
            steps.append(verify_step)

            if create_watch:
                monitor_step = PlanStep(
                    name="監視設定",
                    description="監視ジョブと通知チャネルを有効化する",
                    step_type=StepType.SEQUENTIAL,
                    order=len(steps),
                )
                monitor_step.dependencies = [steps[-1].id]
                steps.append(monitor_step)

        return ExecutionPlan(
            name=f"{blueprint.task_kind} harness",
            description=f"Blueprint: {blueprint.blueprint_id}",
            goal=goal,
            steps=steps,
            context={"blueprint_id": blueprint.blueprint_id},
        )

    @staticmethod
    def _build_workers(
        *,
        blueprint: TaskBlueprint,
        tool_context: ToolContextSnapshot,
        create_watch: bool,
    ) -> list[HarnessWorkerSpec]:
        """worker assignments を構築する."""
        workers = [
            HarnessWorkerSpec(
                worker_id="main_orchestrator",
                role=HarnessWorkerRole.MAIN,
                responsibility="意図分析、計画、補足、safety gating、completion judge を担当する",
                required_capability="coordination",
                allowed_tools=[],
                allowed_mcp_servers=tool_context.available_mcp_servers,
                prompt_sources=list(blueprint.prompt_sources),
            ),
            HarnessWorkerSpec(
                worker_id="result_verifier",
                role=HarnessWorkerRole.VERIFIER,
                responsibility="goal/constraint に基づいて accept or replan を判断する",
                required_capability="verification",
                allowed_tools=[],
                allowed_mcp_servers=[],
                prompt_sources=["verification_spec", "task_result"],
            ),
        ]
        if blueprint.required_capability:
            specialist_tools = [
                tool_name
                for tool_name in tool_context.available_skills
                if tool_name.startswith(("browser_", "web_", "read_", "http_"))
            ]
            workers.append(
                HarnessWorkerSpec(
                    worker_id=f"specialist_{blueprint.required_capability}",
                    role=HarnessWorkerRole.SPECIALIST,
                    responsibility="domain-specific execution と provider interaction を担当する",
                    required_capability=blueprint.required_capability,
                    allowed_tools=specialist_tools,
                    allowed_mcp_servers=tool_context.available_mcp_servers,
                    prompt_sources=["task_blueprint", "provider_strategy", "shared_task_memory"],
                )
            )
        if create_watch:
            workers.append(
                HarnessWorkerSpec(
                    worker_id="monitor_worker",
                    role=HarnessWorkerRole.MONITOR,
                    responsibility="長時間監視、再実行、通知送信を担当する",
                    required_capability="monitoring",
                    allowed_tools=["web_search", "browser_navigate", "browser_get_text"],
                    allowed_mcp_servers=tool_context.available_mcp_servers,
                    prompt_sources=["watch_policy", "baseline_snapshot", "notification_targets"],
                )
            )
        return workers

    @staticmethod
    def _build_memory_plan(
        *,
        blueprint: TaskBlueprint,
        create_watch: bool,
    ) -> HarnessMemoryPlan:
        """main / shared / subagent の記憶分離を定義する."""
        entries = [
            MemoryEntrySpec(
                layer=MemoryLayer.SENSORY,
                owner=MemoryOwner.MAIN_AGENT,
                key="raw_user_request",
                description="ユーザーの原文入力と補足入力を保存する",
            ),
            MemoryEntrySpec(
                layer=MemoryLayer.WORKING,
                owner=MemoryOwner.MAIN_AGENT,
                key="task_plan",
                description="blueprint、worker assignments、tool policy を保持する",
            ),
            MemoryEntrySpec(
                layer=MemoryLayer.WORKING,
                owner=MemoryOwner.SHARED_TASK,
                key="provider_candidates",
                description="発見した website/provider と選定理由を共有する",
            ),
            MemoryEntrySpec(
                layer=MemoryLayer.EPISODIC,
                owner=MemoryOwner.SHARED_TASK,
                key="task_episode",
                description="実行結果、検証結果、再計画理由の要約を保持する",
            ),
            MemoryEntrySpec(
                layer=MemoryLayer.LONG_TERM,
                owner=MemoryOwner.MAIN_AGENT,
                key="user_preferences",
                description="ユーザーの通知先や選好条件など再利用可能な記憶を保持する",
            ),
            MemoryEntrySpec(
                layer=MemoryLayer.WORKING,
                owner=MemoryOwner.SUBAGENT_PRIVATE,
                key="specialist_scratchpad",
                description="subagent の一時推論メモ。main agent へは要約のみ返す",
            ),
        ]
        if create_watch:
            entries.append(
                MemoryEntrySpec(
                    layer=MemoryLayer.LONG_TERM,
                    owner=MemoryOwner.SHARED_TASK,
                    key="monitor_baseline",
                    description="監視ベースライン価格と前回通知状態を保持する",
                )
            )
        return HarnessMemoryPlan(
            entries=entries,
            main_agent_keys=["raw_user_request", "task_plan", "user_preferences"],
            shared_task_keys=["provider_candidates", "task_episode", "monitor_baseline"],
            subagent_private_keys={"specialist": ["specialist_scratchpad"]},
        )

    @staticmethod
    def _build_provider_strategy(
        *,
        blueprint: TaskBlueprint,
        partial_request: dict[str, Any],
    ) -> ProviderStrategySpec:
        """provider strategy を返す."""
        if blueprint.provider_discovery_mode == ProviderDiscoveryMode.NONE:
            return ProviderStrategySpec(
                mode=ProviderDiscoveryMode.NONE,
                rationale="fixed provider discovery is unnecessary for this blueprint",
            )
        origin = str(partial_request.get("origin", "")).strip().upper()
        destination = str(partial_request.get("destination", "")).strip().upper()
        discovery_query = "best websites to compare round trip flight prices"
        if origin and destination:
            discovery_query = f"best websites to compare round trip flight prices {origin} {destination}"
        return ProviderStrategySpec(
            mode=ProviderDiscoveryMode.DISCOVER_FIRST,
            discovery_query=discovery_query,
            candidate_limit=5,
            rationale="固定サイトを前提にせず、先に比較に適した website を見つけてから利用する",
        )

    @staticmethod
    def _build_verification_spec(
        *,
        blueprint: TaskBlueprint,
        create_watch: bool,
    ) -> HarnessVerificationSpec:
        """completion criteria を返す."""
        constraints = [
            VerificationConstraint(
                label="result_present",
                constraint_type="required",
                field="search_result",
            ),
            VerificationConstraint(
                label="offers_found",
                constraint_type="min",
                field="offers_found",
                value=1,
            ),
        ]
        if create_watch:
            constraints.append(
                VerificationConstraint(
                    label="watch_created",
                    constraint_type="required",
                    field="subscription",
                )
            )
        return HarnessVerificationSpec(
            strategy=VerificationStrategy.CONSTRAINT,
            completion_loop="execute -> verify -> replan_or_gap_fill when constraints fail",
            success_signals=list(blueprint.success_signals),
            constraints=constraints,
            ownership_notes=[
                "main agent は verification と final judgment を担当する",
                "subagent は raw scratchpad ではなく要約済み成果のみ main agent に返す",
            ],
        )
