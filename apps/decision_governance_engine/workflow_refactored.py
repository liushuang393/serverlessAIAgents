# -*- coding: utf-8 -*-
"""Decision Governance Engine - リファクタリング済みWorkflow.

重構されたワークフロー:
- AgentPipeline による自動進捗追跡
- AgentRegistry によるYAML駆動のAgent管理
- ReportGenerator による報告書生成の分離
- SSEFlowRunner によるSSE配信の分離

このファイルは元の workflow.py (~1269行) を ~200行に削減したものです。

使用例:
    >>> from apps.decision_governance_engine.workflow_refactored import DecisionWorkflow
    >>> 
    >>> workflow = DecisionWorkflow()
    >>> await workflow.initialize()
    >>> 
    >>> # 同期処理
    >>> report = await workflow.process(request)
    >>> 
    >>> # SSEストリーム処理
    >>> async for event in workflow.process_with_events(request):
    ...     yield event.to_sse()
"""

import logging
import time
import uuid
from collections.abc import AsyncIterator
from typing import Any

from agentflow.core.result_store import ResultStoreManager
from agentflow.integrations.sse_flow_runner import SSEFlowRunner
from agentflow.patterns.agent_pipeline import (
    AgentConfig,
    AgentPipeline,
    PipelineConfig,
    RevisionRequest,
)
from agentflow.patterns.progress_emitter import AgentMeta
from agentflow.protocols.agui_events import (
    AGUIEvent,
    ClarificationQuestion,
    ClarificationRequiredEvent,
    FlowCompleteEvent,
    FlowErrorEvent,
    FlowStartEvent,
)

from apps.decision_governance_engine.schemas.agent_schemas import (
    ClarificationInput,
    ClarificationOutput,
    DaoInput,
    DaoOutput,
    FaInput,
    FaOutput,
    GatekeeperInput,
    QiInput,
    QiOutput,
    ReviewInput,
    ReviewVerdict,
    ShuInput,
    ShuOutput,
)
from apps.decision_governance_engine.schemas.input_schemas import DecisionRequest
from apps.decision_governance_engine.schemas.output_schemas import DecisionReport
from apps.decision_governance_engine.services.agent_registry import AgentRegistry
from apps.decision_governance_engine.services.report_generator import ReportGenerator


class DecisionWorkflow:
    """Decision Governance Engine - リファクタリング済みワークフロー.

    設計原則:
    - 単一責任: ワークフロー編排のみ担当
    - 依存注入: AgentRegistry, ReportGenerator を注入
    - 分離: SSE配信は SSEFlowRunner に委譲

    Attributes:
        registry: AgentRegistry インスタンス
        report_generator: ReportGenerator インスタンス
        pipeline: AgentPipeline インスタンス

    使用例:
        >>> workflow = DecisionWorkflow()
        >>> await workflow.initialize()
        >>> report = await workflow.process(request)
    """

    # REVISE回退設定
    MAX_REVISIONS = 2

    def __init__(
        self,
        llm_client: Any = None,
        enable_rag: bool = True,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント（オプション）
            enable_rag: RAG機能を有効化するか
        """
        self._logger = logging.getLogger("decision_engine.workflow")
        self._llm_client = llm_client
        self._enable_rag = enable_rag
        self._initialized = False

        # サービス（遅延初期化）
        self._registry: AgentRegistry | None = None
        self._report_generator: ReportGenerator | None = None
        self._pipeline: AgentPipeline | None = None

        # 実行時状態
        self._flow_id: str | None = None
        self._context: dict[str, Any] = {}

    async def initialize(self) -> None:
        """ワークフローを初期化.

        AgentRegistryからAgent定義を読み込み、パイプラインを構築。
        """
        if self._initialized:
            return

        self._logger.info("Initializing DecisionWorkflow...")

        # AgentRegistry 初期化
        self._registry = AgentRegistry(
            llm_client=self._llm_client,
            enable_rag=self._enable_rag,
        )
        await self._registry.initialize()

        # RAG初期化（Shu/Qi用）
        await self._registry.initialize_rag_agents()

        # ReportGenerator 初期化
        self._report_generator = ReportGenerator()

        # パイプライン構築（ゲートAgent以外のコアAgent）
        self._build_pipeline()

        self._initialized = True
        self._logger.info(
            f"DecisionWorkflow initialized with {self._registry.total_agents} agents"
        )

    def _build_pipeline(self) -> None:
        """AgentPipeline を構築."""
        # YAML定義からAgentConfigを生成
        configs: list[AgentConfig] = []
        for agent_def in self._registry.get_agent_definitions():
            agent = self._registry.get_agent(agent_def.id)
            config = AgentConfig(
                agent=agent,
                id=agent_def.id,
                name=agent_def.name,
                label=agent_def.label,
                icon=agent_def.icon,
                is_gate=agent_def.is_gate,
            )
            configs.append(config)

        self._pipeline = AgentPipeline(
            agents=configs,
            config=PipelineConfig(
                flow_id=self._registry.flow_id,
                max_revisions=self.MAX_REVISIONS,
            ),
        )

    def _ensure_initialized(self) -> None:
        """初期化されていることを確認."""
        if not self._initialized:
            raise RuntimeError(
                "DecisionWorkflow not initialized. Call initialize() first."
            )

    async def process(self, request: DecisionRequest | str) -> DecisionReport | dict:
        """意思決定プロセスを実行（同期）.

        Args:
            request: 意思決定依頼

        Returns:
            DecisionReport または拒否時のエラー情報
        """
        self._ensure_initialized()

        # 文字列の場合はDecisionRequestに変換
        if isinstance(request, str):
            request = DecisionRequest(question=request)

        self._logger.info(f"Processing: {request.question[:50]}...")
        self._context.clear()

        # Step 0: CognitiveGate
        cognitive_result = await self._run_agent("cognitive_gate", {
            "raw_question": request.question,
            "constraints": [
                *request.constraints.technical,
                *request.constraints.regulatory,
            ],
        })
        if not cognitive_result.get("proceed", True):
            return self._build_cognitive_gate_response(cognitive_result)

        # Step 1: Gatekeeper
        gatekeeper_result = await self._run_agent("gatekeeper", {
            "raw_question": request.question,
        })
        if not gatekeeper_result.get("is_acceptable", False):
            return self._build_rejection_response(gatekeeper_result)

        # Step 2: Clarification
        clarification_result = await self._run_agent("clarification", {
            "raw_question": request.question,
            "constraints": self._build_constraints_list(request),
        })
        self._context["clarification_result"] = clarification_result

        # Step 3-7: Core Agents + Review（REVISEループ）
        for revision in range(self.MAX_REVISIONS + 1):
            results = await self._run_core_agents(request, clarification_result)
            review_result = await self._run_review(results)
            results["review"] = review_result

            verdict = review_result.get("overall_verdict", "PASS")

            if verdict == ReviewVerdict.PASS.value or verdict == ReviewVerdict.PASS:
                return self._report_generator.generate(
                    results=results,
                    original_question=request.question,
                    clarification_result=clarification_result,
                )

            if verdict == ReviewVerdict.REJECT.value or verdict == ReviewVerdict.REJECT:
                return self._build_review_rejection(review_result)

            # REVISE - 回退
            if revision < self.MAX_REVISIONS:
                self._context["revision_round"] = revision + 1
                self._context["revision_feedback"] = review_result.get("findings", [])

        return {
            "status": "max_revisions_reached",
            "message": f"最大リビジョン回数（{self.MAX_REVISIONS}回）に到達",
        }

    async def process_with_events(
        self, request: DecisionRequest | str
    ) -> AsyncIterator[AGUIEvent]:
        """SSEストリーム付きで意思決定プロセスを実行.

        Args:
            request: 意思決定依頼

        Yields:
            AGUIEvent: AG-UIプロトコル準拠イベント
        """
        self._ensure_initialized()

        # 文字列の場合はDecisionRequestに変換
        if isinstance(request, str):
            request = DecisionRequest(question=request)

        self._flow_id = f"decision-{uuid.uuid4().hex[:8]}"
        self._context.clear()

        # Flow開始
        yield FlowStartEvent(
            timestamp=time.time(),
            flow_id=self._flow_id,
            data={"question": request.question[:100]},
        )

        try:
            # CognitiveGate
            async for event in self._run_agent_with_events("cognitive_gate", {
                "raw_question": request.question,
                "constraints": [
                    *request.constraints.technical,
                    *request.constraints.regulatory,
                ],
            }):
                if event:
                    yield event

            cognitive_result = self._context.get("cognitive_gate")
            if not cognitive_result.get("proceed", True):
                yield self._build_clarification_event(request, cognitive_result)
                return

            # Gatekeeper
            async for event in self._run_agent_with_events("gatekeeper", {
                "raw_question": request.question,
            }):
                if event:
                    yield event

            gatekeeper_result = self._context.get("gatekeeper")
            if not gatekeeper_result.get("is_acceptable", False):
                yield FlowErrorEvent(
                    timestamp=time.time(),
                    flow_id=self._flow_id,
                    data={"reason": gatekeeper_result.get("rejection_reason")},
                    error_message=gatekeeper_result.get("rejection_message", "拒否"),
                    error_type="RejectedQuestion",
                )
                return

            # Clarification
            async for event in self._run_agent_with_events("clarification", {
                "raw_question": request.question,
                "constraints": self._build_constraints_list(request),
            }):
                if event:
                    yield event

            clarification_result = self._context.get("clarification")

            # Core Agents
            results = await self._run_core_agents(request, clarification_result)

            # Review
            async for event in self._run_agent_with_events("review", 
                self._build_review_input(results).model_dump()
            ):
                if event:
                    yield event

            review_result = self._context.get("review")
            results["review"] = review_result

            # レポート生成
            verdict = review_result.get("overall_verdict", "PASS")
            if verdict == ReviewVerdict.PASS.value or verdict == ReviewVerdict.PASS:
                report = self._report_generator.generate(
                    results=results,
                    original_question=request.question,
                    clarification_result=clarification_result,
                )
                report_data = report.model_dump(mode="json")

                # 結果保存
                await ResultStoreManager.save(
                    result_id=report.report_id,
                    data=report_data,
                    flow_id=self._registry.flow_id,
                    status="success",
                )

                yield FlowCompleteEvent(
                    timestamp=time.time(),
                    flow_id=self._flow_id,
                    data={"report_id": report.report_id},
                    result_id=report.report_id,
                    result=report_data,
                    include_result=True,
                )
            else:
                yield FlowErrorEvent(
                    timestamp=time.time(),
                    flow_id=self._flow_id,
                    data={"verdict": str(verdict)},
                    error_message=f"検証結果: {verdict}",
                    error_type="ReviewNotPassed",
                )

        except Exception as e:
            self._logger.error(f"Workflow error: {e}")
            yield FlowErrorEvent(
                timestamp=time.time(),
                flow_id=self._flow_id or "unknown",
                data={},
                error_message=str(e),
                error_type=type(e).__name__,
            )
            raise
        finally:
            self._flow_id = None

    # ========================================
    # Private Methods
    # ========================================

    async def _run_agent(self, agent_id: str, input_data: dict) -> dict:
        """指定Agentを実行."""
        agent = self._registry.get_agent(agent_id)
        result = await agent.run(input_data)
        self._context[agent_id] = result
        return result

    async def _run_agent_with_events(
        self, agent_id: str, input_data: dict
    ) -> AsyncIterator[AGUIEvent | None]:
        """Agent実行＋イベント発射."""
        from agentflow.protocols.agui_events import (
            LogEvent,
            NodeCompleteEvent,
            NodeStartEvent,
        )

        agent_def = self._registry.get_agent_definition(agent_id)
        if not agent_def:
            return

        now = time.time()

        # 開始イベント
        yield NodeStartEvent(
            timestamp=now,
            flow_id=self._flow_id,
            data={"label": agent_def.label},
            node_id=agent_id,
            node_name=agent_def.name,
        )
        yield LogEvent(
            timestamp=now,
            flow_id=self._flow_id,
            data={},
            level="INFO",
            message=f"{agent_def.name}({agent_def.label})の分析を開始...",
            source=agent_id,
        )

        # 実行
        result = await self._run_agent(agent_id, input_data)

        # 完了イベント
        yield NodeCompleteEvent(
            timestamp=time.time(),
            flow_id=self._flow_id,
            data={},
            node_id=agent_id,
            node_name=agent_def.name,
        )

    async def _run_core_agents(
        self, request: DecisionRequest, clarification_result: dict
    ) -> dict[str, dict]:
        """Core Agents（Dao→Fa→Shu→Qi）を実行."""
        results: dict[str, dict] = {}

        # Dao
        effective_question = clarification_result.get("refined_question") or request.question
        dao_input = DaoInput(
            question=effective_question,
            constraints=[
                *request.constraints.technical,
                *request.constraints.regulatory,
            ],
            stakeholders=request.constraints.human_resources,
            clarification_result=ClarificationOutput(**clarification_result) if clarification_result else None,
        )
        results["dao"] = await self._run_agent("dao", dao_input.model_dump())

        # Fa
        fa_input = FaInput(
            dao_result=DaoOutput(**results["dao"]),
            available_resources={
                "budget": request.constraints.budget.amount if request.constraints.budget else None,
                "team": request.constraints.human_resources,
            },
            time_horizon=f"{request.constraints.timeline.months}ヶ月" if request.constraints.timeline else "",
        )
        results["fa"] = await self._run_agent("fa", fa_input.model_dump())

        # Shu
        selected_path_id = results["fa"].get("recommended_paths", [{}])[0].get("path_id", "A")
        shu_input = ShuInput(
            fa_result=FaOutput(**results["fa"]),
            selected_path_id=selected_path_id,
        )
        results["shu"] = await self._run_agent("shu", shu_input.model_dump())

        # Qi
        qi_input = QiInput(
            shu_result=ShuOutput(**results["shu"]),
            tech_constraints=request.constraints.technical,
        )
        results["qi"] = await self._run_agent("qi", qi_input.model_dump())

        return results

    async def _run_review(self, results: dict[str, dict]) -> dict:
        """Review検証を実行."""
        review_input = self._build_review_input(results)
        return await self._run_agent("review", review_input.model_dump())

    def _build_review_input(self, results: dict[str, dict]) -> ReviewInput:
        """ReviewInput を構築."""
        return ReviewInput(
            dao_result=DaoOutput(**results["dao"]),
            fa_result=FaOutput(**results["fa"]),
            shu_result=ShuOutput(**results["shu"]),
            qi_result=QiOutput(**results["qi"]),
        )

    def _build_constraints_list(self, request: DecisionRequest) -> list[str]:
        """制約条件をリストに変換."""
        constraints: list[str] = []
        if request.constraints.budget:
            constraints.append(f"予算: {request.constraints.budget.amount}万円")
        if request.constraints.timeline:
            constraints.append(f"期限: {request.constraints.timeline.months}ヶ月")
        constraints.extend(request.constraints.technical)
        constraints.extend(request.constraints.regulatory)
        return constraints

    def _build_cognitive_gate_response(self, result: dict) -> dict:
        """CognitiveGate拒否レスポンスを構築."""
        return {
            "status": "cognitive_gate_blocked",
            "reason": "認知前処理で不足情報を検出",
            "missing_info": result.get("missing_info", []),
            "message": "分析を進める前に、以下の情報を明確にしてください。",
        }

    def _build_rejection_response(self, result: dict) -> dict:
        """Gatekeeper拒否レスポンスを構築."""
        return {
            "status": "rejected",
            "reason": result.get("rejection_reason"),
            "message": result.get("rejection_message"),
            "suggested_rephrase": result.get("suggested_rephrase"),
        }

    def _build_review_rejection(self, result: dict) -> dict:
        """Review却下レスポンスを構築."""
        return {
            "status": "rejected_by_review",
            "verdict": "REJECT",
            "findings": result.get("findings", []),
            "message": "検証の結果、計画に重大な問題があります。",
        }

    def _build_clarification_event(
        self, request: DecisionRequest, cognitive_result: dict
    ) -> ClarificationRequiredEvent:
        """補足要求イベントを構築."""
        questions = [
            ClarificationQuestion(
                id=f"q_{i}",
                text=q,
                type="text",
                required=True,
            )
            for i, q in enumerate(cognitive_result.get("clarification_questions", []))
        ]
        return ClarificationRequiredEvent(
            timestamp=time.time(),
            flow_id=self._flow_id,
            original_question=request.question,
            questions=questions,
            message="分析を進めるために、以下の情報を補足してください。",
            timeout_seconds=300,
        )


# 後方互換エイリアス
DecisionEngine = DecisionWorkflow


__all__ = [
    "DecisionEngine",
    "DecisionWorkflow",
]

