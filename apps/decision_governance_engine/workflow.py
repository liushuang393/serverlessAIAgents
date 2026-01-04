# -*- coding: utf-8 -*-
"""Decision Governance Engine - Workflow定義.

認知前処理・門番・診断・道・法・術・器・検証の8 Agentを協調させる。
状態遷移は順方向のみ、スキップ禁止。REVISE時は該当Agentに回退。

AG-UI連携: AGUIEventEmitter経由でSSE進捗配信対応。
ResultStore連携: フロー完了時に結果を自動保存。

アーキテクチャ:
- AgentRegistry による Agent 管理の一元化
- ReportGenerator による提案書生成の分離
- AgentPipeline による進捗追跡の自動化
- SSE イベント発射と業務ロジックの分離
"""

import logging
import time
import uuid
from collections.abc import AsyncIterator
from typing import Any

from agentflow.core.result_store import ResultStoreManager
from agentflow.patterns.multi_agent import SharedContext

from apps.decision_governance_engine.schemas.agent_schemas import (
    ClarificationInput,
    DaoInput,
    DaoOutput,
    FaInput,
    FaOutput,
    GatekeeperInput,
    QiInput,
    ReviewInput,
    ReviewVerdict,
    ShuInput,
    ShuOutput,
)
from apps.decision_governance_engine.schemas.input_schemas import DecisionRequest
from apps.decision_governance_engine.schemas.output_schemas import DecisionReport
from apps.decision_governance_engine.services.agent_registry import AgentRegistry
from apps.decision_governance_engine.services.report_generator import ReportGenerator

# AG-UI Events
try:
    from agentflow.protocols.agui_events import (
        AGUIEvent,
        ClarificationQuestion,
        ClarificationRequiredEvent,
        FlowCompleteEvent,
        FlowErrorEvent,
        FlowStartEvent,
    )
    AGUI_AVAILABLE = True
except ImportError:
    AGUI_AVAILABLE = False


class DecisionEngine:
    """Decision Governance Engine メインクラス.

    AgentRegistry を使用して8つのAgentを順次実行し、
    署名可能な決策レポートを生成。

    使用例:
        >>> engine = DecisionEngine()
        >>> result = await engine.process("新規事業への投資判断をしたい")
        >>> # SSEストリーム付き処理
        >>> async for event in engine.process_with_events(request):
        ...     print(event.event_type, event.data)
    """

    MAX_REVISIONS = 2

    def __init__(self, llm_client: Any = None, enable_rag: bool = True) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント（オプション、Noneの場合は自動取得）
            enable_rag: RAG機能を有効化するか
        """
        self._logger = logging.getLogger("decision_engine")
        self._registry = AgentRegistry(llm_client=llm_client, enable_rag=enable_rag)
        self._report_generator = ReportGenerator()
        self._context = SharedContext(enable_memory=True)
        self._flow_id: str | None = None

    async def process(self, request: DecisionRequest | str) -> DecisionReport | dict:
        """意思決定プロセスを実行.

        Args:
            request: 意思決定依頼（文字列またはDecisionRequest）

        Returns:
            DecisionReport または拒否時のエラー情報
        """
        if isinstance(request, str):
            request = DecisionRequest(question=request)

        self._logger.info(f"Decision process started: {request.question[:50]}...")
        self._context.clear()

        # レジストリ初期化
        await self._registry.initialize()
        await self._registry.initialize_rag_agents()

        # Step 0: CognitiveGate（認知前処理）
        cognitive_result = await self._run_cognitive_gate(request)
        if not cognitive_result.get("proceed", True):
            return self._create_cognitive_gate_blocked_response(cognitive_result)

        # Step 1: Gatekeeper検証
        gatekeeper_result = await self._run_gatekeeper(request)
        if not gatekeeper_result.get("is_acceptable", False):
            return self._create_rejected_response(gatekeeper_result)

        # Step 2: Clarification（問題診断）
        clarification_result = await self._run_clarification(request)

        # Step 3-7: Core Agents + Review（REVISEループ付き）
        for revision in range(self.MAX_REVISIONS + 1):
            results = await self._run_core_agents(request, clarification_result)
            review_result = await self._run_review(results)
            results["review"] = review_result

            verdict = review_result.get("overall_verdict", "PASS")

            if verdict == ReviewVerdict.PASS.value or verdict == ReviewVerdict.PASS:
                return self._report_generator.generate(
                    results,
                    original_question=request.question,
                    clarification_result=clarification_result,
                )

            if verdict == ReviewVerdict.REJECT.value or verdict == ReviewVerdict.REJECT:
                return self._create_review_rejected_response(review_result)

            # REVISE処理
            if revision < self.MAX_REVISIONS:
                self._setup_revision_context(revision, review_result)

        return self._create_max_revisions_response(review_result)

    async def process_with_events(
        self, request: DecisionRequest | str
    ) -> AsyncIterator[Any]:
        """SSEストリーム付きで意思決定プロセスを実行.

        Args:
            request: 意思決定依頼

        Yields:
            AGUIEvent: AG-UIプロトコル準拠イベント
        """
        if not AGUI_AVAILABLE:
            result = await self.process(request)
            yield {"type": "result", "data": result}
            return

        self._flow_id = f"decision-{uuid.uuid4().hex[:8]}"
        if isinstance(request, str):
            request = DecisionRequest(question=request)

        # Flow開始
        yield FlowStartEvent(
            timestamp=time.time(),
            flow_id=self._flow_id,
            data={"question": request.question[:100]},
        )

        # レジストリ初期化
        await self._registry.initialize()
        await self._registry.initialize_rag_agents()

        try:
            # CognitiveGate
            async for event in self._run_agent_with_events(
                "cognitive_gate", self._run_cognitive_gate, request
            ):
                yield event
            cognitive_result = self._context.get("cognitive_gate_result", {})

            if not cognitive_result.get("proceed", True):
                async for event in self._handle_cognitive_gate_blocked(
                    request, cognitive_result
                ):
                    yield event
                return

            # Gatekeeper
            async for event in self._run_agent_with_events(
                "gatekeeper", self._run_gatekeeper, request
            ):
                yield event
            gatekeeper_result = self._context.get("gatekeeper_result", {})

            if not gatekeeper_result.get("is_acceptable", False):
                yield self._create_flow_error_event(gatekeeper_result)
                return

            # Clarification
            async for event in self._run_agent_with_events(
                "clarification", self._run_clarification, request
            ):
                yield event
            clarification_result = self._context.get("clarification_result", {})

            # Core Agents
            results: dict[str, dict] = {}
            for agent_id in ["dao", "fa", "shu", "qi"]:
                async for event in self._run_core_agent_with_events(
                    agent_id, request, clarification_result, results
                ):
                    yield event

            # Review
            async for event in self._run_agent_with_events(
                "review", self._run_review, results
            ):
                yield event
            review_result = self._context.get("review_result", {})
            results["review"] = review_result

            # 完了処理
            verdict = review_result.get("overall_verdict", "PASS")
            if verdict == ReviewVerdict.PASS.value or verdict == ReviewVerdict.PASS:
                report = self._report_generator.generate(
                    results,
                    original_question=request.question,
                    clarification_result=clarification_result,
                )

                report_data = report.model_dump(mode="json")
                await ResultStoreManager.save(
                    result_id=report.report_id,
                    data=report_data,
                    flow_id="decision-governance-engine",
                    status="success",
                    metadata={"question": request.question},
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
    # Agent実行メソッド
    # ========================================

    async def _run_cognitive_gate(self, request: DecisionRequest) -> dict:
        """CognitiveGateを実行."""
        agent = self._registry.get_agent("cognitive_gate")
        cognitive_input = {
            "raw_question": request.question,
            "constraints": [
                *request.constraints.technical,
                *request.constraints.regulatory,
            ],
        }
        result = await agent.run(cognitive_input)
        self._context.set("cognitive_gate_result", result)
        return result

    async def _run_gatekeeper(self, request: DecisionRequest) -> dict:
        """Gatekeeperを実行."""
        agent = self._registry.get_agent("gatekeeper")
        gatekeeper_input = GatekeeperInput(raw_question=request.question)
        result = await agent.run(gatekeeper_input.model_dump())
        self._context.set("gatekeeper_result", result)
        return result

    async def _run_clarification(self, request: DecisionRequest) -> dict:
        """Clarificationを実行."""
        agent = self._registry.get_agent("clarification")
        constraints = self._build_constraints_list(request)
        clarification_input = ClarificationInput(
            raw_question=request.question,
            constraints=constraints,
        )
        result = await agent.run(clarification_input.model_dump())
        self._context.set("clarification_result", result)
        return result

    async def _run_core_agents(
        self,
        request: DecisionRequest,
        clarification_result: dict,
    ) -> dict[str, dict]:
        """Core Agents（Dao→Fa→Shu→Qi）を実行."""
        results: dict[str, dict] = {}

        # Dao
        effective_question = clarification_result.get(
            "refined_question", request.question
        )
        dao_agent = self._registry.get_agent("dao")
        dao_input = DaoInput(
            question=effective_question,
            constraints=[
                *request.constraints.technical,
                *request.constraints.regulatory,
            ],
            stakeholders=request.constraints.human_resources,
        )
        dao_result = await dao_agent.run(dao_input.model_dump())
        self._context.set("dao_result", dao_result)
        results["dao"] = dao_result

        # Fa
        fa_agent = self._registry.get_agent("fa")
        fa_input = FaInput(
            dao_result=DaoOutput(**dao_result),
            available_resources={
                "budget": request.constraints.budget.amount
                if request.constraints.budget
                else None,
                "team": request.constraints.human_resources,
            },
            time_horizon=f"{request.constraints.timeline.months}ヶ月"
            if request.constraints.timeline
            else "",
        )
        fa_result = await fa_agent.run(fa_input.model_dump())
        self._context.set("fa_result", fa_result)
        results["fa"] = fa_result

        # Shu
        shu_agent = self._registry.get_agent("shu")
        selected_path_id = fa_result.get("recommended_paths", [{}])[0].get(
            "path_id", "A"
        )
        shu_input = ShuInput(
            fa_result=FaOutput(**fa_result),
            selected_path_id=selected_path_id,
        )
        shu_result = await shu_agent.run(shu_input.model_dump())
        self._context.set("shu_result", shu_result)
        results["shu"] = shu_result

        # Qi
        qi_agent = self._registry.get_agent("qi")
        qi_input = QiInput(
            shu_result=ShuOutput(**shu_result),
            tech_constraints=request.constraints.technical,
        )
        qi_result = await qi_agent.run(qi_input.model_dump())
        self._context.set("qi_result", qi_result)
        results["qi"] = qi_result

        return results

    async def _run_review(self, results: dict[str, dict]) -> dict:
        """Reviewを実行."""
        agent = self._registry.get_agent("review")
        review_input = ReviewInput(
            dao_result=DaoOutput(**results["dao"]),
            fa_result=FaOutput(**results["fa"]),
            shu_result=ShuOutput(**results["shu"]),
            qi_result=results["qi"],
        )
        review_result = await agent.run(review_input.model_dump())
        self._context.set("review_result", review_result)
        return review_result

    # ========================================
    # イベント発射ヘルパー
    # ========================================

    async def _run_agent_with_events(
        self,
        agent_id: str,
        run_func: Any,
        *args: Any,
    ) -> AsyncIterator[AGUIEvent]:
        """Agent実行をイベント付きで実行."""
        from agentflow.protocols.agui_events import (
            LogEvent,
            NodeCompleteEvent,
            NodeStartEvent,
        )

        agent_def = self._registry.get_agent_definition(agent_id)
        if not agent_def:
            return

        # 開始イベント
        yield NodeStartEvent(
            timestamp=time.time(),
            flow_id=self._flow_id,
            data={"label": agent_def.label},
            node_id=agent_id,
            node_name=agent_def.name,
        )
        yield LogEvent(
            timestamp=time.time(),
            flow_id=self._flow_id,
            data={},
            level="INFO",
            message=f"{agent_def.name}({agent_def.label})の分析を開始...",
            source=agent_id,
        )

        # Agent実行
        result = await run_func(*args)

        # 完了イベント
        yield NodeCompleteEvent(
            timestamp=time.time(),
            flow_id=self._flow_id,
            data={"result_summary": self._extract_result_summary(agent_id, result)},
            node_id=agent_id,
            node_name=agent_def.name,
        )

    async def _run_core_agent_with_events(
        self,
        agent_id: str,
        request: DecisionRequest,
        clarification_result: dict,
        results: dict[str, dict],
    ) -> AsyncIterator[AGUIEvent]:
        """Core Agentをイベント付きで実行."""
        from agentflow.protocols.agui_events import (
            NodeCompleteEvent,
            NodeStartEvent,
        )

        agent_def = self._registry.get_agent_definition(agent_id)
        if not agent_def:
            return

        yield NodeStartEvent(
            timestamp=time.time(),
            flow_id=self._flow_id,
            data={"label": agent_def.label},
            node_id=agent_id,
            node_name=agent_def.name,
        )

        # Agent実行
        result = await self._run_single_core_agent(
            agent_id, request, clarification_result, results
        )
        results[agent_id] = result

        yield NodeCompleteEvent(
            timestamp=time.time(),
            flow_id=self._flow_id,
            data={"result_summary": self._extract_result_summary(agent_id, result)},
            node_id=agent_id,
            node_name=agent_def.name,
        )

    async def _run_single_core_agent(
        self,
        agent_id: str,
        request: DecisionRequest,
        clarification_result: dict,
        results: dict[str, dict],
    ) -> dict:
        """単一のCore Agentを実行."""
        agent = self._registry.get_agent(agent_id)

        if agent_id == "dao":
            effective_question = clarification_result.get(
                "refined_question", request.question
            )
            dao_input = DaoInput(
                question=effective_question,
                constraints=[
                    *request.constraints.technical,
                    *request.constraints.regulatory,
                ],
                stakeholders=request.constraints.human_resources,
            )
            result = await agent.run(dao_input.model_dump())
            self._context.set("dao_result", result)
            return result

        elif agent_id == "fa":
            fa_input = FaInput(
                dao_result=DaoOutput(**results["dao"]),
                available_resources={
                    "budget": request.constraints.budget.amount
                    if request.constraints.budget
                    else None,
                    "team": request.constraints.human_resources,
                },
                time_horizon=f"{request.constraints.timeline.months}ヶ月"
                if request.constraints.timeline
                else "",
            )
            result = await agent.run(fa_input.model_dump())
            self._context.set("fa_result", result)
            return result

        elif agent_id == "shu":
            selected_path_id = results["fa"].get("recommended_paths", [{}])[0].get(
                "path_id", "A"
            )
            shu_input = ShuInput(
                fa_result=FaOutput(**results["fa"]),
                selected_path_id=selected_path_id,
            )
            result = await agent.run(shu_input.model_dump())
            self._context.set("shu_result", result)
            return result

        elif agent_id == "qi":
            qi_input = QiInput(
                shu_result=ShuOutput(**results["shu"]),
                tech_constraints=request.constraints.technical,
            )
            result = await agent.run(qi_input.model_dump())
            self._context.set("qi_result", result)
            return result

        return {}

    # ========================================
    # ヘルパーメソッド
    # ========================================

    def _build_constraints_list(self, request: DecisionRequest) -> list[str]:
        """制約条件を文字列リストに変換."""
        constraints: list[str] = []
        if request.constraints.budget:
            constraints.append(f"予算: {request.constraints.budget.amount}万円")
        if request.constraints.timeline:
            constraints.append(f"期限: {request.constraints.timeline.months}ヶ月")
        constraints.extend(request.constraints.technical)
        constraints.extend(request.constraints.regulatory)
        return constraints

    def _setup_revision_context(self, revision: int, review_result: dict) -> None:
        """リビジョンコンテキストを設定."""
        findings = review_result.get("findings", [])
        affected = "dao"
        if findings:
            affected_name = findings[0].get("affected_agent", "")
            agent_name_map = self._registry.get_agent_name_map()
            affected = agent_name_map.get(affected_name, "dao")

        self._context.set("revision_round", revision + 1)
        self._context.set("revision_feedback", findings)
        self._context.set("affected_agent", affected)

    def _extract_result_summary(self, agent_id: str, result: dict) -> str:
        """結果からサマリーを抽出."""
        if "problem_type" in result:
            return f"問題タイプ: {result['problem_type']}"
        if "recommended_paths" in result:
            return f"推奨パス: {len(result.get('recommended_paths', []))}件"
        if "phases" in result:
            return f"フェーズ: {len(result.get('phases', []))}件"
        if "implementations" in result:
            return f"実装要素: {len(result.get('implementations', []))}件"
        if "overall_verdict" in result:
            return f"判定: {result['overall_verdict']}"
        if "is_acceptable" in result:
            return "受理" if result["is_acceptable"] else "拒否"
        if "proceed" in result:
            return "通過" if result["proceed"] else "補足情報要求中"
        return "完了"

    def get_agent_definitions(self) -> list[dict[str, Any]]:
        """Agent定義を取得（UI表示用）."""
        return [a.to_frontend_dict() for a in self._registry.get_agent_definitions()]

    def get_context(self) -> SharedContext:
        """SharedContextを取得."""
        return self._context

    # ========================================
    # レスポンス生成
    # ========================================

    def _create_cognitive_gate_blocked_response(self, result: dict) -> dict:
        """CognitiveGateブロック時のレスポンス."""
        return {
            "status": "cognitive_gate_blocked",
            "reason": "認知前処理で不足情報を検出",
            "missing_info": result.get("missing_info", []),
            "message": "分析を進める前に、以下の情報を明確にしてください。",
        }

    def _create_rejected_response(self, result: dict) -> dict:
        """Gatekeeper拒否時のレスポンス."""
        return {
            "status": "rejected",
            "reason": result.get("rejection_reason"),
            "message": result.get("rejection_message"),
            "suggested_rephrase": result.get("suggested_rephrase"),
        }

    def _create_review_rejected_response(self, result: dict) -> dict:
        """Review拒否時のレスポンス."""
        return {
            "status": "rejected_by_review",
            "verdict": "REJECT",
            "findings": result.get("findings", []),
            "message": "検証の結果、計画に重大な問題があります。",
        }

    def _create_max_revisions_response(self, result: dict) -> dict:
        """最大リビジョン到達時のレスポンス."""
        return {
            "status": "max_revisions_reached",
            "message": f"最大リビジョン回数（{self.MAX_REVISIONS}回）に到達しました。",
            "last_review": result,
        }

    def _create_flow_error_event(self, result: dict) -> FlowErrorEvent:
        """FlowErrorEventを生成."""
        return FlowErrorEvent(
            timestamp=time.time(),
            flow_id=self._flow_id or "unknown",
            data={
                "reason": result.get("rejection_reason"),
                "category": str(result.get("category", "")),
            },
            error_message=result.get(
                "rejection_message", "質問が受理されませんでした"
            ),
            error_type="RejectedQuestion",
        )

    async def _handle_cognitive_gate_blocked(
        self,
        request: DecisionRequest,
        cognitive_result: dict,
    ) -> AsyncIterator[AGUIEvent]:
        """CognitiveGateブロック時のイベント処理."""
        clarification_questions = cognitive_result.get("clarification_questions", [])
        if clarification_questions:
            questions = [
                ClarificationQuestion(
                    id=f"q_{i}",
                    text=q,
                    type="text",
                    required=True,
                )
                for i, q in enumerate(clarification_questions)
            ]
            yield ClarificationRequiredEvent(
                timestamp=time.time(),
                flow_id=self._flow_id,
                original_question=request.question,
                questions=questions,
                message="分析を進めるために、以下の情報を補足してください。",
                timeout_seconds=300,
            )
        else:
            yield FlowErrorEvent(
                timestamp=time.time(),
                flow_id=self._flow_id,
                data={"missing_info": cognitive_result.get("missing_info", [])},
                error_message="認知前処理で不足情報を検出しました。",
                error_type="CognitiveGateBlocked",
            )

    # ========================================
    # Flow互換インターフェース
    # ========================================

    async def run(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """Flow互換のrunメソッド."""
        question = inputs.get("question", inputs.get("task", ""))
        if not question:
            return {"status": "error", "message": "question is required"}

        request = DecisionRequest(question=question)
        result = await self.process(request)

        if hasattr(result, "model_dump"):
            return result.model_dump()
        return result

    async def run_stream(
        self, inputs: dict[str, Any]
    ) -> AsyncIterator[dict[str, Any]]:
        """Flow互換のストリーム実行."""
        question = inputs.get("question", inputs.get("task", ""))
        if not question:
            yield {"type": "error", "error": "question is required"}
            return

        request = DecisionRequest(question=question)

        async for event in self.process_with_events(request):
            if hasattr(event, "event_type"):
                yield {
                    "type": event.event_type,
                    "node": getattr(event, "node_name", None),
                    "data": event.data if hasattr(event, "data") else {},
                }
            else:
                yield event

    @property
    def name(self) -> str:
        """Flow名."""
        return "decision-governance-engine"


# グローバルインスタンス（統一入口）
engine = DecisionEngine()

