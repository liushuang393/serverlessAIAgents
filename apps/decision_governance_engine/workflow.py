# -*- coding: utf-8 -*-
"""Decision Governance Engine - Workflow定義.

道・法・術・器・検証の6 AgentをAgentCoordinatorで協調させる。
状態遷移は順方向のみ、スキップ禁止。REVISE時は該当Agentに回退。

AG-UI連携: AGUIEventEmitter経由でSSE進捗配信対応。

変更履歴:
    - 2024: AgentFlow統一入口原則に準拠
    - FlowWrapper互換のrunメソッドを追加
"""

import asyncio
import logging
import time
import uuid
from collections.abc import AsyncIterator
from datetime import datetime
from typing import Any

from agentflow import create_flow
from agentflow.patterns.multi_agent import AgentCoordinator, SharedContext

from apps.decision_governance_engine.agents.base_agent import (
    AgentExecutionError,
    AgentRetryExhaustedError,
    AgentTimeoutError,
)
from apps.decision_governance_engine.agents.dao_agent import DaoAgent
from apps.decision_governance_engine.agents.fa_agent import FaAgent
from apps.decision_governance_engine.agents.gatekeeper_agent import GatekeeperAgent
from apps.decision_governance_engine.agents.qi_agent import QiAgent
from apps.decision_governance_engine.agents.review_agent import ReviewAgent
from apps.decision_governance_engine.agents.shu_agent import ShuAgent
from apps.decision_governance_engine.schemas.agent_schemas import (
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
from apps.decision_governance_engine.schemas.output_schemas import (
    DecisionReport,
    ExecutiveSummary,
)

# AG-UI Events
try:
    from agentflow.protocols.agui_events import (
        AGUIEvent,
        FlowCompleteEvent,
        FlowErrorEvent,
        FlowStartEvent,
        LogEvent,
        NodeCompleteEvent,
        NodeStartEvent,
        ProgressEvent,
    )
    AGUI_AVAILABLE = True
except ImportError:
    AGUI_AVAILABLE = False


class DecisionEngine:
    """Decision Governance Engine メインクラス.

    AgentCoordinatorを使用して6つのAgentを順次実行し、署名可能な決策レポートを生成。
    ReviewAgentがREVISEを返した場合、該当Agentに回退して再実行。

    使用例:
        >>> engine = DecisionEngine()
        >>> result = await engine.process("新規事業への投資判断をしたい")
        >>> # SSEストリーム付き処理
        >>> async for event in engine.process_with_events(request):
        ...     print(event.event_type, event.data)
    """

    # Agent定義（門番・道・法・術・器・検証）
    AGENT_DEFINITIONS = [
        {"id": "gatekeeper", "name": "門番", "label": "入口検証"},
        {"id": "dao", "name": "道", "label": "本質分析"},
        {"id": "fa", "name": "法", "label": "戦略選定"},
        {"id": "shu", "name": "術", "label": "実行計画"},
        {"id": "qi", "name": "器", "label": "技術実装"},
        {"id": "review", "name": "検証", "label": "最終検証"},
    ]
    TOTAL_AGENTS = len(AGENT_DEFINITIONS)

    # REVISE回退設定
    MAX_REVISIONS = 2  # 最大リビジョン回数

    # Agent名マッピング（affected_agent → Agent ID）
    AGENT_NAME_MAP = {
        "GatekeeperAgent": "gatekeeper",
        "DaoAgent": "dao",
        "FaAgent": "fa",
        "ShuAgent": "shu",
        "QiAgent": "qi",
        "ReviewAgent": "review",
    }

    def __init__(self, llm_client: Any = None, enable_rag: bool = True) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント（オプション）
            enable_rag: RAG機能を有効化するか
        """
        self._logger = logging.getLogger("decision_engine")
        self._llm = llm_client
        self._enable_rag = enable_rag
        self._rag_initialized = False

        # Agentを初期化
        self._gatekeeper = GatekeeperAgent(llm_client=llm_client)
        self._dao = DaoAgent(llm_client=llm_client)
        self._fa = FaAgent(llm_client=llm_client)
        self._shu = ShuAgent(llm_client=llm_client)
        self._qi = QiAgent(llm_client=llm_client)
        self._review = ReviewAgent(llm_client=llm_client)

        # Agent辞書（回退時のアクセス用）
        self._agents = {
            "gatekeeper": self._gatekeeper,
            "dao": self._dao,
            "fa": self._fa,
            "shu": self._shu,
            "qi": self._qi,
            "review": self._review,
        }

        # SharedContext初期化（記憶システム有効）
        self._context = SharedContext(enable_memory=True)

        # Core Agents（Gatekeeper除く）のCoordinator
        self._core_coordinator = AgentCoordinator(
            agents=[self._dao, self._fa, self._shu, self._qi],
            pattern="sequential",
            shared_context=self._context,
        )

        # AG-UI イベントキュー（SSE配信用）
        self._event_queue: asyncio.Queue[Any] = asyncio.Queue(maxsize=1000)
        self._flow_id: str | None = None
        self._completed_agents: int = 0

    async def _initialize_rag_skills(self) -> None:
        """RAGスキルを初期化（Shu/Qi Agent用）.

        業界プラクティスと技術ドキュメントをRAGに登録。
        """
        if self._rag_initialized or not self._enable_rag:
            return

        try:
            # Shu AgentのRAG初期化（業界プラクティス）
            await self._shu.initialize_rag()
            self._logger.info("ShuAgent RAG initialized")

            # Qi AgentのRAG初期化（技術ドキュメント）
            await self._qi.initialize_rag()
            self._logger.info("QiAgent RAG initialized")

            self._rag_initialized = True
        except Exception as e:
            self._logger.warning(f"RAG initialization failed (non-fatal): {e}")

    async def process(self, request: DecisionRequest | str) -> DecisionReport | dict:
        """意思決定プロセスを実行.

        Args:
            request: 意思決定依頼（文字列またはDecisionRequest）

        Returns:
            DecisionReport または拒否時のエラー情報
        """
        # 文字列の場合はDecisionRequestに変換
        if isinstance(request, str):
            request = DecisionRequest(question=request)

        self._logger.info(f"Decision process started: {request.question[:50]}...")
        self._context.clear()  # コンテキストをクリア

        # RAGスキル初期化（Shu/Qi用）
        await self._initialize_rag_skills()

        # Step 1: Gatekeeper検証（単独実行）
        gatekeeper_result = await self._run_gatekeeper(request)
        if not gatekeeper_result.get("is_acceptable", False):
            self._logger.warning("Question rejected by Gatekeeper")
            return {
                "status": "rejected",
                "reason": gatekeeper_result.get("rejection_reason"),
                "message": gatekeeper_result.get("rejection_message"),
                "suggested_rephrase": gatekeeper_result.get("suggested_rephrase"),
            }

        # Step 2-6: Core Agents + Review（REVISEループ付き）
        for revision in range(self.MAX_REVISIONS + 1):
            self._logger.info(f"Execution round {revision + 1}/{self.MAX_REVISIONS + 1}")

            # Core Agentsを実行
            results = await self._run_core_agents(request)

            # Reviewを実行
            review_result = await self._run_review(results)
            results["review"] = review_result

            verdict = review_result.get("overall_verdict", "PASS")

            if verdict == ReviewVerdict.PASS.value or verdict == ReviewVerdict.PASS:
                # 合格 - レポート生成
                self._logger.info("Review PASSED - generating report")
                return self._generate_report(results)

            if verdict == ReviewVerdict.REJECT.value or verdict == ReviewVerdict.REJECT:
                # 却下
                self._logger.warning("Review REJECTED - stopping")
                return {
                    "status": "rejected_by_review",
                    "verdict": "REJECT",
                    "findings": review_result.get("findings", []),
                    "message": "検証の結果、計画に重大な問題があります。",
                }

            # REVISE - 回退処理
            if revision < self.MAX_REVISIONS:
                affected = self._get_affected_agent(review_result)
                self._logger.info(f"Review REVISE - retrying from {affected}")
                self._context.set("revision_round", revision + 1)
                self._context.set("revision_feedback", review_result.get("findings", []))
                self._context.set("affected_agent", affected)
                # ループを継続して再実行

        # 最大リビジョン到達
        self._logger.warning("Max revisions reached")
        return {
            "status": "max_revisions_reached",
            "message": f"最大リビジョン回数（{self.MAX_REVISIONS}回）に到達しました。",
            "last_review": review_result,
        }

    async def _run_gatekeeper(self, request: DecisionRequest) -> dict:
        """Gatekeeper検証を実行."""
        gatekeeper_input = GatekeeperInput(raw_question=request.question)
        result = await self._gatekeeper.run(gatekeeper_input.model_dump())
        self._context.set("gatekeeper_result", result)
        return result

    async def _run_core_agents(self, request: DecisionRequest) -> dict[str, dict]:
        """Core Agents（Dao→Fa→Shu→Qi）を実行."""
        results: dict[str, dict] = {}

        # Dao（本質分析）
        dao_input = DaoInput(
            question=request.question,
            constraints=[
                *request.constraints.technical,
                *request.constraints.regulatory,
            ],
            stakeholders=request.constraints.human_resources,
        )
        dao_result = await self._dao.run(dao_input.model_dump())
        self._context.set("dao_result", dao_result)
        results["dao"] = dao_result
        self._logger.info("Dao analysis completed")

        # Fa（戦略選定）
        fa_input = FaInput(
            dao_result=DaoOutput(**dao_result),
            available_resources={
                "budget": request.constraints.budget.amount if request.constraints.budget else None,
                "team": request.constraints.human_resources,
            },
            time_horizon=f"{request.constraints.timeline.months}ヶ月" if request.constraints.timeline else "",
        )
        fa_result = await self._fa.run(fa_input.model_dump())
        self._context.set("fa_result", fa_result)
        results["fa"] = fa_result
        self._logger.info("Fa strategy completed")

        # Shu（実行計画）
        selected_path_id = fa_result.get("recommended_paths", [{}])[0].get("path_id", "A")
        shu_input = ShuInput(
            fa_result=FaOutput(**fa_result),
            selected_path_id=selected_path_id,
        )
        shu_result = await self._shu.run(shu_input.model_dump())
        self._context.set("shu_result", shu_result)
        results["shu"] = shu_result
        self._logger.info("Shu planning completed")

        # Qi（技術実装）
        qi_input = QiInput(
            shu_result=ShuOutput(**shu_result),
            tech_constraints=request.constraints.technical,
        )
        qi_result = await self._qi.run(qi_input.model_dump())
        self._context.set("qi_result", qi_result)
        results["qi"] = qi_result
        self._logger.info("Qi implementation completed")

        return results

    async def _run_review(self, results: dict[str, dict]) -> dict:
        """Review検証を実行."""
        review_input = ReviewInput(
            dao_result=DaoOutput(**results["dao"]),
            fa_result=FaOutput(**results["fa"]),
            shu_result=ShuOutput(**results["shu"]),
            qi_result=QiOutput(**results["qi"]),
        )
        review_result = await self._review.run(review_input.model_dump())
        self._context.set("review_result", review_result)
        self._logger.info(f"Review completed: {review_result.get('overall_verdict')}")
        return review_result

    def _get_affected_agent(self, review_result: dict) -> str:
        """Review結果から影響を受けるAgentを特定."""
        findings = review_result.get("findings", [])
        if findings:
            # 最初のfindingから取得
            affected_name = findings[0].get("affected_agent", "")
            return self.AGENT_NAME_MAP.get(affected_name, "dao")
        return "dao"  # デフォルト

    def _generate_report(self, results: dict[str, dict]) -> DecisionReport:
        """最終レポートを生成."""
        dao_result = results.get("dao", {})
        fa_result = results.get("fa", {})
        shu_result = results.get("shu", {})
        qi_result = results.get("qi", {})
        review_result = results.get("review", {})

        # ExecutiveSummary生成
        recommended = fa_result.get("recommended_paths", [{}])[0]
        summary = ExecutiveSummary(
            one_line_decision=f"{recommended.get('name', '推奨案')}を選択すべき"[:30],
            recommended_action=recommended.get("description", "詳細は法セクション参照"),
            key_risks=list(recommended.get("cons", [])[:3]),
            first_step=shu_result.get("first_action", "キックオフMTG設定"),
            estimated_impact="計画実行により目標達成を見込む",
        )

        return DecisionReport(
            report_id=f"DGE-{datetime.now().strftime('%Y%m%d')}-{uuid.uuid4().hex[:6].upper()}",
            dao=dao_result,
            fa=fa_result,
            shu=shu_result,
            qi=qi_result,
            review=review_result,
            executive_summary=summary,
        )

    # ========================================
    # AG-UI SSE ストリーム配信
    # ========================================

    async def _emit_event(self, event: Any) -> None:
        """イベントをキューに追加（内部用）."""
        if not AGUI_AVAILABLE:
            return
        try:
            await asyncio.wait_for(self._event_queue.put(event), timeout=1.0)
        except TimeoutError:
            self._logger.warning(f"Event queue full, dropping event")

    async def _emit_node_start(self, agent_id: str, agent_name: str, label: str) -> None:
        """ノード開始イベントを発火."""
        if not AGUI_AVAILABLE or not self._flow_id:
            return
        event = NodeStartEvent(
            timestamp=time.time(),
            flow_id=self._flow_id,
            data={"label": label},
            node_id=agent_id,
            node_name=agent_name,
        )
        await self._emit_event(event)
        # ログ発行
        log_event = LogEvent(
            timestamp=time.time(),
            flow_id=self._flow_id,
            data={},
            level="INFO",
            message=f"{agent_name}({label})の分析を開始...",
            source=agent_id,
        )
        await self._emit_event(log_event)

    async def _emit_node_complete(
        self, agent_id: str, agent_name: str, result_summary: str
    ) -> None:
        """ノード完了イベントを発火."""
        if not AGUI_AVAILABLE or not self._flow_id:
            return
        self._completed_agents += 1
        event = NodeCompleteEvent(
            timestamp=time.time(),
            flow_id=self._flow_id,
            data={"result_summary": result_summary},
            node_id=agent_id,
            node_name=agent_name,
        )
        await self._emit_event(event)
        # プログレスイベント
        progress = ProgressEvent(
            timestamp=time.time(),
            flow_id=self._flow_id,
            data={},
            current=self._completed_agents,
            total=self.TOTAL_AGENTS,
            percentage=(self._completed_agents / self.TOTAL_AGENTS) * 100,
        )
        await self._emit_event(progress)

    async def process_with_events(
        self, request: DecisionRequest | str
    ) -> AsyncIterator[Any]:
        """SSEストリーム付きで意思決定プロセスを実行.

        各Agentの開始・完了イベントをリアルタイム配信。

        Args:
            request: 意思決定依頼

        Yields:
            AGUIEvent: AG-UIプロトコル準拠イベント
        """
        if not AGUI_AVAILABLE:
            # AG-UI利用不可の場合は通常処理
            result = await self.process(request)
            yield {"type": "result", "data": result}
            return

        # 初期化
        self._flow_id = f"decision-{uuid.uuid4().hex[:8]}"
        self._completed_agents = 0
        self._event_queue = asyncio.Queue(maxsize=1000)

        # 文字列の場合はDecisionRequestに変換
        if isinstance(request, str):
            request = DecisionRequest(question=request)

        # Flow開始イベント
        start_event = FlowStartEvent(
            timestamp=time.time(),
            flow_id=self._flow_id,
            data={"question": request.question[:100]},
        )
        yield start_event

        # RAGスキル初期化
        await self._initialize_rag_skills()

        try:
            # Gatekeeper
            agent_def = self.AGENT_DEFINITIONS[0]
            await self._emit_node_start(
                agent_def["id"], agent_def["name"], agent_def["label"]
            )
            while not self._event_queue.empty():
                yield await self._event_queue.get()

            gatekeeper_result = await self._run_gatekeeper(request)

            await self._emit_node_complete(
                agent_def["id"],
                agent_def["name"],
                "受理" if gatekeeper_result.get("is_acceptable") else "拒否",
            )
            while not self._event_queue.empty():
                yield await self._event_queue.get()

            if not gatekeeper_result.get("is_acceptable", False):
                error_event = FlowErrorEvent(
                    timestamp=time.time(),
                    flow_id=self._flow_id,
                    data={"reason": gatekeeper_result.get("rejection_reason")},
                    error_message=gatekeeper_result.get(
                        "rejection_message", "質問が受理されませんでした"
                    ),
                    error_type="RejectedQuestion",
                )
                yield error_event
                return

            # Core Agents + Review
            results = await self._run_core_agents_with_events(request)
            while not self._event_queue.empty():
                yield await self._event_queue.get()

            # Review
            agent_def = self.AGENT_DEFINITIONS[5]
            await self._emit_node_start(
                agent_def["id"], agent_def["name"], agent_def["label"]
            )
            while not self._event_queue.empty():
                yield await self._event_queue.get()

            review_result = await self._run_review(results)
            results["review"] = review_result

            verdict = review_result.get("overall_verdict", "PASS")
            await self._emit_node_complete(
                agent_def["id"], agent_def["name"], f"判定: {verdict}"
            )
            while not self._event_queue.empty():
                yield await self._event_queue.get()

            # レポート生成
            if verdict == ReviewVerdict.PASS.value or verdict == ReviewVerdict.PASS:
                report = self._generate_report(results)
                complete_event = FlowCompleteEvent(
                    timestamp=time.time(),
                    flow_id=self._flow_id,
                    data={"report_id": report.report_id},
                )
                yield complete_event
            else:
                error_event = FlowErrorEvent(
                    timestamp=time.time(),
                    flow_id=self._flow_id,
                    data={"verdict": str(verdict)},
                    error_message=f"検証結果: {verdict}",
                    error_type="ReviewNotPassed",
                )
                yield error_event

        except Exception as e:
            error_event = FlowErrorEvent(
                timestamp=time.time(),
                flow_id=self._flow_id or "unknown",
                data={},
                error_message=str(e),
                error_type=type(e).__name__,
            )
            yield error_event
            raise
        finally:
            self._flow_id = None

    async def _run_core_agents_with_events(
        self, request: DecisionRequest
    ) -> dict[str, dict]:
        """Core Agentsをイベント発火付きで実行."""
        results: dict[str, dict] = {}

        # Dao（本質分析）
        agent_def = self.AGENT_DEFINITIONS[1]
        await self._emit_node_start(
            agent_def["id"], agent_def["name"], agent_def["label"]
        )
        dao_input = DaoInput(
            question=request.question,
            constraints=[*request.constraints.technical, *request.constraints.regulatory],
            stakeholders=request.constraints.human_resources,
        )
        dao_result = await self._dao.run(dao_input.model_dump())
        self._context.set("dao_result", dao_result)
        results["dao"] = dao_result
        await self._emit_node_complete(
            agent_def["id"],
            agent_def["name"],
            f"問題タイプ: {dao_result.get('problem_type', 'N/A')}",
        )

        # Fa（戦略選定）
        agent_def = self.AGENT_DEFINITIONS[2]
        await self._emit_node_start(
            agent_def["id"], agent_def["name"], agent_def["label"]
        )
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
        fa_result = await self._fa.run(fa_input.model_dump())
        self._context.set("fa_result", fa_result)
        results["fa"] = fa_result
        paths_count = len(fa_result.get("recommended_paths", []))
        await self._emit_node_complete(
            agent_def["id"], agent_def["name"], f"推奨パス: {paths_count}件"
        )

        # Shu（実行計画）
        agent_def = self.AGENT_DEFINITIONS[3]
        await self._emit_node_start(
            agent_def["id"], agent_def["name"], agent_def["label"]
        )
        selected_path_id = fa_result.get("recommended_paths", [{}])[0].get("path_id", "A")
        shu_input = ShuInput(
            fa_result=FaOutput(**fa_result),
            selected_path_id=selected_path_id,
        )
        shu_result = await self._shu.run(shu_input.model_dump())
        self._context.set("shu_result", shu_result)
        results["shu"] = shu_result
        phases_count = len(shu_result.get("phases", []))
        await self._emit_node_complete(
            agent_def["id"], agent_def["name"], f"フェーズ: {phases_count}件"
        )

        # Qi（技術実装）
        agent_def = self.AGENT_DEFINITIONS[4]
        await self._emit_node_start(
            agent_def["id"], agent_def["name"], agent_def["label"]
        )
        qi_input = QiInput(
            shu_result=ShuOutput(**shu_result),
            tech_constraints=request.constraints.technical,
        )
        qi_result = await self._qi.run(qi_input.model_dump())
        self._context.set("qi_result", qi_result)
        results["qi"] = qi_result
        impl_count = len(qi_result.get("implementations", []))
        await self._emit_node_complete(
            agent_def["id"], agent_def["name"], f"実装要素: {impl_count}件"
        )

        return results

    def get_agent_definitions(self) -> list[dict[str, str]]:
        """Agent定義を取得（UI表示用）."""
        return self.AGENT_DEFINITIONS.copy()

    def get_context(self) -> SharedContext:
        """SharedContextを取得."""
        return self._context

    # ========================================
    # Flow 互換インターフェース
    # ========================================

    async def run(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """Flow互換のrunメソッド.

        Args:
            inputs: 入力データ（question キー必須）

        Returns:
            DecisionReport または拒否情報
        """
        question = inputs.get("question", inputs.get("task", ""))
        if not question:
            return {"status": "error", "message": "question is required"}

        request = DecisionRequest(question=question)

        # 制約条件があれば設定
        if "budget" in inputs:
            request.constraints.budget = {"amount": inputs["budget"]}
        if "timeline_months" in inputs:
            request.constraints.timeline = {"months": inputs["timeline_months"]}
        if "technical_constraints" in inputs:
            request.constraints.technical = inputs["technical_constraints"]

        result = await self.process(request)

        # Pydanticモデルの場合はdict化
        if hasattr(result, "model_dump"):
            return result.model_dump()
        return result

    async def run_stream(
        self, inputs: dict[str, Any]
    ) -> AsyncIterator[dict[str, Any]]:
        """Flow互換のストリーム実行.

        process_with_events を簡素化したラッパー。

        Yields:
            イベント dict（type, node, data）
        """
        question = inputs.get("question", inputs.get("task", ""))
        if not question:
            yield {"type": "error", "error": "question is required"}
            return

        request = DecisionRequest(question=question)

        async for event in self.process_with_events(request):
            # AG-UI形式 → 簡素形式に変換
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


# ========================================
# グローバルインスタンス（統一入口）
# ========================================

engine = DecisionEngine()
