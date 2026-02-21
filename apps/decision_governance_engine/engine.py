"""Decision Governance Engine - メインエンジン.

PipelineEngine パターンを使用した意思決定支援エンジン。

アーキテクチャ（8ステージ順次実行 + コーチング型改善ループ）:
    前処理層: CognitiveGate（認知分析）→ Gatekeeper（門番、Gate判断）
    診断層: Clarification（問題精緻化）
    分析層: Dao → Fa → Shu → Qi（順次実行、依存チェーン）
    検証層: ReviewAgent（PASS/COACH、COACHは改善指導モード）

実行フロー:
    CognitiveGate → Gatekeeper → Clarification → Dao → Fa → Shu → Qi → Review
         ↓              ↓                                              ↓
       分析のみ      拦截判断                                    PASS / COACH
       (通過)      (不適格拒否)                           (COACHは改善提案→ユーザー対話
                                                          →個別再評価→再スコアリング)

設計改善（v2.3）:
    - ReportBuilder インターフェースを使用
    - YAML 定義から StageConfig 自動生成可能（将来対応）

設計改善（v3.0）:
    - DeepAgentAdapter 統合（認知分析、コンテキスト圧縮、自己進化）
    - 品質評審の多次元評価

設計改善（v3.1）:
    - AI安全防護統合（幻覚検出、データ脱敏、注入防護）

設計改善（v3.2）:
    - ステージ単位DB保存（各ステージ完了後にupsert、途中失敗でも結果保持）
    - 完了済みステージのスキップ（再分析防止）

使用例:
    >>> from apps.decision_governance_engine.engine import DecisionEngine
    >>>
    >>> # 同期実行
    >>> engine = DecisionEngine()
    >>> result = await engine.run({"question": "新規事業への投資判断をしたい"})
    >>>
    >>> # SSEストリーム
    >>> async for event in engine.run_stream({"question": "..."}):
    ...     print(event)
"""

import logging
import os
from collections.abc import AsyncIterator
from typing import TYPE_CHECKING, Any

from apps.decision_governance_engine.services.agent_registry import AgentRegistry
from apps.decision_governance_engine.services.decision_report_builder import (
    DecisionReportBuilder,
)
from apps.decision_governance_engine.services.deep_agent_adapter import (
    DeepAgentAdapter,
)

from agentflow.engines import EngineConfig, PipelineEngine
from agentflow.engines.pipeline_engine import StageConfig
from agentflow.providers import get_llm
from agentflow.security import SafetyMixin


if TYPE_CHECKING:
    from uuid import UUID


class DecisionEngine(PipelineEngine, SafetyMixin):
    """Decision Governance Engine.

    PipelineEngine を継承し、8つの専門Agentを順次実行。
    決策レポートを生成する。

    Attributes:
        MAX_REVISIONS: 最大リビジョン回数（デフォルト: 2）

    v3.0 新機能:
        - DeepAgentAdapter 統合（認知分析、コンテキスト圧縮、自己進化）
        - 品質評審の多次元評価
        - 成功パターン学習

    v3.1 新機能:
        - AI安全防護（幻覚検出、データ脱敏、注入防護）

    使用例:
        >>> engine = DecisionEngine()
        >>> result = await engine.run({"question": "投資判断をしたい"})
    """

    MAX_REVISIONS = 2

    def __init__(
        self,
        llm_client: Any = None,
        enable_rag: bool = True,
        enable_deep_agent: bool = True,
        enable_safety: bool = True,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント（省略時は自動取得）
            enable_rag: RAG機能を有効化するか
            enable_deep_agent: DeepAgent機能を有効化するか
            enable_safety: AI安全防護を有効化するか
        """
        # LLM自動取得（省略時）
        if llm_client is None:
            llm_client = get_llm()

        self._llm_client = llm_client

        # 業務コンポーネント
        self._registry = AgentRegistry(llm_client=llm_client, enable_rag=enable_rag)
        self._enable_rag = enable_rag

        # DeepAgentAdapter（v3.0）
        self._enable_deep_agent = enable_deep_agent
        if enable_deep_agent:
            self._deep_adapter = DeepAgentAdapter(
                llm_client=llm_client,
                enable_evolution=True,
            )
        else:
            self._deep_adapter = None

        # PipelineEngine 初期化（ReportBuilder を使用）
        super().__init__(
            stages=[],  # _initialize で動的設定
            max_revisions=self.MAX_REVISIONS,
            report_builder=DecisionReportBuilder(),  # 新 API を使用
            config=EngineConfig(
                name="decision-governance-engine",
                enable_events=True,
                enable_memory=True,
                timeout_seconds=300,
            ),
        )
        self._logger = logging.getLogger("decision_engine")

        # AI安全防護初期化（v3.1）
        self.init_safety(enabled=enable_safety)

        # ステージ単位DB保存設定（v3.2）
        self._enable_stage_persist = os.getenv("ENABLE_DECISION_HISTORY", "true").lower() == "true"
        # DB保存対象のステージ名
        self._persist_stages: set[str] = {
            "cognitive_gate",
            "gatekeeper",
            "dao",
            "fa",
            "shu",
            "qi",
            "review",
        }
        # 実行中の request_id / question（run() 呼出し時にセット）
        self._current_request_id: UUID | None = None
        self._current_question: str | None = None
        self._resume_completed_stages: set[str] = set()
        self._resume_stage_results: dict[str, dict[str, Any]] = {}

    async def run(self, input_data: dict[str, Any]) -> Any:
        """DecisionEngine を実行（入力キー互換）.

        目的:
            既存呼び出しの `question` を `raw_question` に正規化し、
            CognitiveGateAgent 等の入力スキーマ（raw_question 必須）を満たす。

        Args:
            input_data: 入力 dict。`raw_question` 推奨。互換のため `question` のみでも可。

        Returns:
            PipelineEngine.run() の戻り値（DecisionReport または拒否時 dict）。
        """

        prepared_inputs = self._prepare_run_inputs(input_data)
        return await super().run(prepared_inputs)

    async def run_stream(self, input_data: dict[str, Any]) -> AsyncIterator[dict[str, Any]]:
        """DecisionEngine をストリーム実行（入力キー互換 + resume設定）."""
        prepared_inputs = self._prepare_run_inputs(input_data)
        async for event in super().run_stream(prepared_inputs):
            yield event

    def _prepare_run_inputs(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """run/run_stream 共通の入力前処理."""
        normalized = dict(input_data)

        # 入力キー互換（question -> raw_question）
        if "raw_question" not in normalized and "question" in normalized:
            normalized["raw_question"] = normalized.get("question")

        # ステージ単位DB保存用: request_id / question を保持
        if "_request_id" in normalized:
            self._current_request_id = normalized["_request_id"]
        if "raw_question" in normalized:
            self._current_question = normalized["raw_question"]
        elif "question" in normalized:
            self._current_question = normalized["question"]

        # Resume用の完了済みステージと復元結果を保持
        completed_stages_raw = normalized.pop("_resume_completed_stages", [])
        stage_results_raw = normalized.pop("_resume_stage_results", {})
        if isinstance(completed_stages_raw, list):
            self._resume_completed_stages = {str(item) for item in completed_stages_raw if isinstance(item, str)}
        else:
            self._resume_completed_stages = set()
        if isinstance(stage_results_raw, dict):
            self._resume_stage_results = {
                str(k): v for k, v in stage_results_raw.items() if isinstance(k, str) and isinstance(v, dict)
            }
        else:
            self._resume_stage_results = {}

        return normalized

    async def _run_stage(
        self,
        stage: StageConfig,
        inputs: dict[str, Any],
    ) -> dict[str, Any]:
        """ステージ実行 + 完了後のDB自動保存（v3.2）.

        親クラスの _run_stage() を呼び出した後、対象ステージの結果を
        即座にDBへupsertする。これにより途中失敗時でも結果が保持される。
        """
        # Resume 実行時: 完了済みステージは DB の保存結果を再利用（再分析しない）
        if stage.name in self._resume_completed_stages:
            cached = self._resume_stage_results.get(stage.name)
            if isinstance(cached, dict):
                self._logger.info(f"Stage '{stage.name}' skipped (resume): {self._current_request_id}")
                await self._persist_stage_io(
                    stage_name=stage.name,
                    stage_input=inputs,
                    stage_output=cached,
                    status="resumed",
                    skipped=True,
                    resumed=True,
                )
                return cached

        try:
            result = await super()._run_stage(stage, inputs)
        except Exception as e:
            await self._persist_stage_io(
                stage_name=stage.name,
                stage_input=inputs,
                stage_output=None,
                status="failed",
                error_message=str(e),
                skipped=False,
                resumed=False,
            )
            raise

        # DB保存対象ステージの場合、非同期でupsert
        if self._enable_stage_persist and stage.name in self._persist_stages and self._current_request_id is not None:
            await self._persist_stage_result(stage.name, result)

        # 全ステージの入出力を保存（pfpf用の情报蓄積）
        await self._persist_stage_io(
            stage_name=stage.name,
            stage_input=inputs,
            stage_output=result,
            status="completed",
            skipped=False,
            resumed=False,
        )
        return result

    async def _persist_stage_result(
        self,
        stage_name: str,
        stage_result: dict[str, Any],
    ) -> None:
        """ステージ結果をDBへ即時保存（失敗しても処理は継続）."""
        try:
            from apps.decision_governance_engine.repositories import DecisionRepository

            repo = DecisionRepository()
            await repo.upsert_stage(
                request_id=self._current_request_id,  # type: ignore[arg-type]
                question=self._current_question or "",
                stage_name=stage_name,
                stage_result=stage_result,
            )
            self._logger.info(f"Stage '{stage_name}' persisted to DB: {self._current_request_id}")
        except Exception as e:
            self._logger.warning(f"Stage '{stage_name}' DB persist failed (continuing): {e}")

    async def _persist_stage_io(
        self,
        stage_name: str,
        stage_input: dict[str, Any],
        stage_output: dict[str, Any] | None,
        status: str,
        *,
        error_message: str | None = None,
        skipped: bool = False,
        resumed: bool = False,
    ) -> None:
        """ステージ入出力ログをDBへ保存（失敗しても実行継続）."""
        if self._current_request_id is None:
            return

        try:
            from apps.decision_governance_engine.repositories import DecisionRepository

            repo = DecisionRepository()
            await repo.append_stage_io_log(
                request_id=self._current_request_id,
                question=self._current_question or "",
                stage_name=stage_name,
                stage_input=stage_input,
                stage_output=stage_output,
                status=status,
                error_message=error_message,
                skipped=skipped,
                resumed=resumed,
            )
        except Exception as e:
            self._logger.warning(f"Stage '{stage_name}' IO DB persist failed (continuing): {e}")

    async def _setup_stages(self) -> None:
        """ステージを動的に設定.

        AgentRegistry から Agent を取得し、stages を動的構築。
        PipelineEngine の _setup_stages() フックをオーバーライド。
        """
        await self._registry.initialize()
        if self._enable_rag:
            await self._registry.initialize_rag_agents()

        # stages を動的構築
        # 注: 道→法→術→器 は依存関係があるため順次実行
        self._stage_configs = self._parse_stages(
            [
                {
                    "name": "cognitive_gate",
                    "agent": self._registry.get_agent("cognitive_gate"),
                    # CognitiveGateは分析専用、常に通過（拦截はGatekeeperで行う）
                    "gate": False,
                },
                {
                    "name": "gatekeeper",
                    "agent": self._registry.get_agent("gatekeeper"),
                    "gate": True,
                    "gate_check": lambda r: r.get("is_acceptable", False),
                },
                {
                    "name": "clarification",
                    "agent": self._registry.get_agent("clarification"),
                },
                # 道・法・術・器 は依存チェーンのため順次実行
                {
                    "name": "dao",
                    "agent": self._registry.get_agent("dao"),
                },
                {
                    "name": "fa",
                    "agent": self._registry.get_agent("fa"),
                },
                {
                    "name": "shu",
                    "agent": self._registry.get_agent("shu"),
                },
                {
                    "name": "qi",
                    "agent": self._registry.get_agent("qi"),
                },
                {
                    "name": "review",
                    "agent": self._registry.get_agent("review"),
                    "review": True,
                    "retry_from": "dao",
                },
            ]
        )

        # stage_instances 設定（Agent は既に _registry で初期化済み）
        for stage in self._stage_configs:
            instances = []
            if stage.agent:
                instances.append(stage.agent)
            if stage.agents:
                instances.extend(stage.agents)
            self._stage_instances[stage.name] = instances

        self._logger.info("DecisionEngine stages configured")

    # =========================================================================
    # DeepAgent 統合メソッド（v3.0）
    # =========================================================================

    async def analyze_cognitive(self, question: str) -> dict[str, Any]:
        """認知分析を実行.

        Args:
            question: 分析対象の質問

        Returns:
            認知分析結果（dict形式）
        """
        if not self._deep_adapter:
            return {"intent": question, "is_clear": True, "complexity": "medium"}

        cognitive = await self._deep_adapter.analyze_cognitive(question)
        return cognitive.model_dump()

    async def record_success(
        self,
        task: str,
        result: dict[str, Any],
        context: dict[str, Any] | None = None,
    ) -> None:
        """成功パターンを学習.

        Args:
            task: タスク内容
            result: 実行結果
            context: コンテキスト情報
        """
        if self._deep_adapter:
            await self._deep_adapter.record_success(task, result, context)

    def get_learned_hint(self, task: str) -> str | None:
        """学習済みヒントを取得.

        Args:
            task: タスク内容

        Returns:
            学習済みヒント（なければNone）
        """
        if self._deep_adapter:
            return self._deep_adapter.get_learned_hint(task)
        return None

    def get_deep_stats(self) -> dict[str, Any]:
        """DeepAgent統計情報を取得."""
        if self._deep_adapter:
            return self._deep_adapter.get_stats()
        return {}


__all__ = ["DecisionEngine"]
