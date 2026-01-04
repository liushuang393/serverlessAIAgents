# -*- coding: utf-8 -*-
"""Decision Governance Engine - メインエンジン.

PipelineEngine パターンを使用した意思決定支援エンジン。

アーキテクチャ:
    Gate層: CognitiveGate → Gatekeeper（2段階チェック）
    分析層: Clarification → Dao → Fa → Shu → Qi（順次実行）
    検証層: ReviewAgent（REVISE回退対応）

設計改善（v2.3）:
    - ReportBuilder インターフェースを使用
    - YAML 定義から StageConfig 自動生成可能（将来対応）

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
from typing import Any

from agentflow.engines import PipelineEngine, EngineConfig
from agentflow.providers import get_llm

from apps.decision_governance_engine.services.agent_registry import AgentRegistry
from apps.decision_governance_engine.services.decision_report_builder import (
    DecisionReportBuilder,
)


class DecisionEngine(PipelineEngine):
    """Decision Governance Engine.

    PipelineEngine を継承し、8つの専門Agentを順次実行。
    決策レポートを生成する。

    Attributes:
        MAX_REVISIONS: 最大リビジョン回数（デフォルト: 2）

    使用例:
        >>> engine = DecisionEngine()
        >>> result = await engine.run({"question": "投資判断をしたい"})
    """

    MAX_REVISIONS = 2

    def __init__(
        self,
        llm_client: Any = None,
        enable_rag: bool = True,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント（省略時は自動取得）
            enable_rag: RAG機能を有効化するか
        """
        # LLM自動取得（省略時）
        if llm_client is None:
            llm_client = get_llm()

        # 業務コンポーネント
        self._registry = AgentRegistry(llm_client=llm_client, enable_rag=enable_rag)
        self._enable_rag = enable_rag

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
        self._stage_configs = self._parse_stages([
            {
                "name": "cognitive_gate",
                "agent": self._registry.get_agent("cognitive_gate"),
                "gate": True,
                "gate_check": lambda r: r.get("proceed", True),
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
        ])

        # stage_instances 設定（Agent は既に _registry で初期化済み）
        for stage in self._stage_configs:
            instances = []
            if stage.agent:
                instances.append(stage.agent)
            if stage.agents:
                instances.extend(stage.agents)
            self._stage_instances[stage.name] = instances

        self._logger.info("DecisionEngine stages configured")


__all__ = ["DecisionEngine"]

