# -*- coding: utf-8 -*-
"""AgentFlow Engines - AgentFlowの公式API.

このモジュールはAgentFlowの主要な公開APIです。
4種類の事前定義されたEngine Patternを提供し、90%のAIアプリケーションシナリオをカバー：

=============================================================================
Engine Types
=============================================================================

1. SimpleEngine   - 単一Agent質問応答（最もシンプル）
2. GateEngine     - Gateインターセプト + Agent処理
3. PipelineEngine - 複数Agent順次実行 + Reviewループ
4. RAGEngine      - RAG拡張Agent

=============================================================================
設計原則
=============================================================================

- 設定即使用：Pattern選択 + 設定 = デプロイ
- 内部でflow/patternsを活用：フレームワーク機能の再利用
- 拡張可能：BaseEngineを継承して新規Engineを作成可能
- AG-UIプロトコル準拠：run_stream()でリアルタイムイベント発行

=============================================================================
使用例
=============================================================================

    >>> from agentflow import SimpleEngine, PipelineEngine
    >>>
    >>> # 1. シンプルな質問応答
    >>> engine = SimpleEngine(agent=MyAgent)
    >>> result = await engine.run({"question": "こんにちは"})
    >>>
    >>> # 2. マルチステップフロー（Gate + 複数Agent + Review）
    >>> engine = PipelineEngine(
    ...     stages=[
    ...         {"name": "gate", "agent": GateAgent, "gate": True},
    ...         {"name": "analysis", "agents": [DaoAgent, FaAgent]},
    ...         {"name": "review", "agent": ReviewAgent, "review": True},
    ...     ],
    ...     max_revisions=2,
    ... )
    >>> async for event in engine.run_stream(inputs):
    ...     print(event)
    >>>
    >>> # 3. RAG拡張Agent
    >>> engine = RAGEngine(
    ...     agent=MyAgent,
    ...     retriever=my_retriever,
    ... )
    >>> result = await engine.run({"query": "..."})

=============================================================================
カスタムEngine作成
=============================================================================

    >>> from agentflow import BaseEngine, EngineConfig
    >>>
    >>> class MyCustomEngine(BaseEngine):
    ...     async def _initialize(self) -> None:
    ...         # 初期化ロジック
    ...         pass
    ...
    ...     async def _execute(self, inputs: dict) -> dict:
    ...         # 実行ロジック
    ...         return {"result": "..."}
"""

from agentflow.engines.base import BaseEngine, EngineConfig
from agentflow.engines.gate_engine import GateEngine
from agentflow.engines.pipeline_engine import PipelineEngine, StageConfig
from agentflow.engines.rag_engine import RAGEngine
from agentflow.engines.report_builder import ReportBuilder, SimpleReportBuilder
from agentflow.engines.simple_engine import SimpleEngine
from agentflow.patterns.progress_emitter import AgentMeta

__all__ = [
    # 基底クラス（カスタムEngine作成用）
    "BaseEngine",
    "EngineConfig",
    # 4種類の予定義Pattern
    "SimpleEngine",
    "GateEngine",
    "PipelineEngine",
    "RAGEngine",
    # 設定クラス
    "StageConfig",
    "AgentMeta",
    # レポート生成
    "ReportBuilder",
    "SimpleReportBuilder",
]

