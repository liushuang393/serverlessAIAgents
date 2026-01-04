"""Multi-Agent Collaboration Pattern - 複数 Agent の協調.

このモジュールは Multi-Agent Collaboration Pattern を実装します：
- Sequential: 順次実行
- Concurrent: 並行実行
- Handoff: 動的委譲

設計原則：
- 簡単：AgentBlock ベース、理解しやすい
- 柔軟：複数の協調パターンをサポート
- 健壮：Agent 障害時の fallback
- 独立：外部フレームワーク不要

参考（思想のみ吸収）：
- Azure Architecture: AI Agent Orchestration Patterns
- OpenAI: Multi-Agent Collaboration

技術スタック：
- AgentBlock: Agent基底クラス
- asyncio: 並行処理
- PocketFlow: ワークフロー実行
"""

import asyncio
import logging
from datetime import datetime
from typing import Any

from agentflow.core.agent_block import AgentBlock
from agentflow.core.types import WorkflowConfig


class SharedContext:
    """Agent間の共有コンテキスト.

    責務：
    - Agent間で状態を共有
    - 履歴管理
    - スレッドセーフ
    - 記憶システム統合（オプション）

    Example:
        >>> context = SharedContext()
        >>> context.set("result_a", {"data": "A"})
        >>> result = context.get("result_a")

        # 記憶システムを有効化
        >>> context = SharedContext(enable_memory=True)
        >>> await context.start()
        >>> await context.remember("重要な情報", topic="AI")
        >>> memories = await context.recall(topic="AI")
    """

    def __init__(
        self,
        enable_memory: bool = False,
        enable_vector_search: bool = False,
        embedding_dim: int = 384,
    ) -> None:
        """初期化.

        Args:
            enable_memory: 記憶システムを有効化
            enable_vector_search: ベクトル検索を有効化（enable_memory=Trueの場合のみ）
            embedding_dim: 埋め込みベクトルの次元数
        """
        self._data: dict[str, Any] = {}
        self._history: list[dict[str, Any]] = []
        self._logger = logging.getLogger(__name__)
        self._enable_memory = enable_memory
        self._memory_manager = None

        # 記憶システムを初期化
        if self._enable_memory:
            from agentflow.memory import MemoryManager

            self._memory_manager = MemoryManager(
                enable_vector_search=enable_vector_search, embedding_dim=embedding_dim
            )

    def set(self, key: str, value: Any) -> None:
        """値を設定.

        Args:
            key: キー
            value: 値
        """
        self._data[key] = value
        self._history.append({
            "action": "set",
            "key": key,
            "value": value,
            "timestamp": datetime.now().isoformat(),
        })
        self._logger.debug(f"SharedContext.set: {key}")

    def get(self, key: str, default: Any = None) -> Any:
        """値を取得.

        Args:
            key: キー
            default: デフォルト値

        Returns:
            値
        """
        return self._data.get(key, default)

    def get_all(self) -> dict[str, Any]:
        """全ての値を取得.

        Returns:
            全ての値
        """
        return self._data.copy()

    def get_history(self) -> list[dict[str, Any]]:
        """履歴を取得.

        Returns:
            履歴リスト
        """
        return self._history.copy()

    def remove(self, key: str) -> None:
        """値を削除.

        Args:
            key: キー
        """
        if key in self._data:
            del self._data[key]
            self._history.append({
                "action": "remove",
                "key": key,
                "timestamp": datetime.now().isoformat(),
            })
            self._logger.debug(f"SharedContext.remove: {key}")

    def clear(self) -> None:
        """クリア."""
        self._data.clear()
        self._history.clear()

    async def start(self) -> None:
        """記憶システムを開始（記憶システムが有効な場合）."""
        if self._memory_manager:
            await self._memory_manager.start()
            self._logger.info("Memory system started in SharedContext")

    async def stop(self) -> None:
        """記憶システムを停止（記憶システムが有効な場合）."""
        if self._memory_manager:
            await self._memory_manager.stop()
            self._logger.info("Memory system stopped in SharedContext")

    async def remember(
        self,
        text: str,
        topic: str | None = None,
        metadata: dict[str, Any] | None = None,
    ) -> Any:
        """情報を記憶（記憶システムが有効な場合）.

        Args:
            text: 記憶する情報
            topic: トピック名
            metadata: 追加メタデータ

        Returns:
            記憶エントリ、または None

        Raises:
            RuntimeError: 記憶システムが無効な場合
        """
        if not self._memory_manager:
            msg = "Memory system is not enabled. Set enable_memory=True in constructor."
            raise RuntimeError(msg)

        return await self._memory_manager.remember(text, topic, metadata)

    async def recall(
        self,
        topic: str | None = None,
        limit: int = 10,
        min_importance: float = 0.0,
        query: str | None = None,
        min_similarity: float = 0.0,
    ) -> list[Any]:
        """記憶を検索（記憶システムが有効な場合）.

        Args:
            topic: トピック名
            limit: 最大取得数
            min_importance: 最小重要度
            query: ベクトル検索クエリ（enable_vector_search=Trueの場合のみ）
            min_similarity: 最小類似度（ベクトル検索用）

        Returns:
            記憶エントリのリスト

        Raises:
            RuntimeError: 記憶システムが無効な場合
        """
        if not self._memory_manager:
            msg = "Memory system is not enabled. Set enable_memory=True in constructor."
            raise RuntimeError(msg)

        return await self._memory_manager.recall(
            topic, limit, min_importance, query, min_similarity
        )

    def get_memory_status(self) -> dict[str, Any] | None:
        """記憶システムの状態を取得.

        Returns:
            システム状態、または None（記憶システムが無効な場合）
        """
        if self._memory_manager:
            return self._memory_manager.get_status()
        return None


class AgentRouter(AgentBlock):
    """Agentルーティング - タスクに応じてAgentを選択.

    責務：
    - タスクを分析
    - 最適なAgentを選択
    - ルーティング理由を記録

    Example:
        >>> router = AgentRouter(
        ...     agents={
        ...         "research": research_agent,
        ...         "analysis": analysis_agent
        ...     },
        ...     llm_client=my_llm
        ... )
        >>> result = await router.run({"task": "市場調査"})
        >>> selected = result["selected_agent"]
    """

    def __init__(
        self,
        agents: dict[str, AgentBlock],
        llm_client: Any = None,
        **kwargs: Any,
    ) -> None:
        """初期化.

        Args:
            agents: Agent 辞書 {"名前": Agent}
            llm_client: LLM クライアント
            **kwargs: AgentBlock への引数
        """
        super().__init__(**kwargs)
        self._agents = agents
        self._llm = llm_client
        self._logger = logging.getLogger(__name__)

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """最適な Agent を選択して実行.

        Args:
            input_data: {
                "task": タスク内容
            }

        Returns:
            {
                "selected_agent": 選択された Agent 名,
                "result": 実行結果,
                "reasoning": 選択理由
            }
        """
        task = input_data.get("task", "")

        self._logger.info(f"Agent ルーティング開始: task={task[:50]}")

        # Agent を選択
        if self._llm:
            agent_name, reasoning = await self._select_with_llm(task)
        else:
            # Fallback: 最初の Agent を選択
            agent_name = list(self._agents.keys())[0] if self._agents else None
            reasoning = "LLM が設定されていないため最初の Agent を選択"

        if not agent_name or agent_name not in self._agents:
            return {
                "selected_agent": None,
                "result": None,
                "reasoning": "適切な Agent が見つかりません",
                "error": "No suitable agent found",
            }

        # Agent を実行
        agent = self._agents[agent_name]
        result = await agent.run(input_data)

        self._logger.info(f"Agent ルーティング完了: selected={agent_name}")

        return {
            "selected_agent": agent_name,
            "result": result,
            "reasoning": reasoning,
        }

    async def _select_with_llm(self, task: str) -> tuple[str, str]:
        """LLM を使って Agent を選択.

        Args:
            task: タスク内容

        Returns:
            (Agent 名, 選択理由)
        """
        agents_desc = "\n".join([
            f"- {name}: {agent.__class__.__name__}"
            for name, agent in self._agents.items()
        ])

        prompt = f"""以下のタスクを処理するのに最適な Agent を選択してください。

【タスク】
{task}

【利用可能な Agent】
{agents_desc}

最適な Agent の名前のみを返してください（説明不要）。
"""

        response = await self._llm.generate(prompt)

        # 簡易パース: 最初に見つかった Agent 名を返す
        for name in self._agents:
            if name.lower() in response.lower():
                return name, f"LLM が {name} を選択"

        # Fallback
        return list(self._agents.keys())[0], "デフォルト選択"


class AgentCoordinator:
    """Agent協調制御.

    責務：
    - Sequentialパターン（順次実行）
    - Concurrentパターン（並行実行）
    - Handoffパターン（動的委譲）

    Example:
        >>> coordinator = AgentCoordinator(
        ...     agents=[agent_a, agent_b, agent_c],
        ...     pattern="sequential"
        ... )
        >>> result = await coordinator.execute("タスク")
    """

    def __init__(
        self,
        agents: list[AgentBlock],
        pattern: str = "sequential",
        shared_context: SharedContext | None = None,
    ) -> None:
        """初期化.

        Args:
            agents: Agent リスト
            pattern: 協調パターン (sequential, concurrent, handoff)
            shared_context: 共有コンテキスト
        """
        self._agents = agents
        self._pattern = pattern
        self._context = shared_context or SharedContext()
        self._logger = logging.getLogger(__name__)

    async def execute(self, task: str) -> dict[str, Any]:
        """協調パターンに従って実行.

        Args:
            task: タスク内容

        Returns:
            実行結果
        """
        self._logger.info(f"協調実行開始: pattern={self._pattern}")

        if self._pattern == "sequential":
            return await self._execute_sequential(task)
        elif self._pattern == "concurrent":
            return await self._execute_concurrent(task)
        elif self._pattern == "handoff":
            return await self._execute_handoff(task)
        else:
            msg = f"Unknown pattern: {self._pattern}"
            raise ValueError(msg)

    async def _execute_sequential(self, task: str | dict[str, Any]) -> dict[str, Any]:
        """順次実行.

        Args:
            task: タスク内容（文字列または辞書）

        Returns:
            最終結果（フラット化された構造）
        """
        self._logger.info("Sequential 実行開始")

        # taskが辞書の場合はそのまま使用、文字列の場合は{"task": task}に変換
        if isinstance(task, dict):
            result = task
        else:
            result = {"task": task}
        agent_results: dict[str, Any] = {}

        for i, agent in enumerate(self._agents):
            agent_name = agent.__class__.__name__
            self._logger.info(f"Agent {i + 1}/{len(self._agents)} 実行中: {agent_name}")

            # ノード開始コールバック
            if hasattr(self, "_on_node_start") and self._on_node_start:
                try:
                    await self._on_node_start(agent_name, {"index": i, "input": result})
                except Exception as e:
                    self._logger.warning(f"on_node_start callback failed: {e}")

            try:
                # Agent を実行
                result = await agent.run(result)

                # 共有コンテキストに保存
                self._context.set(f"agent_{i}_{agent_name}", result)
                agent_results[agent_name] = result

                # ノード完了コールバック
                if hasattr(self, "_on_node_complete") and self._on_node_complete:
                    try:
                        await self._on_node_complete(agent_name, result)
                    except Exception as e:
                        self._logger.warning(f"on_node_complete callback failed: {e}")

            except Exception as e:
                self._logger.error(f"Agent {i} ({agent_name}) failed: {e}")
                agent_results[agent_name] = {
                    "error": str(e),
                    "error_type": type(e).__name__,
                }
                raise

        self._logger.info("Sequential 実行完了")

        # 一貫した結果構造を返す
        return {
            "final_result": result,
            "agent_results": agent_results,
            "pattern": "sequential",
            "agents_executed": len(self._agents),
        }

    async def _execute_concurrent(self, task: str) -> dict[str, Any]:
        """並行実行.

        Args:
            task: タスク内容

        Returns:
            全 Agent の結果
        """
        self._logger.info("Concurrent 実行開始")

        # 全 Agent を並行実行
        tasks = [agent.run({"task": task}) for agent in self._agents]
        results = await asyncio.gather(*tasks, return_exceptions=True)

        # 結果を集約
        successful_results = []
        errors = []

        for i, result in enumerate(results):
            if isinstance(result, Exception):
                errors.append({
                    "agent_index": i,
                    "error": str(result),
                })
            else:
                successful_results.append(result)
                # 共有コンテキストに保存
                agent_name = self._agents[i].__class__.__name__
                self._context.set(f"agent_{i}_{agent_name}", result)

        self._logger.info(f"Concurrent 実行完了: 成功={len(successful_results)}, 失敗={len(errors)}")

        return {
            "results": successful_results,
            "errors": errors,
            "pattern": "concurrent",
            "agents_executed": len(self._agents),
        }

    async def _execute_handoff(self, task: str) -> dict[str, Any]:
        """委譲実行.

        Args:
            task: タスク内容

        Returns:
            最終結果
        """
        self._logger.info("Handoff 実行開始")

        current_input = {"task": task}
        handoff_chain = []

        for i, agent in enumerate(self._agents):
            self._logger.info(f"Agent {i + 1}/{len(self._agents)} 実行中")

            # Agent を実行
            result = await agent.run(current_input)

            # 履歴に追加
            # Agent に name 属性があればそれを使用、なければクラス名
            agent_name = getattr(agent, "name", agent.__class__.__name__)
            handoff_chain.append({
                "agent": agent_name,
                "result": result,
            })

            # 共有コンテキストに保存
            self._context.set(f"agent_{i}_{agent_name}", result)

            # 委譲判定
            if result.get("handoff_to"):
                # 次の Agent に委譲
                current_input = result
                self._logger.info(f"委譲: {result.get('handoff_to')}")
            else:
                # 完了
                self._logger.info(f"Agent {agent_name} で完了")
                break

        self._logger.info("Handoff 実行完了")

        return {
            "final_result": handoff_chain[-1]["result"] if handoff_chain else None,
            "handoff_chain": handoff_chain,
            "pattern": "handoff",
            "agents_executed": len(handoff_chain),
        }


class MultiAgentWorkflow:
    """Multi-Agent Workflowファクトリー.

    責務：
    - WorkflowConfigを生成
    - 複数の協調パターンをサポート
    - AgentFlowEngineと統合

    Example:
        >>> workflow = MultiAgentWorkflow.create(
        ...     workflow_id="research-pipeline",
        ...     agents=[research_agent, analysis_agent, report_agent],
        ...     pattern="sequential"
        ... )
        >>> engine = AgentFlowEngine()
        >>> engine.register_workflow(workflow)
        >>> result = await engine.execute("research-pipeline", {"task": "市場調査"})
    """

    @staticmethod
    def create(
        workflow_id: str,
        agents: list[AgentBlock],
        pattern: str = "sequential",
        llm_client: Any = None,
        shared_context: SharedContext | None = None,
    ) -> WorkflowConfig:
        """Multi-Agent Workflow を作成.

        Args:
            workflow_id: ワークフロー ID
            agents: Agent リスト
            pattern: 協調パターン (sequential, concurrent, handoff)
            llm_client: LLM クライアント（Router 用）
            shared_context: 共有コンテキスト（オプション、記憶システム付き可能）

        Returns:
            WorkflowConfig
        """
        # SharedContext を作成（提供されていない場合）
        if shared_context is None:
            shared_context = SharedContext()

        # AgentCoordinator を作成
        coordinator = AgentCoordinator(
            agents=agents,
            pattern=pattern,
            shared_context=shared_context,
        )

        # WorkflowConfig を構築
        # Coordinatorが全てのAgentを管理するため、Coordinatorノードのみを作成
        nodes = [
            {
                "id": "coordinator",
                "type": "coordinator",
                "coordinator": coordinator,
            }
        ]
        edges = []  # Coordinatorノードのみなのでedgeは不要

        return WorkflowConfig(
            workflow_id=workflow_id,
            name=f"Multi-Agent Workflow ({pattern})",
            description=f"{len(agents)}個の Agent を{pattern}パターンで協調",
            nodes=nodes,
            edges=edges,
            config={
                "pattern": pattern,
                "num_agents": len(agents),
                "shared_context": shared_context,
                "coordinator": coordinator,  # デバッグ用
            },
        )

    @staticmethod
    def create_with_router(
        workflow_id: str,
        agents: dict[str, AgentBlock],
        llm_client: Any,
    ) -> WorkflowConfig:
        """Router 付き Multi-Agent Workflow を作成.

        Args:
            workflow_id: ワークフロー ID
            agents: Agent 辞書 {"名前": Agent}
            llm_client: LLM クライアント

        Returns:
            WorkflowConfig
        """
        # AgentRouter を作成
        router = AgentRouter(
            agents=agents,
            llm_client=llm_client,
        )

        return WorkflowConfig(
            workflow_id=workflow_id,
            name="Multi-Agent Workflow (Router)",
            description=f"{len(agents)}個の Agent から動的に選択",
            nodes=[
                {
                    "id": "router",
                    "type": "router",
                    "router": router,
                },
            ],
            edges=[],
            config={
                "pattern": "router",
                "num_agents": len(agents),
            },
        )


__all__ = [
    "SharedContext",
    "AgentRouter",
    "AgentCoordinator",
    "MultiAgentWorkflow",
]

