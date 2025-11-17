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

技術栈：
- AgentBlock: Agent 基類
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
    """Agent 間の共有コンテキスト.

    職責：
    - Agent 間で状態を共有
    - 履歴管理
    - スレッドセーフ

    Example:
        >>> context = SharedContext()
        >>> context.set("result_a", {"data": "A"})
        >>> result = context.get("result_a")
    """

    def __init__(self) -> None:
        """初期化."""
        self._data: dict[str, Any] = {}
        self._history: list[dict[str, Any]] = []
        self._logger = logging.getLogger(__name__)

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

    def clear(self) -> None:
        """クリア."""
        self._data.clear()
        self._history.clear()


class AgentRouter(AgentBlock):
    """Agent ルーティング - タスクに応じて Agent を選択.

    職責：
    - タスクを分析
    - 最適な Agent を選択
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
    """Agent 協調制御.

    職責：
    - Sequential パターン（順次実行）
    - Concurrent パターン（並行実行）
    - Handoff パターン（動的委譲）

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

    async def _execute_sequential(self, task: str) -> dict[str, Any]:
        """順次実行.

        Args:
            task: タスク内容

        Returns:
            最終結果
        """
        self._logger.info("Sequential 実行開始")

        result = {"task": task}

        for i, agent in enumerate(self._agents):
            self._logger.info(f"Agent {i + 1}/{len(self._agents)} 実行中")

            # Agent を実行
            result = await agent.run(result)

            # 共有コンテキストに保存
            agent_name = agent.__class__.__name__
            self._context.set(f"agent_{i}_{agent_name}", result)

        self._logger.info("Sequential 実行完了")

        return {
            "final_result": result,
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
    """Multi-Agent Workflow 工厂.

    職責：
    - WorkflowConfig を生成
    - 複数の協調パターンをサポート
    - AgentFlowEngine と統合

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
    ) -> WorkflowConfig:
        """Multi-Agent Workflow を作成.

        Args:
            workflow_id: ワークフロー ID
            agents: Agent リスト
            pattern: 協調パターン (sequential, concurrent, handoff)
            llm_client: LLM クライアント（Router 用）

        Returns:
            WorkflowConfig
        """
        # SharedContext を作成
        shared_context = SharedContext()

        # AgentCoordinator を作成
        coordinator = AgentCoordinator(
            agents=agents,
            pattern=pattern,
            shared_context=shared_context,
        )

        # WorkflowConfig を構築
        # 注意：実際の実装では PocketFlow のノード定義が必要
        # ここでは簡略化のため、メタデータのみ設定
        nodes = []
        edges = []

        # Agent ノードを追加
        for i, agent in enumerate(agents):
            nodes.append({
                "id": f"agent_{i}",
                "type": "agent",
                "agent": agent,
            })

            # エッジを追加（Sequential の場合）
            if pattern == "sequential" and i > 0:
                edges.append({
                    "from": f"agent_{i - 1}",
                    "to": f"agent_{i}",
                })

        # Coordinator ノードを追加
        nodes.append({
            "id": "coordinator",
            "type": "coordinator",
            "coordinator": coordinator,
        })

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

