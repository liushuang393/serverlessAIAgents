"""BaseBuilder - コードビルダー基底クラス."""

from __future__ import annotations

from abc import ABC, abstractmethod
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from agentflow.core.interfaces import CodeGenOptions, WorkflowDefinition


class BaseBuilder(ABC):
    """コードビルダー基底クラス."""

    @abstractmethod
    async def build(
        self,
        workflow: WorkflowDefinition,
        options: CodeGenOptions,
    ) -> dict[str, str]:
        """コードをビルド.

        Args:
            workflow: ワークフロー定義
            options: 生成オプション

        Returns:
            ファイルパス -> 内容のマップ
        """
        ...

    def _get_workflow_info(
        self,
        workflow: WorkflowDefinition,
    ) -> dict[str, str]:
        """ワークフロー情報を取得."""
        return {
            "id": workflow.id,
            "name": workflow.name,
            "description": workflow.description,
            "node_count": str(len(workflow.nodes)),
            "edge_count": str(len(workflow.edges)),
        }

    def _generate_node_list(
        self,
        workflow: WorkflowDefinition,
    ) -> str:
        """ノードリストを生成."""
        lines = []
        for node in workflow.nodes:
            lines.append(f"# - {node.id}: {node.agent_type}")
        return "\n".join(lines) if lines else "# No nodes defined"


__all__ = ["BaseBuilder"]
