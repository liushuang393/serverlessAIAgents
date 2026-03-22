"""サンドボックスノード.

Flow内でサンドボックスコード実行を行うノード。
Daytonaスタイルのライフサイクル管理を統合。

設計原則:
- Flow統合: FlowNodeとして使用可能
- 安全実行: 隔離されたサンドボックス内でコード実行
- 状態追跡: 実行結果をFlowContextに保存

使用例:
    >>> from kernel.flow import create_flow
    >>> from kernel.flow.sandbox_node import SandboxNode
    >>>
    >>> node = SandboxNode(
    ...     id="code_exec",
    ...     name="コード実行",
    ...     code_source=lambda ctx: ctx.get("code"),
    ... )
    >>> flow = create_flow("sandbox_flow").add_node(node)
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any

from kernel.flow.types import NextAction, NodeResult, NodeType


if TYPE_CHECKING:
    from collections.abc import Callable

    from infrastructure.sandbox import (
        ExecutionResult,
        SandboxConfig,
    )
    from kernel.flow.context import FlowContext


logger = logging.getLogger(__name__)


@dataclass
class SandboxNode:
    """サンドボックス実行ノード.

    Flow内でコードを安全に実行するノード。

    Attributes:
        id: ノードID
        name: ノード名
        code_source: コード取得関数（ctx -> str）
        provider: サンドボックスプロバイダ
        config: サンドボックス設定
        packages: インストールするパッケージ
        on_success: 成功時コールバック
        on_error: エラー時コールバック
    """

    id: str
    name: str
    code_source: Callable[[FlowContext], str]
    provider: str = "docker"
    config: SandboxConfig | None = None
    packages: list[str] = field(default_factory=list)
    timeout: float | None = None
    on_success: Callable[[FlowContext, ExecutionResult], Any] | None = None
    on_error: Callable[[FlowContext, ExecutionResult], Any] | None = None
    node_type: NodeType = field(default=NodeType.AGENT)
    label: str = ""
    icon: str = "🔒"

    def __post_init__(self) -> None:
        """初期化."""
        self._logger = logging.getLogger(f"kernel.flow.sandbox_node.{self.id}")
        if not self.label:
            self.label = self.name

    async def execute(self, ctx: FlowContext) -> NodeResult:
        """サンドボックスでコードを実行.

        Args:
            ctx: FlowContext

        Returns:
            NodeResult
        """
        try:
            # コードを取得
            code = self.code_source(ctx)
            if not code:
                return NodeResult(
                    success=False,
                    data={"error": "コードが空です"},
                    action=NextAction.STOP,
                )

            self._logger.info(f"サンドボックス実行開始: {self.id}")

            # サンドボックスを作成・実行
            from infrastructure.sandbox import ManagedSandbox

            async with await ManagedSandbox.create(
                self.provider,
                self.config,
            ) as sandbox:
                await sandbox.start()

                result = await sandbox.execute(
                    code,
                    timeout=self.timeout,
                    packages=self.packages if self.packages else None,
                )

            # 結果をコンテキストに保存
            result_data = {
                "stdout": result.stdout,
                "stderr": result.stderr,
                "exit_code": result.exit_code,
                "duration_ms": result.duration_ms,
                "error": result.error,
            }
            ctx.set_result(self.id, result_data)

            # 成功/エラーコールバック
            if result.exit_code == 0:
                if self.on_success:
                    self.on_success(ctx, result)
                self._logger.info(f"サンドボックス実行成功: {self.id}")
                return NodeResult(success=True, data=result_data, action=NextAction.CONTINUE)
            if self.on_error:
                self.on_error(ctx, result)
            self._logger.warning(f"サンドボックス実行失敗: {self.id}, exit_code={result.exit_code}")
            return NodeResult(success=False, data=result_data, action=NextAction.STOP)

        except Exception as e:
            self._logger.exception(f"サンドボックスノードエラー: {e}")
            return NodeResult(
                success=False,
                data={"error": str(e), "error_type": type(e).__name__},
                action=NextAction.STOP,
            )


@dataclass
class WorkspaceNode:
    """ワークスペース実行ノード.

    ワークスペース内でファイル操作とコード実行を行うノード。

    Attributes:
        id: ノードID
        name: ノード名
        workspace_name: ワークスペース名
        files_source: ファイル取得関数（ctx -> dict[path, content]）
        code_source: コード取得関数（ctx -> str）
        provider: サンドボックスプロバイダ
    """

    id: str
    name: str
    workspace_name: str = ""
    files_source: Callable[[FlowContext], dict[str, bytes | str]] | None = None
    code_source: Callable[[FlowContext], str] | None = None
    provider: str = "docker"
    config: SandboxConfig | None = None
    node_type: NodeType = field(default=NodeType.AGENT)
    label: str = ""
    icon: str = "📁"

    def __post_init__(self) -> None:
        """初期化."""
        self._logger = logging.getLogger(f"kernel.flow.workspace_node.{self.id}")
        if not self.label:
            self.label = self.name

    async def execute(self, ctx: FlowContext) -> NodeResult:
        """ワークスペースでコードを実行.

        Args:
            ctx: FlowContext

        Returns:
            NodeResult
        """
        try:
            from infrastructure.sandbox import ExecutionResult, Workspace

            ws_name = self.workspace_name or f"flow-{self.id}"
            self._logger.info(f"ワークスペース実行開始: {ws_name}")

            async with await Workspace.create(
                name=ws_name,
                provider=self.provider,
                config=self.config,
            ) as workspace:
                # ファイルを配置
                if self.files_source:
                    files = self.files_source(ctx)
                    for path, content in files.items():
                        await workspace.write_file(path, content)

                # コードを実行
                if self.code_source:
                    code = self.code_source(ctx)
                    result = await workspace.execute(code)
                else:
                    # コードがない場合はファイル一覧を返す
                    file_list = await workspace.list_files()
                    result = ExecutionResult(
                        stdout="\n".join(f.path for f in file_list),
                        exit_code=0,
                    )

                # 結果をコンテキストに保存
                result_data = {
                    "workspace_id": workspace.workspace_id,
                    "stdout": result.stdout,
                    "stderr": result.stderr,
                    "exit_code": result.exit_code,
                    "duration_ms": result.duration_ms,
                }
                ctx.set_result(self.id, result_data)

                if result.exit_code == 0:
                    return NodeResult(success=True, data=result_data, action=NextAction.CONTINUE)
                return NodeResult(success=False, data=result_data, action=NextAction.STOP)

        except Exception as e:
            self._logger.exception(f"ワークスペースノードエラー: {e}")
            return NodeResult(
                success=False,
                data={"error": str(e), "error_type": type(e).__name__},
                action=NextAction.STOP,
            )


__all__ = ["SandboxNode", "WorkspaceNode"]
