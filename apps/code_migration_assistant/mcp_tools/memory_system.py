"""MemorySystem MCP Tool.

このモジュールは記憶システムを提供するMCPツールを実装します。

主な機能:
    - 移行パターンの記憶と想起
    - 移行履歴の記録と検索
    - ベストプラクティスの蓄積
"""

from typing import Any

from agentflow import MCPTool, MCPToolRequest, MCPToolResponse


class MemorySystem(MCPTool):
    """MemorySystem MCP Tool.

    移行パターン、履歴、ベストプラクティスを記憶・想起します。

    Input:
        - operation: 操作タイプ（"remember" または "recall"）
        - data: 操作データ
            - remember操作の場合:
                - content: 記憶する内容（必須）
                - topic: トピック（必須）
                - memory_type: 記憶タイプ（"pattern", "history", "best_practice"）
                - importance_score: 重要度スコア（0.0-1.0）
                - metadata: メタデータ
            - recall操作の場合:
                - query: 検索クエリ（必須）
                - memory_type: 記憶タイプフィルタ（オプション）
                - top_k: 取得件数（デフォルト: 5）

    Output:
        - remember操作の場合:
            - memory_id: 記憶ID
            - success: 成功フラグ
        - recall操作の場合:
            - memories: 想起された記憶リスト
            - count: 件数

    Note:
        このMCPツールは実際のMemoryManagerを使用します。
        実際の実装では、MemoryManagerを注入する必要があります。
    """

    def __init__(self, memory_manager: Any | None = None) -> None:
        """MemorySystemを初期化.

        Args:
            memory_manager: Memory Manager（記憶システム）
        """
        super().__init__(tool_name="memory_system", version="1.0.0")
        self.memory_manager = memory_manager

    async def handle_request(self, request: MCPToolRequest) -> MCPToolResponse:
        """記憶システム操作を実行.

        Args:
            request: MCPツールリクエスト

        Returns:
            MCPツールレスポンス
        """
        # 入力パラメータを取得
        operation = request.input.get("operation")
        data = request.input.get("data", {})

        # 必須パラメータチェック
        if not operation:
            return MCPToolResponse(
                success=False,
                errors=["operation is required"],
            )

        if operation not in ["remember", "recall"]:
            return MCPToolResponse(
                success=False,
                errors=["operation must be 'remember' or 'recall'"],
            )

        if not self.memory_manager:
            return MCPToolResponse(
                success=False,
                errors=["Memory Manager is not configured"],
            )

        # 操作実行
        try:
            if operation == "remember":
                result = await self._remember(data)
            else:  # recall
                result = await self._recall(data)

            return MCPToolResponse(
                success=True,
                output=result,
            )

        except Exception as e:
            return MCPToolResponse(
                success=False,
                errors=[f"Memory operation failed: {e!s}"],
            )

    async def _remember(self, data: dict[str, Any]) -> dict[str, Any]:
        """記憶を保存.

        Args:
            data: 記憶データ

        Returns:
            結果
        """
        # 必須パラメータチェック
        content = data.get("content")
        topic = data.get("topic")

        if not content:
            msg = "content is required for remember operation"
            raise ValueError(msg)

        if not topic:
            msg = "topic is required for remember operation"
            raise ValueError(msg)

        # オプションパラメータ
        memory_type = data.get("memory_type", "pattern")
        importance_score = data.get("importance_score", 0.5)
        metadata = data.get("metadata", {})

        # 記憶を保存
        memory_id = await self.memory_manager.remember(
            content=content,
            topic=topic,
            memory_type=memory_type,
            importance_score=importance_score,
            metadata=metadata,
        )

        return {
            "memory_id": memory_id,
            "success": True,
        }

    async def _recall(self, data: dict[str, Any]) -> dict[str, Any]:
        """記憶を想起.

        Args:
            data: 検索データ

        Returns:
            結果
        """
        # 必須パラメータチェック
        query = data.get("query")

        if not query:
            msg = "query is required for recall operation"
            raise ValueError(msg)

        # オプションパラメータ
        memory_type = data.get("memory_type")
        top_k = data.get("top_k", 5)

        # 記憶を検索
        memories = await self.memory_manager.recall(
            query=query,
            memory_type=memory_type,
            top_k=top_k,
        )

        return {
            "memories": memories,
            "count": len(memories),
        }

