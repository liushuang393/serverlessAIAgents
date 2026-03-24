"""kernel/interfaces/tool_provider.py — ツールプロバイダの抽象."""
from __future__ import annotations

from typing import Any, Protocol, runtime_checkable


@runtime_checkable
class ToolProviderService(Protocol):
    """ツール提供の抽象インターフェース.

    Kernel はこの Protocol を通じてツールにアクセスし、
    MCP / ローカル / リモートなど具体的な提供元を知らない。
    """

    async def get_tool(self, name: str) -> Any:
        """名前でツールを取得.

        Args:
            name: ツール名

        Returns:
            ツールオブジェクト（見つからない場合は None）
        """
        ...

    async def list_tools(self) -> list[dict[str, Any]]:
        """利用可能なツール一覧を返す.

        Returns:
            ツール情報の辞書リスト
        """
        ...
