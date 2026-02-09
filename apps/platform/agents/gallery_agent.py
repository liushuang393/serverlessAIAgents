# -*- coding: utf-8 -*-
"""GalleryAgent - Gallery検索・発見Agent.

自然言語でGallery検索を行う。

使用例:
    >>> agent = GalleryAgent()
    >>> result = await agent.run({"query": "PDFを処理できるAgentを探して"})
"""

from __future__ import annotations

from typing import Any

from agentflow.core.agent_block import AgentBlock
from apps.platform.services.gallery_service import GalleryService
from apps.platform.schemas.gallery_schemas import GallerySearchRequest


class GalleryAgent(AgentBlock):
    """Gallery検索Agent.

    自然言語のクエリを解析し、適切なコンポーネントを検索・推薦する。
    """

    name = "gallery-agent"

    def __init__(self, llm_client: Any = None) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント
        """
        super().__init__()
        self._llm_client = llm_client
        self._gallery = GalleryService()

    def _get_system_prompt(self) -> str:
        """システムプロンプトを取得."""
        return """あなたはAgentFlow Galleryの検索アシスタントです。

ユーザーのリクエストを理解し、適切なAgent、Tool、Skill、Flowを検索して推薦します。

タスク:
1. ユーザーのリクエストから検索キーワードと条件を抽出
2. 適切なコンポーネントを検索
3. 検索結果を分かりやすく説明
4. 使用方法のアドバイスを提供

検索可能なコンポーネントタイプ:
- agent: 自律的にタスクを実行するAgent
- tool: 特定の機能を提供するTool
- skill: 再利用可能なSkill
- flow: ワークフロー定義
- engine: エンジンテンプレート
- template: コードテンプレート

回答は日本語で、簡潔かつ分かりやすく行ってください。"""

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """Agent を実行.

        Args:
            input_data: 入力データ（query キー必須）

        Returns:
            検索結果と推薦
        """
        query = input_data.get("query", "")

        if not query:
            return {
                "success": False,
                "error": "検索クエリが指定されていません",
            }

        # 検索を実行
        result = await self._gallery.search(query)

        # 結果を整形
        items = [
            {
                "id": item.id,
                "name": item.name,
                "type": item.type.value,
                "description": item.description,
                "icon": item.icon,
                "rating": item.rating,
                "downloads": item.downloads,
            }
            for item in result.items
        ]

        return {
            "success": True,
            "query": query,
            "total": result.total,
            "items": items,
            "has_more": result.has_more,
        }


__all__ = ["GalleryAgent"]
