"""RAGCapableMixin - RAG 能力を Agent に付与する Mixin クラス.

CapabilityBundle を受け取り、retrieve_context() で RAG 検索を提供する。
RAG が無効（bundle=None / rag_engine=None）の場合は空リストを返す
Graceful Degradation を実装している。

使用例:
    >>> from agentflow.agents.mixins import RAGCapableMixin
    >>> from agentflow.core.agent_block import AgentBlock
    >>>
    >>> class MyAgent(RAGCapableMixin, AgentBlock):
    ...     def __init__(self, bundle: CapabilityBundle) -> None:
    ...         super().__init__()
    ...         self.set_capability_bundle(bundle)
    ...
    ...     async def process(self, input_data: dict) -> dict:
    ...         docs = await self.retrieve_context(input_data["query"])
    ...         # docs が空でも正常動作（RAG設定なし / 検索結果なし）
    ...         return {"context_docs": docs}
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from agentflow.bootstrap.capability_bundle import CapabilityBundle


logger = logging.getLogger(__name__)


class RAGCapableMixin:
    """RAG 能力を付与する Mixin クラス.

    CapabilityBundle を保持し、retrieve_context() を通じて
    RAGPipeline.search() を呼び出す。

    Graceful Degradation:
        - bundle が None → 空リスト返却
        - rag_engine が None（disabled / 設定なし）→ 空リスト返却
        - 検索例外 → ログ警告して空リスト返却

    使用方法:
        1. クラス定義で RAGCapableMixin を継承（AgentBlock より先に）
        2. __init__ で set_capability_bundle(bundle) を呼ぶ
        3. process() / run() 内で await self.retrieve_context(query)
    """

    _bundle: CapabilityBundle | None = None

    def set_capability_bundle(self, bundle: CapabilityBundle) -> None:
        """CapabilityBundle を設定.

        Args:
            bundle: AppCapabilityBootstrapper が構築した能力束
        """
        self._bundle = bundle

    async def retrieve_context(self, query: str) -> list[dict[str, Any]]:
        """RAG 検索を実行してコンテキストドキュメントを返す.

        Args:
            query: 検索クエリ文字列

        Returns:
            検索結果ドキュメントのリスト。
            RAG が無効または検索失敗時は空リストを返す（Graceful Degradation）。

        各ドキュメントは dict で、最低限 "document" または "content" キーを含む。
        """
        if self._bundle is None or self._bundle.rag_engine is None:
            logger.debug("RAG無効: 空のコンテキストを返却 (query=%s...)", query[:30])
            return []

        try:
            results: list[dict[str, Any]] = await self._bundle.rag_engine.search(query)
            logger.debug(
                "RAG検索完了: %d件取得 (query=%s...)",
                len(results),
                query[:30],
            )
            return results
        except Exception as exc:
            logger.warning(
                "RAG検索エラー（Graceful Degradation）: %s",
                exc,
                exc_info=True,
            )
            return []


__all__ = ["RAGCapableMixin"]
