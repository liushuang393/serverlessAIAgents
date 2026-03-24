"""第三者フレームワーク用アダプター基底.

LlamaIndex / LangChain 等の外部フレームワークを
RetrievalBackend として統合するための基底クラス。

使用例（将来実装）:
    >>> class LlamaIndexAdapter(ThirdPartyAdapter):
    ...     async def retrieve(self, query: RetrievalQuery) -> RetrievalResult:
    ...         index = self._get_framework_instance()
    ...         response = index.query(query.query)
    ...         return self._convert_result(response)
"""

from __future__ import annotations

import logging
from abc import abstractmethod
from typing import Any

from apps.faq_system.backend.mcp.backends.base import (
    BackendType,
    RetrievalBackend,
    RetrievalQuery,
    RetrievalResult,
)


logger = logging.getLogger(__name__)


class ThirdPartyAdapter(RetrievalBackend):
    """第三者フレームワーク用アダプター基底.

    外部フレームワーク（LlamaIndex, LangChain 等）を
    RetrievalBackend インターフェースにラップ。

    サブクラスで以下を実装:
    - _create_framework_instance(): フレームワークインスタンス生成
    - retrieve(): 検索実行
    - health_check(): ヘルスチェック
    """

    def __init__(
        self,
        framework_name: str,
        framework_config: dict[str, Any] | None = None,
    ) -> None:
        """初期化.

        Args:
            framework_name: フレームワーク名（例: "llamaindex", "langchain"）
            framework_config: フレームワーク固有設定
        """
        # 第三者フレームワークは外部API扱い
        super().__init__(
            backend_type=BackendType.EXTERNAL_API,
            name=f"adapter:{framework_name}",
        )
        self._framework_name = framework_name
        self._framework_config = framework_config or {}
        self._framework_instance: Any = None  # 理由: フレームワーク型は動的
        self._initialized = False

    async def initialize(self) -> None:
        """フレームワークインスタンスを初期化."""
        if self._initialized:
            return
        self._framework_instance = await self._create_framework_instance()
        self._initialized = True
        self._logger.info(
            "ThirdPartyAdapter 初期化完了: framework=%s",
            self._framework_name,
        )

    @abstractmethod
    async def _create_framework_instance(self) -> Any:  # 理由: フレームワーク型は事前に決定不可
        """フレームワークインスタンスを生成.

        サブクラスでオーバーライドし、LlamaIndex Index や
        LangChain VectorStore 等を返す。

        Returns:
            フレームワークインスタンス
        """

    async def health_check(self) -> bool:
        """ヘルスチェック."""
        return self._initialized and self._framework_instance is not None

    async def cleanup(self) -> None:
        """クリーンアップ."""
        self._framework_instance = None
        self._initialized = False

