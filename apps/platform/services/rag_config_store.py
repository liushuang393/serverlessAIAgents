"""RagConfigStore - RAG 設定変更の永続化 + イベントバス.

Platform が RAG 設定を変更した際に:
1. app_config.json を更新
2. SSE 購読者にイベントを発火

これにより、実行中の App が ConfigWatcher 経由で設定変更を受け取り、
再起動なしで RAGEngine をホットリロードできる。

使用例:
    >>> store = RagConfigStore()
    >>> # PATCH ハンドラーから呼ぶ
    >>> await store.fire_config_change("faq_system", new_rag_config)
    >>>
    >>> # SSE エンドポイントから購読
    >>> async for event in store.subscribe("faq_system"):
    ...     yield ServerSentEvent(data=json.dumps(event))
"""

from __future__ import annotations

import asyncio
import contextlib
import logging
from collections.abc import AsyncGenerator  # noqa: TC003
from typing import Any


logger = logging.getLogger(__name__)


class RagConfigStore:
    """RAG 設定変更のイベントバス.

    各 App 名に対して購読キューを管理し、設定変更時に全購読者へ
    非同期でイベントをブロードキャストする。

    Attributes:
        _subscribers: app_name → Queue リストのマッピング
    """

    def __init__(self) -> None:
        """初期化."""
        # app_name → weakref.WeakSet[asyncio.Queue] で管理
        # WeakSet を使うことで、購読者が切断した際に自動的に GC される
        self._subscribers: dict[str, list[asyncio.Queue[dict[str, Any]]]] = {}
        self._lock = asyncio.Lock()

    async def fire_config_change(
        self,
        app_name: str,
        rag_config: dict[str, Any],
    ) -> int:
        """設定変更イベントを全購読者にブロードキャスト.

        Args:
            app_name: 対象アプリ名
            rag_config: 新しい RAG 設定辞書（contracts.rag の内容）

        Returns:
            イベントを送信した購読者数
        """
        event: dict[str, Any] = {
            "event_type": "rag_config_changed",
            "app_name": app_name,
            "rag_config": rag_config,
        }

        async with self._lock:
            queues = self._subscribers.get(app_name, [])
            # 切断済みのキューを除外してコピー
            active_queues = [q for q in queues if not q.empty() or True]
            count = 0
            for queue in active_queues:
                try:
                    queue.put_nowait(event)
                    count += 1
                except asyncio.QueueFull:
                    logger.warning(
                        "購読キュー満杯のためイベントをスキップ: app=%s",
                        app_name,
                    )

        logger.info(
            "RAG設定変更イベントをブロードキャスト: app=%s, 購読者=%d名",
            app_name,
            count,
        )
        return count

    async def subscribe(
        self,
        app_name: str,
    ) -> AsyncGenerator[dict[str, Any]]:
        """指定アプリの設定変更イベントを SSE ストリームとして購読.

        Args:
            app_name: 購読対象アプリ名

        Yields:
            設定変更イベント辞書
        """
        queue: asyncio.Queue[dict[str, Any]] = asyncio.Queue(maxsize=50)

        async with self._lock:
            if app_name not in self._subscribers:
                self._subscribers[app_name] = []
            self._subscribers[app_name].append(queue)

        logger.info("SSE 購読開始: app=%s", app_name)

        try:
            while True:
                event = await queue.get()
                yield event
        except asyncio.CancelledError:
            logger.info("SSE 購読キャンセル: app=%s", app_name)
        except GeneratorExit:
            logger.info("SSE 購読終了: app=%s", app_name)
        finally:
            # 購読解除
            async with self._lock:
                subscribers = self._subscribers.get(app_name, [])
                with contextlib.suppress(ValueError):
                    subscribers.remove(queue)
            logger.info("SSE 購読解除: app=%s", app_name)

    def subscriber_count(self, app_name: str) -> int:
        """指定アプリの現在の購読者数を返す.

        Args:
            app_name: アプリ名

        Returns:
            購読者数
        """
        return len(self._subscribers.get(app_name, []))


# モジュールレベルシングルトン（Platform main.py で初期化）
_rag_config_store: RagConfigStore | None = None


def init_rag_config_store() -> RagConfigStore:
    """RagConfigStore シングルトンを初期化・返却."""
    global _rag_config_store
    if _rag_config_store is None:
        _rag_config_store = RagConfigStore()
    return _rag_config_store


def get_rag_config_store() -> RagConfigStore:
    """RagConfigStore シングルトンを取得.

    Raises:
        RuntimeError: init_rag_config_store() が未呼び出しの場合
    """
    if _rag_config_store is None:
        msg = "RagConfigStore が未初期化です。init_rag_config_store() を先に呼んでください。"
        raise RuntimeError(msg)
    return _rag_config_store


__all__ = [
    "RagConfigStore",
    "get_rag_config_store",
    "init_rag_config_store",
]
