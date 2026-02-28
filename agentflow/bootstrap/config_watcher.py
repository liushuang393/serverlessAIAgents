"""ConfigWatcher - Platform SSE を購読し RAGEngine をホットリロードするクラス.

Platform から SSE イベントを受信し、config変更時に CapabilityBundle の
rag_engine をアトミックに入れ替える。

接続失敗時は指数バックオフで自動再接続する。
platform_url=None の場合は起動しない（シンプルな環境向け）。

使用例:
    >>> watcher = ConfigWatcher(app_name="faq_system", platform_url="http://localhost:8001")
    >>> task = asyncio.create_task(watcher.watch(bundle))
    >>> # ... app runs ...
    >>> await watcher.stop()
"""

from __future__ import annotations

import asyncio
import logging
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from agentflow.bootstrap.capability_bundle import CapabilityBundle


logger = logging.getLogger(__name__)

# 再接続バックオフ設定
_INITIAL_BACKOFF = 1.0
_MAX_BACKOFF = 60.0
_BACKOFF_FACTOR = 2.0

# SSE イベントパス
_SSE_PATH = "/api/studios/framework/rag/events"


class ConfigWatcher:
    """Platform SSE 購読クラス.

    バックグラウンドタスクとして起動し、設定変更時に
    CapabilityBundle.rag_engine を新しいインスタンスに置換する。

    Attributes:
        _app_name: 購読対象アプリ名
        _platform_url: Platform URL（None なら無効）
        _stop_event: 停止シグナル
    """

    def __init__(
        self,
        app_name: str,
        platform_url: str | None = None,
    ) -> None:
        """ConfigWatcher を初期化.

        Args:
            app_name: 監視対象アプリ名
            platform_url: Platform URL（None なら watch() は即終了）
        """
        self._app_name = app_name
        self._platform_url = platform_url
        self._stop_event = asyncio.Event()

    async def watch(self, bundle: CapabilityBundle) -> None:
        """バックグラウンドタスクとして起動。設定変更時に rag_engine を入れ替える.

        Args:
            bundle: 更新対象の CapabilityBundle
        """
        if not self._platform_url:
            logger.debug("platform_url 未設定: ConfigWatcher を無効化")
            return

        backoff = _INITIAL_BACKOFF
        sse_url = f"{self._platform_url.rstrip('/')}{_SSE_PATH}?app={self._app_name}"

        while not self._stop_event.is_set():
            try:
                await self._subscribe_and_process(sse_url, bundle)
                # 正常終了（stop_event が set された場合）
                break
            except asyncio.CancelledError:
                logger.info("ConfigWatcher キャンセルされました: %s", self._app_name)
                break
            except Exception as exc:
                if self._stop_event.is_set():
                    break
                logger.warning(
                    "ConfigWatcher 接続失敗 (%.1fs 後に再試行): %s",
                    backoff,
                    exc,
                )
                try:
                    await asyncio.wait_for(
                        asyncio.shield(self._stop_event.wait()),
                        timeout=backoff,
                    )
                    break  # stop_event が set された
                except TimeoutError:
                    pass  # バックオフ後に再接続
                backoff = min(backoff * _BACKOFF_FACTOR, _MAX_BACKOFF)

        logger.info("ConfigWatcher 終了: %s", self._app_name)

    async def _subscribe_and_process(
        self,
        sse_url: str,
        bundle: CapabilityBundle,
    ) -> None:
        """SSE に接続してイベントを処理.

        Args:
            sse_url: SSE エンドポイント URL
            bundle: 更新対象の CapabilityBundle
        """
        import httpx
        from httpx_sse import aconnect_sse

        async with httpx.AsyncClient(timeout=None) as client, aconnect_sse(
            client, "GET", sse_url
        ) as event_source:
            logger.info("ConfigWatcher SSE 接続中: %s", sse_url)
            async for sse in event_source.aiter_sse():
                if self._stop_event.is_set():
                    return
                await self._handle_event(sse.event or "message", sse.data, bundle)

    async def _handle_event(
        self,
        event_type: str,
        data: str,
        bundle: CapabilityBundle,
    ) -> None:
        """SSE イベントを処理して rag_engine を更新.

        Args:
            event_type: イベントタイプ
            data: JSON データ文字列
            bundle: 更新対象の CapabilityBundle
        """
        if event_type not in ("rag_config_changed", "message"):
            return

        try:
            import json

            payload: dict[str, Any] = json.loads(data)
        except Exception:
            logger.debug("SSE データのJSONパース失敗: %s", data[:200])
            return

        new_rag_config: dict[str, Any] | None = payload.get("rag_config")
        if new_rag_config is None:
            return

        logger.info("RAG設定変更を検出: %s → RAGEngine を再初期化", self._app_name)

        from agentflow.bootstrap.rag_builder import build_rag_engine

        new_engine = await build_rag_engine(new_rag_config)
        bundle.rag_engine = new_engine  # アトミック置換

        logger.info(
            "RAGEngine リロード完了: %s (rag_engine=%s)",
            self._app_name,
            "有効" if new_engine is not None else "無効",
        )

    async def stop(self) -> None:
        """ConfigWatcher を停止."""
        self._stop_event.set()
        logger.debug("ConfigWatcher 停止シグナル送信: %s", self._app_name)


__all__ = ["ConfigWatcher"]
