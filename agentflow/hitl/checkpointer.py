# -*- coding: utf-8 -*-
"""Checkpointer - 状態永続化モジュール.

中断されたワークフローの状態を保存・復元するためのインターフェース。
LangGraph の Checkpointer パターンを参考に実装。

実装:
    - MemoryCheckpointer: インメモリ（開発/テスト用）
    - RedisCheckpointer: Redis ベース（本番分散環境）
    - DatabaseCheckpointer: DB ベース（永続化必須時）
"""

from __future__ import annotations

import logging
from abc import ABC, abstractmethod
from datetime import datetime
from typing import Any

from pydantic import BaseModel, Field

logger = logging.getLogger(__name__)


class CheckpointData(BaseModel):
    """チェックポイントデータ.

    中断時点の完全な状態を保持。
    """

    checkpoint_id: str = Field(..., description="チェックポイントID")
    thread_id: str = Field(..., description="スレッドID（会話/セッション識別子）")
    flow_id: str | None = Field(None, description="フローID")
    node_id: str | None = Field(None, description="中断ノードID")
    state: dict[str, Any] = Field(default_factory=dict, description="状態データ")
    inputs: dict[str, Any] = Field(default_factory=dict, description="入力データ")
    results: dict[str, Any] = Field(default_factory=dict, description="途中結果")
    interrupt_payload: dict[str, Any] | None = Field(None, description="割り込みペイロード")
    parent_checkpoint_id: str | None = Field(None, description="親チェックポイントID")
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")
    created_at: datetime = Field(default_factory=datetime.utcnow)
    updated_at: datetime = Field(default_factory=datetime.utcnow)


class Checkpointer(ABC):
    """チェックポインター抽象基底クラス.

    すべての Checkpointer 実装はこのインターフェースに従う。
    """

    @abstractmethod
    async def save(self, data: CheckpointData) -> str:
        """チェックポイントを保存.

        Args:
            data: 保存するチェックポイントデータ

        Returns:
            保存されたチェックポイントID
        """
        ...

    @abstractmethod
    async def load(self, checkpoint_id: str) -> CheckpointData | None:
        """チェックポイントを読み込み.

        Args:
            checkpoint_id: チェックポイントID

        Returns:
            チェックポイントデータ、存在しない場合は None
        """
        ...

    @abstractmethod
    async def load_latest(self, thread_id: str) -> CheckpointData | None:
        """指定スレッドの最新チェックポイントを読み込み.

        Args:
            thread_id: スレッドID

        Returns:
            最新のチェックポイントデータ、存在しない場合は None
        """
        ...

    @abstractmethod
    async def delete(self, checkpoint_id: str) -> bool:
        """チェックポイントを削除.

        Args:
            checkpoint_id: チェックポイントID

        Returns:
            削除成功した場合 True
        """
        ...

    @abstractmethod
    async def list_by_thread(self, thread_id: str) -> list[CheckpointData]:
        """指定スレッドの全チェックポイントを取得.

        Args:
            thread_id: スレッドID

        Returns:
            チェックポイントデータのリスト（作成日時降順）
        """
        ...


class MemoryCheckpointer(Checkpointer):
    """インメモリ Checkpointer.

    開発・テスト用。プロセス終了時にデータは消失。
    """

    def __init__(self) -> None:
        """初期化."""
        self._storage: dict[str, CheckpointData] = {}
        self._thread_index: dict[str, list[str]] = {}

    async def save(self, data: CheckpointData) -> str:
        """チェックポイントを保存."""
        data.updated_at = datetime.utcnow()
        self._storage[data.checkpoint_id] = data

        # スレッドインデックスを更新
        if data.thread_id not in self._thread_index:
            self._thread_index[data.thread_id] = []
        if data.checkpoint_id not in self._thread_index[data.thread_id]:
            self._thread_index[data.thread_id].append(data.checkpoint_id)

        logger.debug(f"Checkpoint saved: {data.checkpoint_id} for thread {data.thread_id}")
        return data.checkpoint_id

    async def load(self, checkpoint_id: str) -> CheckpointData | None:
        """チェックポイントを読み込み."""
        return self._storage.get(checkpoint_id)

    async def load_latest(self, thread_id: str) -> CheckpointData | None:
        """指定スレッドの最新チェックポイントを読み込み."""
        checkpoint_ids = self._thread_index.get(thread_id, [])
        if not checkpoint_ids:
            return None
        # 最新のチェックポイントを取得
        latest_id = checkpoint_ids[-1]
        return self._storage.get(latest_id)

    async def delete(self, checkpoint_id: str) -> bool:
        """チェックポイントを削除."""
        if checkpoint_id in self._storage:
            data = self._storage.pop(checkpoint_id)
            # インデックスからも削除
            if data.thread_id in self._thread_index:
                self._thread_index[data.thread_id] = [
                    cid for cid in self._thread_index[data.thread_id] if cid != checkpoint_id
                ]
            return True
        return False

    async def list_by_thread(self, thread_id: str) -> list[CheckpointData]:
        """指定スレッドの全チェックポイントを取得."""
        checkpoint_ids = self._thread_index.get(thread_id, [])
        checkpoints = [self._storage[cid] for cid in checkpoint_ids if cid in self._storage]
        return sorted(checkpoints, key=lambda x: x.created_at, reverse=True)

