"""
Thread（スレッド）コンポーネント

このモジュールは、会話履歴と状態管理のための抽象インターフェースと
具体的な実装を提供します。
"""

import json
import uuid
from abc import ABC, abstractmethod
from datetime import datetime
from typing import Any, Dict, List

from ..utils.logging import get_logger
from .models import Message

logger = get_logger(__name__)


class ThreadInterface(ABC):
    """会話履歴と状態管理のための抽象インターフェース"""

    @abstractmethod
    async def add_message(self, message: Message) -> None:
        """
        メッセージを履歴に追加する

        Args:
            message: 追加するメッセージ
        """
        pass

    @abstractmethod
    async def get_history(self, limit: int = None) -> List[Message]:
        """
        履歴を取得する

        Args:
            limit: 取得するメッセージの最大数（Noneの場合は全て）

        Returns:
            List[Message]: メッセージのリスト
        """
        pass

    @abstractmethod
    async def get_state(self) -> Dict[str, Any]:
        """
        現在の状態を取得する

        Returns:
            Dict[str, Any]: 状態の辞書
        """
        pass

    @abstractmethod
    async def update_state(self, state: Dict[str, Any]) -> None:
        """
        状態を更新する

        Args:
            state: 新しい状態（部分的な更新も可能）
        """
        pass

    @abstractmethod
    async def clear(self) -> None:
        """履歴と状態をクリアする"""
        pass

    @abstractmethod
    async def get_thread_id(self) -> str:
        """
        スレッドIDを取得する

        Returns:
            str: スレッドID
        """
        pass


class SimpleThread(ThreadInterface):
    """シンプルなインメモリスレッド実装"""

    def __init__(self, thread_id: str = None, max_history: int = 100):
        """
        シンプルなスレッドを初期化する

        Args:
            thread_id: スレッドID（Noneの場合は自動生成）
            max_history: 保持する履歴の最大数
        """
        self._thread_id = thread_id or str(uuid.uuid4())
        self._max_history = max_history
        self._history: List[Message] = []
        self._state: Dict[str, Any] = {}

        logger.info(f"スレッドを初期化しました（ID: {self._thread_id[:8]}...）")

    async def add_message(self, message: Message) -> None:
        """
        メッセージを履歴に追加する

        Args:
            message: 追加するメッセージ
        """
        # メッセージを追加
        self._history.append(message)

        # 最大履歴数を超えた場合、古いメッセージを削除
        if self._max_history > 0 and len(self._history) > self._max_history:
            self._history = self._history[-self._max_history :]

        logger.debug(f"メッセージを追加しました（役割: {message.role}, 長さ: {len(message.content)}文字）")

    async def get_history(self, limit: int = None) -> List[Message]:
        """
        履歴を取得する

        Args:
            limit: 取得するメッセージの最大数（Noneの場合は全て）

        Returns:
            List[Message]: メッセージのリスト
        """
        if limit is None:
            return self._history.copy()

        return self._history[-limit:].copy()

    async def get_state(self) -> Dict[str, Any]:
        """
        現在の状態を取得する

        Returns:
            Dict[str, Any]: 状態の辞書
        """
        return self._state.copy()

    async def update_state(self, state: Dict[str, Any]) -> None:
        """
        状態を更新する

        Args:
            state: 新しい状態（部分的な更新も可能）
        """
        self._state.update(state)
        logger.debug(f"状態を更新しました（キー: {', '.join(state.keys())}）")

    async def clear(self) -> None:
        """履歴と状態をクリアする"""
        self._history.clear()
        self._state.clear()
        logger.info("履歴と状態をクリアしました")

    async def get_thread_id(self) -> str:
        """
        スレッドIDを取得する

        Returns:
            str: スレッドID
        """
        return self._thread_id

    async def to_dict(self) -> Dict[str, Any]:
        """
        スレッドを辞書に変換する

        Returns:
            Dict[str, Any]: スレッドの辞書表現
        """
        return {
            "thread_id": self._thread_id,
            "history": [message.dict() for message in self._history],
            "state": self._state,
        }

    @classmethod
    async def from_dict(cls, data: Dict[str, Any]) -> "SimpleThread":
        """
        辞書からスレッドを作成する

        Args:
            data: スレッドの辞書表現

        Returns:
            SimpleThread: 作成されたスレッド
        """
        thread = cls(thread_id=data.get("thread_id"))

        # 履歴を復元
        for message_data in data.get("history", []):
            thread._history.append(Message(**message_data))

        # 状態を復元
        thread._state = data.get("state", {})

        return thread

    async def save_to_file(self, file_path: str) -> None:
        """
        スレッドをファイルに保存する

        Args:
            file_path: 保存先のファイルパス
        """
        data = await self.to_dict()

        # datetime型をシリアライズ可能な形式に変換
        for message in data["history"]:
            if isinstance(message.get("timestamp"), datetime):
                message["timestamp"] = message["timestamp"].isoformat()

        with open(file_path, "w", encoding="utf-8") as f:
            json.dump(data, f, ensure_ascii=False, indent=2)

        logger.info(f"スレッドをファイルに保存しました: {file_path}")

    @classmethod
    async def load_from_file(cls, file_path: str) -> "SimpleThread":
        """
        ファイルからスレッドを読み込む

        Args:
            file_path: 読み込むファイルパス

        Returns:
            SimpleThread: 読み込まれたスレッド
        """
        with open(file_path, "r", encoding="utf-8") as f:
            data = json.load(f)

        # ISO形式の日時文字列をdatetime型に変換
        for message in data.get("history", []):
            if isinstance(message.get("timestamp"), str):
                message["timestamp"] = datetime.fromisoformat(message["timestamp"])

        thread = await cls.from_dict(data)
        logger.info(f"ファイルからスレッドを読み込みました: {file_path}")
        return thread


class PersistentThread(ThreadInterface):
    """永続化対応のスレッド実装"""

    def __init__(
        self,
        thread_id: str = None,
        storage_provider: Any = None,
        max_history: int = 100,
    ):
        """
        永続化対応のスレッドを初期化する

        Args:
            thread_id: スレッドID（Noneの場合は自動生成）
            storage_provider: ストレージプロバイダー
            max_history: 保持する履歴の最大数
        """
        self._thread_id = thread_id or str(uuid.uuid4())
        self._storage = storage_provider
        self._max_history = max_history
        self._history: List[Message] = []
        self._state: Dict[str, Any] = {}
        self._dirty = False  # 変更があったかどうか

        logger.info(f"永続化スレッドを初期化しました（ID: {self._thread_id[:8]}...）")

    async def add_message(self, message: Message) -> None:
        """
        メッセージを履歴に追加する

        Args:
            message: 追加するメッセージ
        """
        # メッセージを追加
        self._history.append(message)

        # 最大履歴数を超えた場合、古いメッセージを削除
        if self._max_history > 0 and len(self._history) > self._max_history:
            self._history = self._history[-self._max_history :]

        self._dirty = True

        # ストレージプロバイダーがある場合は保存
        if self._storage:
            await self._save_to_storage()

        logger.debug(f"メッセージを追加しました（役割: {message.role}, 長さ: {len(message.content)}文字）")

    async def get_history(self, limit: int = None) -> List[Message]:
        """
        履歴を取得する

        Args:
            limit: 取得するメッセージの最大数（Noneの場合は全て）

        Returns:
            List[Message]: メッセージのリスト
        """
        if limit is None:
            return self._history.copy()

        return self._history[-limit:].copy()

    async def get_state(self) -> Dict[str, Any]:
        """
        現在の状態を取得する

        Returns:
            Dict[str, Any]: 状態の辞書
        """
        return self._state.copy()

    async def update_state(self, state: Dict[str, Any]) -> None:
        """
        状態を更新する

        Args:
            state: 新しい状態（部分的な更新も可能）
        """
        self._state.update(state)
        self._dirty = True

        # ストレージプロバイダーがある場合は保存
        if self._storage:
            await self._save_to_storage()

        logger.debug(f"状態を更新しました（キー: {', '.join(state.keys())}）")

    async def clear(self) -> None:
        """履歴と状態をクリアする"""
        self._history.clear()
        self._state.clear()
        self._dirty = True

        # ストレージプロバイダーがある場合は保存
        if self._storage:
            await self._save_to_storage()

        logger.info("履歴と状態をクリアしました")

    async def get_thread_id(self) -> str:
        """
        スレッドIDを取得する

        Returns:
            str: スレッドID
        """
        return self._thread_id

    async def _save_to_storage(self) -> None:
        """ストレージにスレッドを保存する"""
        if not self._dirty or self._storage is None:
            return

        data = {
            "thread_id": self._thread_id,
            "history": [message.dict() for message in self._history],
            "state": self._state,
        }

        await self._storage.save_thread(self._thread_id, data)
        self._dirty = False

        logger.debug(f"スレッドをストレージに保存しました（ID: {self._thread_id[:8]}...）")

    @classmethod
    async def load_from_storage(
        cls, thread_id: str, storage_provider: Any
    ) -> "PersistentThread":
        """
        ストレージからスレッドを読み込む

        Args:
            thread_id: スレッドID
            storage_provider: ストレージプロバイダー

        Returns:
            PersistentThread: 読み込まれたスレッド
        """
        data = await storage_provider.load_thread(thread_id)
        if not data:
            return cls(thread_id=thread_id, storage_provider=storage_provider)

        thread = cls(thread_id=thread_id, storage_provider=storage_provider)

        # 履歴を復元
        for message_data in data.get("history", []):
            thread._history.append(Message(**message_data))

        # 状態を復元
        thread._state = data.get("state", {})

        logger.info(f"ストレージからスレッドを読み込みました（ID: {thread_id[:8]}...）")
        return thread
