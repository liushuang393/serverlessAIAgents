"""ストレージアダプタ."""

from __future__ import annotations

from infrastructure.storage.memory_backend import MemoryStorageBackend


class NoOpBinaryStorage:
    """機能無効時の no-op storage."""

    async def save(self, key: str, data: bytes) -> None:
        del key, data

    async def load(self, key: str) -> bytes | None:
        del key
        return None


class InMemoryBinaryStorage:
    """既存メモリ backend を包む薄いアダプタ。"""

    def __init__(self) -> None:
        self._backend = MemoryStorageBackend()
        self._connected = False

    async def save(self, key: str, data: bytes) -> None:
        await self._ensure_connected()
        await self._backend.set(key, data)

    async def load(self, key: str) -> bytes | None:
        await self._ensure_connected()
        data = await self._backend.get(key)
        return data if isinstance(data, bytes) else None

    async def _ensure_connected(self) -> None:
        if self._connected:
            return
        await self._backend.connect()
        self._connected = True
