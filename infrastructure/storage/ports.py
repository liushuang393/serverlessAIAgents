"""汎用ストレージ port."""

from __future__ import annotations

from typing import Protocol


class BinaryStorage(Protocol):
    """最小限のバイナリ保存契約."""

    async def save(self, key: str, data: bytes) -> None:
        """データを保存する."""

    async def load(self, key: str) -> bytes | None:
        """データを読み出す."""
