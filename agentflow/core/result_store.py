# -*- coding: utf-8 -*-
"""Flow Result Store - フロー実行結果の永続化.

目的:
- フロー実行結果を保存・取得する統一インターフェース
- 複数のバックエンドをサポート（メモリ、ファイル、DB）
- flow.complete後に自動保存

使用例:
    >>> from agentflow.core.result_store import ResultStoreManager, MemoryResultStore
    >>> 
    >>> # メモリストアを使用
    >>> store = MemoryResultStore()
    >>> ResultStoreManager.set_store(store)
    >>> 
    >>> # 結果を保存
    >>> await ResultStoreManager.save("result-123", {"data": "..."})
    >>> 
    >>> # 結果を取得
    >>> result = await ResultStoreManager.get("result-123")
"""

import json
import logging
from abc import ABC, abstractmethod
from datetime import datetime
from pathlib import Path
from typing import Any

from pydantic import BaseModel, Field


logger = logging.getLogger(__name__)


class FlowResult(BaseModel):
    """フロー実行結果.
    
    保存される結果のデータモデル。
    """
    
    result_id: str = Field(..., description="結果ID")
    flow_id: str = Field(..., description="フローID")
    tenant_id: str | None = Field(default=None, description="テナントID")
    status: str = Field(default="success", description="実行ステータス")
    data: dict[str, Any] = Field(default_factory=dict, description="結果データ")
    created_at: datetime = Field(default_factory=datetime.now, description="作成日時")
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")


class ResultStore(ABC):
    """結果ストアの抽象基底クラス."""
    
    @abstractmethod
    async def save(self, result: FlowResult) -> None:
        """結果を保存."""
        pass
    
    @abstractmethod
    async def get(self, result_id: str) -> FlowResult | None:
        """結果を取得."""
        pass
    
    @abstractmethod
    async def delete(self, result_id: str) -> bool:
        """結果を削除."""
        pass
    
    @abstractmethod
    async def list_results(self, flow_id: str | None = None, limit: int = 100) -> list[FlowResult]:
        """結果一覧を取得."""
        pass


class MemoryResultStore(ResultStore):
    """メモリベースの結果ストア（開発・テスト用）."""
    
    def __init__(self, max_size: int = 1000) -> None:
        self._store: dict[str, FlowResult] = {}
        self._max_size = max_size
    
    async def save(self, result: FlowResult) -> None:
        """結果を保存."""
        # 容量制限チェック
        if len(self._store) >= self._max_size:
            oldest_key = min(self._store.keys(), key=lambda k: self._store[k].created_at)
            del self._store[oldest_key]
            logger.debug(f"MemoryResultStore: Evicted oldest result {oldest_key}")
        
        self._store[result.result_id] = result
        logger.debug(f"MemoryResultStore: Saved result {result.result_id}")
    
    async def get(self, result_id: str) -> FlowResult | None:
        """結果を取得."""
        return self._store.get(result_id)
    
    async def delete(self, result_id: str) -> bool:
        """結果を削除."""
        if result_id in self._store:
            del self._store[result_id]
            return True
        return False
    
    async def list_results(self, flow_id: str | None = None, limit: int = 100) -> list[FlowResult]:
        """結果一覧を取得."""
        results = list(self._store.values())
        if flow_id:
            results = [r for r in results if r.flow_id == flow_id]
        results.sort(key=lambda r: r.created_at, reverse=True)
        return results[:limit]


class FileResultStore(ResultStore):
    """ファイルベースの結果ストア."""
    
    def __init__(self, base_path: str | Path = "./.agentflow/results") -> None:
        self._base_path = Path(base_path)
        self._base_path.mkdir(parents=True, exist_ok=True)
    
    def _get_path(self, result_id: str) -> Path:
        """結果ファイルパスを取得."""
        return self._base_path / f"{result_id}.json"
    
    async def save(self, result: FlowResult) -> None:
        """結果を保存."""
        path = self._get_path(result.result_id)
        import aiofiles

        async with aiofiles.open(path, "w", encoding="utf-8") as f:
            await f.write(result.model_dump_json(indent=2))
        logger.debug(f"FileResultStore: Saved result to {path}")
    
    async def get(self, result_id: str) -> FlowResult | None:
        """結果を取得."""
        path = self._get_path(result_id)
        if not path.exists():
            return None
        try:
            import aiofiles

            async with aiofiles.open(path, "r", encoding="utf-8") as f:
                content = await f.read()
            data = json.loads(content)
            return FlowResult(**data)
        except Exception as e:
            logger.error(f"FileResultStore: Failed to load {path}: {e}")
            return None
    
    async def delete(self, result_id: str) -> bool:
        """結果を削除."""
        path = self._get_path(result_id)
        if path.exists():
            path.unlink()
            return True
        return False
    
    async def list_results(self, flow_id: str | None = None, limit: int = 100) -> list[FlowResult]:
        """結果一覧を取得."""
        results: list[FlowResult] = []
        for path in self._base_path.glob("*.json"):
            result = await self.get(path.stem)
            if result and (not flow_id or result.flow_id == flow_id):
                results.append(result)
        results.sort(key=lambda r: r.created_at, reverse=True)
        return results[:limit]


class ResultStoreManager:
    """結果ストアマネージャー（シングルトン）.

    アプリケーション全体で統一されたストアアクセスを提供。

    使用例:
        >>> ResultStoreManager.set_store(MemoryResultStore())
        >>> await ResultStoreManager.save("id", {"data": "..."}, "flow-1")
        >>> result = await ResultStoreManager.get("id")
    """

    _store: ResultStore | None = None

    @classmethod
    def set_store(cls, store: ResultStore) -> None:
        """ストアを設定."""
        cls._store = store
        logger.info(f"ResultStoreManager: Using {type(store).__name__}")

    @classmethod
    def get_store(cls) -> ResultStore:
        """ストアを取得（未設定時はMemoryResultStoreを自動作成）."""
        if cls._store is None:
            cls._store = MemoryResultStore()
            logger.info("ResultStoreManager: Auto-created MemoryResultStore")
        return cls._store

    @classmethod
    async def save(
        cls,
        result_id: str,
        data: dict[str, Any],
        flow_id: str,
        status: str = "success",
        metadata: dict[str, Any] | None = None,
        tenant_id: str | None = None,
    ) -> FlowResult:
        """結果を保存."""
        if tenant_id is None:
            try:
                from agentflow.runtime import get_runtime_context

                runtime = get_runtime_context()
                if runtime is not None:
                    tenant_id = runtime.tenant_id
            except Exception:
                tenant_id = None
        result = FlowResult(
            result_id=result_id,
            flow_id=flow_id,
            tenant_id=tenant_id,
            status=status,
            data=data,
            metadata=metadata or {},
        )
        await cls.get_store().save(result)
        return result

    @classmethod
    async def get(cls, result_id: str) -> FlowResult | None:
        """結果を取得."""
        return await cls.get_store().get(result_id)

    @classmethod
    async def delete(cls, result_id: str) -> bool:
        """結果を削除."""
        return await cls.get_store().delete(result_id)

    @classmethod
    async def list_results(cls, flow_id: str | None = None, limit: int = 100) -> list[FlowResult]:
        """結果一覧を取得."""
        return await cls.get_store().list_results(flow_id, limit)
