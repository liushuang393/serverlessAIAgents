"""ComponentLibrary - 共通コンポーネントライブラリ.

Agent, Flow, Tool, Skill, Engine, Template の統一管理。
Registry パターンを使用。

設計原則:
- 型安全な登録・取得
- マルチテナント対応
- 可視性制御 (PRIVATE, TENANT, PUBLIC)

使用例:
    >>> library = ComponentLibrary()
    >>> # コンポーネント登録
    >>> entry = ComponentEntry(
    ...     id="my-agent",
    ...     name="My Agent",
    ...     type=ComponentType.AGENT,
    ...     visibility=ComponentVisibility.TENANT,
    ... )
    >>> library.register(entry)
    >>> # 取得
    >>> agent = library.get("my-agent")
    >>> # 検索
    >>> results = library.search("agent", types=[ComponentType.AGENT])
"""

from __future__ import annotations

import logging
import threading
import uuid
from dataclasses import dataclass, field
from datetime import UTC, datetime
from enum import Enum
from typing import Any, Protocol

from agentflow.core.registry import Registry
from agentflow.multi_tenant.manager import get_current_tenant


class ComponentType(str, Enum):
    """コンポーネントタイプ."""

    AGENT = "agent"
    FLOW = "flow"
    TOOL = "tool"
    SKILL = "skill"
    ENGINE = "engine"
    TEMPLATE = "template"


class ComponentVisibility(str, Enum):
    """コンポーネント可視性."""

    PRIVATE = "private"  # 所有者のみ
    TENANT = "tenant"  # テナント内共有
    PUBLIC = "public"  # 全体公開


@dataclass
class ComponentEntry:
    """コンポーネントエントリ."""

    id: str
    name: str
    type: ComponentType
    version: str = "1.0.0"
    description: str = ""
    author: str = ""
    category: str = "general"
    tags: list[str] = field(default_factory=list)
    visibility: ComponentVisibility = ComponentVisibility.PRIVATE
    tenant_id: str | None = None
    owner_id: str | None = None
    source_path: str | None = None
    source_code: str | None = None
    config: dict[str, Any] = field(default_factory=dict)
    dependencies: list[str] = field(default_factory=list)
    protocols: list[str] = field(default_factory=list)
    usage_count: int = 0
    created_at: datetime = field(default_factory=lambda: datetime.now(UTC))
    updated_at: datetime = field(default_factory=lambda: datetime.now(UTC))
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "id": self.id,
            "name": self.name,
            "type": self.type.value,
            "version": self.version,
            "description": self.description,
            "author": self.author,
            "category": self.category,
            "tags": self.tags,
            "visibility": self.visibility.value,
            "tenant_id": self.tenant_id,
            "owner_id": self.owner_id,
            "source_path": self.source_path,
            "config": self.config,
            "dependencies": self.dependencies,
            "protocols": self.protocols,
            "usage_count": self.usage_count,
            "created_at": self.created_at.isoformat(),
            "updated_at": self.updated_at.isoformat(),
            "metadata": self.metadata,
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> ComponentEntry:
        """辞書から作成."""
        return cls(
            id=data["id"],
            name=data["name"],
            type=ComponentType(data["type"]),
            version=data.get("version", "1.0.0"),
            description=data.get("description", ""),
            author=data.get("author", ""),
            category=data.get("category", "general"),
            tags=data.get("tags", []),
            visibility=ComponentVisibility(data.get("visibility", "private")),
            tenant_id=data.get("tenant_id"),
            owner_id=data.get("owner_id"),
            source_path=data.get("source_path"),
            source_code=data.get("source_code"),
            config=data.get("config", {}),
            dependencies=data.get("dependencies", []),
            protocols=data.get("protocols", []),
            usage_count=data.get("usage_count", 0),
            created_at=(
                datetime.fromisoformat(data["created_at"])
                if isinstance(data.get("created_at"), str)
                else data.get("created_at", datetime.now(UTC))
            ),
            updated_at=(
                datetime.fromisoformat(data["updated_at"])
                if isinstance(data.get("updated_at"), str)
                else data.get("updated_at", datetime.now(UTC))
            ),
            metadata=data.get("metadata", {}),
        )


class ComponentStore(Protocol):
    """コンポーネントストアプロトコル（永続化層）."""

    async def save(self, entry: ComponentEntry) -> None:
        """保存."""
        ...

    async def load(self, component_id: str) -> ComponentEntry | None:
        """読み込み."""
        ...

    async def delete(self, component_id: str) -> bool:
        """削除."""
        ...

    async def list_all(self) -> list[ComponentEntry]:
        """全件取得."""
        ...


class InMemoryComponentStore:
    """インメモリコンポーネントストア（デフォルト実装）."""

    def __init__(self) -> None:
        """初期化."""
        self._store: dict[str, ComponentEntry] = {}
        self._lock = threading.RLock()

    async def save(self, entry: ComponentEntry) -> None:
        """保存."""
        with self._lock:
            self._store[entry.id] = entry

    async def load(self, component_id: str) -> ComponentEntry | None:
        """読み込み."""
        with self._lock:
            return self._store.get(component_id)

    async def delete(self, component_id: str) -> bool:
        """削除."""
        with self._lock:
            if component_id in self._store:
                del self._store[component_id]
                return True
            return False

    async def list_all(self) -> list[ComponentEntry]:
        """全件取得."""
        with self._lock:
            return list(self._store.values())


class ComponentLibrary(Registry[ComponentEntry]):
    """コンポーネントライブラリ.

    Agent, Flow, Tool, Skill, Engine, Template の統一管理。
    マルチテナント対応、可視性制御付き。
    """

    _instance: ComponentLibrary | None = None
    _instance_lock = threading.Lock()

    def __new__(cls, store: ComponentStore | None = None) -> ComponentLibrary:
        """シングルトンパターン."""
        with cls._instance_lock:
            if cls._instance is None:
                cls._instance = super().__new__(cls)
                cls._instance._initialized = False
            return cls._instance

    def __init__(self, store: ComponentStore | None = None) -> None:
        """初期化.

        Args:
            store: コンポーネントストア（省略時はインメモリ）
        """
        if getattr(self, "_initialized", False):
            return

        super().__init__()
        self._store = store or InMemoryComponentStore()
        self._logger = logging.getLogger(__name__)
        self._initialized = True

    def register(self, entry: ComponentEntry, *, overwrite: bool = False) -> None:
        """コンポーネントを登録.

        Args:
            entry: コンポーネントエントリ
            overwrite: 上書き許可

        Raises:
            ValueError: 既に存在し overwrite=False の場合
        """
        if entry.id in self and not overwrite:
            msg = f"Component already exists: {entry.id}"
            raise ValueError(msg)

        # テナント情報を自動設定
        tenant = get_current_tenant()
        if tenant and not entry.tenant_id:
            entry.tenant_id = tenant.tenant_id

        # 更新日時を設定
        entry.updated_at = datetime.now(UTC)

        # Registry に登録
        super().register(entry.id, entry)
        self._logger.info(f"Component registered: {entry.id} ({entry.type.value})")

    async def register_async(self, entry: ComponentEntry, *, overwrite: bool = False) -> None:
        """コンポーネントを非同期登録.

        Args:
            entry: コンポーネントエントリ
            overwrite: 上書き許可
        """
        self.register(entry, overwrite=overwrite)
        await self._store.save(entry)

    def get_component(self, component_id: str) -> ComponentEntry | None:
        """コンポーネントを取得.

        可視性チェック付き。

        Args:
            component_id: コンポーネントID

        Returns:
            コンポーネントエントリ（権限なしの場合も None）
        """
        entry = self.get(component_id)
        if entry is None:
            return None

        # 可視性チェック
        if not self._check_visibility(entry):
            return None

        return entry

    def _check_visibility(self, entry: ComponentEntry) -> bool:
        """可視性チェック.

        Args:
            entry: コンポーネントエントリ

        Returns:
            アクセス可能な場合 True
        """
        if entry.visibility == ComponentVisibility.PUBLIC:
            return True

        tenant = get_current_tenant()

        if entry.visibility == ComponentVisibility.TENANT:
            # テナント内共有: 同じテナントIDならアクセス可
            return bool(tenant and entry.tenant_id == tenant.tenant_id)

        if entry.visibility == ComponentVisibility.PRIVATE:
            # プライベート: 所有者のみ（簡易実装）
            # 本来は owner_id チェックが必要
            return bool(tenant and entry.tenant_id == tenant.tenant_id)

        return False

    def search(
        self,
        query: str = "",
        *,
        types: list[ComponentType] | None = None,
        categories: list[str] | None = None,
        tags: list[str] | None = None,
        visibility: ComponentVisibility | None = None,
        limit: int = 50,
        offset: int = 0,
    ) -> list[ComponentEntry]:
        """コンポーネントを検索.

        Args:
            query: 検索クエリ（名前、説明にマッチ）
            types: タイプフィルター
            categories: カテゴリフィルター
            tags: タグフィルター
            visibility: 可視性フィルター
            limit: 最大取得数
            offset: オフセット

        Returns:
            マッチしたコンポーネントのリスト
        """
        results: list[ComponentEntry] = []
        query_lower = query.lower()

        for entry in self.list_all().values():
            # 可視性チェック
            if not self._check_visibility(entry):
                continue

            # クエリマッチ
            if query and (
                query_lower not in entry.name.lower()
                and query_lower not in entry.description.lower()
                and query_lower not in entry.id.lower()
            ):
                continue

            # タイプフィルター
            if types and entry.type not in types:
                continue

            # カテゴリフィルター
            if categories and entry.category not in categories:
                continue

            # タグフィルター
            if tags and not any(tag in entry.tags for tag in tags):
                continue

            # 可視性フィルター
            if visibility and entry.visibility != visibility:
                continue

            results.append(entry)

        # ソート（使用回数順）
        results.sort(key=lambda e: e.usage_count, reverse=True)

        # ページネーション
        return results[offset : offset + limit]

    def list_by_type(self, component_type: ComponentType) -> list[ComponentEntry]:
        """タイプ別にコンポーネントを取得.

        Args:
            component_type: コンポーネントタイプ

        Returns:
            コンポーネントのリスト
        """
        return self.search(types=[component_type])

    def increment_usage(self, component_id: str) -> None:
        """使用回数をインクリメント.

        Args:
            component_id: コンポーネントID
        """
        entry = self.get(component_id)
        if entry:
            entry.usage_count += 1
            entry.updated_at = datetime.now(UTC)

    def get_dependencies(self, component_id: str) -> list[ComponentEntry]:
        """依存コンポーネントを取得.

        Args:
            component_id: コンポーネントID

        Returns:
            依存コンポーネントのリスト
        """
        entry = self.get(component_id)
        if not entry:
            return []

        deps = []
        for dep_id in entry.dependencies:
            dep = self.get_component(dep_id)
            if dep:
                deps.append(dep)

        return deps

    def get_dependents(self, component_id: str) -> list[ComponentEntry]:
        """被依存コンポーネントを取得（このコンポーネントに依存しているもの）.

        Args:
            component_id: コンポーネントID

        Returns:
            被依存コンポーネントのリスト
        """
        dependents = []
        for entry in self.list_all().values():
            if component_id in entry.dependencies and self._check_visibility(entry):
                dependents.append(entry)

        return dependents

    def validate_dependencies(self, entry: ComponentEntry) -> list[str]:
        """依存関係を検証.

        Args:
            entry: コンポーネントエントリ

        Returns:
            見つからない依存のリスト
        """
        missing = []
        for dep_id in entry.dependencies:
            if self.get_component(dep_id) is None:
                missing.append(dep_id)

        return missing

    def generate_id(self, name: str, component_type: ComponentType) -> str:
        """コンポーネントIDを生成.

        Args:
            name: コンポーネント名
            component_type: コンポーネントタイプ

        Returns:
            生成されたID
        """
        # スネークケースに変換
        slug = name.lower().replace(" ", "-").replace("_", "-")
        # 重複チェック
        base_id = f"{slug}-{component_type.value}"
        if base_id not in self:
            return base_id

        # ユニークサフィックスを追加
        return f"{base_id}-{uuid.uuid4().hex[:8]}"


# グローバルインスタンス
_default_library: ComponentLibrary | None = None


def get_component_library() -> ComponentLibrary:
    """デフォルトのコンポーネントライブラリを取得.

    Returns:
        ComponentLibrary インスタンス
    """
    global _default_library
    if _default_library is None:
        _default_library = ComponentLibrary()
    return _default_library


__all__ = [
    "ComponentEntry",
    "ComponentLibrary",
    "ComponentStore",
    "ComponentType",
    "ComponentVisibility",
    "InMemoryComponentStore",
    "get_component_library",
]
