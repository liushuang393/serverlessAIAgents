"""隔離知識ベースマネージャー.

複数の知識ベースを物理的に隔離して管理するモジュール。
社内KB/対客KBなど、セキュリティ要件に応じた分離を実現。

設計原則:
- 物理隔離: インデックス・ストレージ・権限を完全分離
- アクセス制御: RBAC/ABAC による厳格な権限管理
- 監査対応: 全アクセスをログ記録

使用例:
    >>> from agentflow.knowledge.isolated_kb import IsolatedKBManager
    >>>
    >>> manager = IsolatedKBManager()
    >>> await manager.start()
    >>>
    >>> # 社内KBに追加（認証必須）
    >>> await manager.add_document(
    ...     kb_type="internal",
    ...     content="社内規則...",
    ...     user_context={"user_id": "123", "role": "admin"},
    ... )
    >>>
    >>> # 検索（権限チェック付き）
    >>> results = await manager.search(
    ...     kb_type="internal",
    ...     query="年休の付与日数",
    ...     user_context={"user_id": "123", "role": "employee"},
    ... )
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

from agentflow.knowledge.rag_pipeline import RAGConfig, RAGPipeline
from agentflow.security.policy_engine import AuthContext, AuthMode, PolicyEngine


logger = logging.getLogger(__name__)


class KBType(str, Enum):
    """知識ベースタイプ."""

    INTERNAL = "internal"  # 社内KB（要認証）
    EXTERNAL = "external"  # 対客KB（公開可）
    CONFIDENTIAL = "confidential"  # 機密KB（高権限）


class KBVisibility(str, Enum):
    """可視性レベル."""

    PUBLIC = "public"  # 公開
    INTERNAL = "internal"  # 社内限定
    CONFIDENTIAL = "confidential"  # 機密
    RESTRICTED = "restricted"  # 制限付き


@dataclass
class KBConfig:
    """知識ベース設定.

    Attributes:
        kb_type: 知識ベースタイプ
        collection_name: Vector DB コレクション名
        storage_path: ストレージパス
        visibility: 可視性レベル
        allowed_roles: アクセス許可ロール
        required_auth: 認証必須か
    """

    kb_type: KBType
    collection_name: str
    storage_path: str = ""
    visibility: KBVisibility = KBVisibility.INTERNAL
    allowed_roles: list[str] = field(default_factory=lambda: ["admin", "employee"])
    required_auth: bool = True
    top_k: int = 5
    min_similarity: float = 0.3


@dataclass
class KBDocument:
    """知識ベースドキュメント.

    Attributes:
        document_id: ドキュメントID
        content: 内容
        metadata: メタデータ
        visibility: 可視性
        owner_id: オーナーID
        created_at: 作成日時
        updated_at: 更新日時
    """

    document_id: str
    content: str
    metadata: dict[str, Any] = field(default_factory=dict)
    visibility: KBVisibility = KBVisibility.INTERNAL
    owner_id: str = ""
    owner_department: str = ""
    created_at: datetime = field(default_factory=datetime.now)
    updated_at: datetime = field(default_factory=datetime.now)
    version: str = "1.0"
    effective_date: datetime | None = None
    expiry_date: datetime | None = None


@dataclass
class SearchResult:
    """検索結果.

    Attributes:
        document_id: ドキュメントID
        content: 内容
        score: スコア
        metadata: メタデータ
        citation: 引用情報
    """

    document_id: str
    content: str
    score: float
    metadata: dict[str, Any] = field(default_factory=dict)
    citation: dict[str, Any] = field(default_factory=dict)


@dataclass
class KBAccessLog:
    """アクセスログ.

    Attributes:
        timestamp: タイムスタンプ
        user_id: ユーザーID
        kb_type: KBタイプ
        action: アクション
        query: クエリ
        results_count: 結果件数
        success: 成功フラグ
    """

    timestamp: datetime
    user_id: str
    kb_type: KBType
    action: str  # search, add, update, delete
    query: str = ""
    results_count: int = 0
    success: bool = True
    error_message: str = ""
    ip_address: str = ""


class IsolatedKBManager:
    """隔離知識ベースマネージャー.

    複数の知識ベースを物理的に隔離して管理。

    Example:
        >>> manager = IsolatedKBManager()
        >>> await manager.start()
        >>>
        >>> # 社内KB検索
        >>> results = await manager.search(
        ...     kb_type=KBType.INTERNAL,
        ...     query="年休の付与日数",
        ...     user_context={"user_id": "123", "role": "employee"},
        ... )
    """

    # デフォルトKB設定
    DEFAULT_KB_CONFIGS = {
        KBType.INTERNAL: KBConfig(
            kb_type=KBType.INTERNAL,
            collection_name="internal_kb",
            storage_path="/data/internal",
            visibility=KBVisibility.INTERNAL,
            allowed_roles=["admin", "manager", "employee"],
            required_auth=True,
        ),
        KBType.EXTERNAL: KBConfig(
            kb_type=KBType.EXTERNAL,
            collection_name="external_kb",
            storage_path="/data/external",
            visibility=KBVisibility.PUBLIC,
            allowed_roles=["*"],  # 全員
            required_auth=False,
        ),
        KBType.CONFIDENTIAL: KBConfig(
            kb_type=KBType.CONFIDENTIAL,
            collection_name="confidential_kb",
            storage_path="/data/confidential",
            visibility=KBVisibility.CONFIDENTIAL,
            allowed_roles=["admin", "manager"],
            required_auth=True,
        ),
    }

    def __init__(
        self,
        configs: dict[KBType, KBConfig] | None = None,
        policy_engine: PolicyEngine | None = None,
    ) -> None:
        """初期化.

        Args:
            configs: KB設定（省略時はデフォルト）
            policy_engine: ポリシーエンジン
        """
        self._configs = configs or self.DEFAULT_KB_CONFIGS.copy()
        self._policy_engine = policy_engine or PolicyEngine()
        self._pipelines: dict[KBType, RAGPipeline] = {}
        self._access_logs: list[KBAccessLog] = []
        self._started = False
        self._logger = logging.getLogger(__name__)

    async def start(self) -> None:
        """マネージャーを開始."""
        if self._started:
            return

        # 各KBのRAGパイプラインを初期化
        for kb_type, config in self._configs.items():
            rag_config = RAGConfig(
                collection_name=config.collection_name,
                top_k=config.top_k,
                min_similarity=config.min_similarity,
            )
            pipeline = RAGPipeline(config=rag_config)
            await pipeline.start()
            self._pipelines[kb_type] = pipeline
            self._logger.info("KB started: %s", kb_type.value)

        self._started = True
        self._logger.info("IsolatedKBManager started with %d KBs", len(self._pipelines))

    async def stop(self) -> None:
        """マネージャーを停止."""
        if not self._started:
            return

        for kb_type, pipeline in self._pipelines.items():
            await pipeline.stop()
            self._logger.info("KB stopped: %s", kb_type.value)

        self._pipelines.clear()
        self._started = False
        self._logger.info("IsolatedKBManager stopped")

    async def search(
        self,
        kb_type: KBType | str,
        query: str,
        user_context: dict[str, Any] | None = None,
        top_k: int | None = None,
        filters: dict[str, Any] | None = None,
    ) -> list[SearchResult]:
        """検索を実行.

        Args:
            kb_type: 知識ベースタイプ
            query: 検索クエリ
            user_context: ユーザーコンテキスト
            top_k: 取得件数
            filters: フィルタ

        Returns:
            検索結果リスト

        Raises:
            PermissionError: 権限がない場合
            RuntimeError: 未開始の場合
        """
        self._ensure_started()

        if isinstance(kb_type, str):
            kb_type = KBType(kb_type)

        config = self._configs.get(kb_type)
        if not config:
            msg = f"Unknown KB type: {kb_type}"
            raise ValueError(msg)

        # 権限チェック
        user_context = user_context or {}
        if config.required_auth:
            auth_result = await self._check_access(kb_type, "search", user_context)
            if not auth_result:
                self._log_access(
                    user_context.get("user_id", "anonymous"),
                    kb_type,
                    "search",
                    query,
                    success=False,
                    error="Permission denied",
                )
                msg = f"Access denied to {kb_type.value} KB"
                raise PermissionError(msg)

        # 検索実行
        pipeline = self._pipelines[kb_type]
        raw_results = await pipeline.search(
            query=query,
            top_k=top_k or config.top_k,
            filters=filters,
        )

        # 結果を変換
        results = []
        for r in raw_results:
            result = SearchResult(
                document_id=r.get("id", ""),
                content=r.get("document", ""),
                score=1.0 - r.get("distance", 0.0),
                metadata=r.get("metadata", {}),
                citation=self._build_citation(r),
            )
            results.append(result)

        # アクセスログ
        self._log_access(
            user_context.get("user_id", "anonymous"),
            kb_type,
            "search",
            query,
            results_count=len(results),
        )

        return results

    async def add_document(
        self,
        kb_type: KBType | str,
        content: str,
        metadata: dict[str, Any] | None = None,
        user_context: dict[str, Any] | None = None,
        visibility: KBVisibility | None = None,
    ) -> str:
        """ドキュメントを追加.

        Args:
            kb_type: 知識ベースタイプ
            content: 内容
            metadata: メタデータ
            user_context: ユーザーコンテキスト
            visibility: 可視性

        Returns:
            ドキュメントID

        Raises:
            PermissionError: 権限がない場合
        """
        self._ensure_started()

        if isinstance(kb_type, str):
            kb_type = KBType(kb_type)

        config = self._configs.get(kb_type)
        if not config:
            msg = f"Unknown KB type: {kb_type}"
            raise ValueError(msg)

        # 権限チェック（追加は常に認証必須）
        user_context = user_context or {}
        auth_result = await self._check_access(kb_type, "write", user_context)
        if not auth_result:
            self._log_access(
                user_context.get("user_id", "anonymous"),
                kb_type,
                "add",
                success=False,
                error="Permission denied",
            )
            msg = f"Write access denied to {kb_type.value} KB"
            raise PermissionError(msg)

        # メタデータを構築
        doc_metadata = metadata or {}
        doc_metadata.update(
            {
                "visibility": (visibility or config.visibility).value,
                "owner_id": user_context.get("user_id", ""),
                "owner_department": user_context.get("department", ""),
                "created_at": datetime.now().isoformat(),
                "created_by": user_context.get("user_id", ""),
            }
        )

        # 追加実行
        pipeline = self._pipelines[kb_type]
        doc_id = await pipeline.add_document(
            content=content,
            metadata=doc_metadata,
        )

        # アクセスログ
        self._log_access(
            user_context.get("user_id", "anonymous"),
            kb_type,
            "add",
        )

        return doc_id

    async def query(
        self,
        kb_type: KBType | str,
        question: str,
        user_context: dict[str, Any] | None = None,
        include_citations: bool = True,
    ) -> dict[str, Any]:
        """RAGクエリを実行（検索 + 回答生成）.

        Args:
            kb_type: 知識ベースタイプ
            question: 質問
            user_context: ユーザーコンテキスト
            include_citations: 引用を含めるか

        Returns:
            回答と引用情報
        """
        self._ensure_started()

        if isinstance(kb_type, str):
            kb_type = KBType(kb_type)

        config = self._configs.get(kb_type)
        if not config:
            msg = f"Unknown KB type: {kb_type}"
            raise ValueError(msg)

        # 権限チェック
        user_context = user_context or {}
        if config.required_auth:
            auth_result = await self._check_access(kb_type, "read", user_context)
            if not auth_result:
                msg = f"Access denied to {kb_type.value} KB"
                raise PermissionError(msg)

        # RAGクエリ実行
        pipeline = self._pipelines[kb_type]
        rag_result = await pipeline.query(question)

        # 結果を構築
        result: dict[str, Any] = {
            "answer": rag_result.answer,
            "query": question,
            "kb_type": kb_type.value,
        }

        if include_citations:
            result["citations"] = [self._build_citation(s) for s in rag_result.sources]

        return result

    async def _check_access(
        self,
        kb_type: KBType,
        action: str,
        user_context: dict[str, Any],
    ) -> bool:
        """アクセス権限をチェック.

        Args:
            kb_type: KBタイプ
            action: アクション（read, write, delete）
            user_context: ユーザーコンテキスト

        Returns:
            許可されている場合True
        """
        config = self._configs[kb_type]

        # 認証不要の場合
        if not config.required_auth:
            return True

        # ユーザーコンテキストがない場合
        if not user_context or not user_context.get("user_id"):
            return False

        # ロールチェック
        user_role = user_context.get("role", "guest")
        if "*" in config.allowed_roles:
            return True
        if user_role not in config.allowed_roles:
            return False

        # PolicyEngine でのチェック
        auth_context = AuthContext(
            subject={
                "user_id": user_context.get("user_id", ""),
                "role": user_role,
                "department": user_context.get("department", ""),
            },
            resource={
                "type": "kb",
                "kb_type": kb_type.value,
                "sensitivity": config.visibility.value,
            },
            action=action,
        )

        result = await self._policy_engine.authorize(
            context=auth_context,
            mode=AuthMode.ABAC,
        )

        return result.allowed

    def _build_citation(self, source: dict[str, Any]) -> dict[str, Any]:
        """引用情報を構築.

        Args:
            source: ソース情報

        Returns:
            引用情報
        """
        metadata = source.get("metadata", {})
        return {
            "document_id": source.get("id", ""),
            "title": metadata.get("title", ""),
            "source": metadata.get("source", ""),
            "version": metadata.get("version", ""),
            "update_date": metadata.get("updated_at", ""),
            "page_number": metadata.get("page_number"),
            "section": metadata.get("section", ""),
            "snippet": source.get("document", "")[:200],
            "relevance_score": 1.0 - source.get("distance", 0.0),
            "owner_department": metadata.get("owner_department", ""),
        }

    def _log_access(
        self,
        user_id: str,
        kb_type: KBType,
        action: str,
        query: str = "",
        results_count: int = 0,
        success: bool = True,
        error: str = "",
    ) -> None:
        """アクセスログを記録.

        Args:
            user_id: ユーザーID
            kb_type: KBタイプ
            action: アクション
            query: クエリ
            results_count: 結果件数
            success: 成功フラグ
            error: エラーメッセージ
        """
        log = KBAccessLog(
            timestamp=datetime.now(),
            user_id=user_id,
            kb_type=kb_type,
            action=action,
            query=query,
            results_count=results_count,
            success=success,
            error_message=error,
        )
        self._access_logs.append(log)

        # ログ出力
        if success:
            self._logger.info(
                "KB access: user=%s, kb=%s, action=%s, results=%d",
                user_id,
                kb_type.value,
                action,
                results_count,
            )
        else:
            self._logger.warning(
                "KB access denied: user=%s, kb=%s, action=%s, error=%s",
                user_id,
                kb_type.value,
                action,
                error,
            )

    def get_access_logs(
        self,
        user_id: str | None = None,
        kb_type: KBType | None = None,
        start_time: datetime | None = None,
        end_time: datetime | None = None,
    ) -> list[KBAccessLog]:
        """アクセスログを取得.

        Args:
            user_id: ユーザーID（フィルタ）
            kb_type: KBタイプ（フィルタ）
            start_time: 開始時刻
            end_time: 終了時刻

        Returns:
            アクセスログリスト
        """
        logs = self._access_logs

        if user_id:
            logs = [l for l in logs if l.user_id == user_id]
        if kb_type:
            logs = [l for l in logs if l.kb_type == kb_type]
        if start_time:
            logs = [l for l in logs if l.timestamp >= start_time]
        if end_time:
            logs = [l for l in logs if l.timestamp <= end_time]

        return logs

    def _ensure_started(self) -> None:
        """開始済みであることを確認."""
        if not self._started:
            msg = "IsolatedKBManager not started. Call start() first."
            raise RuntimeError(msg)

    async def __aenter__(self) -> IsolatedKBManager:
        """非同期コンテキストマネージャーのエントリー."""
        await self.start()
        return self

    async def __aexit__(
        self,
        exc_type: type[BaseException] | None,
        exc_val: BaseException | None,
        exc_tb: Any,
    ) -> None:
        """非同期コンテキストマネージャーの終了."""
        await self.stop()


__all__ = [
    "IsolatedKBManager",
    "KBAccessLog",
    "KBConfig",
    "KBDocument",
    "KBType",
    "KBVisibility",
    "SearchResult",
]
