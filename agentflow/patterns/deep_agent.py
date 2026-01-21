# -*- coding: utf-8 -*-
"""DeepAgentCoordinator - 統一深度Agent協調パターン.

このモジュールはDeepAgents思想を取り入れた統一協調パターンを実装します：

執行フロー:
    認知分析 → 任務分解 → Agent選択/生成 → 並行実行 → 品質評審 → 自己進化
       ↓           ↓           ↓              ↓           ↓
    動機理解     計画生成    AgentPool    進捗管理    反馈学習

特徴:
- 動的タスク分解と計画管理（TodoList思想）
- 5-6個の通用Agent + 動的生成
- コンテキスト・記憶・通信の統一管理
- 自己反省と進化（成功パターン学習、客户反馈）

参考:
- LangChain DeepAgents Framework
- Claude Code Architecture
- AgentFlow既存パターン統合

使用例:
    >>> from agentflow.patterns.deep_agent import DeepAgentCoordinator
    >>>
    >>> coordinator = DeepAgentCoordinator(llm_client=my_llm)
    >>> result = await coordinator.execute("新規事業の投資判断を行いたい")
"""

from __future__ import annotations

import asyncio
import logging
import uuid
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Callable

from pydantic import BaseModel, Field

from agentflow.core.agent_block import AgentBlock
from agentflow.patterns.coordinator import CoordinationPattern, CoordinatorBase
from agentflow.patterns.shared_context import SharedContext
from agentflow.protocols.mcp_client import MCPClient
from agentflow.protocols.mcp_config import MCPConfig
from agentflow.protocols.mcp_lazy_client import LazyMCPClient, ToolSearchResult
from agentflow.providers.tool_provider import RegisteredTool, ToolProvider
from agentflow.skills import Skill, SkillRegistry
from agentflow.skills.engine import SkillEngine


# =============================================================================
# データモデル
# =============================================================================


class TaskStatus(str, Enum):
    """タスク状態."""

    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    FAILED = "failed"
    BLOCKED = "blocked"


class AgentType(str, Enum):
    """通用Agent種別（5-6個の基本Agent）."""

    RESEARCH = "research"      # 調研・検索
    ANALYSIS = "analysis"      # 分析・推理
    PLANNING = "planning"      # 規劃・設計
    EXECUTION = "execution"    # 執行・操作
    REVIEW = "review"          # 審核・検証
    REPORT = "report"          # 報告・総結


class TodoItem(BaseModel):
    """計画項目（DeepAgentsのTodoList思想）.

    Pydanticモデルに変更し、シリアライズ/バリデーションを強化。

    Attributes:
        id: 一意識別子
        task: タスク説明
        agent_type: 実行Agent種別
        status: タスク状態
        priority: 優先度（大きいほど優先）
        dependencies: 依存タスクID
        tools: 使用ツール
        skills: 使用スキル
        metadata: 追加メタデータ
        result: 実行結果
        error: エラー情報
        retry_count: リトライ回数
    """

    model_config = {"arbitrary_types_allowed": True}

    id: str = Field(default_factory=lambda: f"todo-{uuid.uuid4().hex[:8]}")
    task: str = Field(default="", description="タスク説明")
    agent_type: str = Field(default=AgentType.EXECUTION.value, description="実行Agent種別")
    status: TaskStatus = Field(default=TaskStatus.PENDING, description="タスク状態")
    priority: int = Field(default=0, ge=0, le=10, description="優先度（大きいほど優先）")
    dependencies: list[str] = Field(default_factory=list, description="依存タスクID")
    tools: list[str] = Field(default_factory=list, description="使用ツール")
    skills: list[str] = Field(default_factory=list, description="使用スキル")
    metadata: dict[str, Any] = Field(default_factory=dict, description="追加メタデータ")
    result: dict[str, Any] | None = Field(default=None, description="実行結果")
    error: str | None = Field(default=None, description="エラー情報")
    retry_count: int = Field(default=0, ge=0, description="リトライ回数")
    created_at: datetime = Field(default_factory=datetime.now)
    completed_at: datetime | None = Field(default=None)

    def is_ready(self, completed_ids: set[str]) -> bool:
        """実行準備完了かどうか."""
        if self.status != TaskStatus.PENDING:
            return False
        return all(dep in completed_ids for dep in self.dependencies)

    def mark_completed(self) -> None:
        """完了マーク."""
        self.status = TaskStatus.COMPLETED
        self.completed_at = datetime.now()

    def mark_failed(self, error_msg: str) -> None:
        """失敗マーク."""
        self.status = TaskStatus.FAILED
        self.error = error_msg
        self.retry_count += 1


class CognitiveAnalysis(BaseModel):
    """認知分析結果."""

    intent: str = Field(default="", description="用户意図")
    is_clear: bool = Field(default=False, description="意図が明確か")
    clarification_needed: list[str] = Field(default_factory=list, description="追加質問")
    complexity: str = Field(default="medium", description="複雑度: low/medium/high")
    domains: list[str] = Field(default_factory=list, description="関連領域")
    suggested_agents: list[str] = Field(default_factory=list, description="推薦Agent")


class QualityDimension(str, Enum):
    """品質評審次元."""

    COMPLETENESS = "completeness"  # 完全性
    ACCURACY = "accuracy"  # 正確性
    CONSISTENCY = "consistency"  # 一貫性
    EFFICIENCY = "efficiency"  # 効率性
    CLARITY = "clarity"  # 明確性


class QualityReview(BaseModel):
    """品質評審結果.

    多次元の品質評価を提供:
    - 完全性: タスク要件の充足度
    - 正確性: 結果の正確さ
    - 一貫性: 全体の整合性
    - 効率性: リソース使用効率
    - 明確性: 出力の理解しやすさ
    """

    is_acceptable: bool = Field(default=False, description="合格判定")
    score: float = Field(default=0.0, ge=0, le=100, description="総合スコア")
    issues: list[str] = Field(default_factory=list, description="問題点")
    suggestions: list[str] = Field(default_factory=list, description="改善提案")
    retry_tasks: list[str] = Field(default_factory=list, description="再実行タスクID")
    # 詳細スコア
    dimension_scores: dict[str, float] = Field(
        default_factory=dict,
        description="次元別スコア（completeness, accuracy, etc.）",
    )
    verdict: str = Field(
        default="pending",
        description="判定（pass/revise/reject/pending）",
    )
    confidence: float = Field(default=0.5, ge=0, le=1, description="評価信頼度")


class EvolutionRecord(BaseModel):
    """進化記録."""

    timestamp: datetime = Field(default_factory=datetime.now)
    event_type: str = Field(default="", description="success/feedback/education")
    pattern: str = Field(default="", description="学習パターン")
    confidence: float = Field(default=0.5, description="信頼度")
    applied: bool = Field(default=False, description="適用済みか")


class MessageType(str, Enum):
    """Agent間メッセージ種別."""

    RESULT = "result"      # 実行結果
    REQUEST = "request"    # 協力依頼
    NOTIFY = "notify"      # 通知・情報共有
    ERROR = "error"        # エラー報告
    SYSTEM = "system"      # システムメッセージ（圧縮要約など）


class AgentMessage(BaseModel):
    """Agent間通信メッセージ（標準化）.

    Agent間の統一通信プロトコル。
    """

    model_config = {"arbitrary_types_allowed": True}

    id: str = Field(default_factory=lambda: f"msg-{uuid.uuid4().hex[:8]}")
    from_agent: str = Field(..., description="送信Agent")
    to_agent: str = Field(default="*", description="受信Agent（*=ブロードキャスト）")
    msg_type: MessageType = Field(default=MessageType.NOTIFY)
    content: Any = Field(default=None, description="メッセージ内容（任意型）")
    timestamp: datetime = Field(default_factory=datetime.now)
    correlation_id: str | None = Field(default=None, description="関連メッセージID")


class ParallelGroup(BaseModel):
    """並行実行グループ.

    依存関係のないタスクをグループ化。
    """

    group_id: str = Field(default_factory=lambda: f"pg-{uuid.uuid4().hex[:8]}")
    todo_ids: list[str] = Field(default_factory=list)
    status: TaskStatus = Field(default=TaskStatus.PENDING)


# =============================================================================
# 存储抽象（三層設計）
# L1: Runtime Store (Memory/Redis) - 執行中の一時データ
# L2: App Session Store (Protocol) - App固有の業務データ
# L3: Evolution Store (Protocol) - フレームワーク級の進化データ
# =============================================================================


class RuntimeStore(ABC):
    """L1: 実行時ストア（一時データ）.

    TodoList、進捗、Agent通信など実行中のデータを管理。
    デフォルトはメモリ、長時間タスクはRedisでcheckpoint可能。

    Virtual Filesystem機能:
    - Agent間でartifact（ファイル）を共有
    - 実際のI/Oなしで仮想ファイル操作
    - DeepAgents互換のファイルシステム抽象
    """

    # =========================================================================
    # コンテキスト管理
    # =========================================================================

    @abstractmethod
    async def save_context(self, key: str, data: dict[str, Any]) -> None:
        """コンテキストを保存."""
        pass

    @abstractmethod
    async def load_context(self, key: str) -> dict[str, Any] | None:
        """コンテキストを読み込み."""
        pass

    # =========================================================================
    # チェックポイント管理
    # =========================================================================

    @abstractmethod
    async def save_checkpoint(self, checkpoint_id: str, state: dict[str, Any]) -> None:
        """チェックポイントを保存（長時間タスク用）."""
        pass

    @abstractmethod
    async def load_checkpoint(self, checkpoint_id: str) -> dict[str, Any] | None:
        """チェックポイントを復元."""
        pass

    @abstractmethod
    async def list_checkpoints(self) -> list[str]:
        """チェックポイント一覧を取得."""
        pass

    # =========================================================================
    # Virtual Filesystem（Artifact管理）
    # =========================================================================

    @abstractmethod
    async def write_artifact(
        self,
        path: str,
        content: bytes | str,
        metadata: dict[str, Any] | None = None,
    ) -> None:
        """仮想ファイルを書き込み.

        Args:
            path: 仮想パス（例: "/reports/analysis.md"）
            content: ファイル内容（bytes or str）
            metadata: メタデータ（content_type, created_by等）
        """
        pass

    @abstractmethod
    async def read_artifact(self, path: str) -> bytes | None:
        """仮想ファイルを読み込み.

        Args:
            path: 仮想パス

        Returns:
            ファイル内容（存在しない場合はNone）
        """
        pass

    @abstractmethod
    async def list_artifacts(self, prefix: str = "") -> list[dict[str, Any]]:
        """仮想ファイル一覧を取得.

        Args:
            prefix: パスプレフィックスでフィルタ

        Returns:
            ファイル情報リスト（path, size, metadata等）
        """
        pass

    @abstractmethod
    async def delete_artifact(self, path: str) -> bool:
        """仮想ファイルを削除.

        Args:
            path: 仮想パス

        Returns:
            削除成功したかどうか
        """
        pass

    @abstractmethod
    async def artifact_exists(self, path: str) -> bool:
        """仮想ファイルの存在確認."""
        pass

    # =========================================================================
    # クリア
    # =========================================================================

    @abstractmethod
    async def clear(self) -> None:
        """全データをクリア."""
        pass


class EvolutionStore(ABC):
    """L3: 進化ストア（フレームワーク級永続データ）.

    成功パターン、反馈記録、進化したSkillsを永続化。
    全Appで共有され、フレームワークを継続的に強化する。
    """

    @abstractmethod
    async def save_pattern(self, pattern_key: str, pattern_data: dict[str, Any]) -> None:
        """成功パターンを保存."""
        pass

    @abstractmethod
    async def load_pattern(self, pattern_key: str) -> dict[str, Any] | None:
        """成功パターンを読み込み."""
        pass

    @abstractmethod
    async def save_feedback(self, record: EvolutionRecord) -> None:
        """反馈記録を保存."""
        pass

    @abstractmethod
    async def list_patterns(self, limit: int = 100) -> list[dict[str, Any]]:
        """パターン一覧を取得."""
        pass

    @abstractmethod
    async def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        pass


class MemoryRuntimeStore(RuntimeStore):
    """メモリベースの実行時ストア（デフォルト実装）.

    Virtual Filesystem、チェックポイント、コンテキスト管理を提供。
    開発・テスト環境向け。本番環境ではRedis実装を推奨。
    """

    def __init__(self) -> None:
        self._contexts: dict[str, dict[str, Any]] = {}
        self._checkpoints: dict[str, dict[str, Any]] = {}
        # Virtual Filesystem
        self._artifacts: dict[str, bytes] = {}
        self._artifact_metadata: dict[str, dict[str, Any]] = {}

    # =========================================================================
    # コンテキスト管理
    # =========================================================================

    async def save_context(self, key: str, data: dict[str, Any]) -> None:
        """コンテキストを保存."""
        self._contexts[key] = data

    async def load_context(self, key: str) -> dict[str, Any] | None:
        """コンテキストを読み込み."""
        return self._contexts.get(key)

    # =========================================================================
    # チェックポイント管理
    # =========================================================================

    async def save_checkpoint(self, checkpoint_id: str, state: dict[str, Any]) -> None:
        """チェックポイントを保存."""
        self._checkpoints[checkpoint_id] = {
            **state,
            "_saved_at": datetime.now().isoformat(),
        }

    async def load_checkpoint(self, checkpoint_id: str) -> dict[str, Any] | None:
        """チェックポイントを復元."""
        return self._checkpoints.get(checkpoint_id)

    async def list_checkpoints(self) -> list[str]:
        """チェックポイント一覧を取得."""
        return list(self._checkpoints.keys())

    # =========================================================================
    # Virtual Filesystem（Artifact管理）
    # =========================================================================

    async def write_artifact(
        self,
        path: str,
        content: bytes | str,
        metadata: dict[str, Any] | None = None,
    ) -> None:
        """仮想ファイルを書き込み."""
        # パス正規化
        normalized_path = self._normalize_path(path)

        # bytes変換
        if isinstance(content, str):
            content_bytes = content.encode("utf-8")
        else:
            content_bytes = content

        self._artifacts[normalized_path] = content_bytes
        self._artifact_metadata[normalized_path] = {
            "size": len(content_bytes),
            "created_at": datetime.now().isoformat(),
            "content_type": (metadata or {}).get("content_type", "application/octet-stream"),
            **(metadata or {}),
        }

    async def read_artifact(self, path: str) -> bytes | None:
        """仮想ファイルを読み込み."""
        normalized_path = self._normalize_path(path)
        return self._artifacts.get(normalized_path)

    async def list_artifacts(self, prefix: str = "") -> list[dict[str, Any]]:
        """仮想ファイル一覧を取得."""
        normalized_prefix = self._normalize_path(prefix) if prefix else ""
        results: list[dict[str, Any]] = []

        for path, metadata in self._artifact_metadata.items():
            if path.startswith(normalized_prefix):
                results.append({
                    "path": path,
                    **metadata,
                })

        return sorted(results, key=lambda x: x["path"])

    async def delete_artifact(self, path: str) -> bool:
        """仮想ファイルを削除."""
        normalized_path = self._normalize_path(path)
        if normalized_path in self._artifacts:
            del self._artifacts[normalized_path]
            del self._artifact_metadata[normalized_path]
            return True
        return False

    async def artifact_exists(self, path: str) -> bool:
        """仮想ファイルの存在確認."""
        normalized_path = self._normalize_path(path)
        return normalized_path in self._artifacts

    def _normalize_path(self, path: str) -> str:
        """パスを正規化（先頭スラッシュ統一）."""
        if not path.startswith("/"):
            path = "/" + path
        # 連続スラッシュを単一に
        while "//" in path:
            path = path.replace("//", "/")
        return path

    # =========================================================================
    # クリア
    # =========================================================================

    async def clear(self) -> None:
        """全データをクリア."""
        self._contexts.clear()
        self._checkpoints.clear()
        self._artifacts.clear()
        self._artifact_metadata.clear()


class MemoryEvolutionStore(EvolutionStore):
    """メモリベースの進化ストア（開発/テスト用）.

    Note:
        本番環境ではPostgreSQLなどの永続ストアを使用すべき。
    """

    def __init__(self) -> None:
        self._patterns: dict[str, dict[str, Any]] = {}
        self._feedbacks: list[EvolutionRecord] = []

    async def save_pattern(self, pattern_key: str, pattern_data: dict[str, Any]) -> None:
        """成功パターンを保存."""
        self._patterns[pattern_key] = {
            **pattern_data,
            "updated_at": datetime.now().isoformat(),
        }

    async def load_pattern(self, pattern_key: str) -> dict[str, Any] | None:
        """成功パターンを読み込み."""
        return self._patterns.get(pattern_key)

    async def save_feedback(self, record: EvolutionRecord) -> None:
        """反馈記録を保存."""
        self._feedbacks.append(record)

    async def list_patterns(self, limit: int = 100) -> list[dict[str, Any]]:
        """パターン一覧を取得."""
        return list(self._patterns.values())[:limit]

    async def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        return {
            "total_patterns": len(self._patterns),
            "total_feedbacks": len(self._feedbacks),
            "success_feedbacks": sum(1 for f in self._feedbacks if f.event_type == "success"),
        }


# =============================================================================
# ContextCompressor - 専門的コンテキスト圧縮システム
# =============================================================================
# 参考文献:
# - Anthropic Context Engineering (2025.09)
# - Mem0 Memory Architecture (arXiv:2504.19413)
# - MemGPT/Letta OS-style Memory (2024)
# - LLMLingua Selective Context (ACL 2024)


class CompactionStrategy(str, Enum):
    """圧縮戦略."""

    SELECTIVE = "selective"  # 情報重要度に基づく選択的保持
    SUMMARIZE = "summarize"  # LLMによる要約生成
    HIERARCHICAL = "hierarchical"  # 階層的圧縮（詳細→要約→キーポイント）
    HYBRID = "hybrid"  # 複合戦略


@dataclass
class MemoryTier:
    """メモリ層定義（MemGPT/Letta風）.

    Attributes:
        name: 層名
        max_tokens: 最大トークン数
        priority: 優先度（高いほど保持優先）
        content: 内容
    """

    name: str
    max_tokens: int
    priority: int = 0
    content: list[dict[str, Any]] = field(default_factory=list)

    def token_count(self) -> int:
        """概算トークン数を返す（1トークン≈4文字）."""
        total_chars = sum(len(str(item)) for item in self.content)
        return total_chars // 4


@dataclass
class CompactionResult:
    """圧縮結果.

    Attributes:
        original_tokens: 元のトークン数
        compressed_tokens: 圧縮後のトークン数
        compression_ratio: 圧縮率
        preserved_keys: 保持されたキー情報
        summary: 圧縮された要約（あれば）
    """

    original_tokens: int
    compressed_tokens: int
    compression_ratio: float
    preserved_keys: list[str]
    summary: str | None = None


class ContextCompressor:
    """専門的コンテキスト圧縮システム.

    業界最佳実践に基づいた多層コンテキスト圧縮を実装:

    1. 階層的メモリ管理（MemGPT/Letta風）
       - Working Memory: 直近の重要情報
       - Session Memory: セッション内の要約
       - Archival Memory: 長期保存情報

    2. 選択的保持（LLMLingua風）
       - 重要度スコアリング
       - 依存関係追跡
       - 結果・決定の優先保持

    3. 圧縮操作（Mem0風）
       - ADD: 新規追加
       - UPDATE: 既存更新
       - MERGE: 類似項目統合
       - ARCHIVE: 長期保存へ移動
       - DISCARD: 削除

    Example:
        >>> compressor = ContextCompressor(llm_client=my_llm)
        >>> result = await compressor.compact(messages, max_tokens=4000)
    """

    # 保持優先キーワード（削除しない）
    PRESERVE_PREFIXES = ("result_", "final_", "decision_", "error_", "critical_")

    # 圧縮候補キーワード（優先的に圧縮）
    COMPRESS_PREFIXES = ("temp_", "debug_", "log_", "raw_")

    def __init__(
        self,
        llm_client: Any = None,
        max_working_tokens: int = 8000,
        max_session_tokens: int = 16000,
        max_archival_tokens: int = 32000,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント（要約生成用）
            max_working_tokens: Working Memory最大トークン
            max_session_tokens: Session Memory最大トークン
            max_archival_tokens: Archival Memory最大トークン
        """
        self._llm = llm_client
        self._logger = logging.getLogger(__name__)

        # 階層的メモリ層（MemGPT風）
        self._working = MemoryTier("working", max_working_tokens, priority=3)
        self._session = MemoryTier("session", max_session_tokens, priority=2)
        self._archival = MemoryTier("archival", max_archival_tokens, priority=1)

        # メタデータ追跡
        self._key_importance: dict[str, float] = {}  # キー → 重要度スコア
        self._key_access_count: dict[str, int] = {}  # アクセス頻度
        self._key_last_access: dict[str, datetime] = {}  # 最終アクセス

    def score_importance(self, key: str, value: Any) -> float:
        """情報の重要度をスコアリング（0.0-1.0）.

        Args:
            key: キー名
            value: 値

        Returns:
            重要度スコア
        """
        score = 0.5  # ベーススコア

        # プレフィックスによる調整
        if any(key.startswith(p) for p in self.PRESERVE_PREFIXES):
            score += 0.4
        if any(key.startswith(p) for p in self.COMPRESS_PREFIXES):
            score -= 0.3

        # アクセス頻度による調整
        access_count = self._key_access_count.get(key, 0)
        if access_count > 5:
            score += 0.1
        elif access_count > 10:
            score += 0.2

        # 最終アクセス時刻による調整（古いほど低い）
        last_access = self._key_last_access.get(key)
        if last_access:
            age_minutes = (datetime.now() - last_access).total_seconds() / 60
            if age_minutes > 30:
                score -= 0.1
            if age_minutes > 60:
                score -= 0.1

        # 内容サイズによる調整（大きすぎると圧縮候補）
        content_size = len(str(value))
        if content_size > 5000:
            score -= 0.1

        return max(0.0, min(1.0, score))

    def track_access(self, key: str) -> None:
        """キーアクセスを追跡.

        Args:
            key: アクセスされたキー
        """
        self._key_access_count[key] = self._key_access_count.get(key, 0) + 1
        self._key_last_access[key] = datetime.now()

    async def compact_messages(
        self,
        messages: list[AgentMessage],
        max_tokens: int = 4000,
        strategy: CompactionStrategy = CompactionStrategy.HYBRID,
    ) -> tuple[list[AgentMessage], CompactionResult]:
        """メッセージ履歴を圧縮.

        Args:
            messages: メッセージリスト
            max_tokens: 目標最大トークン数
            strategy: 圧縮戦略

        Returns:
            (圧縮後メッセージ, 圧縮結果)
        """
        if not messages:
            return [], CompactionResult(0, 0, 1.0, [])

        original_tokens = sum(len(str(m.content)) // 4 for m in messages)

        if original_tokens <= max_tokens:
            return messages, CompactionResult(
                original_tokens, original_tokens, 1.0,
                [m.id for m in messages]
            )

        # 戦略に基づいて圧縮
        if strategy == CompactionStrategy.SELECTIVE:
            result = await self._selective_compact(messages, max_tokens)
        elif strategy == CompactionStrategy.SUMMARIZE:
            result = await self._summarize_compact(messages, max_tokens)
        elif strategy == CompactionStrategy.HIERARCHICAL:
            result = await self._hierarchical_compact(messages, max_tokens)
        else:  # HYBRID
            result = await self._hybrid_compact(messages, max_tokens)

        return result

    async def _selective_compact(  # noqa: RUF029 - async for interface consistency
        self,
        messages: list[AgentMessage],
        max_tokens: int,
    ) -> tuple[list[AgentMessage], CompactionResult]:
        """選択的圧縮（LLMLingua風）.

        重要度スコアに基づいて選択的にメッセージを保持。
        """
        # 各メッセージに重要度スコアを付与
        scored = []
        for msg in messages:
            score = self._score_message_importance(msg)
            scored.append((score, msg))

        # スコア順にソート（高い順）
        scored.sort(key=lambda x: -x[0])

        # トークン制限内で保持
        kept = []
        current_tokens = 0
        preserved_ids = []

        for score, msg in scored:
            msg_tokens = len(str(msg.content)) // 4
            if current_tokens + msg_tokens <= max_tokens:
                kept.append(msg)
                preserved_ids.append(msg.id)
                current_tokens += msg_tokens

        # 時系列順に戻す
        kept.sort(key=lambda m: m.timestamp)

        original_tokens = sum(len(str(m.content)) // 4 for m in messages)
        return kept, CompactionResult(
            original_tokens=original_tokens,
            compressed_tokens=current_tokens,
            compression_ratio=current_tokens / original_tokens if original_tokens > 0 else 1.0,
            preserved_keys=preserved_ids,
        )

    def _score_message_importance(self, msg: AgentMessage) -> float:
        """メッセージの重要度をスコアリング."""
        score = 0.5

        # メッセージタイプによる調整
        type_scores = {
            MessageType.RESULT: 0.9,
            MessageType.ERROR: 0.95,
            MessageType.REQUEST: 0.6,
            MessageType.NOTIFY: 0.4,
            MessageType.SYSTEM: 0.7,
        }
        score = type_scores.get(msg.msg_type, 0.5)

        # 最近のメッセージは重要
        age_minutes = (datetime.now() - msg.timestamp).total_seconds() / 60
        if age_minutes < 5:
            score += 0.2
        elif age_minutes < 15:
            score += 0.1

        # エラーや結果キーワードを含む場合
        content_str = str(msg.content).lower()
        if any(kw in content_str for kw in ["error", "failed", "exception"]):
            score += 0.2
        if any(kw in content_str for kw in ["result", "success", "completed"]):
            score += 0.15

        return min(1.0, score)

    async def _summarize_compact(
        self,
        messages: list[AgentMessage],
        max_tokens: int,
    ) -> tuple[list[AgentMessage], CompactionResult]:
        """要約圧縮（LLMによる要約生成）."""
        if not self._llm:
            # LLMなしの場合は選択的圧縮にフォールバック
            return await self._selective_compact(messages, max_tokens)

        # 古いメッセージを要約、最新は保持
        split_point = len(messages) * 2 // 3
        old_messages = messages[:split_point]
        recent_messages = messages[split_point:]

        # 古いメッセージを要約
        old_content = "\n".join([
            f"[{m.from_agent}→{m.to_agent}] {m.content}"
            for m in old_messages
        ])

        prompt = f"""以下のAgent間メッセージ履歴を簡潔に要約してください。
重要な決定、結果、エラーを優先的に保持してください。

メッセージ履歴:
{old_content[:8000]}

要約（JSON形式で回答）:
{{"summary": "...", "key_decisions": [...], "errors": [...]}}"""

        try:
            response = await self._llm.generate(prompt)
            summary_content = response.get("content", str(response))

            # 要約メッセージを作成
            summary_msg = AgentMessage(
                from_agent="system",
                to_agent="all",
                content={"type": "summary", "summary": summary_content},
                msg_type=MessageType.SYSTEM,
            )

            result_messages = [summary_msg] + recent_messages
            compressed_tokens = sum(len(str(m.content)) // 4 for m in result_messages)
            original_tokens = sum(len(str(m.content)) // 4 for m in messages)

            return result_messages, CompactionResult(
                original_tokens=original_tokens,
                compressed_tokens=compressed_tokens,
                compression_ratio=compressed_tokens / original_tokens,
                preserved_keys=[m.id for m in recent_messages],
                summary=summary_content,
            )
        except Exception as e:
            self._logger.warning(f"要約生成失敗: {e}")
            return await self._selective_compact(messages, max_tokens)

    async def _hierarchical_compact(  # noqa: RUF029 - async for interface consistency
        self,
        messages: list[AgentMessage],
        max_tokens: int,
    ) -> tuple[list[AgentMessage], CompactionResult]:
        """階層的圧縮（Anthropic Compaction風）.

        詳細 → 要約 → キーポイントの3層に圧縮。
        """
        # 第1層: 最新1/3はそのまま保持
        layer1_count = len(messages) // 3
        layer1 = messages[-layer1_count:] if layer1_count > 0 else messages[-5:]

        # 第2層: 中間1/3は選択的保持
        layer2_start = len(messages) // 3
        layer2_end = len(messages) * 2 // 3
        layer2_candidates = messages[layer2_start:layer2_end]
        layer2 = [m for m in layer2_candidates if self._score_message_importance(m) > 0.6]

        # 第3層: 古い1/3は要約
        layer3_messages = messages[:layer2_start]
        if layer3_messages:
            layer3_summary = AgentMessage(
                from_agent="system",
                to_agent="all",
                content={
                    "type": "archived_summary",
                    "count": len(layer3_messages),
                    "agents": list({m.from_agent for m in layer3_messages}),
                    "period": f"{layer3_messages[0].timestamp} - {layer3_messages[-1].timestamp}",
                },
                msg_type=MessageType.SYSTEM,
            )
            result = [layer3_summary] + layer2 + layer1
        else:
            result = layer2 + layer1

        original_tokens = sum(len(str(m.content)) // 4 for m in messages)
        compressed_tokens = sum(len(str(m.content)) // 4 for m in result)

        return result, CompactionResult(
            original_tokens=original_tokens,
            compressed_tokens=compressed_tokens,
            compression_ratio=compressed_tokens / original_tokens if original_tokens > 0 else 1.0,
            preserved_keys=[m.id for m in layer1 + layer2],
        )

    async def _hybrid_compact(
        self,
        messages: list[AgentMessage],
        max_tokens: int,
    ) -> tuple[list[AgentMessage], CompactionResult]:
        """ハイブリッド圧縮（推奨）.

        階層的圧縮 + 選択的保持の組み合わせ。
        """
        # まず階層的に分類
        result, _ = await self._hierarchical_compact(messages, max_tokens * 2)

        # まだ大きい場合は選択的圧縮を追加適用
        current_tokens = sum(len(str(m.content)) // 4 for m in result)
        if current_tokens > max_tokens:
            result, comp_result = await self._selective_compact(result, max_tokens)
            return result, comp_result

        original_tokens = sum(len(str(m.content)) // 4 for m in messages)
        compressed_tokens = sum(len(str(m.content)) // 4 for m in result)

        return result, CompactionResult(
            original_tokens=original_tokens,
            compressed_tokens=compressed_tokens,
            compression_ratio=compressed_tokens / original_tokens if original_tokens > 0 else 1.0,
            preserved_keys=[m.id for m in result],
        )

    def get_memory_stats(self) -> dict[str, Any]:
        """メモリ使用状況を取得."""
        return {
            "working": {
                "tokens": self._working.token_count(),
                "max": self._working.max_tokens,
                "items": len(self._working.content),
            },
            "session": {
                "tokens": self._session.token_count(),
                "max": self._session.max_tokens,
                "items": len(self._session.content),
            },
            "archival": {
                "tokens": self._archival.token_count(),
                "max": self._archival.max_tokens,
                "items": len(self._archival.content),
            },
            "tracked_keys": len(self._key_importance),
        }


# =============================================================================
# ConversationManager - 対話管理・自動要約
# =============================================================================


class ConversationManager:
    """対話管理・自動要約システム.

    LangChain DeepAgents v0.2 の Conversation Summarization 機能を実装:

    1. トークン制限監視
       - 設定閾値を超えたら自動圧縮トリガー
       - 段階的圧縮（警告→圧縮→強制圧縮）

    2. インテリジェント要約
       - 重要なターニングポイントを保持
       - 決定・結果・エラーを優先保持
       - 古い対話は要約に統合

    3. コンテキストウィンドウ最適化
       - スライディングウィンドウ方式
       - 要約 + 直近N件の完全保持

    Example:
        >>> manager = ConversationManager(llm_client=my_llm, max_tokens=4000)
        >>> manager.add_message(AgentMessage(...))
        >>> if manager.needs_summarization():
        ...     await manager.auto_summarize()
    """

    def __init__(
        self,
        llm_client: Any = None,
        max_tokens: int = 4000,
        warning_threshold: float = 0.8,
        recent_keep_count: int = 5,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント（要約生成用）
            max_tokens: 最大トークン数
            warning_threshold: 警告閾値（0.0-1.0）
            recent_keep_count: 完全保持する直近メッセージ数
        """
        self._llm = llm_client
        self._max_tokens = max_tokens
        self._warning_threshold = warning_threshold
        self._recent_keep_count = recent_keep_count
        self._logger = logging.getLogger(__name__)

        # 対話履歴
        self._messages: list[AgentMessage] = []
        self._summaries: list[str] = []  # 過去の要約
        self._compressor = ContextCompressor(llm_client=llm_client)

    def add_message(self, message: AgentMessage) -> None:
        """メッセージを追加."""
        self._messages.append(message)

    def add_messages(self, messages: list[AgentMessage]) -> None:
        """複数メッセージを追加."""
        self._messages.extend(messages)

    def current_token_count(self) -> int:
        """現在のトークン数を概算."""
        total = sum(len(str(m.content)) // 4 for m in self._messages)
        total += sum(len(s) // 4 for s in self._summaries)
        return total

    def utilization(self) -> float:
        """トークン使用率（0.0-1.0）."""
        return self.current_token_count() / self._max_tokens

    def needs_summarization(self) -> bool:
        """要約が必要かどうか."""
        return self.utilization() > self._warning_threshold

    def is_critical(self) -> bool:
        """クリティカル状態（即座に圧縮必要）."""
        return self.utilization() > 0.95

    async def auto_summarize(self) -> dict[str, Any]:
        """自動要約を実行.

        Returns:
            要約結果の統計情報
        """
        if not self._messages:
            return {"status": "no_messages", "compressed": False}

        original_count = len(self._messages)
        original_tokens = self.current_token_count()

        # 直近N件は完全保持
        preserve_messages = self._messages[-self._recent_keep_count:]
        to_summarize = self._messages[:-self._recent_keep_count] if len(self._messages) > self._recent_keep_count else []

        if not to_summarize:
            return {"status": "nothing_to_summarize", "compressed": False}

        # 要約生成
        summary = await self._generate_summary(to_summarize)
        if summary:
            self._summaries.append(summary)

        # メッセージを更新
        self._messages = preserve_messages

        return {
            "status": "success",
            "compressed": True,
            "original_messages": original_count,
            "preserved_messages": len(preserve_messages),
            "summarized_messages": len(to_summarize),
            "original_tokens": original_tokens,
            "new_tokens": self.current_token_count(),
            "compression_ratio": self.current_token_count() / original_tokens if original_tokens > 0 else 1.0,
        }

    async def _generate_summary(self, messages: list[AgentMessage]) -> str | None:
        """メッセージ群から要約を生成."""
        if not messages:
            return None

        # LLMなしの場合は簡易要約
        if not self._llm:
            return self._simple_summary(messages)

        # LLMで要約生成
        content = "\n".join([
            f"[{m.from_agent}→{m.to_agent}] {m.msg_type.value}: {str(m.content)[:200]}"
            for m in messages
        ])

        prompt = f"""以下の会話を簡潔に要約してください（重要な決定・結果・エラーを優先）:

{content}

要約（100-200文字）:"""

        try:
            response = await self._llm.chat([
                {"role": "system", "content": "あなたは会話を簡潔に要約する専門家です。"},
                {"role": "user", "content": prompt},
            ])
            return response.content if hasattr(response, "content") else str(response)
        except Exception as e:
            self._logger.warning(f"要約生成失敗: {e}")
            return self._simple_summary(messages)

    def _simple_summary(self, messages: list[AgentMessage]) -> str:
        """簡易要約（LLMなし）."""
        if not messages:
            return ""

        agents = set(m.from_agent for m in messages)
        types = set(m.msg_type.value for m in messages)

        # エラーや結果を抽出
        key_points: list[str] = []
        for m in messages:
            content_str = str(m.content).lower()
            if "error" in content_str or "failed" in content_str:
                key_points.append(f"エラー: {str(m.content)[:50]}")
            elif "result" in content_str or "success" in content_str:
                key_points.append(f"結果: {str(m.content)[:50]}")

        summary = f"[要約] {len(messages)}メッセージ, Agents: {', '.join(agents)}, Types: {', '.join(types)}"
        if key_points:
            summary += f", キーポイント: {'; '.join(key_points[:3])}"

        return summary

    def get_context(self) -> dict[str, Any]:
        """現在のコンテキストを取得（LLMに渡す用）."""
        context: dict[str, Any] = {}

        # 要約があれば先頭に
        if self._summaries:
            context["previous_summary"] = "\n".join(self._summaries)

        # 直近メッセージ
        context["recent_messages"] = [
            {
                "from": m.from_agent,
                "to": m.to_agent,
                "type": m.msg_type.value,
                "content": m.content,
            }
            for m in self._messages
        ]

        return context

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        return {
            "message_count": len(self._messages),
            "summary_count": len(self._summaries),
            "current_tokens": self.current_token_count(),
            "max_tokens": self._max_tokens,
            "utilization": self.utilization(),
            "needs_summarization": self.needs_summarization(),
            "is_critical": self.is_critical(),
        }

    def clear(self) -> None:
        """全データをクリア."""
        self._messages.clear()
        self._summaries.clear()


# =============================================================================
# AgentPool - 動的Agent管理
# =============================================================================


class AgentPool:
    """Agent池 - 通用Agent + 動的生成.

    責務:
    - 5-6個の通用Agentを管理
    - 必要に応じて動的にAgentを生成
    - Skills/Tools/MCPのバインディング

    Example:
        >>> pool = AgentPool(llm_client=my_llm)
        >>> agent = await pool.get_or_create(AgentType.RESEARCH, context)
    """

    def __init__(
        self,
        llm_client: Any = None,
        predefined_agents: dict[str, AgentBlock] | None = None,
        tool_provider: ToolProvider | None = None,
        skill_registry: SkillRegistry | None = None,
        skill_engine: SkillEngine | None = None,
        mcp_config: MCPConfig | None = None,
        enable_lazy_mcp: bool = True,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント
            predefined_agents: 事前定義Agent {"名前": Agent}
            tool_provider: ToolProvider インスタンス（Noneの場合は自動発見）
            skill_registry: SkillRegistry インスタンス（Noneの場合は自動初期化）
            skill_engine: SkillEngine（動的 Skill 解析用）
            mcp_config: MCPサーバー設定（Noneの場合はMCP無効）
            enable_lazy_mcp: MCP懒加載を有効にするか（デフォルト: True）
        """
        self._llm = llm_client
        self._agents: dict[str, AgentBlock] = predefined_agents or {}
        self._dynamic_agents: dict[str, AgentBlock] = {}
        self._logger = logging.getLogger(__name__)

        # ツールプロバイダー（@tool登録されたツールを自動発見）
        self._tools = tool_provider or ToolProvider.discover()

        # SkillEngine（動的 Skill 解析用）
        self._skill_engine = skill_engine

        # Skillsレジストリ（SkillEngine があればその内部レジストリを使用）
        self._skills = skill_registry or (skill_engine.get_registry() if skill_engine else SkillRegistry())

        # MCPクライアント（外部ツール統合用、懒加載対応）
        self._mcp_config = mcp_config
        self._enable_lazy_mcp = enable_lazy_mcp
        self._mcp_client: MCPClient | LazyMCPClient | None = None
        self._mcp_connected = False

        # Agent種別ごとのデフォルトツール/スキル/MCPツール
        self._default_bindings: dict[str, dict[str, list[str]]] = {
            AgentType.RESEARCH.value: {"skills": ["rag"], "tools": [], "mcp_tools": []},
            AgentType.ANALYSIS.value: {"skills": [], "tools": [], "mcp_tools": []},
            AgentType.PLANNING.value: {"skills": [], "tools": [], "mcp_tools": []},
            AgentType.EXECUTION.value: {"skills": [], "tools": [], "mcp_tools": []},
            AgentType.REVIEW.value: {"skills": [], "tools": [], "mcp_tools": []},
            AgentType.REPORT.value: {"skills": [], "tools": [], "mcp_tools": []},
        }

    async def connect_mcp(self) -> bool:
        """MCPサーバーに接続（懒加載対応）.

        enable_lazy_mcp が True の場合、LazyMCPClient を使用して
        ツールインデックスのみを初期ロードし、上下文 token を削減。

        Returns:
            接続成功したか
        """
        if self._mcp_config is None:
            self._logger.warning("MCP設定がありません")
            return False

        try:
            # 懒加載を使用するか判定
            use_lazy = self._enable_lazy_mcp and self._mcp_config.should_enable_lazy_loading()

            if use_lazy:
                self._mcp_client = LazyMCPClient(
                    self._mcp_config,
                    enable_lazy_loading=True,
                )
                self._logger.info("懒加載MCPクライアントを使用")
            else:
                self._mcp_client = MCPClient(self._mcp_config)
                self._logger.info("通常MCPクライアントを使用")

            await self._mcp_client.connect()
            self._mcp_connected = True
            self._logger.info("MCPサーバーに接続しました")

            # 懒加載モードの統計をログ
            if use_lazy and isinstance(self._mcp_client, LazyMCPClient):
                stats = self._mcp_client.get_stats()
                self._logger.info(
                    f"MCPツールインデックス: {stats['total_tools']} ツール登録済み"
                )

            return True
        except Exception as e:
            self._logger.error(f"MCP接続エラー: {e}")
            return False

    async def disconnect_mcp(self) -> None:
        """MCPサーバーから切断."""
        if self._mcp_client and self._mcp_connected:
            await self._mcp_client.disconnect()
            self._mcp_connected = False
            self._logger.info("MCPサーバーから切断しました")

    def get_mcp_tools(self) -> list[dict[str, Any]]:
        """利用可能なMCPツール定義を取得.

        懒加載モードの場合、ロード済みツールのみ返す。

        Returns:
            MCPツール定義リスト
        """
        if not self._mcp_client or not self._mcp_connected:
            return []
        return self._mcp_client.get_tool_definitions()

    def get_mcp_tool_index(self) -> list[dict[str, str]]:
        """MCPツールインデックス（軽量版）を取得.

        懒加載モード専用。全ツールの名前と説明のみ含む軽量リスト。
        LLM への system prompt に含めて検索の参考情報とする。

        Returns:
            ツールインデックスのリスト（軽量）
        """
        if not self._mcp_client or not self._mcp_connected:
            return []

        if isinstance(self._mcp_client, LazyMCPClient):
            return self._mcp_client.get_tool_index()

        # 通常クライアントの場合はインデックスに変換
        return [
            {
                "uri": tool["function"]["name"],
                "name": tool["function"]["name"].split("/")[-1],
                "description": tool["function"].get("description", "")[:100],
            }
            for tool in self._mcp_client.get_tool_definitions()
        ]

    def get_mcp_tool_index_prompt(self) -> str:
        """MCPツールインデックスのプロンプトを取得.

        懒加載モード専用。LLM 向けのインデックスプロンプトを生成。

        Returns:
            ツールインデックスを含むプロンプト文字列
        """
        if isinstance(self._mcp_client, LazyMCPClient):
            return self._mcp_client.get_tool_index_prompt()
        return ""

    def search_mcp_tools(self, query: str) -> ToolSearchResult:
        """MCPツールを検索.

        懒加載モード専用。Claude Code の MCPSearch 風のインターフェース。

        Args:
            query: 検索クエリ（キーワードまたは select:tool_name）

        Returns:
            ToolSearchResult: 検索結果
        """
        if not isinstance(self._mcp_client, LazyMCPClient):
            self._logger.warning("懒加載モードでない場合、検索は使用できません")
            return ToolSearchResult(entries=[], query=query)

        return self._mcp_client.search_tools(query)

    def load_mcp_tools(self, tool_uris: list[str]) -> list[dict[str, Any]]:
        """指定したMCPツールの完全定義をロード.

        懒加載モード専用。検索後に必要なツールのみをロード。

        Args:
            tool_uris: ロードするツール URI のリスト

        Returns:
            ロードされたツール定義のリスト
        """
        if not isinstance(self._mcp_client, LazyMCPClient):
            self._logger.warning("懒加載モードでない場合、個別ロードは不要です")
            return []

        return self._mcp_client.load_tools(tool_uris)

    async def call_mcp_tool(
        self,
        tool_uri: str,
        arguments: dict[str, Any],
    ) -> dict[str, Any]:
        """MCPツールを呼び出す.

        懒加載モードの場合、未ロードツールは自動的にロードされる。

        Args:
            tool_uri: ツールURI (例: "mcp://filesystem/read_file")
            arguments: ツール引数

        Returns:
            ツール実行結果
        """
        if not self._mcp_client or not self._mcp_connected:
            return {"error": "MCP not connected"}
        return await self._mcp_client.call_tool(tool_uri, arguments)

    def get_mcp_stats(self) -> dict[str, Any]:
        """MCP懒加載の統計情報を取得.

        Returns:
            統計情報（ツール数、ロード済み数、token削減量など）
        """
        if isinstance(self._mcp_client, LazyMCPClient):
            return self._mcp_client.get_stats()

        # 通常クライアントの場合
        return {
            "total_tools": len(self._mcp_client.list_tools()) if self._mcp_client else 0,
            "loaded_tools": len(self._mcp_client.list_tools()) if self._mcp_client else 0,
            "load_ratio": 1.0,
            "lazy_loading_enabled": False,
        }

    def register_tool(self, tool: RegisteredTool) -> None:
        """ツールを登録.

        Args:
            tool: 登録するツール
        """
        ToolProvider.register(tool)
        # 再発見してキャッシュ更新
        self._tools = ToolProvider.discover()

    def register_skill(self, skill: Skill) -> None:
        """Skillを登録.

        Args:
            skill: 登録するSkill
        """
        self._skills.register_skill(skill)

    def set_default_binding(
        self,
        agent_type: AgentType | str,
        tools: list[str] | None = None,
        skills: list[str] | None = None,
        mcp_tools: list[str] | None = None,
    ) -> None:
        """Agent種別のデフォルトツール/スキル/MCPツールを設定.

        Args:
            agent_type: Agent種別
            tools: デフォルトツール名
            skills: デフォルトスキル名
            mcp_tools: デフォルトMCPツールURI (例: ["mcp://fs/read_file"])
        """
        key = str(agent_type)
        if key not in self._default_bindings:
            self._default_bindings[key] = {"tools": [], "skills": [], "mcp_tools": []}
        if tools is not None:
            self._default_bindings[key]["tools"] = tools
        if skills is not None:
            self._default_bindings[key]["skills"] = skills
        if mcp_tools is not None:
            self._default_bindings[key]["mcp_tools"] = mcp_tools

    def get_tools_for_agent(self, agent_type: AgentType | str) -> list[RegisteredTool]:
        """Agent種別用のツールを取得.

        Args:
            agent_type: Agent種別

        Returns:
            利用可能なツールリスト
        """
        key = str(agent_type)
        bindings = self._default_bindings.get(key, {})
        tool_names = bindings.get("tools", [])

        tools = []
        for name in tool_names:
            tool = self._tools.get_tool(name)
            if tool:
                tools.append(tool)
        return tools

    def get_skills_for_agent(self, agent_type: AgentType | str) -> list[Skill]:
        """Agent種別用のスキルを取得.

        Args:
            agent_type: Agent種別

        Returns:
            利用可能なスキルリスト
        """
        key = str(agent_type)
        bindings = self._default_bindings.get(key, {})
        skill_names = bindings.get("skills", [])

        skills = []
        for name in skill_names:
            skill = self._skills.get(name)
            if skill:
                skills.append(skill)
        return skills

    async def resolve_skill_for_task(self, task_description: str) -> list[str]:
        """タスクに対して動的に Skill を解析.

        SkillEngine を使って、タスクにマッチする Skill を検索/生成。

        Args:
            task_description: タスクの説明

        Returns:
            マッチした Skill 名のリスト
        """
        if not self._skill_engine:
            return []

        try:
            # まず検索のみ試行
            matches = self._skill_engine.find(task_description, top_k=2)
            if matches:
                return [m.skill.name for m in matches]

            # マッチなしなら生成を試行
            result = await self._skill_engine.resolve(task_description)
            if result.skill:
                return [result.skill.name]
        except Exception as e:
            self._logger.warning(f"Skill 解析失敗: {e}")

        return []

    async def get_or_create(
        self,
        agent_type: AgentType | str,
        context: dict[str, Any] | None = None,
        tools: list[Any] | None = None,
        skills: list[str] | None = None,
    ) -> AgentBlock | None:
        """Agentを取得または動的生成.

        Args:
            agent_type: Agent種別
            context: コンテキスト情報
            tools: バインドするツール
            skills: 使用するスキル名

        Returns:
            AgentBlock または None
        """
        agent_key = str(agent_type)

        # 1. 事前定義Agentをチェック
        if agent_key in self._agents:
            self._logger.debug(f"既存Agent使用: {agent_key}")
            return self._agents[agent_key]

        # 2. 動的生成済みをチェック
        if agent_key in self._dynamic_agents:
            self._logger.debug(f"動的Agent再利用: {agent_key}")
            return self._dynamic_agents[agent_key]

        # 3. 動的生成
        agent = await self._create_dynamic_agent(agent_type, context, tools, skills)
        if agent:
            self._dynamic_agents[agent_key] = agent
            self._logger.info(f"動的Agent生成: {agent_key}")

        return agent

    async def _create_dynamic_agent(  # noqa: RUF029 - async reserved for future LLM calls
        self,
        agent_type: AgentType | str,
        context: dict[str, Any] | None,
        tools: list[Any] | None,
        skills: list[str] | None,
    ) -> AgentBlock | None:
        """動的にAgentを生成.

        Note:
            asyncは将来のLLM呼び出し（プロンプト最適化等）のために予約。

        Args:
            agent_type: Agent種別
            context: コンテキスト
            tools: ツール
            skills: スキル

        Returns:
            生成されたAgent
        """
        # 種別に応じたシステムプロンプトを生成
        prompts = {
            AgentType.RESEARCH: "あなたは調査・検索の専門家です。情報収集と整理を行います。",
            AgentType.ANALYSIS: "あなたは分析・推論の専門家です。データを分析し洞察を提供します。",
            AgentType.PLANNING: "あなたは計画・設計の専門家です。戦略と実行計画を立案します。",
            AgentType.EXECUTION: "あなたは実行・操作の専門家です。タスクを確実に遂行します。",
            AgentType.REVIEW: "あなたは審査・検証の専門家です。品質を評価し改善点を指摘します。",
            AgentType.REPORT: "あなたは報告・総括の専門家です。結果を整理し報告書を作成します。",
        }

        system_prompt = prompts.get(agent_type, "あなたは汎用的なAIアシスタントです。")

        # DynamicAgent クラスを生成（SkillRegistry を注入）
        return DynamicAgent(
            name=str(agent_type),
            system_prompt=system_prompt,
            llm_client=self._llm,
            tools=tools,
            skills=skills,
            context=context,
            skill_registry=self._skills,
        )

    def register_agent(self, name: str, agent: AgentBlock) -> None:
        """Agentを登録."""
        self._agents[name] = agent
        self._logger.info(f"Agent登録: {name}")

    def list_agents(self) -> list[str]:
        """利用可能なAgent一覧."""
        return list(self._agents.keys()) + list(self._dynamic_agents.keys())


class DynamicAgent(AgentBlock):
    """動的生成Agent.

    AgentPoolから動的に生成されるAgent。
    Skills の instructions を自動的にシステムプロンプトに注入。
    """

    def __init__(
        self,
        name: str,
        system_prompt: str,
        llm_client: Any = None,
        tools: list[Any] | None = None,
        skills: list[str] | None = None,
        context: dict[str, Any] | None = None,
        skill_registry: SkillRegistry | None = None,
        **kwargs: Any,
    ) -> None:
        """初期化.

        Args:
            name: Agent 名
            system_prompt: システムプロンプト
            llm_client: LLM クライアント
            tools: ツールリスト
            skills: スキル名リスト
            context: コンテキスト
            skill_registry: スキルレジストリ（None の場合はグローバルを使用）
        """
        super().__init__(**kwargs)
        self._name = name
        self._system_prompt = system_prompt
        self._llm = llm_client
        self._tools = tools or []
        self._skills = skills or []
        self._context = context or {}
        self._skill_registry = skill_registry or SkillRegistry()
        self._logger = logging.getLogger(__name__)

    def _build_skill_instructions(self) -> str:
        """Skills の instructions を構築.

        Returns:
            結合された Skill 指示文字列
        """
        if not self._skills:
            return ""

        skill_prompts = []
        for skill_name in self._skills:
            skill = self._skill_registry.get(skill_name)
            if skill:
                # Skill の to_prompt() を使用して指示を取得
                skill_prompts.append(skill.to_prompt())
                self._logger.debug(f"Skill 指示を追加: {skill_name}")
            else:
                self._logger.warning(f"Skill が見つかりません: {skill_name}")

        if skill_prompts:
            return "\n\n---\n\n".join(skill_prompts)
        return ""

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """Agent実行.

        Skills の instructions をシステムプロンプトに注入して LLM を呼び出す。

        Args:
            input_data: 入力データ

        Returns:
            実行結果
        """
        if not self._llm:
            return {"error": "LLM not configured", "agent": self._name}

        # Skills 指示を構築
        skill_instructions = self._build_skill_instructions()

        # システムプロンプトを構築（Skills 指示を追加）
        full_system_prompt = self._system_prompt
        if skill_instructions:
            full_system_prompt = f"{self._system_prompt}\n\n# Skills\n\n{skill_instructions}"

        # メッセージ構築
        messages = [
            {"role": "system", "content": full_system_prompt},
            {"role": "user", "content": str(input_data.get("task", input_data))},
        ]

        # LLM呼び出し
        try:
            response = await self._llm.chat(messages)
            content = response.get("content", "") if isinstance(response, dict) else (
                response.content if hasattr(response, "content") else str(response)
            )
            return {
                "agent": self._name,
                "output": content,
                "status": "success",
                "skills_used": self._skills,
            }
        except Exception as e:
            self._logger.error(f"Agent {self._name} 実行失敗: {e}")
            return {"agent": self._name, "error": str(e), "status": "failed"}


# =============================================================================
# ProgressManager - 進捗・通信管理
# =============================================================================


class ProgressManager:
    """進捗・通信統一管理.

    責務:
    - TodoList管理
    - 進捗追跡
    - Agent間通信（AgentMessage標準化）
    - コンテキスト・記憶管理
    - コンテキスト圧縮（メモリ最適化）
    - 進捗イベント通知

    Example:
        >>> pm = ProgressManager(max_context_items=100)
        >>> pm.on_progress(lambda e: print(f"Progress: {e}"))
        >>> pm.add_todo(TodoItem(...))
    """

    def __init__(
        self,
        enable_memory: bool = True,
        max_context_items: int = 200,
        max_message_history: int = 500,
        compress_threshold: float = 0.8,
        llm_client: Any = None,
        runtime_store: RuntimeStore | None = None,
    ) -> None:
        """初期化.

        Args:
            enable_memory: メモリ機能を有効化
            max_context_items: コンテキスト最大アイテム数
            max_message_history: メッセージ履歴最大数
            compress_threshold: 圧縮トリガー閾値（0.0-1.0）
            llm_client: LLMクライアント（要約圧縮用）
            runtime_store: 実行時ストア（L1層）- デフォルトはメモリストア
        """
        self._todos: dict[str, TodoItem] = {}
        self._context = SharedContext(enable_memory=enable_memory)
        self._messages: list[AgentMessage] = []
        self._logger = logging.getLogger(__name__)

        # ストレージ層（L1 Runtime Store）
        self._runtime_store = runtime_store or MemoryRuntimeStore()

        # コンテキスト管理設定
        self._max_context = max_context_items
        self._max_messages = max_message_history
        self._compress_threshold = compress_threshold
        self._context_keys: list[str] = []  # 挿入順序追跡

        # 専門的コンテキスト圧縮システム
        self._compressor = ContextCompressor(
            llm_client=llm_client,
            max_working_tokens=max_message_history * 20,  # 概算
        )

        # 進捗イベントリスナー
        self._progress_listeners: list[Callable[[dict[str, Any]], None]] = []

    def add_todo(self, todo: TodoItem) -> str:
        """Todo追加."""
        self._todos[todo.id] = todo
        self._logger.debug(f"Todo追加: {todo.id} - {todo.task}")
        return todo.id

    def update_todo(self, todo_id: str, **updates: Any) -> bool:
        """Todo更新."""
        if todo_id not in self._todos:
            return False
        todo = self._todos[todo_id]
        for key, value in updates.items():
            if hasattr(todo, key):
                setattr(todo, key, value)
        return True

    def get_next_todo(self) -> TodoItem | None:
        """次に実行すべきTodoを取得（単一）."""
        ready = self.get_ready_todos()
        return ready[0] if ready else None

    def get_ready_todos(self) -> list[TodoItem]:
        """実行準備完了の全Todoを取得（並行実行用）.

        依存関係が解決済みで、まだPENDING状態のTodoを全て返す。
        優先度順にソート。
        """
        completed_ids = {
            t.id for t in self._todos.values()
            if t.status == TaskStatus.COMPLETED
        }
        ready = [
            t for t in self._todos.values()
            if t.status == TaskStatus.PENDING and t.is_ready(completed_ids)
        ]
        # 優先度でソート（高い順）
        return sorted(ready, key=lambda x: -x.priority)

    def validate_dependencies(self) -> list[str]:
        """依存関係の検証（循環依存検出）.

        トポロジカルソートを使用して、循環依存がないことを確認します。

        Returns:
            エラーメッセージのリスト（空の場合は問題なし）

        Example:
            >>> pm = ProgressManager()
            >>> pm.add_todo(TodoItem(id="a", task="A", dependencies=["b"]))
            >>> pm.add_todo(TodoItem(id="b", task="B", dependencies=["a"]))
            >>> errors = pm.validate_dependencies()
            >>> assert len(errors) > 0  # 循環依存検出
        """
        from graphlib import CycleError, TopologicalSorter

        # 依存関係グラフを構築
        graph: dict[str, set[str]] = {}
        for todo_id, todo in self._todos.items():
            graph[todo_id] = set(todo.dependencies)

        try:
            # トポロジカルソートを試行
            sorter = TopologicalSorter(graph)
            list(sorter.static_order())
            return []  # 成功：循環依存なし
        except CycleError as e:
            # 循環依存検出
            cycle_info = e.args[1] if len(e.args) > 1 else "unknown"
            return [f"循環依存を検出: {cycle_info}"]

    def get_execution_order(self) -> list[str]:
        """タスク実行順序を取得（依存関係考慮）.

        トポロジカルソートに基づいて、依存関係を満たす実行順序を返します。
        循環依存がある場合は空リストを返します。

        Returns:
            タスクIDの実行順序リスト
        """
        from graphlib import CycleError, TopologicalSorter

        graph: dict[str, set[str]] = {}
        for todo_id, todo in self._todos.items():
            graph[todo_id] = set(todo.dependencies)

        try:
            sorter = TopologicalSorter(graph)
            return list(sorter.static_order())
        except CycleError:
            self._logger.error("循環依存のため実行順序を決定できません")
            return []

    def get_parallel_groups(self) -> list[ParallelGroup]:
        """並行実行可能なタスクグループを取得.

        依存関係を考慮して、同時に実行可能なタスクをグループ化。
        循環依存がある場合は空リストを返します。
        """
        # まず循環依存をチェック
        errors = self.validate_dependencies()
        if errors:
            self._logger.error(f"依存関係エラー: {errors}")
            return []

        groups: list[ParallelGroup] = []
        remaining = set(self._todos.keys())
        completed: set[str] = set()

        while remaining:
            # 現在実行可能なTodo
            ready_ids = [
                tid for tid in remaining
                if self._todos[tid].is_ready(completed)
            ]
            if not ready_ids:
                break  # 残りは全て依存関係未解決

            group = ParallelGroup(todo_ids=ready_ids)
            groups.append(group)

            # このグループを完了扱いにして次へ
            completed.update(ready_ids)
            remaining -= set(ready_ids)

        return groups

    def get_progress(self) -> dict[str, Any]:
        """進捗状況を取得."""
        total = len(self._todos)
        completed = sum(1 for t in self._todos.values() if t.status == TaskStatus.COMPLETED)
        failed = sum(1 for t in self._todos.values() if t.status == TaskStatus.FAILED)
        return {
            "total": total,
            "completed": completed,
            "failed": failed,
            "progress": completed / total if total > 0 else 0.0,
            "todos": [
                {"id": t.id, "task": t.task, "status": t.status.value}
                for t in self._todos.values()
            ],
        }

    def send_message(
        self,
        from_agent: str,
        to_agent: str,
        content: Any,
        msg_type: MessageType | str = MessageType.RESULT,
    ) -> AgentMessage:
        """Agent間メッセージ送信（同期版、後方互換性のため維持）.

        Note:
            非同期環境では send_message_async() の使用を推奨します。

        Args:
            from_agent: 送信元Agent名
            to_agent: 送信先Agent名
            content: メッセージ内容
            msg_type: メッセージ種別

        Returns:
            作成されたAgentMessage
        """
        msg = AgentMessage(
            from_agent=from_agent,
            to_agent=to_agent,
            content=content,
            msg_type=MessageType(msg_type) if isinstance(msg_type, str) else msg_type,
        )
        self._messages.append(msg)
        self._context.set(f"msg_{from_agent}_to_{to_agent}", content)

        # メッセージ履歴制限チェック
        if len(self._messages) > self._max_messages:
            self._compress_messages()

        return msg

    async def send_message_async(
        self,
        from_agent: str,
        to_agent: str,
        content: Any,
        msg_type: MessageType | str = MessageType.RESULT,
        persist: bool = True,
    ) -> AgentMessage:
        """Agent間メッセージ送信（非同期版、永続化対応）.

        RuntimeStoreにメッセージを永続化し、障害復旧を可能にします。

        Args:
            from_agent: 送信元Agent名
            to_agent: 送信先Agent名
            content: メッセージ内容
            msg_type: メッセージ種別
            persist: RuntimeStoreに永続化するか

        Returns:
            作成されたAgentMessage
        """
        # 同期版でメモリに追加
        msg = self.send_message(from_agent, to_agent, content, msg_type)

        # RuntimeStoreに永続化
        if persist and self._runtime_store:
            try:
                await self._runtime_store.save_context(
                    f"msg:{msg.id}",
                    msg.model_dump(),
                )
            except Exception as e:
                self._logger.warning(f"メッセージ永続化失敗: {msg.id} - {e}")

        return msg

    async def load_messages_from_store(self) -> int:
        """RuntimeStoreからメッセージを復元.

        Returns:
            復元されたメッセージ数
        """
        if not self._runtime_store:
            return 0

        # 実装: RuntimeStoreからメッセージプレフィックスでロード
        # Note: 完全な実装にはRuntimeStoreにlist_keys()メソッドが必要
        return 0  # 現時点ではプレースホルダー

    def get_messages(
        self,
        agent: str | None = None,
        msg_type: MessageType | None = None,
    ) -> list[AgentMessage]:
        """メッセージ取得.

        Args:
            agent: フィルタするAgent名（送信元または送信先）
            msg_type: フィルタするメッセージ種別

        Returns:
            条件に合うメッセージリスト
        """
        result = self._messages.copy()
        if agent is not None:
            result = [m for m in result if m.to_agent == agent or m.from_agent == agent]
        if msg_type is not None:
            result = [m for m in result if m.msg_type == msg_type]
        return result

    def on_progress(self, listener: Callable[[dict[str, Any]], None]) -> None:
        """進捗イベントリスナーを登録.

        Args:
            listener: イベントコールバック関数
        """
        self._progress_listeners.append(listener)

    def emit_progress(self, event_type: str, data: dict[str, Any] | None = None) -> None:
        """進捗イベントを発火.

        Args:
            event_type: イベント種別（例: "todo_added", "todo_completed"）
            data: イベントデータ
        """
        event = {
            "type": event_type,
            "data": data or {},
            "progress": self.get_progress(),
            "timestamp": datetime.now().isoformat(),
        }
        for listener in self._progress_listeners:
            try:
                listener(event)
            except Exception as e:
                self._logger.warning(f"進捗リスナーエラー: {e}")

    def set_context(self, key: str, value: Any) -> None:
        """コンテキストに値を設定（圧縮対応）.

        Args:
            key: キー
            value: 値
        """
        self._context.set(key, value)
        if key not in self._context_keys:
            self._context_keys.append(key)

        # コンテキスト制限チェック
        if len(self._context_keys) > self._max_context * self._compress_threshold:
            self._compress_context()

    def get_context(self, key: str, default: Any = None) -> Any:
        """コンテキストから値を取得.

        Args:
            key: キー
            default: デフォルト値

        Returns:
            取得した値
        """
        return self._context.get(key, default)

    def _compress_context(self) -> None:
        """コンテキストを圧縮（古いエントリを削除）."""
        if len(self._context_keys) <= self._max_context:
            return

        # 削除する数を計算
        to_remove = len(self._context_keys) - int(self._max_context * 0.7)
        remove_keys = self._context_keys[:to_remove]

        for key in remove_keys:
            # 重要なキーは保持（msg_*, result_*など）
            if not key.startswith(("msg_", "result_", "final_")):
                self._context_keys.remove(key)
                # SharedContextのdeleteは未実装の可能性があるためtry
                try:
                    if hasattr(self._context, "delete"):
                        self._context.delete(key)
                except Exception:
                    pass

        self._logger.debug(f"コンテキスト圧縮: {to_remove}エントリ削除")

    def _compress_messages(self) -> None:
        """メッセージ履歴を圧縮（ContextCompressor使用）.

        専門的コンテキスト圧縮システムを使用して、
        重要度に基づいた選択的圧縮を実行。
        """
        if len(self._messages) <= self._max_messages:
            return

        # 非同期圧縮を同期的に実行（イベントループ内の場合は直接実行）
        import asyncio

        async def _do_compress() -> None:
            target_tokens = self._max_messages * 10  # 概算目標トークン数
            compressed, result = await self._compressor.compact_messages(
                self._messages,
                max_tokens=target_tokens,
                strategy=CompactionStrategy.HYBRID,
            )
            self._messages = compressed
            self._logger.info(
                f"メッセージ圧縮完了: {result.original_tokens}→{result.compressed_tokens} "
                f"(圧縮率: {result.compression_ratio:.2%})"
            )

        try:
            loop = asyncio.get_running_loop()
            # 既存のイベントループ内の場合はタスクとして実行
            loop.create_task(_do_compress())
        except RuntimeError:
            # イベントループがない場合は新規作成して実行
            asyncio.run(_do_compress())

    def get_compressed_context(self, max_tokens: int = 4000) -> str:
        """圧縮されたコンテキスト文字列を取得（LLMプロンプト用）.

        Args:
            max_tokens: 最大トークン数（概算）

        Returns:
            圧縮されたコンテキスト文字列
        """
        lines = []
        total_len = 0
        char_limit = max_tokens * 4  # 概算: 1トークン ≈ 4文字

        # 進捗情報
        progress = self.get_progress()
        lines.append(f"Progress: {progress['completed']}/{progress['total']} tasks")
        total_len += len(lines[-1])

        # メモリ統計情報
        mem_stats = self._compressor.get_memory_stats()
        lines.append(f"Memory: working={mem_stats['working']['items']} items")
        total_len += len(lines[-1])

        # 最新メッセージ
        recent = self._messages[-10:] if self._messages else []
        for msg in recent:
            line = f"[{msg.from_agent}→{msg.to_agent}] {str(msg.content)[:100]}"
            if total_len + len(line) > char_limit:
                break
            lines.append(line)
            total_len += len(line)

        return "\n".join(lines)

    def get_compressor_stats(self) -> dict[str, Any]:
        """コンテキスト圧縮システムの統計情報を取得.

        Returns:
            メモリ使用状況と圧縮統計
        """
        return self._compressor.get_memory_stats()

    @property
    def context(self) -> SharedContext:
        """SharedContextを取得."""
        return self._context


# =============================================================================
# Evolver - 自己進化システム
# =============================================================================


class Evolver:
    """自己進化システム.

    責務:
    - 成功パターンからの学習
    - 客户反馈の処理
    - Skills/Promptの改善
    - 永続化ストアとの統合（オプション）
    - **高信頼パターンの Skill 固化**

    参考文献:
    - DeepAgents Self-Evolution (2025)
    - Mem0 Learning Architecture
    """

    # Skill 固化の閾値
    SKILL_CONSOLIDATION_THRESHOLD = 0.85
    MIN_SUCCESS_COUNT_FOR_SKILL = 3

    def __init__(
        self,
        llm_client: Any = None,
        evolution_store: EvolutionStore | None = None,
        skill_engine: SkillEngine | None = None,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント
            evolution_store: 永続化ストア（L3層、オプション）
            skill_engine: SkillEngine（Skill 固化用、オプション）
        """
        self._llm = llm_client
        self._store = evolution_store
        self._skill_engine = skill_engine
        self._records: list[EvolutionRecord] = []
        self._learned_patterns: dict[str, str] = {}
        self._pattern_scores: dict[str, float] = {}  # パターン信頼度
        self._pattern_success_count: dict[str, int] = {}  # パターン成功回数
        self._consolidated_skills: set[str] = set()  # 既に Skill 化したパターン
        self._logger = logging.getLogger(__name__)

    async def learn_from_success(
        self,
        task: str,
        result: dict[str, Any],
        context: dict[str, Any],
    ) -> EvolutionRecord | None:
        """成功パターンから学習.

        高信頼パターンは自動的に Skill として固化される。

        Args:
            task: 元のタスク
            result: 成功結果
            context: 実行コンテキスト

        Returns:
            進化記録
        """
        pattern_key = self._extract_pattern_key(task)

        # 成功回数をインクリメント
        self._pattern_success_count[pattern_key] = (
            self._pattern_success_count.get(pattern_key, 0) + 1
        )

        # 既存パターンの信頼度を上げる
        current_score = self._pattern_scores.get(pattern_key, 0.5)
        new_score = min(1.0, current_score + 0.1)
        self._pattern_scores[pattern_key] = new_score

        record = EvolutionRecord(
            event_type="success",
            pattern=f"task:{pattern_key}",
            confidence=new_score,
        )
        self._records.append(record)

        # パターンを記録
        approach = result.get("approach", "")
        if approach:
            self._learned_patterns[pattern_key] = str(approach)

        # 永続化ストアがあれば保存
        if self._store:
            await self._store.save_pattern(pattern_key, {
                "task": task[:200],
                "approach": str(approach)[:500],
                "confidence": new_score,
            })
            await self._store.save_feedback(record)

        self._logger.info(f"成功パターン学習: {pattern_key} (信頼度: {new_score:.2f})")

        # Skill 固化を評価
        await self._try_consolidate_skill(task, pattern_key, result, context)

        return record

    async def _try_consolidate_skill(
        self,
        task: str,
        pattern_key: str,
        result: dict[str, Any],
        context: dict[str, Any],
    ) -> bool:
        """高信頼パターンを Skill として固化を試行.

        Args:
            task: タスク
            pattern_key: パターンキー
            result: 実行結果
            context: コンテキスト

        Returns:
            固化成功したか
        """
        # SkillEngine がない場合はスキップ
        if not self._skill_engine:
            return False

        # 既に固化済みの場合はスキップ
        if pattern_key in self._consolidated_skills:
            return False

        # 条件を満たすか確認
        confidence = self._pattern_scores.get(pattern_key, 0.0)
        success_count = self._pattern_success_count.get(pattern_key, 0)

        if (
            confidence < self.SKILL_CONSOLIDATION_THRESHOLD
            or success_count < self.MIN_SUCCESS_COUNT_FOR_SKILL
        ):
            return False

        # Skill 固化を試行
        try:
            approach = self._learned_patterns.get(pattern_key, "")
            agent_type = context.get("agent_type", "unknown")
            skills_used = context.get("skills_used", [])

            self._logger.info(
                f"Skill 固化を開始: {pattern_key} "
                f"(信頼度: {confidence:.2f}, 成功: {success_count}回)"
            )

            # SkillEngine.resolve で新規 Skill を生成
            skill_result = await self._skill_engine.resolve(task)
            if skill_result.generated and skill_result.saved:
                self._consolidated_skills.add(pattern_key)
                self._logger.info(
                    f"Skill 固化成功: {skill_result.skill.name} "
                    f"(パターン: {pattern_key})"
                )
                return True

            return False

        except Exception as e:
            self._logger.warning(f"Skill 固化失敗: {e}")
            return False

    async def process_feedback(
        self,
        feedback_type: str,
        content: str,
        context: dict[str, Any] | None = None,
    ) -> EvolutionRecord:
        """客户反馈を処理.

        Args:
            feedback_type: 反馈類型
                - suggestion: 改善提案
                - correction: 誤り指摘
                - education: 教育的フィードバック
            content: 反馈内容
            context: コンテキスト

        Returns:
            進化記録
        """
        # フィードバックタイプに応じた信頼度
        confidence_map = {
            "education": 0.9,
            "correction": 0.8,
            "suggestion": 0.6,
        }
        confidence = confidence_map.get(feedback_type, 0.5)

        record = EvolutionRecord(
            event_type=f"feedback:{feedback_type}",
            pattern=content[:100],
            confidence=confidence,
        )
        self._records.append(record)

        # 永続化ストアがあれば保存
        if self._store:
            await self._store.save_feedback(record)

        self._logger.info(f"反馈処理: {feedback_type} (信頼度: {confidence:.2f})")
        return record

    def get_learned_hint(self, task: str) -> str | None:
        """学習済みヒントを取得."""
        pattern_key = self._extract_pattern_key(task)
        return self._learned_patterns.get(pattern_key)

    def get_pattern_confidence(self, task: str) -> float:
        """パターンの信頼度を取得."""
        pattern_key = self._extract_pattern_key(task)
        return self._pattern_scores.get(pattern_key, 0.0)

    def _extract_pattern_key(self, task: str) -> str:
        """タスクからパターンキーを抽出."""
        # 簡易実装: 正規化した最初の30文字
        normalized = task[:30].strip().lower()
        # 英数字とアンダースコアのみ保持
        return "".join(c if c.isalnum() else "_" for c in normalized)

    def get_evolution_stats(self) -> dict[str, Any]:
        """進化統計を取得."""
        return {
            "total_records": len(self._records),
            "learned_patterns": len(self._learned_patterns),
            "success_count": sum(1 for r in self._records if r.event_type == "success"),
            "feedback_count": sum(1 for r in self._records if "feedback" in r.event_type),
            "avg_confidence": (
                sum(self._pattern_scores.values()) / len(self._pattern_scores)
                if self._pattern_scores else 0.0
            ),
            "high_confidence_patterns": sum(
                1 for score in self._pattern_scores.values() if score >= 0.8
            ),
        }

    def get_top_patterns(self, limit: int = 10) -> list[dict[str, Any]]:
        """高信頼度パターンを取得.

        Args:
            limit: 取得数

        Returns:
            パターンリスト
        """
        sorted_patterns = sorted(
            self._pattern_scores.items(),
            key=lambda x: -x[1],
        )[:limit]

        return [
            {
                "pattern": key,
                "confidence": score,
                "hint": self._learned_patterns.get(key, ""),
            }
            for key, score in sorted_patterns
        ]


# =============================================================================
# DeepAgentCoordinator - 統一深度Agent協調器
# =============================================================================


class DeepAgentCoordinator(CoordinatorBase):
    """統一深度Agent協調器.

    DeepAgentsの思想を取り入れた統一協調パターン:
    1. 認知分析 - 動機理解・意図判定
    2. 任務分解 - 計画生成・TodoList
    3. Agent選択/生成 - AgentPool管理
    4. 並行実行 - 進捗・通信管理
    5. 品質評審 - 不足は戻る、合格は続行
    6. 自己進化 - 成功学習・反馈処理

    Example:
        >>> coordinator = DeepAgentCoordinator(llm_client=my_llm)
        >>> result = await coordinator.execute("新規事業の投資判断を行いたい")
    """

    # 拡張パターン種別
    PATTERN_NAME = "deep_agent"

    def __init__(
        self,
        llm_client: Any = None,
        predefined_agents: dict[str, AgentBlock] | None = None,
        max_iterations: int = 10,
        max_retries: int = 2,
        quality_threshold: float = 70.0,
        enable_evolution: bool = True,
        enable_memory: bool = True,
        enable_skill_auto_learn: bool = True,
        on_progress: Callable[[dict[str, Any]], None] | None = None,
        runtime_store: RuntimeStore | None = None,
        evolution_store: EvolutionStore | None = None,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント
            predefined_agents: 事前定義Agent
            max_iterations: 最大実行回数
            max_retries: 最大リトライ回数
            quality_threshold: 品質閾値
            enable_evolution: 自己進化を有効化
            enable_memory: 記憶システムを有効化
            enable_skill_auto_learn: Skill 自動学習を有効化
            on_progress: 進捗コールバック
            runtime_store: 実行時ストア（L1層）- デフォルトはメモリストア
            evolution_store: 進化ストア（L3層）- デフォルトはメモリストア
        """
        super().__init__()
        self._llm = llm_client
        self._max_iterations = max_iterations
        self._max_retries = max_retries
        self._quality_threshold = quality_threshold
        self._enable_evolution = enable_evolution
        self._enable_skill_auto_learn = enable_skill_auto_learn
        self._on_progress = on_progress

        # ストレージ層初期化（三層設計: L1 Runtime, L2 App Session, L3 Evolution）
        self._runtime_store = runtime_store or MemoryRuntimeStore()
        self._evolution_store = evolution_store or MemoryEvolutionStore()

        # SkillEngine 初期化（越用越厉害）
        self._skill_engine = SkillEngine(auto_learn=enable_skill_auto_learn)

        # コンポーネント初期化（ストレージ注入、SkillEngine 共有）
        self._agent_pool = AgentPool(
            llm_client=llm_client,
            predefined_agents=predefined_agents,
            skill_engine=self._skill_engine,
        )
        self._progress = ProgressManager(
            enable_memory=enable_memory,
            runtime_store=self._runtime_store,
        )
        self._evolver = (
            Evolver(
                llm_client=llm_client,
                evolution_store=self._evolution_store,
                skill_engine=self._skill_engine if enable_skill_auto_learn else None,
            )
            if enable_evolution
            else None
        )

        self._logger = logging.getLogger(__name__)

        # Checkpoint 管理
        self._current_checkpoint_id: str | None = None
        self._checkpoint_enabled: bool = True

    # =========================================================================
    # Checkpoint/Recovery メソッド
    # =========================================================================

    async def save_checkpoint(self, phase: str, extra_data: dict[str, Any] | None = None) -> str:
        """現在の状態をチェックポイントとして保存.

        長時間実行タスクの途中状態を保存し、障害復旧を可能にします。

        Args:
            phase: 現在のフェーズ（cognitive/decompose/execute/review）
            extra_data: 追加データ

        Returns:
            チェックポイントID
        """
        checkpoint_id = f"cp-{uuid.uuid4().hex[:12]}"

        state = {
            "checkpoint_id": checkpoint_id,
            "phase": phase,
            "timestamp": datetime.now().isoformat(),
            "progress": self._progress.get_progress(),
            "todos": [
                t.model_dump() for t in self._progress._todos.values()
            ],
            "context": self._progress.context.get_all(),
            "extra": extra_data or {},
        }

        await self._runtime_store.save_checkpoint(checkpoint_id, state)
        self._current_checkpoint_id = checkpoint_id
        self._logger.info(f"チェックポイント保存: {checkpoint_id} (phase={phase})")

        return checkpoint_id

    async def load_checkpoint(self, checkpoint_id: str) -> dict[str, Any] | None:
        """チェックポイントを読み込み.

        Args:
            checkpoint_id: チェックポイントID

        Returns:
            チェックポイントデータ（存在しない場合はNone）
        """
        return await self._runtime_store.load_checkpoint(checkpoint_id)

    async def resume_from_checkpoint(
        self,
        checkpoint_id: str,
        task: str,
    ) -> dict[str, Any]:
        """チェックポイントから実行を再開.

        中断された実行を途中から再開します。

        Args:
            checkpoint_id: 再開するチェックポイントID
            task: 元のタスク

        Returns:
            実行結果
        """
        checkpoint = await self.load_checkpoint(checkpoint_id)
        if not checkpoint:
            raise ValueError(f"チェックポイントが見つかりません: {checkpoint_id}")

        self._logger.info(f"チェックポイントから再開: {checkpoint_id}")

        # 状態を復元
        phase = checkpoint.get("phase", "execute")
        todos_data = checkpoint.get("todos", [])

        # TodoList を復元
        for todo_data in todos_data:
            todo = TodoItem(**todo_data)
            if todo.id not in self._progress._todos:
                self._progress.add_todo(todo)
            else:
                # 状態を更新
                self._progress.update_todo(todo.id, status=todo.status)

        # コンテキストを復元
        context_data = checkpoint.get("context", {})
        for key, value in context_data.items():
            self._progress.context.set(key, value)

        self._emit_progress("checkpoint_resumed", {
            "checkpoint_id": checkpoint_id,
            "phase": phase,
        })

        # 指定されたフェーズから再開
        if phase in ("execute", "review"):
            return await self._continue_execution(task, checkpoint)
        else:
            # 最初からやり直し
            return await self.execute(task)

    async def _continue_execution(
        self,
        task: str,
        checkpoint: dict[str, Any],
    ) -> dict[str, Any]:
        """チェックポイントから実行を継続."""
        start_time = datetime.now()
        retry_count = checkpoint.get("extra", {}).get("retry_count", 0)

        try:
            while retry_count < self._max_retries:
                result, errors = await self._execute_todos()

                if errors:
                    self._logger.warning(f"実行エラー発生: {len(errors)}件")

                # チェックポイント保存
                if self._checkpoint_enabled:
                    await self.save_checkpoint("review", {"retry_count": retry_count})

                # 品質評審
                review = await self._quality_review(task, result)
                self._emit_progress("quality_review", review.model_dump())

                if review.is_acceptable:
                    if self._evolver:
                        await self._evolver.learn_from_success(
                            task, result, self._progress.context.get_all()
                        )
                    break

                if review.retry_tasks:
                    for todo_id in review.retry_tasks:
                        self._progress.update_todo(todo_id, status=TaskStatus.PENDING)
                    retry_count += 1
                else:
                    break

            execution_time = (datetime.now() - start_time).total_seconds()

            return {
                "status": "success" if review.is_acceptable else "partial",
                "result": result,
                "progress": self._progress.get_progress(),
                "quality_score": review.score,
                "execution_time": execution_time,
                "retries": retry_count,
                "resumed_from": checkpoint.get("checkpoint_id"),
            }

        except Exception as e:
            self._logger.error(f"復旧後の実行失敗: {e}")
            return {"status": "error", "error": str(e)}

    async def list_checkpoints(self) -> list[str]:
        """利用可能なチェックポイント一覧を取得."""
        return await self._runtime_store.list_checkpoints()

    async def delete_checkpoint(self, checkpoint_id: str) -> bool:
        """チェックポイントを削除.

        Args:
            checkpoint_id: 削除するチェックポイントID

        Returns:
            削除成功の場合True
        """
        # RuntimeStore に delete_checkpoint がない場合は save_context を使う
        try:
            # チェックポイントを None で上書き
            await self._runtime_store.save_context(f"_checkpoint:{checkpoint_id}", None)
            return True
        except Exception:
            return False

    @property
    def pattern(self) -> CoordinationPattern:
        """協調パターン種別."""
        return CoordinationPattern.HIERARCHICAL

    async def execute(self, task: str, **kwargs: Any) -> dict[str, Any]:
        """統一実行フロー.

        Args:
            task: 実行タスク
            **kwargs: 追加パラメータ

        Returns:
            実行結果
        """
        self._logger.info(f"DeepAgent実行開始: {task[:50]}...")
        start_time = datetime.now()

        try:
            # Phase 1: 認知分析
            cognitive = await self._cognitive_analysis(task)
            self._emit_progress("cognitive_analysis", cognitive.model_dump())

            # Phase 2: 明確化（必要な場合）
            if not cognitive.is_clear:
                clarification = await self._clarify(task, cognitive.clarification_needed)
                if clarification.get("need_user_input"):
                    return {"status": "need_clarification", "questions": cognitive.clarification_needed}
                task = clarification.get("refined_task", task)

            # Phase 3: 任務分解
            todos = await self._decompose_task(task, cognitive)
            for todo in todos:
                self._progress.add_todo(todo)
            self._emit_progress("task_decomposed", {"todos": len(todos)})

            # Phase 4: 実行ループ
            retry_count = 0
            all_errors: list[tuple[str, Exception]] = []
            result: dict[str, Any] = {}

            while retry_count < self._max_retries:
                result, errors = await self._execute_todos()
                all_errors.extend(errors)

                # エラーがあった場合ログに記録
                if errors:
                    self._logger.warning(f"実行エラー発生: {len(errors)}件")
                    for todo_id, error in errors:
                        self._logger.warning(f"  - {todo_id}: {error}")

                # Phase 5: 品質評審
                review = await self._quality_review(task, result)
                self._emit_progress("quality_review", review.model_dump())

                if review.is_acceptable:
                    # Phase 6: 自己進化
                    if self._evolver:
                        await self._evolver.learn_from_success(
                            task, result, self._progress.context.get_all()
                        )
                    break

                # 不合格: リトライ対象を再実行
                if review.retry_tasks:
                    for todo_id in review.retry_tasks:
                        self._progress.update_todo(todo_id, status=TaskStatus.PENDING)
                    retry_count += 1
                    self._logger.info(f"品質不足、リトライ {retry_count}/{self._max_retries}")
                else:
                    break

            execution_time = (datetime.now() - start_time).total_seconds()

            return {
                "status": "success" if review.is_acceptable else "partial",
                "result": result,
                "progress": self._progress.get_progress(),
                "quality_score": review.score,
                "execution_time": execution_time,
                "retries": retry_count,
                "errors": [{"todo_id": tid, "error": str(e)} for tid, e in all_errors],
            }

        except Exception as e:
            self._logger.error(f"DeepAgent実行失敗: {e}")
            return {"status": "error", "error": str(e)}

    # =========================================================================
    # 内部メソッド
    # =========================================================================

    async def _cognitive_analysis(self, task: str) -> CognitiveAnalysis:
        """認知分析 - 動機・意図を分析."""
        if not self._llm:
            return CognitiveAnalysis(
                intent=task,
                is_clear=True,
                complexity="medium",
                suggested_agents=[AgentType.ANALYSIS.value, AgentType.EXECUTION.value],
            )

        # LLMで分析
        prompt = f"""タスクを分析してください:
タスク: {task}

以下を判定:
1. ユーザーの意図は明確か？(Yes/No)
2. 不明確な場合、追加質問は？
3. 複雑度は？(low/medium/high)
4. 関連領域は？
5. 推薦するAgent種別は？(research/analysis/planning/execution/review/report)

JSON形式で回答:"""

        try:
            response = await self._llm.chat([
                {"role": "system", "content": "あなたはタスク分析の専門家です。JSON形式で回答してください。"},
                {"role": "user", "content": prompt},
            ], response_format={"type": "json_object"})

            import json
            data = json.loads(response.content if hasattr(response, "content") else str(response))

            return CognitiveAnalysis(
                intent=data.get("intent", task),
                is_clear=data.get("is_clear", True),
                clarification_needed=data.get("clarification_needed", []),
                complexity=data.get("complexity", "medium"),
                domains=data.get("domains", []),
                suggested_agents=data.get("suggested_agents", []),
            )
        except Exception as e:
            self._logger.warning(f"認知分析失敗: {e}")
            return CognitiveAnalysis(intent=task, is_clear=True)

    async def _clarify(
        self,
        task: str,
        questions: list[str],
    ) -> dict[str, Any]:
        """明確化処理."""
        # 実際の実装では、ユーザーインタラクションが必要
        # ここでは自動推論を試行
        return {"refined_task": task, "need_user_input": False}

    async def _decompose_task(
        self,
        task: str,
        cognitive: CognitiveAnalysis,
    ) -> list[TodoItem]:
        """任務分解 - 計画生成.

        各サブタスクに対して SkillEngine を使って適切な Skill を自動マッチング/生成する。
        """
        # 学習済みヒントを確認
        hint = self._evolver.get_learned_hint(task) if self._evolver else None

        if not self._llm:
            # デフォルト分解（Skill マッチングあり）
            todos = [
                TodoItem(task="調査・情報収集", agent_type=AgentType.RESEARCH, priority=3),
                TodoItem(task="分析・評価", agent_type=AgentType.ANALYSIS, priority=2, dependencies=[]),
                TodoItem(task="結論・報告作成", agent_type=AgentType.REPORT, priority=1, dependencies=[]),
            ]
            # 各 Todo に Skill をマッチング
            return await self._resolve_skills_for_todos(todos)

        prompt = f"""タスクを実行可能なステップに分解してください:
タスク: {task}
複雑度: {cognitive.complexity}
推薦Agent: {cognitive.suggested_agents}
{f"過去の成功パターン: {hint}" if hint else ""}

JSON形式で回答:
{{"steps": [{{"task": "...", "agent_type": "research|analysis|planning|execution|review|report", "priority": 1-5, "dependencies": []}}]}}"""

        try:
            response = await self._llm.chat([
                {"role": "system", "content": "あなたは計画立案の専門家です。"},
                {"role": "user", "content": prompt},
            ], response_format={"type": "json_object"})

            import json
            data = json.loads(response.content if hasattr(response, "content") else str(response))

            todos = []
            for i, step in enumerate(data.get("steps", [])):
                todos.append(TodoItem(
                    task=step.get("task", f"Step {i+1}"),
                    agent_type=step.get("agent_type", AgentType.EXECUTION),
                    priority=step.get("priority", 1),
                    dependencies=step.get("dependencies", []),
                ))
            todos = todos if todos else self._default_todos(task)

            # 各 Todo に Skill をマッチング/生成
            return await self._resolve_skills_for_todos(todos)

        except Exception as e:
            self._logger.warning(f"任務分解失敗: {e}")
            return self._default_todos(task)

    async def _resolve_skills_for_todos(self, todos: list[TodoItem]) -> list[TodoItem]:
        """各 Todo に対して SkillEngine で Skill をマッチング/生成.

        Args:
            todos: TodoItem リスト

        Returns:
            Skill がマッチングされた TodoItem リスト
        """
        if not self._enable_skill_auto_learn:
            return todos

        for todo in todos:
            try:
                # タスク説明から Skill を検索（生成なし）
                matches = self._skill_engine.find(todo.task, top_k=2)
                if matches:
                    # マッチした Skill を設定
                    todo.skills = [m.skill.name for m in matches]
                    self._logger.debug(f"Todo '{todo.task}' に Skill をマッチ: {todo.skills}")
                else:
                    # マッチなしの場合、auto_learn が有効なら生成を試行
                    if self._enable_skill_auto_learn:
                        try:
                            result = await self._skill_engine.resolve(todo.task)
                            if result.skill:
                                todo.skills = [result.skill.name]
                                self._logger.info(
                                    f"Todo '{todo.task}' に新規 Skill を生成: {result.skill.name}"
                                )
                        except Exception as gen_err:
                            self._logger.warning(f"Skill 生成失敗（続行）: {gen_err}")
            except Exception as e:
                self._logger.warning(f"Skill マッチング失敗（続行）: {e}")

        return todos

    def _default_todos(self, task: str) -> list[TodoItem]:
        """デフォルトTodo生成."""
        return [
            TodoItem(id="todo-1", task=f"分析: {task}", agent_type=AgentType.ANALYSIS, priority=2),
            TodoItem(id="todo-2", task="結論作成", agent_type=AgentType.REPORT, priority=1, dependencies=["todo-1"]),
        ]

    async def _execute_todos(self) -> tuple[dict[str, Any], list[tuple[str, Exception]]]:
        """Todo実行ループ（並行実行対応）.

        依存関係のないタスクは並行実行し、効率化を図る。

        Returns:
            (結果dict, エラーリスト) のタプル
        """
        results: dict[str, Any] = {}
        all_errors: list[tuple[str, Exception]] = []
        iteration = 0

        while iteration < self._max_iterations:
            # 実行可能なTodoを全て取得
            ready_todos = self._progress.get_ready_todos()
            if not ready_todos:
                break  # 全て完了または実行可能なものがない

            # 並行実行
            if len(ready_todos) > 1:
                self._emit_progress("parallel_start", {"count": len(ready_todos)})
                batch_results, batch_errors = await self._execute_batch(ready_todos, results)
                results.update(batch_results)
                all_errors.extend(batch_errors)
                iteration += len(ready_todos)
            else:
                # 単一タスク
                todo = ready_todos[0]
                try:
                    result = await self._execute_single(todo, results)
                    if result:
                        results[todo.id] = result
                except Exception as e:
                    all_errors.append((todo.id, e))
                    self._logger.error(f"単一タスク実行エラー: {todo.id} - {e}")
                iteration += 1

        return results, all_errors

    async def _execute_single(
        self,
        todo: TodoItem,
        context: dict[str, Any],
    ) -> dict[str, Any] | None:
        """単一Todoを実行."""
        self._progress.update_todo(todo.id, status=TaskStatus.IN_PROGRESS)
        self._emit_progress("executing", {"todo_id": todo.id, "task": todo.task})

        agent = await self._agent_pool.get_or_create(
            todo.agent_type,
            context=self._progress.context.get_all(),
        )

        if not agent:
            self._progress.update_todo(todo.id, status=TaskStatus.FAILED, error="Agent not available")
            return None

        try:
            result = await agent.run({"task": todo.task, "context": context})
            todo.result = result

            if result.get("status") == "failed":
                self._progress.update_todo(todo.id, status=TaskStatus.FAILED, error=result.get("error"))
            else:
                self._progress.update_todo(todo.id, status=TaskStatus.COMPLETED, completed_at=datetime.now())
                await self._progress.send_message_async(str(todo.agent_type), "coordinator", result)
                return result
        except Exception as e:
            self._logger.error(f"Todo {todo.id} 実行失敗: {e}")
            self._progress.update_todo(todo.id, status=TaskStatus.FAILED, error=str(e))

        return None

    async def _execute_batch(
        self,
        todos: list[TodoItem],
        context: dict[str, Any],
    ) -> tuple[dict[str, Any], list[tuple[str, Exception]]]:
        """複数Todoを並行実行.

        asyncio.gatherで並行実行し、全ての結果とエラーを収集。

        Args:
            todos: 実行するTodoリスト
            context: 実行コンテキスト

        Returns:
            (結果dict, エラーリスト) のタプル
            エラーリストは (todo_id, Exception) のタプルリスト
        """
        async def execute_one(todo: TodoItem) -> tuple[str, dict[str, Any] | None, Exception | None]:
            try:
                result = await self._execute_single(todo, context)
                return (todo.id, result, None)
            except Exception as e:
                return (todo.id, None, e)

        # 並行実行
        tasks = [execute_one(todo) for todo in todos]
        batch_results = await asyncio.gather(*tasks, return_exceptions=True)

        # 結果とエラーを分離して集約
        results: dict[str, Any] = {}
        errors: list[tuple[str, Exception]] = []

        for item in batch_results:
            if isinstance(item, Exception):
                # asyncio.gather自体の例外（稀）
                self._logger.error(f"並行実行基盤エラー: {item}")
                errors.append(("unknown", item))
                continue

            todo_id, result, error = item
            if error:
                self._logger.error(f"Todo {todo_id} 実行エラー: {error}")
                errors.append((todo_id, error))
            elif result:
                results[todo_id] = result

        self._emit_progress("parallel_end", {
            "completed": len(results),
            "failed": len(errors),
            "error_ids": [e[0] for e in errors],
        })

        return results, errors

    async def _quality_review(
        self,
        task: str,
        results: dict[str, Any],
    ) -> QualityReview:
        """品質評審（多次元評価）.

        評価次元:
        - completeness: タスク要件の充足度
        - accuracy: 結果の正確さ
        - consistency: 全体の整合性
        - efficiency: リソース使用効率
        - clarity: 出力の理解しやすさ
        """
        progress = self._progress.get_progress()

        if not self._llm:
            # ルールベース簡易評価
            is_ok = progress["failed"] == 0 and progress["completed"] == progress["total"]
            completion_rate = progress["progress"]

            dimension_scores = {
                QualityDimension.COMPLETENESS.value: completion_rate * 100,
                QualityDimension.ACCURACY.value: 70.0 if is_ok else 40.0,
                QualityDimension.CONSISTENCY.value: 75.0,
                QualityDimension.EFFICIENCY.value: 70.0,
                QualityDimension.CLARITY.value: 70.0,
            }
            avg_score = sum(dimension_scores.values()) / len(dimension_scores)

            return QualityReview(
                is_acceptable=is_ok and avg_score >= self._quality_threshold,
                score=avg_score,
                dimension_scores=dimension_scores,
                verdict="pass" if is_ok else "revise",
                confidence=0.6,
            )

        prompt = f"""タスク完了結果を多次元評価してください:
元のタスク: {task}
実行結果: {results}
進捗: {progress}

評価基準: 品質閾値 {self._quality_threshold}点

以下の次元で0-100点評価:
- completeness: タスク要件の充足度
- accuracy: 結果の正確さ
- consistency: 全体の整合性
- efficiency: リソース使用効率
- clarity: 出力の理解しやすさ

JSON形式で回答:
{{
  "is_acceptable": true/false,
  "score": 総合スコア0-100,
  "dimension_scores": {{"completeness": 0-100, "accuracy": 0-100, ...}},
  "verdict": "pass/revise/reject",
  "issues": [],
  "suggestions": [],
  "retry_tasks": []
}}"""

        try:
            response = await self._llm.chat([
                {"role": "system", "content": "あなたは品質評価の専門家です。多次元で厳密に評価してください。"},
                {"role": "user", "content": prompt},
            ], response_format={"type": "json_object"})

            import json
            data = json.loads(response.content if hasattr(response, "content") else str(response))

            return QualityReview(
                is_acceptable=data.get("is_acceptable", False),
                score=data.get("score", 0.0),
                dimension_scores=data.get("dimension_scores", {}),
                verdict=data.get("verdict", "pending"),
                issues=data.get("issues", []),
                suggestions=data.get("suggestions", []),
                retry_tasks=data.get("retry_tasks", []),
                confidence=0.8,
            )
        except Exception as e:
            self._logger.warning(f"品質評審失敗: {e}")
            return QualityReview(
                is_acceptable=True,
                score=70.0,
                verdict="pass",
                confidence=0.4,
            )

    def _emit_progress(self, event: str, data: dict[str, Any]) -> None:
        """進捗イベント発火."""
        if self._on_progress:
            self._on_progress({"event": event, "data": data, "timestamp": datetime.now().isoformat()})

    # =========================================================================
    # 公開API
    # =========================================================================

    def register_agent(self, name: str, agent: AgentBlock) -> None:
        """Agentを登録."""
        self._agent_pool.register_agent(name, agent)

    async def process_feedback(
        self,
        feedback_type: str,
        content: str,
    ) -> dict[str, Any]:
        """客户反馈を処理.

        Args:
            feedback_type: suggestion/指摘/education
            content: 反馈内容

        Returns:
            処理結果
        """
        if not self._evolver:
            return {"status": "evolution_disabled"}

        record = await self._evolver.process_feedback(feedback_type, content)
        return {"status": "processed", "record": record.model_dump()}

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        return {
            "progress": self._progress.get_progress(),
            "available_agents": self._agent_pool.list_agents(),
            "evolution": self._evolver.get_evolution_stats() if self._evolver else None,
        }


# =============================================================================
# エクスポート
# =============================================================================

__all__ = [
    # データモデル - 状態・種別
    "TaskStatus",
    "AgentType",
    "MessageType",
    "CompactionStrategy",
    "QualityDimension",
    # データモデル - 構造体
    "TodoItem",
    "CognitiveAnalysis",
    "QualityReview",
    "EvolutionRecord",
    "AgentMessage",
    "ParallelGroup",
    "MemoryTier",
    "CompactionResult",
    # ストレージ抽象（三層設計）
    "RuntimeStore",
    "EvolutionStore",
    "MemoryRuntimeStore",
    "MemoryEvolutionStore",
    # コンテキスト圧縮システム
    "ContextCompressor",
    # コンポーネント
    "AgentPool",
    "DynamicAgent",
    "ProgressManager",
    "Evolver",
    # メイン協調器
    "DeepAgentCoordinator",
]

