"""AgentFlow Memory System - Type Definitions.

LightMemの思想に基づいた記憶システムの型定義。

参考論文: LightMem - 3段階記憶システム
- Light1: 感覚記憶（予圧縮 + トピック分割）
- Light2: 短期記憶（トピックバッファ + 要約生成）
- Light3: 長期記憶（オンライン追加 + オフライン統合）

拡張機能（HOPE/Evo-Memory思想）:
- 意味レベル分類（エピソード/セマンティック/手続き）
- 安定性レベル（揮発性/固定/結晶化）
- 自動蒸留と忘却
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any


class MemoryType(str, Enum):
    """記憶タイプ.

    - SENSORY: 感覚記憶（一時的、圧縮済み）
    - SHORT_TERM: 短期記憶（トピック別バッファ）
    - LONG_TERM: 長期記憶（永続化、統合済み）
    """

    SENSORY = "sensory"
    SHORT_TERM = "short_term"
    LONG_TERM = "long_term"


class MemorySemanticLevel(str, Enum):
    """記憶の意味レベル（HOPE風アーキテクチャ）.

    人間の記憶モデルに基づく3段階分類:
    - EPISODIC: エピソード記憶（具体的なイベント、時間・場所に関連）
    - SEMANTIC: セマンティック記憶（抽象的な知識、ルール、概念）
    - PROCEDURAL: 手続き記憶（スキル、操作手順、パターン）
    """

    EPISODIC = "episodic"
    SEMANTIC = "semantic"
    PROCEDURAL = "procedural"


class MemoryStability(str, Enum):
    """記憶の安定性レベル.

    記憶の固定度合いを示す:
    - VOLATILE: 揮発性（新情報で上書き可能、短期的）
    - CONSOLIDATED: 固定済み（明示的な更新が必要）
    - CRYSTALLIZED: 結晶化（コア知識、極めて安定）
    """

    VOLATILE = "volatile"
    CONSOLIDATED = "consolidated"
    CRYSTALLIZED = "crystallized"


@dataclass
class MemoryEntry:
    """記憶エントリの基本構造.

    全ての記憶タイプで共通の基本構造。

    Attributes:
        id: 一意識別子
        content: 記憶内容（圧縮済みまたは要約済み）
        topic: トピック名
        timestamp: 作成タイムスタンプ
        memory_type: 記憶タイプ
        importance_score: 重要度スコア（0.0-1.0）
        metadata: 追加メタデータ
        semantic_level: 意味レベル（エピソード/セマンティック/手続き）
        stability: 安定性レベル（揮発性/固定/結晶化）
        access_count: アクセス回数（自動追跡）
        last_accessed: 最終アクセス日時
        parent_id: 蒸留元の記憶ID（派生記憶の場合）
        reinforcement_score: 強化学習スコア（タスク成功/失敗フィードバック）
    """

    id: str
    content: str
    topic: str
    timestamp: datetime
    memory_type: MemoryType
    importance_score: float = 0.5
    metadata: dict[str, Any] = field(default_factory=dict)
    # 拡張フィールド（自動管理、ユーザーは意識不要）
    semantic_level: MemorySemanticLevel = MemorySemanticLevel.EPISODIC
    stability: MemoryStability = MemoryStability.VOLATILE
    access_count: int = 0
    last_accessed: datetime | None = None
    parent_id: str | None = None
    reinforcement_score: float = 0.0

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換.

        Returns:
            辞書形式の記憶エントリ
        """
        return {
            "id": self.id,
            "content": self.content,
            "topic": self.topic,
            "timestamp": self.timestamp.isoformat(),
            "memory_type": self.memory_type.value,
            "importance_score": self.importance_score,
            "metadata": self.metadata,
            "semantic_level": self.semantic_level.value,
            "stability": self.stability.value,
            "access_count": self.access_count,
            "last_accessed": self.last_accessed.isoformat() if self.last_accessed else None,
            "parent_id": self.parent_id,
            "reinforcement_score": self.reinforcement_score,
        }

    def record_access(self) -> None:
        """アクセスを記録（内部使用）."""
        self.access_count += 1
        self.last_accessed = datetime.now()

    def get_effective_importance(self) -> float:
        """実効重要度を計算（重要度 + 強化スコア）.

        Returns:
            実効重要度スコア（0.0-1.0）
        """
        return max(0.0, min(1.0, self.importance_score + self.reinforcement_score * 0.2))


@dataclass
class CompressionConfig:
    """圧縮設定.

    Light1（感覚記憶）の圧縮パラメータ。

    Attributes:
        compression_ratio: 圧縮率（0.0-1.0、0.6 = 40%削減）
        min_importance_threshold: 最小重要度閾値
        enable_topic_segmentation: トピック分割を有効化
    """

    compression_ratio: float = 0.6
    min_importance_threshold: float = 0.3
    enable_topic_segmentation: bool = True


@dataclass
class TopicBuffer:
    """トピックバッファ.

    Light2（短期記憶）のトピック別バッファ構造。

    Attributes:
        topic: トピック名
        entries: 記憶エントリのリスト
        total_tokens: 累積Token数
        created_at: 作成タイムスタンプ
        last_updated: 最終更新タイムスタンプ
    """

    topic: str
    entries: list[MemoryEntry] = field(default_factory=list)
    total_tokens: int = 0
    created_at: datetime = field(default_factory=datetime.now)
    last_updated: datetime = field(default_factory=datetime.now)

    def add_entry(self, entry: MemoryEntry, token_count: int) -> None:
        """エントリを追加.

        Args:
            entry: 記憶エントリ
            token_count: Token数
        """
        self.entries.append(entry)
        self.total_tokens += token_count
        self.last_updated = datetime.now()

    def should_summarize(self, token_threshold: int) -> bool:
        """要約が必要かチェック.

        Args:
            token_threshold: Token閾値

        Returns:
            要約が必要な場合True
        """
        return self.total_tokens >= token_threshold


@dataclass
class UpdateQueue:
    """更新キュー.

    Light3（長期記憶）のオフライン統合用キュー。

    Attributes:
        entry_id: 記憶エントリID
        updates: 更新内容のリスト
        created_at: 作成タイムスタンプ
    """

    entry_id: str
    updates: list[dict[str, Any]] = field(default_factory=list)
    created_at: datetime = field(default_factory=datetime.now)

    def add_update(self, update: dict[str, Any]) -> None:
        """更新を追加.

        Args:
            update: 更新内容
        """
        self.updates.append({
            **update,
            "timestamp": datetime.now().isoformat(),
        })

