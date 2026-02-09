"""EpisodicMemory - エピソード記憶システム（Phase 3.2: 2028）.

【機能】
- タスク実行履歴の自動保存
- 類似タスク検索
- 成功パターン学習
- 経験ベースの意思決定支援

使用例:
    >>> from agentflow.memory import EpisodicMemory
    >>> memory = EpisodicMemory()
    >>> await memory.start()
    >>> await memory.record_episode(task, result, context)
    >>> similar = await memory.find_similar_episodes("市場分析")
"""

from __future__ import annotations

import contextlib
import json
import logging
import uuid
from dataclasses import dataclass, field
from datetime import UTC, datetime
from enum import Enum
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    import asyncio


logger = logging.getLogger(__name__)


class EpisodeOutcome(Enum):
    """エピソード結果."""

    SUCCESS = "success"
    PARTIAL_SUCCESS = "partial_success"
    FAILURE = "failure"
    UNKNOWN = "unknown"


@dataclass
class Episode:
    """エピソード（タスク実行履歴）."""

    episode_id: str = field(default_factory=lambda: str(uuid.uuid4()))
    task: str = ""
    task_type: str = ""
    context: dict[str, Any] = field(default_factory=dict)
    actions: list[dict[str, Any]] = field(default_factory=list)
    result: dict[str, Any] = field(default_factory=dict)
    outcome: EpisodeOutcome = EpisodeOutcome.UNKNOWN
    quality_score: float = 0.0
    duration_seconds: float = 0.0
    timestamp: datetime = field(default_factory=lambda: datetime.now(UTC))
    metadata: dict[str, Any] = field(default_factory=dict)

    # 検索用埋め込み
    embedding: list[float] | None = None

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "episode_id": self.episode_id,
            "task": self.task,
            "task_type": self.task_type,
            "context": self.context,
            "actions": self.actions,
            "result": self.result,
            "outcome": self.outcome.value,
            "quality_score": self.quality_score,
            "duration_seconds": self.duration_seconds,
            "timestamp": self.timestamp.isoformat(),
            "metadata": self.metadata,
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> Episode:
        """辞書から作成."""
        return cls(
            episode_id=data.get("episode_id", str(uuid.uuid4())),
            task=data.get("task", ""),
            task_type=data.get("task_type", ""),
            context=data.get("context", {}),
            actions=data.get("actions", []),
            result=data.get("result", {}),
            outcome=EpisodeOutcome(data.get("outcome", "unknown")),
            quality_score=data.get("quality_score", 0.0),
            duration_seconds=data.get("duration_seconds", 0.0),
            timestamp=datetime.fromisoformat(data["timestamp"])
            if "timestamp" in data
            else datetime.now(UTC),
            metadata=data.get("metadata", {}),
        )


@dataclass
class EpisodeSearchResult:
    """エピソード検索結果."""

    episode: Episode
    similarity: float
    relevance_score: float


class EpisodicMemory:
    """エピソード記憶.

    タスク実行履歴を保存し、類似タスクの検索や成功パターン学習を行う。

    Example:
        >>> memory = EpisodicMemory()
        >>> await memory.start()
        >>> episode = await memory.record_episode("市場分析", result, context)
        >>> similar = await memory.find_similar_episodes("競合分析")
    """

    def __init__(
        self,
        max_episodes: int = 10000,
        embedding_dim: int = 384,
        enable_persistence: bool = False,
        persistence_path: str | None = None,
    ) -> None:
        """初期化.

        Args:
            max_episodes: 最大エピソード数
            embedding_dim: 埋め込み次元数
            enable_persistence: 永続化を有効化
            persistence_path: 永続化パス
        """
        self._max_episodes = max_episodes
        self._embedding_dim = embedding_dim
        self._enable_persistence = enable_persistence
        self._persistence_path = persistence_path

        self._episodes: dict[str, Episode] = {}
        self._by_task_type: dict[str, list[str]] = {}  # task_type -> episode_ids
        self._by_outcome: dict[EpisodeOutcome, list[str]] = {o: [] for o in EpisodeOutcome}

        self._running = False
        self._consolidation_task: asyncio.Task[None] | None = None

    async def start(self) -> None:
        """エピソード記憶を開始."""
        if self._enable_persistence and self._persistence_path:
            await self._load_from_disk()
        self._running = True
        logger.info("Episodic memory started")

    async def stop(self) -> None:
        """エピソード記憶を停止."""
        self._running = False
        if self._enable_persistence and self._persistence_path:
            await self._save_to_disk()
        logger.info("Episodic memory stopped")

    def record_episode(
        self,
        task: str,
        result: dict[str, Any],
        context: dict[str, Any] | None = None,
        actions: list[dict[str, Any]] | None = None,
        task_type: str = "",
        quality_score: float = 0.0,
        duration_seconds: float = 0.0,
    ) -> Episode:
        """エピソードを記録.

        Args:
            task: タスク内容
            result: 実行結果
            context: 実行コンテキスト
            actions: 実行アクションリスト
            task_type: タスクタイプ
            quality_score: 品質スコア
            duration_seconds: 実行時間（秒）

        Returns:
            記録されたエピソード
        """
        # 結果からアウトカムを判定
        outcome = self._determine_outcome(result, quality_score)

        episode = Episode(
            task=task,
            task_type=task_type,
            context=context or {},
            actions=actions or [],
            result=result,
            outcome=outcome,
            quality_score=quality_score,
            duration_seconds=duration_seconds,
        )

        # 埋め込み生成（簡易版: タスク文字列のハッシュベース）
        episode.embedding = self._generate_embedding(task)

        # 保存
        self._episodes[episode.episode_id] = episode

        # インデックス更新
        if task_type:
            if task_type not in self._by_task_type:
                self._by_task_type[task_type] = []
            self._by_task_type[task_type].append(episode.episode_id)

        self._by_outcome[outcome].append(episode.episode_id)

        # 容量制限チェック
        self._enforce_capacity()

        logger.debug(f"Recorded episode: {episode.episode_id} ({outcome.value})")
        return episode

    def find_similar_episodes(
        self,
        query: str,
        limit: int = 5,
        task_type: str | None = None,
        outcome_filter: EpisodeOutcome | None = None,
        min_quality: float = 0.0,
    ) -> list[EpisodeSearchResult]:
        """類似エピソードを検索.

        Args:
            query: 検索クエリ
            limit: 最大件数
            task_type: タスクタイプフィルタ
            outcome_filter: アウトカムフィルタ
            min_quality: 最小品質スコア

        Returns:
            検索結果リスト
        """
        query_embedding = self._generate_embedding(query)
        results: list[EpisodeSearchResult] = []

        # フィルタ対象のエピソードIDを決定
        candidate_ids: set[str]
        if task_type and task_type in self._by_task_type:
            candidate_ids = set(self._by_task_type[task_type])
        elif outcome_filter:
            candidate_ids = set(self._by_outcome[outcome_filter])
        else:
            candidate_ids = set(self._episodes.keys())

        for episode_id in candidate_ids:
            episode = self._episodes.get(episode_id)
            if not episode:
                continue

            # 品質フィルタ
            if episode.quality_score < min_quality:
                continue

            # アウトカムフィルタ
            if outcome_filter and episode.outcome != outcome_filter:
                continue

            # 類似度計算
            similarity = self._cosine_similarity(query_embedding, episode.embedding or [])

            # 関連性スコア（類似度 + 品質 + 成功率）
            relevance = similarity * 0.6 + (episode.quality_score / 100) * 0.3
            if episode.outcome == EpisodeOutcome.SUCCESS:
                relevance += 0.1

            results.append(
                EpisodeSearchResult(
                    episode=episode,
                    similarity=similarity,
                    relevance_score=relevance,
                )
            )

        # 関連性でソート
        results.sort(key=lambda x: x.relevance_score, reverse=True)
        return results[:limit]

    def get_success_patterns(
        self,
        task_type: str | None = None,
        min_quality: float = 70.0,
    ) -> list[Episode]:
        """成功パターンを取得.

        Args:
            task_type: タスクタイプフィルタ
            min_quality: 最小品質スコア

        Returns:
            成功エピソードリスト
        """
        success_ids = self._by_outcome[EpisodeOutcome.SUCCESS]
        patterns: list[Episode] = []

        for episode_id in success_ids:
            episode = self._episodes.get(episode_id)
            if not episode:
                continue

            if episode.quality_score < min_quality:
                continue

            if task_type and episode.task_type != task_type:
                continue

            patterns.append(episode)

        # 品質スコア順でソート
        patterns.sort(key=lambda x: x.quality_score, reverse=True)
        return patterns

    def get_statistics(self) -> dict[str, Any]:
        """統計情報を取得."""
        total = len(self._episodes)
        by_outcome = {o.value: len(ids) for o, ids in self._by_outcome.items()}
        by_type = {t: len(ids) for t, ids in self._by_task_type.items()}

        avg_quality = 0.0
        if total > 0:
            avg_quality = sum(e.quality_score for e in self._episodes.values()) / total

        return {
            "total_episodes": total,
            "by_outcome": by_outcome,
            "by_task_type": by_type,
            "average_quality": avg_quality,
            "success_rate": by_outcome.get("success", 0) / total if total > 0 else 0,
        }

    def _determine_outcome(self, result: dict[str, Any], quality_score: float) -> EpisodeOutcome:
        """結果からアウトカムを判定."""
        if result.get("error"):
            return EpisodeOutcome.FAILURE
        if result.get("success") is False:
            return EpisodeOutcome.FAILURE
        if quality_score >= 80:
            return EpisodeOutcome.SUCCESS
        if quality_score >= 50:
            return EpisodeOutcome.PARTIAL_SUCCESS
        if result.get("success") is True:
            return EpisodeOutcome.SUCCESS
        return EpisodeOutcome.UNKNOWN

    def _generate_embedding(self, text: str) -> list[float]:
        """埋め込みを生成（簡易版）."""
        # 実際の実装ではOpenAI/Sentenceトランスフォーマーを使用
        import hashlib

        hash_bytes = hashlib.sha256(text.encode()).digest()
        # 正規化されたベクトルを生成
        embedding = [b / 255.0 - 0.5 for b in hash_bytes[: self._embedding_dim]]
        # パディング
        while len(embedding) < self._embedding_dim:
            embedding.append(0.0)
        return embedding[: self._embedding_dim]

    def _cosine_similarity(self, vec1: list[float], vec2: list[float]) -> float:
        """コサイン類似度を計算."""
        if not vec1 or not vec2 or len(vec1) != len(vec2):
            return 0.0

        dot_product = sum(a * b for a, b in zip(vec1, vec2, strict=False))
        norm1 = sum(a * a for a in vec1) ** 0.5
        norm2 = sum(b * b for b in vec2) ** 0.5

        if norm1 == 0 or norm2 == 0:
            return 0.0

        return dot_product / (norm1 * norm2)

    def _enforce_capacity(self) -> None:
        """容量制限を適用."""
        if len(self._episodes) <= self._max_episodes:
            return

        # 古いエピソードを削除（品質の低いものを優先）
        episodes_list = list(self._episodes.values())
        episodes_list.sort(key=lambda x: (x.quality_score, x.timestamp))

        remove_count = len(self._episodes) - self._max_episodes
        for episode in episodes_list[:remove_count]:
            del self._episodes[episode.episode_id]
            # インデックスからも削除
            if episode.task_type in self._by_task_type:
                with contextlib.suppress(ValueError):
                    self._by_task_type[episode.task_type].remove(episode.episode_id)
            with contextlib.suppress(ValueError):
                self._by_outcome[episode.outcome].remove(episode.episode_id)

    async def _load_from_disk(self) -> None:
        """ディスクから読み込み."""
        if not self._persistence_path:
            return
        try:
            import aiofiles

            async with aiofiles.open(self._persistence_path) as f:
                data = json.loads(await f.read())
                for ep_data in data.get("episodes", []):
                    episode = Episode.from_dict(ep_data)
                    self._episodes[episode.episode_id] = episode
                    if episode.task_type:
                        if episode.task_type not in self._by_task_type:
                            self._by_task_type[episode.task_type] = []
                        self._by_task_type[episode.task_type].append(episode.episode_id)
                    self._by_outcome[episode.outcome].append(episode.episode_id)
            logger.info(f"Loaded {len(self._episodes)} episodes from disk")
        except FileNotFoundError:
            logger.info("No persisted episodes found")
        except Exception as e:
            logger.warning(f"Failed to load episodes: {e}")

    async def _save_to_disk(self) -> None:
        """ディスクに保存."""
        if not self._persistence_path:
            return
        try:
            import aiofiles

            data = {"episodes": [e.to_dict() for e in self._episodes.values()]}
            async with aiofiles.open(self._persistence_path, "w") as f:
                await f.write(json.dumps(data, ensure_ascii=False, indent=2))
            logger.info(f"Saved {len(self._episodes)} episodes to disk")
        except Exception as e:
            logger.warning(f"Failed to save episodes: {e}")


__all__ = [
    "Episode",
    "EpisodeOutcome",
    "EpisodeSearchResult",
    "EpisodicMemory",
]
