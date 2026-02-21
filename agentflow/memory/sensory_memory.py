"""AgentFlow Memory System - Sensory Memory (Light1).

感覚記憶: 予圧縮 + トピック分割により冗長Tokenを削減。

参考論文: LightMem Light1
- 予圧縮: LLMLingua-2風の重要度スコアリング
- トピック分割: 意味的な境界で分割
- 動的閾値: 百分位数ベースの保持判定
"""

from __future__ import annotations

import logging
import re
import uuid
from datetime import datetime
from typing import Any

from agentflow.memory.types import CompressionConfig, MemoryEntry, MemoryType


class SensoryMemory:
    """感覚記憶（Light1）.

    職責:
    - 入力テキストの予圧縮
    - トピック分割
    - 重要度スコアリング
    - 冗長Token削減（20-80%）

    Example:
        >>> config = CompressionConfig(compression_ratio=0.6)
        >>> sensory = SensoryMemory(config)
        >>> entry = await sensory.process("長いテキスト...", topic="AI")
    """

    def __init__(self, config: CompressionConfig | None = None) -> None:
        """初期化.

        Args:
            config: 圧縮設定
        """
        self._config = config or CompressionConfig()
        self._logger = logging.getLogger(__name__)

    async def process(
        self,
        text: str,
        topic: str | None = None,
        metadata: dict[str, Any] | None = None,
    ) -> MemoryEntry:
        """テキストを処理して感覚記憶エントリを作成.

        Args:
            text: 入力テキスト
            topic: トピック名（自動検出も可能）
            metadata: 追加メタデータ

        Returns:
            圧縮済み記憶エントリ
        """
        self._logger.debug(f"Processing text: {len(text)} chars")

        # トピック検出
        if topic is None:
            topic = self._detect_topic(text)

        # 重要度スコアリング
        importance_scores = self._score_importance(text)

        # 動的閾値による圧縮
        compressed_text = self._compress_text(text, importance_scores)

        # 記憶エントリ作成
        entry = MemoryEntry(
            id=str(uuid.uuid4()),
            content=compressed_text,
            topic=topic,
            timestamp=datetime.now(),
            memory_type=MemoryType.SENSORY,
            importance_score=sum(importance_scores) / len(importance_scores),
            metadata=metadata or {},
        )

        self._logger.info(
            f"Compressed: {len(text)} -> {len(compressed_text)} chars "
            f"({(1 - len(compressed_text) / len(text)) * 100:.1f}% reduction)"
        )

        return entry

    def _detect_topic(self, text: str) -> str:
        """トピックを自動検出.

        Args:
            text: 入力テキスト

        Returns:
            検出されたトピック名
        """
        # 簡易実装: 最初の文からキーワード抽出
        # 実際にはLLMやNLPモデルを使用可能
        first_sentence = text.split(".")[0] if "." in text else text[:100]
        words = re.findall(r"\b\w+\b", first_sentence.lower())

        # 最も長い単語をトピックとする（簡易版）
        return max(words, key=len) if words else "general"

    def _score_importance(self, text: str) -> list[float]:
        """テキストの重要度をスコアリング.

        Args:
            text: 入力テキスト

        Returns:
            単語ごとの重要度スコア（0.0-1.0）
        """
        # 簡易実装: 単語の長さと位置に基づくスコアリング
        # 実際にはLLMLingua-2風のモデルを使用可能
        words = text.split()
        scores = []

        for i, word in enumerate(words):
            # 基本スコア: 単語の長さ
            base_score = min(len(word) / 10.0, 1.0)

            # 位置ボーナス: 最初と最後の単語は重要
            position_bonus = 0.2 if i < 5 or i >= len(words) - 5 else 0.0

            # 大文字ボーナス: 固有名詞など
            capital_bonus = 0.1 if word and word[0].isupper() else 0.0

            score = min(base_score + position_bonus + capital_bonus, 1.0)
            scores.append(score)

        return scores

    def _compress_text(self, text: str, importance_scores: list[float]) -> str:
        """重要度スコアに基づいてテキストを圧縮.

        Args:
            text: 入力テキスト
            importance_scores: 重要度スコアリスト

        Returns:
            圧縮済みテキスト
        """
        words = text.split()

        if len(words) != len(importance_scores):
            self._logger.warning("Word count mismatch, using original text")
            return text

        # 動的閾値: 百分位数ベース
        threshold = self._calculate_threshold(importance_scores)

        # 閾値以上の単語のみ保持
        compressed_words = [word for word, score in zip(words, importance_scores, strict=False) if score >= threshold]

        return " ".join(compressed_words)

    def _calculate_threshold(self, scores: list[float]) -> float:
        """動的閾値を計算.

        Args:
            scores: 重要度スコアリスト

        Returns:
            閾値
        """
        if not scores:
            return 0.0

        # 圧縮率に基づく百分位数
        sorted_scores = sorted(scores)
        percentile_index = int(len(sorted_scores) * (1 - self._config.compression_ratio))
        threshold = sorted_scores[percentile_index] if percentile_index < len(sorted_scores) else 0.0

        # 最小閾値を適用
        return max(threshold, self._config.min_importance_threshold)
