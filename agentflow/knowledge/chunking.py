"""高度なチャンキング戦略モジュール.

業界評価の高いチャンキング戦略を提供:
- SemanticChunker: 意味ベースの分割
- RecursiveChunker: 再帰的文字分割（LangChain式）
- TokenChunker: トークンベースの分割
- SentenceChunker: 文単位の分割

使用例:
    >>> from agentflow.knowledge.chunking import get_chunker, ChunkStrategy
    >>>
    >>> # 戦略を選択してチャンカーを取得
    >>> chunker = get_chunker(ChunkStrategy.SEMANTIC)
    >>> chunks = await chunker.chunk(text)
"""

from __future__ import annotations

import hashlib
import logging
import re
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from enum import Enum
from typing import Any


logger = logging.getLogger(__name__)


class ChunkStrategy(str, Enum):
    """チャンキング戦略."""

    FIXED = "fixed"  # 固定サイズ
    RECURSIVE = "recursive"  # 再帰的分割（LangChain式）
    SEMANTIC = "semantic"  # 意味ベース
    SENTENCE = "sentence"  # 文単位
    TOKEN = "token"  # トークンベース
    MARKDOWN = "markdown"  # Markdownヘッダーベース


@dataclass
class ChunkConfig:
    """チャンキング設定.

    Attributes:
        chunk_size: チャンクサイズ（文字数またはトークン数）
        chunk_overlap: オーバーラップサイズ
        strategy: チャンキング戦略
        separators: 分割に使用するセパレータリスト
        min_chunk_size: 最小チャンクサイズ
        keep_separator: セパレータを保持するか
    """

    chunk_size: int = 1000
    chunk_overlap: int = 200
    strategy: ChunkStrategy = ChunkStrategy.RECURSIVE
    separators: list[str] = field(default_factory=lambda: ["\n\n", "\n", "。", ".", " ", ""])
    min_chunk_size: int = 100
    keep_separator: bool = True


@dataclass
class Chunk:
    """チャンク.

    Attributes:
        id: チャンクID
        content: チャンク内容
        metadata: メタデータ
        index: チャンクインデックス
        start_char: 元テキストでの開始位置
        end_char: 元テキストでの終了位置
    """

    id: str
    content: str
    metadata: dict[str, Any] = field(default_factory=dict)
    index: int = 0
    start_char: int = 0
    end_char: int = 0

    @classmethod
    def create(
        cls,
        content: str,
        index: int = 0,
        metadata: dict[str, Any] | None = None,
        start_char: int = 0,
        end_char: int = 0,
    ) -> Chunk:
        """チャンクを作成."""
        content_hash = hashlib.sha256(content.encode()).hexdigest()[:12]
        return cls(
            id=f"{content_hash}_{index}",
            content=content,
            metadata=metadata or {},
            index=index,
            start_char=start_char,
            end_char=end_char,
        )


class BaseChunker(ABC):
    """チャンカー基底クラス."""

    def __init__(self, config: ChunkConfig | None = None) -> None:
        """初期化."""
        self._config = config or ChunkConfig()
        self._logger = logging.getLogger(self.__class__.__name__)

    @property
    def strategy(self) -> ChunkStrategy:
        """戦略を取得."""
        return self._config.strategy

    @abstractmethod
    async def chunk(self, text: str, metadata: dict[str, Any] | None = None) -> list[Chunk]:
        """テキストをチャンクに分割.

        Args:
            text: 入力テキスト
            metadata: 追加メタデータ

        Returns:
            チャンクのリスト
        """
        ...


class RecursiveChunker(BaseChunker):
    """再帰的チャンカー（LangChain RecursiveCharacterTextSplitter 互換）.

    階層的なセパレータを使用して再帰的に分割。
    最も広く使われているチャンキング戦略。
    """

    async def chunk(self, text: str, metadata: dict[str, Any] | None = None) -> list[Chunk]:
        """テキストを再帰的に分割."""
        chunks = self._split_text(text, self._config.separators)

        result: list[Chunk] = []
        current_pos = 0
        for i, chunk_text in enumerate(chunks):
            if chunk_text.strip():
                start = text.find(chunk_text, current_pos)
                if start == -1:
                    start = current_pos
                end = start + len(chunk_text)

                chunk = Chunk.create(
                    content=chunk_text.strip(),
                    index=i,
                    metadata={**(metadata or {}), "strategy": "recursive"},
                    start_char=start,
                    end_char=end,
                )
                result.append(chunk)
                current_pos = end

        self._logger.debug(f"RecursiveChunker: {len(result)} chunks from {len(text)} chars")
        return result

    def _split_text(self, text: str, separators: list[str]) -> list[str]:
        """セパレータリストで再帰的に分割."""
        if not separators:
            return self._split_by_size(text)

        separator = separators[0]
        next_separators = separators[1:]

        splits = text.split(separator) if separator else list(text)

        # 各分割を処理
        chunks: list[str] = []
        current_chunk = ""

        for split in splits:
            potential = (
                current_chunk
                + (separator if current_chunk and self._config.keep_separator else "")
                + split
            )

            if len(potential) <= self._config.chunk_size:
                current_chunk = potential
            else:
                if current_chunk:
                    chunks.append(current_chunk)

                # 長すぎる場合は次のセパレータで再帰分割
                if len(split) > self._config.chunk_size and next_separators:
                    sub_chunks = self._split_text(split, next_separators)
                    chunks.extend(sub_chunks[:-1])
                    current_chunk = sub_chunks[-1] if sub_chunks else ""
                else:
                    current_chunk = split

        if current_chunk:
            chunks.append(current_chunk)

        # オーバーラップを追加
        return self._add_overlap(chunks)

    def _split_by_size(self, text: str) -> list[str]:
        """サイズで分割."""
        chunks = []
        for i in range(0, len(text), self._config.chunk_size - self._config.chunk_overlap):
            chunk = text[i : i + self._config.chunk_size]
            if chunk:
                chunks.append(chunk)
        return chunks

    def _add_overlap(self, chunks: list[str]) -> list[str]:
        """オーバーラップを追加."""
        if not self._config.chunk_overlap or len(chunks) <= 1:
            return chunks

        result: list[str] = []
        for i, chunk in enumerate(chunks):
            if i > 0:
                # 前のチャンクの末尾を追加
                prev_overlap = chunks[i - 1][-self._config.chunk_overlap :]
                chunk = prev_overlap + chunk
            result.append(chunk)
        return result


class SemanticChunker(BaseChunker):
    """意味ベースチャンカー.

    文の埋め込み類似度を使用して意味的な境界で分割。
    高品質だが計算コストが高い。
    """

    def __init__(
        self,
        config: ChunkConfig | None = None,
        similarity_threshold: float = 0.5,
    ) -> None:
        """初期化."""
        super().__init__(config)
        self._similarity_threshold = similarity_threshold
        self._embedding_provider = None

    async def chunk(self, text: str, metadata: dict[str, Any] | None = None) -> list[Chunk]:
        """意味的に分割."""
        # まず文単位で分割
        sentences = self._split_into_sentences(text)

        if len(sentences) <= 1:
            return [
                Chunk.create(
                    content=text.strip(),
                    index=0,
                    metadata={**(metadata or {}), "strategy": "semantic"},
                )
            ]

        try:
            # 埋め込みを取得
            from agentflow.providers import get_embedding

            embedding_provider = get_embedding()
            embeddings = await embedding_provider.embed_batch(sentences)

            # 類似度に基づいてグループ化
            groups = self._group_by_similarity(sentences, embeddings)
        except Exception as e:
            self._logger.warning(f"Semantic chunking failed, falling back to recursive: {e}")
            # フォールバック: 再帰的分割
            fallback = RecursiveChunker(self._config)
            return await fallback.chunk(text, metadata)

        # グループをチャンクに変換
        result: list[Chunk] = []
        for i, group in enumerate(groups):
            content = " ".join(group)
            if len(content) > self._config.chunk_size:
                # 長すぎる場合は再分割
                sub_chunks = await RecursiveChunker(self._config).chunk(content, metadata)
                for _j, sub_chunk in enumerate(sub_chunks):
                    sub_chunk.index = len(result)
                    sub_chunk.metadata["strategy"] = "semantic"
                    result.append(sub_chunk)
            else:
                chunk = Chunk.create(
                    content=content.strip(),
                    index=i,
                    metadata={**(metadata or {}), "strategy": "semantic"},
                )
                result.append(chunk)

        self._logger.debug(f"SemanticChunker: {len(result)} chunks from {len(sentences)} sentences")
        return result

    def _split_into_sentences(self, text: str) -> list[str]:
        """文単位で分割."""
        # 日本語と英語の文末を考慮
        pattern = r"(?<=[。！？.!?])\s*"
        sentences = re.split(pattern, text)
        return [s.strip() for s in sentences if s.strip()]

    def _group_by_similarity(
        self,
        sentences: list[str],
        embeddings: list[list[float]],
    ) -> list[list[str]]:
        """類似度に基づいてグループ化."""
        import numpy as np

        groups: list[list[str]] = [[sentences[0]]]

        for i in range(1, len(sentences)):
            # 前の文との類似度を計算
            prev_emb = np.array(embeddings[i - 1])
            curr_emb = np.array(embeddings[i])

            similarity = np.dot(prev_emb, curr_emb) / (np.linalg.norm(prev_emb) * np.linalg.norm(curr_emb))

            # 類似度が閾値以上なら同じグループ
            if similarity >= self._similarity_threshold:
                groups[-1].append(sentences[i])
            else:
                groups.append([sentences[i]])

        return groups


class SentenceChunker(BaseChunker):
    """文単位チャンカー.

    文を単位として分割し、指定サイズに収まるようにグループ化。
    """

    async def chunk(self, text: str, metadata: dict[str, Any] | None = None) -> list[Chunk]:
        """文単位で分割."""
        sentences = self._split_into_sentences(text)

        chunks: list[Chunk] = []
        current_chunk: list[str] = []
        current_size = 0

        for sentence in sentences:
            sentence_size = len(sentence)

            if current_size + sentence_size > self._config.chunk_size and current_chunk:
                # 現在のチャンクを保存
                content = " ".join(current_chunk)
                chunk = Chunk.create(
                    content=content.strip(),
                    index=len(chunks),
                    metadata={**(metadata or {}), "strategy": "sentence"},
                )
                chunks.append(chunk)

                # オーバーラップ: 最後の数文を保持
                overlap_sentences: list[str] = []
                overlap_size = 0
                for s in reversed(current_chunk):
                    if overlap_size + len(s) <= self._config.chunk_overlap:
                        overlap_sentences.insert(0, s)
                        overlap_size += len(s)
                    else:
                        break

                current_chunk = overlap_sentences
                current_size = overlap_size

            current_chunk.append(sentence)
            current_size += sentence_size

        # 最後のチャンク
        if current_chunk:
            content = " ".join(current_chunk)
            chunk = Chunk.create(
                content=content.strip(),
                index=len(chunks),
                metadata={**(metadata or {}), "strategy": "sentence"},
            )
            chunks.append(chunk)

        self._logger.debug(f"SentenceChunker: {len(chunks)} chunks from {len(sentences)} sentences")
        return chunks

    def _split_into_sentences(self, text: str) -> list[str]:
        """文単位で分割."""
        pattern = r"(?<=[。！？.!?])\s*"
        sentences = re.split(pattern, text)
        return [s.strip() for s in sentences if s.strip()]


class TokenChunker(BaseChunker):
    """トークンベースチャンカー.

    tiktoken を使用してトークン数でチャンク化。
    LLM のコンテキスト制限に合わせた正確な分割が可能。
    """

    def __init__(
        self,
        config: ChunkConfig | None = None,
        model: str = "gpt-4",
    ) -> None:
        """初期化."""
        super().__init__(config)
        self._model = model
        self._encoder: Any = None

    async def chunk(self, text: str, metadata: dict[str, Any] | None = None) -> list[Chunk]:
        """トークンベースで分割."""
        try:
            import tiktoken

            self._encoder = tiktoken.encoding_for_model(self._model)
        except ImportError:
            self._logger.warning("tiktoken not installed, falling back to recursive")
            fallback = RecursiveChunker(self._config)
            return await fallback.chunk(text, metadata)
        except KeyError:
            self._encoder = tiktoken.get_encoding("cl100k_base")

        tokens = self._encoder.encode(text)

        chunks: list[Chunk] = []
        chunk_size = self._config.chunk_size
        overlap = self._config.chunk_overlap

        i = 0
        while i < len(tokens):
            # チャンクのトークンを取得
            chunk_tokens = tokens[i : i + chunk_size]
            content = self._encoder.decode(chunk_tokens)

            chunk = Chunk.create(
                content=content.strip(),
                index=len(chunks),
                metadata={
                    **(metadata or {}),
                    "strategy": "token",
                    "token_count": len(chunk_tokens),
                },
            )
            chunks.append(chunk)

            i += chunk_size - overlap

        self._logger.debug(f"TokenChunker: {len(chunks)} chunks from {len(tokens)} tokens")
        return chunks


class MarkdownChunker(BaseChunker):
    """Markdownヘッダーベースチャンカー.

    Markdownのヘッダー構造を尊重して分割。
    ドキュメントの論理構造を保持。
    """

    async def chunk(self, text: str, metadata: dict[str, Any] | None = None) -> list[Chunk]:
        """Markdownヘッダーで分割."""
        # ヘッダーパターン
        header_pattern = r"^(#{1,6})\s+(.+)$"

        lines = text.split("\n")
        sections: list[tuple[str, str, list[str]]] = []  # (level, header, content_lines)

        current_header = ""
        current_level = ""
        current_content: list[str] = []

        for line in lines:
            match = re.match(header_pattern, line)
            if match:
                # 前のセクションを保存
                if current_content or current_header:
                    sections.append((current_level, current_header, current_content))

                current_level = match.group(1)
                current_header = match.group(2)
                current_content = []
            else:
                current_content.append(line)

        # 最後のセクション
        if current_content or current_header:
            sections.append((current_level, current_header, current_content))

        # セクションをチャンクに変換
        chunks = []
        for level, header, content_lines in sections:
            content = "\n".join(content_lines).strip()
            if not content and not header:
                continue

            full_content = f"{level} {header}\n\n{content}" if header else content

            # サイズチェック
            if len(full_content) > self._config.chunk_size:
                # 長すぎる場合は再分割
                sub_chunker = RecursiveChunker(self._config)
                sub_chunks = await sub_chunker.chunk(full_content, metadata)
                for sub_chunk in sub_chunks:
                    sub_chunk.metadata["section"] = header
                    sub_chunk.metadata["strategy"] = "markdown"
                    chunks.append(sub_chunk)
            else:
                chunk = Chunk.create(
                    content=full_content,
                    index=len(chunks),
                    metadata={
                        **(metadata or {}),
                        "strategy": "markdown",
                        "section": header,
                        "level": len(level),
                    },
                )
                chunks.append(chunk)

        self._logger.debug(f"MarkdownChunker: {len(chunks)} chunks from {len(sections)} sections")
        return chunks


def get_chunker(strategy: ChunkStrategy | str, config: ChunkConfig | None = None) -> BaseChunker:
    """チャンキング戦略に応じたチャンカーを取得.

    Args:
        strategy: チャンキング戦略
        config: チャンキング設定

    Returns:
        対応するチャンカーインスタンス
    """
    if isinstance(strategy, str):
        strategy = ChunkStrategy(strategy)

    chunkers: dict[ChunkStrategy, type[BaseChunker]] = {
        ChunkStrategy.FIXED: RecursiveChunker,  # Fixed は Recursive のサブセット
        ChunkStrategy.RECURSIVE: RecursiveChunker,
        ChunkStrategy.SEMANTIC: SemanticChunker,
        ChunkStrategy.SENTENCE: SentenceChunker,
        ChunkStrategy.TOKEN: TokenChunker,
        ChunkStrategy.MARKDOWN: MarkdownChunker,
    }

    chunker_cls: type[BaseChunker] = chunkers.get(strategy, RecursiveChunker)

    if config:
        config.strategy = strategy
    else:
        config = ChunkConfig(strategy=strategy)

    return chunker_cls(config)


__all__ = [
    "BaseChunker",
    "Chunk",
    "ChunkConfig",
    "ChunkStrategy",
    "MarkdownChunker",
    "RecursiveChunker",
    "SemanticChunker",
    "SentenceChunker",
    "TokenChunker",
    "get_chunker",
]
