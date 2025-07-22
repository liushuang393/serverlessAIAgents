"""
Chunker（チャンカー）コンポーネント

このモジュールは、長文分割と前処理のための抽象インターフェースと
具体的な実装を提供します。
"""

import re
import uuid
from abc import ABC, abstractmethod
from enum import Enum
from typing import Any, Dict, List, Optional

from ..utils.logging import get_logger
from .models import TextChunk

logger = get_logger(__name__)


class ChunkStrategy(str, Enum):
    """チャンク分割戦略"""

    CHARACTER = "character"  # 文字数ベース
    WORD = "word"  # 単語ベース
    SENTENCE = "sentence"  # 文ベース
    PARAGRAPH = "paragraph"  # 段落ベース
    SEMANTIC = "semantic"  # 意味ベース


class ChunkerInterface(ABC):
    """長文分割と前処理のための抽象インターフェース"""

    @abstractmethod
    async def chunk(
        self,
        text: str,
        chunk_size: int = 1000,
        overlap: int = 200,
        metadata: Optional[Dict[str, Any]] = None,
    ) -> List[TextChunk]:
        """
        テキストをチャンクに分割する

        Args:
            text: 分割するテキスト
            chunk_size: チャンクの最大サイズ
            overlap: チャンク間のオーバーラップサイズ
            metadata: 追加のメタデータ

        Returns:
            List[TextChunk]: 分割されたテキストチャンクのリスト
        """
        pass

    @abstractmethod
    async def merge_chunks(self, chunks: List[TextChunk]) -> str:
        """
        チャンクを結合する

        Args:
            chunks: 結合するチャンクのリスト

        Returns:
            str: 結合されたテキスト
        """
        pass


class SimpleChunker(ChunkerInterface):
    """シンプルな文字数ベースのチャンカー"""

    def __init__(self, strategy: ChunkStrategy = ChunkStrategy.CHARACTER):
        """
        シンプルなチャンカーを初期化する

        Args:
            strategy: チャンク分割戦略
        """
        self.strategy = strategy
        logger.info(f"シンプルなチャンカーを初期化しました（戦略: {strategy}）")

    async def chunk(
        self,
        text: str,
        chunk_size: int = 1000,
        overlap: int = 200,
        metadata: Optional[Dict[str, Any]] = None,
    ) -> List[TextChunk]:
        """
        テキストをチャンクに分割する

        Args:
            text: 分割するテキスト
            chunk_size: チャンクの最大サイズ
            overlap: チャンク間のオーバーラップサイズ
            metadata: 追加のメタデータ

        Returns:
            List[TextChunk]: 分割されたテキストチャンクのリスト
        """
        if not text.strip():
            return []

        # 戦略に応じて分割
        if self.strategy == ChunkStrategy.CHARACTER:
            return await self._chunk_by_character(text, chunk_size, overlap, metadata)
        elif self.strategy == ChunkStrategy.WORD:
            return await self._chunk_by_word(text, chunk_size, overlap, metadata)
        elif self.strategy == ChunkStrategy.SENTENCE:
            return await self._chunk_by_sentence(text, chunk_size, overlap, metadata)
        elif self.strategy == ChunkStrategy.PARAGRAPH:
            return await self._chunk_by_paragraph(text, chunk_size, overlap, metadata)
        else:
            # デフォルトは文字数ベース
            return await self._chunk_by_character(text, chunk_size, overlap, metadata)

    async def merge_chunks(self, chunks: List[TextChunk]) -> str:
        """
        チャンクを結合する

        Args:
            chunks: 結合するチャンクのリスト

        Returns:
            str: 結合されたテキスト
        """
        if not chunks:
            return ""

        # チャンクを位置順にソート
        sorted_chunks = sorted(chunks, key=lambda x: x.start_index)

        # テキストを結合
        merged_text = ""
        last_end = 0

        for chunk in sorted_chunks:
            # ギャップがある場合は空白で埋める
            if chunk.start_index > last_end:
                merged_text += " " * (chunk.start_index - last_end)

            merged_text += chunk.text
            last_end = chunk.end_index

        return merged_text

    async def _chunk_by_character(
        self,
        text: str,
        chunk_size: int,
        overlap: int,
        metadata: Optional[Dict[str, Any]],
    ) -> List[TextChunk]:
        """文字数ベースでチャンクに分割する"""
        chunks: List[TextChunk] = []
        start = 0

        while start < len(text):
            end = min(start + chunk_size, len(text))

            # チャンクテキストを抽出
            chunk_text = text[start:end]

            # チャンクメタデータを作成
            chunk_metadata = metadata.copy() if metadata else {}
            chunk_metadata.update(
                {
                    "strategy": self.strategy.value,
                    "chunk_index": len(chunks),
                    "original_length": len(text),
                }
            )

            # TextChunkオブジェクトを作成
            chunk = TextChunk(
                text=chunk_text,
                start_index=start,
                end_index=end,
                metadata=chunk_metadata,
                chunk_id=str(uuid.uuid4()),
            )

            chunks.append(chunk)

            # 次の開始位置を計算（オーバーラップを考慮）
            start = end - overlap
            if start >= end:  # 無限ループを防ぐ
                break

        logger.debug(f"文字数ベースで{len(chunks)}個のチャンクに分割しました")
        return chunks

    async def _chunk_by_word(
        self,
        text: str,
        chunk_size: int,
        overlap: int,
        metadata: Optional[Dict[str, Any]],
    ) -> List[TextChunk]:
        """単語ベースでチャンクに分割する"""
        # 単語に分割
        words = re.findall(r"\S+|\s+", text)
        chunks: List[TextChunk] = []
        start_word_idx = 0

        while start_word_idx < len(words):
            # チャンクサイズ分の単語を取得
            chunk_words = []
            current_size = 0
            word_idx = start_word_idx

            while word_idx < len(words) and current_size < chunk_size:
                word = words[word_idx]
                chunk_words.append(word)
                current_size += len(word)
                word_idx += 1

            # チャンクテキストを作成
            chunk_text = "".join(chunk_words)

            # 元テキストでの位置を計算
            start_pos = len("".join(words[:start_word_idx]))
            end_pos = start_pos + len(chunk_text)

            # チャンクメタデータを作成
            chunk_metadata = metadata.copy() if metadata else {}
            chunk_metadata.update(
                {
                    "strategy": self.strategy.value,
                    "chunk_index": len(chunks),
                    "word_count": len([w for w in chunk_words if w.strip()]),
                    "original_length": len(text),
                }
            )

            # TextChunkオブジェクトを作成
            chunk = TextChunk(
                text=chunk_text,
                start_index=start_pos,
                end_index=end_pos,
                metadata=chunk_metadata,
                chunk_id=str(uuid.uuid4()),
            )

            chunks.append(chunk)

            # 次の開始位置を計算（オーバーラップを考慮）
            overlap_words = min(overlap // 10, len(chunk_words))  # 概算でオーバーラップ単語数を計算
            start_word_idx = max(
                start_word_idx + len(chunk_words) - overlap_words, start_word_idx + 1
            )

        logger.debug(f"単語ベースで{len(chunks)}個のチャンクに分割しました")
        return chunks

    async def _chunk_by_sentence(
        self,
        text: str,
        chunk_size: int,
        overlap: int,
        metadata: Optional[Dict[str, Any]],
    ) -> List[TextChunk]:
        """文ベースでチャンクに分割する"""
        # 文に分割（日本語と英語に対応）
        sentence_pattern = r"[.!?。！？]+\s*"
        sentences = re.split(sentence_pattern, text)

        # 区切り文字も保持
        separators = re.findall(sentence_pattern, text)

        # 文と区切り文字を結合
        full_sentences = []
        for i, sentence in enumerate(sentences[:-1]):
            if sentence.strip():
                full_sentences.append(sentence + separators[i])

        # 最後の文を追加
        if sentences[-1].strip():
            full_sentences.append(sentences[-1])

        chunks: List[TextChunk] = []
        start_sentence_idx = 0

        while start_sentence_idx < len(full_sentences):
            # チャンクサイズ分の文を取得
            chunk_sentences = []
            current_size = 0
            sentence_idx = start_sentence_idx

            while sentence_idx < len(full_sentences) and current_size < chunk_size:
                sentence = full_sentences[sentence_idx]
                chunk_sentences.append(sentence)
                current_size += len(sentence)
                sentence_idx += 1

            # チャンクテキストを作成
            chunk_text = "".join(chunk_sentences)

            # 元テキストでの位置を計算
            start_pos = len("".join(full_sentences[:start_sentence_idx]))
            end_pos = start_pos + len(chunk_text)

            # チャンクメタデータを作成
            chunk_metadata = metadata.copy() if metadata else {}
            chunk_metadata.update(
                {
                    "strategy": self.strategy.value,
                    "chunk_index": len(chunks),
                    "sentence_count": len(chunk_sentences),
                    "original_length": len(text),
                }
            )

            # TextChunkオブジェクトを作成
            chunk = TextChunk(
                text=chunk_text,
                start_index=start_pos,
                end_index=end_pos,
                metadata=chunk_metadata,
                chunk_id=str(uuid.uuid4()),
            )

            chunks.append(chunk)

            # 次の開始位置を計算（オーバーラップを考慮）
            overlap_sentences = min(
                overlap // 100, len(chunk_sentences)
            )  # 概算でオーバーラップ文数を計算
            start_sentence_idx = max(
                start_sentence_idx + len(chunk_sentences) - overlap_sentences,
                start_sentence_idx + 1,
            )

        logger.debug(f"文ベースで{len(chunks)}個のチャンクに分割しました")
        return chunks

    async def _chunk_by_paragraph(
        self,
        text: str,
        chunk_size: int,
        overlap: int,
        metadata: Optional[Dict[str, Any]],
    ) -> List[TextChunk]:
        """段落ベースでチャンクに分割する"""
        # 段落に分割
        paragraphs = re.split(r"\n\s*\n", text)

        chunks: List[TextChunk] = []
        start_para_idx = 0

        while start_para_idx < len(paragraphs):
            # チャンクサイズ分の段落を取得
            chunk_paragraphs = []
            current_size = 0
            para_idx = start_para_idx

            while para_idx < len(paragraphs) and current_size < chunk_size:
                paragraph = paragraphs[para_idx]
                chunk_paragraphs.append(paragraph)
                current_size += len(paragraph)
                para_idx += 1

            # チャンクテキストを作成
            chunk_text = "\n\n".join(chunk_paragraphs)

            # 元テキストでの位置を計算
            start_pos = 0
            for i in range(start_para_idx):
                start_pos += len(paragraphs[i]) + 2  # \n\n分を追加

            end_pos = start_pos + len(chunk_text)

            # チャンクメタデータを作成
            chunk_metadata = metadata.copy() if metadata else {}
            chunk_metadata.update(
                {
                    "strategy": self.strategy.value,
                    "chunk_index": len(chunks),
                    "paragraph_count": len(chunk_paragraphs),
                    "original_length": len(text),
                }
            )

            # TextChunkオブジェクトを作成
            chunk = TextChunk(
                text=chunk_text,
                start_index=start_pos,
                end_index=end_pos,
                metadata=chunk_metadata,
                chunk_id=str(uuid.uuid4()),
            )

            chunks.append(chunk)

            # 次の開始位置を計算（オーバーラップを考慮）
            overlap_paras = min(1, len(chunk_paragraphs))  # 段落の場合は1段落オーバーラップ
            start_para_idx = max(
                start_para_idx + len(chunk_paragraphs) - overlap_paras,
                start_para_idx + 1,
            )

        logger.debug(f"段落ベースで{len(chunks)}個のチャンクに分割しました")
        return chunks


class SmartChunker(ChunkerInterface):
    """スマートなチャンカー（複数の戦略を組み合わせ）"""

    def __init__(self):
        """スマートなチャンカーを初期化する"""
        self.character_chunker = SimpleChunker(ChunkStrategy.CHARACTER)
        self.sentence_chunker = SimpleChunker(ChunkStrategy.SENTENCE)
        self.paragraph_chunker = SimpleChunker(ChunkStrategy.PARAGRAPH)

        logger.info("スマートなチャンカーを初期化しました")

    async def chunk(
        self,
        text: str,
        chunk_size: int = 1000,
        overlap: int = 200,
        metadata: Optional[Dict[str, Any]] = None,
    ) -> List[TextChunk]:
        """
        テキストをスマートにチャンクに分割する

        Args:
            text: 分割するテキスト
            chunk_size: チャンクの最大サイズ
            overlap: チャンク間のオーバーラップサイズ
            metadata: 追加のメタデータ

        Returns:
            List[TextChunk]: 分割されたテキストチャンクのリスト
        """
        if not text.strip():
            return []

        # テキストの特性を分析
        has_paragraphs = "\n\n" in text
        has_sentences = bool(re.search(r"[.!?。！？]", text))

        # 最適な戦略を選択
        if has_paragraphs and len(text) > chunk_size * 2:
            # 段落がある長いテキストの場合
            return await self.paragraph_chunker.chunk(
                text, chunk_size, overlap, metadata
            )
        elif has_sentences:
            # 文がある場合
            return await self.sentence_chunker.chunk(
                text, chunk_size, overlap, metadata
            )
        else:
            # その他の場合は文字数ベース
            return await self.character_chunker.chunk(
                text, chunk_size, overlap, metadata
            )

    async def merge_chunks(self, chunks: List[TextChunk]) -> str:
        """
        チャンクを結合する

        Args:
            chunks: 結合するチャンクのリスト

        Returns:
            str: 結合されたテキスト
        """
        # 最初のチャンクの戦略を使用
        if (
            chunks
            and chunks[0].metadata.get("strategy") == ChunkStrategy.PARAGRAPH.value
        ):
            return await self.paragraph_chunker.merge_chunks(chunks)
        elif (
            chunks
            and chunks[0].metadata.get("strategy") == ChunkStrategy.SENTENCE.value
        ):
            return await self.sentence_chunker.merge_chunks(chunks)
        else:
            return await self.character_chunker.merge_chunks(chunks)
