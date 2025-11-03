"""
テキスト処理ユーティリティ

このモジュールは、テキストチャンキング（固定サイズ、文ベース、段落ベース、
セマンティック）、テキスト前処理、正規化などの機能を提供します。
"""

import re
from typing import List, Optional, Dict, Any, Callable
import logging

logger = logging.getLogger(__name__)


class TextChunk:
    """テキストチャンククラス"""
    
    def __init__(
        self,
        text: str,
        start_index: int,
        end_index: int,
        chunk_id: Optional[str] = None,
        metadata: Optional[Dict[str, Any]] = None
    ):
        self.text = text
        self.start_index = start_index
        self.end_index = end_index
        self.chunk_id = chunk_id
        self.metadata = metadata or {}
    
    def __len__(self) -> int:
        return len(self.text)
    
    def __str__(self) -> str:
        return self.text
    
    def to_dict(self) -> Dict[str, Any]:
        """辞書形式に変換"""
        return {
            "text": self.text,
            "start_index": self.start_index,
            "end_index": self.end_index,
            "chunk_id": self.chunk_id,
            "metadata": self.metadata,
            "length": len(self.text)
        }


class TextChunker:
    """テキストチャンキングの基底クラス"""
    
    def __init__(self, chunk_size: int = 1000, overlap: int = 200):
        self.chunk_size = chunk_size
        self.overlap = overlap
    
    def chunk(self, text: str, **kwargs) -> List[TextChunk]:
        """テキストをチャンクに分割（サブクラスで実装）"""
        raise NotImplementedError
    
    def _create_chunk(
        self,
        text: str,
        start_index: int,
        end_index: int,
        chunk_id: Optional[str] = None,
        metadata: Optional[Dict[str, Any]] = None
    ) -> TextChunk:
        """TextChunkオブジェクトを作成"""
        return TextChunk(text, start_index, end_index, chunk_id, metadata)


class FixedSizeChunker(TextChunker):
    """固定サイズチャンカー"""
    
    def chunk(self, text: str, **kwargs) -> List[TextChunk]:
        """固定サイズでテキストを分割"""
        chunks: List[TextChunk] = []
        text_length = len(text)
        
        for i in range(0, text_length, self.chunk_size - self.overlap):
            start_index = i
            end_index = min(i + self.chunk_size, text_length)
            
            chunk_text = text[start_index:end_index]
            
            if chunk_text.strip():  # 空でないチャンクのみ追加
                chunk = self._create_chunk(
                    chunk_text,
                    start_index,
                    end_index,
                    chunk_id=f"chunk_{len(chunks)}",
                    metadata={"type": "fixed_size"}
                )
                chunks.append(chunk)
            
            # 最後のチャンクに到達した場合は終了
            if end_index >= text_length:
                break
        
        logger.info(f"固定サイズチャンキング完了: {len(chunks)}個のチャンク")
        return chunks


class SentenceChunker(TextChunker):
    """文ベースチャンカー"""
    
    def __init__(self, chunk_size: int = 1000, overlap: int = 200, max_sentences: int = 5):
        super().__init__(chunk_size, overlap)
        self.max_sentences = max_sentences
    
    def chunk(self, text: str, **kwargs) -> List[TextChunk]:
        """文単位でテキストを分割"""
        try:
            # nltk を使用して文分割
            import nltk
            try:
                sentences = nltk.sent_tokenize(text)
            except LookupError:
                # punkt データがない場合はダウンロード
                nltk.download('punkt', quiet=True)
                sentences = nltk.sent_tokenize(text)
        except ImportError:
            # nltk がない場合は簡易的な文分割
            sentences = self._simple_sentence_split(text)
        
        chunks: List[TextChunk] = []
        current_chunk = ""
        current_sentences: List[str] = []
        start_index = 0
        
        for sentence in sentences:
            # 現在のチャンクに文を追加した場合のサイズをチェック
            potential_chunk = current_chunk + " " + sentence if current_chunk else sentence
            
            if (len(potential_chunk) <= self.chunk_size and 
                len(current_sentences) < self.max_sentences):
                current_chunk = potential_chunk
                current_sentences.append(sentence)
            else:
                # 現在のチャンクを保存
                if current_chunk.strip():
                    end_index = start_index + len(current_chunk)
                    chunk = self._create_chunk(
                        current_chunk.strip(),
                        start_index,
                        end_index,
                        chunk_id=f"chunk_{len(chunks)}",
                        metadata={
                            "type": "sentence_based",
                            "sentence_count": len(current_sentences)
                        }
                    )
                    chunks.append(chunk)
                
                # オーバーラップを考慮して新しいチャンクを開始
                if self.overlap > 0 and current_sentences:
                    overlap_sentences = current_sentences[-1:]  # 最後の文をオーバーラップ
                    current_chunk = " ".join(overlap_sentences) + " " + sentence
                    current_sentences = overlap_sentences + [sentence]
                else:
                    current_chunk = sentence
                    current_sentences = [sentence]
                
                start_index = end_index - (len(" ".join(overlap_sentences)) if self.overlap > 0 and overlap_sentences else 0)
        
        # 最後のチャンクを追加
        if current_chunk.strip():
            end_index = start_index + len(current_chunk)
            chunk = self._create_chunk(
                current_chunk.strip(),
                start_index,
                end_index,
                chunk_id=f"chunk_{len(chunks)}",
                metadata={
                    "type": "sentence_based",
                    "sentence_count": len(current_sentences)
                }
            )
            chunks.append(chunk)
        
        logger.info(f"文ベースチャンキング完了: {len(chunks)}個のチャンク")
        return chunks
    
    def _simple_sentence_split(self, text: str) -> List[str]:
        """簡易的な文分割（nltk がない場合）"""
        # 句読点で分割
        sentences = re.split(r'[.!?。！？]+', text)
        return [s.strip() for s in sentences if s.strip()]


class ParagraphChunker(TextChunker):
    """段落ベースチャンカー"""
    
    def chunk(self, text: str, **kwargs) -> List[TextChunk]:
        """段落単位でテキストを分割"""
        # 段落を分割（連続する改行で区切る）
        paragraphs = re.split(r'\n\s*\n', text)
        
        chunks: List[TextChunk] = []
        current_chunk = ""
        start_index = 0
        
        for paragraph in paragraphs:
            paragraph = paragraph.strip()
            if not paragraph:
                continue
            
            # 現在のチャンクに段落を追加した場合のサイズをチェック
            potential_chunk = current_chunk + "\n\n" + paragraph if current_chunk else paragraph
            
            if len(potential_chunk) <= self.chunk_size:
                current_chunk = potential_chunk
            else:
                # 現在のチャンクを保存
                if current_chunk.strip():
                    end_index = start_index + len(current_chunk)
                    chunk = self._create_chunk(
                        current_chunk.strip(),
                        start_index,
                        end_index,
                        chunk_id=f"chunk_{len(chunks)}",
                        metadata={"type": "paragraph_based"}
                    )
                    chunks.append(chunk)
                
                # 新しいチャンクを開始
                current_chunk = paragraph
                start_index = end_index
        
        # 最後のチャンクを追加
        if current_chunk.strip():
            end_index = start_index + len(current_chunk)
            chunk = self._create_chunk(
                current_chunk.strip(),
                start_index,
                end_index,
                chunk_id=f"chunk_{len(chunks)}",
                metadata={"type": "paragraph_based"}
            )
            chunks.append(chunk)
        
        logger.info(f"段落ベースチャンキング完了: {len(chunks)}個のチャンク")
        return chunks


class SemanticChunker(TextChunker):
    """セマンティックチャンカー（埋め込みベース）"""
    
    def __init__(
        self,
        chunk_size: int = 1000,
        overlap: int = 200,
        similarity_threshold: float = 0.7,
        embedding_function: Optional[Callable[[str], List[float]]] = None
    ):
        super().__init__(chunk_size, overlap)
        self.similarity_threshold = similarity_threshold
        self.embedding_function = embedding_function
    
    def chunk(self, text: str, **kwargs) -> List[TextChunk]:
        """セマンティック類似性に基づいてテキストを分割"""
        if not self.embedding_function:
            logger.warning("埋め込み関数が設定されていません。文ベースチャンキングにフォールバック")
            sentence_chunker = SentenceChunker(self.chunk_size, self.overlap)
            return sentence_chunker.chunk(text)
        
        try:
            # まず文に分割
            import nltk
            try:
                sentences = nltk.sent_tokenize(text)
            except LookupError:
                nltk.download('punkt', quiet=True)
                sentences = nltk.sent_tokenize(text)
        except ImportError:
            sentences = re.split(r'[.!?。！？]+', text)
            sentences = [s.strip() for s in sentences if s.strip()]
        
        if len(sentences) <= 1:
            # 文が1つ以下の場合は固定サイズチャンキング
            fixed_chunker = FixedSizeChunker(self.chunk_size, self.overlap)
            return fixed_chunker.chunk(text)
        
        # 各文の埋め込みを計算
        embeddings = []
        for sentence in sentences:
            try:
                embedding = self.embedding_function(sentence)
                embeddings.append(embedding)
            except Exception as e:
                logger.warning(f"埋め込み計算エラー: {e}")
                # エラーの場合は文ベースチャンキングにフォールバック
                sentence_chunker = SentenceChunker(self.chunk_size, self.overlap)
                return sentence_chunker.chunk(text)
        
        # 類似性に基づいてチャンクを作成
        chunks: List[TextChunk] = []
        current_chunk_sentences = [sentences[0]]
        current_chunk_embeddings = [embeddings[0]]
        start_index = 0
        
        for i in range(1, len(sentences)):
            # 現在のチャンクの平均埋め込みと次の文の類似性を計算
            current_avg_embedding = self._average_embedding(current_chunk_embeddings)
            similarity = self._cosine_similarity(current_avg_embedding, embeddings[i])
            
            # 現在のチャンクに追加した場合のサイズをチェック
            potential_text = " ".join(current_chunk_sentences + [sentences[i]])
            
            if (similarity >= self.similarity_threshold and 
                len(potential_text) <= self.chunk_size):
                current_chunk_sentences.append(sentences[i])
                current_chunk_embeddings.append(embeddings[i])
            else:
                # 現在のチャンクを保存
                chunk_text = " ".join(current_chunk_sentences)
                end_index = start_index + len(chunk_text)
                
                chunk = self._create_chunk(
                    chunk_text,
                    start_index,
                    end_index,
                    chunk_id=f"chunk_{len(chunks)}",
                    metadata={
                        "type": "semantic",
                        "sentence_count": len(current_chunk_sentences),
                        "avg_similarity": similarity
                    }
                )
                chunks.append(chunk)
                
                # 新しいチャンクを開始
                current_chunk_sentences = [sentences[i]]
                current_chunk_embeddings = [embeddings[i]]
                start_index = end_index
        
        # 最後のチャンクを追加
        if current_chunk_sentences:
            chunk_text = " ".join(current_chunk_sentences)
            end_index = start_index + len(chunk_text)
            
            chunk = self._create_chunk(
                chunk_text,
                start_index,
                end_index,
                chunk_id=f"chunk_{len(chunks)}",
                metadata={
                    "type": "semantic",
                    "sentence_count": len(current_chunk_sentences)
                }
            )
            chunks.append(chunk)
        
        logger.info(f"セマンティックチャンキング完了: {len(chunks)}個のチャンク")
        return chunks
    
    def _average_embedding(self, embeddings: List[List[float]]) -> List[float]:
        """埋め込みの平均を計算"""
        import numpy as np
        return np.mean(embeddings, axis=0).tolist()  # type: ignore
    
    def _cosine_similarity(self, vec1: List[float], vec2: List[float]) -> float:
        """コサイン類似度を計算"""
        import numpy as np
        
        vec1 = np.array(vec1)
        vec2 = np.array(vec2)
        
        dot_product = np.dot(vec1, vec2)
        norm1 = np.linalg.norm(vec1)
        norm2 = np.linalg.norm(vec2)
        
        if norm1 == 0 or norm2 == 0:
            return 0.0
        
        return float(dot_product / (norm1 * norm2))


class TextPreprocessor:
    """テキスト前処理クラス"""
    
    @staticmethod
    def clean_text(text: str, **kwargs) -> str:
        """テキストをクリーニング"""
        # HTML タグを除去
        if kwargs.get("remove_html", True):
            text = re.sub(r'<[^>]+>', '', text)
        
        # 余分な空白を除去
        if kwargs.get("normalize_whitespace", True):
            text = re.sub(r'\s+', ' ', text)
        
        # 特殊文字を除去
        if kwargs.get("remove_special_chars", False):
            text = re.sub(r'[^\w\s\u3040-\u309F\u30A0-\u30FF\u4E00-\u9FAF]', '', text)
        
        # 改行を正規化
        if kwargs.get("normalize_newlines", True):
            text = re.sub(r'\r\n|\r', '\n', text)
        
        return text.strip()
    
    @staticmethod
    def extract_sentences(text: str) -> List[str]:
        """文を抽出"""
        try:
            import nltk
            try:
                sentences = nltk.sent_tokenize(text)
            except LookupError:
                nltk.download('punkt', quiet=True)
                sentences = nltk.sent_tokenize(text)
        except ImportError:
            sentences = re.split(r'[.!?。！？]+', text)
            sentences = [s.strip() for s in sentences if s.strip()]
        
        return sentences  # type: ignore
    
    @staticmethod
    def extract_paragraphs(text: str) -> List[str]:
        """段落を抽出"""
        paragraphs = re.split(r'\n\s*\n', text)
        return [p.strip() for p in paragraphs if p.strip()]


# チャンカー管理クラス
class ChunkerManager:
    """チャンカー管理クラス"""
    
    def __init__(self):
        self.chunkers = {
            "fixed": FixedSizeChunker,
            "sentence": SentenceChunker,
            "paragraph": ParagraphChunker,
            "semantic": SemanticChunker
        }
        self.default_chunker = "fixed"
    
    def get_chunker(
        self,
        chunker_type: Optional[str] = None,
        **kwargs
    ) -> TextChunker:
        """チャンカーを取得"""
        chunker_type = chunker_type or self.default_chunker
        
        if chunker_type not in self.chunkers:
            raise ValueError(f"未知のチャンカータイプ: {chunker_type}")
        
        return self.chunkers[chunker_type](**kwargs)  # type: ignore
    
    def chunk_text(
        self,
        text: str,
        chunker_type: Optional[str] = None,
        **kwargs
    ) -> List[TextChunk]:
        """テキストをチャンクに分割"""
        chunker = self.get_chunker(chunker_type, **kwargs)
        return chunker.chunk(text)


# グローバルマネージャー
_chunker_manager: Optional[ChunkerManager] = None


def get_chunker_manager() -> ChunkerManager:
    """グローバルチャンカーマネージャーを取得"""
    global _chunker_manager
    if _chunker_manager is None:
        _chunker_manager = ChunkerManager()
    return _chunker_manager


# 便利関数
def chunk_text(
    text: str,
    chunker_type: str = "fixed",
    chunk_size: int = 1000,
    overlap: int = 200,
    **kwargs
) -> List[TextChunk]:
    """テキストチャンキングの便利関数"""
    manager = get_chunker_manager()
    return manager.chunk_text(
        text,
        chunker_type=chunker_type,
        chunk_size=chunk_size,
        overlap=overlap,
        **kwargs
    )


def preprocess_text(text: str, **kwargs) -> str:
    """テキスト前処理の便利関数"""
    return TextPreprocessor.clean_text(text, **kwargs)


def merge_chunks(chunks: List[TextChunk], separator: str = "\n\n") -> str:
    """チャンクを結合"""
    return separator.join(chunk.text for chunk in chunks)
