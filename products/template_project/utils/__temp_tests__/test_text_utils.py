"""
テキスト処理ユーティリティのテストクラス

このモジュールは、text_utils.pyの機能をテストします。
"""

import unittest
from unittest.mock import Mock, patch
import sys
import os

# テスト対象モジュールのパスを追加
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from text_utils import (
    TextChunk, TextChunker, FixedSizeChunker, SentenceChunker, 
    ParagraphChunker, SemanticChunker, TextPreprocessor,
    ChunkerManager, get_chunker_manager, chunk_text, preprocess_text
)


class TestTextChunk(unittest.TestCase):
    """TextChunkクラスのテスト"""
    
    def test_initialization(self):
        """初期化テスト"""
        chunk = TextChunk(
            text="テストテキスト",
            start_index=0,
            end_index=7,
            chunk_id="chunk_1",
            metadata={"type": "test"}
        )
        
        self.assertEqual(chunk.text, "テストテキスト")
        self.assertEqual(chunk.start_index, 0)
        self.assertEqual(chunk.end_index, 7)
        self.assertEqual(chunk.chunk_id, "chunk_1")
        self.assertEqual(chunk.metadata["type"], "test")
    
    def test_length(self):
        """長さ取得テスト"""
        chunk = TextChunk("テストテキスト", 0, 7)
        self.assertEqual(len(chunk), 7)
    
    def test_string_representation(self):
        """文字列表現テスト"""
        chunk = TextChunk("テストテキスト", 0, 7)
        self.assertEqual(str(chunk), "テストテキスト")
    
    def test_to_dict(self):
        """辞書変換テスト"""
        chunk = TextChunk(
            text="テストテキスト",
            start_index=0,
            end_index=7,
            chunk_id="chunk_1",
            metadata={"type": "test"}
        )
        
        result = chunk.to_dict()
        expected = {
            "text": "テストテキスト",
            "start_index": 0,
            "end_index": 7,
            "chunk_id": "chunk_1",
            "metadata": {"type": "test"},
            "length": 7
        }
        
        self.assertEqual(result, expected)


class TestFixedSizeChunker(unittest.TestCase):
    """FixedSizeChunkerのテスト"""
    
    def setUp(self):
        """テストセットアップ"""
        self.chunker = FixedSizeChunker(chunk_size=10, overlap=2)
    
    def test_initialization(self):
        """初期化テスト"""
        self.assertEqual(self.chunker.chunk_size, 10)
        self.assertEqual(self.chunker.overlap, 2)
    
    def test_simple_chunking(self):
        """シンプルなチャンキングテスト"""
        text = "これはテストテキストです。"
        chunks = self.chunker.chunk(text)
        
        self.assertGreater(len(chunks), 0)
        self.assertIsInstance(chunks[0], TextChunk)
        self.assertEqual(chunks[0].metadata["type"], "fixed_size")
    
    def test_short_text(self):
        """短いテキストのテスト"""
        text = "短い"
        chunks = self.chunker.chunk(text)
        
        self.assertEqual(len(chunks), 1)
        self.assertEqual(chunks[0].text, "短い")
        self.assertEqual(chunks[0].start_index, 0)
        self.assertEqual(chunks[0].end_index, 2)
    
    def test_long_text_with_overlap(self):
        """オーバーラップありの長いテキストテスト"""
        text = "a" * 25  # 25文字のテキスト
        chunks = self.chunker.chunk(text)
        
        # chunk_size=10, overlap=2 なので、複数のチャンクに分割される
        self.assertGreater(len(chunks), 1)
        
        # 各チャンクの長さをチェック
        for chunk in chunks[:-1]:  # 最後以外のチャンク
            self.assertEqual(len(chunk.text), 10)
    
    def test_empty_text(self):
        """空のテキストテスト"""
        text = ""
        chunks = self.chunker.chunk(text)
        
        self.assertEqual(len(chunks), 0)


class TestSentenceChunker(unittest.TestCase):
    """SentenceChunkerのテスト"""
    
    def setUp(self):
        """テストセットアップ"""
        self.chunker = SentenceChunker(chunk_size=50, overlap=10, max_sentences=3)
    
    def test_initialization(self):
        """初期化テスト"""
        self.assertEqual(self.chunker.chunk_size, 50)
        self.assertEqual(self.chunker.overlap, 10)
        self.assertEqual(self.chunker.max_sentences, 3)
    
    @patch('text_utils.nltk')
    def test_nltk_sentence_chunking(self, mock_nltk):
        """NLTK使用時の文チャンキングテスト"""
        # NLTKのモック設定
        mock_nltk.sent_tokenize.return_value = [
            "これは最初の文です。",
            "これは二番目の文です。",
            "これは三番目の文です。"
        ]
        
        text = "これは最初の文です。これは二番目の文です。これは三番目の文です。"
        chunks = self.chunker.chunk(text)
        
        self.assertGreater(len(chunks), 0)
        self.assertEqual(chunks[0].metadata["type"], "sentence_based")
        mock_nltk.sent_tokenize.assert_called_once_with(text)
    
    @patch('text_utils.nltk')
    def test_nltk_not_available(self, mock_nltk):
        """NLTK利用不可時のフォールバックテスト"""
        # NLTKが利用不可の場合をシミュレート
        mock_nltk.sent_tokenize.side_effect = ImportError("NLTK not available")
        
        text = "これは最初の文です。これは二番目の文です。"
        chunks = self.chunker.chunk(text)
        
        # 簡易的な文分割が使用される
        self.assertGreater(len(chunks), 0)
    
    def test_simple_sentence_split(self):
        """簡易文分割のテスト"""
        sentences = self.chunker._simple_sentence_split("文1。文2！文3？")
        expected = ["文1", "文2", "文3"]
        self.assertEqual(sentences, expected)


class TestParagraphChunker(unittest.TestCase):
    """ParagraphChunkerのテスト"""
    
    def setUp(self):
        """テストセットアップ"""
        self.chunker = ParagraphChunker(chunk_size=100, overlap=20)
    
    def test_paragraph_chunking(self):
        """段落チャンキングテスト"""
        text = "段落1の内容です。\n\n段落2の内容です。\n\n段落3の内容です。"
        chunks = self.chunker.chunk(text)
        
        self.assertGreater(len(chunks), 0)
        self.assertEqual(chunks[0].metadata["type"], "paragraph_based")
    
    def test_single_paragraph(self):
        """単一段落のテスト"""
        text = "これは単一の段落です。"
        chunks = self.chunker.chunk(text)
        
        self.assertEqual(len(chunks), 1)
        self.assertEqual(chunks[0].text, "これは単一の段落です。")
    
    def test_empty_paragraphs(self):
        """空の段落を含むテスト"""
        text = "段落1\n\n\n\n段落2"
        chunks = self.chunker.chunk(text)
        
        # 空の段落は無視される
        self.assertEqual(len(chunks), 1)


class TestSemanticChunker(unittest.TestCase):
    """SemanticChunkerのテスト"""
    
    def setUp(self):
        """テストセットアップ"""
        # モック埋め込み関数
        def mock_embedding_function(text):
            # テキストの長さに基づいて簡単な埋め込みを生成
            return [len(text) / 10.0, 0.5, 0.3]
        
        self.chunker = SemanticChunker(
            chunk_size=100,
            overlap=20,
            similarity_threshold=0.7,
            embedding_function=mock_embedding_function
        )
    
    def test_initialization(self):
        """初期化テスト"""
        self.assertEqual(self.chunker.chunk_size, 100)
        self.assertEqual(self.chunker.overlap, 20)
        self.assertEqual(self.chunker.similarity_threshold, 0.7)
        self.assertIsNotNone(self.chunker.embedding_function)
    
    def test_no_embedding_function_fallback(self):
        """埋め込み関数なしのフォールバックテスト"""
        chunker = SemanticChunker(chunk_size=50)
        text = "これはテストテキストです。"
        
        # 文ベースチャンキングにフォールバック
        chunks = chunker.chunk(text)
        self.assertGreater(len(chunks), 0)
    
    @patch('text_utils.nltk')
    def test_semantic_chunking(self, mock_nltk):
        """セマンティックチャンキングテスト"""
        mock_nltk.sent_tokenize.return_value = [
            "短い文。",
            "これは少し長い文です。",
            "これはさらに長い文章になります。"
        ]
        
        text = "短い文。これは少し長い文です。これはさらに長い文章になります。"
        chunks = self.chunker.chunk(text)
        
        self.assertGreater(len(chunks), 0)
        self.assertEqual(chunks[0].metadata["type"], "semantic")
    
    def test_average_embedding(self):
        """平均埋め込み計算テスト"""
        embeddings = [[1.0, 2.0, 3.0], [2.0, 3.0, 4.0]]
        result = self.chunker._average_embedding(embeddings)
        expected = [1.5, 2.5, 3.5]
        self.assertEqual(result, expected)
    
    def test_cosine_similarity(self):
        """コサイン類似度計算テスト"""
        vec1 = [1.0, 0.0, 0.0]
        vec2 = [1.0, 0.0, 0.0]
        similarity = self.chunker._cosine_similarity(vec1, vec2)
        self.assertAlmostEqual(similarity, 1.0, places=5)
        
        vec3 = [1.0, 0.0, 0.0]
        vec4 = [0.0, 1.0, 0.0]
        similarity = self.chunker._cosine_similarity(vec3, vec4)
        self.assertAlmostEqual(similarity, 0.0, places=5)


class TestTextPreprocessor(unittest.TestCase):
    """TextPreprocessorのテスト"""
    
    def test_clean_text_default(self):
        """デフォルト設定でのテキストクリーニングテスト"""
        text = "<p>これは   テスト\n\n\nテキストです。</p>"
        result = TextPreprocessor.clean_text(text)
        expected = "これは テスト テキストです。"
        self.assertEqual(result, expected)
    
    def test_clean_text_no_html_removal(self):
        """HTML除去なしのテスト"""
        text = "<p>テスト</p>"
        result = TextPreprocessor.clean_text(text, remove_html=False)
        self.assertIn("<p>", result)
        self.assertIn("</p>", result)
    
    def test_clean_text_remove_special_chars(self):
        """特殊文字除去テスト"""
        text = "テスト@#$%テキスト"
        result = TextPreprocessor.clean_text(text, remove_special_chars=True)
        self.assertEqual(result, "テストテキスト")
    
    @patch('text_utils.nltk')
    def test_extract_sentences(self, mock_nltk):
        """文抽出テスト"""
        mock_nltk.sent_tokenize.return_value = ["文1。", "文2。"]
        
        text = "文1。文2。"
        sentences = TextPreprocessor.extract_sentences(text)
        
        self.assertEqual(sentences, ["文1。", "文2。"])
        mock_nltk.sent_tokenize.assert_called_once_with(text)
    
    def test_extract_paragraphs(self):
        """段落抽出テスト"""
        text = "段落1\n\n段落2\n\n段落3"
        paragraphs = TextPreprocessor.extract_paragraphs(text)
        expected = ["段落1", "段落2", "段落3"]
        self.assertEqual(paragraphs, expected)


class TestChunkerManager(unittest.TestCase):
    """ChunkerManagerのテスト"""
    
    def setUp(self):
        """テストセットアップ"""
        self.manager = ChunkerManager()
    
    def test_initialization(self):
        """初期化テスト"""
        self.assertEqual(self.manager.default_chunker, "fixed")
        self.assertIn("fixed", self.manager.chunkers)
        self.assertIn("sentence", self.manager.chunkers)
        self.assertIn("paragraph", self.manager.chunkers)
        self.assertIn("semantic", self.manager.chunkers)
    
    def test_get_chunker(self):
        """チャンカー取得テスト"""
        chunker = self.manager.get_chunker("fixed", chunk_size=100)
        self.assertIsInstance(chunker, FixedSizeChunker)
        self.assertEqual(chunker.chunk_size, 100)
    
    def test_get_invalid_chunker(self):
        """無効なチャンカー取得テスト"""
        with self.assertRaises(ValueError) as context:
            self.manager.get_chunker("invalid_type")
        
        self.assertIn("未知のチャンカータイプ", str(context.exception))
    
    def test_chunk_text(self):
        """テキストチャンキングテスト"""
        text = "これはテストテキストです。"
        chunks = self.manager.chunk_text(text, chunker_type="fixed", chunk_size=10)
        
        self.assertGreater(len(chunks), 0)
        self.assertIsInstance(chunks[0], TextChunk)


class TestGlobalFunctions(unittest.TestCase):
    """グローバル関数のテスト"""
    
    def test_get_chunker_manager(self):
        """グローバルマネージャー取得テスト"""
        manager1 = get_chunker_manager()
        manager2 = get_chunker_manager()
        
        # 同じインスタンスが返されることを確認
        self.assertIs(manager1, manager2)
        self.assertIsInstance(manager1, ChunkerManager)
    
    def test_chunk_text_function(self):
        """chunk_text関数のテスト"""
        text = "これはテストテキストです。"
        chunks = chunk_text(text, chunker_type="fixed", chunk_size=10)
        
        self.assertGreater(len(chunks), 0)
        self.assertIsInstance(chunks[0], TextChunk)
    
    def test_preprocess_text_function(self):
        """preprocess_text関数のテスト"""
        text = "<p>テスト   テキスト</p>"
        result = preprocess_text(text)
        expected = "テスト テキスト"
        self.assertEqual(result, expected)


class TestUtilityFunctions(unittest.TestCase):
    """ユーティリティ関数のテスト"""
    
    def test_merge_chunks(self):
        """チャンク結合テスト"""
        from text_utils import merge_chunks
        
        chunks = [
            TextChunk("チャンク1", 0, 4),
            TextChunk("チャンク2", 4, 8),
            TextChunk("チャンク3", 8, 12)
        ]
        
        result = merge_chunks(chunks, separator=" | ")
        expected = "チャンク1 | チャンク2 | チャンク3"
        self.assertEqual(result, expected)
    
    def test_merge_chunks_default_separator(self):
        """デフォルト区切り文字でのチャンク結合テスト"""
        from text_utils import merge_chunks
        
        chunks = [
            TextChunk("チャンク1", 0, 4),
            TextChunk("チャンク2", 4, 8)
        ]
        
        result = merge_chunks(chunks)
        expected = "チャンク1\n\nチャンク2"
        self.assertEqual(result, expected)


if __name__ == '__main__':
    # テストの実行
    unittest.main(verbosity=2)
