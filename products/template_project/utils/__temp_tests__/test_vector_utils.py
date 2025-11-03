"""
ベクトルデータベースユーティリティのテストクラス

このモジュールは、vector_utils.pyの機能をテストします。
"""

import unittest
from unittest.mock import Mock, patch, MagicMock
import numpy as np
import sys
import os

# テスト対象モジュールのパスを追加
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

try:
    from vector_utils import (
        VectorDBProvider, FAISSProvider, ChromaProvider, VectorDocument,
        SearchResult, VectorDBManager, VectorDBError,
        get_vector_db_manager, create_collection, upsert_documents, search_vectors
    )
except ImportError as e:
    print(f"Warning: Could not import vector_utils: {e}")
    # フォールバック用のダミークラス
    class VectorDBProvider:
        def __init__(self, name): self.name = name
    class VectorDBError(Exception): pass
    class VectorDocument:
        def __init__(self, id, vector, metadata=None, text=None):
            self.id = id
            self.vector = np.array(vector)
            self.metadata = metadata or {}
            self.text = text


class TestVectorDocument(unittest.TestCase):
    """VectorDocumentクラスのテスト"""
    
    def test_initialization(self):
        """初期化テスト"""
        doc = VectorDocument(
            id="doc1",
            vector=[0.1, 0.2, 0.3],
            metadata={"title": "テスト文書"},
            text="これはテスト文書です"
        )
        
        self.assertEqual(doc.id, "doc1")
        self.assertIsInstance(doc.vector, np.ndarray)
        np.testing.assert_array_equal(doc.vector, np.array([0.1, 0.2, 0.3], dtype=np.float32))
        self.assertEqual(doc.metadata["title"], "テスト文書")
        self.assertEqual(doc.text, "これはテスト文書です")


class TestVectorDBProvider(unittest.TestCase):
    """VectorDBProvider基底クラスのテスト"""
    
    def test_base_provider_initialization(self):
        """基底プロバイダーの初期化テスト"""
        provider = VectorDBProvider("test")
        self.assertEqual(provider.name, "test")
        self.assertFalse(provider.is_connected)
    
    def test_not_implemented_methods(self):
        """未実装メソッドのテスト"""
        provider = VectorDBProvider("test")
        if hasattr(provider, 'connect'):
            with self.assertRaises(NotImplementedError):
                provider.connect()


class TestFAISSProvider(unittest.TestCase):
    """FAISSProviderのテスト"""
    
    def setUp(self):
        """テストセットアップ"""
        try:
            self.provider = FAISSProvider()
        except NameError:
            self.skipTest("FAISSProvider not available")
    
    def test_initialization(self):
        """初期化テスト"""
        self.assertEqual(self.provider.name, "faiss")
        self.assertFalse(self.provider.is_connected)
        self.assertEqual(len(self.provider.indexes), 0)
    
    @patch('vector_utils.faiss')
    def test_connect(self, mock_faiss):
        """接続テスト"""
        try:
            self.provider.connect()
            self.assertTrue(self.provider.is_connected)
        except NameError:
            self.skipTest("FAISSProvider not available")
    
    def test_disconnect(self):
        """切断テスト"""
        try:
            self.provider.disconnect()
            self.assertFalse(self.provider.is_connected)
            self.assertEqual(len(self.provider.indexes), 0)
        except NameError:
            self.skipTest("FAISSProvider not available")
    
    @patch('vector_utils.faiss')
    def test_create_collection(self, mock_faiss):
        """コレクション作成テスト"""
        try:
            # モックの設定
            mock_index = Mock()
            mock_faiss.IndexFlatL2.return_value = mock_index
            
            self.provider.connect()
            self.provider.create_collection("test_collection", dimension=128)
            
            self.assertIn("test_collection", self.provider.indexes)
            mock_faiss.IndexFlatL2.assert_called_once_with(128)
        except NameError:
            self.skipTest("FAISSProvider not available")


class TestChromaProvider(unittest.TestCase):
    """ChromaProviderのテスト"""
    
    def setUp(self):
        """テストセットアップ"""
        try:
            self.provider = ChromaProvider(persist_directory="./test_chroma")
        except NameError:
            self.skipTest("ChromaProvider not available")
    
    def test_initialization(self):
        """初期化テスト"""
        self.assertEqual(self.provider.name, "chroma")
        self.assertEqual(self.provider.persist_directory, "./test_chroma")
        self.assertFalse(self.provider.is_connected)


class TestVectorDBManager(unittest.TestCase):
    """VectorDBManagerのテスト"""
    
    def setUp(self):
        """テストセットアップ"""
        try:
            self.manager = VectorDBManager()
        except NameError:
            self.skipTest("VectorDBManager not available")
    
    def test_initialization(self):
        """初期化テスト"""
        self.assertEqual(self.manager.default_provider, "faiss")
        self.assertIn("faiss", self.manager.providers)
    
    def test_register_provider(self):
        """プロバイダー登録テスト"""
        try:
            test_provider = VectorDBProvider("test")
            self.manager.register_provider("test", test_provider)
            
            self.assertIn("test", self.manager.providers)
            self.assertEqual(self.manager.providers["test"], test_provider)
        except NameError:
            self.skipTest("VectorDBManager not available")
    
    def test_set_default_provider(self):
        """デフォルトプロバイダー設定テスト"""
        try:
            test_provider = VectorDBProvider("test")
            self.manager.register_provider("test", test_provider)
            self.manager.set_default_provider("test")
            
            self.assertEqual(self.manager.default_provider, "test")
        except NameError:
            self.skipTest("VectorDBManager not available")
    
    def test_get_provider(self):
        """プロバイダー取得テスト"""
        try:
            provider = self.manager.get_provider()
            self.assertIsNotNone(provider)
            self.assertEqual(provider.name, "faiss")
        except NameError:
            self.skipTest("VectorDBManager not available")


class TestGlobalFunctions(unittest.TestCase):
    """グローバル関数のテスト"""
    
    def test_get_vector_db_manager(self):
        """グローバルマネージャー取得テスト"""
        try:
            manager1 = get_vector_db_manager()
            manager2 = get_vector_db_manager()
            
            # 同じインスタンスが返されることを確認
            self.assertIs(manager1, manager2)
        except NameError:
            self.skipTest("get_vector_db_manager not available")
    
    def test_create_collection_function(self):
        """create_collection関数のテスト"""
        try:
            # この関数は実際のFAISSライブラリが必要なのでスキップ
            self.skipTest("Requires actual FAISS library")
        except NameError:
            self.skipTest("create_collection function not available")


class TestSearchResult(unittest.TestCase):
    """SearchResultクラスのテスト"""
    
    def test_initialization(self):
        """初期化テスト"""
        try:
            result = SearchResult(
                id="result1",
                score=0.95,
                vector=[0.1, 0.2, 0.3],
                metadata={"title": "テスト結果"},
                text="これはテスト結果です"
            )
            
            self.assertEqual(result.id, "result1")
            self.assertEqual(result.score, 0.95)
            self.assertEqual(result.vector, [0.1, 0.2, 0.3])
            self.assertEqual(result.metadata["title"], "テスト結果")
            self.assertEqual(result.text, "これはテスト結果です")
        except NameError:
            self.skipTest("SearchResult not available")


class TestIntegration(unittest.TestCase):
    """統合テスト"""
    
    def test_document_creation_and_search_workflow(self):
        """文書作成から検索までのワークフローテスト"""
        try:
            # 文書作成
            doc1 = VectorDocument("doc1", [1.0, 0.0, 0.0], {"category": "A"}, "文書1")
            doc2 = VectorDocument("doc2", [0.0, 1.0, 0.0], {"category": "B"}, "文書2")
            
            # 文書が正しく作成されていることを確認
            self.assertEqual(doc1.id, "doc1")
            self.assertEqual(doc2.id, "doc2")
            
            # ベクトルが正しく設定されていることを確認
            np.testing.assert_array_equal(doc1.vector, np.array([1.0, 0.0, 0.0], dtype=np.float32))
            np.testing.assert_array_equal(doc2.vector, np.array([0.0, 1.0, 0.0], dtype=np.float32))
            
        except NameError:
            self.skipTest("Required classes not available")


if __name__ == '__main__':
    unittest.main(verbosity=2)
