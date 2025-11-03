"""
埋め込みユーティリティのテストクラス

このモジュールは、embedding_utils.pyの機能をテストします。
"""

import unittest
from unittest.mock import Mock, patch, MagicMock
import numpy as np
import sys
import os

# テスト対象モジュールのパスを追加
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from embedding_utils import (
    EmbeddingProvider, OpenAIEmbeddingProvider, AzureOpenAIEmbeddingProvider,
    GoogleEmbeddingProvider, CohereEmbeddingProvider, EmbeddingManager,
    EmbeddingError, get_embedding_manager, embed
)


class TestEmbeddingProvider(unittest.TestCase):
    """EmbeddingProvider基底クラスのテスト"""
    
    def test_base_provider_initialization(self):
        """基底プロバイダーの初期化テスト"""
        provider = EmbeddingProvider("test")
        self.assertEqual(provider.name, "test")
        self.assertEqual(provider.call_count, 0)
        self.assertEqual(provider.error_count, 0)
    
    def test_record_call_success(self):
        """成功時の呼び出し記録テスト"""
        provider = EmbeddingProvider("test")
        provider._record_call(True)
        self.assertEqual(provider.call_count, 1)
        self.assertEqual(provider.error_count, 0)
    
    def test_record_call_failure(self):
        """失敗時の呼び出し記録テスト"""
        provider = EmbeddingProvider("test")
        provider._record_call(False)
        self.assertEqual(provider.call_count, 1)
        self.assertEqual(provider.error_count, 1)
    
    def test_not_implemented_method(self):
        """未実装メソッドのテスト"""
        provider = EmbeddingProvider("test")
        with self.assertRaises(NotImplementedError):
            provider.embed("test text")


class TestOpenAIEmbeddingProvider(unittest.TestCase):
    """OpenAIEmbeddingProviderのテスト"""
    
    def setUp(self):
        """テストセットアップ"""
        self.provider = OpenAIEmbeddingProvider(
            api_key="test_key",
            model="text-embedding-ada-002"
        )
    
    def test_initialization(self):
        """初期化テスト"""
        self.assertEqual(self.provider.name, "openai")
        self.assertEqual(self.provider.api_key, "test_key")
        self.assertEqual(self.provider.model, "text-embedding-ada-002")
    
    def test_no_api_key_error(self):
        """APIキー未設定エラーテスト"""
        provider = OpenAIEmbeddingProvider(api_key=None)
        
        with self.assertRaises(EmbeddingError) as context:
            provider.embed("test text")
        
        self.assertIn("OPENAI_API_KEY が設定されていません", str(context.exception))
    
    @patch('embedding_utils.OpenAI')
    def test_single_text_embedding(self, mock_openai_class):
        """単一テキストの埋め込みテスト"""
        # モックの設定
        mock_client = Mock()
        mock_response = Mock()
        mock_data = Mock()
        mock_data.embedding = [0.1, 0.2, 0.3, 0.4, 0.5]
        mock_response.data = [mock_data]
        mock_client.embeddings.create.return_value = mock_response
        mock_openai_class.return_value = mock_client
        
        result = self.provider.embed("test text")
        
        # 結果の検証
        self.assertIsInstance(result, np.ndarray)
        self.assertEqual(len(result), 5)
        np.testing.assert_array_equal(result, np.array([0.1, 0.2, 0.3, 0.4, 0.5], dtype=np.float32))
        self.assertEqual(self.provider.call_count, 1)
        self.assertEqual(self.provider.error_count, 0)
    
    @patch('embedding_utils.OpenAI')
    def test_multiple_text_embedding(self, mock_openai_class):
        """複数テキストの埋め込みテスト"""
        # モックの設定
        mock_client = Mock()
        mock_response = Mock()
        mock_data1 = Mock()
        mock_data1.embedding = [0.1, 0.2, 0.3]
        mock_data2 = Mock()
        mock_data2.embedding = [0.4, 0.5, 0.6]
        mock_response.data = [mock_data1, mock_data2]
        mock_client.embeddings.create.return_value = mock_response
        mock_openai_class.return_value = mock_client
        
        texts = ["text1", "text2"]
        result = self.provider.embed(texts)
        
        # 結果の検証
        self.assertIsInstance(result, list)
        self.assertEqual(len(result), 2)
        self.assertIsInstance(result[0], np.ndarray)
        self.assertIsInstance(result[1], np.ndarray)
        np.testing.assert_array_equal(result[0], np.array([0.1, 0.2, 0.3], dtype=np.float32))
        np.testing.assert_array_equal(result[1], np.array([0.4, 0.5, 0.6], dtype=np.float32))
    
    @patch('embedding_utils.OpenAI')
    def test_api_error_handling(self, mock_openai_class):
        """APIエラーハンドリングテスト"""
        mock_openai_class.side_effect = Exception("API エラー")
        
        with self.assertRaises(EmbeddingError) as context:
            self.provider.embed("test text")
        
        self.assertIn("OpenAI埋め込みに失敗しました", str(context.exception))
        self.assertEqual(self.provider.call_count, 1)
        self.assertEqual(self.provider.error_count, 1)


class TestAzureOpenAIEmbeddingProvider(unittest.TestCase):
    """AzureOpenAIEmbeddingProviderのテスト"""
    
    def setUp(self):
        """テストセットアップ"""
        self.provider = AzureOpenAIEmbeddingProvider(
            api_key="test_azure_key",
            endpoint="https://test.openai.azure.com/",
            deployment_name="test-embedding"
        )
    
    def test_initialization(self):
        """初期化テスト"""
        self.assertEqual(self.provider.name, "azure_openai")
        self.assertEqual(self.provider.api_key, "test_azure_key")
        self.assertEqual(self.provider.endpoint, "https://test.openai.azure.com/")
        self.assertEqual(self.provider.deployment_name, "test-embedding")
    
    def test_incomplete_config_error(self):
        """設定不完全エラーテスト"""
        provider = AzureOpenAIEmbeddingProvider(api_key=None, endpoint=None)
        
        with self.assertRaises(EmbeddingError) as context:
            provider.embed("test text")
        
        self.assertIn("Azure OpenAI の設定が不完全です", str(context.exception))


class TestCohereEmbeddingProvider(unittest.TestCase):
    """CohereEmbeddingProviderのテスト"""
    
    def setUp(self):
        """テストセットアップ"""
        self.provider = CohereEmbeddingProvider(
            api_key="test_cohere_key",
            model="embed-english-v2.0"
        )
    
    def test_initialization(self):
        """初期化テスト"""
        self.assertEqual(self.provider.name, "cohere")
        self.assertEqual(self.provider.api_key, "test_cohere_key")
        self.assertEqual(self.provider.model, "embed-english-v2.0")
    
    def test_no_api_key_error(self):
        """APIキー未設定エラーテスト"""
        provider = CohereEmbeddingProvider(api_key=None)
        
        with self.assertRaises(EmbeddingError) as context:
            provider.embed("test text")
        
        self.assertIn("COHERE_API_KEY が設定されていません", str(context.exception))


class TestEmbeddingManager(unittest.TestCase):
    """EmbeddingManagerのテスト"""
    
    def setUp(self):
        """テストセットアップ"""
        self.manager = EmbeddingManager()
    
    def test_initialization(self):
        """初期化テスト"""
        self.assertEqual(self.manager.default_provider, "openai")
        self.assertIn("openai", self.manager.providers)
        self.assertIsInstance(self.manager.providers["openai"], OpenAIEmbeddingProvider)
    
    def test_register_provider(self):
        """プロバイダー登録テスト"""
        test_provider = EmbeddingProvider("test")
        self.manager.register_provider("test", test_provider)
        
        self.assertIn("test", self.manager.providers)
        self.assertEqual(self.manager.providers["test"], test_provider)
    
    def test_set_default_provider(self):
        """デフォルトプロバイダー設定テスト"""
        test_provider = EmbeddingProvider("test")
        self.manager.register_provider("test", test_provider)
        self.manager.set_default_provider("test")
        
        self.assertEqual(self.manager.default_provider, "test")
    
    def test_set_invalid_default_provider(self):
        """無効なデフォルトプロバイダー設定テスト"""
        with self.assertRaises(ValueError) as context:
            self.manager.set_default_provider("nonexistent")
        
        self.assertIn("プロバイダー 'nonexistent' が登録されていません", str(context.exception))
    
    @patch('embedding_utils.OpenAI')
    def test_embed_with_default_provider(self, mock_openai_class):
        """デフォルトプロバイダーでの埋め込みテスト"""
        # モックの設定
        mock_client = Mock()
        mock_response = Mock()
        mock_data = Mock()
        mock_data.embedding = [0.1, 0.2, 0.3]
        mock_response.data = [mock_data]
        mock_client.embeddings.create.return_value = mock_response
        mock_openai_class.return_value = mock_client
        
        # テスト用のプロバイダーを設定
        test_provider = OpenAIEmbeddingProvider(api_key="test_key")
        self.manager.providers["openai"] = test_provider
        
        result = self.manager.embed("test text")
        
        self.assertIsInstance(result, np.ndarray)
        self.assertEqual(len(result), 3)
    
    def test_embed_with_invalid_provider(self):
        """無効なプロバイダーでの埋め込みテスト"""
        with self.assertRaises(ValueError) as context:
            self.manager.embed("test text", provider="nonexistent")
        
        self.assertIn("プロバイダー 'nonexistent' が登録されていません", str(context.exception))
    
    def test_get_stats(self):
        """統計情報取得テスト"""
        # テスト用のプロバイダーを作成
        test_provider = EmbeddingProvider("test")
        test_provider._record_call(True)
        test_provider._record_call(False)
        self.manager.register_provider("test", test_provider)
        
        stats = self.manager.get_stats()
        self.assertIn("test", stats)
        self.assertEqual(stats["test"]["call_count"], 2)
        self.assertEqual(stats["test"]["error_count"], 1)
        self.assertEqual(stats["test"]["success_rate"], 0.5)


class TestGlobalFunctions(unittest.TestCase):
    """グローバル関数のテスト"""
    
    def test_get_embedding_manager(self):
        """グローバルマネージャー取得テスト"""
        manager1 = get_embedding_manager()
        manager2 = get_embedding_manager()
        
        # 同じインスタンスが返されることを確認
        self.assertIs(manager1, manager2)
        self.assertIsInstance(manager1, EmbeddingManager)
    
    @patch('embedding_utils.OpenAI')
    def test_embed_function(self, mock_openai_class):
        """embed関数のテスト"""
        # モックの設定
        mock_client = Mock()
        mock_response = Mock()
        mock_data = Mock()
        mock_data.embedding = [0.1, 0.2, 0.3]
        mock_response.data = [mock_data]
        mock_client.embeddings.create.return_value = mock_response
        mock_openai_class.return_value = mock_client
        
        # グローバルマネージャーのプロバイダーを設定
        manager = get_embedding_manager()
        test_provider = OpenAIEmbeddingProvider(api_key="test_key")
        manager.providers["openai"] = test_provider
        
        result = embed("test text")
        
        self.assertIsInstance(result, np.ndarray)
        self.assertEqual(len(result), 3)


class TestSetupEmbeddingProviders(unittest.TestCase):
    """setup_embedding_providers関数のテスト"""
    
    def test_setup_openai_provider(self):
        """OpenAIプロバイダー設定テスト"""
        from embedding_utils import setup_embedding_providers
        
        config = {
            "openai_custom": {
                "type": "openai",
                "api_key": "custom_key",
                "model": "text-embedding-ada-002",
                "default": True
            }
        }
        
        setup_embedding_providers(config)
        
        manager = get_embedding_manager()
        self.assertIn("openai_custom", manager.providers)
        self.assertEqual(manager.default_provider, "openai_custom")
        self.assertIsInstance(manager.providers["openai_custom"], OpenAIEmbeddingProvider)
    
    def test_setup_azure_provider(self):
        """Azureプロバイダー設定テスト"""
        from embedding_utils import setup_embedding_providers
        
        config = {
            "azure_custom": {
                "type": "azure_openai",
                "api_key": "azure_key",
                "endpoint": "https://test.openai.azure.com/",
                "deployment_name": "test-embedding"
            }
        }
        
        setup_embedding_providers(config)
        
        manager = get_embedding_manager()
        self.assertIn("azure_custom", manager.providers)
        self.assertIsInstance(manager.providers["azure_custom"], AzureOpenAIEmbeddingProvider)
    
    def test_setup_unknown_provider_type(self):
        """未知のプロバイダータイプ設定テスト"""
        from embedding_utils import setup_embedding_providers
        
        config = {
            "unknown": {
                "type": "unknown_type",
                "api_key": "test_key"
            }
        }
        
        # 警告が出るが例外は発生しない
        setup_embedding_providers(config)
        
        manager = get_embedding_manager()
        self.assertNotIn("unknown", manager.providers)


if __name__ == '__main__':
    # テストの実行
    unittest.main(verbosity=2)
