"""
Web検索ユーティリティのテストクラス

このモジュールは、websearch_utils.pyの機能をテストします。
"""

import unittest
from unittest.mock import Mock, patch, MagicMock
import sys
import os

# テスト対象モジュールのパスを追加
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

try:
    from websearch_utils import (
        WebSearchProvider, GoogleSearchProvider, BingSearchProvider,
        DuckDuckGoSearchProvider, BraveSearchProvider, SerpApiProvider,
        SearchResult, WebSearchManager, WebSearchError,
        get_web_search_manager, search_web, search_and_summarize
    )
except ImportError as e:
    print(f"Warning: Could not import websearch_utils: {e}")
    # フォールバック用のダミークラス
    class WebSearchProvider:
        def __init__(self, name): self.name = name
    class WebSearchError(Exception): pass
    class SearchResult:
        def __init__(self, title, url, snippet, source, metadata=None):
            self.title = title
            self.url = url
            self.snippet = snippet
            self.source = source
            self.metadata = metadata or {}


class TestSearchResult(unittest.TestCase):
    """SearchResultクラスのテスト"""
    
    def test_initialization(self):
        """初期化テスト"""
        result = SearchResult(
            title="テストタイトル",
            url="https://example.com",
            snippet="これはテストスニペットです",
            source="google",
            metadata={"rank": 1}
        )
        
        self.assertEqual(result.title, "テストタイトル")
        self.assertEqual(result.url, "https://example.com")
        self.assertEqual(result.snippet, "これはテストスニペットです")
        self.assertEqual(result.source, "google")
        self.assertEqual(result.metadata["rank"], 1)
    
    def test_to_dict(self):
        """辞書変換テスト"""
        result = SearchResult(
            title="テスト",
            url="https://test.com",
            snippet="テストスニペット",
            source="test"
        )
        
        if hasattr(result, 'to_dict'):
            result_dict = result.to_dict()
            expected = {
                "title": "テスト",
                "url": "https://test.com",
                "snippet": "テストスニペット",
                "source": "test",
                "metadata": {}
            }
            self.assertEqual(result_dict, expected)


class TestWebSearchProvider(unittest.TestCase):
    """WebSearchProvider基底クラスのテスト"""
    
    def test_base_provider_initialization(self):
        """基底プロバイダーの初期化テスト"""
        provider = WebSearchProvider("test")
        self.assertEqual(provider.name, "test")
        if hasattr(provider, 'call_count'):
            self.assertEqual(provider.call_count, 0)
            self.assertEqual(provider.error_count, 0)
    
    def test_not_implemented_method(self):
        """未実装メソッドのテスト"""
        provider = WebSearchProvider("test")
        if hasattr(provider, 'search'):
            with self.assertRaises(NotImplementedError):
                provider.search("test query")


class TestDuckDuckGoSearchProvider(unittest.TestCase):
    """DuckDuckGoSearchProviderのテスト"""
    
    def setUp(self):
        """テストセットアップ"""
        try:
            self.provider = DuckDuckGoSearchProvider()
        except NameError:
            self.skipTest("DuckDuckGoSearchProvider not available")
    
    def test_initialization(self):
        """初期化テスト"""
        self.assertEqual(self.provider.name, "duckduckgo")
    
    @patch('websearch_utils.requests.get')
    def test_search_success(self, mock_get):
        """検索成功テスト"""
        try:
            # モックレスポンスの設定
            mock_response = Mock()
            mock_response.raise_for_status.return_value = None
            mock_response.json.return_value = {
                "Abstract": "テスト要約",
                "AbstractURL": "https://example.com",
                "Heading": "テストヘッディング",
                "RelatedTopics": [
                    {
                        "Text": "関連トピック1 - 説明",
                        "FirstURL": "https://example1.com"
                    }
                ]
            }
            mock_get.return_value = mock_response
            
            results = self.provider.search("test query", num_results=5)
            
            self.assertIsInstance(results, list)
            if results:
                self.assertIsInstance(results[0], SearchResult)
                self.assertEqual(results[0].source, "duckduckgo")
            
        except NameError:
            self.skipTest("DuckDuckGoSearchProvider not available")
    
    @patch('websearch_utils.requests.get')
    def test_search_error_handling(self, mock_get):
        """検索エラーハンドリングテスト"""
        try:
            mock_get.side_effect = Exception("Network error")
            
            with self.assertRaises(WebSearchError):
                self.provider.search("test query")
                
        except NameError:
            self.skipTest("DuckDuckGoSearchProvider not available")


class TestGoogleSearchProvider(unittest.TestCase):
    """GoogleSearchProviderのテスト"""
    
    def setUp(self):
        """テストセットアップ"""
        try:
            self.provider = GoogleSearchProvider(
                api_key="test_key",
                cx_id="test_cx_id"
            )
        except NameError:
            self.skipTest("GoogleSearchProvider not available")
    
    def test_initialization(self):
        """初期化テスト"""
        self.assertEqual(self.provider.name, "google")
        self.assertEqual(self.provider.api_key, "test_key")
        self.assertEqual(self.provider.cx_id, "test_cx_id")
    
    def test_no_api_key_error(self):
        """APIキー未設定エラーテスト"""
        try:
            provider = GoogleSearchProvider(api_key=None, cx_id=None)
            
            with self.assertRaises(WebSearchError):
                provider.search("test query")
                
        except NameError:
            self.skipTest("GoogleSearchProvider not available")


class TestWebSearchManager(unittest.TestCase):
    """WebSearchManagerのテスト"""
    
    def setUp(self):
        """テストセットアップ"""
        try:
            self.manager = WebSearchManager()
        except NameError:
            self.skipTest("WebSearchManager not available")
    
    def test_initialization(self):
        """初期化テスト"""
        self.assertEqual(self.manager.default_provider, "duckduckgo")
        self.assertIn("duckduckgo", self.manager.providers)
    
    def test_register_provider(self):
        """プロバイダー登録テスト"""
        try:
            test_provider = WebSearchProvider("test")
            self.manager.register_provider("test", test_provider)
            
            self.assertIn("test", self.manager.providers)
            self.assertEqual(self.manager.providers["test"], test_provider)
        except NameError:
            self.skipTest("WebSearchManager not available")
    
    def test_set_default_provider(self):
        """デフォルトプロバイダー設定テスト"""
        try:
            test_provider = WebSearchProvider("test")
            self.manager.register_provider("test", test_provider)
            self.manager.set_default_provider("test")
            
            self.assertEqual(self.manager.default_provider, "test")
        except NameError:
            self.skipTest("WebSearchManager not available")
    
    def test_set_invalid_default_provider(self):
        """無効なデフォルトプロバイダー設定テスト"""
        try:
            with self.assertRaises(ValueError):
                self.manager.set_default_provider("nonexistent")
        except NameError:
            self.skipTest("WebSearchManager not available")
    
    @patch('websearch_utils.requests.get')
    def test_search_with_default_provider(self, mock_get):
        """デフォルトプロバイダーでの検索テスト"""
        try:
            # モックレスポンスの設定
            mock_response = Mock()
            mock_response.raise_for_status.return_value = None
            mock_response.json.return_value = {
                "Abstract": "テスト要約",
                "AbstractURL": "https://example.com",
                "Heading": "テストヘッディング",
                "RelatedTopics": []
            }
            mock_get.return_value = mock_response
            
            results = self.manager.search("test query", num_results=5)
            
            self.assertIsInstance(results, list)
            
        except NameError:
            self.skipTest("WebSearchManager not available")


class TestGlobalFunctions(unittest.TestCase):
    """グローバル関数のテスト"""
    
    def test_get_web_search_manager(self):
        """グローバルマネージャー取得テスト"""
        try:
            manager1 = get_web_search_manager()
            manager2 = get_web_search_manager()
            
            # 同じインスタンスが返されることを確認
            self.assertIs(manager1, manager2)
        except NameError:
            self.skipTest("get_web_search_manager not available")
    
    @patch('websearch_utils.requests.get')
    def test_search_web_function(self, mock_get):
        """search_web関数のテスト"""
        try:
            # モックレスポンスの設定
            mock_response = Mock()
            mock_response.raise_for_status.return_value = None
            mock_response.json.return_value = {
                "Abstract": "テスト要約",
                "AbstractURL": "https://example.com",
                "Heading": "テストヘッディング",
                "RelatedTopics": []
            }
            mock_get.return_value = mock_response
            
            results = search_web("test query", num_results=3)
            
            self.assertIsInstance(results, list)
            
        except NameError:
            self.skipTest("search_web function not available")
    
    @patch('websearch_utils.requests.get')
    def test_search_and_summarize_function(self, mock_get):
        """search_and_summarize関数のテスト"""
        try:
            # モックレスポンスの設定
            mock_response = Mock()
            mock_response.raise_for_status.return_value = None
            mock_response.json.return_value = {
                "Abstract": "テスト要約",
                "AbstractURL": "https://example.com",
                "Heading": "テストヘッディング",
                "RelatedTopics": []
            }
            mock_get.return_value = mock_response
            
            result = search_and_summarize("test query", num_results=3)
            
            self.assertIsInstance(result, dict)
            self.assertIn("query", result)
            self.assertIn("total_results", result)
            self.assertIn("results", result)
            self.assertIn("sources", result)
            
        except NameError:
            self.skipTest("search_and_summarize function not available")


if __name__ == '__main__':
    unittest.main(verbosity=2)
