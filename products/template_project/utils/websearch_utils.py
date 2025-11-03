"""
Web検索ユーティリティ

このモジュールは、複数のWeb検索API（Google Custom Search、Bing、DuckDuckGo、
Brave、SerpApi）への統一されたインターフェースを提供します。
"""

import os
import requests
from typing import List, Dict, Any, Optional
import logging

logger = logging.getLogger(__name__)


class WebSearchError(Exception):
    """Web検索関連のエラー"""
    pass


class SearchResult:
    """検索結果クラス"""
    
    def __init__(
        self,
        title: str,
        url: str,
        snippet: str,
        source: str,
        metadata: Optional[Dict[str, Any]] = None
    ):
        self.title = title
        self.url = url
        self.snippet = snippet
        self.source = source
        self.metadata = metadata or {}
    
    def to_dict(self) -> Dict[str, Any]:
        """辞書形式に変換"""
        return {
            "title": self.title,
            "url": self.url,
            "snippet": self.snippet,
            "source": self.source,
            "metadata": self.metadata
        }


class WebSearchProvider:
    """Web検索プロバイダーの基底クラス"""
    
    def __init__(self, name: str):
        self.name = name
        self.call_count = 0
        self.error_count = 0
    
    def search(self, query: str, num_results: int = 10, **kwargs) -> List[SearchResult]:
        """検索を実行（サブクラスで実装）"""
        raise NotImplementedError
    
    def _record_call(self, success: bool = True):
        """呼び出し統計を記録"""
        self.call_count += 1
        if not success:
            self.error_count += 1


class GoogleSearchProvider(WebSearchProvider):
    """Google Custom Search APIプロバイダー"""
    
    def __init__(self, api_key: Optional[str] = None, cx_id: Optional[str] = None):
        super().__init__("google")
        self.api_key = api_key or os.getenv("GOOGLE_SEARCH_API_KEY")
        self.cx_id = cx_id or os.getenv("GOOGLE_SEARCH_CX_ID")
        
        if not self.api_key or not self.cx_id:
            logger.warning("Google Search API キーまたはCX IDが設定されていません")
    
    def search(self, query: str, num_results: int = 10, **kwargs) -> List[SearchResult]:
        """Google Custom Search APIで検索"""
        try:
            if not self.api_key or not self.cx_id:
                raise WebSearchError("Google Search API キーまたはCX IDが設定されていません")
            
            url = "https://www.googleapis.com/customsearch/v1"
            params = {
                "key": self.api_key,
                "cx": self.cx_id,
                "q": query,
                "num": min(num_results, 10)  # Google APIの制限
            }
            
            # 追加パラメータ
            if "language" in kwargs:
                params["lr"] = f"lang_{kwargs['language']}"
            if "country" in kwargs:
                params["cr"] = f"country{kwargs['country']}"
            
            response = requests.get(url, params=params, timeout=30)  # type: ignore
            response.raise_for_status()
            
            data = response.json()
            results = []
            
            for item in data.get("items", []):
                result = SearchResult(
                    title=item.get("title", ""),
                    url=item.get("link", ""),
                    snippet=item.get("snippet", ""),
                    source="google",
                    metadata={
                        "display_link": item.get("displayLink", ""),
                        "formatted_url": item.get("formattedUrl", "")
                    }
                )
                results.append(result)
            
            logger.info(f"Google検索を実行しました: {len(results)}件の結果")
            self._record_call(True)
            return results
            
        except Exception as e:
            logger.error(f"Google検索エラー: {e}")
            self._record_call(False)
            raise WebSearchError(f"Google検索に失敗しました: {e}")


class BingSearchProvider(WebSearchProvider):
    """Bing Web Search APIプロバイダー"""
    
    def __init__(self, api_key: Optional[str] = None):
        super().__init__("bing")
        self.api_key = api_key or os.getenv("BING_SEARCH_API_KEY")
        
        if not self.api_key:
            logger.warning("Bing Search API キーが設定されていません")
    
    def search(self, query: str, num_results: int = 10, **kwargs) -> List[SearchResult]:
        """Bing Web Search APIで検索"""
        try:
            if not self.api_key:
                raise WebSearchError("Bing Search API キーが設定されていません")
            
            url = "https://api.bing.microsoft.com/v7.0/search"
            headers = {"Ocp-Apim-Subscription-Key": self.api_key}
            params = {
                "q": query,
                "count": min(num_results, 50)  # Bing APIの制限
            }
            
            # 追加パラメータ
            if "language" in kwargs:
                params["setLang"] = kwargs["language"]
            if "country" in kwargs:
                params["cc"] = kwargs["country"]
            
            response = requests.get(url, headers=headers, params=params, timeout=30)  # type: ignore
            response.raise_for_status()
            
            data = response.json()
            results = []
            
            for item in data.get("webPages", {}).get("value", []):
                result = SearchResult(
                    title=item.get("name", ""),
                    url=item.get("url", ""),
                    snippet=item.get("snippet", ""),
                    source="bing",
                    metadata={
                        "display_url": item.get("displayUrl", ""),
                        "date_last_crawled": item.get("dateLastCrawled", "")
                    }
                )
                results.append(result)
            
            logger.info(f"Bing検索を実行しました: {len(results)}件の結果")
            self._record_call(True)
            return results
            
        except Exception as e:
            logger.error(f"Bing検索エラー: {e}")
            self._record_call(False)
            raise WebSearchError(f"Bing検索に失敗しました: {e}")


class DuckDuckGoSearchProvider(WebSearchProvider):
    """DuckDuckGo Instant Answer APIプロバイダー"""
    
    def __init__(self):
        super().__init__("duckduckgo")
    
    def search(self, query: str, num_results: int = 10, **kwargs) -> List[SearchResult]:
        """DuckDuckGo Instant Answer APIで検索"""
        try:
            url = "https://api.duckduckgo.com/"
            params = {
                "q": query,
                "format": "json",
                "no_html": "1",
                "skip_disambig": "1"
            }
            
            response = requests.get(url, params=params, timeout=30)
            response.raise_for_status()
            
            data = response.json()
            results = []
            
            # Abstract（要約）
            if data.get("Abstract"):
                result = SearchResult(
                    title=data.get("Heading", query),
                    url=data.get("AbstractURL", ""),
                    snippet=data.get("Abstract", ""),
                    source="duckduckgo",
                    metadata={"type": "abstract"}
                )
                results.append(result)
            
            # Related Topics
            for topic in data.get("RelatedTopics", [])[:num_results-1]:
                if isinstance(topic, dict) and "Text" in topic:
                    result = SearchResult(
                        title=topic.get("Text", "").split(" - ")[0],
                        url=topic.get("FirstURL", ""),
                        snippet=topic.get("Text", ""),
                        source="duckduckgo",
                        metadata={"type": "related_topic"}
                    )
                    results.append(result)
            
            logger.info(f"DuckDuckGo検索を実行しました: {len(results)}件の結果")
            self._record_call(True)
            return results
            
        except Exception as e:
            logger.error(f"DuckDuckGo検索エラー: {e}")
            self._record_call(False)
            raise WebSearchError(f"DuckDuckGo検索に失敗しました: {e}")


class BraveSearchProvider(WebSearchProvider):
    """Brave Search APIプロバイダー"""
    
    def __init__(self, api_key: Optional[str] = None):
        super().__init__("brave")
        self.api_key = api_key or os.getenv("BRAVE_SEARCH_API_KEY")
        
        if not self.api_key:
            logger.warning("Brave Search API キーが設定されていません")
    
    def search(self, query: str, num_results: int = 10, **kwargs) -> List[SearchResult]:
        """Brave Search APIで検索"""
        try:
            if not self.api_key:
                raise WebSearchError("Brave Search API キーが設定されていません")
            
            url = "https://api.search.brave.com/res/v1/web/search"
            headers = {"X-Subscription-Token": self.api_key}
            params = {
                "q": query,
                "count": min(num_results, 20)  # Brave APIの制限
            }
            
            # 追加パラメータ
            if "country" in kwargs:
                params["country"] = kwargs["country"]
            if "language" in kwargs:
                params["search_lang"] = kwargs["language"]
            
            response = requests.get(url, headers=headers, params=params, timeout=30)  # type: ignore
            response.raise_for_status()
            
            data = response.json()
            results = []
            
            for item in data.get("web", {}).get("results", []):
                result = SearchResult(
                    title=item.get("title", ""),
                    url=item.get("url", ""),
                    snippet=item.get("description", ""),
                    source="brave",
                    metadata={
                        "age": item.get("age", ""),
                        "language": item.get("language", "")
                    }
                )
                results.append(result)
            
            logger.info(f"Brave検索を実行しました: {len(results)}件の結果")
            self._record_call(True)
            return results
            
        except Exception as e:
            logger.error(f"Brave検索エラー: {e}")
            self._record_call(False)
            raise WebSearchError(f"Brave検索に失敗しました: {e}")


class SerpApiProvider(WebSearchProvider):
    """SerpApiプロバイダー"""
    
    def __init__(self, api_key: Optional[str] = None):
        super().__init__("serpapi")
        self.api_key = api_key or os.getenv("SERPAPI_API_KEY")
        
        if not self.api_key:
            logger.warning("SerpApi API キーが設定されていません")
    
    def search(self, query: str, num_results: int = 10, **kwargs) -> List[SearchResult]:
        """SerpApiで検索"""
        try:
            if not self.api_key:
                raise WebSearchError("SerpApi API キーが設定されていません")
            
            url = "https://serpapi.com/search"
            params = {
                "engine": kwargs.get("engine", "google"),
                "q": query,
                "api_key": self.api_key,
                "num": min(num_results, 100)
            }
            
            # 追加パラメータ
            if "country" in kwargs:
                params["gl"] = kwargs["country"]
            if "language" in kwargs:
                params["hl"] = kwargs["language"]
            
            response = requests.get(url, params=params, timeout=30)
            response.raise_for_status()
            
            data = response.json()
            results = []
            
            for item in data.get("organic_results", []):
                result = SearchResult(
                    title=item.get("title", ""),
                    url=item.get("link", ""),
                    snippet=item.get("snippet", ""),
                    source="serpapi",
                    metadata={
                        "position": item.get("position", 0),
                        "displayed_link": item.get("displayed_link", "")
                    }
                )
                results.append(result)
            
            logger.info(f"SerpApi検索を実行しました: {len(results)}件の結果")
            self._record_call(True)
            return results
            
        except Exception as e:
            logger.error(f"SerpApi検索エラー: {e}")
            self._record_call(False)
            raise WebSearchError(f"SerpApi検索に失敗しました: {e}")


class WebSearchManager:
    """Web検索プロバイダー管理クラス"""
    
    def __init__(self):
        self.providers: Dict[str, WebSearchProvider] = {}
        self.default_provider = "duckduckgo"
        
        # デフォルトプロバイダーを初期化（API キー不要）
        self.register_provider("duckduckgo", DuckDuckGoSearchProvider())
    
    def register_provider(self, name: str, provider: WebSearchProvider) -> None:
        """プロバイダーを登録"""
        self.providers[name] = provider
        logger.info(f"Web検索プロバイダーを登録しました: {name}")
    
    def set_default_provider(self, name: str) -> None:
        """デフォルトプロバイダーを設定"""
        if name not in self.providers:
            raise ValueError(f"プロバイダー '{name}' が登録されていません")
        self.default_provider = name
        logger.info(f"デフォルトWeb検索プロバイダーを設定しました: {name}")
    
    def search(
        self,
        query: str,
        num_results: int = 10,
        provider: Optional[str] = None,
        **kwargs
    ) -> List[SearchResult]:
        """Web検索を実行"""
        provider_name = provider or self.default_provider
        
        if provider_name not in self.providers:
            raise ValueError(f"プロバイダー '{provider_name}' が登録されていません")
        
        return self.providers[provider_name].search(query, num_results, **kwargs)
    
    def get_stats(self) -> Dict[str, Dict[str, Any]]:
        """統計情報を取得"""
        stats = {}
        for name, provider in self.providers.items():
            stats[name] = {
                "call_count": provider.call_count,
                "error_count": provider.error_count,
                "success_rate": (
                    (provider.call_count - provider.error_count) / provider.call_count
                    if provider.call_count > 0 else 0.0
                )
            }
        return stats


# グローバルマネージャー
_web_search_manager: Optional[WebSearchManager] = None


def get_web_search_manager() -> WebSearchManager:
    """グローバルWeb検索マネージャーを取得"""
    global _web_search_manager
    if _web_search_manager is None:
        _web_search_manager = WebSearchManager()
    return _web_search_manager


# 便利関数
def search_web(
    query: str,
    num_results: int = 10,
    provider: Optional[str] = None,
    **kwargs
) -> List[SearchResult]:
    """Web検索の便利関数"""
    manager = get_web_search_manager()
    return manager.search(query, num_results, provider, **kwargs)


def setup_search_providers(config: Dict[str, Dict[str, Any]]) -> None:
    """設定からWeb検索プロバイダーを設定"""
    manager = get_web_search_manager()
    
    for name, provider_config in config.items():
        provider_type = provider_config.get("type", "duckduckgo")
        
        provider: WebSearchProvider
        if provider_type == "google":
            provider = GoogleSearchProvider(
                api_key=provider_config.get("api_key"),
                cx_id=provider_config.get("cx_id")
            )
        elif provider_type == "bing":
            provider = BingSearchProvider(
                api_key=provider_config.get("api_key")
            )
        elif provider_type == "duckduckgo":
            provider = DuckDuckGoSearchProvider()
        elif provider_type == "brave":
            provider = BraveSearchProvider(
                api_key=provider_config.get("api_key")
            )
        elif provider_type == "serpapi":
            provider = SerpApiProvider(
                api_key=provider_config.get("api_key")
            )
        else:
            logger.warning(f"未知のWeb検索プロバイダータイプ: {provider_type}")
            continue
        
        manager.register_provider(name, provider)
        
        if provider_config.get("default", False):
            manager.set_default_provider(name)


def search_and_summarize(
    query: str,
    num_results: int = 5,
    provider: Optional[str] = None
) -> Dict[str, Any]:
    """検索結果をまとめて返す便利関数"""
    results = search_web(query, num_results, provider)
    
    return {
        "query": query,
        "total_results": len(results),
        "results": [result.to_dict() for result in results],
        "sources": list(set(result.source for result in results))
    }
