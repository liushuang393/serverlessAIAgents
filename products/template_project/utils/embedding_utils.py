"""
埋め込み（Embedding）ユーティリティ

このモジュールは、複数の埋め込みAPIプロバイダー（OpenAI、Azure、Google、AWS、Cohere、
HuggingFace、Jina）への統一されたインターフェースを提供します。
"""

import os
import numpy as np
from typing import List, Optional, Union, Dict, Any
import logging

logger = logging.getLogger(__name__)


class EmbeddingError(Exception):
    """埋め込み関連のエラー"""
    pass


class EmbeddingProvider:
    """埋め込みプロバイダーの基底クラス"""
    
    def __init__(self, name: str):
        self.name = name
        self.call_count = 0
        self.error_count = 0
    
    def embed(self, texts: Union[str, List[str]], **kwargs) -> Union[List[float], List[List[float]]]:
        """テキストを埋め込みベクトルに変換（サブクラスで実装）"""
        raise NotImplementedError
    
    def _record_call(self, success: bool = True):
        """呼び出し統計を記録"""
        self.call_count += 1
        if not success:
            self.error_count += 1


class OpenAIEmbeddingProvider(EmbeddingProvider):
    """OpenAI埋め込みプロバイダー"""
    
    def __init__(
        self,
        api_key: Optional[str] = None,
        model: str = "text-embedding-ada-002"
    ):
        super().__init__("openai")
        self.api_key = api_key or os.getenv("OPENAI_API_KEY")
        self.model = model
        
        if not self.api_key:
            logger.warning("OPENAI_API_KEY が設定されていません")
    
    def embed(self, texts: Union[str, List[str]], **kwargs) -> Union[List[float], List[List[float]]]:
        """OpenAI APIを使用してテキストを埋め込む"""
        try:
            if not self.api_key:
                raise EmbeddingError("OPENAI_API_KEY が設定されていません")
            
            from openai import OpenAI
            
            client = OpenAI(api_key=self.api_key)
            
            # 単一テキストの場合はリストに変換
            is_single = isinstance(texts, str)
            text_list: List[str]
            if is_single:
                text_list = [texts]  # type: ignore
            else:
                text_list = texts  # type: ignore
            
            response = client.embeddings.create(
                model=self.model,
                input=text_list
            )
            
            embeddings = [np.array(data.embedding, dtype=np.float32) for data in response.data]
            
            logger.info(f"OpenAI埋め込みを取得しました: {len(embeddings)}件")
            self._record_call(True)
            
            return embeddings[0] if is_single else embeddings
            
        except Exception as e:
            logger.error(f"OpenAI埋め込みエラー: {e}")
            self._record_call(False)
            raise EmbeddingError(f"OpenAI埋め込みに失敗しました: {e}")


class AzureOpenAIEmbeddingProvider(EmbeddingProvider):
    """Azure OpenAI埋め込みプロバイダー"""
    
    def __init__(
        self,
        api_key: Optional[str] = None,
        endpoint: Optional[str] = None,
        api_version: str = "2023-05-15",
        deployment_name: str = "text-embedding-ada-002"
    ):
        super().__init__("azure_openai")
        self.api_key = api_key or os.getenv("AZURE_OPENAI_API_KEY")
        self.endpoint = endpoint or os.getenv("AZURE_OPENAI_ENDPOINT")
        self.api_version = api_version
        self.deployment_name = deployment_name
        
        if not self.api_key or not self.endpoint:
            logger.warning("Azure OpenAI の設定が不完全です")
    
    def embed(self, texts: Union[str, List[str]], **kwargs) -> Union[List[float], List[List[float]]]:
        """Azure OpenAI APIを使用してテキストを埋め込む"""
        try:
            if not self.api_key or not self.endpoint:
                raise EmbeddingError("Azure OpenAI の設定が不完全です")
            
            from openai import AzureOpenAI
            
            client = AzureOpenAI(
                api_key=self.api_key,
                azure_endpoint=self.endpoint,
                api_version=self.api_version
            )
            
            is_single = isinstance(texts, str)
            text_list: List[str]
            if is_single:
                text_list = [texts]  # type: ignore
            else:
                text_list = texts  # type: ignore

            response = client.embeddings.create(
                model=self.deployment_name,
                input=text_list
            )
            
            embeddings = [np.array(data.embedding, dtype=np.float32) for data in response.data]
            
            logger.info(f"Azure OpenAI埋め込みを取得しました: {len(embeddings)}件")
            self._record_call(True)
            
            return embeddings[0] if is_single else embeddings
            
        except Exception as e:
            logger.error(f"Azure OpenAI埋め込みエラー: {e}")
            self._record_call(False)
            raise EmbeddingError(f"Azure OpenAI埋め込みに失敗しました: {e}")


class GoogleEmbeddingProvider(EmbeddingProvider):
    """Google Vertex AI埋め込みプロバイダー"""
    
    def __init__(
        self,
        project_id: Optional[str] = None,
        location: str = "us-central1",
        model: str = "textembedding-gecko@001"
    ):
        super().__init__("google")
        self.project_id = project_id or os.getenv("GOOGLE_CLOUD_PROJECT")
        self.location = location
        self.model = model
        
        if not self.project_id:
            logger.warning("GOOGLE_CLOUD_PROJECT が設定されていません")
    
    def embed(self, texts: Union[str, List[str]], **kwargs) -> Union[List[float], List[List[float]]]:
        """Google Vertex AIを使用してテキストを埋め込む"""
        try:
            if not self.project_id:
                raise EmbeddingError("GOOGLE_CLOUD_PROJECT が設定されていません")
            
            import vertexai
            from vertexai.preview.language_models import TextEmbeddingModel
            
            vertexai.init(project=self.project_id, location=self.location)
            model = TextEmbeddingModel.from_pretrained(self.model)
            
            is_single = isinstance(texts, str)
            text_list: List[str]
            if is_single:
                text_list = [texts]  # type: ignore
            else:
                text_list = texts  # type: ignore

            embeddings_response = model.get_embeddings(text_list)
            embeddings = [np.array(emb.values, dtype=np.float32) for emb in embeddings_response]
            
            logger.info(f"Google埋め込みを取得しました: {len(embeddings)}件")
            self._record_call(True)
            
            return embeddings[0] if is_single else embeddings
            
        except Exception as e:
            logger.error(f"Google埋め込みエラー: {e}")
            self._record_call(False)
            raise EmbeddingError(f"Google埋め込みに失敗しました: {e}")


class AWSEmbeddingProvider(EmbeddingProvider):
    """AWS Bedrock埋め込みプロバイダー"""
    
    def __init__(
        self,
        region_name: str = "us-east-1",
        model_id: str = "amazon.titan-embed-text-v2:0"
    ):
        super().__init__("aws")
        self.region_name = region_name
        self.model_id = model_id
    
    def embed(self, texts: Union[str, List[str]], **kwargs) -> Union[List[float], List[List[float]]]:
        """AWS Bedrockを使用してテキストを埋め込む"""
        try:
            import boto3
            import json
            
            client = boto3.client("bedrock-runtime", region_name=self.region_name)
            
            is_single = isinstance(texts, str)
            text_list: List[str]
            if is_single:
                text_list = [texts]  # type: ignore
            else:
                text_list = texts  # type: ignore

            embeddings = []
            for text in text_list:
                body = {"inputText": text}
                response = client.invoke_model(
                    modelId=self.model_id,
                    contentType="application/json",
                    body=json.dumps(body)
                )
                
                response_body = json.loads(response["body"].read())
                embedding = np.array(response_body["embedding"], dtype=np.float32)
                embeddings.append(embedding)
            
            logger.info(f"AWS埋め込みを取得しました: {len(embeddings)}件")
            self._record_call(True)
            
            return embeddings[0] if is_single else embeddings
            
        except Exception as e:
            logger.error(f"AWS埋め込みエラー: {e}")
            self._record_call(False)
            raise EmbeddingError(f"AWS埋め込みに失敗しました: {e}")


class CohereEmbeddingProvider(EmbeddingProvider):
    """Cohere埋め込みプロバイダー"""
    
    def __init__(
        self,
        api_key: Optional[str] = None,
        model: str = "embed-english-v2.0"
    ):
        super().__init__("cohere")
        self.api_key = api_key or os.getenv("COHERE_API_KEY")
        self.model = model
        
        if not self.api_key:
            logger.warning("COHERE_API_KEY が設定されていません")
    
    def embed(self, texts: Union[str, List[str]], **kwargs) -> Union[List[float], List[List[float]]]:
        """Cohereを使用してテキストを埋め込む"""
        try:
            if not self.api_key:
                raise EmbeddingError("COHERE_API_KEY が設定されていません")
            
            import cohere
            
            client = cohere.Client(self.api_key)
            
            is_single = isinstance(texts, str)
            text_list: List[str]
            if is_single:
                text_list = [texts]  # type: ignore
            else:
                text_list = texts  # type: ignore

            response = client.embed(texts=text_list, model=self.model)
            embeddings = [np.array(emb, dtype=np.float32) for emb in response.embeddings]
            
            logger.info(f"Cohere埋め込みを取得しました: {len(embeddings)}件")
            self._record_call(True)
            
            return embeddings[0] if is_single else embeddings
            
        except Exception as e:
            logger.error(f"Cohere埋め込みエラー: {e}")
            self._record_call(False)
            raise EmbeddingError(f"Cohere埋め込みに失敗しました: {e}")


class HuggingFaceEmbeddingProvider(EmbeddingProvider):
    """HuggingFace埋め込みプロバイダー"""
    
    def __init__(
        self,
        api_key: Optional[str] = None,
        model: str = "sentence-transformers/all-MiniLM-L6-v2"
    ):
        super().__init__("huggingface")
        self.api_key = api_key or os.getenv("HUGGINGFACE_API_KEY")
        self.model = model
        
        if not self.api_key:
            logger.warning("HUGGINGFACE_API_KEY が設定されていません")
    
    def embed(self, texts: Union[str, List[str]], **kwargs) -> Union[List[float], List[List[float]]]:
        """HuggingFaceを使用してテキストを埋め込む"""
        try:
            if not self.api_key:
                raise EmbeddingError("HUGGINGFACE_API_KEY が設定されていません")
            
            import requests
            
            api_url = f"https://api-inference.huggingface.co/models/{self.model}"
            headers = {"Authorization": f"Bearer {self.api_key}"}
            
            is_single = isinstance(texts, str)
            text_list: List[str]
            if is_single:
                text_list = [texts]  # type: ignore
            else:
                text_list = texts  # type: ignore

            embeddings = []
            for text in text_list:
                response = requests.post(api_url, headers=headers, json={"inputs": text})
                response.raise_for_status()
                
                embedding = np.array(response.json(), dtype=np.float32)
                embeddings.append(embedding)
            
            logger.info(f"HuggingFace埋め込みを取得しました: {len(embeddings)}件")
            self._record_call(True)
            
            return embeddings[0] if is_single else embeddings
            
        except Exception as e:
            logger.error(f"HuggingFace埋め込みエラー: {e}")
            self._record_call(False)
            raise EmbeddingError(f"HuggingFace埋め込みに失敗しました: {e}")


class JinaEmbeddingProvider(EmbeddingProvider):
    """Jina埋め込みプロバイダー"""
    
    def __init__(
        self,
        api_key: Optional[str] = None,
        model: str = "jina-embeddings-v3"
    ):
        super().__init__("jina")
        self.api_key = api_key or os.getenv("JINA_API_KEY")
        self.model = model
        
        if not self.api_key:
            logger.warning("JINA_API_KEY が設定されていません")
    
    def embed(self, texts: Union[str, List[str]], **kwargs) -> Union[List[float], List[List[float]]]:
        """Jinaを使用してテキストを埋め込む"""
        try:
            if not self.api_key:
                raise EmbeddingError("JINA_API_KEY が設定されていません")
            
            import requests
            
            url = "https://api.jina.ai/v2/embed"
            headers = {"Authorization": f"Bearer {self.api_key}"}
            
            is_single = isinstance(texts, str)
            text_list: List[str]
            if is_single:
                text_list = [texts]  # type: ignore
            else:
                text_list = texts  # type: ignore

            payload = {"data": text_list, "model": self.model}
            response = requests.post(url, headers=headers, json=payload)
            response.raise_for_status()
            
            data = response.json()
            embeddings = [np.array(item["embedding"], dtype=np.float32) for item in data["data"]]
            
            logger.info(f"Jina埋め込みを取得しました: {len(embeddings)}件")
            self._record_call(True)
            
            return embeddings[0] if is_single else embeddings
            
        except Exception as e:
            logger.error(f"Jina埋め込みエラー: {e}")
            self._record_call(False)
            raise EmbeddingError(f"Jina埋め込みに失敗しました: {e}")


class EmbeddingManager:
    """埋め込みプロバイダー管理クラス"""
    
    def __init__(self):
        self.providers: Dict[str, EmbeddingProvider] = {}
        self.default_provider = "openai"
        
        # デフォルトプロバイダーを初期化
        self.register_provider("openai", OpenAIEmbeddingProvider())
    
    def register_provider(self, name: str, provider: EmbeddingProvider) -> None:
        """プロバイダーを登録"""
        self.providers[name] = provider
        logger.info(f"埋め込みプロバイダーを登録しました: {name}")
    
    def set_default_provider(self, name: str) -> None:
        """デフォルトプロバイダーを設定"""
        if name not in self.providers:
            raise ValueError(f"プロバイダー '{name}' が登録されていません")
        self.default_provider = name
        logger.info(f"デフォルト埋め込みプロバイダーを設定しました: {name}")
    
    def embed(
        self,
        texts: Union[str, List[str]],
        provider: Optional[str] = None,
        **kwargs
    ) -> Union[List[float], List[List[float]]]:
        """テキストを埋め込みベクトルに変換"""
        provider_name = provider or self.default_provider
        
        if provider_name not in self.providers:
            raise ValueError(f"プロバイダー '{provider_name}' が登録されていません")
        
        return self.providers[provider_name].embed(texts, **kwargs)
    
    def get_stats(self) -> Dict[str, Dict[str, Union[int, float]]]:
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
_embedding_manager: Optional[EmbeddingManager] = None


def get_embedding_manager() -> EmbeddingManager:
    """グローバル埋め込みマネージャーを取得"""
    global _embedding_manager
    if _embedding_manager is None:
        _embedding_manager = EmbeddingManager()
    return _embedding_manager


# 便利関数
def embed(
    texts: Union[str, List[str]],
    provider: Optional[str] = None,
    **kwargs
) -> Union[List[float], List[List[float]]]:
    """テキスト埋め込みの便利関数"""
    manager = get_embedding_manager()
    return manager.embed(texts, provider, **kwargs)


def setup_embedding_providers(config: Dict[str, Dict[str, Any]]) -> None:
    """設定から埋め込みプロバイダーを設定"""
    manager = get_embedding_manager()
    
    for name, provider_config in config.items():
        provider_type = provider_config.get("type", "openai")
        
        provider: EmbeddingProvider
        if provider_type == "openai":
            provider = OpenAIEmbeddingProvider(
                api_key=provider_config.get("api_key"),
                model=provider_config.get("model", "text-embedding-ada-002")
            )
        elif provider_type == "azure_openai":
            provider = AzureOpenAIEmbeddingProvider(
                api_key=provider_config.get("api_key"),
                endpoint=provider_config.get("endpoint"),
                api_version=provider_config.get("api_version", "2023-05-15"),
                deployment_name=provider_config.get("deployment_name", "text-embedding-ada-002")
            )
        elif provider_type == "google":
            provider = GoogleEmbeddingProvider(
                project_id=provider_config.get("project_id"),
                location=provider_config.get("location", "us-central1"),
                model=provider_config.get("model", "textembedding-gecko@001")
            )
        elif provider_type == "aws":
            provider = AWSEmbeddingProvider(
                region_name=provider_config.get("region_name", "us-east-1"),
                model_id=provider_config.get("model_id", "amazon.titan-embed-text-v2:0")
            )
        elif provider_type == "cohere":
            provider = CohereEmbeddingProvider(
                api_key=provider_config.get("api_key"),
                model=provider_config.get("model", "embed-english-v2.0")
            )
        elif provider_type == "huggingface":
            provider = HuggingFaceEmbeddingProvider(
                api_key=provider_config.get("api_key"),
                model=provider_config.get("model", "sentence-transformers/all-MiniLM-L6-v2")
            )
        elif provider_type == "jina":
            provider = JinaEmbeddingProvider(
                api_key=provider_config.get("api_key"),
                model=provider_config.get("model", "jina-embeddings-v3")
            )
        else:
            logger.warning(f"未知の埋め込みプロバイダータイプ: {provider_type}")
            continue
        
        manager.register_provider(name, provider)
        
        if provider_config.get("default", False):
            manager.set_default_provider(name)
