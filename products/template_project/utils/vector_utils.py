"""
ベクトルデータベースユーティリティ

このモジュールは、複数のベクトルデータベース（FAISS、Pinecone、Qdrant、Weaviate、
Milvus、Chroma、Redis）への統一されたインターフェースを提供します。
"""

import os
import numpy as np
from typing import List, Dict, Any, Optional, Tuple, Union, TYPE_CHECKING

if TYPE_CHECKING:
    import pinecone
    import qdrant_client
    import weaviate
    import chromadb
import logging

logger = logging.getLogger(__name__)


class VectorDBError(Exception):
    """ベクトルデータベース関連のエラー"""
    pass


class VectorDocument:
    """ベクトル文書クラス"""
    
    def __init__(
        self,
        id: str,
        vector: List[float],
        metadata: Optional[Dict[str, Any]] = None,
        text: Optional[str] = None
    ):
        self.id = id
        self.vector = np.array(vector, dtype=np.float32)
        self.metadata = metadata or {}
        self.text = text


class SearchResult:
    """検索結果クラス"""
    
    def __init__(
        self,
        id: str,
        score: float,
        vector: Optional[List[float]] = None,
        metadata: Optional[Dict[str, Any]] = None,
        text: Optional[str] = None
    ):
        self.id = id
        self.score = score
        self.vector = vector
        self.metadata = metadata or {}
        self.text = text


class VectorDBProvider:
    """ベクトルデータベースプロバイダーの基底クラス"""
    
    def __init__(self, name: str):
        self.name = name
        self.is_connected = False
    
    def connect(self, **kwargs) -> None:
        """データベースに接続（サブクラスで実装）"""
        raise NotImplementedError
    
    def disconnect(self) -> None:
        """データベースから切断（サブクラスで実装）"""
        raise NotImplementedError
    
    def create_collection(self, name: str, dimension: int, **kwargs) -> None:
        """コレクション/インデックスを作成（サブクラスで実装）"""
        raise NotImplementedError
    
    def delete_collection(self, name: str) -> None:
        """コレクション/インデックスを削除（サブクラスで実装）"""
        raise NotImplementedError
    
    def upsert(self, collection_name: str, documents: List[VectorDocument]) -> None:
        """文書を挿入/更新（サブクラスで実装）"""
        raise NotImplementedError
    
    def search(
        self,
        collection_name: str,
        query_vector: List[float],
        top_k: int = 10,
        filter_dict: Optional[Dict[str, Any]] = None
    ) -> List[SearchResult]:
        """ベクトル検索（サブクラスで実装）"""
        raise NotImplementedError
    
    def delete(self, collection_name: str, ids: List[str]) -> None:
        """文書を削除（サブクラスで実装）"""
        raise NotImplementedError


class FAISSProvider(VectorDBProvider):
    """FAISSプロバイダー"""
    
    def __init__(self):
        super().__init__("faiss")
        self.indexes = {}
        self.id_maps = {}
        self.metadata_maps = {}
    
    def connect(self, **kwargs) -> None:
        """FAISS接続（ローカルなので常に成功）"""
        try:
            import faiss
            self.faiss = faiss
            self.is_connected = True
            logger.info("FAISSに接続しました")
        except ImportError:
            raise VectorDBError("faissライブラリがインストールされていません")
    
    def disconnect(self) -> None:
        """FAISS切断"""
        self.indexes.clear()
        self.id_maps.clear()
        self.metadata_maps.clear()
        self.is_connected = False
        logger.info("FAISSから切断しました")
    
    def create_collection(self, name: str, dimension: int, **kwargs) -> None:
        """FAISSインデックスを作成"""
        if not self.is_connected:
            raise VectorDBError("FAISSに接続されていません")
        
        index_type = kwargs.get("index_type", "flat")
        
        if index_type == "flat":
            index = self.faiss.IndexFlatL2(dimension)
        elif index_type == "ivf":
            nlist = kwargs.get("nlist", 100)
            quantizer = self.faiss.IndexFlatL2(dimension)
            index = self.faiss.IndexIVFFlat(quantizer, dimension, nlist)
        else:
            raise VectorDBError(f"サポートされていないインデックスタイプ: {index_type}")
        
        self.indexes[name] = index
        self.id_maps[name] = {}
        self.metadata_maps[name] = {}
        
        logger.info(f"FAISSインデックスを作成しました: {name}")
    
    def delete_collection(self, name: str) -> None:
        """FAISSインデックスを削除"""
        if name in self.indexes:
            del self.indexes[name]
            del self.id_maps[name]
            del self.metadata_maps[name]
            logger.info(f"FAISSインデックスを削除しました: {name}")
    
    def upsert(self, collection_name: str, documents: List[VectorDocument]) -> None:
        """FAISSに文書を追加"""
        if collection_name not in self.indexes:
            raise VectorDBError(f"インデックスが存在しません: {collection_name}")
        
        index = self.indexes[collection_name]
        id_map = self.id_maps[collection_name]
        metadata_map = self.metadata_maps[collection_name]
        
        vectors = np.array([doc.vector for doc in documents], dtype=np.float32)
        
        # IVFインデックスの場合はトレーニングが必要
        if hasattr(index, 'is_trained') and not index.is_trained:
            index.train(vectors)
        
        start_idx = index.ntotal
        index.add(vectors)
        
        for i, doc in enumerate(documents):
            faiss_id = start_idx + i
            id_map[doc.id] = faiss_id
            metadata_map[faiss_id] = {
                "id": doc.id,
                "metadata": doc.metadata,
                "text": doc.text
            }
        
        logger.info(f"FAISS に {len(documents)} 件の文書を追加しました")
    
    def search(
        self,
        collection_name: str,
        query_vector: List[float],
        top_k: int = 10,
        filter_dict: Optional[Dict[str, Any]] = None
    ) -> List[SearchResult]:
        """FAISSでベクトル検索"""
        if collection_name not in self.indexes:
            raise VectorDBError(f"インデックスが存在しません: {collection_name}")
        
        index = self.indexes[collection_name]
        metadata_map = self.metadata_maps[collection_name]
        
        query = np.array([query_vector], dtype=np.float32)
        distances, indices = index.search(query, top_k)
        
        results = []
        for i, (distance, idx) in enumerate(zip(distances[0], indices[0])):
            if idx == -1:  # 無効なインデックス
                continue
            
            if idx in metadata_map:
                doc_info = metadata_map[idx]
                
                # フィルタリング
                if filter_dict:
                    match = True
                    for key, value in filter_dict.items():
                        if key not in doc_info["metadata"] or doc_info["metadata"][key] != value:
                            match = False
                            break
                    if not match:
                        continue
                
                result = SearchResult(
                    id=doc_info["id"],
                    score=float(distance),
                    metadata=doc_info["metadata"],
                    text=doc_info["text"]
                )
                results.append(result)
        
        return results
    
    def delete(self, collection_name: str, ids: List[str]) -> None:
        """FAISS文書削除（制限あり）"""
        logger.warning("FAISSは文書の削除をサポートしていません。インデックスを再構築してください。")


class PineconeProvider(VectorDBProvider):
    """Pineconeプロバイダー"""
    
    def __init__(self, api_key: Optional[str] = None, environment: Optional[str] = None):
        super().__init__("pinecone")
        self.api_key = api_key or os.getenv("PINECONE_API_KEY")
        self.environment = environment or os.getenv("PINECONE_ENVIRONMENT")
        self.pinecone: Optional[Any] = None
    
    def connect(self, **kwargs) -> None:
        """Pineconeに接続"""
        try:
            import pinecone
            
            if not self.api_key or not self.environment:
                raise VectorDBError("Pinecone API キーまたは環境が設定されていません")
            
            pinecone.init(api_key=self.api_key, environment=self.environment)
            self.pinecone = pinecone
            self.is_connected = True
            logger.info("Pineconeに接続しました")
            
        except ImportError:
            raise VectorDBError("pinecone-clientライブラリがインストールされていません")
        except Exception as e:
            raise VectorDBError(f"Pinecone接続エラー: {e}")
    
    def disconnect(self) -> None:
        """Pineconeから切断"""
        self.is_connected = False
        logger.info("Pineconeから切断しました")
    
    def create_collection(self, name: str, dimension: int, **kwargs) -> None:
        """Pineconeインデックスを作成"""
        if not self.is_connected or self.pinecone is None:
            raise VectorDBError("Pineconeに接続されていません")

        metric = kwargs.get("metric", "cosine")

        if name not in self.pinecone.list_indexes():
            self.pinecone.create_index(name=name, dimension=dimension, metric=metric)
            logger.info(f"Pineconeインデックスを作成しました: {name}")
        else:
            logger.info(f"Pineconeインデックスは既に存在します: {name}")
    
    def delete_collection(self, name: str) -> None:
        """Pineconeインデックスを削除"""
        if not self.is_connected or self.pinecone is None:
            raise VectorDBError("Pineconeに接続されていません")

        if name in self.pinecone.list_indexes():
            self.pinecone.delete_index(name)
            logger.info(f"Pineconeインデックスを削除しました: {name}")
    
    def upsert(self, collection_name: str, documents: List[VectorDocument]) -> None:
        """Pineconeに文書を挿入"""
        if not self.is_connected or self.pinecone is None:
            raise VectorDBError("Pineconeに接続されていません")

        index = self.pinecone.Index(collection_name)
        
        vectors = []
        for doc in documents:
            vector_data = {
                "id": doc.id,
                "values": doc.vector.tolist(),
                "metadata": doc.metadata.copy()
            }
            if doc.text:
                vector_data["metadata"]["text"] = doc.text
            vectors.append(vector_data)
        
        index.upsert(vectors)
        logger.info(f"Pinecone に {len(documents)} 件の文書を追加しました")
    
    def search(
        self,
        collection_name: str,
        query_vector: List[float],
        top_k: int = 10,
        filter_dict: Optional[Dict[str, Any]] = None
    ) -> List[SearchResult]:
        """Pineconeでベクトル検索"""
        if not self.is_connected or self.pinecone is None:
            raise VectorDBError("Pineconeに接続されていません")

        index = self.pinecone.Index(collection_name)
        
        response = index.query(
            vector=query_vector,
            top_k=top_k,
            filter=filter_dict,
            include_metadata=True
        )
        
        results = []
        for match in response.matches:
            metadata = match.metadata or {}
            text = metadata.pop("text", None)
            
            result = SearchResult(
                id=match.id,
                score=float(match.score),
                metadata=metadata,
                text=text
            )
            results.append(result)
        
        return results
    
    def delete(self, collection_name: str, ids: List[str]) -> None:
        """Pinecone文書を削除"""
        if not self.is_connected or self.pinecone is None:
            raise VectorDBError("Pineconeに接続されていません")

        index = self.pinecone.Index(collection_name)
        index.delete(ids=ids)
        logger.info(f"Pinecone から {len(ids)} 件の文書を削除しました")


class ChromaProvider(VectorDBProvider):
    """Chromaプロバイダー"""
    
    def __init__(self, persist_directory: Optional[str] = None):
        super().__init__("chroma")
        self.persist_directory = persist_directory or "./chroma_data"
        self.client: Optional[Any] = None
    
    def connect(self, **kwargs) -> None:
        """Chromaに接続"""
        try:
            import chromadb
            from chromadb.config import Settings
            
            self.client = chromadb.Client(Settings(
                chroma_db_impl="duckdb+parquet",
                persist_directory=self.persist_directory
            ))
            self.is_connected = True
            logger.info("Chromaに接続しました")
            
        except ImportError:
            raise VectorDBError("chromadbライブラリがインストールされていません")
        except Exception as e:
            raise VectorDBError(f"Chroma接続エラー: {e}")
    
    def disconnect(self) -> None:
        """Chromaから切断"""
        self.is_connected = False
        logger.info("Chromaから切断しました")
    
    def create_collection(self, name: str, dimension: int, **kwargs) -> None:
        """Chromaコレクションを作成"""
        if not self.is_connected or self.client is None:
            raise VectorDBError("Chromaに接続されていません")

        try:
            self.client.create_collection(name)
            logger.info(f"Chromaコレクションを作成しました: {name}")
        except Exception as e:
            if "already exists" in str(e):
                logger.info(f"Chromaコレクションは既に存在します: {name}")
            else:
                raise VectorDBError(f"Chromaコレクション作成エラー: {e}")
    
    def delete_collection(self, name: str) -> None:
        """Chromaコレクションを削除"""
        if not self.is_connected or self.client is None:
            raise VectorDBError("Chromaに接続されていません")

        try:
            self.client.delete_collection(name)
            logger.info(f"Chromaコレクションを削除しました: {name}")
        except Exception as e:
            logger.warning(f"Chromaコレクション削除エラー: {e}")
    
    def upsert(self, collection_name: str, documents: List[VectorDocument]) -> None:
        """Chromaに文書を挿入"""
        if not self.is_connected or self.client is None:
            raise VectorDBError("Chromaに接続されていません")

        collection = self.client.get_collection(collection_name)
        
        ids = [doc.id for doc in documents]
        embeddings = [doc.vector.tolist() for doc in documents]
        metadatas = []
        
        for doc in documents:
            metadata = doc.metadata.copy()
            if doc.text:
                metadata["text"] = doc.text
            metadatas.append(metadata)
        
        collection.add(
            ids=ids,
            embeddings=embeddings,
            metadatas=metadatas
        )
        
        logger.info(f"Chroma に {len(documents)} 件の文書を追加しました")
    
    def search(
        self,
        collection_name: str,
        query_vector: List[float],
        top_k: int = 10,
        filter_dict: Optional[Dict[str, Any]] = None
    ) -> List[SearchResult]:
        """Chromaでベクトル検索"""
        if not self.is_connected or self.client is None:
            raise VectorDBError("Chromaに接続されていません")

        collection = self.client.get_collection(collection_name)
        
        results = collection.query(
            query_embeddings=[query_vector],
            n_results=top_k,
            where=filter_dict
        )
        
        search_results = []
        for i in range(len(results["ids"][0])):
            metadata = results["metadatas"][0][i] or {}
            text = metadata.pop("text", None)
            
            result = SearchResult(
                id=results["ids"][0][i],
                score=float(results["distances"][0][i]),
                metadata=metadata,
                text=text
            )
            search_results.append(result)
        
        return search_results
    
    def delete(self, collection_name: str, ids: List[str]) -> None:
        """Chroma文書を削除"""
        if not self.is_connected or self.client is None:
            raise VectorDBError("Chromaに接続されていません")

        collection = self.client.get_collection(collection_name)
        collection.delete(ids=ids)
        logger.info(f"Chroma から {len(ids)} 件の文書を削除しました")


class VectorDBManager:
    """ベクトルデータベース管理クラス"""
    
    def __init__(self):
        self.providers: Dict[str, VectorDBProvider] = {}
        self.default_provider = "faiss"
    
    def register_provider(self, name: str, provider: VectorDBProvider) -> None:
        """プロバイダーを登録"""
        self.providers[name] = provider
        logger.info(f"ベクトルDBプロバイダーを登録しました: {name}")
    
    def set_default_provider(self, name: str) -> None:
        """デフォルトプロバイダーを設定"""
        if name not in self.providers:
            raise ValueError(f"プロバイダー '{name}' が登録されていません")
        self.default_provider = name
        logger.info(f"デフォルトベクトルDBプロバイダーを設定しました: {name}")
    
    def get_provider(self, name: Optional[str] = None) -> VectorDBProvider:
        """プロバイダーを取得"""
        provider_name = name or self.default_provider
        
        if provider_name not in self.providers:
            raise ValueError(f"プロバイダー '{provider_name}' が登録されていません")
        
        return self.providers[provider_name]


# グローバルマネージャー
_vector_db_manager: Optional[VectorDBManager] = None


def get_vector_db_manager() -> VectorDBManager:
    """グローバルベクトルDBマネージャーを取得"""
    global _vector_db_manager
    if _vector_db_manager is None:
        _vector_db_manager = VectorDBManager()
        # デフォルトプロバイダーを登録
        _vector_db_manager.register_provider("faiss", FAISSProvider())
    return _vector_db_manager


# 便利関数
def create_collection(
    name: str,
    dimension: int,
    provider: Optional[str] = None,
    **kwargs
) -> None:
    """コレクション作成の便利関数"""
    manager = get_vector_db_manager()
    db_provider = manager.get_provider(provider)
    
    if not db_provider.is_connected:
        db_provider.connect()
    
    db_provider.create_collection(name, dimension, **kwargs)


def upsert_documents(
    collection_name: str,
    documents: List[VectorDocument],
    provider: Optional[str] = None
) -> None:
    """文書挿入の便利関数"""
    manager = get_vector_db_manager()
    db_provider = manager.get_provider(provider)
    
    if not db_provider.is_connected:
        db_provider.connect()
    
    db_provider.upsert(collection_name, documents)


def search_vectors(
    collection_name: str,
    query_vector: List[float],
    top_k: int = 10,
    provider: Optional[str] = None,
    filter_dict: Optional[Dict[str, Any]] = None
) -> List[SearchResult]:
    """ベクトル検索の便利関数"""
    manager = get_vector_db_manager()
    db_provider = manager.get_provider(provider)
    
    if not db_provider.is_connected:
        db_provider.connect()
    
    return db_provider.search(collection_name, query_vector, top_k, filter_dict)
