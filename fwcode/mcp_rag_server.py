import hashlib
import json
import logging
from pathlib import Path
from typing import Any, Dict, List, Optional

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from llama_index.core import Document, Settings, VectorStoreIndex
from llama_index.core.node_parser import SentenceSplitter
from llama_index.embeddings.openai import OpenAIEmbedding
from llama_index.llms.openai import OpenAI
from llama_index.readers.file import CSVReader, PDFReader
from pydantic import BaseModel

# ログ設定
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# Pydanticモデル定義
class CreateIndexRequest(BaseModel):
    file_path: str
    index_name: str
    chunk_size: Optional[int] = None
    chunk_overlap: Optional[int] = None
    force_recreate: bool = False


class QueryRequest(BaseModel):
    index_name: str
    query: str
    top_k: int = 5


class SummaryRequest(BaseModel):
    index_name: str
    summary_type: str = "brief"


class RAGServer:
    def __init__(self) -> None:
        self.app = FastAPI(title="RAG-Server", description="RAGシステムAPI")
        self.indices: Dict[str, VectorStoreIndex] = {}
        self.document_cache: Dict[str, List[Document]] = {}
        self.config = self._load_config()

        # CORS設定
        self.app.add_middleware(
            CORSMiddleware,
            allow_origins=["*"],
            allow_credentials=True,
            allow_methods=["*"],
            allow_headers=["*"],
        )

        # LlamaIndex設定を初期化
        Settings.llm = OpenAI(model="gpt-4o-mini")
        Settings.embed_model = OpenAIEmbedding(model="text-embedding-3-small")

        self._register_routes()

    def _load_config(self) -> Dict[str, Any]:
        """設定ファイルを読み込み"""
        config_path = Path("doc_config.json")
        if config_path.exists():
            with open(config_path, "r", encoding="utf-8") as f:
                config: Dict[str, Any] = json.load(f)
                return config
        return {"default_chunk_size": 1024, "default_chunk_overlap": 200}

    def _get_document_hash(
        self, file_path: str, chunk_size: int, chunk_overlap: int
    ) -> str:
        """ドキュメント+パラメータのハッシュ値を計算、キャッシュ判定に使用"""
        file_stat = Path(file_path).stat()
        content = f"{file_path}_{file_stat.st_size}_{file_stat.st_mtime}_{chunk_size}_{chunk_overlap}"
        return hashlib.md5(content.encode()).hexdigest()

    def _parse_documents(
        self, file_path: str, chunk_size: int, chunk_overlap: int
    ) -> List[Document]:
        """ドキュメントを解析、スマートキャッシュをサポート"""
        doc_hash = self._get_document_hash(file_path, chunk_size, chunk_overlap)

        # キャッシュをチェック
        if doc_hash in self.document_cache:
            logger.info(f"キャッシュされたドキュメントを使用: {file_path}")
            return self.document_cache[doc_hash]

        # ファイルタイプに応じてパーサーを選択
        file_path_obj = Path(file_path)
        if file_path_obj.suffix.lower() == ".pdf":
            reader = PDFReader()
        elif file_path_obj.suffix.lower() == ".csv":
            reader = CSVReader()
        else:
            raise ValueError(f"サポートされていないファイル形式: {file_path_obj.suffix}")

        # ドキュメントを解析
        documents = reader.load_data(file_path)

        # チャンク処理
        splitter = SentenceSplitter(chunk_size=chunk_size, chunk_overlap=chunk_overlap)

        nodes = splitter.get_nodes_from_documents(documents)
        processed_docs = [
            Document(text=node.text, metadata=node.metadata) for node in nodes
        ]

        # 結果をキャッシュ
        self.document_cache[doc_hash] = processed_docs
        logger.info(f"ドキュメント解析完了: {file_path}, チャンク数: {len(processed_docs)}")

        return processed_docs

    def _register_routes(self) -> None:
        """FastAPIルートを登録"""

        @self.app.post("/create_index")
        async def create_vector_index(request: CreateIndexRequest) -> Dict[str, str]:
            """
            ベクトルインデックスを作成
            """
            try:
                # デフォルト設定を使用
                chunk_size = request.chunk_size or self.config["default_chunk_size"]
                chunk_overlap = (
                    request.chunk_overlap or self.config["default_chunk_overlap"]
                )

                # 再構築が必要かチェック
                if not request.force_recreate and request.index_name in self.indices:
                    raise HTTPException(
                        status_code=400,
                        detail=f"インデックス {request.index_name} は既に存在します。force_recreate=True で強制再構築してください",
                    )

                # ドキュメントを解析
                documents = self._parse_documents(
                    request.file_path, chunk_size, chunk_overlap
                )

                # インデックスを作成
                index = VectorStoreIndex.from_documents(documents)
                self.indices[request.index_name] = index

                return {
                    "status": "success",
                    "message": f"インデックス {request.index_name} の作成に成功しました。{len(documents)} 個のドキュメントブロックを含みます",
                }

            except Exception as e:
                logger.error(f"インデックス作成失敗: {str(e)}")
                raise HTTPException(status_code=500, detail=f"インデックス作成失敗: {str(e)}")

        @self.app.post("/query")
        async def query_document(request: QueryRequest) -> Dict[str, Any]:
            """
            ドキュメントを検索
            """
            try:
                if request.index_name not in self.indices:
                    raise HTTPException(
                        status_code=404,
                        detail=f"インデックス {request.index_name} が存在しません。まずインデックスを作成してください",
                    )

                # クエリを実行
                query_engine = self.indices[request.index_name].as_query_engine(
                    similarity_top_k=request.top_k
                )
                response = query_engine.query(request.query)

                return {
                    "status": "success",
                    "query": request.query,
                    "response": str(response),
                }

            except Exception as e:
                logger.error(f"検索失敗: {str(e)}")
                raise HTTPException(status_code=500, detail=f"検索失敗: {str(e)}")

        @self.app.post("/summary")
        async def get_document_summary(request: SummaryRequest) -> Dict[str, Any]:
            """
            ドキュメント要約を取得
            """
            try:
                if request.index_name not in self.indices:
                    raise HTTPException(
                        status_code=404, detail=f"インデックス {request.index_name} が存在しません"
                    )

                # 要約を生成
                summary_engine = self.indices[request.index_name].as_query_engine()

                if request.summary_type == "brief":
                    query = "このドキュメントの主要な内容を3-5文で要約してください"
                else:
                    query = "このドキュメントの核心的な観点と重要な情報を詳細に要約してください"

                response = summary_engine.query(query)
                return {
                    "status": "success",
                    "summary_type": request.summary_type,
                    "summary": str(response),
                }

            except Exception as e:
                logger.error(f"要約生成失敗: {str(e)}")
                raise HTTPException(status_code=500, detail=f"要約生成失敗: {str(e)}")

        @self.app.get("/indices")
        async def list_indices() -> Dict[str, Any]:
            """利用可能なすべてのインデックスをリスト表示"""
            if not self.indices:
                return {
                    "status": "success",
                    "message": "現在利用可能なインデックスがありません",
                    "indices": [],
                }

            index_info = []
            for name, index in self.indices.items():
                doc_count = len(index.docstore.docs)
                index_info.append({"name": name, "document_count": doc_count})

            return {
                "status": "success",
                "message": "利用可能なインデックス",
                "indices": index_info,
            }

        @self.app.get("/health")
        async def health_check() -> Dict[str, str]:
            """ヘルスチェックエンドポイント"""
            return {"status": "healthy", "message": "RAGサーバーは正常に動作しています"}


# サーバー起動スクリプト
def create_app() -> FastAPI:
    """FastAPIアプリケーションを作成"""
    server = RAGServer()
    return server.app


app = create_app()

if __name__ == "__main__":
    import uvicorn

    uvicorn.run(app, host="0.0.0.0", port=8000)
