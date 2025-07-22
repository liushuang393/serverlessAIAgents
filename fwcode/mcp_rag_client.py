import asyncio
import json
import logging
from pathlib import Path
from typing import Any, Dict, Optional

import httpx

# ログ設定
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class RAGClient:
    def __init__(self, server_url: str = "http://localhost:8000"):
        self.server_url = server_url
        self.config = self._load_config()
        self.client = httpx.AsyncClient()

    def _load_config(self) -> Dict[str, Any]:
        """設定を読み込み"""
        config_path = Path("mcp_config.json")
        if config_path.exists():
            with open(config_path, "r", encoding="utf-8") as f:
                config: Dict[str, Any] = json.load(f)
                return config
        return {"available_indices": [], "document_descriptions": {}}

    async def create_index(
        self,
        file_path: str,
        index_name: str,
        chunk_size: Optional[int] = None,
        chunk_overlap: Optional[int] = None,
        force_recreate: bool = False,
    ) -> Dict[str, Any]:
        """ベクトルインデックスを作成"""
        try:
            data = {
                "file_path": file_path,
                "index_name": index_name,
                "chunk_size": chunk_size,
                "chunk_overlap": chunk_overlap,
                "force_recreate": force_recreate,
            }
            response = await self.client.post(
                f"{self.server_url}/create_index", json=data
            )
            response.raise_for_status()
            result: Dict[str, Any] = response.json()
            return result
        except Exception as e:
            logger.error(f"インデックス作成エラー: {str(e)}")
            return {"status": "error", "message": str(e)}

    async def query_document(
        self, index_name: str, query: str, top_k: int = 5
    ) -> Dict[str, Any]:
        """ドキュメントを検索"""
        try:
            data = {"index_name": index_name, "query": query, "top_k": top_k}
            response = await self.client.post(f"{self.server_url}/query", json=data)
            response.raise_for_status()
            result: Dict[str, Any] = response.json()
            return result
        except Exception as e:
            logger.error(f"検索エラー: {str(e)}")
            return {"status": "error", "message": str(e)}

    async def get_summary(
        self, index_name: str, summary_type: str = "brief"
    ) -> Dict[str, Any]:
        """ドキュメント要約を取得"""
        try:
            data = {"index_name": index_name, "summary_type": summary_type}
            response = await self.client.post(f"{self.server_url}/summary", json=data)
            response.raise_for_status()
            result: Dict[str, Any] = response.json()
            return result
        except Exception as e:
            logger.error(f"要約取得エラー: {str(e)}")
            return {"status": "error", "message": str(e)}

    async def list_indices(self) -> Dict[str, Any]:
        """利用可能なインデックスをリスト表示"""
        try:
            response = await self.client.get(f"{self.server_url}/indices")
            response.raise_for_status()
            result: Dict[str, Any] = response.json()
            return result
        except Exception as e:
            logger.error(f"インデックスリスト取得エラー: {str(e)}")
            return {"status": "error", "message": str(e)}

    async def health_check(self) -> Dict[str, Any]:
        """サーバーのヘルスチェック"""
        try:
            response = await self.client.get(f"{self.server_url}/health")
            response.raise_for_status()
            result: Dict[str, Any] = response.json()
            return result
        except Exception as e:
            logger.error(f"ヘルスチェックエラー: {str(e)}")
            return {"status": "error", "message": str(e)}

    async def close(self) -> None:
        """クライアントを閉じる"""
        await self.client.aclose()


# クライアント使用例
async def main() -> None:
    client = RAGClient()

    try:
        # ヘルスチェック
        print("=== サーバーヘルスチェック ===")
        health = await client.health_check()
        print(f"ヘルスチェック結果: {health}")

        # インデックスリスト取得
        print("\n=== 利用可能なインデックス ===")
        indices = await client.list_indices()
        print(f"インデックス: {indices}")

        # サンプルクエリ（実際のファイルがある場合のみ実行）
        sample_operations = [
            {
                "operation": "create_index",
                "description": "PDFファイルからインデックスを作成",
                "params": {
                    "file_path": "sample.pdf",
                    "index_name": "sample-doc",
                    "chunk_size": 1024,
                    "chunk_overlap": 200,
                },
            },
            {
                "operation": "query",
                "description": "ドキュメントを検索",
                "params": {
                    "index_name": "sample-doc",
                    "query": "このドキュメントの主要なポイントは何ですか？",
                    "top_k": 3,
                },
            },
            {
                "operation": "summary",
                "description": "ドキュメント要約を取得",
                "params": {"index_name": "sample-doc", "summary_type": "brief"},
            },
        ]

        print("\n=== サンプル操作（実際のファイルがある場合のみ実行） ===")
        for op in sample_operations:
            print(f"\n{op['description']}:")
            print(f"パラメータ: {op['params']}")
            print("（実際のファイルがないため、この操作はスキップされます）")

    except Exception as e:
        logger.error(f"エラーが発生しました: {str(e)}")
    finally:
        await client.close()


if __name__ == "__main__":
    asyncio.run(main())
