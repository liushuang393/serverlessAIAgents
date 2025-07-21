#!/usr/bin/env python3
"""
RAGシステムの簡単なテスト（依存関係なし）
"""

import hashlib
import json
from pathlib import Path
from typing import Any, Dict, List, Optional


class SimpleRAGTest:
    """依存関係なしでRAGシステムの基本機能をテスト"""

    def __init__(self):
        self.indices = {}
        self.document_cache = {}
        self.config = self._load_config()

    def _load_config(self) -> Dict[str, Any]:
        """設定ファイルを読み込み"""
        config_path = Path("doc_config.json")
        if config_path.exists():
            with open(config_path, "r", encoding="utf-8") as f:
                return json.load(f)
        return {"default_chunk_size": 1024, "default_chunk_overlap": 200}

    def _get_document_hash(
        self, file_path: str, chunk_size: int, chunk_overlap: int
    ) -> str:
        """ドキュメント+パラメータのハッシュ値を計算"""
        try:
            file_stat = Path(file_path).stat()
            content = f"{file_path}_{file_stat.st_size}_{file_stat.st_mtime}_{chunk_size}_{chunk_overlap}"
            return hashlib.md5(content.encode()).hexdigest()
        except Exception as e:
            return hashlib.md5(
                f"{file_path}_{chunk_size}_{chunk_overlap}".encode()
            ).hexdigest()

    def _parse_documents(
        self, file_path: str, chunk_size: int, chunk_overlap: int
    ) -> List[str]:
        """ドキュメントを解析（簡略版）"""
        doc_hash = self._get_document_hash(file_path, chunk_size, chunk_overlap)

        # キャッシュをチェック
        if doc_hash in self.document_cache:
            print(f"キャッシュされたドキュメントを使用: {file_path}")
            return self.document_cache[doc_hash]

        # ファイルを読み込み
        try:
            with open(file_path, "r", encoding="utf-8") as f:
                content = f.read()

            # 簡単なチャンク分割
            chunks = []
            words = content.split()

            for i in range(0, len(words), chunk_size // 10):  # 簡略化
                chunk = " ".join(words[i : i + chunk_size // 10])
                if chunk.strip():
                    chunks.append(chunk)

            # キャッシュに保存
            self.document_cache[doc_hash] = chunks
            print(f"ドキュメント解析完了: {file_path}, チャンク数: {len(chunks)}")

            return chunks

        except Exception as e:
            print(f"ファイル読み込みエラー: {e}")
            return []

    def create_vector_index(
        self,
        file_path: str,
        index_name: str,
        chunk_size: Optional[int] = None,
        chunk_overlap: Optional[int] = None,
        force_recreate: bool = False,
    ) -> Dict[str, str]:
        """ベクトルインデックスを作成（簡略版）"""
        try:
            # デフォルト設定を使用
            chunk_size = chunk_size or self.config["default_chunk_size"]
            chunk_overlap = chunk_overlap or self.config["default_chunk_overlap"]

            # 再構築が必要かチェック
            if not force_recreate and index_name in self.indices:
                return {
                    "status": "error",
                    "message": f"インデックス {index_name} は既に存在します。force_recreate=True で強制再構築してください",
                }

            # ドキュメントを解析
            documents = self._parse_documents(file_path, chunk_size, chunk_overlap)

            if not documents:
                return {"status": "error", "message": f"ドキュメントの解析に失敗しました: {file_path}"}

            # インデックスを作成（簡略版）
            self.indices[index_name] = {
                "documents": documents,
                "file_path": file_path,
                "chunk_size": chunk_size,
                "chunk_overlap": chunk_overlap,
            }

            return {
                "status": "success",
                "message": f"インデックス {index_name} の作成に成功しました。{len(documents)} 個のドキュメントブロックを含みます",
            }

        except Exception as e:
            return {"status": "error", "message": f"インデックス作成失敗: {str(e)}"}

    def query_document(
        self, index_name: str, query: str, top_k: int = 5
    ) -> Dict[str, Any]:
        """ドキュメントを検索（簡略版）"""
        try:
            if index_name not in self.indices:
                return {
                    "status": "error",
                    "message": f"インデックス {index_name} が存在しません。まずインデックスを作成してください",
                }

            # 簡単なキーワード検索
            documents = self.indices[index_name]["documents"]
            query_words = query.lower().split()

            results = []
            for i, doc in enumerate(documents):
                score = 0
                doc_lower = doc.lower()
                for word in query_words:
                    score += doc_lower.count(word)

                if score > 0:
                    results.append(
                        {
                            "chunk_id": i,
                            "content": doc[:200] + "..." if len(doc) > 200 else doc,
                            "score": score,
                        }
                    )

            # スコア順にソート
            results.sort(key=lambda x: x["score"], reverse=True)
            results = results[:top_k]

            return {
                "status": "success",
                "query": query,
                "results": results,
                "total_found": len(results),
            }

        except Exception as e:
            return {"status": "error", "message": f"検索失敗: {str(e)}"}

    def list_indices(self) -> Dict[str, Any]:
        """利用可能なインデックスをリスト表示"""
        if not self.indices:
            return {
                "status": "success",
                "message": "現在利用可能なインデックスがありません",
                "indices": [],
            }

        index_info = []
        for name, index_data in self.indices.items():
            index_info.append(
                {
                    "name": name,
                    "document_count": len(index_data["documents"]),
                    "file_path": index_data["file_path"],
                }
            )

        return {"status": "success", "message": "利用可能なインデックス", "indices": index_info}


def main() -> None:
    """メインテスト関数"""
    print("🚀 簡単なRAGシステムテストを開始します\n")

    # RAGシステムを初期化
    rag = SimpleRAGTest()

    # テストドキュメントのパス
    test_docs = ["test_documents/ai_report.txt", "test_documents/market_analysis.txt"]

    # 各ドキュメントでインデックスを作成
    for i, doc_path in enumerate(test_docs):
        if Path(doc_path).exists():
            index_name = f"test-index-{i+1}"
            print(f"=== インデックス作成テスト: {index_name} ===")
            result = rag.create_vector_index(doc_path, index_name)
            print(f"結果: {result}")
            print()
        else:
            print(f"⚠️  テストドキュメントが見つかりません: {doc_path}")

    # インデックスリストを表示
    print("=== インデックスリスト ===")
    indices_result = rag.list_indices()
    print(f"結果: {indices_result}")
    print()

    # 検索テスト
    if rag.indices:
        index_name = list(rag.indices.keys())[0]
        test_queries = ["人工知能", "AI技術", "市場分析", "成長分野"]

        for query in test_queries:
            print(f"=== 検索テスト: '{query}' ===")
            search_result = rag.query_document(index_name, query, top_k=2)
            print(f"結果: {search_result}")
            print()

    print("✅ 簡単なRAGシステムテストが完了しました！")


if __name__ == "__main__":
    main()
