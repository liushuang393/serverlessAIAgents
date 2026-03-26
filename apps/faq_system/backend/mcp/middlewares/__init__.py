"""Pipeline ミドルウェア パッケージ.

RetrievalPipeline に追加可能なミドルウェア:
- reranker: 検索結果のリランキング（BM25/Cohere/CrossEncoder/LLM）
- dedup: 重複排除
- score_normalize: スコア正規化
"""
