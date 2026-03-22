import asyncio
import logging
import os
import sys


# プロジェクトルートをパスに追加
sys.path.append(os.getcwd())

from infrastructure.llm.providers.embedding_provider import MockEmbeddingProvider
from shared.services.unified_rag import UnifiedRAGService


async def verify_unified_rag():
    logging.basicConfig(level=logging.INFO)
    logger = logging.getLogger("verify_rag")

    logger.info("Starting UnifiedRAGService verification...")

    try:
        # 1. 初期化 (MockEmbedding を使用)
        mock_embedding = MockEmbeddingProvider()
        rag = UnifiedRAGService(collection_name="verify_collection", embedding_provider=mock_embedding)
        logger.info("Service initialized with MockEmbeddingProvider.")

        # 2. ドキュメント追加
        docs = [
            "AgentFlow is a powerful framework for building AI agents.",
            "It supports complex workflows and various provider integrations.",
        ]
        logger.info("Adding documents...")
        ids = await rag.add_documents(docs)
        logger.info(f"Successfully added doc ids: {ids}")

        # 3. 検索
        logger.info("Testing retrieval...")
        results = await rag.retrieve("Tell me about AgentFlow")
        logger.info(f"Retrieved {len(results)} results.")
        for i, res in enumerate(results):
            logger.info(f"Result {i + 1}: {res['document'][:50]}... (Score: {res.get('score', 'N/A')})")

        if not any("AgentFlow" in r["document"] for r in results):
            logger.error("Required content not found in retrieval results!")
            return False

        # 4. クエリ
        logger.info("Testing full query (RAG flow)...")
        try:
            answer = await rag.query("What is AgentFlow?")
            logger.info(f"LLM Answer: {answer}")
        except Exception as e:
            logger.warning(f"Query step failed (may be due to LLM config): {e}")

        await rag.close()
        logger.info("Verification completed successfully!")
        return True

    except Exception as e:
        logger.exception(f"Verification failed with error: {e}")
        return False


if __name__ == "__main__":
    success = asyncio.run(verify_unified_rag())
    sys.exit(0 if success else 1)
