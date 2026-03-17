
import asyncio
import logging
from shared.services.unified_rag import UnifiedRAGService, RAGPattern
from infrastructure.llm.providers import EmbeddingProvider, VectorDBProvider, LLMProvider, get_llm
from infrastructure.llm.providers.embedding_provider import MockEmbeddingProvider
from infrastructure.llm.providers.vectordb_provider import MockVectorDBProvider

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("verify_patterns")

async def verify_patterns():
    logger.info("Starting RAG patterns verification...")
    
    # Mock providers for stable testing
    embedding = MockEmbeddingProvider()
    vector_db = MockVectorDBProvider()
    llm = get_llm() # Using real LLM for query rewriting if possible, or mock it if needed
    
    # Initialize service with Advanced pattern
    service = UnifiedRAGService(
        collection_name="test_patterns",
        embedding_provider=embedding,
        vector_db=vector_db,
        llm=llm,
        pattern=RAGPattern.ADVANCED
    )
    
    # Add some test data
    docs = [
        "AgentFlow supports multiple RAG patterns including Basic, Hybrid, and Advanced.",
        "Advanced RAG pattern uses query rewriting and reranking to improve accuracy."
    ]
    await service.add_documents(docs)
    logger.info("Documents added.")
    
    # Test query with Advanced pattern (should trigger rewrite)
    query = "How to improve RAG accuracy?"
    logger.info(f"Querying with ADVANCED pattern: {query}")
    answer = await service.query(query)
    logger.info(f"Answer: {answer}")
    
    # Verify citations are present
    if "[Source" in answer:
        logger.info("Citations verified.")
    else:
        logger.warning("Citations missing in answer.")

    # Test Basic pattern
    service.pattern = RAGPattern.BASIC
    logger.info(f"Querying with BASIC pattern: {query}")
    answer_basic = await service.query(query)
    logger.info(f"Answer (Basic): {answer_basic}")

    await service.close()
    logger.info("Verification completed.")

if __name__ == "__main__":
    asyncio.run(verify_patterns())
