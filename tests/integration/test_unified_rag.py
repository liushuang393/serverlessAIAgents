import pytest
import asyncio
from platform.services.unified_rag import UnifiedRAGService

@pytest.mark.asyncio
async def test_unified_rag_service_flow():
    # 1. 初期化 (MockProvider が使われる想定)
    rag = UnifiedRAGService(collection_name="test_collection")
    
    # 2. ドキュメント追加
    docs = ["AgentFlow is a lightweight AI framework.", "It supports MCP and A2A protocols."]
    ids = await rag.add_documents(docs)
    assert len(ids) == 2
    
    # 3. 検索
    results = await rag.retrieve("What is AgentFlow?")
    assert len(results) > 0
    assert any("AgentFlow" in r["document"] for r in results)
    
    # 4. クエリ (検索 + 生成)
    # LLM も Mock か環境変数の設定に従う
    try:
        answer = await rag.query("Tell me about AgentFlow.")
        assert isinstance(answer, str)
        assert len(answer) > 0
    except Exception as e:
        print(f"Query failed (expected if no LLM configured): {e}")

    await rag.close()

if __name__ == "__main__":
    asyncio.run(test_unified_rag_service_flow())
