# LlamaIndex RAG + MCP 示例
import asyncio

from llama_index.agent.openai import OpenAIAgent
from llama_index.core import Document, Settings, VectorStoreIndex
from llama_index.core.tools import QueryEngineTool
from llama_index.embeddings.openai import OpenAIEmbedding
from llama_index.llms.openai import OpenAI
from llama_index.tools.mcp import McpToolSpec
from mcp_client import BasicMCPClient


async def create_rag_agent():
    Settings.llm = OpenAI(model="gpt-4")
    Settings.embed_model = OpenAIEmbedding(model="text-embedding-3-large")
    documents = [Document(text=t) for t in ["AI 简介", "机器学习概述", "深度学习技术"]]
    index = VectorStoreIndex.from_documents(documents)
    query_engine = index.as_query_engine()
    mcp = BasicMCPClient()
    await mcp.connect_stdio(command="npx", args=["@tavily/mcp-server"])
    tools = [
        QueryEngineTool.from_defaults(
            query_engine=query_engine, name="kb", description="查询KB"
        )
    ] + McpToolSpec(mcp).to_tool_list()
    agent = OpenAIAgent.from_tools(
        tools=tools, system_prompt="你是AI知识助手，结合内部KB和搜索工具回答", verbose=True
    )
    return agent, mcp


async def run():
    agent, mcp = await create_rag_agent()
    resp = await agent.achat("什么是深度学习？它在2025年有哪些最新进展？")
    print(resp.response)
    await mcp.disconnect()


if __name__ == "__main__":
    asyncio.run(run())
