# LlamaIndex 基础 Agent + MCP 示例
import asyncio

from llama_index.agent.openai import OpenAIAgent
from llama_index.core import Settings
from llama_index.llms.openai import OpenAI
from llama_index.tools.mcp import McpToolSpec
from mcp_client import BasicMCPClient


async def create():
    Settings.llm = OpenAI(model="gpt-4")
    mcp = BasicMCPClient()
    await mcp.connect_stdio(command="npx", args=["@tavily/mcp-server"])
    tools = McpToolSpec(mcp).to_tool_list()
    agent = OpenAIAgent.from_tools(
        tools=tools, system_prompt="你是专业AI助手，具备搜索分析能力", verbose=True
    )
    return agent, mcp


async def run():
    agent, mcp = await create()
    resp = await agent.achat("2024年大语言模型发展趋势总结")
    print(resp.response)
    await mcp.disconnect()


if __name__ == "__main__":
    asyncio.run(run())
