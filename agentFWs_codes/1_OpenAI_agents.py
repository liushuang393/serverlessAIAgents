# OpenAI Agents SDK + MCP 示例
import asyncio

from mcp_client import MCPClient
from openai_agents import create_agent


async def setup_openai_agent_with_mcp():
    mcp = MCPClient()
    await mcp.connect_stdio(command="npx", args=["@tavily/mcp-server"])
    tools = await mcp.get_tools()
    agent = create_agent(
        name="検索アシスタント", instructions="你是一个专业的搜索助手", tools=tools, model="gpt-4"
    )
    return agent, mcp


async def main():
    agent, mcp = await setup_openai_agent_with_mcp()
    response = await agent.run("2025年のAI分野の重要な進展を検索して")
    print(response.content)
    await mcp.disconnect()


if __name__ == "__main__":
    asyncio.run(main())
