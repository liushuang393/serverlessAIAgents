# SmolAgents 基础示例
import asyncio

from mcp_client import MCPClient
from smolagents import CodeAgent, ToolCollection


async def main():
    mcp = MCPClient()
    await mcp.connect_stdio(command="npx", args=["@tavily/mcp-server"])
    tools = ToolCollection.from_mcp(mcp)
    agent = CodeAgent(tools=tools, model="gpt-4", system_prompt="你是代码生成助手")
    result = await agent.run("搜索 Python 异步最佳实践，并提取要点")
    print("代码:", result.code)
    print("输出:", result.output)
    await mcp.disconnect()


if __name__ == "__main__":
    asyncio.run(main())
