# SmolAgents 批量任务
import asyncio

from mcp_client import MCPClient
from smolagents import CodeAgent, ToolCollection


async def main():
    mcp = MCPClient()
    await mcp.connect_stdio(command="npx", args=["@tavily/mcp-server"])
    tools = ToolCollection.from_mcp(mcp)
    agent = CodeAgent(tools=tools, model="gpt-4", system_prompt="批量任务处理专家")
    queries = ["AI in finance", "区块链发展", "云计算安全", "物联网趋势"]
    result = await agent.run(f"批量搜索并总结：{queries}")
    print("代码:", result.code)
    print("输出:", result.output)
    await mcp.disconnect()


if __name__ == "__main__":
    asyncio.run(main())
