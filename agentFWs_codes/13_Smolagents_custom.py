# SmolAgents 自定义工具
import asyncio

from mcp_client import MCPClient
from smolagents import CodeAgent, Tool


class CustomTool(Tool):
    name = "custom_search"
    description = "自定义搜索"

    def __init__(self, mcp):
        self.mcp = mcp
        super().__init__()

    async def forward(self, query: str):
        return await self.mcp.call_tool("tavily_search", {"query": query})


async def main():
    mcp = MCPClient()
    await mcp.connect_stdio(command="npx", args=["@tavily/mcp-server"])
    tool = CustomTool(mcp)
    agent = CodeAgent(tools=[tool], model="gpt-4", system_prompt="自定义搜索分析师")
    result = await agent.run("使用自定义工具搜索机器学习模型部署")
    print("代码:", result.code)
    print("输出:", result.output)
    await mcp.disconnect()


if __name__ == "__main__":
    asyncio.run(main())
