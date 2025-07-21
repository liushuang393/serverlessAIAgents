# AutoGen 自定义 MCP 工具
import asyncio

from autogen_agentchat.agents import AssistantAgent
from autogen_agentchat.base import Tool
from llama_index.llms.openai import OpenAI
from mcp_client import AutoGenMCPClient, StdioServerParams


class CustomSearchTool(Tool):
    name = "tavily_search"
    description = "自定义搜索工具"

    def __init__(self, mcp):
        self.mcp = mcp
        super().__init__()

    async def run(self, query: str):
        return await self.mcp.call_tool("tavily_search", {"query": query})

    @property
    def schema(self):
        return {
            "type": "function",
            "function": {
                "name": self.name,
                "description": self.description,
                "parameters": {
                    "type": "object",
                    "properties": {"query": {"type": "string"}},
                    "required": ["query"],
                },
            },
        }


async def run():
    mcp = AutoGenMCPClient()
    await mcp.connect_server(
        "search", StdioServerParams(command="npx", args=["@tavily/mcp-server"])
    )
    tool = CustomSearchTool(mcp)
    agent = AssistantAgent(
        name="多工具专家",
        model_client=OpenAI(model="gpt-4"),
        tools=[tool],
        system_message="你可用多工具",
    )
    from autogen_agentchat.teams import Swarm

    team = Swarm([agent])
    result = await team.run("查找Python asyncio最佳实践")
    print("结果:", result.messages[-1].content)
    await mcp.disconnect_all()


if __name__ == "__main__":
    asyncio.run(run())
