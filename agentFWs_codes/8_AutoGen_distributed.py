# AutoGen 分布式 MCP 示例
import asyncio

from autogen_agentchat.agents import AssistantAgent
from autogen_agentchat.teams import RoundRobinGroupChat
from llama_index.llms.openai import OpenAI
from mcp_client import AutoGenMCPClient, SseServerParams


async def run():
    mcp_search = AutoGenMCPClient()
    await mcp_search.connect_server(
        "remote_search",
        SseServerParams(
            url="http://localhost:8000/mcp", headers={"Authorization": "Bearer token"}
        ),
    )
    searcher = AssistantAgent(
        name="搜索专家",
        model_client=OpenAI(model="gpt-4"),
        tools=await mcp_search.get_tools("remote_search"),
        system_message="你负责搜索",
    )
    analyst = AssistantAgent(
        name="本地分析师", model_client=OpenAI(model="gpt-4"), system_message="你负责分析"
    )
    team = RoundRobinGroupChat([searcher, analyst])
    result = await team.run(task="分析云计算市场发展")
    print("输出:", result.messages[-1].content)
    await mcp_search.disconnect_all()


if __name__ == "__main__":
    asyncio.run(run())
