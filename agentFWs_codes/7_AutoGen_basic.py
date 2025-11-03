# AutoGen 多 Agent 协作基础
import asyncio

from autogen_agentchat.agents import AssistantAgent
from autogen_agentchat.teams import RoundRobinGroupChat
from autogen_agentchat.ui import Console
from llama_index.llms.openai import OpenAI
from mcp_client import StdioServerParams
from mcp_client.integrations.autogen import AutoGenMCPClient


async def create_autogen_agents():
    mcp = AutoGenMCPClient()
    await mcp.connect_server(
        "search_tools", StdioServerParams(command="npx", args=["@tavily/mcp-server"])
    )
    tools = await mcp.get_tools("search_tools")
    researcher = AssistantAgent(
        name="研究员",
        model_client=OpenAI(model="gpt-4"),
        tools=tools,
        system_message="你是研究员",
    )
    analyst = AssistantAgent(
        name="分析师", model_client=OpenAI(model="gpt-4"), system_message="你是分析师"
    )
    reporter = AssistantAgent(
        name="报告员", model_client=OpenAI(model="gpt-4"), system_message="你是报告员"
    )
    return researcher, analyst, reporter, mcp


async def run():
    r, a, p, mcp = await create_autogen_agents()
    team = RoundRobinGroupChat([r, a, p])
    result = await Console(team.run_stream(task="请研究分析2024年区块链发展趋势"))
    print("协作结果:", result.messages[-1].content)
    await mcp.disconnect_all()


if __name__ == "__main__":
    asyncio.run(run())
