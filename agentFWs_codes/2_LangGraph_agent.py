# LangGraph + MCP 基本用法
import asyncio

from langchain_openai import ChatOpenAI
from langgraph.prebuilt import create_react_agent
from mcp_client import load_mcp_tools


async def create_langgraph_agent():
    tools = await load_mcp_tools(
        server_config={"command": "npx", "args": ["@tavily/mcp-server"]}
    )
    llm = ChatOpenAI(model="gpt-4")
    agent = create_react_agent(llm=llm, tools=tools, state_modifier="你是一个专业的AI助手")
    return agent


async def run():
    agent = await create_langgraph_agent()
    result = await agent.ainvoke(
        {"messages": [("user", "Python 3.12 の新機能をまとめて")]},
        config={"configurable": {"thread_id": "1"}},
    )
    print(result["messages"][-1].content)


if __name__ == "__main__":
    asyncio.run(run())
