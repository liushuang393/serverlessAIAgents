# LangGraph 多MCP Server 示例
import asyncio

from langchain_openai import ChatOpenAI
from langgraph.mcp import MultiServerMCPClient
from langgraph.prebuilt import create_react_agent


async def create_multi():
    mcp = MultiServerMCPClient()
    await mcp.add_server("search", command="npx", args=["@tavily/mcp-server"])
    await mcp.add_server("db", command="npx", args=["@example/database-mcp-server"])
    tools = await mcp.get_all_tools()
    llm = ChatOpenAI(model="gpt-4")
    return create_react_agent(llm=llm, tools=tools), mcp


if __name__ == "__main__":

    async def run():
        agent, mcp = await create_multi()
        res = await agent.ainvoke(
            {"messages": [("user", "Python async 特性まとめ")]}, config={}
        )
        print(res["messages"][-1].content)
        await mcp.disconnect()

    asyncio.run(run())
