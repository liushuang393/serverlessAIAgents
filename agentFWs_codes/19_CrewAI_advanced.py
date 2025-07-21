# CrewAI 高级
import asyncio

from crewai import Agent, Crew, Process, Task
from crewai.tools import BaseTool
from mcp_client import MCPClient


class MCPSearchTool(BaseTool):
    name = "MCP搜索"
    description = "高级搜索"

    def __init__(self, mcp):
        self.mcp = mcp
        super().__init__()

    def _run(self, query: str):
        return asyncio.run(self.mcp.call_tool("tavily_search", {"query": query}))


async def run():
    mcp = MCPClient()
    await mcp.connect_stdio(command="npx", args=["@tavily/mcp-server"])
    tool = MCPSearchTool(mcp)
    researcher = Agent(
        role="市场研究", goal="市场研究", tools=[tool], verbose=True, max_iter=3, memory=True
    )
    analyst = Agent(
        role="技术分析", goal="技术分析", tools=[tool], verbose=True, max_iter=3, memory=True
    )
    strategist = Agent(role="战略", goal="战略建议", verbose=True, max_iter=3, memory=True)
    tasks = [
        Task("市场调研", "市场报告", researcher),
        Task("技术分析", "技术报告", analyst),
        Task("战略制定", "战略方案", strategist),
    ]
    crew = Crew(
        agents=[researcher, analyst, strategist],
        tasks=tasks,
        process=Process.sequential,
        verbose=2,
    )
    result = crew.kickoff()
    print(result)
    await mcp.disconnect()


if __name__ == "__main__":
    asyncio.run(run())
