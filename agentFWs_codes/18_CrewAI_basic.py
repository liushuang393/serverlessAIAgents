# CrewAI 基础
import asyncio

from crewai import Agent, Crew, Process, Task
from crewai.tools import BaseTool
from mcp_client import MCPClient


class MCPSearchTool(BaseTool):
    name = "MCP搜索"
    description = "搜索工具"

    def __init__(self, mcp):
        self.mcp = mcp
        super().__init__()

    def _run(self, query: str):
        return asyncio.run(self.mcp.call_tool("tavily_search", {"query": query}))


async def run():
    mcp = MCPClient()
    await mcp.connect_stdio(command="npx", args=["@tavily/mcp-server"])
    tool = MCPSearchTool(mcp)
    researcher = Agent(role="研究员", goal="研究AI医疗", tools=[tool], verbose=True)
    analyst = Agent(role="分析师", goal="分析", verbose=True)
    writer = Agent(role="撰稿人", goal="写报告", verbose=True)
    tasks = [
        Task("研究", "研究报告", researcher),
        Task("分析", "分析报告", analyst),
        Task("写作", "文章", writer),
    ]
    crew = Crew(
        agents=[researcher, analyst, writer],
        tasks=tasks,
        process=Process.sequential,
        verbose=2,
    )
    result = crew.kickoff()
    print(result)
    await mcp.disconnect()


if __name__ == "__main__":
    asyncio.run(run())
