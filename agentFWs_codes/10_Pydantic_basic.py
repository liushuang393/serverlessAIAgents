# Pydantic AI：结构化输出基础
import asyncio

from mcp_client import MCPServerStdio
from pydantic import BaseModel, Field
from pydantic_ai import Agent


class SearchResult(BaseModel):
    title: str
    content: str
    url: str
    relevance_score: float = Field(ge=0.0, le=1.0)


class ResearchReport(BaseModel):
    topic: str
    executive_summary: str
    key_findings: list[SearchResult]
    conclusion: str
    confidence_level: float = Field(ge=0.0, le=1.0)


async def main():
    mcp = MCPServerStdio(command="npx", args=["@tavily/mcp-server"])
    tools = await mcp.get_tools()
    agent = Agent(
        model="openai:gpt-4",
        tools=tools,
        system_prompt="你是研究助手，输出结构化报告",
        result_type=ResearchReport,
    )
    result = await agent.run("研究AI在教育领域的应用现状")
    print(result.data)
    await mcp.disconnect()


if __name__ == "__main__":
    asyncio.run(main())
