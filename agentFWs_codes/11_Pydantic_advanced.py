# Pydantic AI：多步骤分析
import asyncio

from mcp_client import MCPServerHTTP
from pydantic import BaseModel, Field
from pydantic_ai import Agent


class AnalysisStep(BaseModel):
    step_name: str
    description: str
    tools_used: list[str]
    findings: str
    confidence: float = Field(ge=0.0, le=1.0)


class MultiStepAnalysis(BaseModel):
    query: str
    analysis_steps: list[AnalysisStep]
    final_answer: str
    overall_confidence: float = Field(ge=0.0, le=1.0)
    sources_cited: int


async def main():
    mcp = MCPServerHTTP(
        base_url="http://localhost:8000/mcp", headers={"Authorization": "Bearer token"}
    )
    tools = await mcp.get_tools()
    agent = Agent(
        model="openai:gpt-4",
        tools=tools,
        system_prompt="多步骤分析专家",
        result_type=MultiStepAnalysis,
    )
    result = await agent.run("分析区块链在供应链中的前景和挑战")
    print(result.data)
    await mcp.disconnect()


if __name__ == "__main__":
    asyncio.run(main())
