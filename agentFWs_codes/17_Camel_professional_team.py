# Camel 专业团队
import asyncio

from camel.agents import ChatAgent
from camel.messages import BaseMessage
from camel.toolkits import MCPToolkit
from camel.types import ModelType
from mcp_client import MCPClient


async def main():
    mcp = MCPClient()
    await mcp.connect_stdio(command="npx", args=["@tavily/mcp-server"])
    tools = MCPToolkit(mcp).get_tools()
    roles = {"数据科学家": "数据分析", "产品经理": "产品策略", "架构师": "技术架构", "分析师": "业务分析"}
    agents = {
        r: ChatAgent(
            system_message=BaseMessage.make_assistant_message(r, f"{desc}专家"),
            model_type=ModelType.GPT_4,
            tools=tools if r != "架构师" else [],
        )
        for r, desc in roles.items()
    }
    for role, agent in agents.items():
        prompt = BaseMessage.make_user_message("项目经理", f"就 AI 客服项目，{role} 提供建议")
        res = await agent.step(prompt)
        print(f"{role}: {res.msg.content}")
    await mcp.disconnect()


if __name__ == "__main__":
    asyncio.run(main())
