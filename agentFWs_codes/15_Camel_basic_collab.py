# Camel 协作基础
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
    researcher = ChatAgent(
        system_message=BaseMessage.make_assistant_message("研究员", "负责研究"),
        model_type=ModelType.GPT_4,
        tools=tools,
    )
    strategist = ChatAgent(
        system_message=BaseMessage.make_assistant_message("策略师", "负责策略"),
        model_type=ModelType.GPT_4,
    )
    executor = ChatAgent(
        system_message=BaseMessage.make_assistant_message("执行官", "负责执行"),
        model_type=ModelType.GPT_4,
    )
    prompt = BaseMessage.make_user_message("项目经理", "AI 在零售业的商业策略")
    res1 = await researcher.step(prompt)
    res2 = await strategist.step(
        BaseMessage.make_user_message("项目经理", res1.msg.content)
    )
    res3 = await executor.step(BaseMessage.make_user_message("项目经理", res2.msg.content))
    print("研究:", res1.msg.content)
    print("策略:", res2.msg.content)
    print("执行:", res3.msg.content)
    await mcp.disconnect()


if __name__ == "__main__":
    asyncio.run(main())
