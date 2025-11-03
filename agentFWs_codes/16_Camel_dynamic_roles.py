# Camel 动态角色扮演
import asyncio

from camel.messages import BaseMessage
from camel.societies import RolePlaying
from camel.toolkits import MCPToolkit
from camel.types import ModelType
from mcp_client import MCPClient


async def main():
    mcp = MCPClient()
    await mcp.connect_stdio(command="npx", args=["@tavily/mcp-server"])
    tools = MCPToolkit(mcp).get_tools()
    role_play = RolePlaying(
        assistant_role_name="AI专家",
        user_role_name="顾问",
        assistant_agent_kwargs={"model_type": ModelType.GPT_4, "tools": tools},
        user_agent_kwargs={"model_type": ModelType.GPT_4},
    )
    prompt = BaseMessage.make_user_message("项目经理", "制定制造业AI转型战略")
    for i in range(3):
        a_msg, u_msg = await role_play.step(prompt)
        print(f"第{i+1}轮 AI专家: {a_msg.content}")
        print(f"第{i+1}轮 顾问: {u_msg.content}")
        prompt = u_msg
    await mcp.disconnect()


if __name__ == "__main__":
    asyncio.run(main())
