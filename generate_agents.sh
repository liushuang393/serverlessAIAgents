#!/usr/bin/env bash

# 创建文件夹
mkdir -p agent_codes

echo "🚀 开始生成 19 个 Python 文件..."

# ========== 文件 1 ==========
cat << 'EOF' > agent_codes/1_OpenAI_agents.py
# OpenAI Agents SDK + MCP 示例
import asyncio
from openai_agents import Agent, create_agent
from mcp_client import MCPClient

async def setup_openai_agent_with_mcp():
    mcp = MCPClient()
    await mcp.connect_stdio(command="npx", args=["@tavily/mcp-server"])
    tools = await mcp.get_tools()
    agent = create_agent(name="検索アシスタント", instructions="你是一个专业的搜索助手", tools=tools, model="gpt-4")
    return agent, mcp

async def main():
    agent, mcp = await setup_openai_agent_with_mcp()
    response = await agent.run("2025年のAI分野の重要な進展を検索して")
    print(response.content)
    await mcp.disconnect()

if __name__ == "__main__":
    asyncio.run(main())
EOF

# ========== 文件 2 ==========
cat << 'EOF' > agent_codes/2_LangGraph_agent.py
# LangGraph + MCP 基本用法
import asyncio
from langgraph.prebuilt import create_react_agent
from langchain_openai import ChatOpenAI
from mcp_client import load_mcp_tools

async def create_langgraph_agent():
    tools = await load_mcp_tools(server_config={"command":"npx","args":["@tavily/mcp-server"]})
    llm = ChatOpenAI(model="gpt-4")
    agent = create_react_agent(llm=llm, tools=tools, state_modifier="你是一个专业的AI助手")
    return agent

async def run():
    agent = await create_langgraph_agent()
    result = await agent.ainvoke({"messages":[("user","Python 3.12 の新機能をまとめて")]}, config={"configurable":{"thread_id":"1"}})
    print(result["messages"][-1].content)

if __name__=="__main__":
    asyncio.run(run())
EOF

# ========== 文件 3 ==========
cat << 'EOF' > agent_codes/3_LangGraph_multi_server.py
# LangGraph 多MCP Server 示例
import asyncio
from langgraph.prebuilt import create_react_agent
from langchain_openai import ChatOpenAI
from langgraph.mcp import MultiServerMCPClient

async def create_multi():
    mcp = MultiServerMCPClient()
    await mcp.add_server("search", command="npx", args=["@tavily/mcp-server"])
    await mcp.add_server("db", command="npx", args=["@example/database-mcp-server"])
    tools = await mcp.get_all_tools()
    llm = ChatOpenAI(model="gpt-4")
    return create_react_agent(llm=llm, tools=tools), mcp

if __name__=="__main__":
    async def run():
        agent, mcp = await create_multi()
        res = await agent.ainvoke({"messages":[("user","Python async 特性まとめ")]}, config={})
        print(res["messages"][-1].content)
        await mcp.disconnect()
    asyncio.run(run())
EOF

# ========== 文件 4 ==========
cat << 'EOF' > agent_codes/4_LangGraph_complex_workflow.py
# LangGraph 复杂工作流示例
import asyncio
from langgraph.graph import StateGraph, START, END
from typing import TypedDict
from langchain_openai import ChatOpenAI
from langgraph.prebuilt import create_react_agent
from mcp_client import load_mcp_tools

class AgentState(TypedDict):
    messages: list
    research_data: dict
    final_report: str

async def create_research_workflow():
    tools = await load_mcp_tools(server_config={"command": "npx", "args": ["@tavily/mcp-server"]})
    llm = ChatOpenAI(model="gpt-4")

    async def research_node(state: AgentState):
        agent = create_react_agent(llm=llm, tools=tools, state_modifier="你是研究员")
        res = await agent.ainvoke(
            {"messages": state["messages"] + [("user", f"深入研究：{state['messages'][-1][1]}")]},
            config={"configurable": {"thread_id": "research"}}
        )
        return {"messages": res["messages"], "research_data": {"raw_data": res["messages"][-1].content}, "final_report": ""}

    async def analysis_node(state: AgentState):
        agent = create_react_agent(llm=llm, tools=[], state_modifier="你是分析师")
        prompt = f"基于：{state['research_data']['raw_data']}，请分析关键发现和潜在影响"
        res = await agent.ainvoke({"messages": [("user", prompt)]}, config={"configurable": {"thread_id": "analysis"}})
        state["research_data"]["analysis"] = res["messages"][-1].content
        state["messages"] = res["messages"]
        return state

    async def report_node(state: AgentState):
        agent = create_react_agent(llm=llm, tools=[], state_modifier="你是报告专家")
        prompt = f"请生成报告：原始研究：{state['research_data']['raw_data']}，分析结果：{state['research_data']['analysis']}"
        res = await agent.ainvoke({"messages": [("user", prompt)]}, config={"configurable": {"thread_id": "report"}})
        state["final_report"] = res["messages"][-1].content
        return state

    wf = StateGraph(AgentState)
    wf.add_node("research", research_node)
    wf.add_node("analysis", analysis_node)
    wf.add_node("report", report_node)
    wf.add_edge(START, "research")
    wf.add_edge("research", "analysis")
    wf.add_edge("analysis", "report")
    wf.add_edge("report", END)
    return wf.compile()

async def run():
    app = await create_research_workflow()
    result = await app.ainvoke({"messages":[("user","AI 医疗领域应用前景")],"research_data":{},"final_report":""})
    print("最终报告：", result["final_report"])

if __name__=="__main__":
    asyncio.run(run())
EOF

# ========== 文件 5 ==========
cat << 'EOF' > agent_codes/5_LlamaIndex_basic.py
# LlamaIndex 基础 Agent + MCP 示例
import asyncio
from llama_index.core import Settings
from llama_index.llms.openai import OpenAI
from llama_index.agent.openai import OpenAIAgent
from llama_index.tools.mcp import McpToolSpec
from mcp_client import BasicMCPClient

async def create():
    Settings.llm = OpenAI(model="gpt-4")
    mcp = BasicMCPClient()
    await mcp.connect_stdio(command="npx", args=["@tavily/mcp-server"])
    tools = McpToolSpec(mcp).to_tool_list()
    agent = OpenAIAgent.from_tools(
        tools=tools,
        system_prompt="你是专业AI助手，具备搜索分析能力",
        verbose=True
    )
    return agent, mcp

async def run():
    agent, mcp = await create()
    resp = await agent.achat("2024年大语言模型发展趋势总结")
    print(resp.response)
    await mcp.disconnect()

if __name__=="__main__":
    asyncio.run(run())
EOF

# ========== 文件 6 ==========
cat << 'EOF' > agent_codes/6_LlamaIndex_rag_agent.py
# LlamaIndex RAG + MCP 示例
import asyncio
from llama_index.core import Settings
from llama_index.llms.openai import OpenAI
from llama_index.core import VectorStoreIndex, Document
from llama_index.embeddings.openai import OpenAIEmbedding
from llama_index.agent.openai import OpenAIAgent
from llama_index.tools.mcp import McpToolSpec
from llama_index.core.tools import QueryEngineTool
from mcp_client import BasicMCPClient

async def create_rag_agent():
    Settings.llm = OpenAI(model="gpt-4")
    Settings.embed_model = OpenAIEmbedding(model="text-embedding-3-large")
    documents = [Document(text=t) for t in ["AI 简介", "机器学习概述", "深度学习技术"]]
    index = VectorStoreIndex.from_documents(documents)
    query_engine = index.as_query_engine()
    mcp = BasicMCPClient()
    await mcp.connect_stdio(command="npx", args=["@tavily/mcp-server"])
    tools = [QueryEngineTool.from_defaults(query_engine=query_engine, name="kb", description="查询KB")] + McpToolSpec(mcp).to_tool_list()
    agent = OpenAIAgent.from_tools(
        tools=tools,
        system_prompt="你是AI知识助手，结合内部KB和搜索工具回答",
        verbose=True
    )
    return agent, mcp

async def run():
    agent, mcp = await create_rag_agent()
    resp = await agent.achat("什么是深度学习？它在2025年有哪些最新进展？")
    print(resp.response)
    await mcp.disconnect()

if __name__=="__main__":
    asyncio.run(run())
EOF

# ========== 文件 7 ==========
cat << 'EOF' > agent_codes/7_AutoGen_basic.py
# AutoGen 多 Agent 协作基础
import asyncio
from autogen_agentchat.agents import AssistantAgent
from autogen_agentchat.teams import RoundRobinGroupChat
from autogen_agentchat.ui import Console
from mcp_client.integrations.autogen import AutoGenMCPClient
from mcp_client import StdioServerParams
from llama_index.llms.openai import OpenAI

async def create_autogen_agents():
    mcp = AutoGenMCPClient()
    await mcp.connect_server("search_tools", StdioServerParams(command="npx", args=["@tavily/mcp-server"]))
    tools = await mcp.get_tools("search_tools")
    researcher = AssistantAgent(name="研究员", model_client=OpenAI(model="gpt-4"), tools=tools, system_message="你是研究员")
    analyst = AssistantAgent(name="分析师", model_client=OpenAI(model="gpt-4"), system_message="你是分析师")
    reporter = AssistantAgent(name="报告员", model_client=OpenAI(model="gpt-4"), system_message="你是报告员")
    return researcher, analyst, reporter, mcp

async def run():
    r, a, p, mcp = await create_autogen_agents()
    team = RoundRobinGroupChat([r, a, p])
    result = await Console(team.run_stream(task="请研究分析2024年区块链发展趋势"))
    print("协作结果:", result.messages[-1].content)
    await mcp.disconnect_all()

if __name__=="__main__":
    asyncio.run(run())
EOF
# ========== 文件 8 ==========
cat << 'EOF' > agent_codes/8_AutoGen_distributed.py
# AutoGen 分布式 MCP 示例
import asyncio
from autogen_agentchat.agents import AssistantAgent
from autogen_agentchat.teams import RoundRobinGroupChat
from llama_index.llms.openai import OpenAI
from mcp_client import AutoGenMCPClient, SseServerParams

async def run():
    mcp_search = AutoGenMCPClient()
    await mcp_search.connect_server("remote_search", SseServerParams(url="http://localhost:8000/mcp", headers={"Authorization":"Bearer token"}))
    searcher = AssistantAgent(name="搜索专家", model_client=OpenAI(model="gpt-4"), tools=await mcp_search.get_tools("remote_search"), system_message="你负责搜索")
    analyst = AssistantAgent(name="本地分析师", model_client=OpenAI(model="gpt-4"), system_message="你负责分析")
    team = RoundRobinGroupChat([searcher, analyst])
    result = await team.run(task="分析云计算市场发展")
    print("输出:", result.messages[-1].content)
    await mcp_search.disconnect_all()

if __name__=="__main__":
    asyncio.run(run())
EOF

# ========== 文件 9 ==========
cat << 'EOF' > agent_codes/9_AutoGen_custom_tool.py
# AutoGen 自定义 MCP 工具
import asyncio
from autogen_agentchat.base import Tool
from llama_index.llms.openai import OpenAI
from autogen_agentchat.agents import AssistantAgent
from mcp_client import AutoGenMCPClient, StdioServerParams

class CustomSearchTool(Tool):
    name = "tavily_search"
    description = "自定义搜索工具"
    def __init__(self, mcp): self.mcp = mcp; super().__init__()
    async def run(self, query:str): return await self.mcp.call_tool("tavily_search", {"query":query})
    @property
    def schema(self):
        return {"type":"function","function":{"name":self.name,"description":self.description,"parameters":{"type":"object","properties":{"query":{"type":"string"}},"required":["query"]}}}

async def run():
    mcp = AutoGenMCPClient()
    await mcp.connect_server("search", StdioServerParams(command="npx", args=["@tavily/mcp-server"]))
    tool = CustomSearchTool(mcp)
    agent = AssistantAgent(name="多工具专家", model_client=OpenAI(model="gpt-4"), tools=[tool], system_message="你可用多工具")
    from autogen_agentchat.teams import Swarm
    team = Swarm([agent])
    result = await team.run("查找Python asyncio最佳实践")
    print("结果:", result.messages[-1].content)
    await mcp.disconnect_all()

if __name__=="__main__":
    asyncio.run(run())
EOF

# ========== 文件 10 ==========
cat << 'EOF' > agent_codes/10_Pydantic_basic.py
# Pydantic AI：结构化输出基础
import asyncio
from pydantic_ai import Agent
from pydantic import BaseModel, Field
from mcp_client import MCPServerStdio

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
    agent = Agent(model="openai:gpt-4", tools=tools, system_prompt="你是研究助手，输出结构化报告", result_type=ResearchReport)
    result = await agent.run("研究AI在教育领域的应用现状")
    print(result.data)
    await mcp.disconnect()

if __name__=="__main__":
    asyncio.run(main())
EOF

# ========== 文件 11 ==========
cat << 'EOF' > agent_codes/11_Pydantic_advanced.py
# Pydantic AI：多步骤分析
import asyncio
from pydantic_ai import Agent
from pydantic import BaseModel, Field
from mcp_client import MCPServerHTTP

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
    mcp = MCPServerHTTP(base_url="http://localhost:8000/mcp", headers={"Authorization":"Bearer token"})
    tools = await mcp.get_tools()
    agent = Agent(model="openai:gpt-4", tools=tools, system_prompt="多步骤分析专家", result_type=MultiStepAnalysis)
    result = await agent.run("分析区块链在供应链中的前景和挑战")
    print(result.data)
    await mcp.disconnect()

if __name__=="__main__":
    asyncio.run(main())
EOF

# ========== 文件 12 ==========
cat << 'EOF' > agent_codes/12_Smolagents_basic.py
# SmolAgents 基础示例
import asyncio
from smolagents import CodeAgent, ToolCollection
from mcp_client import MCPClient

async def main():
    mcp = MCPClient()
    await mcp.connect_stdio(command="npx", args=["@tavily/mcp-server"])
    tools = ToolCollection.from_mcp(mcp)
    agent = CodeAgent(tools=tools, model="gpt-4", system_prompt="你是代码生成助手")
    result = await agent.run("搜索 Python 异步最佳实践，并提取要点")
    print("代码:", result.code)
    print("输出:", result.output)
    await mcp.disconnect()

if __name__=="__main__":
    asyncio.run(main())
EOF

# ========== 文件 13 ==========
cat << 'EOF' > agent_codes/13_Smolagents_custom.py
# SmolAgents 自定义工具
import asyncio
from smolagents import CodeAgent, Tool
from mcp_client import MCPClient

class CustomTool(Tool):
    name = "custom_search"
    description = "自定义搜索"
    def __init__(self, mcp): self.mcp = mcp; super().__init__()
    async def forward(self, query: str): return await self.mcp.call_tool("tavily_search", {"query": query})

async def main():
    mcp = MCPClient()
    await mcp.connect_stdio(command="npx", args=["@tavily/mcp-server"])
    tool = CustomTool(mcp)
    agent = CodeAgent(tools=[tool], model="gpt-4", system_prompt="自定义搜索分析师")
    result = await agent.run("使用自定义工具搜索机器学习模型部署")
    print("代码:", result.code)
    print("输出:", result.output)
    await mcp.disconnect()

if __name__=="__main__":
    asyncio.run(main())
EOF

# ========== 文件 14 ==========
cat << 'EOF' > agent_codes/14_Smolagents_batch.py
# SmolAgents 批量任务
import asyncio
from smolagents import CodeAgent, ToolCollection
from mcp_client import MCPClient

async def main():
    mcp = MCPClient()
    await mcp.connect_stdio(command="npx", args=["@tavily/mcp-server"])
    tools = ToolCollection.from_mcp(mcp)
    agent = CodeAgent(tools=tools, model="gpt-4", system_prompt="批量任务处理专家")
    queries = ["AI in finance", "区块链发展", "云计算安全", "物联网趋势"]
    result = await agent.run(f"批量搜索并总结：{queries}")
    print("代码:", result.code)
    print("输出:", result.output)
    await mcp.disconnect()

if __name__=="__main__":
    asyncio.run(main())
EOF

# ========== 文件 15 ==========
cat << 'EOF' > agent_codes/15_Camel_basic_collab.py
# Camel 协作基础
import asyncio
from camel.agents import ChatAgent
from camel.messages import BaseMessage
from camel.types import ModelType
from camel.toolkits import MCPToolkit
from mcp_client import MCPClient

async def main():
    mcp = MCPClient()
    await mcp.connect_stdio(command="npx", args=["@tavily/mcp-server"])
    tools = MCPToolkit(mcp).get_tools()
    researcher = ChatAgent(system_message=BaseMessage.make_assistant_message("研究员", "负责研究"), model_type=ModelType.GPT_4, tools=tools)
    strategist = ChatAgent(system_message=BaseMessage.make_assistant_message("策略师", "负责策略"), model_type=ModelType.GPT_4)
    executor = ChatAgent(system_message=BaseMessage.make_assistant_message("执行官", "负责执行"), model_type=ModelType.GPT_4)
    prompt = BaseMessage.make_user_message("项目经理", "AI 在零售业的商业策略")
    res1 = await researcher.step(prompt)
    res2 = await strategist.step(BaseMessage.make_user_message("项目经理", res1.msg.content))
    res3 = await executor.step(BaseMessage.make_user_message("项目经理", res2.msg.content))
    print("研究:", res1.msg.content)
    print("策略:", res2.msg.content)
    print("执行:", res3.msg.content)
    await mcp.disconnect()

if __name__=="__main__":
    asyncio.run(main())
EOF

# ========== 文件 16 ==========
cat << 'EOF' > agent_codes/16_Camel_dynamic_roles.py
# Camel 动态角色扮演
import asyncio
from camel.societies import RolePlaying
from camel.toolkits import MCPToolkit
from mcp_client import MCPClient
from camel.types import ModelType
from camel.messages import BaseMessage

async def main():
    mcp = MCPClient()
    await mcp.connect_stdio(command="npx", args=["@tavily/mcp-server"])
    tools = MCPToolkit(mcp).get_tools()
    role_play = RolePlaying(assistant_role_name="AI专家", user_role_name="顾问", assistant_agent_kwargs={"model_type":ModelType.GPT_4, "tools":tools}, user_agent_kwargs={"model_type":ModelType.GPT_4})
    prompt = BaseMessage.make_user_message("项目经理", "制定制造业AI转型战略")
    for i in range(3):
        a_msg, u_msg = await role_play.step(prompt)
        print(f"第{i+1}轮 AI专家: {a_msg.content}")
        print(f"第{i+1}轮 顾问: {u_msg.content}")
        prompt = u_msg
    await mcp.disconnect()

if __name__=="__main__":
    asyncio.run(main())
EOF

# ========== 文件 17 ==========
cat << 'EOF' > agent_codes/17_Camel_professional_team.py
# Camel 专业团队
import asyncio
from camel.agents import ChatAgent
from camel.messages import BaseMessage
from camel.types import ModelType
from camel.toolkits import MCPToolkit
from mcp_client import MCPClient

async def main():
    mcp = MCPClient()
    await mcp.connect_stdio(command="npx", args=["@tavily/mcp-server"])
    tools = MCPToolkit(mcp).get_tools()
    roles = {"数据科学家": "数据分析", "产品经理": "产品策略", "架构师": "技术架构", "分析师": "业务分析"}
    agents = {r: ChatAgent(system_message=BaseMessage.make_assistant_message(r, f"{desc}专家"), model_type=ModelType.GPT_4, tools=tools if r != "架构师" else []) for r, desc in roles.items()}
    for role, agent in agents.items():
        prompt = BaseMessage.make_user_message("项目经理", f"就 AI 客服项目，{role} 提供建议")
        res = await agent.step(prompt)
        print(f"{role}: {res.msg.content}")
    await mcp.disconnect()

if __name__=="__main__":
    asyncio.run(main())
EOF

# ========== 文件 18 ==========
cat << 'EOF' > agent_codes/18_CrewAI_basic.py
# CrewAI 基础
import asyncio
from crewai import Agent, Task, Crew, Process
from crewai.tools import BaseTool
from mcp_client import MCPClient

class MCPSearchTool(BaseTool):
    name = "MCP搜索"
    description = "搜索工具"
    def __init__(self, mcp): self.mcp = mcp; super().__init__()
    def _run(self, query: str): return asyncio.run(self.mcp.call_tool("tavily_search", {"query": query}))

async def run():
    mcp = MCPClient()
    await mcp.connect_stdio(command="npx", args=["@tavily/mcp-server"])
    tool = MCPSearchTool(mcp)
    researcher = Agent(role="研究员", goal="研究AI医疗", tools=[tool], verbose=True)
    analyst = Agent(role="分析师", goal="分析", verbose=True)
    writer = Agent(role="撰稿人", goal="写报告", verbose=True)
    tasks = [Task("研究", "研究报告", researcher), Task("分析", "分析报告", analyst), Task("写作", "文章", writer)]
    crew = Crew(agents=[researcher, analyst, writer], tasks=tasks, process=Process.sequential, verbose=2)
    result = crew.kickoff()
    print(result)
    await mcp.disconnect()

if __name__=="__main__":
    asyncio.run(run())
EOF

# ========== 文件 19 ==========
cat << 'EOF' > agent_codes/19_CrewAI_advanced.py
# CrewAI 高级
import asyncio
from crewai import Agent, Task, Crew, Process
from crewai.tools import BaseTool
from mcp_client import MCPClient

class MCPSearchTool(BaseTool):
    name = "MCP搜索"
    description = "高级搜索"
    def __init__(self, mcp): self.mcp = mcp; super().__init__()
    def _run(self, query: str): return asyncio.run(self.mcp.call_tool("tavily_search", {"query": query}))

async def run():
    mcp = MCPClient()
    await mcp.connect_stdio(command="npx", args=["@tavily/mcp-server"])
    tool = MCPSearchTool(mcp)
    researcher = Agent(role="市场研究", goal="市场研究", tools=[tool], verbose=True, max_iter=3, memory=True)
    analyst = Agent(role="技术分析", goal="技术分析", tools=[tool], verbose=True, max_iter=3, memory=True)
    strategist = Agent(role="战略", goal="战略建议", verbose=True, max_iter=3, memory=True)
    tasks = [Task("市场调研", "市场报告", researcher), Task("技术分析", "技术报告", analyst), Task("战略制定", "战略方案", strategist)]
    crew = Crew(agents=[researcher, analyst, strategist], tasks=tasks, process=Process.sequential, verbose=2)
    result = crew.kickoff()
    print(result)
    await mcp.disconnect()

if __name__=="__main__":
    asyncio.run(run())
EOF

echo "✅ 文件 1 ~ 19 全部生成完成！"


