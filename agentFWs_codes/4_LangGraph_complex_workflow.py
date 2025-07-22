# LangGraph 复杂工作流示例
import asyncio
from typing import TypedDict

from langchain_openai import ChatOpenAI
from langgraph.graph import END, START, StateGraph
from langgraph.prebuilt import create_react_agent
from mcp_client import load_mcp_tools


class AgentState(TypedDict):
    messages: list
    research_data: dict
    final_report: str


async def create_research_workflow():
    tools = await load_mcp_tools(
        server_config={"command": "npx", "args": ["@tavily/mcp-server"]}
    )
    llm = ChatOpenAI(model="gpt-4")

    async def research_node(state: AgentState):
        agent = create_react_agent(llm=llm, tools=tools, state_modifier="你是研究员")
        res = await agent.ainvoke(
            {
                "messages": state["messages"]
                + [("user", f"深入研究：{state['messages'][-1][1]}")]
            },
            config={"configurable": {"thread_id": "research"}},
        )
        return {
            "messages": res["messages"],
            "research_data": {"raw_data": res["messages"][-1].content},
            "final_report": "",
        }

    async def analysis_node(state: AgentState):
        agent = create_react_agent(llm=llm, tools=[], state_modifier="你是分析师")
        prompt = f"基于：{state['research_data']['raw_data']}，请分析关键发现和潜在影响"
        res = await agent.ainvoke(
            {"messages": [("user", prompt)]},
            config={"configurable": {"thread_id": "analysis"}},
        )
        state["research_data"]["analysis"] = res["messages"][-1].content
        state["messages"] = res["messages"]
        return state

    async def report_node(state: AgentState):
        agent = create_react_agent(llm=llm, tools=[], state_modifier="你是报告专家")
        prompt = (
            f"请生成报告：原始研究：{state['research_data']['raw_data']}，"
            f"分析结果：{state['research_data']['analysis']}"
        )
        res = await agent.ainvoke(
            {"messages": [("user", prompt)]},
            config={"configurable": {"thread_id": "report"}},
        )
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
    result = await app.ainvoke(
        {"messages": [("user", "AI 医疗领域应用前景")], "research_data": {}, "final_report": ""}
    )
    print("最终报告：", result["final_report"])


if __name__ == "__main__":
    asyncio.run(run())
