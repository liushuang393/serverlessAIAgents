"""Auto-Agent フロー統合テスト.

統一ツール・Agentレジストリアーキテクチャの完全なワークフローをテスト。
"""

import pytest


@pytest.fixture(autouse=True)
def reset_registries():
    """テスト前後にレジストリをリセット."""
    from agentflow.core.agent_registry import reset_global_agent_registry
    from agentflow.core.tool_registry import reset_global_tool_registry

    reset_global_tool_registry()
    reset_global_agent_registry()
    yield
    reset_global_tool_registry()
    reset_global_agent_registry()


@pytest.mark.integration
class TestAutoAgentFlow:
    """Auto-Agent ワークフローの統合テスト."""

    def test_tool_registration_and_discovery(self):
        """ツール登録と発見のテスト."""
        from agentflow import (
            ToolDefinition,
            ToolDiscoveryService,
            ToolRegistry,
            ToolSource,
        )

        registry = ToolRegistry()
        service = ToolDiscoveryService(registry)

        # ツールを登録
        tool = ToolDefinition(
            uri="tool://builtin/calculator",
            name="calculator",
            description="算術計算を実行",
            source=ToolSource.BUILTIN,
            input_schema={"type": "object", "properties": {"expr": {"type": "string"}}},
        )
        registry.register(tool)

        # スキルを発見
        skills = [
            {"name": "summarize", "description": "テキストを要約"},
        ]
        import asyncio

        asyncio.get_event_loop().run_until_complete(service.discover_skills(skills))

        # 検証
        assert len(registry) == 2
        assert registry.get("tool://builtin/calculator") is not None
        assert registry.get("tool://skill/summarize") is not None

    def test_agent_registration_and_capability_matching(self):
        """Agent登録と能力マッチングのテスト."""
        from agentflow import (
            AgentCapabilitySpec,
            AgentRegistry,
            CapabilityRequirement,
        )

        registry = AgentRegistry()

        # Agent能力を定義
        pdf_cap = AgentCapabilitySpec(
            id="pdf_analyzer",
            name="PDF Analyzer",
            description="PDF文書を分析して情報を抽出",
            tags=["pdf", "analysis", "extraction"],
            required_tools=["tool://mcp/filesystem/read_file"],
        )

        text_cap = AgentCapabilitySpec(
            id="text_summarizer",
            name="Text Summarizer",
            description="テキストを要約",
            tags=["text", "summarization"],
        )

        # Agentを登録
        registry.register("pdf_agent", pdf_cap, lambda: "PDFAgent instance")
        registry.register("text_agent", text_cap, lambda: "TextAgent instance")

        # タスク要件でマッチング
        req = CapabilityRequirement(
            description="PDF文書を分析",
            required_tags=["pdf"],
        )

        matches = registry.find_matching(req)

        # 検証
        assert len(matches) > 0
        assert matches[0][0] == "pdf_agent"  # 最もマッチするAgent

    @pytest.mark.asyncio
    async def test_tool_binding_for_agent(self):
        """Agent へのツールバインディングのテスト."""
        from agentflow import (
            AgentCapabilitySpec,
            ToolBinder,
            ToolDefinition,
            ToolRegistry,
            ToolSource,
        )

        # ツールレジストリを設定
        registry = ToolRegistry()
        registry.register(
            ToolDefinition(
                uri="tool://builtin/search",
                name="search",
                description="ドキュメントを検索",
                source=ToolSource.BUILTIN,
            )
        )
        registry.register(
            ToolDefinition(
                uri="tool://mcp/fs/read",
                name="read",
                description="ファイルを読み取る",
                source=ToolSource.MCP,
            )
        )

        # Agent能力を定義
        cap = AgentCapabilitySpec(
            id="doc_agent",
            name="Document Agent",
            description="ドキュメントを処理",
            required_tools=["tool://builtin/search", "tool://mcp/fs/read"],
        )

        # モックAgentを作成
        class MockAgent:
            _tools = None
            _tool_executor = None

        agent = MockAgent()

        # ツールをバインド
        binder = ToolBinder(registry)
        bound = await binder.bind_for_capability(agent, cap)

        # 検証
        assert bound._tools is not None
        assert len(bound._tools) == 2
        assert bound._tools.get("tool://builtin/search") is not None

    def test_full_workflow_tool_to_agent(self):
        """ツール登録からAgent発見までの完全ワークフロー."""
        from agentflow import (
            AgentCapabilitySpec,
            CapabilityRequirement,
            ToolDiscoveryService,
            get_global_agent_registry,
            get_global_tool_registry,
        )

        # Step 1: グローバルレジストリを取得
        tool_registry = get_global_tool_registry()
        agent_registry = get_global_agent_registry()

        # Step 2: ツールを発見・登録
        service = ToolDiscoveryService(tool_registry)
        service.register_builtin(
            name="echo",
            description="入力をそのまま返す",
            input_schema={"type": "object", "properties": {"text": {"type": "string"}}},
        )

        # Step 3: Agent能力を定義・登録
        echo_cap = AgentCapabilitySpec(
            id="echo_agent",
            name="Echo Agent",
            description="テキストをエコーする",
            tags=["echo", "text"],
            required_tools=["tool://builtin/echo"],
        )

        agent_registry.register(
            agent_id="EchoAgent",
            capability=echo_cap,
            factory=lambda: "EchoAgent instance",
        )

        # Step 4: タスク要件でAgent検索
        req = CapabilityRequirement(
            description="テキストをエコー",
            required_tags=["echo"],
        )

        matches = agent_registry.find_matching(req)

        # 検証
        assert len(matches) > 0
        assert matches[0][0] == "EchoAgent"

        # Step 5: Agentインスタンスを取得
        factory = agent_registry.get_factory("EchoAgent")
        assert factory is not None
        agent_instance = factory()
        assert agent_instance == "EchoAgent instance"

    def test_capability_to_mcp_format(self):
        """能力が必要とするツールをMCP形式に変換."""
        import asyncio

        from agentflow import (
            AgentCapabilitySpec,
            ToolBinder,
            ToolDefinition,
            ToolRegistry,
            ToolSource,
        )

        # レジストリを設定
        registry = ToolRegistry()
        registry.register(
            ToolDefinition(
                uri="tool://builtin/calc",
                name="calc",
                description="計算",
                source=ToolSource.BUILTIN,
                input_schema={"type": "object", "properties": {"expr": {"type": "string"}}},
            )
        )

        # Agent能力
        cap = AgentCapabilitySpec(
            id="calc_agent",
            name="Calculator Agent",
            description="計算を実行",
            required_tools=["tool://builtin/calc"],
        )

        # バインド
        class MockAgent:
            _tools = None
            _tool_executor = None

        agent = MockAgent()
        binder = ToolBinder(registry)
        asyncio.get_event_loop().run_until_complete(binder.bind_for_capability(agent, cap))

        # MCP形式に変換
        mcp_tools = agent._tools.to_mcp_format()

        # 検証
        assert len(mcp_tools) == 1
        assert mcp_tools[0]["name"] == "calc"
        assert "inputSchema" in mcp_tools[0]
