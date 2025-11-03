"""A2A サーバーのユニットテスト."""

import asyncio

import pytest

from agentflow.protocols.a2a_card import AgentCard, AgentSkill
from agentflow.protocols.a2a_server import A2AServer


@pytest.fixture
def sample_agent_card() -> AgentCard:
    """サンプルエージェントカードを作成.

    Returns:
        AgentCard インスタンス
    """
    return AgentCard(
        name="test-agent",
        description="A test agent",
        version="1.0.0",
        author="Test Author",
        skills=[
            AgentSkill(
                name="greet",
                description="Greet a person",
                input_schema={"type": "object", "properties": {"name": {"type": "string"}}},
                output_schema={"type": "object", "properties": {"message": {"type": "string"}}},
            ),
            AgentSkill(
                name="calculate",
                description="Calculate a sum",
                input_schema={
                    "type": "object",
                    "properties": {"a": {"type": "number"}, "b": {"type": "number"}},
                },
                output_schema={"type": "object", "properties": {"result": {"type": "number"}}},
            ),
        ],
    )


@pytest.fixture
def sample_handlers() -> dict[str, callable]:
    """サンプルハンドラーを作成.

    Returns:
        ハンドラー関数の辞書
    """

    async def greet_handler(inputs: dict) -> dict:
        name = inputs.get("name", "World")
        return {"message": f"Hello, {name}!"}

    async def calculate_handler(inputs: dict) -> dict:
        a = inputs.get("a", 0)
        b = inputs.get("b", 0)
        return {"result": a + b}

    return {
        "greet": greet_handler,
        "calculate": calculate_handler,
    }


class TestA2AServer:
    """A2A サーバーのテストスイート."""

    def test_server_initialization(self) -> None:
        """サーバーの初期化をテスト."""
        server = A2AServer()
        assert len(server.list_agents()) == 0

    def test_register_agent(self, sample_agent_card: AgentCard, sample_handlers: dict) -> None:
        """エージェント登録をテスト."""
        server = A2AServer()
        server.register_agent(sample_agent_card, sample_handlers)

        assert len(server.list_agents()) == 1
        assert "test-agent" in server.list_agents()

    def test_register_duplicate_agent(
        self, sample_agent_card: AgentCard, sample_handlers: dict
    ) -> None:
        """重複エージェント登録をテスト."""
        server = A2AServer()
        server.register_agent(sample_agent_card, sample_handlers)

        with pytest.raises(ValueError, match="Agent already registered"):
            server.register_agent(sample_agent_card, sample_handlers)

    def test_unregister_agent(self, sample_agent_card: AgentCard, sample_handlers: dict) -> None:
        """エージェント削除をテスト."""
        server = A2AServer()
        server.register_agent(sample_agent_card, sample_handlers)
        server.unregister_agent("test-agent")

        assert len(server.list_agents()) == 0

    def test_unregister_nonexistent_agent(self) -> None:
        """存在しないエージェントの削除をテスト."""
        server = A2AServer()

        with pytest.raises(ValueError, match="Agent not found"):
            server.unregister_agent("nonexistent")

    def test_get_agent_card(self, sample_agent_card: AgentCard, sample_handlers: dict) -> None:
        """エージェントカード取得をテスト."""
        server = A2AServer()
        server.register_agent(sample_agent_card, sample_handlers)

        card = server.get_agent_card("test-agent")
        assert card is not None
        assert card.name == "test-agent"

    def test_get_nonexistent_agent_card(self) -> None:
        """存在しないエージェントカード取得をテスト."""
        server = A2AServer()
        card = server.get_agent_card("nonexistent")
        assert card is None

    async def test_handle_task_success(
        self, sample_agent_card: AgentCard, sample_handlers: dict
    ) -> None:
        """タスク処理の成功をテスト."""
        server = A2AServer()
        server.register_agent(sample_agent_card, sample_handlers)

        result = await server.handle_task("test-agent", "greet", {"name": "Alice"})

        assert result["status"] == "success"
        assert result["result"]["message"] == "Hello, Alice!"
        assert result["agent"] == "test-agent"
        assert result["skill"] == "greet"

    async def test_handle_task_nonexistent_agent(self) -> None:
        """存在しないエージェントへのタスクをテスト."""
        server = A2AServer()

        with pytest.raises(ValueError, match="Agent not found"):
            await server.handle_task("nonexistent", "greet", {})

    async def test_handle_task_nonexistent_skill(
        self, sample_agent_card: AgentCard, sample_handlers: dict
    ) -> None:
        """存在しないスキルへのタスクをテスト."""
        server = A2AServer()
        server.register_agent(sample_agent_card, sample_handlers)

        with pytest.raises(ValueError, match="Skill not found"):
            await server.handle_task("test-agent", "nonexistent", {})

    async def test_handle_task_timeout(self, sample_agent_card: AgentCard) -> None:
        """タスクタイムアウトをテスト."""

        async def slow_handler(_inputs: dict) -> dict:
            await asyncio.sleep(2)
            return {"result": "done"}

        handlers = {"greet": slow_handler}

        server = A2AServer(default_timeout=0.1)
        server.register_agent(sample_agent_card, handlers)

        result = await server.handle_task("test-agent", "greet", {})

        assert result["status"] == "error"
        assert result["error"] == "Task timeout"

    async def test_handle_task_with_sync_handler(self, sample_agent_card: AgentCard) -> None:
        """同期ハンドラーでのタスク処理をテスト."""

        def sync_handler(inputs: dict) -> dict:
            name = inputs.get("name", "World")
            return {"message": f"Hello, {name}!"}

        handlers = {"greet": sync_handler}

        server = A2AServer()
        server.register_agent(sample_agent_card, handlers)

        result = await server.handle_task("test-agent", "greet", {"name": "Bob"})

        assert result["status"] == "success"
        assert result["result"]["message"] == "Hello, Bob!"

    def test_get_all_agent_cards(self, sample_agent_card: AgentCard, sample_handlers: dict) -> None:
        """全エージェントカード取得をテスト."""
        server = A2AServer()
        server.register_agent(sample_agent_card, sample_handlers)

        cards = server.get_all_agent_cards()

        assert len(cards) == 1
        assert cards[0]["name"] == "test-agent"
        assert len(cards[0]["skills"]) == 2

    def test_get_agent_skills(self, sample_agent_card: AgentCard, sample_handlers: dict) -> None:
        """エージェントスキルリスト取得をテスト."""
        server = A2AServer()
        server.register_agent(sample_agent_card, sample_handlers)

        skills = server.get_agent_skills("test-agent")

        assert len(skills) == 2
        assert "greet" in skills
        assert "calculate" in skills

    def test_get_skills_for_nonexistent_agent(self) -> None:
        """存在しないエージェントのスキルリスト取得をテスト."""
        server = A2AServer()
        skills = server.get_agent_skills("nonexistent")
        assert len(skills) == 0


class TestAgentCard:
    """AgentCard のテストスイート."""

    def test_agent_card_creation(self) -> None:
        """AgentCard の作成をテスト."""
        card = AgentCard(
            name="test",
            description="Test agent",
            version="1.0.0",
        )

        assert card.name == "test"
        assert card.description == "Test agent"
        assert card.version == "1.0.0"

    def test_agent_card_from_yaml(self) -> None:
        """YAML からの AgentCard 作成をテスト."""
        yaml_data = {
            "name": "test-agent",
            "description": "A test agent",
            "version": "2.0.0",
            "author": "Test Author",
            "skills": [
                {
                    "name": "skill1",
                    "description": "First skill",
                    "input_schema": {"type": "object"},
                    "output_schema": {"type": "object"},
                }
            ],
            "metadata": {"key": "value"},
        }

        card = AgentCard.from_yaml(yaml_data)

        assert card.name == "test-agent"
        assert card.version == "2.0.0"
        assert len(card.skills) == 1
        assert card.skills[0].name == "skill1"

    def test_agent_card_to_a2a_format(self) -> None:
        """AgentCard の A2A 形式変換をテスト."""
        card = AgentCard(
            name="test",
            description="Test agent",
            skills=[
                AgentSkill(
                    name="skill1",
                    description="First skill",
                    input_schema={"type": "object"},
                    output_schema={"type": "object"},
                )
            ],
        )

        a2a_format = card.to_a2a_format()

        assert a2a_format["name"] == "test"
        assert len(a2a_format["skills"]) == 1
        assert a2a_format["skills"][0]["name"] == "skill1"
