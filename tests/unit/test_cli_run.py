"""AgentFlow CLI run コマンドのテスト."""

import json
from pathlib import Path

from click.testing import CliRunner

from platform.cli.main import cli


class TestRunCommand:
    """run コマンドのテスト."""

    def test_run_help(self) -> None:
        """run --help が動作することをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["run", "--help"])

        assert result.exit_code == 0
        assert "run" in result.output.lower()
        assert "AGENT_PATH" in result.output

    def test_run_with_simple_agent(self, tmp_path: Path) -> None:
        """シンプルなエージェントを実行できることをテスト."""
        runner = CliRunner()

        # テスト用エージェントを作成
        agent_dir = tmp_path / "test-agent"
        agent_dir.mkdir()

        # agent.yaml を作成
        agent_yaml = agent_dir / "agent.yaml"
        agent_yaml.write_text(
            """meta:
  id: test-agent
  name: Test Agent
  version: 1.0.0
  author: Test
  icon: 🤖
  category: test
  description: Test agent

interfaces:
  inputs: []
  outputs: []

protocols:
  mcp:
    enabled: false
  a2a:
    enabled: false
  agui:
    enabled: false

dependencies:
  agents: []
  tools: []
  packages: []

pocketflow:
  entry: main.py:TestFlow
  shared_schema: main.py:dict

visual:
  color: "#3B82F6"
  size: "medium"
  ports: {}
""",
            encoding="utf-8",
        )

        # main.py を作成
        main_py = agent_dir / "main.py"
        main_py.write_text(
            """from pocketflow import AsyncFlow, AsyncNode

class ProcessNode(AsyncNode):
    async def exec_async(self, prep_res):
        input_text = prep_res.get("input", "")
        return {"output": f"Processed: {input_text}"}

    async def prep_async(self, shared):
        return shared

    async def post_async(self, shared, prep_res, exec_res):
        shared.update(exec_res)
        return None  # 次のアクションなし

TestFlow = AsyncFlow()
TestFlow.start(ProcessNode())
""",
            encoding="utf-8",
        )

        # エージェントを実行
        result = runner.invoke(
            cli,
            ["run", str(agent_dir), "--input", '{"input": "test"}'],
        )

        # デバッグ用に出力を表示
        if result.exit_code != 0:
            print(f"Exit code: {result.exit_code}")
            print(f"Output: {result.output}")
            if result.exception:
                print(f"Exception: {result.exception}")
                import traceback

                traceback.print_exception(type(result.exception), result.exception, result.exception.__traceback__)

        assert result.exit_code == 0
        assert "success" in result.output.lower() or "processed" in result.output.lower()

    def test_run_with_input_file(self, tmp_path: Path) -> None:
        """入力ファイルからデータを読み込めることをテスト."""
        runner = CliRunner()

        # テスト用エージェントを作成
        agent_dir = tmp_path / "test-agent"
        agent_dir.mkdir()

        agent_yaml = agent_dir / "agent.yaml"
        agent_yaml.write_text(
            """meta:
  id: test-agent
  name: Test Agent
  version: 1.0.0
  author: Test
  icon: 🤖
  category: test
  description: Test agent

interfaces:
  inputs: []
  outputs: []

protocols:
  mcp:
    enabled: false
  a2a:
    enabled: false
  agui:
    enabled: false

dependencies:
  agents: []
  tools: []
  packages: []

pocketflow:
  entry: main.py:TestFlow
  shared_schema: main.py:dict

visual:
  color: "#3B82F6"
  size: "medium"
  ports: {}
""",
            encoding="utf-8",
        )

        main_py = agent_dir / "main.py"
        main_py.write_text(
            """from pocketflow import AsyncFlow, AsyncNode

class ProcessNode(AsyncNode):
    async def exec_async(self, prep_res):
        return {"result": "ok"}

    async def prep_async(self, shared):
        return shared

    async def post_async(self, shared, prep_res, exec_res):
        shared.update(exec_res)
        return None  # 次のアクションなし

TestFlow = AsyncFlow()
TestFlow.start(ProcessNode())
""",
            encoding="utf-8",
        )

        # 入力ファイルを作成
        input_file = tmp_path / "input.json"
        input_file.write_text('{"data": "test"}', encoding="utf-8")

        # エージェントを実行
        result = runner.invoke(
            cli,
            ["run", str(agent_dir), "--input", str(input_file)],
        )

        assert result.exit_code == 0

    def test_run_with_output_file(self, tmp_path: Path) -> None:
        """出力ファイルに結果を保存できることをテスト."""
        runner = CliRunner()

        # テスト用エージェントを作成
        agent_dir = tmp_path / "test-agent"
        agent_dir.mkdir()

        agent_yaml = agent_dir / "agent.yaml"
        agent_yaml.write_text(
            """meta:
  id: test-agent
  name: Test Agent
  version: 1.0.0
  author: Test
  icon: 🤖
  category: test
  description: Test agent

interfaces:
  inputs: []
  outputs: []

protocols:
  mcp:
    enabled: false
  a2a:
    enabled: false
  agui:
    enabled: false

dependencies:
  agents: []
  tools: []
  packages: []

pocketflow:
  entry: main.py:TestFlow
  shared_schema: main.py:dict

visual:
  color: "#3B82F6"
  size: "medium"
  ports: {}
""",
            encoding="utf-8",
        )

        main_py = agent_dir / "main.py"
        main_py.write_text(
            """from pocketflow import AsyncFlow, AsyncNode

class ProcessNode(AsyncNode):
    async def exec_async(self, prep_res):
        return {"result": "success"}

    async def prep_async(self, shared):
        return shared

    async def post_async(self, shared, prep_res, exec_res):
        shared.update(exec_res)
        return None  # 次のアクションなし

TestFlow = AsyncFlow()
TestFlow.start(ProcessNode())
""",
            encoding="utf-8",
        )

        output_file = tmp_path / "output.json"

        # エージェントを実行
        result = runner.invoke(
            cli,
            ["run", str(agent_dir), "--output", str(output_file)],
        )

        assert result.exit_code == 0
        assert output_file.exists()

        # 出力ファイルの内容を確認
        output_data = json.loads(output_file.read_text(encoding="utf-8"))
        assert "result" in output_data

    def test_run_nonexistent_agent(self, tmp_path: Path) -> None:
        """存在しないエージェントの実行がエラーになることをテスト."""
        runner = CliRunner()

        nonexistent = tmp_path / "nonexistent"

        result = runner.invoke(cli, ["run", str(nonexistent)])

        assert result.exit_code != 0

    def test_run_missing_agent_yaml(self, tmp_path: Path) -> None:
        """agent.yaml がない場合にエラーになることをテスト."""
        runner = CliRunner()

        agent_dir = tmp_path / "test-agent"
        agent_dir.mkdir()

        result = runner.invoke(cli, ["run", str(agent_dir)])

        assert result.exit_code != 0
        assert "agent.yaml" in result.output.lower()

    def test_run_invalid_json_input(self, tmp_path: Path) -> None:
        """無効な JSON 入力でエラーになることをテスト."""
        runner = CliRunner()

        agent_dir = tmp_path / "test-agent"
        agent_dir.mkdir()

        agent_yaml = agent_dir / "agent.yaml"
        agent_yaml.write_text(
            """meta:
  id: test-agent
  name: Test Agent
  version: 1.0.0
  author: Test
  icon: 🤖
  category: test
  description: Test agent

interfaces:
  inputs: []
  outputs: []

protocols:
  mcp:
    enabled: false
  a2a:
    enabled: false
  agui:
    enabled: false

dependencies:
  agents: []
  tools: []
  packages: []

pocketflow:
  entry: main.py:TestFlow
  shared_schema: main.py:dict

visual:
  color: "#3B82F6"
  size: "medium"
  ports: {}
""",
            encoding="utf-8",
        )

        result = runner.invoke(
            cli,
            ["run", str(agent_dir), "--input", "invalid json"],
        )

        assert result.exit_code != 0
        # JSON エラーまたはスキーマエラーのいずれかが発生
        assert "json" in result.output.lower() or "error" in result.output.lower()
