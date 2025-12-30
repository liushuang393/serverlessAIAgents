"""AgentFlow CLI メインエントリーポイント.

このモジュールは AgentFlow の CLI ツールのメインエントリーポイントを提供します。
"""

import asyncio
import json
import sys
import traceback
from importlib.util import module_from_spec, spec_from_file_location
from pathlib import Path

import click
from rich.console import Console
from rich.panel import Panel
from rich.progress import Progress, SpinnerColumn, TextColumn
from rich.text import Text

from agentflow.cli.commands.create import create
from agentflow.cli.commands.init import init
from agentflow.cli.commands.marketplace import marketplace
from agentflow.cli.commands.skills import skills
from agentflow.cli.commands.template import template
from agentflow.core.schemas import SchemaLoader


# Rich Console インスタンス
console = Console()

# 定数
ENTRY_PARTS_COUNT = 2


class AgentFlowCLI(click.Group):
    """AgentFlow CLI グループクラス.

    カスタムヘルプフォーマットと共通オプションを提供します。
    """

    def format_help(self, ctx: click.Context, formatter: click.HelpFormatter) -> None:
        """ヘルプメッセージをフォーマット.

        Args:
            ctx: Click コンテキスト
            formatter: ヘルプフォーマッター
        """
        # タイトル
        title = Text("AgentFlow CLI", style="bold cyan")
        subtitle = Text("Lightweight AI Agent Development Framework", style="dim")

        console.print()
        console.print(Panel(title, subtitle=subtitle, border_style="cyan"))
        console.print()

        # デフォルトのヘルプを表示
        super().format_help(ctx, formatter)


@click.group(cls=AgentFlowCLI)
@click.version_option(version="0.2.0", prog_name="agentflow")
@click.option(
    "--verbose",
    "-v",
    is_flag=True,
    help="詳細な出力を表示",
)
@click.pass_context
def cli(ctx: click.Context, verbose: bool) -> None:
    """AgentFlow - Lightweight AI Agent Development Framework.

    PocketFlow ベースの軽量 AI エージェント開発フレームワーク。
    MCP、A2A、AG-UI の3つのオープンプロトコルをサポートします。

    使用例:

        \b
        # 新しいプロジェクトを作成
        $ agentflow init my-agent

        \b
        # エージェントを実行
        $ agentflow run my-agent

        \b
        # エージェントをマーケットプレイスに公開
        $ agentflow publish my-agent
    """
    # コンテキストオブジェクトを初期化
    ctx.ensure_object(dict)
    ctx.obj["verbose"] = verbose
    ctx.obj["console"] = console


@cli.group()
@click.pass_context
def protocols(ctx: click.Context) -> None:
    """プロトコル関連のコマンド."""


@cli.command()
@click.argument("agent_path", type=click.Path(exists=True, path_type=Path))
@click.option(
    "--input",
    "-i",
    "input_data",
    help="入力データ (JSON 形式または JSON ファイルパス)",
)
@click.option(
    "--output",
    "-o",
    "output_file",
    type=click.Path(path_type=Path),
    help="出力ファイルパス (指定しない場合は標準出力)",
)
@click.option(
    "--json",
    "json_output",
    is_flag=True,
    help="JSON 形式で出力",
)
@click.option(
    "--agent-name",
    "-n",
    "agent_name",
    help="@agent デコレータで定義されたAgent名（agent.yaml不要）",
)
@click.option(
    "--stream",
    "-s",
    is_flag=True,
    help="ストリームモード（SSE形式で進捗表示）",
)
@click.pass_context
def run(
    ctx: click.Context,
    agent_path: Path,
    input_data: str | None,
    output_file: Path | None,
    json_output: bool,
    agent_name: str | None,
    stream: bool,
) -> None:
    """エージェントを実行.

    AGENT_PATH: エージェントディレクトリのパス または @agent で定義されたAgent名

    例:
        # 方式1: agent.yaml ベース（従来）
        agentflow run my-agent --input '{"text": "Hello"}'

        # 方式2: @agent デコレータ（v0.2.0 NEW）
        agentflow run . --agent-name QAAgent --input '{"question": "Hello"}'

        # 方式3: ストリームモード
        agentflow run my-agent --input data.json --stream
    """
    verbose = ctx.obj["verbose"]

    try:
        # 入力データを解析
        shared_data: dict = {}
        if input_data:
            input_path = Path(input_data)
            if input_path.exists() and input_path.is_file():
                with input_path.open("r", encoding="utf-8") as f:
                    shared_data = json.load(f)
                if verbose:
                    console.print(f"[dim]Loaded input from: {input_path}[/dim]")
            else:
                try:
                    shared_data = json.loads(input_data)
                except json.JSONDecodeError as e:
                    console.print(f"[red]Error: Invalid JSON input: {e}[/red]")
                    ctx.exit(1)

        # 方式1: @agent デコレータで定義されたAgent（v0.2.0 NEW）
        if agent_name:
            from agentflow.agent_decorator import AgentClient

            if verbose:
                console.print(f"[dim]Using @agent decorator: {agent_name}[/dim]")

            # エージェントモジュールをロード
            if agent_path.is_dir():
                # ディレクトリの場合、main.py を探す
                main_py = agent_path / "main.py"
                if main_py.exists():
                    spec = spec_from_file_location("agent_module", main_py)
                    if spec and spec.loader:
                        module = module_from_spec(spec)
                        sys.modules["agent_module"] = module
                        spec.loader.exec_module(module)
            else:
                # ファイルの場合
                spec = spec_from_file_location("agent_module", agent_path)
                if spec and spec.loader:
                    module = module_from_spec(spec)
                    sys.modules["agent_module"] = module
                    spec.loader.exec_module(module)

            # AgentClientで実行
            async def execute_agent() -> dict:
                client = AgentClient.get(agent_name)
                if stream:
                    # ストリームモード
                    async for chunk in client.stream(shared_data):
                        if json_output:
                            console.print(json.dumps(chunk, ensure_ascii=False))
                        else:
                            console.print(f"[dim]Chunk:[/dim] {chunk}")
                    return {}
                else:
                    return await client.invoke(shared_data)

            result = asyncio.run(execute_agent())

        # 方式2: agent.yaml ベース（従来）
        else:
            # エージェントディレクトリの確認
            if not agent_path.is_dir():
                console.print(f"[red]Error: {agent_path} is not a directory[/red]")
                ctx.exit(1)

            # agent.yaml を読み込み
            agent_yaml = agent_path / "agent.yaml"
            if not agent_yaml.exists():
                console.print(f"[red]Error: agent.yaml not found in {agent_path}[/red]")
                ctx.exit(1)

            loader = SchemaLoader()
            metadata = loader.load_from_file(agent_yaml)

            # メタデータからAgent名を設定（結果表示用）
            agent_name = metadata.meta.name

            if verbose:
                console.print(f"[dim]Loading agent: {metadata.meta.name}[/dim]")

            # エージェントのメインモジュールを動的にロード
            entry_parts = metadata.pocketflow.entry.split(":")
            if len(entry_parts) != ENTRY_PARTS_COUNT:
                console.print(
                    f"[red]Error: Invalid entry format: {metadata.pocketflow.entry}. "
                    f"Expected 'module.py:FlowName'[/red]"
                )
                ctx.exit(1)

            module_path, flow_name = entry_parts
            entry_point = agent_path / module_path

            if not entry_point.exists():
                console.print(f"[red]Error: Entry point not found: {entry_point}[/red]")
                ctx.exit(1)

            spec = spec_from_file_location("agent_module", entry_point)
            if spec is None or spec.loader is None:
                console.print(f"[red]Error: Failed to load module: {entry_point}[/red]")
                ctx.exit(1)

            module = module_from_spec(spec)
            sys.modules["agent_module"] = module
            spec.loader.exec_module(module)

            # Flowを取得（create_flow または従来のFlow）
            flow = getattr(module, flow_name, None)
            if flow is None:
                console.print(f"[red]Error: Flow '{flow_name}' not found in {entry_point}[/red]")
                ctx.exit(1)

            # 実行
            async def execute() -> dict:
                with Progress(
                    SpinnerColumn(),
                    TextColumn("[progress.description]{task.description}"),
                    console=console,
                ) as progress:
                    progress.add_task(description="Executing agent...", total=None)
                    if stream:
                        # create_flow の run_stream を使用
                        if hasattr(flow, "run_stream"):
                            async for event in flow.run_stream(shared_data):
                                if json_output:
                                    console.print(json.dumps(event, ensure_ascii=False))
                                else:
                                    console.print(f"[dim]{event.get('type', 'event')}:[/dim] {event.get('node', '')}")
                            return {}
                        else:
                            console.print("[yellow]Warning: Stream mode not supported, using normal mode[/yellow]")
                    # 通常実行
                    if hasattr(flow, "run"):
                        return await flow.run(shared_data)
                    elif hasattr(flow, "run_async"):
                        await flow.run_async(shared_data)
                        return shared_data
                    else:
                        msg = f"Flow '{flow_name}' has no run/run_async method"
                        raise ValueError(msg)

            result = asyncio.run(execute())

        # 結果を出力
        if json_output or output_file:
            output_data = json.dumps(result, indent=2, ensure_ascii=False)

            if output_file:
                output_file.write_text(output_data, encoding="utf-8")
                console.print(
                    Panel(
                        f"[green]✓[/green] Output saved to: {output_file}",
                        title="Success",
                        border_style="green",
                    )
                )
            else:
                console.print(output_data)
        elif not stream:
            console.print(
                Panel(
                    f"[green]✓[/green] Agent executed successfully!\n\n"
                    f"[dim]Result:[/dim]\n{json.dumps(result, indent=2, ensure_ascii=False)}",
                    title=agent_name or "Agent",
                    border_style="green",
                )
            )

    except Exception as e:
        console.print(
            Panel(
                f"[red]Error: {e!s}[/red]",
                title="Execution Failed",
                border_style="red",
            )
        )
        if verbose:
            console.print(f"[dim]{traceback.format_exc()}[/dim]")
        ctx.exit(1)


@cli.command(name="list")
@click.pass_context
def list_agents(ctx: click.Context) -> None:
    """インストール済みエージェントを一覧表示.

    v0.2.0: @agent デコレータで定義されたAgentも表示
    """
    verbose = ctx.obj.get("verbose", False) if ctx.obj else False

    if verbose:
        console.print("[dim]Listing installed agents...[/dim]")

    from rich.table import Table

    table = Table(title="Installed Agents", show_header=True, header_style="bold cyan")
    table.add_column("ID", style="cyan")
    table.add_column("Type", style="yellow")
    table.add_column("Description")

    # 方式1: @agent デコレータで定義されたAgent（v0.2.0 NEW）
    try:
        from agentflow.agent_decorator import AgentClient

        decorated_agents = AgentClient.list_agents()
        for agent_name in decorated_agents:
            table.add_row(agent_name, "@agent", f"Decorator-based agent: {agent_name}")
    except Exception:
        pass

    # 方式2: agent.yaml ベース（将来実装）
    # TODO: registry から取得

    if table.rows:
        console.print()
        console.print(table)
    else:
        console.print("[yellow]⚠ No agents found[/yellow]")
        console.print("[dim]Tip: Use 'agentflow create agent <name>' to create a new agent[/dim]")


@cli.command()
@click.argument("agent_id")
@click.pass_context
def info(ctx: click.Context, agent_id: str) -> None:
    """エージェントの詳細情報を表示.

    AGENT_ID: エージェント ID
    """
    verbose = ctx.obj.get("verbose", False) if ctx.obj else False

    if verbose:
        console.print(f"[dim]Getting info for: {agent_id}[/dim]")

    console.print("[yellow]⚠ Not implemented yet[/yellow]")


@cli.command()
@click.option(
    "--system",
    "-s",
    "system_prompt",
    default="あなたは親切で知識豊富なアシスタントです。",
    help="システムプロンプト",
)
@click.option(
    "--model",
    "-m",
    default="gpt-4o-mini",
    help="使用する LLM モデル",
)
@click.pass_context
def chat(ctx: click.Context, system_prompt: str, model: str) -> None:
    """対話型チャットセッションを開始.

    例:
        agentflow chat
        agentflow chat --system "あなたはコード専門家です"
        agentflow chat --model gpt-4o
    """
    from agentflow.llm.llm_client import LLMConfig
    from agentflow.skills.chatbot import ChatBotConfig, ChatBotSkill

    console.print(
        Panel(
            "[cyan]AgentFlow Chat[/cyan]\n"
            "[dim]終了するには 'exit' または 'quit' と入力してください[/dim]",
            border_style="cyan",
        )
    )

    # ChatBot 初期化
    llm_config = LLMConfig(model=model)
    bot_config = ChatBotConfig(system_prompt=system_prompt)
    chatbot = ChatBotSkill(llm_config=llm_config, config=bot_config)
    session = chatbot.create_session()

    async def chat_loop() -> None:
        """チャットループ."""
        while True:
            try:
                user_input = console.input("[bold green]You:[/bold green] ")
                if user_input.lower() in ("exit", "quit", "q"):
                    console.print("[dim]チャットを終了します[/dim]")
                    break

                if not user_input.strip():
                    continue

                with Progress(
                    SpinnerColumn(),
                    TextColumn("[progress.description]{task.description}"),
                    console=console,
                    transient=True,
                ) as progress:
                    progress.add_task(description="考え中...", total=None)
                    response = await chatbot.chat(session.id, user_input)

                console.print(f"[bold blue]Assistant:[/bold blue] {response}")
                console.print()

            except KeyboardInterrupt:
                console.print("\n[dim]チャットを終了します[/dim]")
                break

    asyncio.run(chat_loop())


@cli.command()
@click.option(
    "--host",
    "-h",
    default="127.0.0.1",
    help="サーバーホスト",
)
@click.option(
    "--port",
    "-p",
    default=8000,
    type=int,
    help="サーバーポート",
)
@click.option(
    "--reload",
    is_flag=True,
    help="開発モード（自動リロード）",
)
@click.pass_context
def studio(ctx: click.Context, host: str, port: int, reload: bool) -> None:
    """AgentFlow Studio サーバーを起動.

    例:
        agentflow studio
        agentflow studio --port 3000
        agentflow studio --reload
    """
    import uvicorn

    from agentflow.studio.api import create_app

    console.print(
        Panel(
            f"[cyan]AgentFlow Studio[/cyan]\n"
            f"[dim]http://{host}:{port}[/dim]\n"
            f"[dim]API Docs: http://{host}:{port}/api/docs[/dim]",
            border_style="cyan",
        )
    )

    app = create_app()
    uvicorn.run(
        app if not reload else "agentflow.studio.api:create_app",
        host=host,
        port=port,
        reload=reload,
        factory=reload,
    )


def handle_error(error: Exception, verbose: bool = False) -> None:
    """エラーを処理して表示.

    Args:
        error: 発生した例外
        verbose: 詳細表示フラグ
    """
    console.print()
    console.print(
        Panel(
            f"[bold red]Error:[/bold red] {error!s}",
            border_style="red",
            title="❌ エラー",
        )
    )

    if verbose:
        console.print()
        console.print("[dim]詳細:[/dim]")
        console.print_exception()

    console.print()
    console.print("[dim]ヘルプ: agentflow --help[/dim]")


# コマンドを登録
cli.add_command(init)
cli.add_command(create)
cli.add_command(marketplace)
cli.add_command(skills)
cli.add_command(template)


def main() -> None:
    """CLI メインエントリーポイント."""
    try:
        cli(obj={})
    except Exception as e:
        handle_error(e, verbose=False)
        sys.exit(1)


if __name__ == "__main__":
    main()
