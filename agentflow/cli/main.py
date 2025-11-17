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
@click.version_option(version="0.1.0", prog_name="agentflow")
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
@click.pass_context
def run(
    ctx: click.Context,
    agent_path: Path,
    input_data: str | None,
    output_file: Path | None,
    json_output: bool,
) -> None:
    """エージェントを実行.

    AGENT_PATH: エージェントディレクトリのパス

    例:
        agentflow run my-agent --input '{"text": "Hello"}'
        agentflow run my-agent --input data.json
        agentflow run my-agent --input data.json --output result.json
    """
    verbose = ctx.obj["verbose"]

    try:
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

        if verbose:
            console.print(f"[dim]Loading agent: {metadata.meta.name}[/dim]")

        # 入力データを解析
        shared_data: dict = {}
        if input_data:
            # ファイルパスか JSON 文字列かを判定
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

        # エージェントのメインモジュールを動的にロード
        # entry フィールドは "module.py:FlowName" 形式
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

        # フローを取得
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
                # AsyncFlow を直接実行
                await flow.run_async(shared_data)
                # shared_data が更新されているので、それを返す
                return shared_data

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
        else:
            console.print(
                Panel(
                    f"[green]✓[/green] Agent executed successfully!\n\n"
                    f"[dim]Result:[/dim]\n{json.dumps(result, indent=2, ensure_ascii=False)}",
                    title=metadata.meta.name,
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
    """インストール済みエージェントを一覧表示."""
    verbose = ctx.obj.get("verbose", False) if ctx.obj else False

    if verbose:
        console.print("[dim]Listing installed agents...[/dim]")

    console.print("[yellow]⚠ Not implemented yet[/yellow]")


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
