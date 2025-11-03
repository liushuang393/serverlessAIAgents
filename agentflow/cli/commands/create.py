"""AgentFlow CLI create コマンド.

このモジュールは新しいエージェントやツールを作成するコマンドを提供します。
"""

import re
from pathlib import Path

import click
from jinja2 import Environment, FileSystemLoader
from rich.console import Console
from rich.panel import Panel
from rich.prompt import Confirm, Prompt


console = Console()


def validate_agent_id(agent_id: str) -> bool:
    """エージェント ID が kebab-case 形式かを検証.

    Args:
        agent_id: 検証するエージェント ID

    Returns:
        有効な場合 True
    """
    pattern = r"^[a-z0-9-]+$"
    return bool(re.match(pattern, agent_id))


def create_agent_from_template(
    agent_path: Path,
    agent_id: str,
    agent_name: str,
    author: str,
    description: str,
    icon: str,
    category: str,
    enable_mcp: bool,
    enable_a2a: bool,
    enable_agui: bool,
    entry_point: str,
    flow_name: str,
    color: str,
) -> None:
    """テンプレートからエージェントを作成.

    Args:
        agent_path: エージェントディレクトリのパス
        agent_id: エージェント ID (kebab-case)
        agent_name: エージェント表示名
        author: 作成者名
        description: エージェントの説明
        icon: アイコン絵文字
        category: カテゴリ
        enable_mcp: MCP プロトコルを有効化
        enable_a2a: A2A プロトコルを有効化
        enable_agui: AG-UI プロトコルを有効化
        entry_point: エントリーポイントファイル名
        flow_name: フロー名
        color: カラーコード
    """
    # テンプレートディレクトリを取得
    template_dir = Path(__file__).parent.parent.parent / "templates" / "agent_template"

    if not template_dir.exists():
        msg = f"Template directory not found: {template_dir}"
        raise FileNotFoundError(msg)

    # Jinja2 環境を設定
    env = Environment(loader=FileSystemLoader(str(template_dir)))

    # テンプレート変数
    context = {
        "agent_id": agent_id,
        "agent_name": agent_name,
        "author": author,
        "description": description,
        "icon": icon,
        "category": category,
        "enable_mcp": enable_mcp,
        "enable_a2a": enable_a2a,
        "enable_agui": enable_agui,
        "entry_point": entry_point,
        "flow_name": flow_name,
        "color": color,
    }

    # エージェントディレクトリを作成
    agent_path.mkdir(parents=True, exist_ok=True)

    # agent.yaml を生成
    template = env.get_template("agent.yaml.template")
    content = template.render(**context)
    (agent_path / "agent.yaml").write_text(content, encoding="utf-8")
    console.print("✓ Created: agent.yaml")

    # main.py を生成
    template = env.get_template("main.py.template")
    content = template.render(**context)
    (agent_path / entry_point).write_text(content, encoding="utf-8")
    console.print(f"✓ Created: {entry_point}")

    # tests ディレクトリを作成
    tests_dir = agent_path / "tests"
    tests_dir.mkdir(exist_ok=True)
    (tests_dir / "__init__.py").touch()
    console.print("✓ Created: tests/")


@click.group()
def create() -> None:
    """エージェントやツールを作成."""


@create.command()
@click.argument("agent_name")
@click.option(
    "--author",
    "-a",
    help="作成者名",
)
@click.option(
    "--description",
    "-d",
    help="エージェントの説明",
)
@click.option(
    "--icon",
    "-i",
    default="🤖",
    help="アイコン絵文字",
)
@click.option(
    "--category",
    "-c",
    default="general",
    help="カテゴリ",
)
@click.option(
    "--mcp/--no-mcp",
    default=True,
    help="MCP プロトコルを有効化",
)
@click.option(
    "--a2a/--no-a2a",
    default=True,
    help="A2A プロトコルを有効化",
)
@click.option(
    "--agui/--no-agui",
    default=True,
    help="AG-UI プロトコルを有効化",
)
@click.option(
    "--interactive",
    "-I",
    is_flag=True,
    help="対話モードで設定",
)
@click.pass_context
def agent(
    ctx: click.Context,
    agent_name: str,
    author: str | None,
    description: str | None,
    icon: str,
    category: str,
    mcp: bool,
    a2a: bool,
    agui: bool,
    interactive: bool,
) -> None:
    """新しいエージェントを作成.

    AGENT_NAME: エージェント名 (kebab-case 推奨)

    例:

        \b
        # 基本的な使用方法
        $ agentflow create agent my-agent

        \b
        # 対話モードで作成
        $ agentflow create agent my-agent --interactive

        \b
        # プロトコルを指定
        $ agentflow create agent my-agent --no-agui
    """
    verbose = ctx.obj.get("verbose", False)

    # エージェント ID を kebab-case に変換
    agent_id = agent_name.lower().replace("_", "-").replace(" ", "-")

    # ID の検証
    if not validate_agent_id(agent_id):
        console.print(
            Panel(
                f"[red]Invalid agent name: {agent_name}[/red]\n\n"
                "Agent name must be in kebab-case format.\n"
                f"Suggested: {agent_id}",
                title="❌ エラー",
                border_style="red",
            )
        )
        raise click.Abort

    # 対話モードの場合、追加情報を収集
    if interactive:
        console.print()
        console.print(
            Panel(
                "[bold cyan]対話モードでエージェントを作成[/bold cyan]",
                border_style="cyan",
            )
        )
        console.print()

        if not author:
            author = Prompt.ask("作成者名", default="AgentFlow User")

        if not description:
            description = Prompt.ask(
                "エージェントの説明",
                default=f"A new {agent_name} agent",
            )

        icon = Prompt.ask("アイコン絵文字", default=icon)
        category = Prompt.ask("カテゴリ", default=category)

        mcp = Confirm.ask("MCP プロトコルを有効化?", default=mcp)
        a2a = Confirm.ask("A2A プロトコルを有効化?", default=a2a)
        agui = Confirm.ask("AG-UI プロトコルを有効化?", default=agui)

    # デフォルト値を設定
    if not author:
        author = "AgentFlow User"
    if not description:
        description = f"A new {agent_name} agent"

    # エージェントパス
    agent_path = Path.cwd() / agent_id

    # 既存エージェントのチェック
    if agent_path.exists():
        console.print(
            Panel(
                f"[red]Agent already exists: {agent_path}[/red]\n\n"
                "Please choose a different name or remove the existing directory.",
                title="❌ エラー",
                border_style="red",
            )
        )
        raise click.Abort

    if verbose:
        console.print(f"[dim]Agent ID: {agent_id}[/dim]")
        console.print(f"[dim]Author: {author}[/dim]")
        console.print(f"[dim]Category: {category}[/dim]")
        console.print()

    # エージェントを作成
    try:
        console.print()
        console.print(
            Panel(
                f"[bold cyan]Creating agent: {agent_id}[/bold cyan]",
                border_style="cyan",
            )
        )
        console.print()

        create_agent_from_template(
            agent_path=agent_path,
            agent_id=agent_id,
            agent_name=agent_name,
            author=author,
            description=description,
            icon=icon,
            category=category,
            enable_mcp=mcp,
            enable_a2a=a2a,
            enable_agui=agui,
            entry_point="main.py",
            flow_name="MainFlow",
            color="#3B82F6",
        )

        console.print()
        console.print(
            Panel(
                f"[bold green]✓ Agent created successfully![/bold green]\n\n"
                f"Next steps:\n"
                f"  1. cd {agent_id}\n"
                f"  2. Edit main.py to implement your logic\n"
                f"  3. python main.py",
                title="🎉 成功",
                border_style="green",
            )
        )

    except Exception as e:
        console.print()
        console.print(
            Panel(
                f"[bold red]Error:[/bold red] {e!s}",
                title="❌ エラー",
                border_style="red",
            )
        )
        if verbose:
            console.print_exception()
        raise click.Abort from e
