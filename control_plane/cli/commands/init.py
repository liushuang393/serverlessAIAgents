"""AgentFlow CLI init コマンド.

このモジュールは新しい AgentFlow プロジェクトを初期化するコマンドを提供します。
"""

import re
from pathlib import Path

import click
from jinja2 import Environment, FileSystemLoader
from rich.console import Console
from rich.panel import Panel
from rich.progress import Progress, SpinnerColumn, TextColumn


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


def create_project_structure(
    project_path: Path,
    agent_id: str,
    agent_name: str,
    author: str,
    description: str,
    protocols: list[str],
    dry_run: bool = False,
) -> None:
    """プロジェクト構造を作成.

    Args:
        project_path: プロジェクトディレクトリのパス
        agent_id: エージェント ID (kebab-case)
        agent_name: エージェント表示名
        author: 作成者名
        description: エージェントの説明
        protocols: 有効化するプロトコルのリスト
        dry_run: ドライランモード
    """
    # テンプレートディレクトリを取得
    template_dir = Path(__file__).parent.parent.parent / "templates" / "project_template"

    if not template_dir.exists():
        msg = f"Template directory not found: {template_dir}"
        raise FileNotFoundError(msg)

    # Jinja2 環境を設定（CLIコード生成用途 - HTML出力なし）
    env = Environment(loader=FileSystemLoader(str(template_dir)))  # nosec B701

    # テンプレート変数
    context = {
        "agent_id": agent_id,
        "agent_name": agent_name,
        "author": author,
        "description": description,
        "protocols": protocols,
    }

    # 作成するファイルのリスト
    files_to_create = [
        ("agent.yaml.template", "agent.yaml"),
        ("main.py.template", "main.py"),
        ("README.md.template", "README.md"),
        ("requirements.txt.template", "requirements.txt"),
        (".gitignore.template", ".gitignore"),
    ]

    if dry_run:
        console.print("[dim]Dry-run mode: No files will be created[/dim]")
        console.print()

    with Progress(
        SpinnerColumn(),
        TextColumn("[progress.description]{task.description}"),
        console=console,
    ) as progress:
        task = progress.add_task("Creating project...", total=len(files_to_create) + 2)

        # プロジェクトディレクトリを作成
        if not dry_run:
            project_path.mkdir(parents=True, exist_ok=True)
        console.print(f"✓ Created directory: {project_path}")
        progress.advance(task)

        # tests ディレクトリを作成
        tests_dir = project_path / "tests"
        if not dry_run:
            tests_dir.mkdir(exist_ok=True)
            (tests_dir / "__init__.py").touch()
        console.print(f"✓ Created directory: {tests_dir}")
        progress.advance(task)

        # テンプレートファイルを生成
        for template_name, output_name in files_to_create:
            template = env.get_template(template_name)
            content = template.render(**context)

            output_path = project_path / output_name
            if not dry_run:
                output_path.write_text(content, encoding="utf-8")

            console.print(f"✓ Created file: {output_name}")
            progress.advance(task)


@click.command()
@click.argument("project_name")
@click.option(
    "--protocols",
    "-p",
    multiple=True,
    type=click.Choice(["mcp", "a2a", "agui"], case_sensitive=False),
    default=["mcp", "a2a", "agui"],
    help="有効化するプロトコル (デフォルト: すべて)",
)
@click.option(
    "--author",
    "-a",
    default="AgentFlow User",
    help="作成者名",
)
@click.option(
    "--description",
    "-d",
    default="A new AgentFlow agent",
    help="エージェントの説明",
)
@click.option(
    "--dry-run",
    is_flag=True,
    help="ドライランモード (ファイルを作成しない)",
)
@click.pass_context
def init(
    ctx: click.Context,
    project_name: str,
    protocols: tuple[str, ...],
    author: str,
    description: str,
    dry_run: bool,
) -> None:
    """新しい AgentFlow プロジェクトを初期化.

    PROJECT_NAME: プロジェクト名 (kebab-case 推奨)

    例:

        \b
        # すべてのプロトコルを有効化
        $ bizcore init my-agent

        \b
        # MCP のみを有効化
        $ bizcore init my-agent --protocols mcp

        \b
        # ドライランモード
        $ bizcore init my-agent --dry-run
    """
    verbose = ctx.obj.get("verbose", False)

    # プロジェクト名を kebab-case に変換
    agent_id = project_name.lower().replace("_", "-").replace(" ", "-")

    # ID の検証
    if not validate_agent_id(agent_id):
        console.print(
            Panel(
                f"[red]Invalid project name: {project_name}[/red]\n\n"
                "Project name must be in kebab-case format "
                "(lowercase letters, numbers, and hyphens only).\n"
                f"Suggested: {agent_id}",
                title="❌ エラー",
                border_style="red",
            )
        )
        raise click.Abort

    # プロジェクトパス
    project_path = Path.cwd() / agent_id

    # 既存プロジェクトのチェック
    if project_path.exists() and not dry_run:
        console.print(
            Panel(
                f"[red]Project already exists: {project_path}[/red]\n\n"
                "Please choose a different name or remove the existing directory.",
                title="❌ エラー",
                border_style="red",
            )
        )
        raise click.Abort

    # プロトコルリストを作成
    protocol_list = list(protocols) if protocols else ["mcp", "a2a", "agui"]

    if verbose:
        console.print(f"[dim]Project name: {agent_id}[/dim]")
        console.print(f"[dim]Author: {author}[/dim]")
        console.print(f"[dim]Protocols: {', '.join(protocol_list)}[/dim]")
        console.print()

    # プロジェクトを作成
    try:
        console.print()
        console.print(
            Panel(
                f"[bold cyan]Creating project: {agent_id}[/bold cyan]",
                border_style="cyan",
            )
        )
        console.print()

        create_project_structure(
            project_path=project_path,
            agent_id=agent_id,
            agent_name=project_name,
            author=author,
            description=description,
            protocols=protocol_list,
            dry_run=dry_run,
        )

        console.print()
        console.print(
            Panel(
                f"[bold green]✓ Project created successfully![/bold green]\n\n"
                f"Next steps:\n"
                f"  1. cd {agent_id}\n"
                f"  2. pip install -r requirements.txt\n"
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
