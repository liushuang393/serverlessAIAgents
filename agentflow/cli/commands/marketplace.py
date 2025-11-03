"""AgentFlow CLI マーケットプレイスコマンド.

このモジュールはマーケットプレイス関連の CLI コマンドを提供します。
"""

import click
from rich.console import Console
from rich.panel import Panel
from rich.table import Table

from agentflow.marketplace import MarketplaceClient


console = Console()

# 定数
MAX_DESCRIPTION_LENGTH = 50


@click.group()
def marketplace() -> None:
    """マーケットプレイスからエージェントを検索・インストール."""


@marketplace.command()
@click.argument("query", required=False)
@click.option(
    "--category",
    "-c",
    help="カテゴリでフィルター",
)
@click.option(
    "--protocols",
    "-p",
    multiple=True,
    help="プロトコルでフィルター (例: mcp, a2a, agui)",
)
@click.option(
    "--limit",
    "-l",
    default=10,
    type=int,
    help="最大結果数",
)
@click.pass_context
def search(
    ctx: click.Context,
    query: str | None,
    category: str | None,
    protocols: tuple[str, ...],
    limit: int,
) -> None:
    """マーケットプレイスでエージェントを検索.

    例:
        agentflow marketplace search "PDF"
        agentflow marketplace search --category document
        agentflow marketplace search --protocols mcp --protocols a2a
    """
    try:
        client = MarketplaceClient()

        # 検索実行
        results = client.search(
            query=query,
            category=category,
            protocols=list(protocols) if protocols else None,
            limit=limit,
        )

        if not results:
            console.print("[yellow]No agents found.[/yellow]")
            return

        # 結果をテーブルで表示
        table = Table(title=f"Search Results ({len(results)} agents)")
        table.add_column("ID", style="cyan")
        table.add_column("Name", style="green")
        table.add_column("Version", style="blue")
        table.add_column("Category", style="magenta")
        table.add_column("Protocols", style="yellow")
        table.add_column("Description")

        for agent in results:
            table.add_row(
                agent.id,
                agent.name,
                agent.version,
                agent.category,
                ", ".join(agent.protocols),
                (
                    agent.description[:MAX_DESCRIPTION_LENGTH] + "..."
                    if len(agent.description) > MAX_DESCRIPTION_LENGTH
                    else agent.description
                ),
            )

        console.print(table)
        console.print(
            "\n[dim]Use 'agentflow marketplace install <agent-id>' to install an agent.[/dim]"
        )

        client.close()

    except Exception as e:
        console.print(
            Panel(
                f"[red]Error: {e!s}[/red]",
                title="Search Failed",
                border_style="red",
            )
        )
        ctx.exit(1)


@marketplace.command()
@click.argument("agent_id")
@click.option(
    "--version",
    "-v",
    help="インストールするバージョン (デフォルト: 最新)",
)
@click.option(
    "--force",
    "-f",
    is_flag=True,
    help="既存エージェントを上書き",
)
@click.pass_context
def install(
    ctx: click.Context,
    agent_id: str,
    version: str | None,
    force: bool,
) -> None:
    """マーケットプレイスからエージェントをインストール.

    例:
        agentflow marketplace install pdf-processor
        agentflow marketplace install text-analyzer --version 1.2.0
        agentflow marketplace install my-agent --force
    """
    try:
        client = MarketplaceClient()

        console.print(f"[cyan]Installing agent: {agent_id}...[/cyan]")

        # インストール実行
        install_path = client.install(agent_id, version=version, force=force)

        console.print(
            Panel(
                f"[green]✓[/green] Agent installed successfully!\n\n"
                f"[dim]Install path:[/dim] {install_path}\n"
                f"[dim]Run with:[/dim] agentflow run {install_path}",
                title=f"Installed: {agent_id}",
                border_style="green",
            )
        )

        client.close()

    except ValueError as e:
        console.print(
            Panel(
                f"[red]Error: {e!s}[/red]",
                title="Installation Failed",
                border_style="red",
            )
        )
        ctx.exit(1)
    except Exception as e:
        console.print(
            Panel(
                f"[red]Unexpected error: {e!s}[/red]",
                title="Installation Failed",
                border_style="red",
            )
        )
        ctx.exit(1)


@marketplace.command()
@click.argument("agent_id")
@click.option(
    "--yes",
    "-y",
    is_flag=True,
    help="確認をスキップ",
)
@click.pass_context
def uninstall(
    ctx: click.Context,
    agent_id: str,
    yes: bool,
) -> None:
    """インストール済みエージェントをアンインストール.

    例:
        agentflow marketplace uninstall pdf-processor
        agentflow marketplace uninstall my-agent --yes
    """
    try:
        client = MarketplaceClient()

        # エージェントが存在するか確認
        if not client.registry.is_installed(agent_id):
            console.print(f"[yellow]Agent not installed: {agent_id}[/yellow]")
            client.close()
            ctx.exit(1)

        # 確認
        if not yes:
            confirm = click.confirm(f"Are you sure you want to uninstall '{agent_id}'?")
            if not confirm:
                console.print("[yellow]Uninstall cancelled.[/yellow]")
                client.close()
                return

        # アンインストール実行
        success = client.uninstall(agent_id)

        if success:
            console.print(
                Panel(
                    "[green]✓[/green] Agent uninstalled successfully!",
                    title=f"Uninstalled: {agent_id}",
                    border_style="green",
                )
            )
        else:
            console.print(f"[red]Failed to uninstall agent: {agent_id}[/red]")
            ctx.exit(1)

        client.close()

    except Exception as e:
        console.print(
            Panel(
                f"[red]Error: {e!s}[/red]",
                title="Uninstall Failed",
                border_style="red",
            )
        )
        ctx.exit(1)


@marketplace.command(name="list")
@click.pass_context
def list_installed(ctx: click.Context) -> None:
    """インストール済みエージェントを一覧表示.

    例:
        agentflow marketplace list
    """
    try:
        client = MarketplaceClient()

        agents = client.list_installed()

        if not agents:
            console.print("[yellow]No agents installed.[/yellow]")
            console.print("[dim]Use 'agentflow marketplace search' to find agents.[/dim]")
            client.close()
            return

        # 結果をテーブルで表示
        table = Table(title=f"Installed Agents ({len(agents)})")
        table.add_column("ID", style="cyan")
        table.add_column("Name", style="green")
        table.add_column("Version", style="blue")
        table.add_column("Category", style="magenta")
        table.add_column("Installed At", style="yellow")
        table.add_column("Path")

        for agent in agents:
            table.add_row(
                agent.id,
                agent.name,
                agent.version,
                agent.category,
                agent.installed_at[:10],  # 日付のみ表示
                agent.install_path,
            )

        console.print(table)

        client.close()

    except Exception as e:
        console.print(
            Panel(
                f"[red]Error: {e!s}[/red]",
                title="List Failed",
                border_style="red",
            )
        )
        ctx.exit(1)
