# -*- coding: utf-8 -*-
"""AgentFlow CLI サンドボックスコマンド.

Daytonaスタイルのサンドボックス管理CLIを提供。

使用例:
    # サンドボックス作成
    $ agentflow sandbox create --provider docker

    # 一覧表示
    $ agentflow sandbox list

    # コード実行
    $ agentflow sandbox exec <sandbox_id> "print('Hello')"

    # 削除
    $ agentflow sandbox delete <sandbox_id>
"""

import asyncio
import json
from typing import Any

import click
from rich.console import Console
from rich.panel import Panel
from rich.table import Table

console = Console()


@click.group()
def sandbox() -> None:
    """サンドボックス管理コマンド."""


@sandbox.command("create")
@click.option(
    "--provider",
    "-p",
    type=click.Choice(["docker", "microsandbox", "e2b"]),
    default="docker",
    help="サンドボックスプロバイダ",
)
@click.option("--name", "-n", default=None, help="サンドボックス名")
@click.option("--timeout", "-t", type=int, default=60, help="タイムアウト秒")
@click.option("--memory", "-m", type=int, default=1024, help="メモリ制限（MB）")
@click.option("--start", "-s", is_flag=True, help="作成後に起動")
def create_sandbox(
    provider: str,
    name: str | None,
    timeout: int,
    memory: int,
    start: bool,
) -> None:
    """サンドボックスを作成."""
    from agentflow.sandbox import ManagedSandbox, SandboxConfig

    async def _create() -> dict[str, Any]:
        config = SandboxConfig(timeout=float(timeout), memory_mb=memory)
        sandbox = await ManagedSandbox.create(provider, config, name)

        if start:
            await sandbox.start()

        return sandbox.stats

    stats = asyncio.run(_create())

    console.print(Panel(
        f"[bold green]✓[/] サンドボックスを作成しました\n\n"
        f"ID: [cyan]{stats['sandbox_id']}[/]\n"
        f"状態: [yellow]{stats['state']}[/]",
        title="サンドボックス作成",
        border_style="green",
    ))


@sandbox.command("list")
@click.option("--state", "-s", default=None, help="状態でフィルタ")
@click.option("--json", "as_json", is_flag=True, help="JSON形式で出力")
def list_sandboxes(state: str | None, as_json: bool) -> None:
    """サンドボックス一覧を表示."""
    from agentflow.sandbox import SandboxState, get_sandbox_manager

    manager = get_sandbox_manager()
    filter_state = SandboxState(state) if state else None
    sandboxes = manager.list(state=filter_state)

    if as_json:
        data = [s.stats for s in sandboxes]
        console.print(json.dumps(data, ensure_ascii=False, indent=2))
        return

    if not sandboxes:
        console.print("[dim]サンドボックスがありません[/]")
        return

    table = Table(title="サンドボックス一覧")
    table.add_column("ID", style="cyan")
    table.add_column("状態", style="yellow")
    table.add_column("実行回数", justify="right")
    table.add_column("稼働時間", justify="right")
    table.add_column("最終アクティビティ")

    for sb in sandboxes:
        stats = sb.stats
        table.add_row(
            stats["sandbox_id"],
            stats["state"],
            str(stats["execution_count"]),
            f"{stats['uptime_seconds']:.1f}s",
            stats["last_activity_at"][:19],
        )

    console.print(table)


@sandbox.command("start")
@click.argument("sandbox_id")
def start_sandbox(sandbox_id: str) -> None:
    """サンドボックスを起動."""
    from agentflow.sandbox import get_sandbox_manager

    async def _start() -> None:
        manager = get_sandbox_manager()
        sb = manager.get(sandbox_id)
        if not sb:
            console.print(f"[red]エラー:[/] サンドボックスが見つかりません: {sandbox_id}")
            return
        await sb.start()
        console.print(f"[green]✓[/] サンドボックスを起動しました: {sandbox_id}")

    asyncio.run(_start())


@sandbox.command("stop")
@click.argument("sandbox_id")
def stop_sandbox(sandbox_id: str) -> None:
    """サンドボックスを停止."""
    from agentflow.sandbox import get_sandbox_manager

    async def _stop() -> None:
        manager = get_sandbox_manager()
        sb = manager.get(sandbox_id)
        if not sb:
            console.print(f"[red]エラー:[/] サンドボックスが見つかりません: {sandbox_id}")
            return
        await sb.stop()
        console.print(f"[green]✓[/] サンドボックスを停止しました: {sandbox_id}")

    asyncio.run(_stop())


@sandbox.command("delete")
@click.argument("sandbox_id")
@click.option("--force", "-f", is_flag=True, help="強制削除")
def delete_sandbox(sandbox_id: str, force: bool) -> None:
    """サンドボックスを削除."""
    from agentflow.sandbox import get_sandbox_manager

    async def _delete() -> bool:
        manager = get_sandbox_manager()
        return await manager.delete(sandbox_id)

    if not force:
        if not click.confirm(f"サンドボックス {sandbox_id} を削除しますか？"):
            return

    success = asyncio.run(_delete())
    if success:
        console.print(f"[green]✓[/] サンドボックスを削除しました: {sandbox_id}")
    else:
        console.print(f"[red]エラー:[/] サンドボックスが見つかりません: {sandbox_id}")


@sandbox.command("exec")
@click.argument("sandbox_id")
@click.argument("code")
@click.option("--timeout", "-t", type=float, default=30.0, help="タイムアウト秒")
def exec_code(sandbox_id: str, code: str, timeout: float) -> None:
    """サンドボックスでコードを実行."""
    from agentflow.sandbox import get_sandbox_manager

    async def _exec() -> dict[str, Any]:
        manager = get_sandbox_manager()
        sb = manager.get(sandbox_id)
        if not sb:
            return {"error": f"サンドボックスが見つかりません: {sandbox_id}"}

        if not sb.is_running:
            return {"error": f"サンドボックスは実行中ではありません: {sb.state.value}"}

        result = await sb.execute(code, timeout=timeout)
        return {
            "success": result.success,
            "stdout": result.stdout,
            "stderr": result.stderr,
            "exit_code": result.exit_code,
            "duration_ms": result.duration_ms,
        }

    result = asyncio.run(_exec())

    if "error" in result:
        console.print(f"[red]エラー:[/] {result['error']}")
        return

    if result["success"]:
        console.print(Panel(
            result["stdout"] or "[dim]出力なし[/]",
            title=f"実行結果 ({result['duration_ms']:.1f}ms)",
            border_style="green",
        ))
    else:
        console.print(Panel(
            result["stderr"] or result.get("error", "不明なエラー"),
            title=f"エラー (exit: {result['exit_code']})",
            border_style="red",
        ))


@sandbox.command("stats")
def show_stats() -> None:
    """マネージャー統計を表示."""
    from agentflow.sandbox import get_sandbox_manager

    manager = get_sandbox_manager()
    stats = manager.get_stats()

    table = Table(title="サンドボックスマネージャー統計")
    table.add_column("項目", style="cyan")
    table.add_column("値", justify="right")

    table.add_row("総サンドボックス数", str(stats["total_sandboxes"]))
    table.add_row("総実行回数", str(stats["total_executions"]))
    table.add_row("総実行時間", f"{stats['total_execution_ms']:.1f}ms")
    table.add_row("自動クリーンアップ", "有効" if stats["auto_cleanup_active"] else "無効")

    console.print(table)

    # 状態別カウント
    if stats["states"]:
        state_table = Table(title="状態別カウント")
        state_table.add_column("状態", style="yellow")
        state_table.add_column("数", justify="right")

        for state, count in stats["states"].items():
            if count > 0:
                state_table.add_row(state, str(count))

        console.print(state_table)


@sandbox.command("cleanup")
@click.option("--max-idle", "-i", type=int, default=3600, help="最大アイドル時間（秒）")
@click.option("--dry-run", "-d", is_flag=True, help="実際には削除しない")
def cleanup_sandboxes(max_idle: int, dry_run: bool) -> None:
    """非アクティブなサンドボックスをクリーンアップ."""
    from datetime import UTC, datetime

    from agentflow.sandbox import SandboxState, get_sandbox_manager

    manager = get_sandbox_manager()
    now = datetime.now(UTC)

    # 対象を特定
    targets: list[str] = []
    for sb in manager.list():
        idle_seconds = (now - sb.last_activity_at).total_seconds()
        if sb.state in (SandboxState.STOPPED, SandboxState.ARCHIVED):
            if idle_seconds > max_idle:
                targets.append(sb.sandbox_id)

    if not targets:
        console.print("[dim]クリーンアップ対象がありません[/]")
        return

    if dry_run:
        console.print(f"[yellow]ドライラン:[/] {len(targets)} 個のサンドボックスが対象")
        for t in targets:
            console.print(f"  - {t}")
        return

    async def _cleanup() -> list[str]:
        return await manager.cleanup_inactive(max_idle)

    deleted = asyncio.run(_cleanup())
    console.print(f"[green]✓[/] {len(deleted)} 個のサンドボックスを削除しました")

