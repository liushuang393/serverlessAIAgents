"""CLI workspace コマンド.

ワークスペース管理のためのCLIコマンド。Daytonaスタイルの操作を提供。

使用例:
    $ agentflow workspace create --name my-project
    $ agentflow workspace list
    $ agentflow workspace files ws-abc123
    $ agentflow workspace run ws-abc123 main.py
"""

from __future__ import annotations

import asyncio
import json
import logging

import click

from agentflow.sandbox import (
    SandboxConfig,
    SandboxState,
    get_workspace_manager,
)


logger = logging.getLogger(__name__)


@click.group()
def workspace() -> None:
    """ワークスペース管理コマンド."""


@workspace.command()
@click.option("--name", "-n", default="", help="ワークスペース名")
@click.option("--provider", "-p", default="docker", help="プロバイダ (docker/microsandbox/e2b)")
@click.option("--timeout", "-t", default=30.0, help="タイムアウト秒")
def create(name: str, provider: str, timeout: float) -> None:
    """ワークスペースを作成."""

    async def _create() -> None:
        manager = get_workspace_manager()
        config = SandboxConfig(timeout=timeout)
        ws = await manager.create(name=name, provider=provider, config=config)
        click.echo(f"ワークスペース作成: {ws.workspace_id}")
        click.echo(f"  名前: {ws.name}")
        click.echo(f"  状態: {ws.state.value}")

    asyncio.run(_create())


@workspace.command("list")
@click.option("--state", "-s", default=None, help="状態でフィルタ")
@click.option("--json", "as_json", is_flag=True, help="JSON形式で出力")
def list_workspaces(state: str | None, as_json: bool) -> None:
    """ワークスペース一覧を表示."""
    manager = get_workspace_manager()

    state_filter = SandboxState(state) if state else None
    workspaces = manager.list(state=state_filter)

    if as_json:
        data = [ws.get_stats() for ws in workspaces]
        click.echo(json.dumps(data, indent=2, ensure_ascii=False))
    else:
        if not workspaces:
            click.echo("ワークスペースがありません")
            return

        click.echo(f"ワークスペース数: {len(workspaces)}")
        for ws in workspaces:
            click.echo(f"  {ws.workspace_id}: {ws.name} [{ws.state.value}]")


@workspace.command()
@click.argument("workspace_id")
def start(workspace_id: str) -> None:
    """ワークスペースを起動."""

    async def _start() -> None:
        manager = get_workspace_manager()
        ws = manager.get(workspace_id)
        if not ws:
            click.echo(f"ワークスペースが見つかりません: {workspace_id}", err=True)
            return
        await ws.start()
        click.echo(f"ワークスペース起動: {workspace_id}")

    asyncio.run(_start())


@workspace.command()
@click.argument("workspace_id")
def stop(workspace_id: str) -> None:
    """ワークスペースを停止."""

    async def _stop() -> None:
        manager = get_workspace_manager()
        ws = manager.get(workspace_id)
        if not ws:
            click.echo(f"ワークスペースが見つかりません: {workspace_id}", err=True)
            return
        await ws.stop()
        click.echo(f"ワークスペース停止: {workspace_id}")

    asyncio.run(_stop())


@workspace.command()
@click.argument("workspace_id")
def delete(workspace_id: str) -> None:
    """ワークスペースを削除."""

    async def _delete() -> None:
        manager = get_workspace_manager()
        if await manager.delete(workspace_id):
            click.echo(f"ワークスペース削除: {workspace_id}")
        else:
            click.echo(f"ワークスペースが見つかりません: {workspace_id}", err=True)

    asyncio.run(_delete())


@workspace.command()
@click.argument("workspace_id")
@click.option("--directory", "-d", default="", help="ディレクトリパス")
def files(workspace_id: str, directory: str) -> None:
    """ワークスペースのファイル一覧を表示."""

    async def _files() -> None:
        manager = get_workspace_manager()
        ws = manager.get(workspace_id)
        if not ws:
            click.echo(f"ワークスペースが見つかりません: {workspace_id}", err=True)
            return

        file_list = await ws.list_files(directory)
        if not file_list:
            click.echo("ファイルがありません")
            return

        for f in file_list:
            click.echo(f"  {f.path} ({f.size} bytes)")

    asyncio.run(_files())


@workspace.command()
@click.argument("workspace_id")
@click.argument("file_path")
def run(workspace_id: str, file_path: str) -> None:
    """ワークスペース内のファイルを実行."""

    async def _run() -> None:
        manager = get_workspace_manager()
        ws = manager.get(workspace_id)
        if not ws:
            click.echo(f"ワークスペースが見つかりません: {workspace_id}", err=True)
            return

        result = await ws.run_file(file_path)
        if result.stdout:
            click.echo(result.stdout)
        if result.stderr:
            click.echo(result.stderr, err=True)
        if result.error:
            click.echo(f"エラー: {result.error}", err=True)

    asyncio.run(_run())


@workspace.command()
@click.argument("workspace_id")
@click.argument("output_path")
def save(workspace_id: str, output_path: str) -> None:
    """ワークスペース状態をファイルに保存."""

    async def _save() -> None:
        manager = get_workspace_manager()
        if await manager.save_state(workspace_id, output_path):
            click.echo(f"状態保存: {workspace_id} -> {output_path}")
        else:
            click.echo(f"ワークスペースが見つかりません: {workspace_id}", err=True)

    asyncio.run(_save())


@workspace.command()
@click.argument("input_path")
@click.option("--provider", "-p", default="docker", help="プロバイダ")
def restore(input_path: str, provider: str) -> None:
    """ファイルからワークスペース状態を復元."""

    async def _restore() -> None:
        manager = get_workspace_manager()
        ws = await manager.restore_state(input_path, provider)
        if ws:
            click.echo(f"状態復元: {ws.workspace_id}")
        else:
            click.echo(f"ファイルが見つかりません: {input_path}", err=True)

    asyncio.run(_restore())


@workspace.command()
@click.option("--json", "as_json", is_flag=True, help="JSON形式で出力")
def stats(as_json: bool) -> None:
    """ワークスペース統計を表示."""
    manager = get_workspace_manager()
    data = manager.get_stats()

    if as_json:
        click.echo(json.dumps(data, indent=2, ensure_ascii=False))
    else:
        click.echo(f"総ワークスペース数: {data['total_workspaces']}")
        click.echo("状態別:")
        for state, count in data.get("state_counts", {}).items():
            click.echo(f"  {state}: {count}")
