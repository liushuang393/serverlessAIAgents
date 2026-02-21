"""AgentFlow CLI flow コマンド.

workflow YAML を直接実行するための CLI コマンドを提供。
"""

from __future__ import annotations

import asyncio
import json
from pathlib import Path
from typing import Any

import click
import yaml
from rich.console import Console
from rich.panel import Panel

from agentflow.services import WorkflowService


console = Console()


def _parse_input_data(input_data: str | None) -> dict[str, Any]:
    """入力データを解析（JSON文字列またはJSONファイル）."""
    if not input_data:
        return {}

    input_path = Path(input_data)
    if input_path.exists() and input_path.is_file():
        loaded = json.loads(input_path.read_text(encoding="utf-8"))
        if isinstance(loaded, dict):
            return loaded
        msg = "Input JSON file must contain an object"
        raise ValueError(msg)

    try:
        loaded = json.loads(input_data)
        if isinstance(loaded, dict):
            return loaded
        msg = "Input JSON must contain an object"
        raise ValueError(msg)
    except json.JSONDecodeError as e:
        msg = f"Invalid JSON input: {e}"
        raise ValueError(msg) from e


def _load_workflow_spec(workflow_path: Path) -> dict[str, Any]:
    """workflow YAML を読み込む."""
    data = yaml.safe_load(workflow_path.read_text(encoding="utf-8"))
    if not isinstance(data, dict):
        msg = "Workflow file must be a YAML mapping object"
        raise ValueError(msg)
    return data


def _extract_workflow_args(spec: dict[str, Any]) -> tuple[str, str, dict[str, Any], dict[str, Any]]:
    """workflow 実行引数を抽出."""
    workflow_type = spec.get("workflow_type") or spec.get("type")
    if not workflow_type or not isinstance(workflow_type, str):
        msg = "workflow_type (or type) is required in workflow YAML"
        raise ValueError(msg)

    task = spec.get("task", "")
    if not isinstance(task, str):
        msg = "task must be a string when provided"
        raise ValueError(msg)

    workflow_input = spec.get("input_data", {})
    if not isinstance(workflow_input, dict):
        msg = "input_data must be an object"
        raise ValueError(msg)

    config = spec.get("config", {})
    if not isinstance(config, dict):
        msg = "config must be an object"
        raise ValueError(msg)

    return workflow_type, task, workflow_input, config


@click.group()
def flow() -> None:
    """ワークフロー関連コマンド."""


@flow.command("run")
@click.argument(
    "workflow_path",
    type=click.Path(exists=True, dir_okay=False, path_type=Path),
)
@click.option(
    "--input",
    "-i",
    "input_data",
    help="追加入力データ（JSON文字列またはJSONファイルパス）",
)
@click.option(
    "--output",
    "-o",
    "output_file",
    type=click.Path(path_type=Path),
    help="出力ファイルパス",
)
@click.option(
    "--json",
    "json_output",
    is_flag=True,
    help="JSON形式で出力",
)
@click.option(
    "--stream",
    "-s",
    is_flag=True,
    help="ストリームモードで実行",
)
def run_flow(
    workflow_path: Path,
    input_data: str | None,
    output_file: Path | None,
    json_output: bool,
    stream: bool,
) -> None:
    """workflow YAML を実行."""
    try:
        spec = _load_workflow_spec(workflow_path)
        workflow_type, task, workflow_input, config = _extract_workflow_args(spec)
        cli_input = _parse_input_data(input_data)
        merged_input = {**workflow_input, **cli_input}

        service = WorkflowService()

        if stream:
            events = asyncio.run(
                _run_flow_stream(
                    service=service,
                    workflow_type=workflow_type,
                    task=task,
                    input_data=merged_input,
                    config=config,
                    json_output=json_output,
                )
            )
            if output_file:
                output_file.write_text(
                    json.dumps(events, indent=2, ensure_ascii=False),
                    encoding="utf-8",
                )
                console.print(
                    Panel(
                        f"[green]✓[/green] Output saved to: {output_file}",
                        title="Success",
                        border_style="green",
                    )
                )
            return

        result = asyncio.run(
            service.execute(
                workflow_type=workflow_type,
                task=task,
                input_data=merged_input,
                config=config,
            )
        )
        payload = result.model_dump(mode="json")

        if not result.success:
            console.print(
                Panel(
                    f"[red]Workflow execution failed:[/red] {result.error_message or 'unknown error'}",
                    title="Execution Failed",
                    border_style="red",
                )
            )
            raise click.exceptions.Exit(1)

        output_data = json.dumps(payload, indent=2, ensure_ascii=False)
        if json_output or output_file:
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
            return

        console.print(
            Panel(
                f"[green]✓[/green] Workflow executed successfully!\n\n"
                f"[dim]Execution ID:[/dim] {payload['execution_id']}\n"
                f"[dim]Duration:[/dim] {payload['duration_ms']:.2f} ms",
                title=workflow_type,
                border_style="green",
            )
        )

    except click.exceptions.Exit:
        raise
    except Exception as e:  # pragma: no cover - click entrypoint safety
        console.print(
            Panel(
                f"[red]Error:[/red] {e!s}",
                title="Flow Run Failed",
                border_style="red",
            )
        )
        raise click.exceptions.Exit(1) from e


async def _run_flow_stream(
    *,
    service: WorkflowService,
    workflow_type: str,
    task: str,
    input_data: dict[str, Any],
    config: dict[str, Any],
    json_output: bool,
) -> list[dict[str, Any]]:
    """ワークフローをストリーム実行."""
    events: list[dict[str, Any]] = []

    async for event in service.execute_stream(
        workflow_type=workflow_type,
        task=task,
        input_data=input_data,
        config=config,
    ):
        payload = event.model_dump(mode="json")
        events.append(payload)
        if json_output:
            console.print(json.dumps(payload, ensure_ascii=False))
            continue

        event_type = payload.get("type", "")
        message = payload.get("message", "")
        if event_type == "progress":
            progress = payload.get("progress", 0.0)
            console.print(f"[dim]progress[/dim] {progress:.1f}% {message}")
        else:
            console.print(f"[dim]{event_type}[/dim] {message}")

    return events
