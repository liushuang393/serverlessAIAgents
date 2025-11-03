"""AgentFlow CLI テンプレートコマンド.

このモジュールはテンプレート関連のコマンドを提供します。
"""

from pathlib import Path

import click
from rich.console import Console
from rich.panel import Panel
from rich.table import Table

from agentflow.templates.template_manager import TemplateManager


# Rich Console インスタンス
console = Console()


@click.group()
def template() -> None:
    """テンプレート管理コマンド.

    エージェントテンプレートの一覧表示と生成を行います。
    """


@template.command()
def list_templates() -> None:
    """利用可能なテンプレート一覧を表示.

    使用例:

        \b
        $ agentflow template list
    """
    try:
        manager = TemplateManager()
        templates = manager.list_templates()

        if not templates:
            console.print("[yellow]利用可能なテンプレートがありません。[/yellow]")
            return

        # テーブルを作成
        table = Table(title="利用可能なテンプレート", show_header=True)
        table.add_column("ID", style="cyan", no_wrap=True)
        table.add_column("名前", style="green")
        table.add_column("カテゴリ", style="magenta")
        table.add_column("説明", style="white")

        for tmpl in templates:
            table.add_row(
                tmpl.id,
                tmpl.name,
                tmpl.category,
                tmpl.description,
            )

        console.print()
        console.print(table)
        console.print()
        console.print(f"[dim]合計 {len(templates)} 個のテンプレートが利用可能です。[/dim]")
        console.print()

    except Exception as e:
        console.print(f"[red]エラー: {e}[/red]")
        raise click.Abort from e


@template.command()
@click.argument("template_id", type=str)
def show(template_id: str) -> None:
    """テンプレートの詳細を表示.

    使用例:

        \b
        $ agentflow template show invoice-processor

    Args:
        template_id: テンプレート ID
    """
    try:
        manager = TemplateManager()
        metadata = manager.get_template(template_id)

        if metadata is None:
            console.print(f"[red]テンプレートが見つかりません: {template_id}[/red]")
            raise click.Abort

        # テンプレート情報を表示
        console.print()
        console.print(
            Panel(
                f"[bold cyan]{metadata.name}[/bold cyan]\n[dim]{metadata.description}[/dim]",
                title=f"テンプレート: {metadata.id}",
                border_style="cyan",
            )
        )
        console.print()

        # メタデータを表示
        console.print("[bold]メタデータ:[/bold]")
        console.print(f"  カテゴリ: [magenta]{metadata.category}[/magenta]")
        console.print(f"  作成者: {metadata.author}")
        console.print(f"  バージョン: {metadata.version}")
        console.print()

        # パラメーターを表示
        if metadata.parameters:
            console.print("[bold]パラメーター:[/bold]")
            for param in metadata.parameters:
                required = "[red]*[/red]" if param.required else ""
                default = f" (default: {param.default})" if param.default is not None else ""
                choices = f" [choices: {', '.join(param.choices)}]" if param.choices else ""

                console.print(
                    f"  {required} [cyan]{param.name}[/cyan] "
                    f"([yellow]{param.type}[/yellow]){default}{choices}"
                )
                console.print(f"    {param.description}")
            console.print()

    except Exception as e:
        console.print(f"[red]エラー: {e}[/red]")
        raise click.Abort from e


@template.command()
@click.argument("template_id", type=str)
@click.argument("output_dir", type=click.Path(path_type=Path))
@click.option(
    "--param",
    "-p",
    multiple=True,
    help="パラメーター (key=value 形式)",
)
@click.option(
    "--interactive",
    "-i",
    is_flag=True,
    help="対話モードでパラメーターを入力",
)
def generate(
    template_id: str,
    output_dir: Path,
    param: tuple[str, ...],
    interactive: bool,
) -> None:
    """テンプレートからプロジェクトを生成.

    使用例:

        \b
        # パラメーターを指定して生成
        $ agentflow template generate invoice-processor my-agent \\
            -p agent_name=my-invoice-processor \\
            -p database_type=postgresql

        \b
        # 対話モードで生成
        $ agentflow template generate chatbot my-chatbot -i

    Args:
        template_id: テンプレート ID
        output_dir: 出力ディレクトリ
        param: パラメーター (key=value 形式)
        interactive: 対話モードでパラメーターを入力
    """
    try:
        manager = TemplateManager()
        metadata = manager.get_template(template_id)

        if metadata is None:
            console.print(f"[red]テンプレートが見つかりません: {template_id}[/red]")
            raise click.Abort

        # パラメーターを解析
        parameters = {}
        for p in param:
            if "=" not in p:
                console.print(f"[red]無効なパラメーター形式: {p}[/red]")
                console.print("[yellow]正しい形式: key=value[/yellow]")
                raise click.Abort

            key, value = p.split("=", 1)
            parameters[key] = value

        # 対話モードの場合
        if interactive:
            console.print()
            console.print(f"[bold cyan]テンプレート: {metadata.name}[/bold cyan]")
            console.print(f"[dim]{metadata.description}[/dim]")
            console.print()

            for param_def in metadata.parameters:
                # デフォルト値を表示
                default_str = ""
                if param_def.default is not None:
                    default_str = f" [{param_def.default}]"

                # 選択肢がある場合
                if param_def.choices:
                    console.print(f"[cyan]{param_def.name}[/cyan] ({param_def.description})")
                    console.print(f"  選択肢: {', '.join(param_def.choices)}")
                    value = click.prompt(
                        f"  値{default_str}",
                        default=param_def.default,
                        type=click.Choice(param_def.choices),
                    )
                # 型に応じて入力
                elif param_def.type == "bool":
                    value = click.confirm(
                        f"{param_def.name} ({param_def.description})",
                        default=param_def.default or False,
                    )
                elif param_def.type == "int":
                    value = click.prompt(
                        f"{param_def.name} ({param_def.description}){default_str}",
                        default=param_def.default,
                        type=int,
                    )
                else:
                    value = click.prompt(
                        f"{param_def.name} ({param_def.description}){default_str}",
                        default=param_def.default,
                    )

                parameters[param_def.name] = value

            console.print()

        # プロジェクトを生成
        console.print("[cyan]プロジェクトを生成中...[/cyan]")
        manager.generate_project(template_id, output_dir, parameters)

        console.print()
        console.print(
            Panel(
                f"[green]✓[/green] プロジェクトを生成しました: [cyan]{output_dir}[/cyan]",
                border_style="green",
            )
        )
        console.print()
        console.print("[bold]次のステップ:[/bold]")
        console.print(f"  1. cd {output_dir}")
        console.print("  2. pip install -r requirements.txt")
        console.print("  3. python agent.py")
        console.print()

    except FileExistsError as e:
        console.print(f"[red]エラー: {e}[/red]")
        console.print("[yellow]別のディレクトリ名を指定してください。[/yellow]")
        raise click.Abort from e
    except ValueError as e:
        console.print(f"[red]エラー: {e}[/red]")
        raise click.Abort from e
    except Exception as e:
        console.print(f"[red]エラー: {e}[/red]")
        raise click.Abort from e
