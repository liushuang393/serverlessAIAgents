"""AgentFlow CLI skills コマンド.

このモジュールは Skills を管理するコマンドを提供します：
- list: 全 Skills を一覧表示
- show: Skill 詳細を表示
- create: 新しい Skill を作成
- validate: Skill を検証
- search: クエリで Skills を検索
"""

import re
from pathlib import Path

import click
from rich.console import Console
from rich.panel import Panel
from rich.prompt import Confirm, Prompt
from rich.table import Table

from agentflow.skills import (
    Skill,
    SkillLoader,
    SkillMatcher,
    SkillRegistry,
    SkillValidator,
)


console = Console()


def get_skill_dirs() -> list[Path]:
    """Skill ディレクトリ一覧を取得."""
    dirs = [
        Path.home() / ".agentflow" / "skills",
        Path.home() / ".agentflow" / "learned_skills",
        Path(".agentflow") / "skills",
    ]
    return [d for d in dirs if d.exists()]


def load_all_skills() -> list[Skill]:
    """全ディレクトリから Skills を読み込み."""
    registry = SkillRegistry()
    registry.clear()
    loader = SkillLoader(registry)
    skills: list[Skill] = []

    for skill_dir in get_skill_dirs():
        loaded = loader.load_directory(skill_dir, recursive=True)
        skills.extend(loaded)

    return skills


@click.group()
def skills() -> None:
    """Skills を管理.

    Skills は SKILL.md ファイルで定義された再利用可能な能力です。
    Claude Code Skills 形式と完全互換です。
    """


@skills.command(name="list")
@click.option(
    "--learned",
    "-l",
    is_flag=True,
    help="学習済み Skills のみ表示",
)
@click.option(
    "--project",
    "-p",
    is_flag=True,
    help="プロジェクト Skills のみ表示",
)
@click.pass_context
def list_skills(ctx: click.Context, learned: bool, project: bool) -> None:
    """全 Skills を一覧表示.

    例:
        agentflow skills list
        agentflow skills list --learned
    """
    all_skills = load_all_skills()

    if not all_skills:
        console.print("[yellow]No skills found.[/yellow]")
        console.print("[dim]Skill directories searched:[/dim]")
        for d in get_skill_dirs():
            console.print(f"  - {d}")
        return

    # フィルタリング
    if learned:
        Path.home() / ".agentflow" / "learned_skills"
        all_skills = [s for s in all_skills if s.metadata.learned]
    elif project:
        Path(".agentflow") / "skills"
        # プロジェクト Skills は learned フラグなし
        all_skills = [s for s in all_skills if not s.metadata.learned]

    # テーブル表示
    table = Table(title="Skills", show_header=True, header_style="bold cyan")
    table.add_column("Name", style="green")
    table.add_column("Version", style="dim")
    table.add_column("Description", max_width=40)
    table.add_column("Triggers", style="yellow")
    table.add_column("Learned", style="magenta")

    for skill in all_skills:
        triggers = ", ".join(skill.metadata.triggers[:3])
        if len(skill.metadata.triggers) > 3:
            triggers += "..."
        table.add_row(
            skill.name,
            skill.metadata.version,
            skill.metadata.description[:40] + "..." if len(skill.metadata.description) > 40 else skill.metadata.description,
            triggers,
            "✓" if skill.metadata.learned else "",
        )

    console.print(table)
    console.print(f"\n[dim]Total: {len(all_skills)} skills[/dim]")


@skills.command()
@click.argument("name")
@click.pass_context
def show(ctx: click.Context, name: str) -> None:
    """Skill の詳細を表示.

    NAME: Skill 名

    例:
        agentflow skills show pdf-extractor
    """
    all_skills = load_all_skills()
    skill = next((s for s in all_skills if s.name == name), None)

    if not skill:
        console.print(f"[red]Skill not found: {name}[/red]")
        return

    # 詳細パネル
    meta = skill.metadata
    info_lines = [
        f"[bold]Name:[/bold] {meta.name}",
        f"[bold]Version:[/bold] {meta.version}",
        f"[bold]Author:[/bold] {meta.author}",
        f"[bold]Description:[/bold] {meta.description}",
        f"[bold]Triggers:[/bold] {', '.join(meta.triggers) or 'None'}",
        f"[bold]Tags:[/bold] {', '.join(meta.tags) or 'None'}",
        f"[bold]Requirements:[/bold] {', '.join(meta.requirements) or 'None'}",
        f"[bold]Learned:[/bold] {'Yes' if meta.learned else 'No'}",
        f"[bold]Confidence:[/bold] {meta.confidence:.2f}",
        f"[bold]Usage Count:[/bold] {meta.usage_count}",
    ]

    console.print(Panel(
        "\n".join(info_lines),
        title=f"Skill: {name}",
        border_style="cyan",
    ))

    # Instructions
    console.print("\n[bold cyan]Instructions:[/bold cyan]")
    console.print(Panel(skill.instructions, border_style="dim"))


@skills.command()
@click.argument("name")
@click.option("--description", "-d", help="Skill の説明")
@click.option("--triggers", "-t", help="トリガーワード（カンマ区切り）")
@click.option("--scope", "-s", type=click.Choice(["project", "global"]), default="project", help="保存先")
@click.option("--interactive", "-i", is_flag=True, help="対話モードで作成")
@click.pass_context
def create(
    ctx: click.Context,
    name: str,
    description: str | None,
    triggers: str | None,
    scope: str,
    interactive: bool,
) -> None:
    """新しい Skill を作成.

    NAME: Skill 名 (kebab-case)

    例:
        agentflow skills create my-skill
        agentflow skills create my-skill --interactive
        agentflow skills create my-skill -d "My skill description" -t "my,skill"
    """
    # kebab-case 検証
    if not re.match(r"^[a-z0-9]+(-[a-z0-9]+)*$", name):
        console.print(f"[red]Invalid skill name: {name}[/red]")
        console.print("[dim]Name must be in kebab-case format (e.g., my-skill)[/dim]")
        return

    # 対話モード
    if interactive:
        console.print(Panel("[bold cyan]Create New Skill[/bold cyan]", border_style="cyan"))

        if not description:
            description = Prompt.ask("Description")

        if not triggers:
            triggers = Prompt.ask("Triggers (comma-separated)", default="")

        instructions = Prompt.ask("Instructions (basic content)", default="# Instructions\n\nAdd your instructions here.")

    else:
        description = description or f"A skill for {name}"
        instructions = "# Instructions\n\nAdd your instructions here."

    # トリガーリストを作成
    trigger_list = [t.strip() for t in (triggers or "").split(",") if t.strip()]

    # SKILL.md を生成
    skill_content = f"""---
name: {name}
description: {description}
version: 1.0.0
triggers:
{chr(10).join(f'  - {t}' for t in trigger_list) if trigger_list else '  []'}
tags: []
---

{instructions}
"""

    # 保存先決定
    if scope == "project":
        base_dir = Path(".agentflow") / "skills"
    else:
        base_dir = Path.home() / ".agentflow" / "skills"

    skill_dir = base_dir / name
    skill_dir.mkdir(parents=True, exist_ok=True)

    skill_file = skill_dir / "SKILL.md"
    if skill_file.exists() and not Confirm.ask(f"[yellow]Overwrite existing {skill_file}?[/yellow]"):
        console.print("[dim]Cancelled.[/dim]")
        return

    skill_file.write_text(skill_content, encoding="utf-8")

    console.print(Panel(
        f"[green]✓ Skill created successfully![/green]\n\n"
        f"Location: {skill_file}\n\n"
        f"Next steps:\n"
        f"  1. Edit {skill_file} to add instructions\n"
        f"  2. Run: agentflow skills show {name}",
        title="Success",
        border_style="green",
    ))


@skills.command()
@click.argument("path", type=click.Path(exists=True, path_type=Path))
@click.option("--strict", is_flag=True, help="厳格モード（警告もエラーとして扱う）")
@click.pass_context
def validate(ctx: click.Context, path: Path, strict: bool) -> None:
    """Skill を検証.

    PATH: Skill ディレクトリまたは SKILL.md ファイルのパス

    例:
        agentflow skills validate .agentflow/skills/my-skill
        agentflow skills validate ./SKILL.md --strict
    """
    # パスを解決
    if path.is_file() and path.name == "SKILL.md":
        skill_dir = path.parent
    elif path.is_dir():
        skill_dir = path
    else:
        console.print(f"[red]Invalid path: {path}[/red]")
        return

    # Skill を読み込み
    try:
        skill = Skill.load(skill_dir)
    except Exception as e:
        console.print(f"[red]Failed to load skill: {e}[/red]")
        return

    # 検証
    validator = SkillValidator(strict=strict)
    result = validator.validate(skill)

    # 結果表示
    if result.valid:
        console.print(Panel(
            f"[green]✓ Skill '{skill.name}' is valid![/green]",
            title="Validation Passed",
            border_style="green",
        ))
    else:
        error_lines = [f"[red]✗ {e}[/red]" for e in result.errors]
        console.print(Panel(
            "\n".join(error_lines),
            title="Validation Failed",
            border_style="red",
        ))

    # 警告表示
    if result.warnings:
        warning_lines = [f"[yellow]⚠ {w}[/yellow]" for w in result.warnings]
        console.print(Panel(
            "\n".join(warning_lines),
            title="Warnings",
            border_style="yellow",
        ))


@skills.command()
@click.argument("query")
@click.option("--top", "-n", default=5, help="表示する結果数")
@click.pass_context
def search(ctx: click.Context, query: str, top: int) -> None:
    """クエリで Skills を検索.

    QUERY: 検索クエリ（自然言語）

    例:
        agentflow skills search "PDF からテキスト抽出"
        agentflow skills search "excel spreadsheet" --top 10
    """
    all_skills = load_all_skills()

    if not all_skills:
        console.print("[yellow]No skills available to search.[/yellow]")
        return

    # マッチング
    matcher = SkillMatcher(all_skills)
    results = matcher.match(query, top_k=top)

    if not results:
        console.print(f"[yellow]No skills matched for: {query}[/yellow]")
        return

    # テーブル表示
    table = Table(title=f"Search Results for: '{query}'", show_header=True, header_style="bold cyan")
    table.add_column("#", style="dim", width=3)
    table.add_column("Name", style="green")
    table.add_column("Score", style="yellow")
    table.add_column("Reason", max_width=40)
    table.add_column("Description", max_width=30)

    for i, r in enumerate(results, 1):
        table.add_row(
            str(i),
            r.skill.name,
            f"{r.score:.2f}",
            r.reason,
            r.skill.metadata.description[:30] + "..." if len(r.skill.metadata.description) > 30 else r.skill.metadata.description,
        )

    console.print(table)


@skills.command()
@click.argument("name")
@click.option("--scope", "-s", type=click.Choice(["learned", "project", "global"]), default="learned", help="削除対象")
@click.option("--force", "-f", is_flag=True, help="確認なしで削除")
@click.pass_context
def delete(ctx: click.Context, name: str, scope: str, force: bool) -> None:
    """Skill を削除.

    NAME: Skill 名

    例:
        agentflow skills delete my-skill
        agentflow skills delete my-skill --scope project --force
    """
    # 削除先決定
    if scope == "learned":
        base_dir = Path.home() / ".agentflow" / "learned_skills"
    elif scope == "project":
        base_dir = Path(".agentflow") / "skills"
    else:
        base_dir = Path.home() / ".agentflow" / "skills"

    skill_dir = base_dir / name

    if not skill_dir.exists():
        console.print(f"[red]Skill not found: {skill_dir}[/red]")
        return

    if not force and not Confirm.ask(f"[yellow]Delete skill '{name}' at {skill_dir}?[/yellow]"):
        console.print("[dim]Cancelled.[/dim]")
        return

    # 削除
    import shutil
    shutil.rmtree(skill_dir)
    console.print(f"[green]✓ Deleted skill: {name}[/green]")

