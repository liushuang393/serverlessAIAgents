"""Migration Studio CLI エントリポイント.

使用例:
    python -m apps.migration_studio.cli migrate path/to/program.cbl \\
        --output /tmp/migration_output \\
        --fast
"""

from __future__ import annotations

import argparse
import logging
import os
import sys
import tempfile
from pathlib import Path
from typing import Any


def _setup_logging(verbose: bool) -> None:
    """ロギングを設定する."""
    level = logging.DEBUG if verbose else logging.INFO
    logging.basicConfig(
        level=level,
        format="%(asctime)s [%(levelname)s] %(name)s: %(message)s",
        datefmt="%H:%M:%S",
    )


def _validate_api_key() -> None:
    """ANTHROPIC_API_KEY の存在を確認する."""
    if not os.environ.get("ANTHROPIC_API_KEY"):
        # .env ファイルを試みる
        env_files = [
            Path(".env"),
            Path(__file__).parent.parent.parent / ".env",
        ]
        for env_file in env_files:
            if env_file.exists():
                from dotenv import load_dotenv  # type: ignore[import-not-found]
                load_dotenv(env_file)
                break

        if not os.environ.get("ANTHROPIC_API_KEY"):
            print(
                "エラー: ANTHROPIC_API_KEY が設定されていません。\n"
                "  export ANTHROPIC_API_KEY=sk-ant-... を実行するか、"
                ".env ファイルに設定してください。",
                file=sys.stderr,
            )
            sys.exit(1)


def cmd_migrate(args: argparse.Namespace) -> int:
    """migrate コマンドを実行する.

    Args:
        args: コマンドライン引数

    Returns:
        終了コード（0: 成功、1: 失敗）
    """
    from apps.migration_studio.pipeline.engine import run_migration_sync
    from apps.migration_studio.pipeline.project import COBOLProject

    source_path = Path(args.source).resolve()
    output_root = Path(args.output).resolve()
    model = args.model or os.environ.get("MIGRATION_MODEL", "claude-opus-4-6")

    print("\n🚀 Migration Studio")
    print(f"  ソース: {source_path}")
    print(f"  出力:   {output_root}")
    print(f"  モード: {'高速（実行比較スキップ）' if args.fast else '通常'}")
    print(f"  モデル: {model}")
    print()

    # 作業ディレクトリ（zip展開等）
    with tempfile.TemporaryDirectory(prefix="migration_") as work_dir_str:
        work_dir = Path(work_dir_str)

        # COBOLプロジェクトの初期化
        project = COBOLProject(source=source_path, work_dir=work_dir)
        try:
            project.setup()
        except (FileNotFoundError, ValueError) as exc:
            print(f"エラー: {exc}", file=sys.stderr)
            return 1

        cobol_files = project.get_cobol_files()
        if not cobol_files:
            print("エラー: 変換対象のCOBOLファイルが見つかりませんでした。", file=sys.stderr)
            return 1

        print(f"変換対象: {len(cobol_files)} ファイル")
        for f in cobol_files:
            print(f"  - {f.program_name} ({f.relative_path})")
        print()

        # 各COBOLファイルを変換
        exit_code = 0
        for cobol_file in cobol_files:
            print(f"⚙  {cobol_file.program_name} を変換中...")

            result = run_migration_sync(
                cobol_file=cobol_file,
                output_root=output_root,
                fast_mode=args.fast,
                model=model,
            )

            if result.success:
                print(f"  ✅ {cobol_file.program_name}: {result.decision}")
                print(f"     出力: {result.output_dir}")
            else:
                print(f"  ⚠  {cobol_file.program_name}: {result.decision}")
                if result.error_message:
                    print(f"     エラー: {result.error_message}")
                if result.decision == "ENV_ISSUE":
                    print(
                        "     ヒント: --fast フラグを使うと実行比較をスキップできます。",
                    )
                exit_code = max(exit_code, 1)

            print()

        return exit_code


def cmd_show(args: argparse.Namespace) -> int:
    """show コマンドを実行する（プログラムの成果物詳細表示）.

    Args:
        args: コマンドライン引数

    Returns:
        終了コード
    """
    from apps.migration_studio.output.organizer import OutputOrganizer

    output_root = Path(args.output).resolve()
    program_name = args.program.upper()

    organizer = OutputOrganizer(output_root)
    program_dir = organizer.get_program_dir(program_name)

    if not program_dir.exists():
        print(f"エラー: プログラムが見つかりません: {program_name}", file=sys.stderr)
        print(f"  検索パス: {program_dir}", file=sys.stderr)
        return 1

    version_count = organizer.get_version_count(program_name)
    print(f"\n📦 {program_name}  ({version_count} バージョン)")
    print(f"   ディレクトリ: {program_dir}")

    for v in range(1, version_count + 1):
        version_dir = organizer.get_version_dir(program_name, v)
        print(f"\n  ── v{v} ──────────────────────")

        # 各ステージの成果物を確認
        stage_dirs = {
            "analyzer":     "01_analysis/analyzer.json",
            "designer":     "02_design/designer.json",
            "transformer":  "03_transform/transformer.json",
            "test_generator": "03_transform/test_generator.json",
            "verifier":     "04_verification/verifier.json",
            "quality_gate": "04_verification/quality_gate.json",
        }
        for stage, rel_path in stage_dirs.items():
            artifact = version_dir / rel_path
            if artifact.exists():
                import json as _json
                try:
                    data = _json.loads(artifact.read_text(encoding="utf-8"))
                    decision = data.get("decision", "")
                    summary = f" → {decision}" if decision else ""
                    print(f"    ✓ {stage}{summary}")
                except Exception:
                    print(f"    ✓ {stage}")
            else:
                print(f"    · {stage}  (未実行)")

        # report
        report = version_dir / "05_report" / "report.md"
        if report.exists():
            print(f"    ✓ report  ({report})")

        # Java ファイル一覧
        java_dir = version_dir / "03_transform" / "src"
        if java_dir.exists():
            java_files = list(java_dir.rglob("*.java"))
            print(f"    📝 Java ファイル: {len(java_files)} 個")
            if args.verbose:
                for f in sorted(java_files):
                    print(f"       {f.relative_to(version_dir)}")

    # evolution.json があれば表示
    evolution_file = program_dir / "evolution.json"
    if evolution_file.exists():
        import json as _json
        ev = _json.loads(evolution_file.read_text(encoding="utf-8"))
        total = ev.get("total_iterations", 0)
        print(f"\n  🔄 Evolution 履歴: {total} 回")

    print()
    return 0


def cmd_retry(args: argparse.Namespace) -> int:
    """retry コマンドを実行する（特定ステージから再実行）.

    Args:
        args: コマンドライン引数

    Returns:
        終了コード（0: 成功、1: 失敗）
    """
    from apps.migration_studio.output.organizer import OutputOrganizer
    from apps.migration_studio.pipeline.engine import MigrationEngine
    from apps.migration_studio.pipeline.project import COBOLProject

    source_path = Path(args.source).resolve()
    output_root = Path(args.output).resolve()
    model = args.model or os.environ.get("MIGRATION_MODEL", "claude-opus-4-6")
    start_stage = args.stage

    valid_stages = ["analyzer", "designer", "transformer", "test_generator", "verifier", "quality_gate"]
    if start_stage not in valid_stages:
        print(f"エラー: 無効なステージ名: {start_stage}", file=sys.stderr)
        print(f"  有効なステージ: {', '.join(valid_stages)}", file=sys.stderr)
        return 1

    print("\n🔄 Migration Studio — ステージ再実行")
    print(f"  ソース: {source_path}")
    print(f"  出力:   {output_root}")
    print(f"  再実行ステージ: {start_stage} 以降")
    print(f"  モデル: {model}")
    print()

    with tempfile.TemporaryDirectory(prefix="migration_retry_") as work_dir_str:
        work_dir = Path(work_dir_str)

        project = COBOLProject(source=source_path, work_dir=work_dir)
        try:
            project.setup()
        except (FileNotFoundError, ValueError) as exc:
            print(f"エラー: {exc}", file=sys.stderr)
            return 1

        cobol_files = project.get_cobol_files()
        if not cobol_files:
            print("エラー: 変換対象のCOBOLファイルが見つかりませんでした。", file=sys.stderr)
            return 1

        engine = MigrationEngine(
            output_root=output_root,
            fast_mode=args.fast,
            model=model,
        )

        import anyio

        exit_code = 0
        for cobol_file in cobol_files:
            organizer = OutputOrganizer(output_root)
            version = organizer.get_version_count(cobol_file.program_name)
            if version == 0:
                print(f"  ⚠  {cobol_file.program_name}: 既存バージョンがありません。migrate を先に実行してください。", file=sys.stderr)
                exit_code = 1
                continue

            print(f"⚙  {cobol_file.program_name} (v{version}) を {start_stage} から再実行中...")

            result_decision = _run_retry_sync(engine, cobol_file, start_stage, anyio)

            if result_decision in ("PASSED", "KNOWN_LEGACY"):
                print(f"  ✅ {cobol_file.program_name}: {result_decision}")
            else:
                print(f"  ⚠  {cobol_file.program_name}: {result_decision}")
                exit_code = max(exit_code, 1)
            print()

        return exit_code


def _run_retry_sync(engine: Any, cobol_file: Any, start_stage: str, anyio: Any) -> str:
    """retry の1ファイル分の同期ラッパー."""
    result_decision = "UNKNOWN"

    async def _run() -> None:
        nonlocal result_decision
        async for event in engine.run_file_from_stage(
            cobol_file=cobol_file,
            start_stage=start_stage,
        ):
            _print_progress_cli(event)
            if event.event_type == "complete":
                result_decision = event.data.get("decision", "UNKNOWN")

    anyio.run(_run)
    return result_decision


def _print_progress_cli(event: "SSEEvent") -> None:  # type: ignore[name-defined]
    """CLIにイベント進捗を表示する."""
    icon_map = {
        "stage_start": "▶",
        "stage_complete": "✓",
        "evolution": "↺",
        "complete": "✅",
        "error": "✗",
    }
    icon = icon_map.get(event.event_type, "·")
    stage = event.stage or ""
    msg = event.data.get("message", "") or event.data.get("decision", "")
    print(f"  {icon} [{stage}] {msg}")


def cmd_list(args: argparse.Namespace) -> int:
    """list コマンドを実行する（移行成果物一覧表示）.

    Args:
        args: コマンドライン引数

    Returns:
        終了コード
    """
    output_root = Path(args.output).resolve()
    if not output_root.exists():
        print(f"エラー: 出力ディレクトリが存在しません: {output_root}", file=sys.stderr)
        return 1

    programs = [d for d in output_root.iterdir() if d.is_dir()]
    if not programs:
        print("移行済みプログラムはありません。")
        return 0

    print(f"移行済みプログラム一覧: {output_root}")
    for program_dir in sorted(programs):
        versions = sorted([d for d in program_dir.iterdir() if d.is_dir() and d.name.startswith("v")])
        print(f"\n  {program_dir.name} ({len(versions)} バージョン)")
        for v_dir in versions:
            report = v_dir / "05_report" / "report.md"
            status = "✓" if report.exists() else "·"
            print(f"    {status} {v_dir.name}")

    return 0


def main() -> None:
    """CLIメインエントリポイント."""
    parser = argparse.ArgumentParser(
        prog="migration_studio",
        description="COBOL→Java Spring Boot 自動移行ツール",
    )
    parser.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="詳細ログを表示",
    )

    subparsers = parser.add_subparsers(dest="command", help="コマンド")

    # migrate コマンド
    migrate_parser = subparsers.add_parser(
        "migrate",
        help="COBOLファイルをJava Spring Bootに変換する",
    )
    migrate_parser.add_argument(
        "source",
        help="COBOLファイルまたはzipアーカイブのパス",
    )
    migrate_parser.add_argument(
        "--output", "-o",
        default="migration_output",
        help="出力ディレクトリ（デフォルト: migration_output）",
    )
    migrate_parser.add_argument(
        "--fast",
        action="store_true",
        help="高速モード（実行比較をスキップ、java/gnucobol不要）",
    )
    migrate_parser.add_argument(
        "--model",
        default=None,
        help="使用するClaudeモデル（デフォルト: MIGRATION_MODEL 環境変数 or claude-opus-4-6）",
    )

    # list コマンド
    list_parser = subparsers.add_parser(
        "list",
        help="移行済みプログラムの一覧を表示する",
    )
    list_parser.add_argument(
        "--output", "-o",
        default="migration_output",
        help="出力ディレクトリ（デフォルト: migration_output）",
    )

    # show コマンド
    show_parser = subparsers.add_parser(
        "show",
        help="プログラムの移行成果物を詳細表示する",
    )
    show_parser.add_argument(
        "program",
        help="プログラム名（例: SAMPLE）",
    )
    show_parser.add_argument(
        "--output", "-o",
        default="migration_output",
        help="出力ディレクトリ（デフォルト: migration_output）",
    )

    # retry コマンド
    retry_parser = subparsers.add_parser(
        "retry",
        help="特定ステージから移行を再実行する（既存成果物を再利用）",
    )
    retry_parser.add_argument(
        "source",
        help="COBOLファイルまたはzipアーカイブのパス",
    )
    retry_parser.add_argument(
        "--stage", "-s",
        required=True,
        help="再実行を開始するステージ（analyzer/designer/transformer/test_generator/verifier/quality_gate）",
    )
    retry_parser.add_argument(
        "--output", "-o",
        default="migration_output",
        help="出力ディレクトリ（デフォルト: migration_output）",
    )
    retry_parser.add_argument(
        "--fast",
        action="store_true",
        help="高速モード（実行比較をスキップ）",
    )
    retry_parser.add_argument(
        "--model",
        default=None,
        help="使用するClaudeモデル（デフォルト: MIGRATION_MODEL 環境変数 or claude-opus-4-6）",
    )

    args = parser.parse_args()
    _setup_logging(args.verbose)

    if args.command is None:
        parser.print_help()
        sys.exit(0)

    _validate_api_key()

    if args.command == "migrate":
        sys.exit(cmd_migrate(args))
    elif args.command == "list":
        sys.exit(cmd_list(args))
    elif args.command == "show":
        sys.exit(cmd_show(args))
    elif args.command == "retry":
        sys.exit(cmd_retry(args))
    else:
        parser.print_help()
        sys.exit(1)


if __name__ == "__main__":
    main()
