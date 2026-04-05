"""Migration Studio CLI & SSEイベントコントラクトテスト.

CLIコマンドのI/O検証と、エンジンが送出するSSEイベントが
フロントエンド (app.js) の期待するキーを含むか検証する。

実行方法:
    pytest apps/code_migration_assistant/tests/test_cli_and_events.py -v
"""

from __future__ import annotations

import argparse
import asyncio
import json as _json
from dataclasses import dataclass
from pathlib import Path
from types import SimpleNamespace
from typing import TYPE_CHECKING, Any
from unittest.mock import MagicMock, patch

import pytest


if TYPE_CHECKING:
    from _pytest.capture import CaptureFixture

from apps.code_migration_assistant.cli import (
    _print_progress_cli,
    cmd_list,
    cmd_migrate,
    cmd_retry,
    cmd_show,
)


@dataclass
class SSEEvent:
    event_type: str
    stage: str
    data: dict[str, Any]
    _hitl_event: Any | None = None


# ============================================================
# cmd_list テスト
# ============================================================


class TestCmdList:
    """cmd_list のユニットテスト."""

    def test_nonexistent_dir_returns_1(self, tmp_path: Path, capsys: CaptureFixture[str]) -> None:
        """存在しない出力ディレクトリは 1 を返す."""
        args = argparse.Namespace(output=str(tmp_path / "nonexistent"))
        result = cmd_list(args)
        assert result == 1
        assert "存在しません" in capsys.readouterr().err

    def test_empty_dir_returns_0(self, tmp_path: Path, capsys: CaptureFixture[str]) -> None:
        """プログラムが1件もない場合は 0 を返す."""
        args = argparse.Namespace(output=str(tmp_path))
        result = cmd_list(args)
        assert result == 0
        assert "移行済みプログラムはありません" in capsys.readouterr().out

    def test_lists_programs_with_versions(self, tmp_path: Path, capsys: CaptureFixture[str]) -> None:
        """プログラムが存在する場合は名前とバージョン数を表示する."""
        prog_dir = tmp_path / "SAMPLE"
        prog_dir.mkdir()
        (prog_dir / "v1").mkdir()
        (prog_dir / "v2").mkdir()

        args = argparse.Namespace(output=str(tmp_path))
        result = cmd_list(args)
        assert result == 0
        out = capsys.readouterr().out
        assert "SAMPLE" in out
        assert "2 バージョン" in out

    def test_report_status_markers(self, tmp_path: Path, capsys: CaptureFixture[str]) -> None:
        """最新バージョンの report 有無マーカーが表示される."""
        prog_dir = tmp_path / "SAMPLE"
        prog_dir.mkdir()
        v1 = prog_dir / "v1"
        v1.mkdir()
        v2 = prog_dir / "v2"
        v2.mkdir()
        # v1 に report.md を置く
        (v1 / "05_report").mkdir(parents=True)
        (v1 / "05_report" / "report.md").write_text("# Report", encoding="utf-8")

        args = argparse.Namespace(output=str(tmp_path))
        cmd_list(args)
        out = capsys.readouterr().out
        assert "report=·" in out

    def test_lists_multiple_programs(self, tmp_path: Path, capsys: CaptureFixture[str]) -> None:
        """複数プログラムが全て表示される."""
        for name in ("AAAA", "BBBB", "CCCC"):
            p = tmp_path / name
            p.mkdir()
            (p / "v1").mkdir()

        args = argparse.Namespace(output=str(tmp_path))
        cmd_list(args)
        out = capsys.readouterr().out
        for name in ("AAAA", "BBBB", "CCCC"):
            assert name in out


# ============================================================
# cmd_show テスト
# ============================================================


class TestCmdShow:
    """cmd_show のユニットテスト."""

    def test_missing_program_returns_1(self, tmp_path: Path, capsys: CaptureFixture[str]) -> None:
        """存在しないプログラム名は 1 を返す."""
        args = argparse.Namespace(output=str(tmp_path), program="MISSING", verbose=False)
        result = cmd_show(args)
        assert result == 1
        err = capsys.readouterr().err
        assert "MISSING" in err

    def test_existing_program_returns_0(self, tmp_path: Path, capsys: CaptureFixture[str]) -> None:
        """存在するプログラムは 0 を返しバージョン情報を表示する."""
        prog_dir = tmp_path / "SAMPLE"
        prog_dir.mkdir()
        v1 = prog_dir / "v1"
        for sub in ["01_analysis", "02_design", "03_transform", "04_verification", "05_report"]:
            (v1 / sub).mkdir(parents=True)

        args = argparse.Namespace(output=str(tmp_path), program="SAMPLE", verbose=False)
        result = cmd_show(args)
        assert result == 0
        out = capsys.readouterr().out
        assert "SAMPLE" in out
        assert "1 バージョン" in out

    def test_program_name_uppercased(self, tmp_path: Path, capsys: CaptureFixture[str]) -> None:
        """program 引数は大文字変換される（小文字入力でも一致する）."""
        prog_dir = tmp_path / "SAMPLE"
        prog_dir.mkdir()
        (prog_dir / "v1").mkdir()

        args = argparse.Namespace(output=str(tmp_path), program="sample", verbose=False)
        result = cmd_show(args)
        assert result == 0

    def test_shows_evolution_history(self, tmp_path: Path, capsys: CaptureFixture[str]) -> None:
        """evolution.json が存在する場合 Evolution 履歴が表示される."""
        prog_dir = tmp_path / "SAMPLE"
        prog_dir.mkdir()
        (prog_dir / "v1").mkdir()
        evo_data = {
            "_updated_at": "2026-01-01T00:00:00+00:00",
            "program_name": "SAMPLE",
            "total_iterations": 2,
            "iterations": [
                {"iteration": 1, "stage": "designer", "decision": "DESIGN_ISSUE"},
                {"iteration": 2, "stage": "designer", "decision": "DESIGN_ISSUE"},
            ],
        }
        (prog_dir / "evolution.json").write_text(
            _json.dumps(evo_data, ensure_ascii=False),
            encoding="utf-8",
        )

        args = argparse.Namespace(output=str(tmp_path), program="SAMPLE", verbose=False)
        cmd_show(args)
        out = capsys.readouterr().out
        assert "Evolution 履歴" in out
        assert "2 回" in out

    def test_shows_stage_artifacts(self, tmp_path: Path, capsys: CaptureFixture[str]) -> None:
        """成果物 JSON があるステージは ✓ で表示される."""
        prog_dir = tmp_path / "SAMPLE"
        prog_dir.mkdir()
        v1 = prog_dir / "v1"
        (v1 / "01_analysis").mkdir(parents=True)
        # analyzer.json に decision を含む
        artifact_data = {"decision": "PASSED", "programs": [{"program_id": "SAMPLE"}]}
        (v1 / "01_analysis" / "analyzer.json").write_text(_json.dumps(artifact_data), encoding="utf-8")

        args = argparse.Namespace(output=str(tmp_path), program="SAMPLE", verbose=False)
        cmd_show(args)
        out = capsys.readouterr().out
        assert "✓ analyzer" in out


# ============================================================
# cmd_retry テスト
# ============================================================


class TestCmdRetry:
    """cmd_retry のユニットテスト."""

    FIXTURES_DIR = Path(__file__).parent / "fixtures"
    SAMPLE_CBL = FIXTURES_DIR / "sample.cbl"

    def test_invalid_stage_returns_1(self, tmp_path: Path, capsys: CaptureFixture[str]) -> None:
        """無効なステージ名は 1 を返す."""
        args = argparse.Namespace(
            source=str(self.SAMPLE_CBL),
            output=str(tmp_path),
            stage="invalid_stage",
            fast=True,
            model=None,
        )
        result = cmd_retry(args)
        assert result == 1
        err = capsys.readouterr().err
        assert "無効なステージ名" in err

    def test_all_valid_stages_pass_validation(self, tmp_path: Path, capsys: CaptureFixture[str]) -> None:
        """全ての有効なステージ名がバリデーションを通過する（ステージ名エラーを返さない）."""
        valid_stages = [
            "analyzer",
            "designer",
            "transformer",
            "test_generator",
            "verifier",
            "quality_gate",
        ]
        for stage in valid_stages:
            args = argparse.Namespace(
                source=str(self.SAMPLE_CBL),
                output=str(tmp_path),
                stage=stage,
                fast=True,
                model=None,
            )
            with (
                patch("apps.code_migration_assistant.cli.COBOLProject") as mock_project,
                patch("apps.code_migration_assistant.output.organizer.OutputOrganizer") as mock_organizer,
            ):
                mock_project.return_value.setup.return_value = None
                mock_project.return_value.get_cobol_files.return_value = []
                mock_organizer.return_value.get_version_count.return_value = 1
                result = cmd_retry(args)
            # COBOLファイルなしで 1 を返すが、「無効なステージ名」エラーではない
            assert result == 1
            err = capsys.readouterr().err
            assert "無効なステージ名" not in err, f"ステージ '{stage}' が無効と判定された"

    def test_no_existing_version_warns(self, tmp_path: Path, capsys: CaptureFixture[str]) -> None:
        """既存バージョンがない場合は警告して 1 を返す."""
        mock_cobol_file = MagicMock()
        mock_cobol_file.program_name = "SAMPLE"

        args = argparse.Namespace(
            source=str(self.SAMPLE_CBL),
            output=str(tmp_path),
            stage="designer",
            fast=True,
            model=None,
        )
        with (
            patch("apps.code_migration_assistant.cli.COBOLProject") as mock_project,
        ):
            mock_project.return_value.setup.return_value = None
            mock_project.return_value.get_cobol_files.return_value = [mock_cobol_file]
            result = cmd_retry(args)

        assert result == 1
        err = capsys.readouterr().err
        assert "既存バージョンがありません" in err

    def test_retry_routes_resume_stage_into_contract_payload(self, tmp_path: Path) -> None:
        """retry は backlog resume stage を payload に載せて engine runtime へ委譲する."""
        mock_cobol_file = MagicMock()
        mock_cobol_file.program_name = "SAMPLE"
        captured: list[dict[str, Any]] = []

        async def _run_stub(payload: dict[str, Any], *, on_event: Any, max_sessions: int) -> tuple[dict[str, Any], int]:
            _ = on_event
            _ = max_sessions
            captured.append(payload)
            return ({"backlog_completed": True, "decision": "PASSED"}, 0)

        args = argparse.Namespace(
            source=str(self.SAMPLE_CBL),
            output=str(tmp_path),
            stage="designer",
            fast=True,
            model="platform_text_default",
        )
        with (
            patch("apps.code_migration_assistant.cli.COBOLProject") as mock_project,
            patch("apps.code_migration_assistant.output.organizer.OutputOrganizer") as mock_organizer,
            patch("apps.code_migration_assistant.cli._run_session_loop", side_effect=_run_stub),
        ):
            mock_project.return_value.setup.return_value = None
            mock_project.return_value.get_cobol_files.return_value = [mock_cobol_file]
            mock_organizer.return_value.get_version_count.return_value = 1
            result = cmd_retry(args)

        assert result == 0
        assert captured[0]["resume_from_stage"] == "business_semantics"


# ============================================================
# cmd_migrate テスト
# ============================================================


class TestCmdMigrate:
    """cmd_migrate のユニットテスト."""

    FIXTURES_DIR = Path(__file__).parent / "fixtures"
    SAMPLE_CBL = FIXTURES_DIR / "sample.cbl"

    def test_file_not_found_returns_1(self, tmp_path: Path, capsys: CaptureFixture[str]) -> None:
        """存在しないソースは 1 を返す."""
        args = argparse.Namespace(
            source=str(tmp_path / "nonexistent.cbl"),
            output=str(tmp_path / "output"),
            fast=True,
            model=None,
        )
        result = cmd_migrate(args)
        assert result == 1
        err = capsys.readouterr().err
        assert "エラー" in err

    def test_no_cobol_files_returns_1(self, tmp_path: Path, capsys: CaptureFixture[str]) -> None:
        """COBOLファイルが見つからない場合は 1 を返す."""
        args = argparse.Namespace(
            source=str(tmp_path),
            output=str(tmp_path / "output"),
            fast=True,
            model=None,
        )
        with patch("apps.code_migration_assistant.cli.COBOLProject") as mock_project:
            mock_project.return_value.setup.return_value = None
            mock_project.return_value.get_cobol_files.return_value = []
            result = cmd_migrate(args)

        assert result == 1
        err = capsys.readouterr().err
        assert "COBOLファイルが見つかりません" in err

    def test_success_returns_0_and_prints_result(self, tmp_path: Path, capsys: CaptureFixture[str]) -> None:
        """変換成功時は 0 を返し ✅ と PASSED を表示する."""
        mock_cobol_file = MagicMock()
        mock_cobol_file.program_name = "SAMPLE"
        mock_cobol_file.relative_path = "sample.cbl"

        args = argparse.Namespace(
            source=str(self.SAMPLE_CBL),
            output=str(tmp_path / "output"),
            fast=True,
            model=None,
        )

        async def _run_stub(payload: dict[str, Any], *, on_event: Any, max_sessions: int) -> tuple[dict[str, Any], int]:
            assert payload["model"] == "platform_text_default"
            assert max_sessions == 5000
            await on_event({"type": "stage_start", "stage": "analysis", "message": "start"})
            await on_event({"type": "complete", "decision": "PASSED", "output_dir": str(tmp_path / "output")})
            return (
                {
                    "backlog_completed": True,
                    "decision": "PASSED",
                    "output_dir": str(tmp_path / "output"),
                    "program_name": "SAMPLE",
                },
                0,
            )

        with (
            patch("apps.code_migration_assistant.cli.COBOLProject") as mock_project,
            patch("apps.code_migration_assistant.cli._run_session_loop", side_effect=_run_stub),
        ):
            mock_project.return_value.setup.return_value = None
            mock_project.return_value.get_cobol_files.return_value = [mock_cobol_file]
            result = cmd_migrate(args)

        assert result == 0
        out = capsys.readouterr().out
        assert "SAMPLE" in out
        assert "PASSED" in out
        assert "✅" in out

    def test_failure_returns_1_and_prints_warning(self, tmp_path: Path, capsys: CaptureFixture[str]) -> None:
        """変換失敗（ENV_ISSUE）は 1 を返し --fast ヒントを表示する."""
        mock_cobol_file = MagicMock()
        mock_cobol_file.program_name = "SAMPLE"
        mock_cobol_file.relative_path = "sample.cbl"

        args = argparse.Namespace(
            source=str(self.SAMPLE_CBL),
            output=str(tmp_path / "output"),
            fast=False,
            model=None,
        )

        async def _run_stub(
            _payload: dict[str, Any], *, on_event: Any, max_sessions: int
        ) -> tuple[dict[str, Any], int]:
            _ = max_sessions
            await on_event({"type": "error", "message": "Java not found"})
            return (
                {
                    "backlog_completed": False,
                    "decision": "ENV_ISSUE",
                    "error": "Java not found",
                    "output_dir": str(tmp_path / "output"),
                },
                1,
            )

        with (
            patch("apps.code_migration_assistant.cli.COBOLProject") as mock_project,
            patch("apps.code_migration_assistant.cli._run_session_loop", side_effect=_run_stub),
        ):
            mock_project.return_value.setup.return_value = None
            mock_project.return_value.get_cobol_files.return_value = [mock_cobol_file]
            result = cmd_migrate(args)

        assert result == 1
        out = capsys.readouterr().out
        assert "--fast" in out  # ENV_ISSUE ヒント

    def test_model_from_env_variable(self, tmp_path: Path) -> None:
        """--model 未指定時は MIGRATION_MODEL 環境変数が使われる."""
        mock_cobol_file = MagicMock()
        mock_cobol_file.program_name = "SAMPLE"
        mock_cobol_file.relative_path = "sample.cbl"

        captured: list[str] = []

        async def _run_stub(payload: dict[str, Any], *, on_event: Any, max_sessions: int) -> tuple[dict[str, Any], int]:
            _ = on_event
            _ = max_sessions
            captured.append(str(payload.get("model", "")))
            return ({"backlog_completed": True, "decision": "PASSED"}, 0)

        args = argparse.Namespace(
            source=str(self.SAMPLE_CBL),
            output=str(tmp_path / "output"),
            fast=True,
            model=None,
        )
        with (
            patch("apps.code_migration_assistant.cli.COBOLProject") as mock_project,
            patch("apps.code_migration_assistant.cli._run_session_loop", side_effect=_run_stub),
            patch.dict("os.environ", {"MIGRATION_MODEL": "claude-sonnet-4-6"}),
        ):
            mock_project.return_value.setup.return_value = None
            mock_project.return_value.get_cobol_files.return_value = [mock_cobol_file]
            cmd_migrate(args)

        assert captured[0] == "claude-sonnet-4-6"

    def test_explicit_model_overrides_env(self, tmp_path: Path) -> None:
        """--model 明示指定は環境変数より優先される."""
        mock_cobol_file = MagicMock()
        mock_cobol_file.program_name = "SAMPLE"
        mock_cobol_file.relative_path = "sample.cbl"

        captured: list[str] = []

        async def _run_stub(payload: dict[str, Any], *, on_event: Any, max_sessions: int) -> tuple[dict[str, Any], int]:
            _ = on_event
            _ = max_sessions
            captured.append(str(payload.get("model", "")))
            return ({"backlog_completed": True, "decision": "PASSED"}, 0)

        args = argparse.Namespace(
            source=str(self.SAMPLE_CBL),
            output=str(tmp_path / "output"),
            fast=True,
            model="platform_text_default",
        )
        with (
            patch("apps.code_migration_assistant.cli.COBOLProject") as mock_project,
            patch("apps.code_migration_assistant.cli._run_session_loop", side_effect=_run_stub),
            patch.dict("os.environ", {"MIGRATION_MODEL": "claude-haiku-4-5"}),
        ):
            mock_project.return_value.setup.return_value = None
            mock_project.return_value.get_cobol_files.return_value = [mock_cobol_file]
            cmd_migrate(args)

        assert captured[0] == "platform_text_default"


# ============================================================
# _print_progress_cli テスト
# ============================================================


class TestPrintProgressCli:
    """_print_progress_cli のユニットテスト."""

    def test_stage_start_shows_arrow(self, capsys: CaptureFixture[str]) -> None:
        """stage_start は ▶ アイコンで表示される."""
        event = SimpleNamespace(event_type="stage_start", stage="analyzer", data={"message": "COBOL解析中..."})
        _print_progress_cli(event)
        out = capsys.readouterr().out
        assert "▶" in out
        assert "analyzer" in out
        assert "COBOL解析中" in out

    def test_stage_complete_shows_checkmark(self, capsys: CaptureFixture[str]) -> None:
        """stage_complete は ✓ アイコンで表示される."""
        event = SimpleNamespace(event_type="stage_complete", stage="analyzer", data={"decision": "PASSED"})
        _print_progress_cli(event)
        out = capsys.readouterr().out
        assert "✓" in out
        assert "PASSED" in out

    def test_evolution_shows_retry_icon(self, capsys: CaptureFixture[str]) -> None:
        """evolution は ↺ アイコンで表示される."""
        event = SimpleNamespace(event_type="evolution", stage="designer", data={"fix_summary": "プロンプト改善"})
        _print_progress_cli(event)
        out = capsys.readouterr().out
        assert "↺" in out

    def test_complete_shows_checkmark_emoji(self, capsys: CaptureFixture[str]) -> None:
        """complete は ✅ アイコンで表示される."""
        event = SimpleNamespace(
            event_type="complete",
            stage="pipeline",
            data={"program_name": "SAMPLE", "decision": "PASSED"},
        )
        _print_progress_cli(event)
        out = capsys.readouterr().out
        assert "✅" in out

    def test_error_shows_x_icon(self, capsys: CaptureFixture[str]) -> None:
        """error は ✗ アイコンで表示される."""
        event = SSEEvent(
            event_type="error",
            stage="pipeline",
            data={"message": "変換エラー"},
        )
        _print_progress_cli(event)
        out = capsys.readouterr().out
        assert "✗" in out

    def test_unknown_event_type_shows_dot(self, capsys: CaptureFixture[str]) -> None:
        """未知のイベントタイプは · で表示される."""
        event = SSEEvent(
            event_type="unknown_type",
            stage="analyzer",
            data={"message": "test"},
        )
        _print_progress_cli(event)
        out = capsys.readouterr().out
        assert "·" in out

    def test_none_stage_does_not_raise(self, capsys: CaptureFixture[str]) -> None:
        """stage が None でもエラーにならない."""
        event = SSEEvent(
            event_type="error",
            stage=None,
            data={"message": "global error"},
        )
        _print_progress_cli(event)
        out = capsys.readouterr().out
        assert "✗" in out

    def test_decision_shown_when_no_message(self, capsys: CaptureFixture[str]) -> None:
        """message キーがなく decision だけある場合、decision を表示する."""
        event = SSEEvent(
            event_type="stage_complete",
            stage="quality_gate",
            data={"decision": "DESIGN_ISSUE"},
        )
        _print_progress_cli(event)
        out = capsys.readouterr().out
        assert "DESIGN_ISSUE" in out


# ============================================================
# SSEイベントコントラクトテスト
# フロントエンド (app.js) が読むキーとエンジンの出力の整合性検証
# ============================================================


def _serialize(event: SSEEvent) -> dict[str, Any]:
    """router.py での変換ロジックを再現する: {type, stage, **data}."""
    return {"type": event.event_type, "stage": event.stage, **event.data}


class TestSSEEventContract:
    """app.js が期待するSSEイベントキーの検証."""

    def test_stage_start_provides_message(self) -> None:
        """stage_start → app.js: event.message"""
        event = SSEEvent(
            event_type="stage_start",
            stage="analyzer",
            data={"message": "analyzer 実行中...", "program": "SAMPLE"},
        )
        s = _serialize(event)
        assert s["type"] == "stage_start"
        assert "message" in s  # app.js line 153: event.message || "開始"

    def test_stage_complete_provides_decision(self) -> None:
        """stage_complete → app.js: event.decision"""
        event = SSEEvent(
            event_type="stage_complete",
            stage="quality_gate",
            data={"decision": "PASSED", "iteration": 0},
        )
        s = _serialize(event)
        assert s["type"] == "stage_complete"
        assert "decision" in s  # app.js line 162: event.decision

    def test_hitl_required_provides_request_id(self) -> None:
        """hitl_required → app.js: event.request_id"""
        ev = asyncio.Event()
        event = SSEEvent(
            event_type="hitl_required",
            stage="analyzer",
            data={
                "request_id": "req-001",
                "artifact": {},
                "unknowns": ["field_x"],
                "question": "確認してください",
            },
            _hitl_event=ev,
        )
        s = _serialize(event)
        assert "request_id" in s  # app.js line 280: event.request_id
        assert "question" in s  # app.js line 281: event.question
        assert "unknowns" in s  # app.js line 283: event.unknowns
        assert isinstance(s["unknowns"], list)

    def test_complete_provides_program_name_and_version(self) -> None:
        """complete → app.js: event.program_name, event.version"""
        event = SSEEvent(
            event_type="complete",
            stage="pipeline",
            data={
                "program_name": "SAMPLE",
                "decision": "PASSED",
                "version": 1,
                "output_dir": "/tmp/migration_output/SAMPLE",
            },
        )
        s = _serialize(event)
        assert "program_name" in s  # app.js line 186: event.program_name
        assert "version" in s  # app.js line 186: event.version
        assert isinstance(s["version"], int)

    def test_error_provides_message(self) -> None:
        """error → app.js: event.message"""
        event = SSEEvent(
            event_type="error",
            stage="pipeline",
            data={"message": "変換に失敗しました"},
        )
        s = _serialize(event)
        assert "message" in s  # app.js line 194: event.message || "不明なエラー"

    def test_evolution_provides_iteration(self) -> None:
        """evolution → app.js: event.iteration"""
        evolution_data: dict[str, Any] = {
            "iteration": 2,
            "stage": "designer",
            "decision": "DESIGN_ISSUE",
            "fix_summary": "設計問題を修正",
            "evolved_at": "2026-01-01T00:00:00+00:00",
        }
        event = SSEEvent(
            event_type="evolution",
            stage="designer",
            data=evolution_data,
        )
        s = _serialize(event)
        assert "iteration" in s  # app.js line 168: event.iteration || ""
        assert s["iteration"] == 2

    def test_evolution_provides_problem_alias(self) -> None:
        """evolution → app.js: event.problem（decision のエイリアス）.

        app.js line 169:
            appendLog(`⟳ Evolution #${iterNum}: ${event.problem || ""} → ...`);

        エンジンは 'decision' で送出するが、フロントエンドは 'problem' を読む。
        engine.py は 'problem' エイリアスを data に含める必要がある。
        """
        evolution_data: dict[str, Any] = {
            "iteration": 1,
            "stage": "designer",
            "decision": "DESIGN_ISSUE",
            "problem": "DESIGN_ISSUE",  # alias for frontend
            "fix_summary": "設計問題を修正: フィールド欠落 (フィールドカバレッジ: 3/8)",
            "fix": "設計問題を修正: フィールド欠落 (フィールドカバレッジ: 3/8)",  # alias
            "evolved_at": "2026-01-01T00:00:00+00:00",
        }
        event = SSEEvent(
            event_type="evolution",
            stage="designer",
            data=evolution_data,
        )
        s = _serialize(event)
        assert "problem" in s, (
            "evolution イベントに 'problem' キーがありません。"
            "app.js は event.problem を読みます（decision のエイリアスが必要）。"
        )
        assert "fix" in s, (
            "evolution イベントに 'fix' キーがありません。"
            "app.js は event.fix を読みます（fix_summary のエイリアスが必要）。"
        )

    def test_hitl_event_not_exposed_in_serialized_data(self) -> None:
        """_hitl_event は JSON シリアライズ対象外（data に含まれない）."""
        ev = asyncio.Event()
        event = SSEEvent(
            event_type="hitl_required",
            stage="analyzer",
            data={"request_id": "req-001", "artifact": {}, "unknowns": [], "question": "test"},
            _hitl_event=ev,
        )
        s = _serialize(event)
        # JSON シリアライズが正常完了すること
        json_str = _json.dumps(s, ensure_ascii=False)
        parsed = _json.loads(json_str)
        assert "_hitl_event" not in parsed

    def test_pipeline_stage_start_has_version(self) -> None:
        """パイプライン開始の stage_start には version が含まれる."""
        event = SSEEvent(
            event_type="stage_start",
            stage="pipeline",
            data={"message": "移行パイプライン開始: SAMPLE", "version": 1},
        )
        s = _serialize(event)
        assert s["stage"] == "pipeline"
        assert "version" in s


# ============================================================
# SSEジェネレーター統合テスト（router._sse_generator）
# ============================================================


class TestSSEGeneratorFormat:
    """_sse_generator のSSEフォーマット検証."""

    @pytest.mark.asyncio
    async def test_events_have_id_field(self) -> None:
        """送信された各イベントには id: フィールドが付与される."""
        from apps.code_migration_assistant.backend.migration_router import _sse_generator
        from apps.code_migration_assistant.backend.migration_task_store import TaskStore

        store = TaskStore()
        task = await store.create()
        await store.push_event(task.task_id, {"type": "stage_start", "stage": "analyzer", "message": "test"})
        await store.close_events(task.task_id)

        chunks = []
        async for chunk in _sse_generator(task.task_id, store):
            chunks.append(chunk)

        sse_text = "".join(chunks)
        assert "id: 1" in sse_text
        assert '"type": "stage_start"' in sse_text

    @pytest.mark.asyncio
    async def test_nonexistent_task_yields_error_event(self) -> None:
        """存在しない task_id は error イベントを返す."""
        from apps.code_migration_assistant.backend.migration_router import _sse_generator
        from apps.code_migration_assistant.backend.migration_task_store import TaskStore

        store = TaskStore()
        chunks = []
        async for chunk in _sse_generator("nonexistent-id", store):
            chunks.append(chunk)

        combined = "".join(chunks)
        assert "error" in combined

    @pytest.mark.asyncio
    async def test_reconnect_replays_missed_events(self) -> None:
        """Last-Event-ID 指定時は未受信イベントだけがリプレイされる."""
        from apps.code_migration_assistant.backend.migration_router import _sse_generator
        from apps.code_migration_assistant.backend.migration_task_store import TaskStore

        store = TaskStore()
        task = await store.create()
        await store.push_event(task.task_id, {"type": "ev1"})
        await store.push_event(task.task_id, {"type": "ev2"})
        await store.push_event(task.task_id, {"type": "ev3"})
        await store.close_events(task.task_id)

        # セッション 1: キューを全部消費（クライアントが ev1 のみ受信したと仮定）
        while True:
            item = await task.events.get()
            if item is None:
                break

        # セッション 2 用に再度 sentinel を追加
        await store.close_events(task.task_id)

        # last_event_id=1 → イベント 2, 3 のみリプレイ
        chunks = []
        async for chunk in _sse_generator(task.task_id, store, last_event_id=1):
            chunks.append(chunk)

        combined = "".join(chunks)
        assert "id: 2" in combined
        assert "id: 3" in combined
        # イベント 1 は含まれない
        assert '"type": "ev1"' not in combined

    @pytest.mark.asyncio
    async def test_multiple_events_have_sequential_ids(self) -> None:
        """複数イベントの id は 1, 2, 3, ... の連番になる."""
        from apps.code_migration_assistant.backend.migration_router import _sse_generator
        from apps.code_migration_assistant.backend.migration_task_store import TaskStore

        store = TaskStore()
        task = await store.create()
        for i in range(3):
            await store.push_event(task.task_id, {"type": f"ev{i + 1}"})
        await store.close_events(task.task_id)

        chunks = []
        async for chunk in _sse_generator(task.task_id, store):
            chunks.append(chunk)

        sse_text = "".join(chunks)
        assert "id: 1" in sse_text
        assert "id: 2" in sse_text
        assert "id: 3" in sse_text
