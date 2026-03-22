"""Migration Studio E2Eテスト.

Claude Agent SDK を使った実際のパイプライン実行テスト。
ANTHROPIC_API_KEY が設定されている場合のみ実行される。

実行方法:
    # 高速モード（推奨: APIキーが必要）
    pytest apps/code_migration_assistant/tests/test_e2e.py -v -k fast

    # 全テスト（APIキー必要）
    pytest apps/code_migration_assistant/tests/test_e2e.py -v
"""

from __future__ import annotations

import json
import os
import tempfile
from pathlib import Path
from typing import Any

import pytest

from apps.code_migration_assistant.evolution.manager import EvolutionManager
from apps.code_migration_assistant.output.organizer import OutputOrganizer
from apps.code_migration_assistant.output.packager import OutputPackager
from apps.code_migration_assistant.pipeline.project import COBOLProject


# サンプルCOBOLファイルのパス
FIXTURES_DIR = Path(__file__).parent / "fixtures"
SAMPLE_CBL = FIXTURES_DIR / "sample.cbl"

# APIキーが設定されているか確認
HAS_API_KEY = bool(os.environ.get("ANTHROPIC_API_KEY"))


# ============================================================
# COBOLProject テスト（APIキー不要）
# ============================================================


class TestCOBOLProject:
    """COBOLProjectクラスのテスト."""

    def test_setup_single_file(self, tmp_path: Path) -> None:
        """単一COBOLファイルからセットアップできる."""
        project = COBOLProject(source=SAMPLE_CBL, work_dir=tmp_path)
        project.setup()

        files = project.get_cobol_files()
        assert len(files) == 1
        assert files[0].program_name == "SAMPLE"

    def test_setup_directory(self, tmp_path: Path) -> None:
        """ディレクトリからCOBOLファイルを検索できる."""
        # テスト用ディレクトリにCOBOLファイルをコピー
        src_dir = tmp_path / "cobol_project"
        src_dir.mkdir()
        import shutil

        shutil.copy(SAMPLE_CBL, src_dir / "sample.cbl")

        project = COBOLProject(source=src_dir, work_dir=tmp_path)
        project.setup()

        files = project.get_cobol_files()
        assert len(files) == 1
        assert files[0].program_name == "SAMPLE"

    def test_setup_zip(self, tmp_path: Path) -> None:
        """zipアーカイブからセットアップできる."""
        import zipfile as _zipfile

        zip_path = tmp_path / "project.zip"
        with _zipfile.ZipFile(zip_path, "w") as zf:
            zf.write(SAMPLE_CBL, "sample.cbl")

        project = COBOLProject(source=zip_path, work_dir=tmp_path)
        project.setup()

        files = project.get_cobol_files()
        assert len(files) == 1

    def test_setup_file_not_found(self, tmp_path: Path) -> None:
        """存在しないファイルで FileNotFoundError が発生する."""
        project = COBOLProject(
            source=tmp_path / "nonexistent.cbl",
            work_dir=tmp_path,
        )
        with pytest.raises(FileNotFoundError):
            project.setup()

    def test_cobol_file_content(self, tmp_path: Path) -> None:
        """COBOLFileクラスがコンテンツを正しく返す."""
        project = COBOLProject(source=SAMPLE_CBL, work_dir=tmp_path)
        project.setup()

        files = project.get_cobol_files()
        content = files[0].content
        assert "PROGRAM-ID" in content
        assert "CUSTPROC" in content


# ============================================================
# OutputOrganizer テスト（APIキー不要）
# ============================================================


class TestOutputOrganizer:
    """OutputOrganizerクラスのテスト."""

    def test_create_version_dir(self, tmp_path: Path) -> None:
        """バージョンディレクトリが作成される."""
        organizer = OutputOrganizer(tmp_path)
        version_dir = organizer.create_version_dir("SAMPLE", 1)

        assert version_dir.exists()
        assert version_dir.name == "v1"
        # ステージディレクトリが事前作成される
        assert (version_dir / "01_analysis").exists()
        assert (version_dir / "03_transform").exists()

    def test_save_artifact(self, tmp_path: Path) -> None:
        """成果物がJSONで保存される."""
        organizer = OutputOrganizer(tmp_path)
        version_dir = organizer.create_version_dir("SAMPLE", 1)

        data: dict[str, Any] = {"programs": [{"program_id": "SAMPLE"}]}
        saved_path = organizer.save_artifact(data, "analyzer", version_dir)

        assert saved_path.exists()
        content = json.loads(saved_path.read_text(encoding="utf-8"))
        assert content["programs"][0]["program_id"] == "SAMPLE"
        assert "_saved_at" in content

    def test_save_java_files_with_markers(self, tmp_path: Path) -> None:
        """マーカー区切りのJavaコードが正しく保存される."""
        organizer = OutputOrganizer(tmp_path)
        version_dir = organizer.create_version_dir("SAMPLE", 1)

        generated_code = (
            "// --- [FILE: src/main/java/com/company/SampleController.java] ---\n"
            "public class SampleController {}\n"
            "\n"
            "// --- [FILE: pom.xml] ---\n"
            "<project/>\n"
        )
        saved_files = organizer.save_java_files(generated_code, version_dir)

        assert len(saved_files) == 2
        controller_file = next((f for f in saved_files if "Controller" in f.name), None)
        assert controller_file is not None

    def test_save_java_files_no_markers(self, tmp_path: Path) -> None:
        """マーカーなしのコードは Main.java として保存される."""
        organizer = OutputOrganizer(tmp_path)
        version_dir = organizer.create_version_dir("SAMPLE", 1)

        generated_code = "public class Main {}"
        saved_files = organizer.save_java_files(generated_code, version_dir)

        assert len(saved_files) == 1
        assert saved_files[0].name == "Main.java"

    def test_get_version_count(self, tmp_path: Path) -> None:
        """バージョン数が正しく返される."""
        organizer = OutputOrganizer(tmp_path)
        assert organizer.get_version_count("SAMPLE") == 0

        organizer.create_version_dir("SAMPLE", 1)
        assert organizer.get_version_count("SAMPLE") == 1

        organizer.create_version_dir("SAMPLE", 2)
        assert organizer.get_version_count("SAMPLE") == 2


# ============================================================
# OutputPackager テスト（APIキー不要）
# ============================================================


class TestOutputPackager:
    """OutputPackagerクラスのテスト."""

    def test_create_download_package(self, tmp_path: Path) -> None:
        """ダウンロード用zipが作成される."""
        organizer = OutputOrganizer(tmp_path)
        version_dir = organizer.create_version_dir("SAMPLE", 1)

        # ダミーJavaファイルを作成
        (version_dir / "03_transform").mkdir(exist_ok=True)
        (version_dir / "03_transform" / "Sample.java").write_text(
            "public class Sample {}",
            encoding="utf-8",
        )

        packager = OutputPackager(tmp_path)
        zip_path = packager.create_download_package("SAMPLE", 1)

        assert zip_path.exists()
        assert zip_path.suffix == ".zip"

    def test_list_packages(self, tmp_path: Path) -> None:
        """パッケージ一覧が返される."""
        organizer = OutputOrganizer(tmp_path)
        organizer.create_version_dir("SAMPLE", 1)

        packager = OutputPackager(tmp_path)

        # パッケージが存在しない場合は空リスト
        packages = packager.list_packages("SAMPLE")
        assert isinstance(packages, list)


# ============================================================
# EvolutionManager テスト（APIキー不要）
# ============================================================


class TestEvolutionManager:
    """EvolutionManagerクラスのテスト."""

    def test_can_evolve_initial(self, tmp_path: Path) -> None:
        """初期状態では進化可能."""
        manager = EvolutionManager(tmp_path, max_iterations=3)
        assert manager.can_evolve("SAMPLE", "DESIGN_ISSUE") is True

    def test_can_evolve_max_reached(self, tmp_path: Path) -> None:
        """最大反復数に達した場合は進化不可."""
        manager = EvolutionManager(tmp_path, max_iterations=2)

        # 2回進化させる
        version_dir = tmp_path / "v1"
        version_dir.mkdir()
        manager.evolve("SAMPLE", "DESIGN_ISSUE", {"reason": "test"}, version_dir)
        manager.evolve("SAMPLE", "DESIGN_ISSUE", {"reason": "test"}, version_dir)

        assert manager.can_evolve("SAMPLE", "DESIGN_ISSUE") is False

    def test_get_retry_stage(self, tmp_path: Path) -> None:
        """判定結果から再実行ステージが取得できる."""
        manager = EvolutionManager(tmp_path)
        assert manager.get_retry_stage("DESIGN_ISSUE") == "designer"
        assert manager.get_retry_stage("TRANSFORM_ISSUE") == "transformer"
        assert manager.get_retry_stage("TEST_ISSUE") == "test_generator"
        assert manager.get_retry_stage("ENV_ISSUE") is None
        assert manager.get_retry_stage("PASSED") is None

    def test_evolve_records_iteration(self, tmp_path: Path) -> None:
        """evolve() がイテレーション情報を返す."""
        manager = EvolutionManager(tmp_path)
        version_dir = tmp_path / "v1"
        version_dir.mkdir()

        result = manager.evolve(
            "SAMPLE",
            "DESIGN_ISSUE",
            {"reason": "フィールド欠落", "evidence": {"field_coverage": "3/8"}},
            version_dir,
        )

        assert result is not None
        assert result["iteration"] == 1
        assert result["stage"] == "designer"
        assert "fix_summary" in result

    def test_record_evolution_creates_json(self, tmp_path: Path) -> None:
        """evolution.json が作成される."""
        manager = EvolutionManager(tmp_path)
        program_dir = tmp_path / "SAMPLE"
        program_dir.mkdir()

        evolution_info: dict[str, Any] = {
            "iteration": 1,
            "stage": "designer",
            "decision": "DESIGN_ISSUE",
            "fix_summary": "テスト修正",
            "evolved_at": "2026-01-01T00:00:00+00:00",
        }
        manager.record_evolution(evolution_info, program_dir)

        evolution_path = program_dir / "evolution.json"
        assert evolution_path.exists()

        data = json.loads(evolution_path.read_text(encoding="utf-8"))
        assert data["total_iterations"] == 1
        assert len(data["iterations"]) == 1


# ============================================================
# E2Eテスト（APIキーが必要）
# ============================================================


@pytest.mark.skipif(not HAS_API_KEY, reason="ANTHROPIC_API_KEY が設定されていません")
class TestMigrationPipelineFast:
    """パイプラインのE2Eテスト（APIキー必要、高速モード）."""

    def test_single_file_fast_mode(self, tmp_path: Path) -> None:
        """単一COBOLファイルを高速モードで変換できる."""
        from apps.code_migration_assistant.pipeline.engine import run_migration_sync
        from apps.code_migration_assistant.pipeline.project import COBOLProject

        project = COBOLProject(source=SAMPLE_CBL, work_dir=tmp_path)
        project.setup()
        cobol_file = project.get_cobol_files()[0]

        result = run_migration_sync(
            cobol_file=cobol_file,
            output_root=tmp_path / "output",
            fast_mode=True,
        )

        assert result.program_name == "SAMPLE"
        assert result.decision in (
            "PASSED",
            "KNOWN_LEGACY",
            "DESIGN_ISSUE",
            "TRANSFORM_ISSUE",
            "TEST_ISSUE",
            "ENV_ISSUE",
        )

        # 出力ディレクトリが作成されていること
        output_dir = tmp_path / "output" / "SAMPLE"
        assert output_dir.exists()


# ============================================================
# パイプライン単体テスト（モック使用）
# ============================================================


class TestMigrationEngineUnit:
    """MigrationEngineの単体テスト（モック使用）."""

    def test_build_prompt_analyzer(self) -> None:
        """analyzer ステージのプロンプトが正しく構築される."""
        from apps.code_migration_assistant.pipeline.engine import MigrationEngine

        with tempfile.TemporaryDirectory() as tmp:
            engine = MigrationEngine(Path(tmp))
            prompt = engine._build_prompt(
                stage="analyzer",
                cobol_content="PROGRAM-ID. SAMPLE.",
                artifacts={},
                fast_mode=False,
            )
            assert "PROGRAM-ID" in prompt
            assert "SAMPLE" in prompt

    def test_parse_stage_result_json_block(self) -> None:
        """JSONブロックを正しくパースできる."""
        from apps.code_migration_assistant.pipeline.engine import MigrationEngine

        result_text = '```json\n{"programs": [{"id": "SAMPLE"}]}\n```'
        parsed = MigrationEngine._parse_stage_result("analyzer", result_text)
        assert parsed["programs"][0]["id"] == "SAMPLE"

    def test_parse_stage_result_raw_json(self) -> None:
        """生JSONを正しくパースできる."""
        from apps.code_migration_assistant.pipeline.engine import MigrationEngine

        result_text = '{"decision": "PASSED", "reason": "ok"}'
        parsed = MigrationEngine._parse_stage_result("quality_gate", result_text)
        assert parsed["decision"] == "PASSED"

    def test_parse_stage_result_transformer_fallback(self) -> None:
        """transformer ステージはJSONでない場合もtarget_codeとして保存."""
        from apps.code_migration_assistant.pipeline.engine import MigrationEngine

        result_text = "public class Sample {}"
        parsed = MigrationEngine._parse_stage_result("transformer", result_text)
        assert "target_code" in parsed
        assert "public class" in parsed["target_code"]
