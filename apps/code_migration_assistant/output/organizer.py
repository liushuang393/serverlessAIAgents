"""出力フォルダ構造管理モジュール.

移行成果物を以下の構造で整理する:
migration_output/{task_id}/{program_name}/v{n}/
  01_analysis/analysis.json
  02_design/design.json
  03_transform/src/main/java/.../*.java
  04_verification/ast_comparison.json
  05_report/report.md
"""

from __future__ import annotations

import json
import logging
from datetime import UTC, datetime
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from pathlib import Path


logger = logging.getLogger(__name__)


class OutputOrganizer:
    """移行成果物の出力フォルダを管理するクラス."""

    # ステージ番号とフォルダ名のマッピング
    STAGE_DIRS: dict[str, str] = {
        "analyzer": "01_analysis",
        "designer": "02_design",
        "transformer": "03_transform",
        "test_generator": "03_transform",  # 同じフォルダ内
        "verifier": "04_verification",
        "quality_gate": "04_verification",  # 同じフォルダ内
        "report": "05_report",
    }

    def __init__(self, output_root: Path) -> None:
        """初期化.

        Args:
            output_root: 出力ルートディレクトリ
        """
        self.output_root = output_root
        output_root.mkdir(parents=True, exist_ok=True)

    def create_version_dir(self, program_name: str, version: int) -> Path:
        """バージョンディレクトリを作成する.

        Args:
            program_name: COBOLプログラム名
            version: バージョン番号（1始まり）

        Returns:
            作成したバージョンディレクトリのパス
        """
        version_dir = self.output_root / program_name / f"v{version}"
        version_dir.mkdir(parents=True, exist_ok=True)

        # 各ステージフォルダを事前作成
        for stage_dir in set(self.STAGE_DIRS.values()):
            (version_dir / stage_dir).mkdir(exist_ok=True)

        return version_dir

    def get_program_dir(self, program_name: str) -> Path:
        """プログラムディレクトリのパスを返す."""
        return self.output_root / program_name

    def get_version_dir(self, program_name: str, version: int) -> Path:
        """バージョンディレクトリのパスを返す（作成しない）."""
        return self.output_root / program_name / f"v{version}"

    def load_artifacts(
        self,
        version_dir: Path,
        up_to_stage: str | None = None,
    ) -> dict[str, Any]:
        """バージョンディレクトリから成果物JSONを読み込む.

        Args:
            version_dir: バージョンディレクトリ
            up_to_stage: このステージ（含まず）までの成果物を読み込む。
                         None の場合は全ステージ読み込む。

        Returns:
            {stage: artifact_data} の辞書
        """
        stage_order = [
            "analyzer",
            "designer",
            "transformer",
            "test_generator",
            "verifier",
            "quality_gate",
        ]
        artifacts: dict[str, Any] = {}

        for stage in stage_order:
            if up_to_stage is not None and stage == up_to_stage:
                break  # このステージから再実行するので読み込まない

            stage_dir_name = self.STAGE_DIRS.get(stage, stage)
            artifact_file = version_dir / stage_dir_name / f"{stage}.json"

            if artifact_file.exists():
                try:
                    raw = json.loads(artifact_file.read_text(encoding="utf-8"))
                    # _saved_at, _stage などメタ情報を除外して返す
                    artifacts[stage] = {k: v for k, v in raw.items() if not k.startswith("_")}
                except (json.JSONDecodeError, OSError) as exc:
                    logger.warning("成果物読み込み失敗 %s: %s", artifact_file, exc)

        return artifacts

    def get_version_count(self, program_name: str) -> int:
        """現在のバージョン数を返す."""
        program_dir = self.get_program_dir(program_name)
        if not program_dir.exists():
            return 0
        versions = [d for d in program_dir.iterdir() if d.is_dir() and d.name.startswith("v")]
        return len(versions)

    def save_artifact(
        self,
        artifact_data: dict[str, Any],
        stage: str,
        version_dir: Path,
        filename: str | None = None,
    ) -> Path:
        """成果物をJSONで保存する.

        Args:
            artifact_data: 保存するデータ
            stage: ステージ名
            version_dir: バージョンディレクトリ
            filename: ファイル名（省略時はステージ名.json）

        Returns:
            保存したファイルパス
        """
        stage_dir_name = self.STAGE_DIRS.get(stage, stage)
        output_dir = version_dir / stage_dir_name
        output_dir.mkdir(parents=True, exist_ok=True)

        if filename is None:
            filename = f"{stage}.json"

        output_path = output_dir / filename

        # メタ情報を追加
        save_data = {
            "_saved_at": datetime.now(UTC).isoformat(),
            "_stage": stage,
            **artifact_data,
        }

        output_path.write_text(
            json.dumps(save_data, ensure_ascii=False, indent=2),
            encoding="utf-8",
        )
        return output_path

    def save_java_files(self, generated_code: str, version_dir: Path) -> list[Path]:
        """生成されたJavaコードをファイルに保存する.

        コードは // --- [FILE: path] --- マーカーで区切られている。

        Args:
            generated_code: 生成されたコード（マーカー区切り）
            version_dir: バージョンディレクトリ

        Returns:
            保存したファイルパスのリスト
        """
        saved_files: list[Path] = []
        transform_dir = version_dir / "03_transform"

        current_file_path: str | None = None
        current_content: list[str] = []

        for line in generated_code.splitlines():
            if line.startswith("// --- [FILE:") and line.endswith("] ---"):
                # 前のファイルを保存
                if current_file_path and current_content:
                    saved = self._write_java_file(
                        transform_dir / current_file_path,
                        "\n".join(current_content),
                    )
                    saved_files.append(saved)
                # 新しいファイル開始
                current_file_path = line[len("// --- [FILE: ") : -len("] ---")].strip()
                current_content = []
            else:
                current_content.append(line)

        # 最後のファイルを保存
        if current_file_path and current_content:
            saved = self._write_java_file(
                transform_dir / current_file_path,
                "\n".join(current_content),
            )
            saved_files.append(saved)

        # マーカーがない場合、全体を Main.java として保存
        if not saved_files and generated_code.strip():
            saved = self._write_java_file(
                transform_dir / "src" / "main" / "java" / "Main.java",
                generated_code,
            )
            saved_files.append(saved)

        return saved_files

    def save_report(self, report_md: str, version_dir: Path) -> Path:
        """マークダウンレポートを保存する.

        Args:
            report_md: マークダウン形式のレポート
            version_dir: バージョンディレクトリ

        Returns:
            保存したレポートのパス
        """
        report_dir = version_dir / "05_report"
        report_dir.mkdir(parents=True, exist_ok=True)
        report_path = report_dir / "report.md"
        report_path.write_text(report_md, encoding="utf-8")
        return report_path

    def save_evolution_history(
        self,
        evolution_data: dict[str, Any],
        program_name: str,
    ) -> Path:
        """進化履歴を保存する.

        Args:
            evolution_data: 進化履歴データ
            program_name: プログラム名

        Returns:
            保存したファイルパス
        """
        program_dir = self.get_program_dir(program_name)
        program_dir.mkdir(parents=True, exist_ok=True)
        evolution_path = program_dir / "evolution.json"

        # 既存データがあればマージ
        existing: dict[str, Any] = {}
        if evolution_path.exists():
            try:
                existing = json.loads(evolution_path.read_text(encoding="utf-8"))
            except json.JSONDecodeError:
                existing = {}

        iterations: list[dict[str, Any]] = existing.get("iterations", [])
        if "iteration" in evolution_data:
            iterations.append(evolution_data)
        else:
            iterations.extend(evolution_data.get("iterations", []))

        merged = {
            "_updated_at": datetime.now(UTC).isoformat(),
            "program_name": program_name,
            "total_iterations": len(iterations),
            "iterations": iterations,
        }

        evolution_path.write_text(
            json.dumps(merged, ensure_ascii=False, indent=2),
            encoding="utf-8",
        )
        return evolution_path

    def generate_summary_report(
        self,
        program_name: str,
        all_versions: list[dict[str, Any]],
    ) -> Path:
        """全バージョンのサマリーレポートを生成する.

        Args:
            program_name: プログラム名
            all_versions: 各バージョンの成果物データのリスト

        Returns:
            保存したサマリーレポートパス
        """
        program_dir = self.get_program_dir(program_name)
        final_version = all_versions[-1] if all_versions else {}

        lines = [
            f"# Migration Studio - 移行レポート: {program_name}",
            f"\n生成日時: {datetime.now(UTC).isoformat()}",
            f"バージョン数: {len(all_versions)}",
            "",
            "## 最終結果",
        ]

        quality = final_version.get("quality_gate", {})
        decision = quality.get("decision", "未完了")
        lines.append(f"品質判定: **{decision}**")
        lines.append(f"理由: {quality.get('reason', '不明')}")

        lines.extend(
            [
                "",
                "## 生成ファイル",
            ]
        )

        transform = final_version.get("transform_dir", "")
        if transform:
            lines.append(f"変換コード: `{transform}`")

        report_path = program_dir / "final_report.md"
        report_path.write_text("\n".join(lines), encoding="utf-8")
        return report_path

    @staticmethod
    def _write_java_file(file_path: Path, content: str) -> Path:
        """Javaファイルを書き込む（ディレクトリも作成）."""
        file_path.parent.mkdir(parents=True, exist_ok=True)
        file_path.write_text(content, encoding="utf-8")
        return file_path
