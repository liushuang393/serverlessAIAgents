"""zipパッケージ生成モジュール.

移行成果物（Java Spring Bootプロジェクト + テスト + 設計書）を
zipファイルにパッケージする。
"""

from __future__ import annotations

import zipfile
from datetime import UTC, datetime
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from pathlib import Path


class OutputPackager:
    """移行成果物をzipにパッケージするクラス."""

    def __init__(self, output_root: Path) -> None:
        """初期化.

        Args:
            output_root: 出力ルートディレクトリ
        """
        self.output_root = output_root

    def create_download_package(
        self,
        program_name: str,
        version: int | None = None,
    ) -> Path:
        """ダウンロード用zipパッケージを作成する.

        Args:
            program_name: COBOLプログラム名
            version: バージョン番号（None の場合は最新バージョン）

        Returns:
            作成したzipファイルパス

        Raises:
            FileNotFoundError: 指定されたバージョンが存在しない場合
        """
        program_dir = self.output_root / program_name
        if not program_dir.exists():
            msg = f"プログラムディレクトリが存在しません: {program_dir}"
            raise FileNotFoundError(msg)

        # バージョン決定
        if version is None:
            version = self._get_latest_version(program_dir)
        version_dir = program_dir / f"v{version}"
        if not version_dir.exists():
            msg = f"バージョンディレクトリが存在しません: {version_dir}"
            raise FileNotFoundError(msg)

        # zipファイルパス
        timestamp = datetime.now(UTC).strftime("%Y%m%d_%H%M%S")
        zip_path = program_dir / f"download_{program_name}_v{version}_{timestamp}.zip"

        with zipfile.ZipFile(zip_path, "w", zipfile.ZIP_DEFLATED) as zf:
            # 変換済みJavaファイル（Spring Boot プロジェクト）
            transform_dir = version_dir / "03_transform"
            if transform_dir.exists():
                self._add_directory_to_zip(zf, transform_dir, "java_project")

            # テストレポートと検証結果
            verify_dir = version_dir / "04_verification"
            if verify_dir.exists():
                self._add_directory_to_zip(zf, verify_dir, "verification")

            # 設計書・レポート
            report_dir = version_dir / "05_report"
            if report_dir.exists():
                self._add_directory_to_zip(zf, report_dir, "reports")

            # 分析結果
            analysis_dir = version_dir / "01_analysis"
            if analysis_dir.exists():
                self._add_directory_to_zip(zf, analysis_dir, "analysis")

            # 設計成果物
            design_dir = version_dir / "02_design"
            if design_dir.exists():
                self._add_directory_to_zip(zf, design_dir, "design")

            # 進化履歴
            evolution_file = program_dir / "evolution.json"
            if evolution_file.exists():
                zf.write(evolution_file, "evolution.json")

            # サマリーレポート
            final_report = program_dir / "final_report.md"
            if final_report.exists():
                zf.write(final_report, "final_report.md")

        return zip_path

    def create_java_only_package(
        self,
        program_name: str,
        version: int | None = None,
    ) -> Path:
        """Javaコードのみのzipパッケージを作成する.

        Args:
            program_name: COBOLプログラム名
            version: バージョン番号

        Returns:
            作成したzipファイルパス
        """
        program_dir = self.output_root / program_name
        if version is None:
            version = self._get_latest_version(program_dir)

        version_dir = program_dir / f"v{version}"
        transform_dir = version_dir / "03_transform"

        if not transform_dir.exists():
            msg = f"変換ディレクトリが存在しません: {transform_dir}"
            raise FileNotFoundError(msg)

        timestamp = datetime.now(UTC).strftime("%Y%m%d_%H%M%S")
        zip_path = program_dir / f"java_{program_name}_v{version}_{timestamp}.zip"

        with zipfile.ZipFile(zip_path, "w", zipfile.ZIP_DEFLATED) as zf:
            self._add_directory_to_zip(zf, transform_dir, "")

        return zip_path

    def list_packages(self, program_name: str) -> list[Path]:
        """作成済みzipパッケージの一覧を返す.

        Args:
            program_name: プログラム名

        Returns:
            zipファイルパスのリスト（更新日時降順）
        """
        program_dir = self.output_root / program_name
        if not program_dir.exists():
            return []
        zips = sorted(program_dir.glob("*.zip"), key=lambda p: p.stat().st_mtime, reverse=True)
        return list(zips)

    @staticmethod
    def _get_latest_version(program_dir: Path) -> int:
        """最新バージョン番号を取得する."""
        versions = [
            d for d in program_dir.iterdir()
            if d.is_dir() and d.name.startswith("v")
        ]
        if not versions:
            msg = f"バージョンが存在しません: {program_dir}"
            raise FileNotFoundError(msg)
        return max(int(v.name[1:]) for v in versions)

    @staticmethod
    def _add_directory_to_zip(
        zf: zipfile.ZipFile,
        source_dir: Path,
        archive_prefix: str,
    ) -> None:
        """ディレクトリをzipに追加する.

        Args:
            zf: ZipFileオブジェクト
            source_dir: 追加するソースディレクトリ
            archive_prefix: アーカイブ内のプレフィックス
        """
        for file_path in source_dir.rglob("*"):
            if file_path.is_file():
                relative = file_path.relative_to(source_dir)
                if archive_prefix:
                    arcname = f"{archive_prefix}/{relative}"
                else:
                    arcname = str(relative)
                zf.write(file_path, arcname)
