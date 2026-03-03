"""COBOL プロジェクト管理モジュール.

COBOLファイル（単一または zip）を受け取り、
変換対象ファイルの列挙とメタデータ管理を行う。
"""

from __future__ import annotations

import zipfile
from pathlib import Path
from typing import Any


_COBOL_EXTENSIONS = {".cbl", ".cob", ".cobol", ".cpy"}


class COBOLFile:
    """単一COBOLファイルの情報を保持するデータクラス."""

    def __init__(self, path: Path, project_root: Path) -> None:
        """初期化.

        Args:
            path: COBOLファイルの絶対パス
            project_root: プロジェクトルートディレクトリ
        """
        self.path = path
        self.project_root = project_root

    @property
    def program_name(self) -> str:
        """プログラム名（ファイル名から拡張子を除いた大文字）を返す."""
        return self.path.stem.upper()

    @property
    def relative_path(self) -> Path:
        """プロジェクトルートからの相対パスを返す."""
        return self.path.relative_to(self.project_root)

    @property
    def content(self) -> str:
        """COBOLファイルの内容を返す."""
        return self.path.read_text(encoding="utf-8", errors="replace")

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換する."""
        return {
            "program_name": self.program_name,
            "file_path": str(self.path),
            "relative_path": str(self.relative_path),
        }


class COBOLProject:
    """COBOLプロジェクトを管理するクラス.

    単一COBOLファイルまたはzipアーカイブを受け取り、
    変換対象ファイルを列挙する。
    """

    def __init__(self, source: Path, work_dir: Path) -> None:
        """初期化.

        Args:
            source: COBOLファイルまたはzipアーカイブのパス
            work_dir: 作業ディレクトリ（zip展開先）
        """
        self.source = source
        self.work_dir = work_dir
        self._project_root: Path | None = None
        self._cobol_files: list[COBOLFile] | None = None

    def setup(self) -> None:
        """プロジェクトを初期化する（zip展開等）.

        Raises:
            FileNotFoundError: ソースファイルが存在しない場合
            ValueError: 未対応のファイル形式の場合
        """
        if not self.source.exists():
            raise FileNotFoundError(f"ソースファイルが存在しません: {self.source}")

        if self.source.suffix.lower() == ".zip":
            self._setup_from_zip()
        elif self.source.suffix.lower() in _COBOL_EXTENSIONS:
            self._setup_from_single_file()
        elif self.source.is_dir():
            self._setup_from_directory()
        else:
            raise ValueError(
                f"未対応のファイル形式: {self.source.suffix}. "
                f"対応形式: {', '.join(['.zip', *_COBOL_EXTENSIONS])}"
            )

    def get_cobol_files(self) -> list[COBOLFile]:
        """変換対象COBOLファイルのリストを返す.

        Returns:
            COBOLFileオブジェクトのリスト（名前順）

        Raises:
            RuntimeError: setup() が未実行の場合
        """
        if self._cobol_files is None:
            raise RuntimeError("setup() を先に実行してください")
        return self._cobol_files

    def get_project_root(self) -> Path:
        """プロジェクトルートディレクトリを返す.

        Raises:
            RuntimeError: setup() が未実行の場合
        """
        if self._project_root is None:
            raise RuntimeError("setup() を先に実行してください")
        return self._project_root

    def get_copy_files(self) -> list[Path]:
        """COPYライブラリ（.cpy）ファイルのリストを返す."""
        if self._project_root is None:
            return []
        return sorted(self._project_root.rglob("*.cpy"))

    def to_dict(self) -> dict[str, Any]:
        """プロジェクト情報を辞書形式で返す."""
        files = self.get_cobol_files() if self._cobol_files is not None else []
        return {
            "source": str(self.source),
            "project_root": str(self._project_root),
            "cobol_files": [f.to_dict() for f in files],
            "total_files": len(files),
        }

    def _setup_from_single_file(self) -> None:
        self._project_root = self.source.parent
        self._cobol_files = [COBOLFile(self.source, self._project_root)]

    def _setup_from_zip(self) -> None:
        extract_dir = self.work_dir / self.source.stem
        extract_dir.mkdir(parents=True, exist_ok=True)

        with zipfile.ZipFile(self.source, "r") as zf:
            for member in zf.namelist():
                member_path = extract_dir / member
                if not str(member_path.resolve()).startswith(str(extract_dir.resolve())):
                    continue
                zf.extract(member, extract_dir)

        self._project_root = extract_dir
        self._cobol_files = self._find_cobol_files(extract_dir)

    def _setup_from_directory(self) -> None:
        self._project_root = self.source
        self._cobol_files = self._find_cobol_files(self.source)

    @staticmethod
    def _find_cobol_files(root: Path) -> list[COBOLFile]:
        files: list[COBOLFile] = []
        for ext in _COBOL_EXTENSIONS:
            if ext == ".cpy":
                continue
            for path in sorted(root.rglob(f"*{ext}")):
                files.append(COBOLFile(path, root))
        return files

