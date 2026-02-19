"""MigrationTracker - 迁移追踪器.

迁移项目的持久化、进度追踪、报告生成。

使用例:
    >>> tracker = MigrationTracker(storage_path="./migrations")
    >>> project = tracker.create_project("legacy", "cobol", "java")
    >>> tracker.update_file_status(project.id, file_id, FileStatus.TRANSFORMED)
    >>> report = tracker.generate_report(project.id)
"""

from __future__ import annotations

import json
import logging
from datetime import UTC, datetime
from pathlib import Path
from typing import Any
from uuid import UUID

from agentflow.code_intelligence.migration.project import (
    FileStatus,
    MigrationPhase,
    MigrationProject,
    QualityMetrics,
    SourceFile,
)


_logger = logging.getLogger(__name__)


class MigrationTracker:
    """迁移追踪器.

    プロジェクトの保存、読み込み、進捗追跡を行います。
    """

    def __init__(self, storage_path: str | Path | None = None) -> None:
        """初期化.

        Args:
            storage_path: ストレージパス（デフォルトは ./.migrations）
        """
        self._storage_path = Path(storage_path) if storage_path else Path("./.migrations")
        self._storage_path.mkdir(parents=True, exist_ok=True)
        self._projects: dict[UUID, MigrationProject] = {}
        self._load_all_projects()

    def _get_project_path(self, project_id: UUID) -> Path:
        """プロジェクトファイルパスを取得."""
        return self._storage_path / f"{project_id}.json"

    def _load_all_projects(self) -> None:
        """全プロジェクトを読み込み."""
        for file_path in self._storage_path.glob("*.json"):
            try:
                with file_path.open("r", encoding="utf-8") as f:
                    data = json.load(f)
                project = MigrationProject.from_dict(data)
                self._projects[project.id] = project
                _logger.debug(f"Loaded project: {project.name} ({project.id})")
            except Exception as e:
                _logger.warning(f"Failed to load project from {file_path}: {e}")

    def _save_project(self, project: MigrationProject) -> None:
        """プロジェクトを保存."""
        file_path = self._get_project_path(project.id)
        with file_path.open("w", encoding="utf-8") as f:
            json.dump(project.to_dict(), f, indent=2, ensure_ascii=False)
        _logger.debug(f"Saved project: {project.name} ({project.id})")

    def create_project(
        self,
        name: str,
        source_language: str,
        target_language: str,
        source_root: str = "",
        target_root: str = "",
        metadata: dict[str, Any] | None = None,
    ) -> MigrationProject:
        """プロジェクトを作成.

        Args:
            name: プロジェクト名
            source_language: ソース言語
            target_language: ターゲット言語
            source_root: ソースルート
            target_root: ターゲットルート
            metadata: メタデータ

        Returns:
            作成されたプロジェクト
        """
        project = MigrationProject(
            name=name,
            source_language=source_language,
            target_language=target_language,
            source_root=source_root,
            target_root=target_root,
            metadata=metadata or {},
        )
        self._projects[project.id] = project
        self._save_project(project)
        _logger.info(f"Created project: {name} ({project.id})")
        return project

    def create_project_from_directory(
        self,
        name: str,
        source_dir: str | Path,
        source_language: str,
        target_language: str,
        extensions: list[str] | None = None,
    ) -> MigrationProject:
        """ディレクトリからプロジェクトを作成.

        Args:
            name: プロジェクト名
            source_dir: ソースディレクトリ
            source_language: ソース言語
            target_language: ターゲット言語
            extensions: ファイル拡張子

        Returns:
            作成されたプロジェクト
        """
        project = MigrationProject.from_directory(
            name=name,
            source_dir=source_dir,
            source_language=source_language,
            target_language=target_language,
            extensions=extensions,
        )
        self._projects[project.id] = project
        self._save_project(project)
        _logger.info(f"Created project from directory: {name} ({len(project.source_files)} files)")
        return project

    def get_project(self, project_id: UUID | str) -> MigrationProject | None:
        """プロジェクトを取得."""
        if isinstance(project_id, str):
            project_id = UUID(project_id)
        return self._projects.get(project_id)

    def get_project_or_raise(self, project_id: UUID | str) -> MigrationProject:
        """プロジェクトを取得（存在しない場合は例外）."""
        project = self.get_project(project_id)
        if project is None:
            msg = f"Project not found: {project_id}"
            raise ValueError(msg)
        return project

    def list_projects(self) -> list[MigrationProject]:
        """全プロジェクトを取得."""
        return list(self._projects.values())

    def delete_project(self, project_id: UUID | str) -> bool:
        """プロジェクトを削除."""
        if isinstance(project_id, str):
            project_id = UUID(project_id)
        if project_id in self._projects:
            del self._projects[project_id]
            file_path = self._get_project_path(project_id)
            if file_path.exists():
                file_path.unlink()
            _logger.info(f"Deleted project: {project_id}")
            return True
        return False

    # ファイル操作
    def add_file(
        self,
        project_id: UUID | str,
        file: SourceFile,
    ) -> None:
        """ファイルを追加."""
        project = self.get_project_or_raise(project_id)
        project.add_file(file)
        self._save_project(project)

    def update_file_status(
        self,
        project_id: UUID | str,
        file_id: UUID | str,
        status: FileStatus,
        metadata: dict[str, Any] | None = None,
    ) -> None:
        """ファイルステータスを更新."""
        project = self.get_project_or_raise(project_id)
        if isinstance(file_id, str):
            file_id = UUID(file_id)
        file = project.get_file(file_id)
        if file is None:
            msg = f"File not found: {file_id}"
            raise ValueError(msg)
        file.status = status
        file.updated_at = datetime.now(UTC)
        if metadata:
            file.metadata.update(metadata)
        self._save_project(project)

    def add_file_issue(
        self,
        project_id: UUID | str,
        file_id: UUID | str,
        issue: dict[str, Any],
    ) -> None:
        """ファイルに問題を追加."""
        project = self.get_project_or_raise(project_id)
        if isinstance(file_id, str):
            file_id = UUID(file_id)
        file = project.get_file(file_id)
        if file is None:
            msg = f"File not found: {file_id}"
            raise ValueError(msg)
        file.issues.append(issue)
        file.updated_at = datetime.now(UTC)
        self._save_project(project)

    # フェーズ操作
    def start_phase(
        self,
        project_id: UUID | str,
        phase: MigrationPhase,
    ) -> None:
        """フェーズを開始."""
        project = self.get_project_or_raise(project_id)
        project.start_phase(phase)
        self._save_project(project)
        _logger.info(f"Started phase {phase.value} for project {project.name}")

    def complete_phase(
        self,
        project_id: UUID | str,
        phase: MigrationPhase,
        files_processed: int = 0,
        errors: int = 0,
        notes: str = "",
    ) -> None:
        """フェーズを完了."""
        project = self.get_project_or_raise(project_id)
        project.complete_phase(phase, files_processed, errors, notes)
        self._save_project(project)
        _logger.info(f"Completed phase {phase.value} for project {project.name}")

    # 品質メトリクス
    def update_quality_metrics(
        self,
        project_id: UUID | str,
        metrics: QualityMetrics,
    ) -> None:
        """品質メトリクスを更新."""
        project = self.get_project_or_raise(project_id)
        project.quality_metrics = metrics
        project.quality_metrics.calculate_overall()
        project.updated_at = datetime.now(UTC)
        self._save_project(project)

    # レポート生成
    def generate_report(self, project_id: UUID | str) -> dict[str, Any]:
        """レポートを生成.

        Args:
            project_id: プロジェクトID

        Returns:
            レポートデータ
        """
        project = self.get_project_or_raise(project_id)
        progress = project.get_progress()

        # ステータス別ファイル数
        status_counts: dict[str, int] = {}
        for status in FileStatus:
            count = len(project.get_files_by_status(status))
            if count > 0:
                status_counts[status.value] = count

        # フェーズサマリー
        phase_summary = []
        for record in project.phases:
            phase_summary.append(
                {
                    "phase": record.phase.value,
                    "status": record.status.value,
                    "files_processed": record.files_processed,
                    "errors": record.errors,
                    "duration_minutes": (
                        (record.completed_at - record.started_at).total_seconds() / 60
                        if record.started_at and record.completed_at
                        else None
                    ),
                }
            )

        # 問題サマリー
        all_issues: list[dict[str, Any]] = []
        for file in project.source_files.values():
            for issue in file.issues:
                all_issues.append(
                    {
                        "file": file.path,
                        **issue,
                    }
                )

        return {
            "project": {
                "id": str(project.id),
                "name": project.name,
                "source_language": project.source_language,
                "target_language": project.target_language,
                "created_at": project.created_at.isoformat(),
                "updated_at": project.updated_at.isoformat(),
            },
            "progress": progress,
            "status_counts": status_counts,
            "phases": phase_summary,
            "quality_metrics": project.quality_metrics.to_dict(),
            "issues": all_issues,
            "generated_at": datetime.now(UTC).isoformat(),
        }

    def generate_summary(self) -> dict[str, Any]:
        """全プロジェクトのサマリーを生成."""
        projects_summary = []
        for project in self._projects.values():
            progress = project.get_progress()
            projects_summary.append(
                {
                    "id": str(project.id),
                    "name": project.name,
                    "source_language": project.source_language,
                    "target_language": project.target_language,
                    "total_files": progress["total_files"],
                    "file_progress": progress["file_progress"],
                    "phase_progress": progress["phase_progress"],
                    "current_phase": progress["current_phase"],
                    "overall_quality": project.quality_metrics.overall_score,
                }
            )

        return {
            "total_projects": len(self._projects),
            "projects": projects_summary,
            "generated_at": datetime.now(UTC).isoformat(),
        }


__all__ = ["MigrationTracker"]
