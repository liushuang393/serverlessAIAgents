# -*- coding: utf-8 -*-
"""MigrationProject - 迁移项目管理.

迁移项目的追踪和管理，包含阶段、文件状态、质量指标等。

使用例:
    >>> project = MigrationProject(
    ...     name="legacy-to-modern",
    ...     source_language="cobol",
    ...     target_language="java",
    ... )
    >>> project.add_file(SourceFile(path="main.cob"))
    >>> project.start_phase(MigrationPhase.ANALYSIS)
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime, UTC
from enum import Enum
from pathlib import Path
from typing import Any
from uuid import UUID, uuid4


class PhaseStatus(str, Enum):
    """フェーズステータス."""

    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    FAILED = "failed"
    SKIPPED = "skipped"


class FileStatus(str, Enum):
    """ファイルステータス."""

    PENDING = "pending"
    ANALYZING = "analyzing"
    ANALYZED = "analyzed"
    TRANSFORMING = "transforming"
    TRANSFORMED = "transformed"
    VALIDATING = "validating"
    VALIDATED = "validated"
    FAILED = "failed"
    SKIPPED = "skipped"


class MigrationPhase(str, Enum):
    """迁移阶段."""

    DISCOVERY = "discovery"  # 发现和盘点
    ANALYSIS = "analysis"  # 代码分析
    PLANNING = "planning"  # 迁移计划
    TRANSFORMATION = "transformation"  # 代码转换
    VALIDATION = "validation"  # 验证测试
    DEPLOYMENT = "deployment"  # 部署
    CUTOVER = "cutover"  # 切换


@dataclass
class SourceFile:
    """ソースファイル.

    Attributes:
        id: ファイルID
        path: ファイルパス
        language: プログラミング言語
        status: ステータス
        loc: コード行数
        complexity: 複雑度スコア
        dependencies: 依存ファイル
        target_path: 変換後のパス
        issues: 検出された問題
        metadata: メタデータ
    """

    path: str
    language: str = ""
    status: FileStatus = FileStatus.PENDING
    loc: int = 0
    complexity: float = 0.0
    dependencies: list[str] = field(default_factory=list)
    target_path: str = ""
    issues: list[dict[str, Any]] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)
    id: UUID = field(default_factory=uuid4)
    created_at: datetime = field(default_factory=lambda: datetime.now(UTC))
    updated_at: datetime = field(default_factory=lambda: datetime.now(UTC))

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "id": str(self.id),
            "path": self.path,
            "language": self.language,
            "status": self.status.value,
            "loc": self.loc,
            "complexity": self.complexity,
            "dependencies": self.dependencies,
            "target_path": self.target_path,
            "issues": self.issues,
            "metadata": self.metadata,
            "created_at": self.created_at.isoformat(),
            "updated_at": self.updated_at.isoformat(),
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "SourceFile":
        """辞書から作成."""
        return cls(
            id=UUID(data["id"]) if "id" in data else uuid4(),
            path=data["path"],
            language=data.get("language", ""),
            status=FileStatus(data.get("status", "pending")),
            loc=data.get("loc", 0),
            complexity=data.get("complexity", 0.0),
            dependencies=data.get("dependencies", []),
            target_path=data.get("target_path", ""),
            issues=data.get("issues", []),
            metadata=data.get("metadata", {}),
        )


@dataclass
class PhaseRecord:
    """フェーズ記録.

    Attributes:
        phase: フェーズ
        status: ステータス
        started_at: 開始時刻
        completed_at: 完了時刻
        files_processed: 処理ファイル数
        errors: エラー数
        notes: メモ
    """

    phase: MigrationPhase
    status: PhaseStatus = PhaseStatus.PENDING
    started_at: datetime | None = None
    completed_at: datetime | None = None
    files_processed: int = 0
    errors: int = 0
    notes: str = ""

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "phase": self.phase.value,
            "status": self.status.value,
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "files_processed": self.files_processed,
            "errors": self.errors,
            "notes": self.notes,
        }


@dataclass
class QualityMetrics:
    """品質メトリクス.

    Attributes:
        syntax_score: 構文正確性スコア (0-100)
        semantic_score: 意味保持スコア (0-100)
        test_coverage: テストカバレッジ (0-100)
        security_score: セキュリティスコア (0-100)
        performance_score: パフォーマンススコア (0-100)
        overall_score: 総合スコア (0-100)
    """

    syntax_score: float = 0.0
    semantic_score: float = 0.0
    test_coverage: float = 0.0
    security_score: float = 0.0
    performance_score: float = 0.0
    overall_score: float = 0.0

    def calculate_overall(self) -> float:
        """総合スコアを計算."""
        weights = {
            "syntax": 0.25,
            "semantic": 0.30,
            "test": 0.20,
            "security": 0.15,
            "performance": 0.10,
        }
        self.overall_score = (
            self.syntax_score * weights["syntax"]
            + self.semantic_score * weights["semantic"]
            + self.test_coverage * weights["test"]
            + self.security_score * weights["security"]
            + self.performance_score * weights["performance"]
        )
        return self.overall_score

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "syntax_score": self.syntax_score,
            "semantic_score": self.semantic_score,
            "test_coverage": self.test_coverage,
            "security_score": self.security_score,
            "performance_score": self.performance_score,
            "overall_score": self.overall_score,
        }


@dataclass
class MigrationProject:
    """迁移项目.

    Attributes:
        id: プロジェクトID
        name: プロジェクト名
        source_language: ソース言語
        target_language: ターゲット言語
        source_root: ソースルートディレクトリ
        target_root: ターゲットルートディレクトリ
        source_files: ソースファイルマップ
        phases: フェーズ記録
        quality_metrics: 品質メトリクス
        current_phase: 現在のフェーズ
        metadata: メタデータ
    """

    name: str
    source_language: str
    target_language: str
    source_root: str = ""
    target_root: str = ""
    source_files: dict[UUID, SourceFile] = field(default_factory=dict)
    phases: list[PhaseRecord] = field(default_factory=list)
    quality_metrics: QualityMetrics = field(default_factory=QualityMetrics)
    current_phase: MigrationPhase | None = None
    metadata: dict[str, Any] = field(default_factory=dict)
    id: UUID = field(default_factory=uuid4)
    created_at: datetime = field(default_factory=lambda: datetime.now(UTC))
    updated_at: datetime = field(default_factory=lambda: datetime.now(UTC))

    def __post_init__(self) -> None:
        """初期化後処理."""
        if not self.phases:
            # デフォルトフェーズを作成
            for phase in MigrationPhase:
                self.phases.append(PhaseRecord(phase=phase))

    def add_file(self, file: SourceFile) -> None:
        """ファイルを追加."""
        if not file.language:
            file.language = self.source_language
        self.source_files[file.id] = file
        self.updated_at = datetime.now(UTC)

    def remove_file(self, file_id: UUID) -> bool:
        """ファイルを削除."""
        if file_id in self.source_files:
            del self.source_files[file_id]
            self.updated_at = datetime.now(UTC)
            return True
        return False

    def get_file(self, file_id: UUID) -> SourceFile | None:
        """ファイルを取得."""
        return self.source_files.get(file_id)

    def get_files_by_status(self, status: FileStatus) -> list[SourceFile]:
        """ステータスでファイルを取得."""
        return [f for f in self.source_files.values() if f.status == status]

    def start_phase(self, phase: MigrationPhase) -> PhaseRecord:
        """フェーズを開始."""
        for record in self.phases:
            if record.phase == phase:
                record.status = PhaseStatus.IN_PROGRESS
                record.started_at = datetime.now(UTC)
                self.current_phase = phase
                self.updated_at = datetime.now(UTC)
                return record
        msg = f"Phase not found: {phase}"
        raise ValueError(msg)

    def complete_phase(
        self,
        phase: MigrationPhase,
        files_processed: int = 0,
        errors: int = 0,
        notes: str = "",
    ) -> PhaseRecord:
        """フェーズを完了."""
        for record in self.phases:
            if record.phase == phase:
                record.status = PhaseStatus.COMPLETED if errors == 0 else PhaseStatus.FAILED
                record.completed_at = datetime.now(UTC)
                record.files_processed = files_processed
                record.errors = errors
                record.notes = notes
                self.updated_at = datetime.now(UTC)
                return record
        msg = f"Phase not found: {phase}"
        raise ValueError(msg)

    def get_phase_record(self, phase: MigrationPhase) -> PhaseRecord | None:
        """フェーズ記録を取得."""
        for record in self.phases:
            if record.phase == phase:
                return record
        return None

    def get_progress(self) -> dict[str, Any]:
        """進捗を取得."""
        total_files = len(self.source_files)
        completed_files = len(self.get_files_by_status(FileStatus.VALIDATED))
        failed_files = len(self.get_files_by_status(FileStatus.FAILED))

        completed_phases = sum(1 for p in self.phases if p.status == PhaseStatus.COMPLETED)
        total_phases = len(self.phases)

        return {
            "total_files": total_files,
            "completed_files": completed_files,
            "failed_files": failed_files,
            "file_progress": completed_files / total_files * 100 if total_files > 0 else 0,
            "completed_phases": completed_phases,
            "total_phases": total_phases,
            "phase_progress": completed_phases / total_phases * 100 if total_phases > 0 else 0,
            "current_phase": self.current_phase.value if self.current_phase else None,
        }

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "id": str(self.id),
            "name": self.name,
            "source_language": self.source_language,
            "target_language": self.target_language,
            "source_root": self.source_root,
            "target_root": self.target_root,
            "source_files": {str(k): v.to_dict() for k, v in self.source_files.items()},
            "phases": [p.to_dict() for p in self.phases],
            "quality_metrics": self.quality_metrics.to_dict(),
            "current_phase": self.current_phase.value if self.current_phase else None,
            "metadata": self.metadata,
            "created_at": self.created_at.isoformat(),
            "updated_at": self.updated_at.isoformat(),
            "progress": self.get_progress(),
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "MigrationProject":
        """辞書から作成."""
        project = cls(
            id=UUID(data["id"]) if "id" in data else uuid4(),
            name=data["name"],
            source_language=data["source_language"],
            target_language=data["target_language"],
            source_root=data.get("source_root", ""),
            target_root=data.get("target_root", ""),
            metadata=data.get("metadata", {}),
        )

        # ファイルを復元
        for file_id, file_data in data.get("source_files", {}).items():
            file = SourceFile.from_dict(file_data)
            project.source_files[file.id] = file

        # フェーズを復元
        if "phases" in data:
            project.phases = []
            for phase_data in data["phases"]:
                record = PhaseRecord(
                    phase=MigrationPhase(phase_data["phase"]),
                    status=PhaseStatus(phase_data.get("status", "pending")),
                    files_processed=phase_data.get("files_processed", 0),
                    errors=phase_data.get("errors", 0),
                    notes=phase_data.get("notes", ""),
                )
                project.phases.append(record)

        # 品質メトリクス
        if "quality_metrics" in data:
            qm = data["quality_metrics"]
            project.quality_metrics = QualityMetrics(
                syntax_score=qm.get("syntax_score", 0.0),
                semantic_score=qm.get("semantic_score", 0.0),
                test_coverage=qm.get("test_coverage", 0.0),
                security_score=qm.get("security_score", 0.0),
                performance_score=qm.get("performance_score", 0.0),
                overall_score=qm.get("overall_score", 0.0),
            )

        if "current_phase" in data and data["current_phase"]:
            project.current_phase = MigrationPhase(data["current_phase"])

        return project

    @classmethod
    def from_directory(
        cls,
        name: str,
        source_dir: Path | str,
        source_language: str,
        target_language: str,
        extensions: list[str] | None = None,
    ) -> "MigrationProject":
        """ディレクトリから作成."""
        source_dir = Path(source_dir)
        project = cls(
            name=name,
            source_language=source_language,
            target_language=target_language,
            source_root=str(source_dir),
        )

        # 拡張子マッピング
        default_extensions = {
            "cobol": [".cob", ".cbl", ".cpy"],
            "vb6": [".bas", ".frm", ".cls"],
            "delphi": [".pas", ".dfm", ".dpr"],
            "java": [".java"],
            "python": [".py"],
            "typescript": [".ts", ".tsx"],
        }

        exts = extensions or default_extensions.get(source_language.lower(), [])

        # ファイルを収集
        for ext in exts:
            for file_path in source_dir.rglob(f"*{ext}"):
                relative_path = file_path.relative_to(source_dir)
                source_file = SourceFile(
                    path=str(relative_path),
                    language=source_language,
                    loc=len(file_path.read_text(encoding="utf-8", errors="ignore").splitlines()),
                )
                project.add_file(source_file)

        return project


__all__ = [
    "PhaseStatus",
    "FileStatus",
    "MigrationPhase",
    "SourceFile",
    "PhaseRecord",
    "QualityMetrics",
    "MigrationProject",
]
