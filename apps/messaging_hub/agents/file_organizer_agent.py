"""ファイル整理エージェント.

ディレクトリの分析、ファイル整理、重複検出、クリーンアップを担当。

使用例:
    >>> agent = FileOrganizerAgent(gateway)
    >>> analysis = await agent.analyze_directory("~/Downloads")
    >>> result = await agent.organize("~/Downloads", dry_run=True)
"""

from __future__ import annotations

import hashlib
import logging
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from agentflow.skills.gateway import SkillGateway


@dataclass
class DirectoryAnalysis:
    """ディレクトリ分析結果.

    Attributes:
        path: 分析パス
        total_files: 総ファイル数
        total_dirs: 総ディレクトリ数
        total_size_bytes: 総サイズ（バイト）
        by_category: カテゴリ別統計
        by_extension: 拡張子別統計
        old_files: 古いファイルリスト
        large_files: 大きいファイルリスト
        empty_dirs: 空ディレクトリリスト
        recommendations: 推奨アクション
    """

    path: str
    total_files: int = 0
    total_dirs: int = 0
    total_size_bytes: int = 0
    by_category: dict[str, dict[str, Any]] = field(default_factory=dict)
    by_extension: dict[str, int] = field(default_factory=dict)
    old_files: list[dict[str, Any]] = field(default_factory=list)
    large_files: list[dict[str, Any]] = field(default_factory=list)
    empty_dirs: list[str] = field(default_factory=list)
    recommendations: list[str] = field(default_factory=list)
    analyzed_at: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "path": self.path,
            "total_files": self.total_files,
            "total_dirs": self.total_dirs,
            "total_size_mb": round(self.total_size_bytes / (1024 * 1024), 2),
            "by_category": self.by_category,
            "by_extension": self.by_extension,
            "old_files_count": len(self.old_files),
            "large_files_count": len(self.large_files),
            "empty_dirs_count": len(self.empty_dirs),
            "recommendations": self.recommendations,
            "analyzed_at": self.analyzed_at.isoformat(),
        }


@dataclass
class OrganizationResult:
    """整理結果.

    Attributes:
        files_moved: 移動したファイル数
        files_renamed: リネームしたファイル数
        dirs_created: 作成したディレクトリ数
        errors: エラーリスト
        actions: 実行したアクションリスト
        dry_run: ドライランか
    """

    files_moved: int = 0
    files_renamed: int = 0
    dirs_created: int = 0
    errors: list[str] = field(default_factory=list)
    actions: list[dict[str, Any]] = field(default_factory=list)
    dry_run: bool = True

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "files_moved": self.files_moved,
            "files_renamed": self.files_renamed,
            "dirs_created": self.dirs_created,
            "errors": self.errors,
            "actions": self.actions[:50],  # 最大50件
            "total_actions": len(self.actions),
            "dry_run": self.dry_run,
        }


@dataclass
class DuplicateGroup:
    """重複ファイルグループ.

    Attributes:
        hash: ファイルハッシュ
        size: ファイルサイズ
        files: ファイルパスリスト
    """

    hash: str
    size: int
    files: list[str]

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "hash": self.hash[:16] + "...",
            "size_mb": round(self.size / (1024 * 1024), 2),
            "files": self.files,
            "duplicate_count": len(self.files) - 1,
            "potential_savings_mb": round(self.size * (len(self.files) - 1) / (1024 * 1024), 2),
        }


class FileOrganizerAgent:
    """ファイル整理エージェント.

    SkillGateway経由でファイルシステムを操作し、
    インテリジェントなファイル整理を行う。
    """

    # カテゴリ定義
    CATEGORIES = {
        "documents": {
            "extensions": {
                ".pdf",
                ".doc",
                ".docx",
                ".xls",
                ".xlsx",
                ".ppt",
                ".pptx",
                ".txt",
                ".md",
                ".rtf",
                ".odt",
            },
            "emoji": "📄",
        },
        "images": {
            "extensions": {
                ".jpg",
                ".jpeg",
                ".png",
                ".gif",
                ".bmp",
                ".svg",
                ".webp",
                ".ico",
                ".tiff",
                ".raw",
            },
            "emoji": "🖼️",
        },
        "videos": {
            "extensions": {".mp4", ".avi", ".mov", ".wmv", ".mkv", ".webm", ".flv", ".m4v"},
            "emoji": "🎬",
        },
        "audio": {
            "extensions": {".mp3", ".wav", ".flac", ".aac", ".ogg", ".wma", ".m4a"},
            "emoji": "🎵",
        },
        "archives": {
            "extensions": {".zip", ".rar", ".7z", ".tar", ".gz", ".bz2", ".xz"},
            "emoji": "📦",
        },
        "code": {
            "extensions": {
                ".py",
                ".js",
                ".ts",
                ".java",
                ".cpp",
                ".c",
                ".h",
                ".css",
                ".html",
                ".json",
                ".yaml",
                ".yml",
            },
            "emoji": "💻",
        },
        "executables": {
            "extensions": {".exe", ".msi", ".dmg", ".app", ".deb", ".rpm"},
            "emoji": "⚙️",
        },
    }

    def __init__(
        self,
        gateway: SkillGateway | None = None,
        days_old_threshold: int = 30,
        large_file_mb: int = 100,
    ) -> None:
        """初期化.

        Args:
            gateway: スキルゲートウェイ
            days_old_threshold: 古いファイルの閾値（日）
            large_file_mb: 大きいファイルの閾値（MB）
        """
        self._gateway = gateway
        self._days_old = days_old_threshold
        self._large_file_bytes = large_file_mb * 1024 * 1024
        self._logger = logging.getLogger(__name__)

    def _get_category(self, filename: str) -> str:
        """ファイル名からカテゴリを取得."""
        ext = Path(filename).suffix.lower()
        for category, config in self.CATEGORIES.items():
            if ext in config["extensions"]:
                return category
        return "others"

    async def analyze_directory(
        self,
        path: str,
        recursive: bool = True,
    ) -> DirectoryAnalysis:
        """ディレクトリを分析.

        Args:
            path: 分析対象パス
            recursive: 再帰的に分析するか

        Returns:
            分析結果
        """
        expanded_path = str(Path(path).expanduser())
        analysis = DirectoryAnalysis(path=expanded_path)

        # Gateway経由でファイル一覧を取得
        if self._gateway:
            try:
                result = await self._gateway.call("list_dir", {"path": expanded_path})
                if not result.success:
                    self._logger.error("ディレクトリ一覧取得失敗: %s", result.error)
                    return analysis

                files = result.result or []
            except Exception as e:
                self._logger.exception("Gateway呼び出しエラー: %s", e)
                return analysis
        else:
            # ローカルファイルシステムを直接使用（フォールバック）
            try:
                p = Path(expanded_path)
                if not p.exists():
                    return analysis
                files = [
                    {
                        "name": f.name,
                        "size": f.stat().st_size if f.is_file() else 0,
                        "modified": f.stat().st_mtime,
                        "is_dir": f.is_dir(),
                    }
                    for f in p.iterdir()
                ]
            except Exception as e:
                self._logger.exception("ローカルファイル取得エラー: %s", e)
                return analysis

        # 分析
        import time

        now = time.time()
        threshold_seconds = self._days_old * 24 * 60 * 60

        by_category: dict[str, dict[str, Any]] = defaultdict(lambda: {"count": 0, "size": 0, "files": []})
        by_extension: dict[str, int] = defaultdict(int)

        for file_info in files:
            name = file_info.get("name", "")
            size = file_info.get("size", 0)
            modified = file_info.get("modified", now)
            is_dir = file_info.get("is_dir", False)

            if is_dir:
                analysis.total_dirs += 1
                # 空ディレクトリチェック（簡易版）
                continue

            analysis.total_files += 1
            analysis.total_size_bytes += size

            # カテゴリ分類
            category = self._get_category(name)
            by_category[category]["count"] += 1
            by_category[category]["size"] += size

            # 拡張子統計
            ext = Path(name).suffix.lower()
            if ext:
                by_extension[ext] += 1

            # 古いファイル
            if isinstance(modified, (int, float)):
                age_seconds = now - modified
                if age_seconds > threshold_seconds:
                    analysis.old_files.append(
                        {
                            "name": name,
                            "size_mb": round(size / (1024 * 1024), 2),
                            "age_days": int(age_seconds / (24 * 60 * 60)),
                        }
                    )

            # 大きいファイル
            if size > self._large_file_bytes:
                analysis.large_files.append(
                    {
                        "name": name,
                        "size_mb": round(size / (1024 * 1024), 2),
                    }
                )

        analysis.by_category = {k: dict(v) for k, v in by_category.items()}
        analysis.by_extension = dict(by_extension)

        # 推奨アクションを生成
        analysis.recommendations = self._generate_recommendations(analysis)

        self._logger.info(
            "ディレクトリ分析完了: path=%s, files=%d, size=%dMB",
            expanded_path,
            analysis.total_files,
            round(analysis.total_size_bytes / (1024 * 1024)),
        )

        return analysis

    def _generate_recommendations(self, analysis: DirectoryAnalysis) -> list[str]:
        """推奨アクションを生成."""
        recommendations = []

        if len(analysis.old_files) > 10:
            total_old_size = sum(f.get("size_mb", 0) for f in analysis.old_files)
            recommendations.append(
                f"古いファイル{len(analysis.old_files)}件（計{total_old_size:.1f}MB）の削除を検討してください"
            )

        if len(analysis.large_files) > 5:
            recommendations.append(
                f"大きいファイル{len(analysis.large_files)}件があります。外部ストレージへの移動を検討してください"
            )

        if analysis.total_files > 100:
            recommendations.append("ファイル数が多いです。カテゴリ別にフォルダ分けすることをお勧めします")

        if len(analysis.empty_dirs) > 0:
            recommendations.append(f"空のディレクトリ{len(analysis.empty_dirs)}件の削除を検討してください")

        return recommendations

    async def organize(
        self,
        path: str,
        rules: dict[str, Any] | None = None,
        dry_run: bool = True,
    ) -> OrganizationResult:
        """ファイルを整理.

        Args:
            path: 整理対象パス
            rules: 整理ルール（カテゴリ別のサブフォルダ名等）
            dry_run: True の場合は実際の操作を行わない

        Returns:
            整理結果
        """
        expanded_path = str(Path(path).expanduser())
        result = OrganizationResult(dry_run=dry_run)

        # デフォルトルール
        default_rules = {
            "create_category_folders": True,
            "category_names": {
                "documents": "Documents",
                "images": "Images",
                "videos": "Videos",
                "audio": "Audio",
                "archives": "Archives",
                "code": "Code",
                "executables": "Programs",
                "others": "Others",
            },
        }
        rules = {**default_rules, **(rules or {})}

        # 分析
        analysis = await self.analyze_directory(expanded_path, recursive=False)

        if analysis.total_files == 0:
            return result

        # LLMで整理計画を生成
        plan = await self._generate_organization_plan(analysis, rules)

        # アクションを実行/記録
        for action in plan:
            action_type = action.get("type")
            action.get("source")
            target = action.get("target")

            if action_type == "create_dir":
                result.actions.append(action)
                if not dry_run and self._gateway:
                    # Gateway経由でディレクトリ作成
                    try:
                        await self._gateway.call(
                            "write_file",
                            {
                                "path": f"{target}/.keep",
                                "content": "",
                            },
                        )
                        result.dirs_created += 1
                    except Exception as e:
                        result.errors.append(f"ディレクトリ作成失敗: {target} - {e}")
                else:
                    result.dirs_created += 1

            elif action_type == "move":
                result.actions.append(action)
                if not dry_run and self._gateway:
                    # Gateway経由でファイル移動（読み取り→書き込み→削除）
                    # 注: 実際の実装ではmoveコマンドを使用
                    result.errors.append("move操作は未実装（dry_runモードで使用してください）")
                else:
                    result.files_moved += 1

            elif action_type == "rename":
                result.actions.append(action)
                if not dry_run:
                    result.errors.append("rename操作は未実装（dry_runモードで使用してください）")
                else:
                    result.files_renamed += 1

        self._logger.info(
            "ファイル整理完了: path=%s, moved=%d, renamed=%d, dry_run=%s",
            expanded_path,
            result.files_moved,
            result.files_renamed,
            dry_run,
        )

        return result

    async def _generate_organization_plan(
        self,
        analysis: DirectoryAnalysis,
        rules: dict[str, Any],
    ) -> list[dict[str, Any]]:
        """整理計画を生成."""
        actions: list[dict[str, Any]] = []
        category_names = rules.get("category_names", {})

        # カテゴリフォルダの作成
        if rules.get("create_category_folders"):
            for category in analysis.by_category:
                folder_name = category_names.get(category, category.capitalize())
                target_path = str(Path(analysis.path) / folder_name)
                actions.append(
                    {
                        "type": "create_dir",
                        "target": target_path,
                    }
                )

        # TODO: 実際のファイル移動計画（Gateway経由でファイル一覧を再取得して計画）

        return actions

    async def smart_rename(
        self,
        path: str,
        pattern: str | None = None,
    ) -> OrganizationResult:
        """スマートリネーム.

        Args:
            path: 対象パス
            pattern: リネームパターン（Noneの場合はLLMで生成）

        Returns:
            リネーム結果
        """
        return OrganizationResult(dry_run=True)

        # TODO: LLMを使用してファイル名を分析し、統一的なリネームを提案

    async def find_duplicates(
        self,
        path: str,
        by_content: bool = True,
    ) -> list[DuplicateGroup]:
        """重複ファイルを検出.

        Args:
            path: 検索パス
            by_content: 内容で比較（Falseの場合はファイル名とサイズ）

        Returns:
            重複グループリスト
        """
        expanded_path = str(Path(path).expanduser())
        duplicates: list[DuplicateGroup] = []

        # Gateway経由でファイル一覧を取得
        if self._gateway:
            result = await self._gateway.call("list_dir", {"path": expanded_path})
            if not result.success:
                return duplicates
            files = result.result or []
        else:
            return duplicates

        # サイズでグループ化
        size_groups: dict[int, list[dict[str, Any]]] = defaultdict(list)
        for f in files:
            if not f.get("is_dir", False):
                size_groups[f.get("size", 0)].append(f)

        # 同サイズファイルを詳細比較
        for size, file_list in size_groups.items():
            if len(file_list) < 2:
                continue

            if by_content:
                # 内容ハッシュで比較（簡易版：ファイル名+サイズ）
                # 実際にはファイル内容のハッシュを計算
                hash_groups: dict[str, list[str]] = defaultdict(list)
                for f in file_list:
                    # 簡易ハッシュ（実際にはファイル内容をハッシュ）
                    simple_hash = hashlib.md5(f"{f.get('name', '')}_{size}".encode()).hexdigest()
                    hash_groups[simple_hash].append(str(Path(expanded_path) / f.get("name", "")))

                for file_hash, paths in hash_groups.items():
                    if len(paths) > 1:
                        duplicates.append(
                            DuplicateGroup(
                                hash=file_hash,
                                size=size,
                                files=paths,
                            )
                        )
            else:
                # 名前とサイズで比較
                duplicates.append(
                    DuplicateGroup(
                        hash="size_match",
                        size=size,
                        files=[str(Path(expanded_path) / f.get("name", "")) for f in file_list],
                    )
                )

        self._logger.info("重複検出完了: path=%s, groups=%d", expanded_path, len(duplicates))

        return duplicates

    async def cleanup_old_files(
        self,
        path: str,
        days_old: int = 30,
        dry_run: bool = True,
    ) -> OrganizationResult:
        """古いファイルをクリーンアップ.

        Args:
            path: 対象パス
            days_old: 何日以上古いファイルを対象にするか
            dry_run: True の場合は実際の削除を行わない

        Returns:
            クリーンアップ結果
        """
        result = OrganizationResult(dry_run=dry_run)

        # 分析で古いファイルを特定
        old_threshold = self._days_old
        self._days_old = days_old
        analysis = await self.analyze_directory(path)
        self._days_old = old_threshold

        for old_file in analysis.old_files:
            action = {
                "type": "delete",
                "target": str(Path(path).expanduser() / old_file["name"]),
                "reason": f"{old_file.get('age_days', 0)}日以上古い",
            }
            result.actions.append(action)

            if not dry_run:
                result.errors.append("delete操作は未実装（dry_runモードで使用してください）")

        self._logger.info(
            "クリーンアップ完了: path=%s, old_files=%d, dry_run=%s",
            path,
            len(analysis.old_files),
            dry_run,
        )

        return result
