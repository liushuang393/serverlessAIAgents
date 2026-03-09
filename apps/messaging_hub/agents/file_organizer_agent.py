"""ファイル整理エージェント.

ディレクトリの分析、ファイル整理、重複検出、クリーンアップを担当。

使用例:
    >>> agent = FileOrganizerAgent(gateway)
    >>> analysis = await agent.analyze_directory("~/Downloads")
    >>> result = await agent.organize("~/Downloads", dry_run=True)
"""
# ruff: noqa: ASYNC240

from __future__ import annotations

import hashlib
import logging
import shutil
import time
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

    @staticmethod
    def _normalize_mtime(modified: Any, fallback: float) -> float:
        """modified 値を epoch 秒へ正規化."""
        if isinstance(modified, (int, float)):
            return float(modified)
        if isinstance(modified, datetime):
            return modified.timestamp()
        if isinstance(modified, str):
            try:
                return datetime.fromisoformat(modified.replace("Z", "+00:00")).timestamp()
            except ValueError:
                return fallback
        return fallback

    def _normalize_file_record(self, base: Path, raw: dict[str, Any], now: float) -> dict[str, Any]:
        """Gateway/ローカルのファイル情報を統一."""
        name_value = str(raw.get("name", "")).strip()
        path_raw = str(raw.get("path", name_value)).strip()
        rel_path = Path(path_raw) if path_raw else Path(name_value)
        if rel_path.is_absolute():
            full_path = rel_path
            rel_path = Path(name_value) if name_value else rel_path.name
        else:
            workspace_path = (Path.cwd() / rel_path).resolve()
            if workspace_path.exists() and self._is_subpath(workspace_path, base):
                full_path = workspace_path
                rel_path = workspace_path.relative_to(base)
            else:
                full_path = base / rel_path
        name = name_value or full_path.name

        is_dir = bool(raw.get("is_dir", False))
        is_file = bool(raw.get("is_file", not is_dir))
        if is_dir:
            is_file = False
        size = int(raw.get("size", raw.get("size_bytes", 0)) or 0)
        modified = self._normalize_mtime(raw.get("modified", raw.get("modified_at")), now)
        return {
            "name": name,
            "path": str(rel_path),
            "full_path": full_path,
            "size": size,
            "modified": modified,
            "is_dir": is_dir,
            "is_file": is_file,
        }

    @staticmethod
    def _coerce_record(raw: Any) -> dict[str, Any] | None:
        """Gateway 返却値を dict へ正規化する."""
        if isinstance(raw, dict):
            return raw
        if hasattr(raw, "model_dump"):
            dumped = raw.model_dump()  # type: ignore[no-any-return]
            if isinstance(dumped, dict):
                return dumped
        if hasattr(raw, "__dict__"):
            raw_dict = {key: value for key, value in vars(raw).items() if not key.startswith("_")}
            if raw_dict:
                return raw_dict
        return None

    async def _list_directory_records(self, base_path: str, recursive: bool) -> list[dict[str, Any]]:
        """ディレクトリを走査して統一フォーマットへ変換."""
        root = Path(base_path).expanduser().resolve()
        now = time.time()

        if self._gateway:
            try:
                result = await self._gateway.call("list_dir", {"path": str(root)})
                if not result.success:
                    self._logger.error("ディレクトリ一覧取得失敗: %s", result.error)
                    return []
                raw_items = result.result if isinstance(result.result, list) else []
                normalized: list[dict[str, Any]] = []
                for raw_item in raw_items:
                    item = self._coerce_record(raw_item)
                    if item is None:
                        continue
                    normalized.append(self._normalize_file_record(root, item, now))
            except Exception as exc:
                self._logger.exception("Gateway呼び出しエラー: %s", exc)
                return []
        else:
            if not root.exists() or not root.is_dir():
                return []
            normalized = []
            for item in root.iterdir():
                stat = item.stat()
                normalized.append(
                    {
                        "name": item.name,
                        "path": item.name,
                        "full_path": item,
                        "size": int(stat.st_size),
                        "modified": float(stat.st_mtime),
                        "is_dir": item.is_dir(),
                        "is_file": item.is_file(),
                    }
                )

        if not recursive:
            return normalized

        recursive_rows = list(normalized)
        for row in normalized:
            if not row["is_dir"]:
                continue
            try:
                for child in Path(row["full_path"]).rglob("*"):
                    stat = child.stat()
                    recursive_rows.append(
                        {
                            "name": child.name,
                            "path": str(child.relative_to(root)),
                            "full_path": child,
                            "size": int(stat.st_size),
                            "modified": float(stat.st_mtime),
                            "is_dir": child.is_dir(),
                            "is_file": child.is_file(),
                        }
                    )
            except OSError as exc:
                self._logger.warning("再帰走査失敗: %s (%s)", row["full_path"], exc)
        return recursive_rows

    @staticmethod
    def _is_subpath(child: Path, parent: Path) -> bool:
        """child が parent 配下かを判定."""
        try:
            child.resolve().relative_to(parent.resolve())
            return True
        except ValueError:
            return False

    def _safe_target_name(self, raw_name: str) -> str:
        """危険なファイル名を排除した安全名へ変換."""
        cleaned = raw_name.strip().replace("/", "_").replace("\\", "_")
        return cleaned or "untitled"

    async def _delete_file(self, file_path: Path) -> None:
        """ファイル削除."""
        if self._gateway:
            # delete skill が標準提供されないため、ローカル安全パスで削除する。
            file_path.unlink(missing_ok=True)
            return
        file_path.unlink(missing_ok=True)

    async def _rename_file(self, source: Path, target: Path) -> None:
        """ファイル名変更."""
        if self._gateway:
            source.rename(target)
            return
        source.rename(target)

    async def _move_file(self, source: Path, target: Path) -> None:
        """ファイル移動."""
        if self._gateway:
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.move(str(source), str(target))
            return
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.move(str(source), str(target))

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
        files = await self._list_directory_records(expanded_path, recursive=recursive)
        if not files:
            return analysis
        root = Path(expanded_path).expanduser().resolve()
        now = time.time()
        threshold_seconds = self._days_old * 24 * 60 * 60

        by_category: dict[str, dict[str, Any]] = defaultdict(lambda: {"count": 0, "size": 0, "files": []})
        by_extension: dict[str, int] = defaultdict(int)

        for file_info in files:
            name = file_info.get("name", "")
            size = file_info.get("size", 0)
            modified = file_info.get("modified", now)
            is_dir = file_info.get("is_dir", False)
            full_path = Path(file_info.get("full_path", Path(expanded_path)))

            if is_dir:
                analysis.total_dirs += 1
                try:
                    if full_path.exists() and full_path.is_dir() and not any(full_path.iterdir()):
                        analysis.empty_dirs.append(str(full_path.relative_to(root)))
                except OSError:
                    pass
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
                            "path": str(file_info.get("path", name)),
                            "size_mb": round(size / (1024 * 1024), 2),
                            "age_days": int(age_seconds / (24 * 60 * 60)),
                        }
                    )

            # 大きいファイル
            if size > self._large_file_bytes:
                analysis.large_files.append(
                    {
                        "name": name,
                        "path": str(file_info.get("path", name)),
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
        root = Path(expanded_path).resolve()
        for action in plan:
            action_type = action.get("type")
            source = action.get("source")
            target = action.get("target")

            if action_type == "create_dir":
                result.actions.append(action)
                if not dry_run:
                    try:
                        Path(str(target)).mkdir(parents=True, exist_ok=True)
                        result.dirs_created += 1
                    except Exception as e:
                        result.errors.append(f"ディレクトリ作成失敗: {target} - {e}")
                else:
                    result.dirs_created += 1

            elif action_type == "move":
                result.actions.append(action)
                if dry_run:
                    result.files_moved += 1
                    continue
                source_path = Path(str(source)).resolve()
                target_path = Path(str(target)).resolve()
                if not self._is_subpath(source_path, root) or not self._is_subpath(target_path, root):
                    result.errors.append(f"move拒否（workspace外）: {source_path} -> {target_path}")
                    continue
                if not source_path.exists() or not source_path.is_file():
                    result.errors.append(f"move失敗（source不存在）: {source_path}")
                    continue
                if source_path == target_path:
                    continue
                try:
                    await self._move_file(source_path, target_path)
                    result.files_moved += 1
                except Exception as e:
                    result.errors.append(f"move失敗: {source_path} -> {target_path} ({e})")

            elif action_type == "rename":
                result.actions.append(action)
                if dry_run:
                    result.files_renamed += 1
                    continue
                source_path = Path(str(source)).resolve()
                target_path = Path(str(target)).resolve()
                if not self._is_subpath(source_path, root) or not self._is_subpath(target_path, root):
                    result.errors.append(f"rename拒否（workspace外）: {source_path} -> {target_path}")
                    continue
                if not source_path.exists() or not source_path.is_file():
                    result.errors.append(f"rename失敗（source不存在）: {source_path}")
                    continue
                if source_path == target_path:
                    continue
                try:
                    await self._rename_file(source_path, target_path)
                    result.files_renamed += 1
                except Exception as e:
                    result.errors.append(f"rename失敗: {source_path} -> {target_path} ({e})")

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
        source_files = await self._list_directory_records(analysis.path, recursive=False)

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

        root = Path(analysis.path).resolve()
        for file_info in source_files:
            if file_info.get("is_dir", False):
                continue
            source = Path(str(file_info["full_path"]))
            category = self._get_category(file_info.get("name", ""))
            folder_name = str(category_names.get(category, category.capitalize()))
            target_dir = root / folder_name
            target_file = target_dir / source.name
            if source.parent == target_dir:
                continue
            suffix_idx = 1
            while target_file.exists():
                target_file = target_dir / f"{source.stem}_{suffix_idx}{source.suffix}"
                suffix_idx += 1
            actions.append(
                {
                    "type": "move",
                    "source": str(source),
                    "target": str(target_file),
                    "category": category,
                }
            )

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
        expanded = Path(path).expanduser().resolve()
        result = OrganizationResult(dry_run=False)
        files = await self._list_directory_records(str(expanded), recursive=False)
        if not files:
            return result

        rename_pattern = pattern or "{index:03d}_{stem}{ext}"
        used_names: set[str] = set()
        for index, file_info in enumerate(sorted(files, key=lambda row: row.get("name", "")), start=1):
            if file_info.get("is_dir", False):
                continue
            source = Path(str(file_info["full_path"]))
            ext = source.suffix.lower()
            stem = self._safe_target_name(source.stem.lower().replace(" ", "_"))
            target_name = rename_pattern.format(index=index, stem=stem, ext=ext)
            target_name = self._safe_target_name(target_name)

            base = Path(target_name).stem
            suffix = Path(target_name).suffix
            candidate = target_name
            incr = 1
            while candidate in used_names or (expanded / candidate).exists():
                candidate = f"{base}_{incr}{suffix}"
                incr += 1
            used_names.add(candidate)
            target = expanded / candidate
            if source == target:
                continue
            result.actions.append(
                {
                    "type": "rename",
                    "source": str(source),
                    "target": str(target),
                }
            )
            try:
                await self._rename_file(source, target)
                result.files_renamed += 1
            except Exception as exc:
                result.errors.append(f"rename失敗: {source} -> {target} ({exc})")
        return result

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
        files = await self._list_directory_records(expanded_path, recursive=True)
        if not files:
            return duplicates

        # サイズでグループ化
        size_groups: dict[int, list[dict[str, Any]]] = defaultdict(list)
        for f in files:
            if f.get("is_file", False):
                size_groups[int(f.get("size", 0))].append(f)

        # 同サイズファイルを詳細比較
        for size, file_list in size_groups.items():
            if len(file_list) < 2:
                continue

            if by_content:
                hash_groups: dict[str, list[str]] = defaultdict(list)
                for f in file_list:
                    file_path = Path(str(f.get("full_path", "")))
                    if not file_path.exists() or not file_path.is_file():
                        continue
                    digest = hashlib.sha256()
                    try:
                        with file_path.open("rb") as fp:
                            while True:
                                chunk = fp.read(1024 * 1024)
                                if not chunk:
                                    break
                                digest.update(chunk)
                    except OSError as exc:
                        self._logger.warning("hash計算失敗: %s (%s)", file_path, exc)
                        continue
                    hash_groups[digest.hexdigest()].append(str(file_path))

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
                        files=[str(Path(str(f.get("full_path", "")))) for f in file_list],
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
            relative_target = str(old_file.get("path") or old_file.get("name", ""))
            target_file = Path(path).expanduser() / relative_target
            action = {
                "type": "delete",
                "target": str(target_file),
                "reason": f"{old_file.get('age_days', 0)}日以上古い",
            }
            result.actions.append(action)

            if not dry_run:
                try:
                    await self._delete_file(target_file)
                except Exception as exc:
                    result.errors.append(f"delete失敗: {target_file} ({exc})")

        self._logger.info(
            "クリーンアップ完了: path=%s, old_files=%d, dry_run=%s",
            path,
            len(analysis.old_files),
            dry_run,
        )

        return result
