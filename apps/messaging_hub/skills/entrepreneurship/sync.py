"""スキルパック GitHub 同期モジュール.

pack_manifest.json を持つスキルパックに対して、
GitHub リポジトリの最新コミットを確認し、更新があればダウンロード・上書きする。

フロー:
    1. pack_manifest.json から source.repo, branch, local_commit_sha を読み取る
    2. GitHub API で最新コミット SHA を取得
    3. local_commit_sha と比較 → 同じならスキップ
    4. 異なれば tarball をダウンロード → 展開 → ローカル上書き
    5. pack_manifest.json の local_commit_sha を更新

使用例:
    >>> updated = await sync_skill_pack(
    ...     Path("skills/minimalist-entrepreneur-skills")
    ... )
    >>> if updated:
    ...     print("スキルパック更新済み")
"""

from __future__ import annotations

import json
import logging
import shutil
import tarfile
import tempfile
from io import BytesIO
from pathlib import Path
from typing import Any

import aiohttp


logger = logging.getLogger(__name__)

# GitHub API タイムアウト（秒）
_API_TIMEOUT = 15
_DOWNLOAD_TIMEOUT = 60


async def _get_latest_commit_sha(
    repo: str,
    branch: str,
) -> str | None:
    """GitHub API で最新コミット SHA を取得する.

    Args:
        repo: owner/repo 形式（例: "slavingia/skills"）
        branch: ブランチ名

    Returns:
        コミット SHA。取得失敗時は None
    """
    url = f"https://api.github.com/repos/{repo}/commits/{branch}"
    headers = {
        "Accept": "application/vnd.github.v3+json",
        "User-Agent": "AgentFlow-SkillSync",
    }

    try:
        timeout = aiohttp.ClientTimeout(total=_API_TIMEOUT)
        async with aiohttp.ClientSession(timeout=timeout) as session, session.get(url, headers=headers) as resp:
            if resp.status != 200:
                logger.warning(
                    "GitHub API エラー: %s/%s → %d",
                    repo,
                    branch,
                    resp.status,
                )
                return None
            data = await resp.json()
            sha = data.get("sha", "")
            return sha if isinstance(sha, str) and sha else None
    except (TimeoutError, aiohttp.ClientError) as e:
        logger.warning("GitHub API 接続エラー: %s", e)
        return None


async def _download_and_extract(
    repo: str,
    branch: str,
    skills_path: str,
    target_dir: Path,
) -> bool:
    """GitHub tarball をダウンロードし、skills_path 部分を target_dir に展開する.

    Args:
        repo: owner/repo 形式
        branch: ブランチ名
        skills_path: リポジトリ内のスキルディレクトリパス（例: "skills"）
        target_dir: ローカルの展開先ディレクトリ

    Returns:
        成功したかどうか
    """
    url = f"https://api.github.com/repos/{repo}/tarball/{branch}"
    headers = {
        "Accept": "application/vnd.github.v3+json",
        "User-Agent": "AgentFlow-SkillSync",
    }

    try:
        timeout = aiohttp.ClientTimeout(total=_DOWNLOAD_TIMEOUT)
        async with aiohttp.ClientSession(timeout=timeout) as session, session.get(url, headers=headers) as resp:
            if resp.status != 200:
                logger.warning("tarball ダウンロード失敗: %d", resp.status)
                return False
            data = await resp.read()
    except (TimeoutError, aiohttp.ClientError) as e:
        logger.warning("tarball ダウンロードエラー: %s", e)
        return False

    # tarball を一時ディレクトリに展開
    try:
        with tempfile.TemporaryDirectory(prefix="skill_sync_") as tmp:
            tmp_path = Path(tmp)
            tar_buf = BytesIO(data)
            with tarfile.open(fileobj=tar_buf, mode="r:gz") as tar:
                tar.extractall(path=tmp_path, filter="data")

            # tarball のトップレベルディレクトリを特定
            # （GitHub は "<owner>-<repo>-<sha[:7]>/" 形式）
            top_dirs = [d for d in tmp_path.iterdir() if d.is_dir()]
            if not top_dirs:
                logger.warning("tarball にディレクトリが見つかりません")
                return False

            source_root = top_dirs[0] / skills_path
            if not source_root.is_dir():
                logger.warning("skills_path '%s' が tarball 内に見つかりません", skills_path)
                return False

            # ローカルのスキルディレクトリを更新
            # SKILL.md を含むサブディレクトリのみコピー（マニフェスト等は保持）
            updated_count = 0
            for skill_dir in sorted(source_root.iterdir()):
                if not skill_dir.is_dir():
                    continue
                skill_md = skill_dir / "SKILL.md"
                if not skill_md.exists():
                    continue

                local_skill_dir = target_dir / skill_dir.name
                # 既存のスキルディレクトリがあれば削除して置き換え
                if local_skill_dir.exists():
                    shutil.rmtree(local_skill_dir)
                shutil.copytree(skill_dir, local_skill_dir)
                updated_count += 1

            logger.info("スキルファイル %d 件を更新しました", updated_count)
            return updated_count > 0

    except (tarfile.TarError, OSError) as e:
        logger.warning("tarball 展開エラー: %s", e)
        return False


def _read_manifest(pack_dir: Path) -> dict[str, Any] | None:
    """pack_manifest.json を読み込む.

    Args:
        pack_dir: スキルパックのディレクトリ

    Returns:
        マニフェスト辞書。存在しない場合は None
    """
    manifest_path = pack_dir / "pack_manifest.json"
    if not manifest_path.exists():
        return None
    try:
        return json.loads(manifest_path.read_text(encoding="utf-8"))  # type: ignore[no-any-return]
    except (OSError, json.JSONDecodeError) as e:
        logger.warning("マニフェスト読み込みエラー: %s", e)
        return None


def _write_manifest(pack_dir: Path, manifest: dict[str, Any]) -> None:
    """pack_manifest.json を書き込む.

    Args:
        pack_dir: スキルパックのディレクトリ
        manifest: マニフェスト辞書
    """
    manifest_path = pack_dir / "pack_manifest.json"
    try:
        manifest_path.write_text(
            json.dumps(manifest, indent=2, ensure_ascii=False) + "\n",
            encoding="utf-8",
        )
    except OSError as e:
        logger.warning("マニフェスト書き込みエラー: %s", e)


async def sync_skill_pack(pack_dir: Path) -> bool:
    """スキルパックを GitHub の最新版と同期する.

    pack_manifest.json が存在し、source.type == "github" の場合のみ動作。
    ネットワークエラーや API 制限時はスキップ（ローカルのまま動作継続）。

    Args:
        pack_dir: スキルパックのディレクトリ

    Returns:
        更新があった場合 True
    """
    manifest = _read_manifest(pack_dir)
    if manifest is None:
        return False

    source = manifest.get("source", {})
    if source.get("type") != "github":
        return False

    repo = source.get("repo", "")
    branch = source.get("branch", "main")
    skills_path = source.get("skills_path", "skills")

    if not repo:
        logger.warning("マニフェストに repo が未設定: %s", pack_dir.name)
        return False

    # 1. 最新コミット SHA を取得
    remote_sha = await _get_latest_commit_sha(repo, branch)
    if remote_sha is None:
        logger.info(
            "GitHub API に接続できません。ローカル版を使用: %s",
            pack_dir.name,
        )
        return False

    # 2. ローカル SHA と比較
    local_sha = manifest.get("local_commit_sha")
    if local_sha == remote_sha:
        logger.info(
            "スキルパック '%s' は最新です (SHA: %s)",
            pack_dir.name,
            remote_sha[:8],
        )
        return False

    # 3. 更新がある → ダウンロード・展開
    logger.info(
        "スキルパック '%s' の更新を検出: %s → %s",
        pack_dir.name,
        (local_sha or "初回")[:8],
        remote_sha[:8],
    )

    success = await _download_and_extract(repo, branch, skills_path, pack_dir)
    if not success:
        logger.warning("スキルパック '%s' の更新に失敗しました", pack_dir.name)
        return False

    # 4. マニフェストの SHA を更新
    manifest["local_commit_sha"] = remote_sha
    _write_manifest(pack_dir, manifest)
    logger.info(
        "スキルパック '%s' を更新しました (SHA: %s)",
        pack_dir.name,
        remote_sha[:8],
    )
    return True


async def sync_all_packs(skills_root: Path | None = None) -> dict[str, bool]:
    """全スキルパックの GitHub 同期を実行する.

    pack_manifest.json を持つ全パックを検出し、順次同期する。

    Args:
        skills_root: スキルのルートディレクトリ

    Returns:
        パック名 → 更新があったかの辞書
    """
    from apps.messaging_hub.skills.entrepreneurship.loader import _SKILLS_ROOT

    root = skills_root or _SKILLS_ROOT
    if not root.is_dir():
        return {}

    results: dict[str, bool] = {}
    for candidate in sorted(root.iterdir()):
        if not candidate.is_dir():
            continue
        manifest_path = candidate / "pack_manifest.json"
        if manifest_path.exists():
            updated = await sync_skill_pack(candidate)
            results[candidate.name] = updated

    total_updated = sum(1 for v in results.values() if v)
    logger.info(
        "全パック同期完了: %d パック確認、%d 件更新",
        len(results),
        total_updated,
    )
    return results
