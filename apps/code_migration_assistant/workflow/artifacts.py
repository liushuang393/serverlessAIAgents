"""成果物ストア.

Agent 間通信を artifacts/ 配下の JSON/生成物に限定するための I/O ユーティリティ。
"""

from __future__ import annotations

import asyncio
import json
from pathlib import Path
from typing import Any


_APP_ROOT = Path(__file__).resolve().parents[1]


class ArtifactStore:
    """成果物の永続化を担うストア."""

    _STAGE_DIR_MAP: dict[str, str] = {
        "analysis": "analysis",
        "design": "design",
        "code": "code",
        "tests": "tests",
        "diff": "diff",
        "quality": "quality",
        "fix": "fix",
        "logs": "logs",
    }

    def __init__(
        self,
        base_dir: Path | None = None,
        decisions_path: Path | None = None,
        failures_path: Path | None = None,
    ) -> None:
        """初期化.

        Args:
            base_dir: 成果物ルートディレクトリ
            decisions_path: DECISIONS.md の保存先
            failures_path: FAILURES.md の保存先
        """
        self._base_dir = base_dir or (_APP_ROOT / "artifacts")
        self._decisions_path = decisions_path or (_APP_ROOT / "DECISIONS.md")
        self._failures_path = failures_path or (_APP_ROOT / "FAILURES.md")
        self._tasks_dir = _APP_ROOT / "current_tasks"

    async def initialize(self) -> None:
        """ディレクトリを初期化."""
        self._base_dir.mkdir(parents=True, exist_ok=True)
        self._tasks_dir.mkdir(parents=True, exist_ok=True)
        for stage_dir in self._STAGE_DIR_MAP.values():
            (self._base_dir / stage_dir).mkdir(parents=True, exist_ok=True)

    def _resolve_stage_dir(self, stage: str) -> Path:
        """ステージディレクトリを解決."""
        directory = self._STAGE_DIR_MAP.get(stage)
        if directory is None:
            msg = f"Unknown stage: {stage}"
            raise ValueError(msg)
        return self._base_dir / directory

    async def write_json(
        self,
        *,
        stage: str,
        task_id: str,
        artifact_name: str,
        payload: dict[str, Any],
    ) -> Path:
        """JSON成果物を書き込む."""
        directory = self._resolve_stage_dir(stage)
        path = directory / f"{task_id}_{artifact_name}.json"
        text = json.dumps(payload, ensure_ascii=False, indent=2)
        await asyncio.to_thread(path.write_text, text, encoding="utf-8")
        return path

    async def read_json(self, path: Path) -> dict[str, Any]:
        """JSON成果物を読み込む."""
        raw = await asyncio.to_thread(path.read_text, encoding="utf-8")
        return json.loads(raw)

    async def write_text(
        self,
        *,
        stage: str,
        task_id: str,
        artifact_name: str,
        content: str,
        extension: str = "txt",
    ) -> Path:
        """テキスト成果物を書き込む."""
        directory = self._resolve_stage_dir(stage)
        path = directory / f"{task_id}_{artifact_name}.{extension}"
        await asyncio.to_thread(path.write_text, content, encoding="utf-8")
        return path

    async def append_decision(self, task_id: str, message: str) -> None:
        """DECISIONS.md に追記."""
        line = f"- task={task_id}: {message}\n"
        await asyncio.to_thread(self._append_to_file, self._decisions_path, line)

    async def append_failure(
        self,
        *,
        task_id: str,
        stage: str,
        responsible_stage: str,
        reason: str,
    ) -> None:
        """FAILURES.md に追記."""
        line = f"- task={task_id} stage={stage} responsible={responsible_stage} reason={reason}\n"
        await asyncio.to_thread(self._append_to_file, self._failures_path, line)

    @staticmethod
    def _append_to_file(path: Path, content: str) -> None:
        """ファイルに追記するヘルパー（スレッドプール用）."""
        with path.open("a", encoding="utf-8") as handle:
            handle.write(content)

    async def acquire_lock(self, task_id: str) -> Path:
        """タスクロックを取得."""
        lock_path = self._tasks_dir / f"{task_id}.lock"
        await asyncio.to_thread(lock_path.write_text, task_id, encoding="utf-8")
        return lock_path

    async def release_lock(self, lock_path: Path) -> None:
        """タスクロックを解放."""
        if lock_path.exists():
            await asyncio.to_thread(lock_path.unlink)
