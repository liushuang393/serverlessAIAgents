"""Framework Env Service.

リポジトリローカルの `.env` 系ファイルを、コメントを保ったまま
キー単位で更新する。
"""

from __future__ import annotations

import re
from pathlib import Path


class FrameworkEnvService:
    """ローカル env 更新サービス."""

    _KEY_VALUE_PATTERN = re.compile(r"^\s*(?:export\s+)?([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(.*)$")

    def __init__(self, env_path: Path) -> None:
        """初期化.

        Args:
            env_path: 更新対象 env ファイルパス
        """
        self._env_path = env_path

    @property
    def env_path(self) -> Path:
        """更新対象パス."""
        return self._env_path

    def upsert(self, entries: dict[str, str]) -> list[str]:
        """キーを追加/更新して保存.

        Args:
            entries: KEY -> VALUE

        Returns:
            更新したキー一覧
        """
        self._env_path.parent.mkdir(parents=True, exist_ok=True)

        lines: list[str]
        if self._env_path.exists():
            lines = self._env_path.read_text("utf-8").splitlines()
        else:
            lines = []

        index: dict[str, int] = {}
        for i, line in enumerate(lines):
            match = self._KEY_VALUE_PATTERN.match(line)
            if match:
                index[match.group(1)] = i

        changed: list[str] = []
        for key, value in entries.items():
            rendered = f"{key}={self._format_value(value)}"
            if key in index:
                lines[index[key]] = rendered
            else:
                lines.append(rendered)
            changed.append(key)

        content = "\n".join(lines)
        self._env_path.write_text(content.rstrip() + "\n", encoding="utf-8")
        return changed

    @staticmethod
    def _format_value(value: str) -> str:
        """.env へ書き込む値を整形."""
        normalized = value.replace("\n", "\\n")
        if any(ch in normalized for ch in [" ", "\t", "#", '"']):
            escaped = normalized.replace("\\", "\\\\").replace('"', '\\"')
            return f'"{escaped}"'
        return normalized
