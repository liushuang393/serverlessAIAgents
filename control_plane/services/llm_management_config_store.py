"""Persistent store for `.bizcore/llm_gateway.yaml` with atomic writes."""

from __future__ import annotations

import hashlib
import os
from contextlib import contextmanager
from pathlib import Path
from tempfile import NamedTemporaryFile
from typing import TYPE_CHECKING

import yaml

from infrastructure.llm.gateway import LLMGatewayConfig, load_gateway_config


if TYPE_CHECKING:
    from collections.abc import Iterator
    from types import ModuleType


def _load_fcntl() -> ModuleType | None:
    """fcntl モジュールを遅延読み込みする."""
    try:
        import fcntl
    except ImportError:  # pragma: no cover - non-posix fallback
        return None
    return fcntl


class LLMConfigStore:
    """Read/write gateway config atomically with lock protection."""

    def __init__(self, config_path: Path) -> None:
        self._config_path = config_path
        self._lock_path = config_path.with_suffix(f"{config_path.suffix}.lock")

    @property
    def config_path(self) -> Path:
        """Return canonical config path."""
        return self._config_path

    def load(self) -> LLMGatewayConfig:
        """Load config from disk (creates default when missing)."""
        with self._lock(exclusive=True):
            return load_gateway_config(self._config_path)

    def load_with_version(self) -> tuple[LLMGatewayConfig, str | None]:
        """Load config and current version digest."""
        with self._lock(exclusive=True):
            config = load_gateway_config(self._config_path)
            version = self._file_version()
        return config, version

    def save(self, config: LLMGatewayConfig) -> str:
        """Persist config with atomic replace and return new version."""
        payload = yaml.safe_dump(
            config.model_dump(mode="python"),
            sort_keys=False,
            allow_unicode=True,
        )
        with self._lock(exclusive=True):
            self._config_path.parent.mkdir(parents=True, exist_ok=True)
            with NamedTemporaryFile(
                mode="w",
                encoding="utf-8",
                dir=str(self._config_path.parent),
                prefix=f"{self._config_path.name}.",
                suffix=".tmp",
                delete=False,
            ) as temp:
                temp.write(payload)
                temp.flush()
                os.fsync(temp.fileno())
                temp_path = Path(temp.name)

            temp_path.replace(self._config_path)
            version = self._file_version()
        return version or ""

    def version(self) -> str | None:
        """Return current config version digest."""
        with self._lock(exclusive=False):
            return self._file_version()

    def exists(self) -> bool:
        """Return whether config file exists."""
        return self._config_path.exists()

    def _file_version(self) -> str | None:
        if not self._config_path.exists():
            return None
        digest = hashlib.sha256(self._config_path.read_bytes()).hexdigest()
        return digest[:16]

    @contextmanager
    def _lock(self, *, exclusive: bool) -> Iterator[None]:
        """Lock helper. Uses advisory file lock when available."""
        self._lock_path.parent.mkdir(parents=True, exist_ok=True)
        lock_file = self._lock_path.open("a+", encoding="utf-8")
        fcntl_module = _load_fcntl()
        try:
            if fcntl_module is not None:
                flag = fcntl_module.LOCK_EX if exclusive else fcntl_module.LOCK_SH
                fcntl_module.flock(lock_file.fileno(), flag)
            yield
        finally:
            if fcntl_module is not None:
                fcntl_module.flock(lock_file.fileno(), fcntl_module.LOCK_UN)
            lock_file.close()
