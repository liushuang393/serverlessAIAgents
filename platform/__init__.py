"""Layer 5 の公開 API.

このパッケージ名は Python 標準ライブラリ ``platform`` と衝突するため、
stdlib の公開属性を透過的に再エクスポートしつつ、
BizCore Platform 固有の Layer 5 API も同居させる。
"""

from __future__ import annotations

import importlib
import importlib.util
import sysconfig
from pathlib import Path
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from types import ModuleType


def _load_stdlib_platform() -> ModuleType:
    """標準ライブラリ版 ``platform`` を別名で読み込む。"""
    stdlib_dir = sysconfig.get_path("stdlib")
    if stdlib_dir is None:
        msg = "stdlib の platform.py を解決できません"
        raise RuntimeError(msg)

    module_path = Path(stdlib_dir) / "platform.py"
    spec = importlib.util.spec_from_file_location("_bizcore_stdlib_platform", module_path)
    if spec is None or spec.loader is None:
        msg = f"stdlib platform の spec 解決に失敗しました: {module_path}"
        raise RuntimeError(msg)

    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


_STDLIB_PLATFORM = _load_stdlib_platform()
_RESERVED_NAMES = {
    "__builtins__",
    "__cached__",
    "__doc__",
    "__file__",
    "__loader__",
    "__name__",
    "__package__",
    "__path__",
    "__spec__",
}

for _name in dir(_STDLIB_PLATFORM):
    if _name in _RESERVED_NAMES:
        continue
    globals().setdefault(_name, getattr(_STDLIB_PLATFORM, _name))

_stdlib_all = list(getattr(_STDLIB_PLATFORM, "__all__", ()))
_SERVICE_MODULES = {
    "AppRegistryService": "platform.registry",
    "DiscoveryService": "platform.discovery",
    "LifecycleService": "platform.lifecycle",
}


def __getattr__(name: str) -> Any:
    """Layer 5 の公開サービスを遅延解決する。"""
    if name in _SERVICE_MODULES:
        module = importlib.import_module(_SERVICE_MODULES[name])
        return getattr(module, name)
    if hasattr(_STDLIB_PLATFORM, name):
        return getattr(_STDLIB_PLATFORM, name)

    msg = f"module 'platform' has no attribute {name!r}"
    raise AttributeError(msg)

__all__ = tuple(
    sorted(
        {
            *_stdlib_all,
            "AppRegistryService",
            "DiscoveryService",
            "LifecycleService",
        }
    )
)
