"""プロジェクト起動時の import bootstrap.

stdlib ``platform`` が先に読み込まれた環境でも、
repo-root の ``platform/`` 実装を優先できるように差し替える。
"""

from __future__ import annotations

import importlib.util
import sys
from pathlib import Path


def _bootstrap_project_platform() -> None:
    """Layer 5 の ``platform`` package を優先登録する。"""
    package_init = Path(__file__).resolve().parent / "platform" / "__init__.py"
    if not package_init.exists():
        return

    current = sys.modules.get("platform")
    if current is not None and getattr(current, "__file__", None) == str(package_init):
        return

    spec = importlib.util.spec_from_file_location(
        "platform",
        package_init,
        submodule_search_locations=[str(package_init.parent)],
    )
    if spec is None or spec.loader is None:
        return

    module = importlib.util.module_from_spec(spec)
    sys.modules["platform"] = module
    spec.loader.exec_module(module)


_bootstrap_project_platform()
