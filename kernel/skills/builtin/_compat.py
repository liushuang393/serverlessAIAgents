"""後方互換ヘルパー: 旧パスから新パスへのモジュールエイリアスを登録する."""

from __future__ import annotations

import importlib
import sys


def register_submodule_alias(
    old_prefix: str,
    new_prefix: str,
    submodules: list[str],
) -> None:
    """旧モジュールパスを新パスへ sys.modules エイリアスで登録する.

    Args:
        old_prefix: 旧パッケージ接頭辞 (例: "kernel.skills.builtin.rag")
        new_prefix: 新パッケージ接頭辞 (例: "shared.skills.builtin.rag")
        submodules: サブモジュール名リスト (例: ["rag"])
    """
    # パッケージ本体をエイリアス
    try:
        pkg = importlib.import_module(new_prefix)
        sys.modules.setdefault(old_prefix, pkg)
    except ImportError:
        return

    # サブモジュールをエイリアス
    for sub in submodules:
        new_path = f"{new_prefix}.{sub}"
        old_path = f"{old_prefix}.{sub}"
        try:
            mod = importlib.import_module(new_path)
            sys.modules.setdefault(old_path, mod)
        except ImportError:
            pass
