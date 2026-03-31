"""汎用スキルパックローダー & GitHub 同期パッケージ.

SKILL.md フロントマター形式のスキルを自動検出・ロードし、
pack_manifest.json を持つパックは GitHub から自動更新する。

使い方:
    # GitHub 同期 → 全パック自動検出・ロード
    >>> await sync_all_packs()
    >>> discover_and_load_all_packs(gateway)

    # 特定パック一括ロード
    >>> load_skill_pack(gateway, Path("skills/minimalist-entrepreneur-skills"))

    # タスク向けスキルセット
    >>> load_skills_for_task(gateway, task="product_launch")

    # 単一スキルロード
    >>> load_skill(gateway, Path("skills/pack/pricing/SKILL.md"))
"""

from apps.messaging_hub.skills.entrepreneurship.loader import (
    TASK_SKILL_SETS,
    discover_and_load_all_packs,
    load_skill,
    load_skill_pack,
    load_skills_for_task,
    register_entrepreneurship_skills,
)
from apps.messaging_hub.skills.entrepreneurship.sync import (
    sync_all_packs,
    sync_skill_pack,
)


__all__ = [
    "TASK_SKILL_SETS",
    "discover_and_load_all_packs",
    "load_skill",
    "load_skill_pack",
    "load_skills_for_task",
    "register_entrepreneurship_skills",
    "sync_all_packs",
    "sync_skill_pack",
]
