# -*- coding: utf-8 -*-
"""Code Migration Assistant Skills.

コード移行に関するSkillを提供する。
"""

# 注意: cobol-migration Skill はSKILL.md形式を使用する
# SkillLoaderで読み込める
# もしくはCobolMigrationSkillを直接インポートする

from pathlib import Path

# Skillディレクトリのパス
SKILLS_DIR = Path(__file__).parent

__all__ = ["SKILLS_DIR"]
