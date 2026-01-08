# -*- coding: utf-8 -*-
"""Code Migration Assistant Skills.

提供代码移行相关的 Skill。
"""

# 注意: cobol-migration Skill 使用 SKILL.md 格式
# 可以通过 SkillLoader 加载
# 或者直接导入 CobolMigrationSkill 类

from pathlib import Path

# Skill 目录路径
SKILLS_DIR = Path(__file__).parent

__all__ = ["SKILLS_DIR"]

