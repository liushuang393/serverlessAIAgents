# -*- coding: utf-8 -*-
"""ルートスキルディレクトリからの読み込みテスト."""

import pytest
from pathlib import Path


class TestRootSkillsDirectory:
    """ルートスキルディレクトリのテスト."""

    def test_root_skills_directory_exists(self):
        """ルートスキルディレクトリが存在することを確認."""
        root_dir = Path(__file__).parent.parent.parent.parent / "skills"
        assert root_dir.exists(), f"skills/ ディレクトリが存在しません: {root_dir}"

    def test_root_skills_subdirectories_exist(self):
        """サブディレクトリが存在することを確認."""
        root_dir = Path(__file__).parent.parent.parent.parent / "skills"

        # builtin, user, apps サブディレクトリ
        assert (root_dir / "builtin").exists(), "skills/builtin/ が存在しません"
        assert (root_dir / "user").exists(), "skills/user/ が存在しません"
        assert (root_dir / "apps").exists(), "skills/apps/ が存在しません"

    def test_load_from_root_skills_directory(self):
        """ルートスキルディレクトリからの読み込みテスト."""
        from agentflow.skills.loader import SkillLoader

        loader = SkillLoader()

        # ルートスキルディレクトリ
        root_dir = Path(__file__).parent.parent.parent.parent / "skills"

        if root_dir.exists():
            skills = loader.load_directory(root_dir, recursive=True)
            # 読み込み成功を確認
            assert isinstance(skills, list)

    def test_readme_exists(self):
        """README.md が存在することを確認."""
        root_dir = Path(__file__).parent.parent.parent.parent / "skills"
        readme = root_dir / "README.md"
        assert readme.exists(), "skills/README.md が存在しません"
