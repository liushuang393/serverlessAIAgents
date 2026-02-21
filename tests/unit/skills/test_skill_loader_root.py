"""ルートスキルディレクトリからの読み込みテスト."""

from pathlib import Path


class TestRootSkillsDirectory:
    """ルートスキルディレクトリのテスト."""

    def test_root_skills_directory_exists(self):
        """ルートスキルディレクトリが存在することを確認."""
        root_dir = Path(__file__).parent.parent.parent.parent / "skills"
        assert root_dir.exists(), f"skills/ ディレクトリが存在しません: {root_dir}"

    def test_root_skills_subdirectories_exist(self):
        """サブディレクトリが存在することを確認."""
        repo_root = Path(__file__).parent.parent.parent.parent
        root_dir = repo_root / "skills"

        # builtin はリポジトリの移行により agentflow/skills 配下へ配置される場合がある
        builtin_candidates = [
            root_dir / "builtin",
            repo_root / "agentflow" / "skills" / "builtin",
        ]
        assert any(path.exists() for path in builtin_candidates), "builtin スキルディレクトリが見つかりません"

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
