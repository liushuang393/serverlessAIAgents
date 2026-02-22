"""ルートスキルディレクトリからの読み込みテスト."""

from pathlib import Path


class TestRootSkillsDirectory:
    """ルートスキルディレクトリのテスト."""

    @staticmethod
    def _skill_dir_candidates() -> list[Path]:
        repo_root = Path(__file__).parent.parent.parent.parent
        return [
            repo_root / "skills",
            repo_root / "agentflow" / "skills",
        ]

    def test_root_skills_directory_exists(self):
        """ルートスキルディレクトリが存在することを確認."""
        candidates = self._skill_dir_candidates()
        assert any(path.exists() for path in candidates), "skills ディレクトリが見つかりません: " + ", ".join(
            str(path) for path in candidates
        )

    def test_root_skills_subdirectories_exist(self):
        """サブディレクトリが存在することを確認."""
        builtin_candidates = [path / "builtin" for path in self._skill_dir_candidates()]
        assert any(path.exists() for path in builtin_candidates), "builtin スキルディレクトリが見つかりません"

    def test_load_from_root_skills_directory(self):
        """ルートスキルディレクトリからの読み込みテスト."""
        from agentflow.skills.loader import SkillLoader

        loader = SkillLoader()
        root_dir = next((path for path in self._skill_dir_candidates() if path.exists()), None)
        assert root_dir is not None, "スキルディレクトリが存在しません"
        skills = loader.load_directory(root_dir, recursive=True)
        # 読み込み成功を確認
        assert isinstance(skills, list)

    def test_readme_exists(self):
        """README.md が存在することを確認."""
        doc_candidates: list[Path] = []
        for root_dir in self._skill_dir_candidates():
            doc_candidates.extend([root_dir / "README.md", root_dir / "AGENTS.md"])
        assert any(path.exists() for path in doc_candidates), "skills の説明ファイルが見つかりません: " + ", ".join(
            str(path) for path in doc_candidates
        )
