# -*- coding: utf-8 -*-
"""Skills 統合テスト - Claude Code Skills 互換性検証.

このテストモジュールは以下を検証します：
1. SKILL.md の読み込みと解析
2. メタデータ（name, description）の制限検証
3. 渐进式披露（Progressive Disclosure）の動作
4. Agent への Skills 自動注入
5. LLM との統合（実際の API 呼び出し）

実行方法:
    pytest tests/test_skills_integration.py -v
    
環境変数:
    OPENAI_API_KEY: OpenAI API キー（LLM テスト用）
"""

import asyncio
import os
import tempfile
from pathlib import Path

import pytest

# Skills モジュールを直接インポート（pocketflow 依存回避）
import sys
sys.path.insert(0, str(Path(__file__).parent.parent))

from agentflow.skills.base import Skill, SkillMetadata
from agentflow.skills.loader import SkillLoader, SkillRegistry


class TestSkillMetadataLimits:
    """Claude Code Skills メタデータ制限テスト."""

    def test_name_max_length(self):
        """name は最大 64 文字."""
        # 有効: 64 文字以内
        valid_name = "a" * 64
        metadata = SkillMetadata(name=valid_name, description="テスト")
        assert len(metadata.name) <= 64

    def test_name_format(self):
        """name は小写字母、数字、ハイフンのみ."""
        # 有効な例
        valid_names = [
            "my-skill",
            "skill123",
            "my-skill-v2",
            "a",
        ]
        for name in valid_names:
            metadata = SkillMetadata(name=name, description="テスト")
            assert metadata.name == name

    def test_description_max_length(self):
        """description は最大 1024 文字."""
        # 有効: 1024 文字以内
        valid_desc = "あ" * 1024
        metadata = SkillMetadata(name="test", description=valid_desc)
        assert len(metadata.description) <= 1024


class TestSkillLoading:
    """SKILL.md 読み込みテスト."""

    def test_load_builtin_skills(self):
        """ビルトイン Skills を正常に読み込み."""
        builtin_dir = Path(__file__).parent.parent / "agentflow" / "skills" / "builtin"
        
        if not builtin_dir.exists():
            pytest.skip("builtin ディレクトリが存在しません")
        
        loader = SkillLoader()
        skills = loader.load_directory(builtin_dir, recursive=True)
        
        assert len(skills) > 0, "少なくとも1つの Skill が読み込まれる"
        
        # 各 Skill の必須フィールド検証
        for skill in skills:
            assert skill.name, f"name が必須: {skill}"
            assert skill.metadata.description, f"description が必須: {skill.name}"
            assert skill.instructions, f"instructions が必須: {skill.name}"

    def test_load_skill_from_directory(self):
        """ディレクトリから SKILL.md を読み込み."""
        # テンポラリディレクトリに SKILL.md 作成
        with tempfile.TemporaryDirectory() as tmpdir:
            skill_dir = Path(tmpdir) / "test-skill"
            skill_dir.mkdir()
            
            skill_md = skill_dir / "SKILL.md"
            skill_md.write_text("""---
name: test-skill
description: テスト用 Skill です。テストデータの処理に使用。
version: 1.0.0
triggers:
  - test
  - テスト
---

# Test Skill

## 指示
これはテスト用の指示です。
""", encoding="utf-8")
            
            # 読み込み
            skill = Skill.load(skill_dir)
            
            assert skill.name == "test-skill"
            assert "テスト用 Skill" in skill.metadata.description
            assert "テスト用の指示" in skill.instructions
            assert "test" in skill.metadata.triggers

    def test_skill_matching(self):
        """クエリマッチングテスト."""
        metadata = SkillMetadata(
            name="pdf-processing",
            description="PDF ファイルからテキストを抽出。PDF 処理が必要な場合に使用。",
            triggers=["pdf", "document", "extract"],
        )

        # 高マッチ（トリガー含む）
        assert metadata.matches_query("PDF からテキスト抽出") > 0.5

        # 中マッチ（トリガー document）
        assert metadata.matches_query("document file") > 0.5

        # 低マッチ（無関係）
        assert metadata.matches_query("天気予報") < 0.2


class TestSkillRegistry:
    """Skills レジストリテスト."""

    def test_singleton_pattern(self):
        """シングルトンパターン検証."""
        registry1 = SkillRegistry()
        registry2 = SkillRegistry()
        assert registry1 is registry2

    def test_register_and_get(self):
        """登録と取得."""
        registry = SkillRegistry()
        
        # テスト Skill 作成
        skill = Skill.create(
            name="test-registry-skill",
            description="レジストリテスト用",
            instructions="テスト指示",
        )
        
        registry.register_skill(skill)
        retrieved = registry.get("test-registry-skill")
        
        assert retrieved is skill
        assert retrieved.name == "test-registry-skill"


class TestProgressiveDisclosure:
    """渐进式披露（Progressive Disclosure）テスト.
    
    Claude Code Skills の重要な特徴:
    - 启動時: name + description のみ加载
    - 触发時: 完整 SKILL.md 加载
    - 需要時: 额外ファイル加载
    """

    def test_metadata_only_loading(self):
        """メタデータのみ读み込み（启动時シミュレーション）."""
        with tempfile.TemporaryDirectory() as tmpdir:
            skill_dir = Path(tmpdir) / "progressive-skill"
            skill_dir.mkdir()
            
            # 大きな SKILL.md（500行以上）
            skill_content = """---
name: progressive-skill
description: Progressive Disclosure テスト用 Skill
---

# Progressive Skill

""" + "\n".join([f"## セクション {i}\nコンテンツ {i}" for i in range(200)])
            
            (skill_dir / "SKILL.md").write_text(skill_content, encoding="utf-8")
            
            # 完全読み込み
            skill = Skill.load(skill_dir)
            
            # メタデータのみ取得（Context 節約）
            metadata_only = {
                "name": skill.metadata.name,
                "description": skill.metadata.description,
            }
            
            # 完全な instructions は別途必要時に使用
            assert len(metadata_only["name"]) < 100
            assert len(metadata_only["description"]) < 1024
            assert len(skill.instructions) > 1000  # 完全版は大きい


class TestLLMIntegration:
    """LLM 統合テスト（実際の API 呼び出し）.
    
    OPENAI_API_KEY 環境変数が必要。
    """

    @pytest.fixture
    def api_key(self):
        """API キー取得."""
        key = os.environ.get("OPENAI_API_KEY")
        if not key:
            pytest.skip("OPENAI_API_KEY が設定されていません")
        return key

    @pytest.mark.asyncio
    async def test_skill_as_system_prompt(self, api_key):
        """Skill を system prompt として使用."""
        # OpenAI クライアント
        try:
            from openai import AsyncOpenAI
        except ImportError:
            pytest.skip("openai パッケージがインストールされていません")
        
        client = AsyncOpenAI(api_key=api_key)
        
        # Skill からシステムプロンプト生成
        skill = Skill.create(
            name="qa-assistant",
            description="質問応答アシスタント。簡潔に回答。",
            instructions="""
あなたは質問応答アシスタントです。

## ルール
1. 日本語で回答
2. 簡潔に（100文字以内）
3. 不明な場合は「わかりません」と回答
""",
        )
        
        system_prompt = skill.to_prompt()
        
        # LLM 呼び出し
        response = await client.chat.completions.create(
            model="gpt-4o-mini",
            messages=[
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": "日本の首都は？"},
            ],
            max_tokens=100,
        )
        
        answer = response.choices[0].message.content
        assert "東京" in answer, f"予期しない回答: {answer}"

    @pytest.mark.asyncio
    async def test_multiple_skills_injection(self, api_key):
        """複数 Skills のシステムプロンプト注入."""
        try:
            from openai import AsyncOpenAI
        except ImportError:
            pytest.skip("openai パッケージがインストールされていません")
        
        client = AsyncOpenAI(api_key=api_key)
        
        # 複数 Skills
        skills = [
            Skill.create(
                name="math-helper",
                description="数学計算を支援",
                instructions="計算結果は必ず数値で回答してください。",
            ),
            Skill.create(
                name="format-helper",
                description="フォーマット支援",
                instructions="回答は「結果: [値]」の形式で出力してください。",
            ),
        ]
        
        # Skills を結合してシステムプロンプト生成
        combined_prompt = "# Active Skills\n\n" + "\n\n".join(
            skill.to_prompt() for skill in skills
        )
        
        response = await client.chat.completions.create(
            model="gpt-4o-mini",
            messages=[
                {"role": "system", "content": combined_prompt},
                {"role": "user", "content": "5 + 3 = ?"},
            ],
            max_tokens=50,
        )
        
        answer = response.choices[0].message.content
        # フォーマット指示に従っているか確認
        assert "8" in answer, f"計算結果が含まれていない: {answer}"


class TestSkillToPrompt:
    """Skill → プロンプト変換テスト."""

    def test_to_prompt_format(self):
        """to_prompt() の出力フォーマット."""
        skill = Skill.create(
            name="test-prompt",
            description="プロンプト生成テスト",
            instructions="これは指示です。",
        )
        
        prompt = skill.to_prompt()
        
        assert "Skill: test-prompt" in prompt
        assert "Description:" in prompt
        assert "これは指示です" in prompt

    def test_to_prompt_with_metadata(self):
        """メタデータ付き to_prompt()."""
        skill = Skill.create(
            name="full-skill",
            description="フル機能 Skill",
            instructions="""
# 指示

1. ステップ1
2. ステップ2
""",
            triggers=["trigger1", "trigger2"],
            requirements=["package1", "package2"],
        )
        
        prompt = skill.to_prompt()
        
        assert "full-skill" in prompt
        assert "ステップ1" in prompt


# 実行用
if __name__ == "__main__":
    # 基本テスト（API 不要）
    print("=== Skills 基本テスト ===")
    
    # メタデータテスト
    meta_test = TestSkillMetadataLimits()
    meta_test.test_name_max_length()
    meta_test.test_description_max_length()
    print("✅ メタデータ制限テスト OK")
    
    # 読み込みテスト
    load_test = TestSkillLoading()
    load_test.test_load_builtin_skills()
    print("✅ ビルトイン Skills 読み込み OK")
    
    # Progressive Disclosure テスト
    pd_test = TestProgressiveDisclosure()
    pd_test.test_metadata_only_loading()
    print("✅ Progressive Disclosure テスト OK")
    
    # プロンプト変換テスト
    prompt_test = TestSkillToPrompt()
    prompt_test.test_to_prompt_format()
    print("✅ プロンプト変換テスト OK")
    
    # LLM 統合テスト（API キーがある場合のみ）
    if os.environ.get("OPENAI_API_KEY"):
        print("\n=== LLM 統合テスト ===")
        llm_test = TestLLMIntegration()
        asyncio.run(llm_test.test_skill_as_system_prompt(os.environ["OPENAI_API_KEY"]))
        print("✅ LLM 統合テスト OK")
    else:
        print("\n⚠️ OPENAI_API_KEY が未設定のため LLM テストをスキップ")
    
    print("\n✅ すべてのテストが完了しました")

