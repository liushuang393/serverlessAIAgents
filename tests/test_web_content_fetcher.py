"""web-content-fetcher Skill 統合テスト.

フレームワークの自動パイプラインを検証:
  SkillEngine（自動発見） → SkillMatcher（自然言語マッチ） → SkillRuntime（自動実行）

パス指定なし・スクリプト名指定なし。
ユーザーが自然言語で依頼 → Markdown が返る、を検証。

実行方法:
    pytest tests/test_web_content_fetcher.py -v --no-cov
"""

from __future__ import annotations

import re

import pytest

from kernel.skills.core.base import Skill
from kernel.skills.core.loader import SkillLoader, SkillRegistry
from kernel.skills.core.matcher import SkillMatcher
from kernel.skills.core.runtime import SkillRuntime


# ---------------------------------------------------------------------------
# fixture — パス指定なし、フレームワークのデフォルト機構のみ使用
# ---------------------------------------------------------------------------

@pytest.fixture(scope="module")
def loaded_skills() -> list[Skill]:
    """SkillLoader.load_default_paths() でデフォルトパスから自動発見."""
    SkillRegistry._instance = None
    loader = SkillLoader()
    skills = loader.load_default_paths()
    assert len(skills) > 0, "デフォルトパスから Skill が1つもロードされません"
    return skills


@pytest.fixture(scope="module")
def matcher(loaded_skills: list[Skill]) -> SkillMatcher:
    """ロード済み Skill で SkillMatcher を構築."""
    return SkillMatcher(skills=loaded_skills)


@pytest.fixture(scope="module")
def runtime() -> SkillRuntime:
    """SkillRuntime を構築."""
    return SkillRuntime()


# ---------------------------------------------------------------------------
# 1. 自動発見テスト — パス指定なし
# ---------------------------------------------------------------------------
class TestAutoDiscovery:
    """SkillLoader.load_default_paths() が web-content-fetcher を自動発見."""

    def test_skill_discovered(self, loaded_skills: list[Skill]):
        """デフォルトパスから web-content-fetcher が見つかる."""
        names = [s.name for s in loaded_skills]
        assert "web-content-fetcher" in names

    def test_skill_has_triggers(self, loaded_skills: list[Skill]):
        """triggers が定義されていてマッチングに使える."""
        skill = next(s for s in loaded_skills if s.name == "web-content-fetcher")
        assert len(skill.metadata.triggers) > 0, "triggers が未定義"

    def test_to_prompt_contains_strategies(self, loaded_skills: list[Skill]):
        """to_prompt() に三段階フォールバック戦略が含まれる."""
        skill = next(s for s in loaded_skills if s.name == "web-content-fetcher")
        prompt = skill.to_prompt()
        assert "Jina" in prompt
        assert "Scrapling" in prompt or "scrapling" in prompt


# ---------------------------------------------------------------------------
# 2. 自然言語マッチングテスト
# ---------------------------------------------------------------------------
class TestQueryMatching:
    """ユーザーの自然言語クエリから自動選出."""

    @pytest.mark.parametrize(
        "query",
        [
            "このURLの記事を要約して: https://mp.weixin.qq.com/s/xxx",
            "この記事の内容を日本語で整理して https://example.com/article",
            "网页内容を抽出して",
            "提取这个链接的正文内容",
            "Webページの正文を取得したい",
            "抓取某个URL的正文内容",
        ],
    )
    def test_matches_web_fetch_queries(self, matcher: SkillMatcher, query: str):
        """Webコンテンツ取得系クエリにマッチ."""
        results = matcher.match(query, top_k=3)
        matched_names = [r.skill.name for r in results]
        assert "web-content-fetcher" in matched_names, (
            f"'{query}' にマッチしません。結果: {[(r.skill.name, r.score) for r in results]}"
        )

    def test_best_match(self, matcher: SkillMatcher):
        """URL抽出リクエストで best match."""
        best = matcher.find_best("この記事を読み取って要約して https://example.com")
        assert best is not None
        assert best.name == "web-content-fetcher"

    def test_no_false_positive(self, matcher: SkillMatcher):
        """無関係なクエリにはマッチしない."""
        results = matcher.match("天気予報を教えて")
        matched_names = [r.skill.name for r in results]
        assert "web-content-fetcher" not in matched_names


# ---------------------------------------------------------------------------
# 3. スクリプト自動実行テスト — スクリプト名指定なし
# ---------------------------------------------------------------------------
class TestAutoExecution:
    """SkillRuntime.run() がスクリプトを自動選択・実行."""

    def test_auto_select_script(self, loaded_skills: list[Skill], runtime: SkillRuntime):
        """_auto_select_script で fetch が自動選択される."""
        skill = next(s for s in loaded_skills if s.name == "web-content-fetcher")
        selected = runtime._auto_select_script(skill)
        assert selected == "fetch", f"自動選択されたスクリプト: {selected}"

    @pytest.mark.network
    async def test_runtime_run_auto(self, loaded_skills: list[Skill], runtime: SkillRuntime):
        """runtime.run() でスクリプト名指定なしで実行."""
        skill = next(s for s in loaded_skills if s.name == "web-content-fetcher")
        result = await runtime.run(
            skill=skill,
            input_data={"url": "https://github.com/anthropics/claude-code", "max_chars": 5000},
        )
        assert result.success, f"実行失敗: {result.error}"
        assert result.output.get("markdown"), f"Markdown 空: {result.output.get('error', '')}"
        assert result.output.get("chars", 0) > 0


# ---------------------------------------------------------------------------
# 4. エンドツーエンド — 自然言語 → Markdown（全自動）
# ---------------------------------------------------------------------------
class TestEndToEnd:
    """ユーザーが自然言語で依頼 → フレームワークが全自動で Markdown を返す."""

    @pytest.mark.network
    async def test_wechat_full_pipeline(
        self, loaded_skills: list[Skill], matcher: SkillMatcher, runtime: SkillRuntime
    ):
        """微信記事: 自然言語 → 自動マッチ → 自動実行 → Markdown品質検証."""
        # Step 1: ユーザーの自然言語入力
        user_query = "この記事の内容を日本語で整理して https://mp.weixin.qq.com/s/ljMffydOigAl1muyLFhQhw"

        # Step 2: SkillMatcher が自動マッチ
        best = matcher.find_best(user_query)
        assert best is not None, "マッチする Skill がありません"
        assert best.name == "web-content-fetcher"

        # Step 3: SkillRuntime.run() で自動実行（スクリプト名指定なし）
        result = await runtime.run(
            skill=best,
            input_data={
                "url": "https://mp.weixin.qq.com/s/ljMffydOigAl1muyLFhQhw",
                "max_chars": 30000,
            },
        )
        assert result.success, f"実行失敗: {result.error}"
        assert not result.output.get("error"), f"fetch エラー: {result.output['error']}"

        # Step 4: Markdown 品質検証
        md = result.output["markdown"]
        assert len(md) > 500, f"Markdown が短すぎ: {len(md)} chars"

        # 中国語コンテンツ（元記事）
        chinese_chars = re.findall(r"[\u4e00-\u9fff]", md)
        assert len(chinese_chars) > 50, "元記事のコンテンツ不足"

        # HTMLタグ残留なし
        html_tags = re.findall(r"<(div|span|script|style)\b", md, re.IGNORECASE)
        assert len(html_tags) == 0, f"HTMLタグ残留: {html_tags[:5]}"

        # Markdown構造
        assert "#" in md or "](http" in md or "![" in md, "Markdown構造なし"

    @pytest.mark.network
    async def test_github_full_pipeline(
        self, loaded_skills: list[Skill], matcher: SkillMatcher, runtime: SkillRuntime
    ):
        """GitHub: 自然言語 → 自動マッチ → 自動実行 → 品質検証."""
        best = matcher.find_best("このページの内容を読み取って https://github.com/anthropics/claude-code")
        assert best is not None
        assert best.name == "web-content-fetcher"

        result = await runtime.run(
            skill=best,
            input_data={"url": "https://github.com/anthropics/claude-code", "max_chars": 10000},
        )
        assert result.success, f"実行失敗: {result.error}"
        md = result.output.get("markdown", "")
        assert len(md) > 200
        assert "claude" in md.lower() or "anthropic" in md.lower()
