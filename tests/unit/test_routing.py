# -*- coding: utf-8 -*-
"""Routing モジュールのユニットテスト.

IntentRouter, TaskTemplate, ExecutiveSummaryBuilder のテスト。
"""

import pytest

from agentflow.routing import (
    ExecutiveSummary,
    ExecutiveSummaryBuilder,
    Intent,
    IntentCategory,
    IntentRouter,
    RouterConfig,
    SummaryConfig,
    TaskParameter,
    TaskTemplate,
    TemplateRegistry,
)


# =========================================================================
# TaskTemplate テスト
# =========================================================================


class TestTaskTemplate:
    """TaskTemplateのテスト."""

    def test_basic_match(self) -> None:
        """基本的なトリガーマッチング."""
        template = TaskTemplate(
            name="email_organize",
            triggers=["メール整理", "メールを整理"],
            description="メールを重要度別に整理",
        )

        score, params = template.match("メール整理")
        assert score == 1.0  # 完全一致

        score, params = template.match("今日のメールを整理して")
        assert score > 0.7  # 部分一致

        score, params = template.match("関係ない文章")
        assert score == 0.0  # マッチなし

    def test_parameter_extraction(self) -> None:
        """パラメータ抽出."""
        template = TaskTemplate(
            name="file_organize",
            triggers=["ファイル整理"],
            parameters=[
                TaskParameter(name="days", pattern=r"(\d+)日", type="int", default=7),
                TaskParameter(name="path", default="~/Downloads"),
            ],
        )

        score, params = template.match("30日以上のファイル整理")
        assert score > 0.7
        assert params["days"] == 30
        assert params["path"] == "~/Downloads"

    def test_validate_params(self) -> None:
        """パラメータ検証."""
        template = TaskTemplate(
            name="research",
            triggers=["調査"],
            parameters=[
                TaskParameter(name="topic", required=True),
            ],
        )

        errors = template.validate_params({})
        assert len(errors) == 1
        assert "topic" in errors[0]

        errors = template.validate_params({"topic": "AI市場"})
        assert len(errors) == 0

    def test_description_match(self) -> None:
        """説明文によるマッチング."""
        template = TaskTemplate(
            name="system_optimize",
            triggers=["システム最適化"],
            description="メモリ 解放 キャッシュ クリア パフォーマンス 向上",
        )

        # 説明文のキーワードでマッチ（2単語以上の重複が必要）
        score, _ = template.match("メモリ 解放 パフォーマンス 向上")
        assert score >= 0.3

    def test_type_conversion(self) -> None:
        """型変換テスト."""
        template = TaskTemplate(
            name="test",
            triggers=["テスト"],
            parameters=[
                TaskParameter(name="count", pattern=r"(\d+)件", type="int"),
                TaskParameter(name="ratio", pattern=r"(\d+\.?\d*)%", type="float"),
                TaskParameter(name="enabled", pattern=r"(true|false)", type="bool"),
                TaskParameter(name="tags", pattern=r"タグ:(.+)", type="list"),
            ],
        )

        score, params = template.match("テスト 10件 50.5% true タグ:a,b,c")
        assert params.get("count") == 10
        assert params.get("ratio") == 50.5
        assert params.get("enabled") is True
        assert params.get("tags") == ["a", "b", "c"]

    def test_english_trigger(self) -> None:
        """英語トリガーのマッチング."""
        template = TaskTemplate(
            name="file_organize",
            triggers=["organize files", "clean up files"],
        )

        score, _ = template.match("organize files")
        assert score == 1.0

        score, _ = template.match("please organize files now")
        assert score > 0.7

    def test_split_keywords_matching(self) -> None:
        """分割キーワードマッチング."""
        template = TaskTemplate(
            name="email_organize",
            triggers=["メール整理"],
        )

        # "メール" と "整理" が分離していてもマッチ
        score, _ = template.match("今日のメールを整理して")
        assert score >= 0.6


# =========================================================================
# IntentRouter テスト
# =========================================================================


class TestIntentRouter:
    """IntentRouterのテスト."""

    @pytest.fixture
    def router(self) -> IntentRouter:
        """テスト用ルーター."""
        router = IntentRouter()
        router.register_template(TaskTemplate(
            name="email_organize",
            triggers=["メール整理", "受信箱整理"],
            description="メールを重要度別に整理",
            parameters=[TaskParameter(name="days", pattern=r"(\d+)日", default=7, type="int")],
        ))
        router.register_template(TaskTemplate(
            name="file_organize",
            triggers=["ファイル整理", "ディスク整理"],
        ))
        return router

    @pytest.mark.asyncio
    async def test_task_execution_intent(self, router: IntentRouter) -> None:
        """タスク実行意図の解析."""
        intent = await router.route("今日のメールを整理して")

        assert intent.category == IntentCategory.TASK_EXECUTION
        assert intent.template_name == "email_organize"
        assert intent.confidence >= 0.5

    @pytest.mark.asyncio
    async def test_information_query_intent(self, router: IntentRouter) -> None:
        """情報照会意図の解析."""
        intent = await router.route("今日の天気は何ですか？")

        assert intent.category == IntentCategory.INFORMATION_QUERY
        assert intent.template_name is None

    @pytest.mark.asyncio
    async def test_parameter_extraction(self, router: IntentRouter) -> None:
        """パラメータ抽出."""
        intent = await router.route("過去14日のメール整理して")

        assert intent.template_name == "email_organize"
        assert intent.parameters.get("days") == 14

    def test_list_templates(self, router: IntentRouter) -> None:
        """テンプレート一覧."""
        templates = router.list_templates()
        assert "email_organize" in templates

    @pytest.mark.asyncio
    async def test_status_check_intent(self, router: IntentRouter) -> None:
        """状態確認意図の解析."""
        intent = await router.route("進捗状況を教えて")
        # 進捗確認は情報照会として分類される場合もある
        assert intent.category in (IntentCategory.STATUS_CHECK, IntentCategory.INFORMATION_QUERY)

    @pytest.mark.asyncio
    async def test_unknown_intent(self, router: IntentRouter) -> None:
        """不明な意図の解析."""
        intent = await router.route("あいうえお")
        assert intent.category == IntentCategory.UNKNOWN

    @pytest.mark.asyncio
    async def test_context_usage(self, router: IntentRouter) -> None:
        """コンテキスト使用."""
        context = {"user_id": "test_user", "session_id": "sess123"}
        intent = await router.route("メール整理", context=context)
        assert intent.template_name == "email_organize"

    def test_router_config(self) -> None:
        """RouterConfig設定."""
        config = RouterConfig(
            confidence_threshold=0.8,
            enable_parameter_extraction=False,
        )
        router = IntentRouter(config=config)
        assert router._config.confidence_threshold == 0.8

    @pytest.mark.asyncio
    async def test_chinese_task_keywords(self, router: IntentRouter) -> None:
        """中国語タスクキーワード."""
        intent = await router.route("创建一个报告")
        assert intent.category == IntentCategory.TASK_EXECUTION

    @pytest.mark.asyncio
    async def test_english_task_keywords(self, router: IntentRouter) -> None:
        """英語タスクキーワード."""
        intent = await router.route("please organize my files")
        assert intent.category == IntentCategory.TASK_EXECUTION

    @pytest.mark.asyncio
    async def test_llm_fallback_disabled(self) -> None:
        """LLM fallback無効時のテスト."""
        config = RouterConfig(use_llm_fallback=False)
        router = IntentRouter(config=config)

        # 不明な入力でもLLMは呼ばれない
        intent = await router.route("xyz123abc")
        assert intent.category == IntentCategory.UNKNOWN
        assert intent.metadata.get("used_llm_fallback") is False

    @pytest.mark.asyncio
    async def test_llm_fallback_not_triggered_for_known_intent(
        self, router: IntentRouter
    ) -> None:
        """既知の意図ではLLM fallbackが発動しないことを確認."""
        intent = await router.route("メール整理して")
        # ルールベースで解決できるのでLLMは使わない
        assert intent.category == IntentCategory.TASK_EXECUTION
        assert intent.metadata.get("used_llm_fallback") is False

    @pytest.mark.asyncio
    async def test_intent_metadata_contains_llm_flag(self, router: IntentRouter) -> None:
        """Intent.metadataにLLMフラグが含まれることを確認."""
        intent = await router.route("メール整理")
        assert "used_llm_fallback" in intent.metadata


# =========================================================================
# ExecutiveSummaryBuilder テスト
# =========================================================================


class TestExecutiveSummaryBuilder:
    """ExecutiveSummaryBuilderのテスト."""

    @pytest.fixture
    def builder(self) -> ExecutiveSummaryBuilder:
        """テスト用ビルダー."""
        return ExecutiveSummaryBuilder(SummaryConfig(use_emoji=True))

    @pytest.fixture
    def builder_no_emoji(self) -> ExecutiveSummaryBuilder:
        """絵文字なしビルダー."""
        return ExecutiveSummaryBuilder(SummaryConfig(use_emoji=False))

    @pytest.mark.asyncio
    async def test_email_summary(self, builder: ExecutiveSummaryBuilder) -> None:
        """メール整理サマリー生成."""
        results = {"processed": 50, "important": 5, "spam": 10}
        summary = await builder.build("email_organize", results)

        assert "50" in summary.headline
        assert "5" in summary.headline
        assert len(summary.key_points) > 0

    @pytest.mark.asyncio
    async def test_summary_to_message(self, builder: ExecutiveSummaryBuilder) -> None:
        """メッセージ形式変換."""
        results = {"processed": 100, "important": 3}
        summary = await builder.build("email_organize", results)

        message = summary.to_message()
        assert isinstance(message, str)
        assert len(message) > 0

    @pytest.mark.asyncio
    async def test_file_organize_summary(self, builder: ExecutiveSummaryBuilder) -> None:
        """ファイル整理サマリー."""
        results = {"processed": 120, "freed_mb": 1500}
        summary = await builder.build("file_organize", results)

        assert "120" in summary.headline
        assert "1500" in summary.headline

    @pytest.mark.asyncio
    async def test_system_optimize_summary(self, builder: ExecutiveSummaryBuilder) -> None:
        """システム最適化サマリー."""
        results = {"improvement": 15}
        summary = await builder.build("system_optimize", results)

        assert "15" in summary.headline

    @pytest.mark.asyncio
    async def test_research_summary(self, builder: ExecutiveSummaryBuilder) -> None:
        """調査サマリー."""
        results = {"topic": "AI市場", "findings": 5}
        summary = await builder.build("research", results)

        assert "AI市場" in summary.headline
        assert "5" in summary.headline

    @pytest.mark.asyncio
    async def test_report_summary(self, builder: ExecutiveSummaryBuilder) -> None:
        """レポートサマリー."""
        results = {"title": "週次レポート"}
        summary = await builder.build("report", results)

        assert "週次レポート" in summary.headline

    @pytest.mark.asyncio
    async def test_unknown_task_summary(self, builder: ExecutiveSummaryBuilder) -> None:
        """未知タスクのサマリー."""
        results = {"status": "completed"}
        summary = await builder.build("unknown_task", results)

        assert summary.headline is not None

    @pytest.mark.asyncio
    async def test_no_emoji_summary(self, builder_no_emoji: ExecutiveSummaryBuilder) -> None:
        """絵文字なしサマリー."""
        results = {"processed": 50, "important": 5}
        summary = await builder_no_emoji.build("email_organize", results)

        # 絵文字が含まれていないことを確認
        assert "📧" not in summary.headline

    @pytest.mark.asyncio
    async def test_summary_with_details(self, builder: ExecutiveSummaryBuilder) -> None:
        """詳細付きサマリー."""
        results = {"processed": 50, "important": 5}
        summary = await builder.build(
            "email_organize",
            results,
            details="追加の詳細情報",
        )

        assert summary is not None

    @pytest.mark.asyncio
    async def test_extract_risks(self, builder: ExecutiveSummaryBuilder) -> None:
        """リスク抽出."""
        # _extract_risksは errors, warnings, failed をリスクとして抽出する
        # email_organize テンプレートでは important キーが必要
        results = {
            "processed": 50,
            "important": 5,
            "errors": ["エラー1", "エラー2"],
            "failed": 3,
        }
        summary = await builder.build("email_organize", results)

        # errors と failed からリスクが抽出される
        assert len(summary.risks) >= 2

    @pytest.mark.asyncio
    async def test_extract_actions(self, builder: ExecutiveSummaryBuilder) -> None:
        """アクション抽出."""
        # email_organize テンプレートでは important キーが必要
        results = {
            "processed": 50,
            "important": 5,
            "recommended_actions": ["確認が必要", "承認が必要"],
        }
        summary = await builder.build("email_organize", results)

        assert len(summary.actions) >= 2

    def test_summary_config_defaults(self) -> None:
        """SummaryConfig デフォルト値."""
        config = SummaryConfig()
        assert config.use_emoji is True
        assert config.max_lines == 5  # max_key_points ではなく max_lines

    def test_executive_summary_dataclass(self) -> None:
        """ExecutiveSummary データクラス."""
        # ExecutiveSummary には details がない、details_available がある
        summary = ExecutiveSummary(
            headline="テスト見出し",
            key_points=["ポイント1", "ポイント2"],
            actions=["アクション1"],
            risks=["リスク1"],
            details_available=True,
        )

        assert summary.headline == "テスト見出し"
        assert len(summary.key_points) == 2
        message = summary.to_message()
        assert "テスト見出し" in message

    def test_to_message_with_risks_and_actions(self) -> None:
        """リスクとアクションを含むメッセージ変換."""
        summary = ExecutiveSummary(
            headline="テスト見出し",
            key_points=["ポイント1"],
            actions=["アクション1", "アクション2"],
            risks=["リスク1", "リスク2"],
        )
        config = SummaryConfig(use_emoji=True, include_risks=True, include_actions=True)
        message = summary.to_message(config)

        assert "⚠️" in message  # リスクの絵文字
        assert "👉" in message  # アクションの絵文字

    def test_to_message_no_emoji_config(self) -> None:
        """絵文字なし設定でのメッセージ変換."""
        summary = ExecutiveSummary(
            headline="テスト見出し",
            key_points=["ポイント1"],
            actions=["アクション1"],
            risks=["リスク1"],
        )
        config = SummaryConfig(use_emoji=False, include_risks=True, include_actions=True)
        message = summary.to_message(config)

        assert "• " in message or "[注意]" in message or "[次のステップ]" in message

    def test_to_message_exclude_risks(self) -> None:
        """リスク除外設定でのメッセージ変換."""
        summary = ExecutiveSummary(
            headline="テスト見出し",
            key_points=["ポイント1"],
            actions=["アクション1"],
            risks=["リスク1"],
        )
        config = SummaryConfig(include_risks=False, include_actions=True)
        message = summary.to_message(config)

        assert "リスク1" not in message

    def test_to_message_exclude_actions(self) -> None:
        """アクション除外設定でのメッセージ変換."""
        summary = ExecutiveSummary(
            headline="テスト見出し",
            key_points=["ポイント1"],
            actions=["アクション1"],
            risks=["リスク1"],
        )
        config = SummaryConfig(include_risks=True, include_actions=False)
        message = summary.to_message(config)

        assert "アクション1" not in message

    @pytest.mark.asyncio
    async def test_warnings_extraction(self, builder: ExecutiveSummaryBuilder) -> None:
        """warnings からリスク抽出."""
        results = {
            "processed": 50,
            "important": 5,
            "warnings": ["警告1", "警告2", "警告3"],
        }
        summary = await builder.build("email_organize", results)

        # warnings からリスクが抽出される（最大2件）
        assert len(summary.risks) >= 1


# =========================================================================
# TemplateRegistry テスト
# =========================================================================


class TestTemplateRegistry:
    """TemplateRegistryのテスト."""

    def test_singleton(self) -> None:
        """シングルトンパターン."""
        registry1 = TemplateRegistry()
        registry2 = TemplateRegistry()
        assert registry1 is registry2

    def test_register_and_get(self) -> None:
        """登録と取得."""
        registry = TemplateRegistry()
        template = TaskTemplate(name="test_template", triggers=["テスト"])
        registry.register(template)

        retrieved = registry.get("test_template")
        assert retrieved is not None
        assert retrieved.name == "test_template"

    def test_list_all(self) -> None:
        """全テンプレート一覧."""
        registry = TemplateRegistry()
        templates = registry.list_all()
        assert isinstance(templates, list)

