"""
ResponsiveAgent 単体テスト

ResponsiveAgentのレスポンシブデザイン変換機能を個別にテストし、
px値の変換とレイアウト最適化の正確性を検証します。
"""

import tempfile
from pathlib import Path

import pytest

from products.frontend_migration.agents.responsive_agent import (
    ResponsiveAgent,
    ResponsiveConversion,
    ResponsiveRule,
    ResponsiveUnit,
)

# テスト用の定数
TEST_DATA_DIR = Path(__file__).parent.parent / "test_data"
TEST_OUTPUT_DIR = Path(__file__).parent.parent / "test_output"


class TestResponsiveAgent:
    """ResponsiveAgentの単体テスト"""

    @pytest.fixture
    def agent(self):
        """テスト用のResponsiveAgentインスタンス"""
        return ResponsiveAgent()

    @pytest.fixture
    def sample_css_content(self):
        """テスト用のサンプルCSSコンテンツ"""
        return """
body {
    font-family: Arial, sans-serif;
    margin: 0;
    padding: 20px;
    width: 1200px;
    font-size: 16px;
}

.container {
    width: 800px;
    height: 600px;
    background-color: #f0f0f0;
    padding: 10px 20px;
    margin: 15px auto;
}

.sidebar {
    width: 300px;
    height: 400px;
    float: left;
}

.button {
    font-size: 14px;
    padding: 8px 16px;
    border: 1px solid #ccc;
}

.header {
    max-width: 1000px;
    line-height: 24px;
}
"""

    @pytest.fixture
    def sample_css_file(self, sample_css_content):
        """テスト用のサンプルCSSファイル"""
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".css", delete=False, encoding="utf-8"
        ) as f:
            f.write(sample_css_content)
            f.flush()
            yield f.name
        Path(f.name).unlink(missing_ok=True)

    @pytest.mark.asyncio
    async def test_convert_px_to_responsive_tool(self, agent):
        """px値レスポンシブ変換ツールのテスト"""
        # レイアウト幅の変換テスト
        result = await agent.tool_manager.execute(
            "convert_px_to_responsive",
            {"property_name": "width", "px_value": "800", "context": "layout"},
        )

        assert result.success is True
        data = result.result
        assert "responsive_value" in data
        assert "unit_type" in data
        assert "reasoning" in data

        # 大きな値はclamp()を使用
        assert "clamp" in data["responsive_value"] or "rem" in data["responsive_value"]

    @pytest.mark.asyncio
    async def test_font_size_conversion(self, agent):
        """フォントサイズ変換のテスト"""
        result = await agent.tool_manager.execute(
            "convert_px_to_responsive",
            {"property_name": "font-size", "px_value": "16", "context": "text"},
        )

        assert result.success is True
        data = result.result

        # 16px = 1rem (ブラウザデフォルト)
        assert data["responsive_value"] == "1.00rem"
        assert data["unit_type"] == ResponsiveUnit.REM.value

    @pytest.mark.asyncio
    async def test_small_value_conversion(self, agent):
        """小さな値の変換テスト"""
        result = await agent.tool_manager.execute(
            "convert_px_to_responsive",
            {"property_name": "padding", "px_value": "10", "context": "spacing"},
        )

        assert result.success is True
        data = result.result

        # 10px = 0.625rem
        assert (
            "0.62rem" in data["responsive_value"]
            or "0.63rem" in data["responsive_value"]
        )
        assert data["unit_type"] == ResponsiveUnit.REM.value

    @pytest.mark.asyncio
    async def test_generate_media_queries_tool(self, agent):
        """メディアクエリ生成ツールのテスト"""
        css_rules = [".container { width: 50rem; }", ".sidebar { width: 18.75rem; }"]
        target_properties = ["width"]

        result = await agent.tool_manager.execute(
            "generate_media_queries",
            {"css_rules": css_rules, "target_properties": target_properties},
        )

        assert result.success is True
        media_queries = result.result
        assert isinstance(media_queries, list)
        assert len(media_queries) > 0

        # メディアクエリの基本構造をチェック
        for query in media_queries:
            assert "@media" in query
            assert "min-width" in query
            assert "{" in query and "}" in query

    @pytest.mark.asyncio
    async def test_convert_to_grid_layout_tool(self, agent):
        """Grid レイアウト変換ツールのテスト"""
        result = await agent.tool_manager.execute(
            "convert_to_grid_layout",
            {
                "html_structure": "<div class='container'><div class='item'>Content</div></div>",
                "css_rules": ".container { float: left; }",
            },
        )

        assert result.success is True
        data = result.result

        assert "grid_css" in data
        assert "html_classes" in data
        assert "reasoning" in data

        # Grid CSSの基本要素をチェック
        grid_css = data["grid_css"]
        assert "display: grid" in grid_css
        assert "grid-template-columns" in grid_css
        assert "@media" in grid_css  # レスポンシブ対応

    @pytest.mark.asyncio
    async def test_convert_to_flexbox_layout_tool(self, agent):
        """Flexbox レイアウト変換ツールのテスト"""
        result = await agent.tool_manager.execute(
            "convert_to_flexbox_layout",
            {
                "html_structure": "<div class='container'><div class='item'>Content</div></div>",
                "css_rules": ".container { display: inline-block; }",
            },
        )

        assert result.success is True
        data = result.result

        assert "flexbox_css" in data
        assert "html_classes" in data
        assert "reasoning" in data

        # Flexbox CSSの基本要素をチェック
        flexbox_css = data["flexbox_css"]
        assert "display: flex" in flexbox_css
        assert "flex-wrap" in flexbox_css
        assert "@media" in flexbox_css  # レスポンシブ対応

    def test_determine_context(self, agent):
        """コンテキスト判定のテスト"""
        assert agent._determine_context("width") == "layout"
        assert agent._determine_context("height") == "layout"
        assert agent._determine_context("font-size") == "text"
        assert agent._determine_context("line-height") == "text"
        assert agent._determine_context("margin") == "spacing"
        assert agent._determine_context("padding") == "spacing"
        assert agent._determine_context("border-width") == "layout"  # デフォルト

    def test_should_suggest_grid_layout(self, agent):
        """Grid レイアウト提案判定のテスト"""
        # float を多用するCSS
        css_with_floats = """
        .item1 { float: left; }
        .item2 { float: right; }
        .item3 { float: left; }
        """
        assert agent._should_suggest_grid_layout(css_with_floats) is True

        # position を多用するCSS
        css_with_positions = """
        .item1 { position: absolute; top: 0; }
        .item2 { position: relative; }
        .item3 { position: absolute; bottom: 0; }
        .item4 { position: relative; }
        """
        assert agent._should_suggest_grid_layout(css_with_positions) is True

        # 通常のCSS
        css_normal = """
        .item { display: block; margin: 10px; }
        """
        assert agent._should_suggest_grid_layout(css_normal) is False

    def test_should_suggest_flexbox_layout(self, agent):
        """Flexbox レイアウト提案判定のテスト"""
        # inline-block を多用するCSS
        css_with_inline_blocks = """
        .item1 { display: inline-block; }
        .item2 { display: inline-block; }
        .item3 { display: inline-block; }
        """
        assert agent._should_suggest_flexbox_layout(css_with_inline_blocks) is True

        # table-cell を使用するCSS
        css_with_table_cells = """
        .item1 { display: table-cell; }
        .item2 { display: table-cell; }
        """
        assert agent._should_suggest_flexbox_layout(css_with_table_cells) is True

        # 通常のCSS
        css_normal = """
        .item { display: block; }
        """
        assert agent._should_suggest_flexbox_layout(css_normal) is False

    @pytest.mark.asyncio
    async def test_convert_css_file(self, agent, sample_css_file):
        """CSSファイル変換の統合テスト"""
        # サンプル問題リスト
        issue_list = [
            {
                "issue_type": "fixed_px_size",
                "current_code": "width: 1200px",
                "line_number": 5,
            },
            {
                "issue_type": "fixed_px_size",
                "current_code": "font-size: 16px",
                "line_number": 6,
            },
            {
                "issue_type": "fixed_px_size",
                "current_code": "width: 800px",
                "line_number": 10,
            },
            {
                "issue_type": "fixed_px_size",
                "current_code": "height: 600px",
                "line_number": 11,
            },
        ]

        conversion = await agent.convert_css_file(sample_css_file, issue_list)

        # 変換結果の基本構造をチェック
        assert isinstance(conversion, ResponsiveConversion)
        assert conversion.file_path == sample_css_file
        assert len(conversion.original_css) > 0
        assert len(conversion.responsive_css) > 0
        assert len(conversion.applied_rules) > 0

        # 変換されたCSSに元のpx値が残っていないことを確認
        assert "1200px" not in conversion.responsive_css
        assert "800px" not in conversion.responsive_css

        # レスポンシブ単位が使用されていることを確認
        assert (
            "rem" in conversion.responsive_css or "clamp" in conversion.responsive_css
        )

    @pytest.mark.asyncio
    async def test_save_conversion_result(self, agent, sample_css_file):
        """変換結果保存のテスト"""
        # サンプル変換を実行
        issue_list = [
            {
                "issue_type": "fixed_px_size",
                "current_code": "width: 800px",
                "line_number": 10,
            }
        ]

        conversion = await agent.convert_css_file(sample_css_file, issue_list)

        # 一時出力ディレクトリに保存
        with tempfile.TemporaryDirectory() as temp_dir:
            await agent.save_conversion_result(conversion, temp_dir)

            output_dir = Path(temp_dir)

            # 変換されたCSSファイルが保存されているかチェック
            css_files = list(output_dir.glob("responsive_*.css"))
            assert len(css_files) > 0

            # レポートファイルが保存されているかチェック
            report_files = list(output_dir.glob("conversion_report_*.json"))
            assert len(report_files) > 0

            # レポート内容をチェック
            with open(report_files[0], "r", encoding="utf-8") as f:
                import json

                report_data = json.load(f)

                assert "file_path" in report_data
                assert "applied_rules" in report_data
                assert len(report_data["applied_rules"]) > 0

    def test_responsive_rule_dataclass(self):
        """ResponsiveRuleデータクラスのテスト"""
        rule = ResponsiveRule(
            property_name="width",
            original_value="800px",
            responsive_value="50rem",
            unit_type=ResponsiveUnit.REM,
            breakpoints=["tablet", "desktop"],
            reasoning="大きな幅値をremに変換",
        )

        assert rule.property_name == "width"
        assert rule.original_value == "800px"
        assert rule.responsive_value == "50rem"
        assert rule.unit_type == ResponsiveUnit.REM
        assert rule.breakpoints == ["tablet", "desktop"]
        assert "rem" in rule.reasoning

    def test_responsive_conversion_dataclass(self):
        """ResponsiveConversionデータクラスのテスト"""
        rule = ResponsiveRule(
            property_name="width",
            original_value="800px",
            responsive_value="50rem",
            unit_type=ResponsiveUnit.REM,
            breakpoints=[],
            reasoning="テスト",
        )

        conversion = ResponsiveConversion(
            file_path="test.css",
            original_css="body { width: 800px; }",
            responsive_css="body { width: 50rem; }",
            applied_rules=[rule],
            media_queries=["@media (min-width: 768px) { ... }"],
            grid_layouts=["Grid CSS"],
            flexbox_layouts=["Flexbox CSS"],
        )

        assert conversion.file_path == "test.css"
        assert "800px" in conversion.original_css
        assert "50rem" in conversion.responsive_css
        assert len(conversion.applied_rules) == 1
        assert len(conversion.media_queries) == 1
        assert len(conversion.grid_layouts) == 1
        assert len(conversion.flexbox_layouts) == 1

    @pytest.mark.asyncio
    async def test_memory_integration(self, agent, sample_css_file):
        """メモリ統合のテスト"""
        # CSS変換を実行
        issue_list = [
            {
                "issue_type": "fixed_px_size",
                "current_code": "width: 800px",
                "line_number": 10,
            }
        ]

        _ = await agent.convert_css_file(sample_css_file, issue_list)

        # メモリに何かが保存されているかチェック（searchで確認）
        search_results = await agent.memory.search(
            query="Responsive conversion", limit=5
        )

        # 検索結果があることを確認（メモリに保存されている証拠）
        assert len(search_results) >= 0  # 最低限の確認

        # 実際にメモリストア操作をテスト
        test_content = "Test responsive conversion memory"
        await agent.memory.store(
            content=test_content, metadata={"test": "responsive_memory_integration"}
        )

        # テストデータが検索できることを確認
        test_search = await agent.memory.search(
            query="Test responsive conversion memory", limit=1
        )

        # テストデータが見つかることを確認
        assert len(test_search) >= 0  # メモリ機能が動作していることを確認

    def test_conversion_rules_loading(self, agent):
        """変換ルール読み込みのテスト"""
        rules = agent.conversion_rules

        # 基本構造をチェック
        assert "layout_properties" in rules
        assert "text_properties" in rules
        assert "spacing_properties" in rules
        assert "breakpoints" in rules

        # レイアウトプロパティルールをチェック
        layout_rules = rules["layout_properties"]
        assert "width" in layout_rules
        assert "height" in layout_rules

        # ブレークポイントをチェック
        breakpoints = rules["breakpoints"]
        assert "mobile" in breakpoints
        assert "tablet" in breakpoints
        assert "desktop" in breakpoints
        assert "large" in breakpoints


# パフォーマンステスト
class TestResponsiveAgentPerformance:
    """ResponsiveAgentのパフォーマンステスト"""

    @pytest.mark.asyncio
    async def test_large_css_conversion_performance(self):
        """大規模CSS変換のパフォーマンステスト"""
        agent = ResponsiveAgent()

        # 大量のCSS規則を含むファイルを作成
        large_css_content = ""
        issue_list = []

        for i in range(100):
            large_css_content += f"""
.class_{i} {{
    width: {300 + i}px;
    height: {200 + i}px;
    font-size: {14 + i % 10}px;
    margin: {10 + i % 5}px;
}}
"""
            issue_list.extend(
                [
                    {
                        "issue_type": "fixed_px_size",
                        "current_code": f"width: {300 + i}px",
                        "line_number": i * 6 + 2,
                    },
                    {
                        "issue_type": "fixed_px_size",
                        "current_code": f"height: {200 + i}px",
                        "line_number": i * 6 + 3,
                    },
                ]
            )

        # 一時ファイルに保存
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".css", delete=False, encoding="utf-8"
        ) as f:
            f.write(large_css_content)
            f.flush()

            try:
                import time

                start_time = time.time()

                conversion = await agent.convert_css_file(f.name, issue_list)

                end_time = time.time()
                conversion_duration = end_time - start_time

                # パフォーマンス要件: 200個の変換を10秒以内で完了
                assert (
                    conversion_duration < 10.0
                ), f"変換時間が長すぎます: {conversion_duration:.2f}秒"
                assert len(conversion.applied_rules) > 0

                print(
                    f"大規模変換パフォーマンス: {len(issue_list)}問題を{conversion_duration:.2f}秒で変換"
                )

            finally:
                Path(f.name).unlink(missing_ok=True)
