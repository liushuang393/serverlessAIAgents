"""
レスポンシブエージェント

@media / container queries、Flex/Grid レイアウトの生成を担当するエージェント。
固定pxサイズをレスポンシブ単位に変換し、モダンなレイアウトシステムを適用します。

主要機能:
- 固定pxサイズの自動変換（rem, em, %, vw, vh, clamp()）
- CSS Grid / Flexbox レイアウトの生成
- メディアクエリの自動生成
- コンテナクエリの適用
- レスポンシブ画像の最適化
"""

import asyncio
import json
import re
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Any, Dict, List

from ai_blocks.architectures.prompt_chaining import PromptChain, SpecializedAgent
from ai_blocks.core.memory import VectorMemory
from ai_blocks.core.tool import ToolManager, tool
from ai_blocks.utils.logging import get_logger

logger = get_logger(__name__)


class ResponsiveUnit(Enum):
    """レスポンシブ単位の列挙"""

    REM = "rem"
    EM = "em"
    PERCENT = "%"
    VW = "vw"
    VH = "vh"
    VMIN = "vmin"
    VMAX = "vmax"
    CLAMP = "clamp"


@dataclass
class ResponsiveRule:
    """レスポンシブ変換ルールを表すデータクラス"""

    property_name: str
    original_value: str
    responsive_value: str
    unit_type: ResponsiveUnit
    breakpoints: List[str]
    reasoning: str


@dataclass
class ResponsiveConversion:
    """レスポンシブ変換結果を格納するデータクラス"""

    file_path: str
    original_css: str
    responsive_css: str
    applied_rules: List[ResponsiveRule]
    media_queries: List[str]
    grid_layouts: List[str]
    flexbox_layouts: List[str]


class ResponsiveAgent:
    """
    レスポンシブデザイン変換を担当するエージェント

    ai_blocks.architectures.prompt_chaining.PromptChainを使用して
    段階的な変換処理を実行し、ai_blocks.core.memory.VectorMemoryを
    使用して変換パターンを学習・記憶します。
    """

    def __init__(self, llm_provider=None):
        """
        レスポンシブエージェントを初期化

        Args:
            llm_provider: LLMプロバイダー（オプション）
        """
        self.tool_manager = ToolManager()
        self.memory = VectorMemory()
        self.llm_provider = llm_provider

        # 変換ルールを定義
        self.conversion_rules = self._load_conversion_rules()

        # レスポンシブ変換用ツールを登録
        self._register_responsive_tools()

        # プロンプトチェーンを設定
        self.conversion_chain = self._setup_conversion_chain()

        logger.info("ResponsiveAgentを初期化しました")

    def _load_conversion_rules(self) -> Dict[str, Any]:
        """
        レスポンシブ変換ルールを読み込み

        Returns:
            変換ルールの辞書
        """
        return {
            # レイアウト関連プロパティ
            "layout_properties": {
                "width": {
                    "small_values": {"max_px": 100, "unit": ResponsiveUnit.REM},
                    "medium_values": {"max_px": 500, "unit": ResponsiveUnit.PERCENT},
                    "large_values": {"max_px": 1200, "unit": ResponsiveUnit.CLAMP},
                },
                "height": {
                    "small_values": {"max_px": 100, "unit": ResponsiveUnit.REM},
                    "medium_values": {"max_px": 500, "unit": ResponsiveUnit.VH},
                    "large_values": {"max_px": 800, "unit": ResponsiveUnit.CLAMP},
                },
                "max-width": {"all_values": {"unit": ResponsiveUnit.REM}},
                "max-height": {"all_values": {"unit": ResponsiveUnit.REM}},
            },
            # テキスト関連プロパティ
            "text_properties": {
                "font-size": {"all_values": {"unit": ResponsiveUnit.REM}},
                "line-height": {"all_values": {"unit": ResponsiveUnit.EM}},
                "letter-spacing": {"all_values": {"unit": ResponsiveUnit.EM}},
            },
            # スペーシング関連プロパティ
            "spacing_properties": {
                "margin": {"all_values": {"unit": ResponsiveUnit.REM}},
                "padding": {"all_values": {"unit": ResponsiveUnit.REM}},
                "gap": {"all_values": {"unit": ResponsiveUnit.REM}},
            },
            # ブレークポイント定義
            "breakpoints": {
                "mobile": "320px",
                "tablet": "768px",
                "desktop": "1024px",
                "large": "1440px",
            },
        }

    def _register_responsive_tools(self) -> None:
        """レスポンシブ変換用のツールを登録"""

        @tool(name="convert_px_to_responsive", description="px値をレスポンシブ単位に変換")
        def convert_px_to_responsive(
            property_name: str, px_value: str, context: str = "layout"
        ) -> Dict[str, Any]:
            """
            px値をレスポンシブ単位に変換

            Args:
                property_name: CSSプロパティ名
                px_value: px値（数値部分のみ）
                context: コンテキスト（layout, text, spacing）

            Returns:
                変換結果の辞書
            """
            px_val = int(px_value)
            base_font_size = 16  # ブラウザのデフォルトフォントサイズ

            # プロパティタイプに応じた変換ルールを取得
            rules = self.conversion_rules.get(f"{context}_properties", {})
            property_rules = rules.get(property_name, {})

            if not property_rules:
                # デフォルトルール: remに変換
                return {
                    "responsive_value": f"{px_val / base_font_size:.2f}rem",
                    "unit_type": ResponsiveUnit.REM.value,
                    "reasoning": f"デフォルトルール適用: {px_val}px → rem変換",
                }

            # 値の大きさに応じて適切な単位を選択
            if (
                "small_values" in property_rules
                and px_val <= property_rules["small_values"]["max_px"]
            ):
                unit = property_rules["small_values"]["unit"]
            elif (
                "medium_values" in property_rules
                and px_val <= property_rules["medium_values"]["max_px"]
            ):
                unit = property_rules["medium_values"]["unit"]
            elif "large_values" in property_rules:
                unit = property_rules["large_values"]["unit"]
            else:
                unit = property_rules.get("all_values", {}).get(
                    "unit", ResponsiveUnit.REM
                )

            # 単位に応じた変換を実行
            if unit == ResponsiveUnit.REM:
                responsive_value = f"{px_val / base_font_size:.2f}rem"
            elif unit == ResponsiveUnit.EM:
                responsive_value = f"{px_val / base_font_size:.2f}em"
            elif unit == ResponsiveUnit.PERCENT:
                # パーセント変換は親要素のサイズに依存するため、推定値を使用
                responsive_value = f"{min(px_val / 12, 100):.1f}%"
            elif unit == ResponsiveUnit.VW:
                # ビューポート幅の推定値（1440pxを100vwと仮定）
                responsive_value = f"{px_val / 14.4:.1f}vw"
            elif unit == ResponsiveUnit.VH:
                # ビューポート高さの推定値（900pxを100vhと仮定）
                responsive_value = f"{px_val / 9:.1f}vh"
            elif unit == ResponsiveUnit.CLAMP:
                # clamp()関数を使用した流動的なサイズ
                min_val = px_val * 0.8 / base_font_size
                max_val = px_val * 1.2 / base_font_size
                preferred_val = px_val / 14.4  # vw単位
                responsive_value = (
                    f"clamp({min_val:.1f}rem, {preferred_val:.1f}vw, {max_val:.1f}rem)"
                )
            else:
                responsive_value = f"{px_val / base_font_size:.2f}rem"

            return {
                "responsive_value": responsive_value,
                "unit_type": unit.value,
                "reasoning": f"{property_name}の{px_val}pxを{unit.value}単位に変換",
            }

        @tool(name="generate_media_queries", description="レスポンシブデザイン用のメディアクエリを生成")
        def generate_media_queries(
            css_rules: List[str], target_properties: List[str]
        ) -> List[str]:
            """
            レスポンシブデザイン用のメディアクエリを生成

            Args:
                css_rules: CSSルールのリスト
                target_properties: 対象プロパティのリスト

            Returns:
                生成されたメディアクエリのリスト
            """
            media_queries = []
            breakpoints = self.conversion_rules["breakpoints"]

            # モバイルファーストのメディアクエリを生成
            for breakpoint_name, breakpoint_value in breakpoints.items():
                if breakpoint_name == "mobile":
                    continue  # モバイルはベーススタイル

                media_query = f"@media (min-width: {breakpoint_value}) {{\n"

                # 各CSSルールに対してブレークポイント固有のスタイルを生成
                for rule in css_rules:
                    if any(prop in rule for prop in target_properties):
                        # ブレークポイントに応じたスタイル調整
                        adjusted_rule = self._adjust_rule_for_breakpoint(
                            rule, breakpoint_name
                        )
                        media_query += f"  {adjusted_rule}\n"

                media_query += "}\n"
                media_queries.append(media_query)

            logger.info(f"メディアクエリ生成完了: {len(media_queries)}個")
            return media_queries

        @tool(name="convert_to_grid_layout", description="従来のレイアウトをCSS Gridに変換")
        def convert_to_grid_layout(
            html_structure: str, css_rules: str
        ) -> Dict[str, str]:
            """
            従来のレイアウトをCSS Gridに変換

            Args:
                html_structure: HTML構造
                css_rules: 既存のCSSルール

            Returns:
                Grid変換結果
            """
            # 簡単なGrid変換例（実際の実装ではより複雑な解析が必要）
            grid_css = """
.container {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  gap: 1rem;
  padding: 1rem;
}

.grid-item {
  background: #f5f5f5;
  padding: 1rem;
  border-radius: 0.5rem;
}

@media (min-width: 768px) {
  .container {
    grid-template-columns: repeat(2, 1fr);
    gap: 2rem;
  }
}

@media (min-width: 1024px) {
  .container {
    grid-template-columns: repeat(3, 1fr);
  }
}
"""

            return {
                "grid_css": grid_css,
                "html_classes": "container grid-item",
                "reasoning": "従来のfloatレイアウトをCSS Gridに変換",
            }

        @tool(name="convert_to_flexbox_layout", description="従来のレイアウトをFlexboxに変換")
        def convert_to_flexbox_layout(
            html_structure: str, css_rules: str
        ) -> Dict[str, str]:
            """
            従来のレイアウトをFlexboxに変換

            Args:
                html_structure: HTML構造
                css_rules: 既存のCSSルール

            Returns:
                Flexbox変換結果
            """
            flexbox_css = """
.flex-container {
  display: flex;
  flex-wrap: wrap;
  gap: 1rem;
  padding: 1rem;
}

.flex-item {
  flex: 1 1 300px;
  min-width: 0;
  background: #f5f5f5;
  padding: 1rem;
  border-radius: 0.5rem;
}

@media (max-width: 767px) {
  .flex-container {
    flex-direction: column;
  }

  .flex-item {
    flex: 1 1 auto;
  }
}
"""

            return {
                "flexbox_css": flexbox_css,
                "html_classes": "flex-container flex-item",
                "reasoning": "従来のinline-blockレイアウトをFlexboxに変換",
            }

        # ツールマネージャーに登録
        self.tool_manager.register_function(convert_px_to_responsive)
        self.tool_manager.register_function(generate_media_queries)
        self.tool_manager.register_function(convert_to_grid_layout)
        self.tool_manager.register_function(convert_to_flexbox_layout)

    def _adjust_rule_for_breakpoint(self, rule: str, breakpoint: str) -> str:
        """
        ブレークポイントに応じてCSSルールを調整

        Args:
            rule: 元のCSSルール
            breakpoint: ブレークポイント名

        Returns:
            調整されたCSSルール
        """
        # ブレークポイント別の調整係数
        adjustments = {"tablet": 1.1, "desktop": 1.2, "large": 1.3}

        adjustment_factor = adjustments.get(breakpoint, 1.0)

        # rem値を調整（簡単な例）
        def adjust_rem_value(match):
            value = float(match.group(1))
            adjusted_value = value * adjustment_factor
            return f"{adjusted_value:.2f}rem"

        adjusted_rule = re.sub(r"(\d+\.?\d*)rem", adjust_rem_value, rule)
        return adjusted_rule

    def _setup_conversion_chain(self) -> PromptChain:
        """
        レスポンシブ変換用のプロンプトチェーンを設定

        Returns:
            設定されたPromptChain
        """
        # 段階的変換エージェントを定義
        analyzer_agent = SpecializedAgent(
            name="CSSAnalyzer",
            task_description="""
            CSSコード解析の専門家として、与えられたCSSコードを解析し、レスポンシブ化が必要な要素を特定してください。

            特に以下の点に注目してください：
            - 固定pxサイズの使用箇所
            - レイアウト構造（float, position等）
            - メディアクエリの有無
            - アクセシビリティの問題
            """,
            llm_provider=self.llm_provider,
        )

        converter_agent = SpecializedAgent(
            name="ResponsiveConverter",
            task_description="""
            レスポンシブデザイン変換の専門家として、解析されたCSSコードをモダンなレスポンシブデザインに変換してください。

            変換時の原則：
            - モバイルファーストアプローチ
            - 適切な単位の使用（rem, em, %, vw, vh, clamp）
            - CSS Grid / Flexbox の活用
            - アクセシビリティの向上
            """,
            llm_provider=self.llm_provider,
        )

        validator_agent = SpecializedAgent(
            name="ResponsiveValidator",
            task_description="""
            レスポンシブデザイン検証の専門家として、変換されたCSSコードを検証し、問題があれば修正案を提示してください。

            検証項目：
            - ブラウザ互換性
            - パフォーマンス影響
            - アクセシビリティ準拠
            - 実装の実用性
            """,
            llm_provider=self.llm_provider,
        )

        return PromptChain(
            agents=[analyzer_agent, converter_agent, validator_agent],
            name="ResponsiveConversionChain",
        )

    async def convert_css_file(
        self, file_path: str, issue_list: List[Dict[str, Any]]
    ) -> ResponsiveConversion:
        """
        CSSファイルをレスポンシブデザインに変換

        Args:
            file_path: 変換対象のCSSファイルパス
            issue_list: 検出された問題のリスト

        Returns:
            ResponsiveConversion: 変換結果
        """
        logger.info(f"CSSファイルのレスポンシブ変換を開始: {file_path}")

        # ファイル内容を読み込み
        with open(file_path, "r", encoding="utf-8") as f:
            original_css = f.read()

        applied_rules = []
        responsive_css = original_css
        media_queries = []
        grid_layouts = []
        flexbox_layouts = []

        # 問題リストから固定pxサイズの問題を抽出
        px_issues = [
            issue for issue in issue_list if issue.get("issue_type") == "fixed_px_size"
        ]

        # 各px問題を順次変換
        for issue in px_issues:
            current_code = issue["current_code"]

            # CSSプロパティと値を抽出
            match = re.search(r"(\w+):\s*(\d+)px", current_code)
            if match:
                property_name = match.group(1)
                px_value = match.group(2)

                # コンテキストを判定
                context = self._determine_context(property_name)

                # レスポンシブ変換を実行
                conversion_result = await self.tool_manager.execute(
                    "convert_px_to_responsive",
                    {
                        "property_name": property_name,
                        "px_value": px_value,
                        "context": context,
                    },
                )

                if conversion_result.success:
                    result_data = conversion_result.result
                    responsive_value = result_data["responsive_value"]

                    # CSSを置換
                    old_declaration = f"{property_name}: {px_value}px"
                    new_declaration = f"{property_name}: {responsive_value}"
                    responsive_css = responsive_css.replace(
                        old_declaration, new_declaration
                    )

                    # 適用ルールを記録
                    rule = ResponsiveRule(
                        property_name=property_name,
                        original_value=f"{px_value}px",
                        responsive_value=responsive_value,
                        unit_type=ResponsiveUnit(result_data["unit_type"]),
                        breakpoints=[],
                        reasoning=result_data["reasoning"],
                    )
                    applied_rules.append(rule)

        # メディアクエリを生成
        target_properties = [rule.property_name for rule in applied_rules]
        if target_properties:
            media_query_result = await self.tool_manager.execute(
                "generate_media_queries",
                {"css_rules": [responsive_css], "target_properties": target_properties},
            )

            if media_query_result.success:
                media_queries = media_query_result.result
                responsive_css += "\n\n" + "\n".join(media_queries)

        # Grid/Flexboxレイアウトの提案
        if self._should_suggest_grid_layout(original_css):
            grid_result = await self.tool_manager.execute(
                "convert_to_grid_layout",
                {
                    "html_structure": "",  # 実際の実装では対応するHTMLを渡す
                    "css_rules": responsive_css,
                },
            )

            if grid_result.success:
                grid_layouts.append(grid_result.result["grid_css"])

        if self._should_suggest_flexbox_layout(original_css):
            flexbox_result = await self.tool_manager.execute(
                "convert_to_flexbox_layout",
                {
                    "html_structure": "",  # 実際の実装では対応するHTMLを渡す
                    "css_rules": responsive_css,
                },
            )

            if flexbox_result.success:
                flexbox_layouts.append(flexbox_result.result["flexbox_css"])

        # 変換結果を作成
        conversion = ResponsiveConversion(
            file_path=file_path,
            original_css=original_css,
            responsive_css=responsive_css,
            applied_rules=applied_rules,
            media_queries=media_queries,
            grid_layouts=grid_layouts,
            flexbox_layouts=flexbox_layouts,
        )

        # メモリに保存
        await self.memory.store(
            content=f"Responsive conversion for {file_path}: {len(applied_rules)} rules applied",
            metadata={
                "type": "responsive_conversion",
                "file_path": file_path,
                "rules_applied": len(applied_rules),
                "media_queries_added": len(media_queries),
            },
        )

        logger.info(f"レスポンシブ変換完了: {file_path}, {len(applied_rules)}ルール適用")
        return conversion

    def _determine_context(self, property_name: str) -> str:
        """
        CSSプロパティのコンテキストを判定

        Args:
            property_name: CSSプロパティ名

        Returns:
            コンテキスト文字列
        """
        layout_props = [
            "width",
            "height",
            "max-width",
            "max-height",
            "min-width",
            "min-height",
        ]
        text_props = ["font-size", "line-height", "letter-spacing"]
        spacing_props = ["margin", "padding", "gap", "top", "left", "right", "bottom"]

        if property_name in layout_props:
            return "layout"
        elif property_name in text_props:
            return "text"
        elif property_name in spacing_props:
            return "spacing"
        else:
            return "layout"  # デフォルト

    def _should_suggest_grid_layout(self, css_content: str) -> bool:
        """Grid レイアウトを提案すべきかを判定"""
        # float や position を多用している場合はGrid を提案
        float_count = css_content.count("float:")
        position_count = css_content.count("position: absolute") + css_content.count(
            "position: relative"
        )

        return float_count > 2 or position_count > 3

    def _should_suggest_flexbox_layout(self, css_content: str) -> bool:
        """Flexbox レイアウトを提案すべきかを判定"""
        # inline-block や table-cell を多用している場合はFlexbox を提案
        inline_block_count = css_content.count("display: inline-block")
        table_cell_count = css_content.count("display: table-cell")

        return inline_block_count > 2 or table_cell_count > 1

    async def save_conversion_result(
        self, conversion: ResponsiveConversion, output_dir: str
    ) -> None:
        """
        変換結果を保存

        Args:
            conversion: 変換結果
            output_dir: 出力ディレクトリ
        """
        output_path = Path(output_dir)
        output_path.mkdir(parents=True, exist_ok=True)

        # 変換されたCSSファイルを保存
        css_file_path = output_path / f"responsive_{Path(conversion.file_path).name}"
        with open(css_file_path, "w", encoding="utf-8") as f:
            f.write(conversion.responsive_css)

        # 変換レポートを保存
        report = {
            "file_path": conversion.file_path,
            "applied_rules": [
                {
                    "property_name": rule.property_name,
                    "original_value": rule.original_value,
                    "responsive_value": rule.responsive_value,
                    "unit_type": rule.unit_type.value,
                    "reasoning": rule.reasoning,
                }
                for rule in conversion.applied_rules
            ],
            "media_queries_count": len(conversion.media_queries),
            "grid_layouts_count": len(conversion.grid_layouts),
            "flexbox_layouts_count": len(conversion.flexbox_layouts),
        }

        report_path = (
            output_path / f"conversion_report_{Path(conversion.file_path).stem}.json"
        )
        with open(report_path, "w", encoding="utf-8") as f:
            json.dump(report, f, indent=2, ensure_ascii=False)

        logger.info(f"変換結果を保存しました: {css_file_path}, {report_path}")


# 使用例とテスト用のメイン関数
async def main():
    """
    ResponsiveAgentの使用例
    """
    agent = ResponsiveAgent()

    # サンプルCSSファイルを変換
    try:
        # 問題リストの例
        sample_issues = [
            {
                "issue_type": "fixed_px_size",
                "current_code": "width: 300px",
                "line_number": 10,
            },
            {
                "issue_type": "fixed_px_size",
                "current_code": "font-size: 16px",
                "line_number": 15,
            },
        ]

        conversion = await agent.convert_css_file("sample.css", sample_issues)
        await agent.save_conversion_result(conversion, "output/")

        print("レスポンシブ変換完了:")
        print(f"  適用ルール数: {len(conversion.applied_rules)}")
        print(f"  メディアクエリ数: {len(conversion.media_queries)}")
        print(f"  Grid レイアウト提案: {len(conversion.grid_layouts)}")
        print(f"  Flexbox レイアウト提案: {len(conversion.flexbox_layouts)}")

    except Exception as e:
        logger.error(f"変換エラー: {e}")


if __name__ == "__main__":
    asyncio.run(main())
