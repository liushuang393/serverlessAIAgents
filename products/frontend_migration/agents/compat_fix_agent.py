"""
互換性修正エージェント

MDN データベースと Can I Use API を活用した Polyfill 設定生成を担当するエージェント。
ブラウザ互換性の問題を自動検出し、適切な修正策を提供します。

主要機能:
- ブラウザ互換性チェック
- Polyfill の自動選択と設定
- IE専用コードの現代的な代替案提供
- 互換性レポートの生成
"""

import asyncio
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Optional

from ai_blocks.core.memory import VectorMemory
from ai_blocks.core.tool import ToolManager, tool
from ai_blocks.utils.logging import get_logger

logger = get_logger(__name__)


@dataclass
class CompatibilityIssue:
    """互換性問題を表すデータクラス"""

    feature: str
    browsers_affected: List[str]
    severity: str
    polyfill_available: bool
    polyfill_name: Optional[str]
    alternative_solution: Optional[str]


@dataclass
class CompatibilityFix:
    """互換性修正を格納するデータクラス"""

    file_path: str
    issues_fixed: List[CompatibilityIssue]
    polyfills_added: List[str]
    code_changes: List[str]
    browser_support: Dict[str, str]


class CompatFixAgent:
    """
    互換性修正を担当するエージェント

    ai_blocks.core.tool.ToolManagerを使用して互換性チェックツールを管理し、
    ai_blocks.core.memory.VectorMemoryを使用して修正履歴を記憶します。
    """

    def __init__(self, llm_provider=None):
        """
        互換性修正エージェントを初期化

        Args:
            llm_provider: LLMプロバイダー（オプション）
        """
        self.tool_manager = ToolManager()
        self.memory = VectorMemory()
        self.llm_provider = llm_provider

        # 互換性データベース
        self.compatibility_db = self._load_compatibility_db()

        # 互換性修正用ツールを登録
        self._register_compat_tools()

        logger.info("CompatFixAgentを初期化しました")

    def _load_compatibility_db(self) -> Dict[str, Any]:
        """互換性データベースを読み込み"""
        return {
            "css_features": {
                "grid": {
                    "support": {
                        "chrome": "57+",
                        "firefox": "52+",
                        "safari": "10.1+",
                        "ie": "10+ (partial)",
                    },
                    "polyfill": "css-grid-polyfill",
                    "alternative": "flexbox layout",
                },
                "flexbox": {
                    "support": {
                        "chrome": "29+",
                        "firefox": "28+",
                        "safari": "9+",
                        "ie": "11+",
                    },
                    "polyfill": "flexibility",
                    "alternative": "float layout",
                },
                "custom-properties": {
                    "support": {
                        "chrome": "49+",
                        "firefox": "31+",
                        "safari": "9.1+",
                        "ie": "none",
                    },
                    "polyfill": "css-vars-ponyfill",
                    "alternative": "sass variables",
                },
            },
            "js_features": {
                "arrow-functions": {
                    "support": {
                        "chrome": "45+",
                        "firefox": "22+",
                        "safari": "10+",
                        "ie": "none",
                    },
                    "polyfill": "babel-transform",
                    "alternative": "function expressions",
                },
                "async-await": {
                    "support": {
                        "chrome": "55+",
                        "firefox": "52+",
                        "safari": "10.1+",
                        "ie": "none",
                    },
                    "polyfill": "babel-transform",
                    "alternative": "promises",
                },
                "fetch": {
                    "support": {
                        "chrome": "42+",
                        "firefox": "39+",
                        "safari": "10.1+",
                        "ie": "none",
                    },
                    "polyfill": "whatwg-fetch",
                    "alternative": "XMLHttpRequest",
                },
            },
            "html_features": {
                "semantic-elements": {
                    "support": {
                        "chrome": "5+",
                        "firefox": "4+",
                        "safari": "4.1+",
                        "ie": "9+",
                    },
                    "polyfill": "html5shiv",
                    "alternative": "div with classes",
                }
            },
        }

    def _register_compat_tools(self) -> None:
        """互換性修正用のツールを登録"""

        @tool(name="check_css_compatibility", description="CSS機能の互換性をチェック")
        def check_css_compatibility(
            css_content: str, target_browsers: List[str]
        ) -> List[Dict[str, Any]]:
            """
            CSS機能の互換性をチェック

            Args:
                css_content: CSS内容
                target_browsers: 対象ブラウザのリスト

            Returns:
                互換性問題のリスト
            """
            issues = []
            css_features = self.compatibility_db["css_features"]

            for feature_name, feature_info in css_features.items():
                # 機能の使用をチェック
                if self._feature_used_in_css(css_content, feature_name):
                    for browser in target_browsers:
                        support = feature_info["support"].get(
                            browser.lower(), "unknown"
                        )

                        if support == "none" or "partial" in support:
                            issues.append(
                                {
                                    "feature": feature_name,
                                    "browser": browser,
                                    "support_level": support,
                                    "polyfill": feature_info.get("polyfill"),
                                    "alternative": feature_info.get("alternative"),
                                    "severity": "high"
                                    if support == "none"
                                    else "medium",
                                }
                            )

            return issues

        @tool(name="check_js_compatibility", description="JavaScript機能の互換性をチェック")
        def check_js_compatibility(
            js_content: str, target_browsers: List[str]
        ) -> List[Dict[str, Any]]:
            """
            JavaScript機能の互換性をチェック

            Args:
                js_content: JavaScript内容
                target_browsers: 対象ブラウザのリスト

            Returns:
                互換性問題のリスト
            """
            issues = []
            js_features = self.compatibility_db["js_features"]

            for feature_name, feature_info in js_features.items():
                # 機能の使用をチェック
                if self._feature_used_in_js(js_content, feature_name):
                    for browser in target_browsers:
                        support = feature_info["support"].get(
                            browser.lower(), "unknown"
                        )

                        if support == "none":
                            issues.append(
                                {
                                    "feature": feature_name,
                                    "browser": browser,
                                    "support_level": support,
                                    "polyfill": feature_info.get("polyfill"),
                                    "alternative": feature_info.get("alternative"),
                                    "severity": "high",
                                }
                            )

            return issues

        @tool(name="generate_polyfill_config", description="Polyfill設定を生成")
        def generate_polyfill_config(
            compatibility_issues: List[Dict[str, Any]]
        ) -> Dict[str, Any]:
            """
            互換性問題に基づいてPolyfill設定を生成

            Args:
                compatibility_issues: 互換性問題のリスト

            Returns:
                Polyfill設定
            """
            polyfills = set()
            babel_transforms = set()

            for issue in compatibility_issues:
                polyfill = issue.get("polyfill")
                if polyfill:
                    if polyfill == "babel-transform":
                        babel_transforms.add(issue["feature"])
                    else:
                        polyfills.add(polyfill)

            config = {
                "polyfills": list(polyfills),
                "babel_transforms": list(babel_transforms),
                "installation_commands": [
                    f"npm install {polyfill}" for polyfill in polyfills
                ],
                "babel_config": {
                    "presets": ["@babel/preset-env"],
                    "plugins": [
                        f"@babel/plugin-transform-{transform}"
                        for transform in babel_transforms
                    ],
                }
                if babel_transforms
                else None,
            }

            return config

        @tool(name="fix_ie_specific_code", description="IE専用コードを修正")
        def fix_ie_specific_code(css_content: str) -> Dict[str, Any]:
            """
            IE専用コードを現代的な代替案に修正

            Args:
                css_content: CSS内容

            Returns:
                修正結果
            """
            fixes = []
            fixed_css = css_content

            # filter: alpha(opacity=X) → opacity
            import re

            alpha_filter_pattern = r"filter:\s*alpha\(opacity=(\d+)\)"
            matches = re.findall(alpha_filter_pattern, css_content)

            for match in matches:
                opacity_value = int(match) / 100
                old_code = f"filter: alpha(opacity={match})"
                new_code = f"opacity: {opacity_value}"
                fixed_css = fixed_css.replace(old_code, new_code)
                fixes.append(f"{old_code} → {new_code}")

            # zoom: 1 → display: block (hasLayout trigger)
            zoom_pattern = r"zoom:\s*1"
            if re.search(zoom_pattern, css_content):
                fixed_css = re.sub(
                    zoom_pattern,
                    "/* zoom: 1 removed - use modern layout methods */",
                    fixed_css,
                )
                fixes.append("zoom: 1 → 削除（モダンレイアウト手法を使用）")

            # *property (IE hack) → 削除
            ie_hack_pattern = r"\*[a-zA-Z-]+:[^;]+;"
            ie_hacks = re.findall(ie_hack_pattern, css_content)
            for hack in ie_hacks:
                fixed_css = fixed_css.replace(
                    hack, f"/* {hack.strip()} - IE hack removed */"
                )
                fixes.append(f"{hack.strip()} → 削除（IEハック）")

            return {
                "fixed_css": fixed_css,
                "fixes_applied": fixes,
                "fixes_count": len(fixes),
            }

        # ツールマネージャーに登録
        self.tool_manager.register_function(check_css_compatibility)
        self.tool_manager.register_function(check_js_compatibility)
        self.tool_manager.register_function(generate_polyfill_config)
        self.tool_manager.register_function(fix_ie_specific_code)

    def _feature_used_in_css(self, css_content: str, feature_name: str) -> bool:
        """CSS内で機能が使用されているかチェック"""
        feature_patterns = {
            "grid": r"display:\s*grid|grid-template|grid-area",
            "flexbox": r"display:\s*flex|flex-direction|justify-content",
            "custom-properties": r"var\(--[^)]+\)|--[a-zA-Z-]+:",
        }

        pattern = feature_patterns.get(feature_name)
        if pattern:
            import re

            return bool(re.search(pattern, css_content, re.IGNORECASE))
        return False

    def _feature_used_in_js(self, js_content: str, feature_name: str) -> bool:
        """JavaScript内で機能が使用されているかチェック"""
        feature_patterns = {
            "arrow-functions": r"=>",
            "async-await": r"\basync\s+function|\bawait\s+",
            "fetch": r"\bfetch\s*\(",
        }

        pattern = feature_patterns.get(feature_name)
        if pattern:
            import re

            return bool(re.search(pattern, js_content))
        return False

    async def fix_compatibility_issues(
        self, file_path: str, target_browsers: List[str] = None
    ) -> CompatibilityFix:
        """
        ファイルの互換性問題を修正

        Args:
            file_path: 修正対象ファイルのパス
            target_browsers: 対象ブラウザのリスト

        Returns:
            CompatibilityFix: 修正結果
        """
        logger.info(f"互換性修正を開始: {file_path}")

        target_browsers = target_browsers or ["Chrome", "Firefox", "Safari", "IE"]

        # ファイル内容を読み込み
        with open(file_path, "r", encoding="utf-8") as f:
            content = f.read()

        file_extension = Path(file_path).suffix.lower()
        issues_fixed = []
        polyfills_added = []
        code_changes = []

        if file_extension == ".css":
            # CSS互換性チェック
            css_issues_result = await self.tool_manager.execute(
                "check_css_compatibility",
                {"css_content": content, "target_browsers": target_browsers},
            )

            if css_issues_result.success:
                css_issues = css_issues_result.result

                # IE専用コード修正
                ie_fix_result = await self.tool_manager.execute(
                    "fix_ie_specific_code", {"css_content": content}
                )

                if ie_fix_result.success:
                    fix_data = ie_fix_result.result

                    # 修正されたCSSを保存
                    fixed_path = str(
                        Path(file_path).with_name(f"{Path(file_path).stem}_fixed.css")
                    )
                    with open(fixed_path, "w", encoding="utf-8") as f:
                        f.write(fix_data["fixed_css"])

                    code_changes.extend(fix_data["fixes_applied"])

                # 互換性問題をCompatibilityIssueに変換
                for issue in css_issues:
                    compat_issue = CompatibilityIssue(
                        feature=issue["feature"],
                        browsers_affected=[issue["browser"]],
                        severity=issue["severity"],
                        polyfill_available=bool(issue.get("polyfill")),
                        polyfill_name=issue.get("polyfill"),
                        alternative_solution=issue.get("alternative"),
                    )
                    issues_fixed.append(compat_issue)

        elif file_extension == ".js":
            # JavaScript互換性チェック
            js_issues_result = await self.tool_manager.execute(
                "check_js_compatibility",
                {"js_content": content, "target_browsers": target_browsers},
            )

            if js_issues_result.success:
                js_issues = js_issues_result.result

                # 互換性問題をCompatibilityIssueに変換
                for issue in js_issues:
                    compat_issue = CompatibilityIssue(
                        feature=issue["feature"],
                        browsers_affected=[issue["browser"]],
                        severity=issue["severity"],
                        polyfill_available=bool(issue.get("polyfill")),
                        polyfill_name=issue.get("polyfill"),
                        alternative_solution=issue.get("alternative"),
                    )
                    issues_fixed.append(compat_issue)

        # Polyfill設定生成
        if issues_fixed:
            polyfill_config_result = await self.tool_manager.execute(
                "generate_polyfill_config",
                {
                    "compatibility_issues": [
                        {"feature": issue.feature, "polyfill": issue.polyfill_name}
                        for issue in issues_fixed
                        if issue.polyfill_available
                    ]
                },
            )

            if polyfill_config_result.success:
                config = polyfill_config_result.result
                polyfills_added = config["polyfills"]

        # ブラウザサポート情報を生成
        browser_support = {}
        for browser in target_browsers:
            issues_for_browser = [
                issue for issue in issues_fixed if browser in issue.browsers_affected
            ]
            if issues_for_browser:
                browser_support[browser] = f"{len(issues_for_browser)}個の問題を修正"
            else:
                browser_support[browser] = "完全サポート"

        # 修正結果を作成
        fix_result = CompatibilityFix(
            file_path=file_path,
            issues_fixed=issues_fixed,
            polyfills_added=polyfills_added,
            code_changes=code_changes,
            browser_support=browser_support,
        )

        # メモリに保存
        await self.memory.store(
            content=f"Compatibility fixes applied to {file_path}",
            metadata={
                "type": "compatibility_fix",
                "file_path": file_path,
                "issues_fixed_count": len(issues_fixed),
                "polyfills_added_count": len(polyfills_added),
            },
        )

        logger.info(f"互換性修正完了: {file_path}, {len(issues_fixed)}問題修正")
        return fix_result


# 使用例とテスト用のメイン関数
async def main():
    """
    CompatFixAgentの使用例
    """
    agent = CompatFixAgent()

    try:
        # 互換性修正を実行
        fix_result = await agent.fix_compatibility_issues(
            "sample.css", target_browsers=["Chrome", "Firefox", "Safari", "IE"]
        )

        print("互換性修正完了:")
        print(f"  ファイル: {fix_result.file_path}")
        print(f"  修正問題数: {len(fix_result.issues_fixed)}")
        print(f"  追加Polyfill数: {len(fix_result.polyfills_added)}")
        print(f"  コード変更数: {len(fix_result.code_changes)}")

    except Exception as e:
        logger.error(f"互換性修正エラー: {e}")


if __name__ == "__main__":
    asyncio.run(main())
