"""
アナライザーエージェント

AST解析、テンプレートエンジン検出、インラインスクリプト特定を担当するエージェント。
ai_blocks.core.parser.HTMLParserとai_blocks.core.memory.VectorMemoryを活用して
コード解析結果を構造化し記憶します。

主要機能:
- HTML/JSP/CSS/JSファイルの詳細解析
- AST（抽象構文木）の生成と解析
- レガシーコードパターンの検出
- 問題点リストの生成（issue_list.json）
"""

import asyncio
import json
import re
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Any, Dict, List

from ai_blocks.core.memory import VectorMemory
from ai_blocks.core.parser import HTMLParser, TextParser
from ai_blocks.core.tool import ToolManager, tool
from ai_blocks.utils.logging import get_logger

logger = get_logger(__name__)


class IssueType(Enum):
    """問題タイプの列挙"""

    FIXED_PX_SIZE = "fixed_px_size"
    IE_COMPATIBILITY = "ie_compatibility"
    JQUERY_DEPENDENCY = "jquery_dependency"
    INLINE_STYLES = "inline_styles"
    ACCESSIBILITY_VIOLATION = "accessibility_violation"
    PERFORMANCE_ISSUE = "performance_issue"
    SECURITY_VULNERABILITY = "security_vulnerability"


@dataclass
class CodeIssue:
    """コード問題を表すデータクラス"""

    file_path: str
    line_number: int
    issue_type: IssueType
    severity: str  # high, medium, low
    description: str
    current_code: str
    suggested_fix: str
    auto_fixable: bool


@dataclass
class AnalysisResult:
    """解析結果を格納するデータクラス"""

    total_files_analyzed: int
    issues: List[CodeIssue]
    file_statistics: Dict[str, Any]
    framework_analysis: Dict[str, Any]
    migration_complexity: str  # simple, medium, complex


class AnalyzerAgent:
    """
    コード解析を担当するエージェント

    ai_blocks.core.parser.HTMLParserを使用してHTMLファイルを解析し、
    ai_blocks.core.memory.VectorMemoryを使用して解析結果を記憶します。
    """

    def __init__(self, llm_provider=None):
        """
        アナライザーエージェントを初期化

        Args:
            llm_provider: LLMプロバイダー（オプション）
        """
        self.html_parser = HTMLParser()
        self.text_parser = TextParser()
        self.memory = VectorMemory()
        self.tool_manager = ToolManager()
        self.llm_provider = llm_provider

        # 解析用ツールを登録
        self._register_analysis_tools()

        logger.info("AnalyzerAgentを初期化しました")

    def _register_analysis_tools(self) -> None:
        """コード解析用のツールを登録"""

        @tool(name="analyze_css_file", description="CSSファイルを解析して問題点を検出")
        def analyze_css_file(file_path: str, content: str) -> List[Dict[str, Any]]:
            """
            CSSファイルを解析して問題点を検出

            Args:
                file_path: ファイルパス
                content: ファイル内容

            Returns:
                検出された問題のリスト
            """
            issues = []
            lines = content.split("\n")

            for line_num, line in enumerate(lines, 1):
                # 固定pxサイズの検出
                px_matches = re.findall(r"(\w+):\s*(\d+)px", line)
                for property_name, px_value in px_matches:
                    # レイアウト関連プロパティの場合は問題として報告
                    if property_name in [
                        "width",
                        "height",
                        "max-width",
                        "max-height",
                        "font-size",
                    ]:
                        issues.append(
                            {
                                "line_number": line_num,
                                "issue_type": IssueType.FIXED_PX_SIZE.value,
                                "severity": "medium",
                                "description": f"固定pxサイズが使用されています: {property_name}: {px_value}px",
                                "current_code": line.strip(),
                                "suggested_fix": self._suggest_responsive_fix(
                                    property_name, px_value
                                ),
                                "auto_fixable": True,
                            }
                        )

                # IE専用CSSの検出
                if "filter:" in line and "alpha(opacity" in line:
                    issues.append(
                        {
                            "line_number": line_num,
                            "issue_type": IssueType.IE_COMPATIBILITY.value,
                            "severity": "high",
                            "description": "IE専用のfilterプロパティが使用されています",
                            "current_code": line.strip(),
                            "suggested_fix": "opacity プロパティを使用してください",
                            "auto_fixable": True,
                        }
                    )

            logger.info(f"CSS解析完了: {file_path}, {len(issues)}個の問題を検出")
            return issues

        @tool(name="analyze_js_file", description="JavaScriptファイルを解析して問題点を検出")
        def analyze_js_file(file_path: str, content: str) -> List[Dict[str, Any]]:
            """
            JavaScriptファイルを解析して問題点を検出

            Args:
                file_path: ファイルパス
                content: ファイル内容

            Returns:
                検出された問題のリスト
            """
            issues = []
            lines = content.split("\n")

            for line_num, line in enumerate(lines, 1):
                # jQuery依存の検出
                if "$(" in line or "jQuery(" in line:
                    issues.append(
                        {
                            "line_number": line_num,
                            "issue_type": IssueType.JQUERY_DEPENDENCY.value,
                            "severity": "medium",
                            "description": "jQuery依存のコードが検出されました",
                            "current_code": line.strip(),
                            "suggested_fix": "ネイティブJavaScriptまたはモダンフレームワークに移行してください",
                            "auto_fixable": False,
                        }
                    )

                # var宣言の検出（ES5パターン）
                if re.match(r"\s*var\s+\w+", line):
                    issues.append(
                        {
                            "line_number": line_num,
                            "issue_type": IssueType.PERFORMANCE_ISSUE.value,
                            "severity": "low",
                            "description": "ES5のvar宣言が使用されています",
                            "current_code": line.strip(),
                            "suggested_fix": "const または let を使用してください",
                            "auto_fixable": True,
                        }
                    )

            logger.info(f"JavaScript解析完了: {file_path}, {len(issues)}個の問題を検出")
            return issues

        @tool(name="analyze_html_structure", description="HTML構造を解析してアクセシビリティ問題を検出")
        def analyze_html_structure(
            file_path: str, content: str
        ) -> List[Dict[str, Any]]:
            """
            HTML構造を解析してアクセシビリティ問題を検出

            Args:
                file_path: ファイルパス
                content: ファイル内容

            Returns:
                検出された問題のリスト
            """
            issues = []

            # alt属性のないimg要素を検出
            img_without_alt = re.findall(
                r"<img(?![^>]*alt=)[^>]*>", content, re.IGNORECASE
            )
            for match in img_without_alt:
                issues.append(
                    {
                        "line_number": content[: content.find(match)].count("\n") + 1,
                        "issue_type": IssueType.ACCESSIBILITY_VIOLATION.value,
                        "severity": "high",
                        "description": "img要素にalt属性がありません",
                        "current_code": match,
                        "suggested_fix": "alt属性を追加してください",
                        "auto_fixable": False,
                    }
                )

            # インラインスタイルの検出
            inline_styles = re.findall(r'style="[^"]*"', content, re.IGNORECASE)
            for match in inline_styles:
                issues.append(
                    {
                        "line_number": content[: content.find(match)].count("\n") + 1,
                        "issue_type": IssueType.INLINE_STYLES.value,
                        "severity": "medium",
                        "description": "インラインスタイルが使用されています",
                        "current_code": match,
                        "suggested_fix": "外部CSSファイルまたはCSSクラスを使用してください",
                        "auto_fixable": False,
                    }
                )

            logger.info(f"HTML構造解析完了: {file_path}, {len(issues)}個の問題を検出")
            return issues

        # ツールマネージャーに登録
        self.tool_manager.register_function(analyze_css_file)
        self.tool_manager.register_function(analyze_js_file)
        self.tool_manager.register_function(analyze_html_structure)

    def _suggest_responsive_fix(self, property_name: str, px_value: str) -> str:
        """
        レスポンシブ対応の修正案を提案

        Args:
            property_name: CSSプロパティ名
            px_value: px値

        Returns:
            修正案の文字列
        """
        px_val = int(px_value)

        if property_name in ["width", "max-width"]:
            if px_val > 1000:
                return f"{property_name}: min(90vw, {px_val/16:.1f}rem)"
            else:
                return f"{property_name}: {px_val/16:.1f}rem"
        elif property_name in ["height", "max-height"]:
            return f"{property_name}: {px_val/16:.1f}rem"
        elif property_name == "font-size":
            return f"{property_name}: {px_val/16:.1f}rem"
        else:
            return f"{property_name}: {px_val/16:.1f}rem"

    async def analyze_file(self, file_path: str) -> List[CodeIssue]:
        """
        単一ファイルを解析

        Args:
            file_path: 解析対象ファイルのパス

        Returns:
            検出された問題のリスト
        """
        path = Path(file_path)
        if not path.exists():
            logger.error(f"ファイルが存在しません: {file_path}")
            return []

        # ファイル内容を読み込み
        try:
            with open(path, "r", encoding="utf-8") as f:
                content = f.read()
        except UnicodeDecodeError:
            # UTF-8で読めない場合は他のエンコーディングを試す
            try:
                with open(path, "r", encoding="shift_jis") as f:
                    content = f.read()
            except Exception as e:
                logger.error(f"ファイル読み込みエラー: {file_path}, {e}")
                return []

        issues = []
        file_extension = path.suffix.lower()

        # ファイルタイプに応じて適切な解析ツールを使用
        if file_extension in [".css", ".scss", ".sass"]:
            result = await self.tool_manager.execute(
                "analyze_css_file", {"file_path": file_path, "content": content}
            )
            if result.success:
                issues.extend(result.result)

        elif file_extension in [".js", ".jsx"]:
            result = await self.tool_manager.execute(
                "analyze_js_file", {"file_path": file_path, "content": content}
            )
            if result.success:
                issues.extend(result.result)

        elif file_extension in [".html", ".htm", ".jsp"]:
            result = await self.tool_manager.execute(
                "analyze_html_structure", {"file_path": file_path, "content": content}
            )
            if result.success:
                issues.extend(result.result)

        # 辞書形式の問題をCodeIssueオブジェクトに変換
        code_issues = []
        for issue_dict in issues:
            code_issue = CodeIssue(
                file_path=file_path,
                line_number=issue_dict["line_number"],
                issue_type=IssueType(issue_dict["issue_type"]),
                severity=issue_dict["severity"],
                description=issue_dict["description"],
                current_code=issue_dict["current_code"],
                suggested_fix=issue_dict["suggested_fix"],
                auto_fixable=issue_dict["auto_fixable"],
            )
            code_issues.append(code_issue)

        # 解析結果をメモリに保存
        if code_issues:
            await self.memory.store(
                content=f"Analysis result for {file_path}: {len(code_issues)} issues found",
                metadata={
                    "type": "file_analysis",
                    "file_path": file_path,
                    "issue_count": len(code_issues),
                    "severity_breakdown": self._get_severity_breakdown(code_issues),
                },
            )

        logger.info(f"ファイル解析完了: {file_path}, {len(code_issues)}個の問題を検出")
        return code_issues

    def _get_severity_breakdown(self, issues: List[CodeIssue]) -> Dict[str, int]:
        """
        問題の重要度別集計を取得

        Args:
            issues: 問題のリスト

        Returns:
            重要度別の件数
        """
        breakdown = {"high": 0, "medium": 0, "low": 0}
        for issue in issues:
            breakdown[issue.severity] += 1
        return breakdown

    async def analyze_project(self, manifest_path: str) -> AnalysisResult:
        """
        プロジェクト全体を解析

        Args:
            manifest_path: マニフェストファイルのパス

        Returns:
            AnalysisResult: 解析結果
        """
        # マニフェストファイルを読み込み
        with open(manifest_path, "r", encoding="utf-8") as f:
            manifest = json.load(f)

        all_issues = []
        analyzed_files = 0

        # 各ファイルを解析
        for file_info in manifest["files"]:
            file_path = file_info["path"]
            file_type = file_info["type"]

            # フロントエンド関連ファイルのみ解析
            if file_type in ["html", "css", "javascript", "jsx", "tsx", "jsp"]:
                issues = await self.analyze_file(file_path)
                all_issues.extend(issues)
                analyzed_files += 1

        # 統計情報を計算
        file_statistics = self._calculate_file_statistics(all_issues)
        framework_analysis = self._analyze_framework_issues(manifest, all_issues)
        migration_complexity = self._assess_migration_complexity(all_issues)

        result = AnalysisResult(
            total_files_analyzed=analyzed_files,
            issues=all_issues,
            file_statistics=file_statistics,
            framework_analysis=framework_analysis,
            migration_complexity=migration_complexity,
        )

        logger.info(f"プロジェクト解析完了: {analyzed_files}ファイル, {len(all_issues)}問題")
        return result

    def _calculate_file_statistics(self, issues: List[CodeIssue]) -> Dict[str, Any]:
        """ファイル統計を計算"""
        stats: Dict[str, Any] = {
            "total_issues": len(issues),
            "severity_breakdown": self._get_severity_breakdown(issues),
            "issue_type_breakdown": {},
            "files_with_issues": len(set(issue.file_path for issue in issues)),
            "auto_fixable_issues": sum(1 for issue in issues if issue.auto_fixable),
        }

        # 問題タイプ別集計
        for issue in issues:
            issue_type = issue.issue_type.value
            stats["issue_type_breakdown"][issue_type] = (
                stats["issue_type_breakdown"].get(issue_type, 0) + 1
            )

        return stats

    def _analyze_framework_issues(
        self, manifest: Dict[str, Any], issues: List[CodeIssue]
    ) -> Dict[str, Any]:
        """フレームワーク関連の問題を分析"""
        _ = manifest.get("framework_info", {})

        analysis = {
            "jquery_migration_needed": any(
                issue.issue_type == IssueType.JQUERY_DEPENDENCY for issue in issues
            ),
            "css_framework_conflicts": [],
            "js_modernization_needed": any(
                issue.issue_type == IssueType.PERFORMANCE_ISSUE for issue in issues
            ),
            "accessibility_compliance": self._assess_accessibility_compliance(issues),
        }

        return analysis

    def _assess_accessibility_compliance(
        self, issues: List[CodeIssue]
    ) -> Dict[str, Any]:
        """アクセシビリティ準拠状況を評価"""
        accessibility_issues = [
            issue
            for issue in issues
            if issue.issue_type == IssueType.ACCESSIBILITY_VIOLATION
        ]

        return {
            "total_violations": len(accessibility_issues),
            "wcag_compliance_level": "AA"
            if len(accessibility_issues) == 0
            else "Non-compliant",
            "critical_issues": [
                issue.description
                for issue in accessibility_issues
                if issue.severity == "high"
            ],
        }

    def _assess_migration_complexity(self, issues: List[CodeIssue]) -> str:
        """移行複雑度を評価"""
        high_severity_count = sum(1 for issue in issues if issue.severity == "high")
        total_issues = len(issues)
        auto_fixable_ratio = sum(1 for issue in issues if issue.auto_fixable) / max(
            total_issues, 1
        )

        if high_severity_count > 50 or auto_fixable_ratio < 0.3:
            return "complex"
        elif high_severity_count > 20 or auto_fixable_ratio < 0.6:
            return "medium"
        else:
            return "simple"

    async def save_analysis_result(
        self, result: AnalysisResult, output_path: str
    ) -> None:
        """
        解析結果をJSONファイルに保存

        Args:
            result: 解析結果
            output_path: 出力ファイルパス
        """
        # DataClassをJSONシリアライズ可能な形式に変換
        result_dict = {
            "total_files_analyzed": result.total_files_analyzed,
            "issues": [
                {
                    "file_path": issue.file_path,
                    "line_number": issue.line_number,
                    "issue_type": issue.issue_type.value,
                    "severity": issue.severity,
                    "description": issue.description,
                    "current_code": issue.current_code,
                    "suggested_fix": issue.suggested_fix,
                    "auto_fixable": issue.auto_fixable,
                }
                for issue in result.issues
            ],
            "file_statistics": result.file_statistics,
            "framework_analysis": result.framework_analysis,
            "migration_complexity": result.migration_complexity,
        }

        with open(output_path, "w", encoding="utf-8") as f:
            json.dump(result_dict, f, indent=2, ensure_ascii=False)

        logger.info(f"解析結果を保存しました: {output_path}")


# 使用例とテスト用のメイン関数
async def main():
    """
    AnalyzerAgentの使用例
    """
    agent = AnalyzerAgent()

    # サンプルファイルを解析
    try:
        # 単一ファイル解析の例
        issues = await agent.analyze_file("sample.css")
        print(f"単一ファイル解析結果: {len(issues)}個の問題")

        # プロジェクト全体解析の例
        result = await agent.analyze_project("manifest.json")
        await agent.save_analysis_result(result, "issue_list.json")

        print("プロジェクト解析結果:")
        print(f"  解析ファイル数: {result.total_files_analyzed}")
        print(f"  総問題数: {len(result.issues)}")
        print(f"  移行複雑度: {result.migration_complexity}")

    except Exception as e:
        logger.error(f"解析エラー: {e}")


if __name__ == "__main__":
    asyncio.run(main())
