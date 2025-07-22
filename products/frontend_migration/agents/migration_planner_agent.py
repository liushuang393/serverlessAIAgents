"""
移行プランナーエージェント

移行ブループリント生成：技術スタック、ページ別戦略、優先度を担当するエージェント。
ai_blocks.core.router.LLMBasedRouterとai_blocks.architectures.prompt_chaining.PromptChainを
活用して段階的な移行計画を生成します。

主要機能:
- 技術スタック分析と移行戦略の策定
- ページ別移行優先度の決定
- リスク評価と対策計画の生成
- 移行ブループリント（plan.yaml）の出力
"""

import asyncio
import json
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Any, Dict, List, Optional

import yaml

from ai_blocks.architectures.prompt_chaining import PromptChain, SpecializedAgent
from ai_blocks.core.memory import VectorMemory
from ai_blocks.core.router import LLMBasedRouter
from ai_blocks.utils.logging import get_logger

logger = get_logger(__name__)


class MigrationPriority(Enum):
    """移行優先度の列挙"""

    CRITICAL = "critical"
    HIGH = "high"
    MEDIUM = "medium"
    LOW = "low"


class MigrationStrategy(Enum):
    """移行戦略の列挙"""

    FULL_REWRITE = "full_rewrite"
    INCREMENTAL = "incremental"
    HYBRID = "hybrid"
    MINIMAL = "minimal"


@dataclass
class PageMigrationPlan:
    """ページ移行計画を表すデータクラス"""

    page_path: str
    current_tech: List[str]
    target_tech: List[str]
    priority: MigrationPriority
    strategy: MigrationStrategy
    estimated_effort: int  # 時間（時）
    dependencies: List[str]
    risks: List[str]
    notes: str


@dataclass
class MigrationBlueprint:
    """移行ブループリントを格納するデータクラス"""

    project_name: str
    overall_strategy: MigrationStrategy
    target_framework: str
    target_css_framework: str
    page_plans: List[PageMigrationPlan]
    global_changes: Dict[str, Any]
    timeline_weeks: int
    risk_assessment: Dict[str, Any]
    success_criteria: List[str]


class MigrationPlannerAgent:
    """
    移行計画を生成するエージェント

    ai_blocks.core.router.LLMBasedRouterを使用して処理フローを制御し、
    ai_blocks.architectures.prompt_chaining.PromptChainを使用して
    段階的な計画生成を行います。
    """

    def __init__(self, llm_provider=None):
        """
        移行プランナーエージェントを初期化

        Args:
            llm_provider: LLMプロバイダー（オプション）
        """
        self.router = LLMBasedRouter(llm_provider=llm_provider)
        self.memory = VectorMemory()
        self.llm_provider = llm_provider

        # 計画生成用のプロンプトチェーンを設定
        self.planning_chain = self._setup_planning_chain()

        logger.info("MigrationPlannerAgentを初期化しました")

    def _setup_planning_chain(self) -> PromptChain:
        """
        移行計画生成用のプロンプトチェーンを設定

        Returns:
            設定されたPromptChain
        """
        # 技術分析エージェント
        tech_analyzer = SpecializedAgent(
            name="TechAnalyzer",
            task_description="""
            技術スタック分析の専門家として、現在の技術構成を分析し、
            最適な移行先技術スタックを提案してください。

            分析項目：
            - 現在使用されている技術とバージョン
            - 技術的負債の評価
            - モダン技術への移行可能性
            - パフォーマンスとメンテナンス性の改善見込み
            """,
            llm_provider=self.llm_provider,
        )

        # 戦略策定エージェント
        strategy_planner = SpecializedAgent(
            name="StrategyPlanner",
            task_description="""
            移行戦略策定の専門家として、プロジェクトの特性に応じた
            最適な移行戦略を策定してください。

            考慮事項：
            - プロジェクトの規模と複雑さ
            - 開発チームのスキルレベル
            - ビジネス要件と制約
            - リスクとリターンのバランス
            """,
            llm_provider=self.llm_provider,
        )

        # リスク評価エージェント
        risk_assessor = SpecializedAgent(
            name="RiskAssessor",
            task_description="""
            リスク評価の専門家として、移行プロジェクトのリスクを
            特定し、対策を提案してください。

            評価項目：
            - 技術的リスク
            - スケジュールリスク
            - 品質リスク
            - ビジネスインパクト
            """,
            llm_provider=self.llm_provider,
        )

        return PromptChain(
            agents=[tech_analyzer, strategy_planner, risk_assessor],
            name="MigrationPlanningChain",
        )

    async def generate_migration_plan(
        self,
        manifest_path: str,
        issue_list_path: str,
        requirements: Optional[Dict[str, Any]] = None,
    ) -> MigrationBlueprint:
        """
        移行計画を生成

        Args:
            manifest_path: プロジェクトマニフェストのパス
            issue_list_path: 問題リストのパス
            requirements: 追加要件（オプション）

        Returns:
            MigrationBlueprint: 生成された移行ブループリント
        """
        logger.info("移行計画の生成を開始します")

        # マニフェストと問題リストを読み込み
        with open(manifest_path, "r", encoding="utf-8") as f:
            manifest = json.load(f)

        with open(issue_list_path, "r", encoding="utf-8") as f:
            issue_list = json.load(f)

        requirements = requirements or {}

        # 技術スタック分析
        tech_analysis = await self._analyze_tech_stack(manifest, issue_list)

        # 移行戦略の決定
        migration_strategy = await self._determine_migration_strategy(
            manifest, issue_list, tech_analysis, requirements
        )

        # ページ別計画の生成
        page_plans = await self._generate_page_plans(
            manifest, issue_list, migration_strategy
        )

        # リスク評価
        risk_assessment = await self._assess_risks(
            manifest, issue_list, migration_strategy, page_plans
        )

        # タイムライン計算
        timeline_weeks = self._calculate_timeline(page_plans)

        # 成功基準の定義
        success_criteria = self._define_success_criteria(requirements)

        # ブループリントを作成
        blueprint = MigrationBlueprint(
            project_name=manifest["project_name"],
            overall_strategy=migration_strategy["strategy"],
            target_framework=migration_strategy["target_framework"],
            target_css_framework=migration_strategy["target_css_framework"],
            page_plans=page_plans,
            global_changes=migration_strategy["global_changes"],
            timeline_weeks=timeline_weeks,
            risk_assessment=risk_assessment,
            success_criteria=success_criteria,
        )

        # メモリに保存
        await self.memory.store(
            content=f"Migration plan generated for {manifest['project_name']}",
            metadata={
                "type": "migration_plan",
                "project_name": manifest["project_name"],
                "strategy": migration_strategy["strategy"].value,
                "timeline_weeks": timeline_weeks,
            },
        )

        logger.info(f"移行計画生成完了: {len(page_plans)}ページ、{timeline_weeks}週間の計画")
        return blueprint

    async def _analyze_tech_stack(
        self, manifest: Dict[str, Any], issue_list: Dict[str, Any]
    ) -> Dict[str, Any]:
        """技術スタックを分析"""
        framework_info = manifest.get("framework_info", {})
        dependencies = manifest.get("dependencies", {})
        issues = issue_list.get("issues", [])

        # 現在の技術スタック
        current_tech = {
            "frontend_frameworks": framework_info.get("frontend", []),
            "css_frameworks": framework_info.get("css_frameworks", []),
            "build_tools": framework_info.get("build_tools", []),
            "backend": framework_info.get("backend", []),
            "dependencies": dependencies,
        }

        # 問題の分析
        jquery_issues = len(
            [i for i in issues if i.get("issue_type") == "jquery_dependency"]
        )
        ie_issues = len(
            [i for i in issues if i.get("issue_type") == "ie_compatibility"]
        )
        px_issues = len([i for i in issues if i.get("issue_type") == "fixed_px_size"])

        # 推奨技術スタック
        recommended_tech = self._recommend_tech_stack(
            current_tech,
            {
                "jquery_issues": jquery_issues,
                "ie_issues": ie_issues,
                "px_issues": px_issues,
            },
        )

        return {
            "current": current_tech,
            "recommended": recommended_tech,
            "migration_complexity": issue_list.get("migration_complexity", "medium"),
            "issue_summary": {
                "jquery_dependencies": jquery_issues,
                "ie_compatibility": ie_issues,
                "responsive_issues": px_issues,
            },
        }

    def _recommend_tech_stack(
        self, current_tech: Dict[str, Any], issue_summary: Dict[str, int]
    ) -> Dict[str, Any]:
        """推奨技術スタックを決定"""
        # jQuery依存が多い場合はReactを推奨
        if issue_summary["jquery_issues"] > 10:
            frontend_framework = "React"
        elif "Vue.js" in current_tech["frontend_frameworks"]:
            frontend_framework = "Vue.js"  # 既存のVue.jsを維持
        else:
            frontend_framework = "React"  # デフォルト

        # CSS フレームワーク
        if "Bootstrap" in current_tech["css_frameworks"]:
            css_framework = "Bootstrap 5"  # アップグレード
        else:
            css_framework = "Tailwind CSS"  # モダンな選択

        # ビルドツール
        if "Webpack" in current_tech["build_tools"]:
            build_tool = "Webpack 5"
        else:
            build_tool = "Vite"  # 高速なビルドツール

        return {
            "frontend_framework": frontend_framework,
            "css_framework": css_framework,
            "build_tool": build_tool,
            "typescript": True,
            "testing_framework": "Jest + React Testing Library",
            "linting": "ESLint + Prettier",
        }

    async def _determine_migration_strategy(
        self,
        manifest: Dict[str, Any],
        issue_list: Dict[str, Any],
        tech_analysis: Dict[str, Any],
        requirements: Dict[str, Any],
    ) -> Dict[str, Any]:
        """移行戦略を決定"""
        complexity = issue_list.get("migration_complexity", "medium")
        total_files = manifest.get("total_files", 0)
        _ = len(issue_list.get("issues", []))

        # 戦略決定ロジック
        if complexity == "simple" and total_files < 20:
            strategy = MigrationStrategy.FULL_REWRITE
        elif complexity == "complex" or total_files > 100:
            strategy = MigrationStrategy.INCREMENTAL
        else:
            strategy = MigrationStrategy.HYBRID

        return {
            "strategy": strategy,
            "target_framework": tech_analysis["recommended"]["frontend_framework"],
            "target_css_framework": tech_analysis["recommended"]["css_framework"],
            "global_changes": {
                "typescript_migration": True,
                "css_modernization": True,
                "accessibility_improvements": True,
                "performance_optimization": True,
            },
        }

    async def _generate_page_plans(
        self,
        manifest: Dict[str, Any],
        issue_list: Dict[str, Any],
        migration_strategy: Dict[str, Any],
    ) -> List[PageMigrationPlan]:
        """ページ別移行計画を生成"""
        page_plans = []
        files = manifest.get("files", [])
        issues = issue_list.get("issues", [])

        # HTMLファイルを基準にページを特定
        html_files = [f for f in files if f["type"] == "html"]

        for html_file in html_files:
            file_path = html_file["path"]

            # このページに関連する問題を抽出
            page_issues = [i for i in issues if i["file_path"] == file_path]

            # 優先度を決定
            priority = self._determine_page_priority(html_file, page_issues)

            # 工数見積もり
            effort = self._estimate_effort(html_file, page_issues, migration_strategy)

            # 依存関係を特定
            dependencies = self._identify_dependencies(html_file, files)

            # リスクを特定
            risks = self._identify_page_risks(html_file, page_issues)

            plan = PageMigrationPlan(
                page_path=file_path,
                current_tech=["HTML", "CSS", "JavaScript"],
                target_tech=[
                    migration_strategy["target_framework"],
                    "TypeScript",
                    migration_strategy["target_css_framework"],
                ],
                priority=priority,
                strategy=migration_strategy["strategy"],
                estimated_effort=effort,
                dependencies=dependencies,
                risks=risks,
                notes=f"{len(page_issues)}個の問題を修正予定",
            )

            page_plans.append(plan)

        return page_plans

    def _determine_page_priority(
        self, html_file: Dict[str, Any], page_issues: List[Dict[str, Any]]
    ) -> MigrationPriority:
        """ページの移行優先度を決定"""
        # ファイル名から重要度を推測
        file_path = html_file["path"].lower()

        if "index" in file_path or "home" in file_path:
            return MigrationPriority.CRITICAL
        elif "login" in file_path or "auth" in file_path:
            return MigrationPriority.HIGH
        elif len(page_issues) > 10:
            return MigrationPriority.HIGH
        elif len(page_issues) > 5:
            return MigrationPriority.MEDIUM
        else:
            return MigrationPriority.LOW

    def _estimate_effort(
        self,
        html_file: Dict[str, Any],
        page_issues: List[Dict[str, Any]],
        migration_strategy: Dict[str, Any],
    ) -> int:
        """工数を見積もり（時間）"""
        base_effort = 8  # 基本工数（時間）

        # ファイルサイズによる調整
        file_size = html_file.get("size", 0)
        if file_size > 10000:
            base_effort += 4
        elif file_size > 5000:
            base_effort += 2

        # 問題数による調整
        issue_effort = len(page_issues) * 0.5

        # 戦略による調整
        if migration_strategy["strategy"] == MigrationStrategy.FULL_REWRITE:
            base_effort = int(base_effort * 1.5)
        elif migration_strategy["strategy"] == MigrationStrategy.INCREMENTAL:
            base_effort = int(base_effort * 1.2)

        return int(base_effort + issue_effort)

    def _identify_dependencies(
        self, html_file: Dict[str, Any], all_files: List[Dict[str, Any]]
    ) -> List[str]:
        """依存関係を特定"""
        dependencies = []

        # 同じディレクトリのCSSファイル
        html_dir = str(Path(html_file["path"]).parent)
        css_files = [
            f["path"]
            for f in all_files
            if f["type"] == "css" and str(Path(f["path"]).parent) == html_dir
        ]
        dependencies.extend(css_files)

        return dependencies

    def _identify_page_risks(
        self, html_file: Dict[str, Any], page_issues: List[Dict[str, Any]]
    ) -> List[str]:
        """ページ固有のリスクを特定"""
        risks = []

        # 高重要度の問題があるかチェック
        high_severity_issues = [i for i in page_issues if i.get("severity") == "high"]
        if high_severity_issues:
            risks.append(f"高重要度の問題が{len(high_severity_issues)}件存在")

        # jQuery依存のチェック
        jquery_issues = [
            i for i in page_issues if i.get("issue_type") == "jquery_dependency"
        ]
        if jquery_issues:
            risks.append("jQuery依存の解消が必要")

        # アクセシビリティ問題のチェック
        a11y_issues = [
            i for i in page_issues if i.get("issue_type") == "accessibility_violation"
        ]
        if a11y_issues:
            risks.append("アクセシビリティ対応が必要")

        return risks

    async def _assess_risks(
        self,
        manifest: Dict[str, Any],
        issue_list: Dict[str, Any],
        migration_strategy: Dict[str, Any],
        page_plans: List[PageMigrationPlan],
    ) -> Dict[str, Any]:
        """リスク評価を実行"""
        total_effort = sum(plan.estimated_effort for plan in page_plans)
        high_priority_pages = len(
            [p for p in page_plans if p.priority == MigrationPriority.CRITICAL]
        )

        return {
            "technical_risks": ["レガシーコードの複雑な依存関係", "ブラウザ互換性の問題", "パフォーマンス劣化の可能性"],
            "schedule_risks": [
                f"総工数{total_effort}時間の見積もり精度",
                "開発チームのスキル習得時間",
                "テストとデバッグの時間",
            ],
            "business_risks": [
                f"重要ページ{high_priority_pages}件の移行リスク",
                "ユーザー体験の一時的な低下",
                "SEOへの影響",
            ],
            "mitigation_strategies": ["段階的リリースによるリスク軽減", "包括的なテスト戦略の実装", "ロールバック計画の準備"],
        }

    def _calculate_timeline(self, page_plans: List[PageMigrationPlan]) -> int:
        """タイムラインを計算（週）"""
        total_effort_hours = sum(plan.estimated_effort for plan in page_plans)

        # 1週間40時間、並行作業を考慮して効率70%
        effective_hours_per_week = 40 * 0.7

        # 最低4週間は確保
        weeks = max(4, int(total_effort_hours / effective_hours_per_week))

        return weeks

    def _define_success_criteria(self, requirements: Dict[str, Any]) -> List[str]:
        """成功基準を定義"""
        criteria = [
            "全ページのレスポンシブ対応完了",
            "WCAG 2.2 AA準拠の達成",
            "Lighthouse スコア 90点以上",
            "全ブラウザでの動作確認完了",
            "パフォーマンス改善（LCP < 2.5s）",
        ]

        # 要件に応じて追加
        if requirements.get("seo_important"):
            criteria.append("SEO スコアの維持・向上")

        if requirements.get("mobile_first"):
            criteria.append("モバイルファーストデザインの実装")

        return criteria

    async def save_blueprint(
        self, blueprint: MigrationBlueprint, output_path: str
    ) -> None:
        """
        移行ブループリントをYAMLファイルに保存

        Args:
            blueprint: 保存するブループリント
            output_path: 出力ファイルパス
        """
        # DataClassをYAMLシリアライズ可能な形式に変換
        blueprint_dict = {
            "project_name": blueprint.project_name,
            "overall_strategy": blueprint.overall_strategy.value,
            "target_framework": blueprint.target_framework,
            "target_css_framework": blueprint.target_css_framework,
            "timeline_weeks": blueprint.timeline_weeks,
            "global_changes": blueprint.global_changes,
            "page_plans": [
                {
                    "page_path": plan.page_path,
                    "current_tech": plan.current_tech,
                    "target_tech": plan.target_tech,
                    "priority": plan.priority.value,
                    "strategy": plan.strategy.value,
                    "estimated_effort": plan.estimated_effort,
                    "dependencies": plan.dependencies,
                    "risks": plan.risks,
                    "notes": plan.notes,
                }
                for plan in blueprint.page_plans
            ],
            "risk_assessment": blueprint.risk_assessment,
            "success_criteria": blueprint.success_criteria,
        }

        with open(output_path, "w", encoding="utf-8") as f:
            yaml.dump(blueprint_dict, f, default_flow_style=False, allow_unicode=True)

        logger.info(f"移行ブループリントを保存しました: {output_path}")


# 使用例とテスト用のメイン関数
async def main():
    """
    MigrationPlannerAgentの使用例
    """
    agent = MigrationPlannerAgent()

    try:
        # 移行計画を生成
        blueprint = await agent.generate_migration_plan(
            "manifest.json",
            "issue_list.json",
            requirements={"seo_important": True, "mobile_first": True},
        )

        # ブループリントを保存
        await agent.save_blueprint(blueprint, "migration_plan.yaml")

        print("移行計画生成完了:")
        print(f"  プロジェクト: {blueprint.project_name}")
        print(f"  戦略: {blueprint.overall_strategy.value}")
        print(f"  対象フレームワーク: {blueprint.target_framework}")
        print(f"  ページ数: {len(blueprint.page_plans)}")
        print(f"  予想期間: {blueprint.timeline_weeks}週間")

    except Exception as e:
        logger.error(f"移行計画生成エラー: {e}")


if __name__ == "__main__":
    asyncio.run(main())
