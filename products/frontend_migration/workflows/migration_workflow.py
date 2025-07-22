"""
移行ワークフロー

フロントエンド移行プロセス全体を統合・管理するオーケストレーター。
各エージェントを適切な順序で実行し、結果を統合して最終的な移行を完了します。

主要機能:
- エージェント間の連携制御
- 進捗監視とエラーハンドリング
- 結果の統合と品質チェック
- レポート生成と出力管理
"""

import asyncio
import json
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional

from ai_blocks.core.memory import VectorMemory
from ai_blocks.core.router import LLMBasedRouter
from ai_blocks.utils.logging import get_logger

from ..agents.analyzer_agent import AnalyzerAgent
from ..agents.cd_orchestrator import CDOrchestrator
from ..agents.compat_fix_agent import CompatFixAgent
from ..agents.inventory_agent import InventoryAgent
from ..agents.migration_planner_agent import MigrationPlannerAgent
from ..agents.qa_report_agent import QAReportAgent
from ..agents.refactor_agent import RefactorAgent
from ..agents.responsive_agent import ResponsiveAgent
from ..agents.test_agent import TestAgent

logger = get_logger(__name__)


@dataclass
class MigrationResult:
    """移行結果を格納するデータクラス"""

    project_name: str
    start_time: datetime
    end_time: datetime
    total_files_processed: int
    issues_found: int
    issues_fixed: int
    conversion_success_rate: float
    output_directory: str
    reports: List[str]


class MigrationOrchestrator:
    """
    フロントエンド移行プロセスを統合管理するオーケストレーター

    ai_blocks.core.router.LLMBasedRouterを使用してエージェント間の
    フロー制御を行い、ai_blocks.core.memory.VectorMemoryを使用して
    プロセス全体の状態を管理します。
    """

    def __init__(self, llm_provider=None, config: Optional[Dict[str, Any]] = None):
        """
        移行オーケストレーターを初期化

        Args:
            llm_provider: LLMプロバイダー（オプション）
            config: 設定辞書（オプション）
        """
        self.llm_provider = llm_provider
        self.config = config or self._get_default_config()

        # コアコンポーネントを初期化
        self.memory = VectorMemory()
        self.router = LLMBasedRouter(llm_provider=llm_provider)

        # エージェントを初期化
        self.inventory_agent = InventoryAgent(llm_provider)
        self.analyzer_agent = AnalyzerAgent(llm_provider)
        self.responsive_agent = ResponsiveAgent(llm_provider)
        self.planner_agent = MigrationPlannerAgent(llm_provider)
        self.refactor_agent = RefactorAgent(llm_provider)
        self.compat_agent = CompatFixAgent(llm_provider)
        self.test_agent = TestAgent(llm_provider)
        self.qa_agent = QAReportAgent(llm_provider)
        self.cd_orchestrator = CDOrchestrator(llm_provider)

        # ルーティングルールを設定
        self._setup_routing_rules()

        logger.info("MigrationOrchestratorを初期化しました")

    def _get_default_config(self) -> Dict[str, Any]:
        """
        デフォルト設定を取得

        Returns:
            デフォルト設定の辞書
        """
        return {
            "output_directory": "migration_output",
            "backup_original": True,
            "parallel_processing": False,
            "max_concurrent_files": 5,
            "quality_threshold": 0.8,
            "auto_fix_enabled": True,
            "generate_reports": True,
            "supported_file_types": ["html", "css", "js", "jsx", "tsx", "jsp"],
        }

    def _setup_routing_rules(self) -> None:
        """エージェント間のルーティングルールを設定"""
        from ai_blocks.core.router import RouteDefinition

        # インベントリ → アナライザー
        self.router.register_route(
            RouteDefinition(
                pattern="inventory_complete",
                target="analyzer",
                description="インベントリ完了後にアナライザーに移行",
            )
        )

        # アナライザー → レスポンシブエージェント
        self.router.register_route(
            RouteDefinition(
                pattern="analysis_complete",
                target="responsive",
                description="解析完了後にレスポンシブ変換に移行",
            )
        )

        # レスポンシブエージェント → 完了
        self.router.register_route(
            RouteDefinition(
                pattern="conversion_complete",
                target="finalize",
                description="変換完了後に最終処理に移行",
            )
        )

    async def migrate_project(self, project_path: str) -> MigrationResult:
        """
        プロジェクト全体の移行を実行

        Args:
            project_path: 移行対象プロジェクトのパス

        Returns:
            MigrationResult: 移行結果
        """
        start_time = datetime.now()
        logger.info(f"プロジェクト移行を開始: {project_path}")

        try:
            # 出力ディレクトリを準備
            output_dir = Path(self.config["output_directory"])
            output_dir.mkdir(parents=True, exist_ok=True)

            # ステップ1: インベントリ作成
            logger.info("ステップ1: リポジトリインベントリを作成中...")
            manifest = await self.inventory_agent.scan_repository(project_path)
            manifest_path = output_dir / "manifest.json"
            await self.inventory_agent.save_manifest(manifest, str(manifest_path))

            # プロセス状態をメモリに保存
            await self.memory.store(
                content=f"Inventory completed for {project_path}",
                metadata={
                    "step": "inventory",
                    "project_path": project_path,
                    "total_files": manifest.total_files,
                    "timestamp": start_time.isoformat(),
                },
            )

            # ステップ2: コード解析
            logger.info("ステップ2: コード解析を実行中...")
            analysis_result = await self.analyzer_agent.analyze_project(
                str(manifest_path)
            )
            analysis_path = output_dir / "issue_list.json"
            await self.analyzer_agent.save_analysis_result(
                analysis_result, str(analysis_path)
            )

            # プロセス状態をメモリに保存
            await self.memory.store(
                content=f"Analysis completed for {project_path}",
                metadata={
                    "step": "analysis",
                    "issues_found": len(analysis_result.issues),
                    "complexity": analysis_result.migration_complexity,
                    "timestamp": datetime.now().isoformat(),
                },
            )

            # ステップ3: レスポンシブ変換
            logger.info("ステップ3: レスポンシブデザイン変換を実行中...")
            conversion_results = []
            issues_fixed = 0

            # CSS/HTML ファイルを順次変換
            for file_info in manifest.files:
                if file_info.type in ["css", "html", "jsx", "tsx"]:
                    # 該当ファイルの問題を抽出
                    file_issues = [
                        {
                            "issue_type": issue.issue_type.value,
                            "current_code": issue.current_code,
                            "line_number": issue.line_number,
                        }
                        for issue in analysis_result.issues
                        if issue.file_path == file_info.path
                    ]

                    if file_issues:
                        try:
                            conversion = await self.responsive_agent.convert_css_file(
                                file_info.path, file_issues
                            )
                            conversion_results.append(conversion)
                            issues_fixed += len(conversion.applied_rules)

                            # 変換結果を保存
                            await self.responsive_agent.save_conversion_result(
                                conversion, str(output_dir / "converted")
                            )

                        except Exception as e:
                            logger.error(f"ファイル変換エラー: {file_info.path}, {e}")

            # プロセス状態をメモリに保存
            await self.memory.store(
                content=f"Responsive conversion completed for {project_path}",
                metadata={
                    "step": "conversion",
                    "files_converted": len(conversion_results),
                    "issues_fixed": issues_fixed,
                    "timestamp": datetime.now().isoformat(),
                },
            )

            # ステップ4: 最終レポート生成
            logger.info("ステップ4: 最終レポートを生成中...")
            end_time = datetime.now()

            migration_result = MigrationResult(
                project_name=manifest.project_name,
                start_time=start_time,
                end_time=end_time,
                total_files_processed=len(conversion_results),
                issues_found=len(analysis_result.issues),
                issues_fixed=issues_fixed,
                conversion_success_rate=issues_fixed
                / max(len(analysis_result.issues), 1),
                output_directory=str(output_dir),
                reports=[],
            )

            # 最終レポートを保存
            if self.config["generate_reports"]:
                report_paths = await self._generate_final_reports(
                    migration_result,
                    manifest,
                    analysis_result,
                    conversion_results,
                    output_dir,
                )
                migration_result.reports = report_paths

            # 成功をメモリに記録
            await self.memory.store(
                content=f"Migration completed successfully for {project_path}",
                metadata={
                    "step": "completed",
                    "success_rate": migration_result.conversion_success_rate,
                    "duration_minutes": (end_time - start_time).total_seconds() / 60,
                    "timestamp": end_time.isoformat(),
                },
            )

            logger.info(f"プロジェクト移行完了: {project_path}")
            logger.info(f"  処理ファイル数: {migration_result.total_files_processed}")
            logger.info(f"  発見問題数: {migration_result.issues_found}")
            logger.info(f"  修正問題数: {migration_result.issues_fixed}")
            logger.info(f"  成功率: {migration_result.conversion_success_rate:.1%}")
            logger.info(f"  処理時間: {(end_time - start_time).total_seconds():.1f}秒")

            return migration_result

        except Exception as e:
            logger.error(f"プロジェクト移行エラー: {e}")
            # エラーをメモリに記録
            await self.memory.store(
                content=f"Migration failed for {project_path}: {str(e)}",
                metadata={
                    "step": "error",
                    "error_message": str(e),
                    "timestamp": datetime.now().isoformat(),
                },
            )
            raise

    async def _generate_final_reports(
        self,
        migration_result: MigrationResult,
        manifest,
        analysis_result,
        conversion_results,
        output_dir: Path,
    ) -> List[str]:
        """
        最終レポートを生成

        Args:
            migration_result: 移行結果
            manifest: プロジェクトマニフェスト
            analysis_result: 解析結果
            conversion_results: 変換結果のリスト
            output_dir: 出力ディレクトリ

        Returns:
            生成されたレポートファイルのパスリスト
        """
        report_paths = []

        # 1. サマリーレポート（JSON）
        summary_report = {
            "project_name": migration_result.project_name,
            "migration_summary": {
                "start_time": migration_result.start_time.isoformat(),
                "end_time": migration_result.end_time.isoformat(),
                "duration_seconds": (
                    migration_result.end_time - migration_result.start_time
                ).total_seconds(),
                "total_files_processed": migration_result.total_files_processed,
                "issues_found": migration_result.issues_found,
                "issues_fixed": migration_result.issues_fixed,
                "conversion_success_rate": migration_result.conversion_success_rate,
            },
            "file_statistics": analysis_result.file_statistics,
            "framework_analysis": analysis_result.framework_analysis,
            "migration_complexity": analysis_result.migration_complexity,
        }

        summary_path = output_dir / "migration_summary.json"
        with open(summary_path, "w", encoding="utf-8") as f:
            json.dump(summary_report, f, indent=2, ensure_ascii=False)
        report_paths.append(str(summary_path))

        # 2. 詳細レポート（Markdown）
        markdown_report = self._generate_markdown_report(
            migration_result, manifest, analysis_result, conversion_results
        )

        markdown_path = output_dir / "migration_report.md"
        with open(markdown_path, "w", encoding="utf-8") as f:
            f.write(markdown_report)
        report_paths.append(str(markdown_path))

        logger.info(f"最終レポートを生成しました: {len(report_paths)}ファイル")
        return report_paths

    def _generate_markdown_report(
        self, migration_result, manifest, analysis_result, conversion_results
    ) -> str:
        """
        Markdown形式の詳細レポートを生成

        Returns:
            Markdownレポートの文字列
        """
        duration = migration_result.end_time - migration_result.start_time

        report = f"""# フロントエンド移行レポート

## プロジェクト概要
- **プロジェクト名**: {migration_result.project_name}
- **移行開始時刻**: {migration_result.start_time.strftime('%Y-%m-%d %H:%M:%S')}
- **移行完了時刻**: {migration_result.end_time.strftime('%Y-%m-%d %H:%M:%S')}
- **処理時間**: {duration.total_seconds():.1f}秒

## 移行結果サマリー
- **処理ファイル数**: {migration_result.total_files_processed}
- **発見問題数**: {migration_result.issues_found}
- **修正問題数**: {migration_result.issues_fixed}
- **成功率**: {migration_result.conversion_success_rate:.1%}
- **移行複雑度**: {analysis_result.migration_complexity}

## ファイル統計
- **総問題数**: {analysis_result.file_statistics['total_issues']}
- **自動修正可能**: {analysis_result.file_statistics['auto_fixable_issues']}
- **問題のあるファイル数**: {analysis_result.file_statistics['files_with_issues']}

### 重要度別内訳
- **高**: {analysis_result.file_statistics['severity_breakdown']['high']}
- **中**: {analysis_result.file_statistics['severity_breakdown']['medium']}
- **低**: {analysis_result.file_statistics['severity_breakdown']['low']}

## フレームワーク分析
- **jQuery移行必要**: {'はい' if analysis_result.framework_analysis['jquery_migration_needed'] else 'いいえ'}
- **JS現代化必要**: {'はい' if analysis_result.framework_analysis['js_modernization_needed'] else 'いいえ'}
- **アクセシビリティ準拠**: {analysis_result.framework_analysis['accessibility_compliance']['wcag_compliance_level']}

## 変換結果詳細
"""

        for i, conversion in enumerate(conversion_results, 1):
            report += f"""
### ファイル {i}: {Path(conversion.file_path).name}
- **適用ルール数**: {len(conversion.applied_rules)}
- **メディアクエリ追加**: {len(conversion.media_queries)}
- **Grid レイアウト提案**: {len(conversion.grid_layouts)}
- **Flexbox レイアウト提案**: {len(conversion.flexbox_layouts)}
"""

        report += f"""
## 出力ファイル
- **出力ディレクトリ**: {migration_result.output_directory}
- **生成レポート**: {len(migration_result.reports)}

---
*このレポートは AI Blocks フロントエンド移行システムによって自動生成されました。*
"""

        return report


# 使用例とテスト用のメイン関数
async def main():
    """
    MigrationOrchestratorの使用例
    """
    orchestrator = MigrationOrchestrator()

    # サンプルプロジェクトを移行
    try:
        result = await orchestrator.migrate_project("/path/to/legacy/project")

        print("移行完了:")
        print(f"  プロジェクト: {result.project_name}")
        print(f"  成功率: {result.conversion_success_rate:.1%}")
        print(f"  出力: {result.output_directory}")
        print(f"  レポート: {len(result.reports)}ファイル")

    except Exception as e:
        logger.error(f"移行エラー: {e}")


if __name__ == "__main__":
    asyncio.run(main())
