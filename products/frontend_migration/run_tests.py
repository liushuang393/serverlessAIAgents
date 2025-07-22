#!/usr/bin/env python3
"""
フロントエンド移行システム テスト実行スクリプト

このスクリプトは、単体テスト、統合テスト、パフォーマンステストを実行し、
包括的なテストレポートを生成します。

使用方法:
    python run_tests.py [--unit] [--integration] [--performance] [--all]

オプション:
    --unit: 単体テストのみ実行
    --integration: 統合テストのみ実行
    --performance: パフォーマンステストのみ実行
    --all: 全てのテストを実行（デフォルト）
    --report: HTMLレポートを生成
    --coverage: カバレッジレポートを生成
"""

import argparse
import json
import subprocess
import sys
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional

# プロジェクトルートをPythonパスに追加
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))


class TestRunner:
    """テスト実行とレポート生成を管理するクラス"""

    def __init__(self, output_dir: str = "test_reports"):
        """
        テストランナーを初期化

        Args:
            output_dir: レポート出力ディレクトリ
        """
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(exist_ok=True)
        self.test_results: Dict[str, Any] = {}
        self.start_time: Optional[datetime] = None
        self.end_time: Optional[datetime] = None

    def run_unit_tests(self) -> Dict[str, Any]:
        """
        単体テストを実行

        Returns:
            テスト結果の辞書
        """
        print("🧪 単体テストを実行中...")

        cmd = [
            "conda",
            "run",
            "-n",
            "agent_ragenv",
            "python",
            "-m",
            "pytest",
            "products/frontend_migration/tests/unit/",
            "-v",
            "--tb=short",
            "--json-report",
            f"--json-report-file=products/frontend_migration/{self.output_dir}/unit_test_results.json",
        ]

        try:
            result = subprocess.run(
                cmd, capture_output=True, text=True, cwd=project_root
            )

            # JSON結果を読み込み
            json_file = (
                Path("products/frontend_migration")
                / self.output_dir
                / "unit_test_results.json"
            )
            if json_file.exists():
                with open(json_file, "r", encoding="utf-8") as f:
                    test_data = json.load(f)
                # summaryの構造を確認して適切に取得
                summary = test_data.get("summary", {})
                if "total" not in summary and "collected" in summary:
                    # pytest-json-reportの新しい形式に対応
                    summary = {
                        "total": summary.get("collected", 0),
                        "passed": summary.get("passed", 0),
                        "failed": summary.get("failed", 0),
                    }
                elif "total" in summary:
                    # 既存の形式をそのまま使用
                    pass
                else:
                    # フォールバック
                    summary = {"total": 0, "passed": 0, "failed": 0}
                test_data["summary"] = summary
            else:
                test_data = {"summary": {"total": 0, "passed": 0, "failed": 0}}

            return {
                "type": "unit",
                "status": "passed" if result.returncode == 0 else "failed",
                "return_code": result.returncode,
                "stdout": result.stdout,
                "stderr": result.stderr,
                "summary": test_data.get("summary", {}),
                "duration": test_data.get("duration", 0),
            }

        except Exception as e:
            return {
                "type": "unit",
                "status": "error",
                "error": str(e),
                "return_code": -1,
            }

    def run_integration_tests(self) -> Dict[str, Any]:
        """
        統合テストを実行

        Returns:
            テスト結果の辞書
        """
        print("🔗 統合テストを実行中...")

        cmd = [
            "conda",
            "run",
            "-n",
            "agent_ragenv",
            "python",
            "-m",
            "pytest",
            "products/frontend_migration/tests/integration/",
            "-v",
            "--tb=short",
            "--json-report",
            f"--json-report-file=products/frontend_migration/{self.output_dir}/integration_test_results.json",
        ]

        try:
            result = subprocess.run(
                cmd, capture_output=True, text=True, cwd=project_root
            )

            # JSON結果を読み込み
            json_file = (
                Path("products/frontend_migration")
                / self.output_dir
                / "integration_test_results.json"
            )
            if json_file.exists():
                with open(json_file, "r", encoding="utf-8") as f:
                    test_data = json.load(f)
                # summaryの構造を確認して適切に取得
                summary = test_data.get("summary", {})
                if "total" not in summary and "collected" in summary:
                    # pytest-json-reportの新しい形式に対応
                    summary = {
                        "total": summary.get("collected", 0),
                        "passed": summary.get("passed", 0),
                        "failed": summary.get("failed", 0),
                    }
                elif "total" in summary:
                    # 既存の形式をそのまま使用
                    pass
                else:
                    # フォールバック
                    summary = {"total": 0, "passed": 0, "failed": 0}
                test_data["summary"] = summary
            else:
                test_data = {"summary": {"total": 0, "passed": 0, "failed": 0}}

            return {
                "type": "integration",
                "status": "passed" if result.returncode == 0 else "failed",
                "return_code": result.returncode,
                "stdout": result.stdout,
                "stderr": result.stderr,
                "summary": test_data.get("summary", {}),
                "duration": test_data.get("duration", 0),
            }

        except Exception as e:
            return {
                "type": "integration",
                "status": "error",
                "error": str(e),
                "return_code": -1,
            }

    def run_performance_tests(self) -> Dict[str, Any]:
        """
        パフォーマンステストを実行

        Returns:
            テスト結果の辞書
        """
        print("⚡ パフォーマンステストを実行中...")

        cmd = [
            "conda",
            "run",
            "-n",
            "agent_ragenv",
            "python",
            "-m",
            "pytest",
            "products/frontend_migration/tests/",
            "-v",
            "-k",
            "performance",
            "--tb=short",
            "--json-report",
            f"--json-report-file=products/frontend_migration/{self.output_dir}/performance_test_results.json",
        ]

        try:
            result = subprocess.run(
                cmd, capture_output=True, text=True, cwd=project_root
            )

            # JSON結果を読み込み
            json_file = (
                Path("products/frontend_migration")
                / self.output_dir
                / "performance_test_results.json"
            )
            if json_file.exists():
                with open(json_file, "r", encoding="utf-8") as f:
                    test_data = json.load(f)
                # summaryの構造を確認して適切に取得
                summary = test_data.get("summary", {})
                if "total" not in summary and "collected" in summary:
                    # pytest-json-reportの新しい形式に対応
                    summary = {
                        "total": summary.get("collected", 0),
                        "passed": summary.get("passed", 0),
                        "failed": summary.get("failed", 0),
                    }
                elif "total" in summary:
                    # 既存の形式をそのまま使用
                    pass
                else:
                    # フォールバック
                    summary = {"total": 0, "passed": 0, "failed": 0}
                test_data["summary"] = summary
            else:
                test_data = {"summary": {"total": 0, "passed": 0, "failed": 0}}

            return {
                "type": "performance",
                "status": "passed" if result.returncode == 0 else "failed",
                "return_code": result.returncode,
                "stdout": result.stdout,
                "stderr": result.stderr,
                "summary": test_data.get("summary", {}),
                "duration": test_data.get("duration", 0),
            }

        except Exception as e:
            return {
                "type": "performance",
                "status": "error",
                "error": str(e),
                "return_code": -1,
            }

    def run_coverage_analysis(self) -> Dict[str, Any]:
        """
        カバレッジ解析を実行

        Returns:
            カバレッジ結果の辞書
        """
        print("📊 カバレッジ解析を実行中...")

        cmd = [
            "conda",
            "run",
            "-n",
            "agent_ragenv",
            "python",
            "-m",
            "pytest",
            "products/frontend_migration/tests/",
            "--cov=products.frontend_migration",
            "--cov-report=html:products/frontend_migration/"
            + str(self.output_dir / "coverage_html"),
            "--cov-report=json:products/frontend_migration/"
            + str(self.output_dir / "coverage.json"),
            "--cov-report=term",
        ]

        try:
            _ = subprocess.run(cmd, capture_output=True, text=True, cwd=project_root)

            # カバレッジJSONを読み込み
            coverage_file = (
                Path("products/frontend_migration") / self.output_dir / "coverage.json"
            )
            if coverage_file.exists():
                with open(coverage_file, "r", encoding="utf-8") as f:
                    coverage_data = json.load(f)

                total_coverage = coverage_data.get("totals", {}).get(
                    "percent_covered", 0
                )
            else:
                total_coverage = 0

            return {
                "type": "coverage",
                "status": "completed",
                "total_coverage": total_coverage,
                "html_report": str(self.output_dir / "coverage_html" / "index.html"),
                "json_report": str(coverage_file),
            }

        except Exception as e:
            return {"type": "coverage", "status": "error", "error": str(e)}

    def generate_summary_report(self) -> None:
        """総合レポートを生成"""
        print("📋 総合レポートを生成中...")

        # 総合統計を計算
        total_tests = 0
        total_passed = 0
        total_failed = 0
        total_duration = 0

        for result in self.test_results.values():
            if "summary" in result:
                summary = result["summary"]
                total_tests += summary.get("total", 0)
                total_passed += summary.get("passed", 0)
                total_failed += summary.get("failed", 0)
                total_duration += result.get("duration", 0)

        # レポートデータを作成
        report_data = {
            "test_run_info": {
                "start_time": self.start_time.isoformat() if self.start_time else None,
                "end_time": self.end_time.isoformat() if self.end_time else None,
                "total_duration": (self.end_time - self.start_time).total_seconds()
                if self.start_time and self.end_time
                else 0,
                "timestamp": datetime.now().isoformat(),
            },
            "summary": {
                "total_tests": total_tests,
                "passed_tests": total_passed,
                "failed_tests": total_failed,
                "success_rate": (total_passed / max(total_tests, 1)) * 100,
                "total_test_duration": total_duration,
            },
            "test_results": self.test_results,
        }

        # JSONレポートを保存
        json_report_path = self.output_dir / "test_summary.json"
        with open(json_report_path, "w", encoding="utf-8") as f:
            json.dump(report_data, f, indent=2, ensure_ascii=False)

        # Markdownレポートを生成
        markdown_report = self._generate_markdown_report(report_data)
        markdown_report_path = self.output_dir / "test_summary.md"
        with open(markdown_report_path, "w", encoding="utf-8") as f:
            f.write(markdown_report)

        print("✅ 総合レポートを生成しました:")
        print(f"   JSON: {json_report_path}")
        print(f"   Markdown: {markdown_report_path}")

    def _generate_markdown_report(self, report_data: Dict[str, Any]) -> str:
        """Markdown形式のレポートを生成"""
        summary = report_data["summary"]
        test_info = report_data["test_run_info"]

        report = f"""# フロントエンド移行システム テストレポート

## テスト実行情報
- **実行開始時刻**: {test_info.get('start_time', 'N/A')}
- **実行終了時刻**: {test_info.get('end_time', 'N/A')}
- **総実行時間**: {test_info.get('total_duration', 0):.2f}秒

## テスト結果サマリー
- **総テスト数**: {summary['total_tests']}
- **成功**: {summary['passed_tests']}
- **失敗**: {summary['failed_tests']}
- **成功率**: {summary['success_rate']:.1f}%
- **テスト実行時間**: {summary['total_test_duration']:.2f}秒

## 詳細結果

"""

        for test_type, result in report_data["test_results"].items():
            status_emoji = (
                "✅"
                if result["status"] == "passed"
                else "❌"
                if result["status"] == "failed"
                else "⚠️"
            )

            report += f"""### {test_type.title()}テスト {status_emoji}
- **ステータス**: {result["status"]}
- **実行時間**: {result.get("duration", 0):.2f}秒
"""

            if "summary" in result:
                test_summary = result["summary"]
                report += f"""- **総テスト数**: {test_summary.get("total", 0)}
- **成功**: {test_summary.get("passed", 0)}
- **失敗**: {test_summary.get("failed", 0)}
"""

            if result["status"] == "failed" and "stderr" in result:
                report += f"""
**エラー詳細**:
```
{result["stderr"][:500]}...
```
"""

            report += "\n"

        # カバレッジ情報を追加
        if "coverage" in report_data["test_results"]:
            coverage_result = report_data["test_results"]["coverage"]
            if coverage_result["status"] == "completed":
                report += f"""## カバレッジ情報
- **総カバレッジ**: {coverage_result.get('total_coverage', 0):.1f}%
- **HTMLレポート**: {coverage_result.get('html_report', 'N/A')}

"""

        report += """---
*このレポートは AI Blocks フロントエンド移行システムのテストランナーによって自動生成されました。*
"""

        return report

    def run_tests(self, test_types: List[str], include_coverage: bool = False) -> None:
        """
        指定されたテストタイプを実行

        Args:
            test_types: 実行するテストタイプのリスト
            include_coverage: カバレッジ解析を含めるかどうか
        """
        self.start_time = datetime.now()

        print(f"🚀 テスト実行を開始: {', '.join(test_types)}")
        print(f"📁 レポート出力先: {self.output_dir}")

        # 各テストタイプを実行
        if "unit" in test_types:
            self.test_results["unit"] = self.run_unit_tests()

        if "integration" in test_types:
            self.test_results["integration"] = self.run_integration_tests()

        if "performance" in test_types:
            self.test_results["performance"] = self.run_performance_tests()

        # カバレッジ解析を実行
        if include_coverage:
            self.test_results["coverage"] = self.run_coverage_analysis()

        self.end_time = datetime.now()

        # 総合レポートを生成
        self.generate_summary_report()

        # 結果を表示
        self._print_summary()

    def _print_summary(self) -> None:
        """テスト結果のサマリーを表示"""
        print("\n" + "=" * 60)
        print("📊 テスト実行結果サマリー")
        print("=" * 60)

        total_tests = 0
        total_passed = 0
        total_failed = 0

        for test_type, result in self.test_results.items():
            if test_type == "coverage":
                continue

            status_emoji = (
                "✅"
                if result["status"] == "passed"
                else "❌"
                if result["status"] == "failed"
                else "⚠️"
            )
            print(f"{status_emoji} {test_type.title()}テスト: {result['status']}")

            if "summary" in result:
                summary = result["summary"]
                total_tests += summary.get("total", 0)
                total_passed += summary.get("passed", 0)
                total_failed += summary.get("failed", 0)
                print(
                    f"   テスト数: {summary.get('total', 0)}, "
                    f"成功: {summary.get('passed', 0)}, "
                    f"失敗: {summary.get('failed', 0)}"
                )

        print("-" * 60)
        success_rate = (total_passed / max(total_tests, 1)) * 100
        print(f"📈 総合結果: {total_tests}テスト中 {total_passed}成功 ({success_rate:.1f}%)")

        if "coverage" in self.test_results:
            coverage_result = self.test_results["coverage"]
            if coverage_result["status"] == "completed":
                print(f"📊 コードカバレッジ: {coverage_result.get('total_coverage', 0):.1f}%")

        if self.end_time and self.start_time:
            duration = (self.end_time - self.start_time).total_seconds()
            print(f"⏱️  実行時間: {duration:.2f}秒")
        else:
            print("⏱️  実行時間: 不明")
        print("=" * 60)


def main():
    """メイン関数"""
    parser = argparse.ArgumentParser(description="フロントエンド移行システム テスト実行")
    parser.add_argument("--unit", action="store_true", help="単体テストを実行")
    parser.add_argument("--integration", action="store_true", help="統合テストを実行")
    parser.add_argument("--performance", action="store_true", help="パフォーマンステストを実行")
    parser.add_argument("--all", action="store_true", help="全てのテストを実行")
    parser.add_argument("--coverage", action="store_true", help="カバレッジ解析を実行")
    parser.add_argument("--output", default="test_reports", help="レポート出力ディレクトリ")

    args = parser.parse_args()

    # テストタイプを決定
    test_types = []
    if args.unit:
        test_types.append("unit")
    if args.integration:
        test_types.append("integration")
    if args.performance:
        test_types.append("performance")

    # デフォルトまたは--allが指定された場合は全てのテストを実行
    if args.all or not test_types:
        test_types = ["unit", "integration", "performance"]

    # テストランナーを作成して実行
    runner = TestRunner(args.output)
    runner.run_tests(test_types, include_coverage=args.coverage)


if __name__ == "__main__":
    main()
