"""
移行ワークフロー統合テスト

エージェント間の連携とワークフロー全体の動作を検証する統合テストです。
実際の移行プロセスをエンドツーエンドでテストし、品質を保証します。
"""

import json
import tempfile
from datetime import datetime
from pathlib import Path

import pytest

from products.frontend_migration.workflows.migration_workflow import (
    MigrationOrchestrator,
    MigrationResult,
)

# テスト用の定数
TEST_DATA_DIR = Path(__file__).parent.parent / "test_data"
TEST_OUTPUT_DIR = Path(__file__).parent.parent / "test_output"


class TestMigrationWorkflow:
    """移行ワークフロー統合テスト"""

    @pytest.fixture
    def orchestrator(self):
        """テスト用のMigrationOrchestratorインスタンス"""
        config = {
            "output_directory": str(TEST_OUTPUT_DIR / "integration_test"),
            "backup_original": True,
            "parallel_processing": False,
            "max_concurrent_files": 3,
            "quality_threshold": 0.7,
            "auto_fix_enabled": True,
            "generate_reports": True,
            "supported_file_types": ["html", "css", "js", "jsx", "tsx", "jsp"],
        }
        return MigrationOrchestrator(config=config)

    @pytest.fixture
    def complex_sample_project(self):
        """複雑なサンプルプロジェクトを作成"""
        with tempfile.TemporaryDirectory() as temp_dir:
            project_path = Path(temp_dir)

            # HTML ファイル
            (project_path / "index.html").write_text(
                """
<!DOCTYPE html>
<html lang="ja">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>サンプルプロジェクト</title>
    <link rel="stylesheet" href="styles/main.css">
    <link rel="stylesheet" href="styles/responsive.css">
</head>
<body>
    <header class="header">
        <h1>メインタイトル</h1>
        <nav class="navigation">
            <ul>
                <li><a href="#home">ホーム</a></li>
                <li><a href="#about">概要</a></li>
                <li><a href="#contact">連絡先</a></li>
            </ul>
        </nav>
    </header>

    <main class="main-content">
        <section class="hero">
            <h2>ヒーローセクション</h2>
            <p>重要なメッセージがここに表示されます。</p>
            <button class="cta-button">アクションボタン</button>
        </section>

        <section class="features">
            <div class="feature-grid">
                <div class="feature-item">
                    <img src="images/feature1.jpg" alt="">
                    <h3>機能1</h3>
                    <p>機能1の説明</p>
                </div>
                <div class="feature-item">
                    <img src="images/feature2.jpg" alt="">
                    <h3>機能2</h3>
                    <p>機能2の説明</p>
                </div>
                <div class="feature-item">
                    <img src="images/feature3.jpg" alt="">
                    <h3>機能3</h3>
                    <p>機能3の説明</p>
                </div>
            </div>
        </section>
    </main>

    <footer class="footer">
        <p>&copy; 2024 サンプルプロジェクト</p>
    </footer>

    <script src="scripts/jquery-3.6.0.min.js"></script>
    <script src="scripts/main.js"></script>
</body>
</html>
            """,
                encoding="utf-8",
            )

            # メインCSS ファイル（問題のあるレガシーCSS）
            (project_path / "styles").mkdir()
            (project_path / "styles" / "main.css").write_text(
                """
/* レガシーCSS - 多くの問題を含む */
body {
    font-family: Arial, sans-serif;
    margin: 0;
    padding: 0;
    width: 1200px;
    font-size: 16px;
    background-color: #ffffff;
}

.header {
    width: 1200px;
    height: 80px;
    background-color: #333333;
    padding: 10px 20px;
    position: fixed;
    top: 0;
    left: 0;
}

.header h1 {
    color: white;
    font-size: 24px;
    margin: 0;
    float: left;
}

.navigation {
    float: right;
    margin-top: 15px;
}

.navigation ul {
    list-style: none;
    margin: 0;
    padding: 0;
}

.navigation li {
    display: inline-block;
    margin-left: 20px;
}

.navigation a {
    color: white;
    text-decoration: none;
    font-size: 14px;
    padding: 5px 10px;
}

.main-content {
    width: 1200px;
    margin: 100px auto 0;
    padding: 20px;
}

.hero {
    width: 1160px;
    height: 400px;
    background-color: #f0f0f0;
    padding: 20px;
    text-align: center;
    margin-bottom: 40px;
}

.hero h2 {
    font-size: 32px;
    margin-bottom: 20px;
}

.hero p {
    font-size: 18px;
    margin-bottom: 30px;
}

.cta-button {
    background-color: #007bff;
    color: white;
    border: none;
    padding: 12px 24px;
    font-size: 16px;
    cursor: pointer;
    border-radius: 4px;
}

.feature-grid {
    width: 1160px;
    margin: 0 auto;
}

.feature-item {
    width: 360px;
    height: 300px;
    float: left;
    margin-right: 40px;
    background-color: #f8f9fa;
    padding: 20px;
    border: 1px solid #dee2e6;
}

.feature-item:last-child {
    margin-right: 0;
}

.feature-item img {
    width: 360px;
    height: 200px;
    object-fit: cover;
}

.feature-item h3 {
    font-size: 20px;
    margin: 15px 0 10px;
}

.feature-item p {
    font-size: 14px;
    line-height: 20px;
}

.footer {
    width: 1200px;
    height: 60px;
    background-color: #333333;
    color: white;
    text-align: center;
    padding: 20px;
    margin-top: 40px;
}

/* IE専用スタイル */
.ie-only {
    filter: alpha(opacity=50);
    zoom: 1;
}

/* 古いブラウザ対応 */
.clearfix {
    *zoom: 1;
}

.clearfix:before,
.clearfix:after {
    content: "";
    display: table;
}

.clearfix:after {
    clear: both;
}
            """,
                encoding="utf-8",
            )

            # JavaScript ファイル（jQuery依存）
            (project_path / "scripts").mkdir()
            (project_path / "scripts" / "main.js").write_text(
                """
// レガシーJavaScript - jQuery依存
$(document).ready(function() {
    var isMenuOpen = false;

    // ナビゲーションメニューの制御
    $('.navigation').click(function() {
        if (isMenuOpen) {
            $(this).removeClass('open');
            isMenuOpen = false;
        } else {
            $(this).addClass('open');
            isMenuOpen = true;
        }
    });

    // CTAボタンのクリックイベント
    $('.cta-button').click(function() {
        var message = "ボタンがクリックされました！";
        alert(message);
    });

    // 機能アイテムのホバーエフェクト
    $('.feature-item').hover(
        function() {
            $(this).css('background-color', '#e9ecef');
        },
        function() {
            $(this).css('background-color', '#f8f9fa');
        }
    );

    // スクロールイベント
    $(window).scroll(function() {
        var scrollTop = $(this).scrollTop();
        if (scrollTop > 100) {
            $('.header').addClass('scrolled');
        } else {
            $('.header').removeClass('scrolled');
        }
    });

    // 古いJavaScript構文
    var oldFunction = function() {
        var data = {
            name: "sample",
            value: 123
        };
        return data;
    };

    // 非推奨のメソッド使用
    $('.feature-item').live('click', function() {
        console.log('Feature clicked');
    });
});
            """,
                encoding="utf-8",
            )

            # package.json
            (project_path / "package.json").write_text(
                json.dumps(
                    {
                        "name": "complex-sample-project",
                        "version": "1.0.0",
                        "description": "複雑なサンプルプロジェクト",
                        "dependencies": {"jquery": "^3.6.0", "bootstrap": "^4.6.0"},
                        "devDependencies": {
                            "webpack": "^5.0.0",
                            "babel-core": "^6.26.3",
                        },
                    }
                ),
                encoding="utf-8",
            )

            yield str(project_path)

    @pytest.mark.asyncio
    async def test_complete_migration_workflow(
        self, orchestrator, complex_sample_project
    ):
        """完全な移行ワークフローのテスト"""
        result = await orchestrator.migrate_project(complex_sample_project)

        # 基本的な結果構造をチェック
        assert isinstance(result, MigrationResult)
        assert result.project_name == Path(complex_sample_project).name
        assert isinstance(result.start_time, datetime)
        assert isinstance(result.end_time, datetime)
        assert result.end_time > result.start_time

        # 処理結果をチェック
        assert result.total_files_processed > 0
        assert result.issues_found > 0
        assert result.issues_fixed >= 0
        assert 0 <= result.conversion_success_rate <= 1

        # 出力ディレクトリが作成されているかチェック
        output_dir = Path(result.output_directory)
        assert output_dir.exists()

        # 必要なファイルが生成されているかチェック
        assert (output_dir / "manifest.json").exists()
        assert (output_dir / "issue_list.json").exists()
        assert (output_dir / "migration_summary.json").exists()
        assert (output_dir / "migration_report.md").exists()

        # 変換されたファイルが存在するかチェック
        converted_dir = output_dir / "converted"
        if converted_dir.exists():
            converted_files = list(converted_dir.glob("responsive_*.css"))
            assert len(converted_files) > 0

    @pytest.mark.asyncio
    async def test_manifest_generation(self, orchestrator, complex_sample_project):
        """マニフェスト生成のテスト"""
        result = await orchestrator.migrate_project(complex_sample_project)

        # マニフェストファイルを読み込み
        manifest_path = Path(result.output_directory) / "manifest.json"
        with open(manifest_path, "r", encoding="utf-8") as f:
            manifest = json.load(f)

        # マニフェストの内容をチェック
        assert manifest["project_name"] == Path(complex_sample_project).name
        assert manifest["total_files"] > 0
        assert "file_types" in manifest
        assert "files" in manifest
        assert "dependencies" in manifest
        assert "framework_info" in manifest

        # ファイルタイプが正しく検出されているかチェック
        file_types = manifest["file_types"]
        assert "html" in file_types
        assert "css" in file_types
        assert "javascript" in file_types

        # 依存関係が正しく抽出されているかチェック
        dependencies = manifest["dependencies"]
        assert "jquery" in dependencies
        assert "bootstrap" in dependencies

        # フレームワーク情報が正しく検出されているかチェック
        framework_info = manifest["framework_info"]
        assert "jQuery" in framework_info["frontend"]
        assert "Bootstrap" in framework_info["css_frameworks"]

    @pytest.mark.asyncio
    async def test_issue_analysis(self, orchestrator, complex_sample_project):
        """問題解析のテスト"""
        result = await orchestrator.migrate_project(complex_sample_project)

        # 問題リストファイルを読み込み
        issue_list_path = Path(result.output_directory) / "issue_list.json"
        with open(issue_list_path, "r", encoding="utf-8") as f:
            analysis_result = json.load(f)

        # 解析結果の基本構造をチェック
        assert "total_files_analyzed" in analysis_result
        assert "issues" in analysis_result
        assert "file_statistics" in analysis_result
        assert "framework_analysis" in analysis_result
        assert "migration_complexity" in analysis_result

        # 問題が正しく検出されているかチェック
        issues = analysis_result["issues"]
        assert len(issues) > 0

        # 固定pxサイズの問題が検出されているかチェック
        px_issues = [
            issue for issue in issues if issue["issue_type"] == "fixed_px_size"
        ]
        assert len(px_issues) > 0

        # jQuery依存の問題が検出されているかチェック
        jquery_issues = [
            issue for issue in issues if issue["issue_type"] == "jquery_dependency"
        ]
        assert len(jquery_issues) > 0

        # IE互換性の問題が検出されているかチェック
        ie_issues = [
            issue for issue in issues if issue["issue_type"] == "ie_compatibility"
        ]
        assert len(ie_issues) > 0

        # アクセシビリティ問題が検出されているかチェック
        a11y_issues = [
            issue
            for issue in issues
            if issue["issue_type"] == "accessibility_violation"
        ]
        assert len(a11y_issues) > 0  # alt属性なしのimg要素があるため

    @pytest.mark.asyncio
    async def test_responsive_conversion(self, orchestrator, complex_sample_project):
        """レスポンシブ変換のテスト"""
        result = await orchestrator.migrate_project(complex_sample_project)

        # 変換されたファイルをチェック
        converted_dir = Path(result.output_directory) / "converted"
        if converted_dir.exists():
            css_files = list(converted_dir.glob("responsive_*.css"))

            for css_file in css_files:
                with open(css_file, "r", encoding="utf-8") as f:
                    converted_css = f.read()

                # 固定pxサイズが削除されているかチェック
                assert "1200px" not in converted_css
                assert "800px" not in converted_css

                # レスポンシブ単位が使用されているかチェック
                assert (
                    "rem" in converted_css
                    or "clamp" in converted_css
                    or "%" in converted_css
                )

                # メディアクエリが追加されているかチェック
                assert "@media" in converted_css

    @pytest.mark.asyncio
    async def test_report_generation(self, orchestrator, complex_sample_project):
        """レポート生成のテスト"""
        result = await orchestrator.migrate_project(complex_sample_project)

        # サマリーレポートをチェック
        summary_path = Path(result.output_directory) / "migration_summary.json"
        with open(summary_path, "r", encoding="utf-8") as f:
            summary = json.load(f)

        assert "project_name" in summary
        assert "migration_summary" in summary
        assert "file_statistics" in summary
        assert "framework_analysis" in summary
        assert "migration_complexity" in summary

        # Markdownレポートをチェック
        markdown_path = Path(result.output_directory) / "migration_report.md"
        with open(markdown_path, "r", encoding="utf-8") as f:
            markdown_content = f.read()

        assert "# フロントエンド移行レポート" in markdown_content
        assert "## プロジェクト概要" in markdown_content
        assert "## 移行結果サマリー" in markdown_content
        assert result.project_name in markdown_content

    @pytest.mark.asyncio
    async def test_memory_integration(self, orchestrator, complex_sample_project):
        """メモリ統合のテスト"""
        # 移行前のメモリ状態を確認
        initial_count = await orchestrator.memory.count()

        # 移行を実行
        _ = await orchestrator.migrate_project(complex_sample_project)

        # メモリに移行プロセスが記録されているかチェック
        final_count = await orchestrator.memory.count()
        assert final_count > initial_count

        # 各ステップの記憶を検索
        inventory_memories = await orchestrator.memory.search(
            "Inventory completed", limit=5
        )
        assert len(inventory_memories) > 0

        analysis_memories = await orchestrator.memory.search(
            "Analysis completed", limit=5
        )
        assert len(analysis_memories) > 0

        conversion_memories = await orchestrator.memory.search(
            "Responsive conversion completed", limit=5
        )
        assert len(conversion_memories) > 0

        completion_memories = await orchestrator.memory.search(
            "Migration completed successfully", limit=5
        )
        assert len(completion_memories) > 0

    @pytest.mark.asyncio
    async def test_error_handling(self, orchestrator):
        """エラーハンドリングのテスト"""
        # 存在しないプロジェクトパスでテスト
        with pytest.raises(Exception):
            await orchestrator.migrate_project("/nonexistent/project")

        # エラーがメモリに記録されているかチェック
        error_memories = await orchestrator.memory.search("Migration failed", limit=5)
        assert len(error_memories) > 0

    def test_migration_result_dataclass(self):
        """MigrationResultデータクラスのテスト"""
        start_time = datetime.now()
        end_time = datetime.now()

        result = MigrationResult(
            project_name="test-project",
            start_time=start_time,
            end_time=end_time,
            total_files_processed=10,
            issues_found=20,
            issues_fixed=15,
            conversion_success_rate=0.75,
            output_directory="/output",
            reports=["report1.json", "report2.md"],
        )

        assert result.project_name == "test-project"
        assert result.start_time == start_time
        assert result.end_time == end_time
        assert result.total_files_processed == 10
        assert result.issues_found == 20
        assert result.issues_fixed == 15
        assert result.conversion_success_rate == 0.75
        assert result.output_directory == "/output"
        assert len(result.reports) == 2


# パフォーマンステスト
class TestMigrationWorkflowPerformance:
    """移行ワークフローのパフォーマンステスト"""

    @pytest.mark.asyncio
    async def test_large_project_migration_performance(self):
        """大規模プロジェクト移行のパフォーマンステスト"""
        orchestrator = MigrationOrchestrator()

        # 大規模プロジェクトを作成
        with tempfile.TemporaryDirectory() as temp_dir:
            project_path = Path(temp_dir)

            # 50個のHTMLファイルと50個のCSSファイルを作成
            for i in range(50):
                (project_path / f"page_{i}.html").write_text(
                    f"""
<!DOCTYPE html>
<html>
<head><title>Page {i}</title></head>
<body><h1>Content {i}</h1></body>
</html>
                """
                )

                (project_path / f"style_{i}.css").write_text(
                    f"""
.container_{i} {{
    width: {800 + i}px;
    height: {600 + i}px;
    font-size: {16 + i % 8}px;
}}
                """
                )

            # package.jsonを作成
            (project_path / "package.json").write_text(
                json.dumps(
                    {"name": "large-test-project", "dependencies": {"jquery": "^3.6.0"}}
                )
            )

            import time

            start_time = time.time()

            result = await orchestrator.migrate_project(str(project_path))

            end_time = time.time()
            migration_duration = end_time - start_time

            # パフォーマンス要件: 100ファイルを30秒以内で移行
            assert migration_duration < 30.0, f"移行時間が長すぎます: {migration_duration:.2f}秒"
            assert result.total_files_processed > 0

            print(
                f"大規模移行パフォーマンス: {result.total_files_processed}ファイルを{migration_duration:.2f}秒で移行"
            )
