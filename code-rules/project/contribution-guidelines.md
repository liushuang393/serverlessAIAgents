# コントリビューションガイドライン

> **バージョン**: 1.0.0
> **適用範囲**: AgentFlow Framework 全コントリビューター
> **最終更新**: 2025-01-19

## 📋 目次

1. [コントリビューションの種類](#コントリビューションの種類)
2. [開発環境セットアップ](#開発環境セットアップ)
3. [開発ワークフロー](#開発ワークフロー)
4. [コーディング規約](#コーディング規約)
5. [プルリクエストプロセス](#プルリクエストプロセス)
6. [レビュープロセス](#レビュープロセス)
7. [リリースプロセス](#リリースプロセス)

---

## 🎯 コントリビューションの種類

### コードコントリビューション
- **新機能開発**: 新しい Agent、ツール、プロトコルの実装
- **バグ修正**: 既存の不具合修正とテスト追加
- **パフォーマンス改善**: 最適化と効率化
- **リファクタリング**: コード品質改善と技術的負債解消

### 非コードコントリビューション
- **ドキュメント**: ガイド、チュートリアル、API リファレンス
- **テスト**: テストケース追加と品質向上
- **翻訳**: 多言語対応（英語、日本語、中文）
- **デザイン**: UI/UX 改善提案
- **バグ報告**: 詳細な再現手順付き Issue

### コミュニティサポート
- **質問回答**: GitHub Discussions での技術サポート
- **コードレビュー**: プルリクエストのレビュー参加
- **メンタリング**: 新規コントリビューターの指導
- **イベント**: ミートアップ、ワークショップの企画

---

## 🛠️ 開発環境セットアップ

### 前提条件
```bash
# Python バージョン確認
python --version  # 3.13+ 必須

# Git バージョン確認
git --version     # 2.0+ 推奨

# Docker バージョン確認（オプション）
docker --version  # 開発環境用
```

### リポジトリクローン
```bash
# HTTPS でクローン
git clone https://github.com/liushuang393/serverlessAIAgents.git
cd serverlessAIAgents

# または SSH でクローン（推奨）
git clone git@github.com:liushuang393/serverlessAIAgents.git
cd serverlessAIAgents

# アップストリーム設定（フォークした場合）
git remote add upstream https://github.com/liushuang393/serverlessAIAgents.git
```

### Conda 環境セットアップ（推奨）
```bash
# Conda 環境作成
conda create -n agentflow python=3.13 -y
conda activate agentflow

# 開発用依存関係インストール
pip install -e ".[dev]"

# インストール確認
agentflow --version
python -c "import agentflow; print(agentflow.__version__)"
```

### 仮想環境セットアップ（Conda なし）
```bash
# 仮想環境作成
python -m venv .venv

# アクティベート（Linux/Mac）
source .venv/bin/activate

# アクティベート（Windows）
.venv\Scripts\activate

# 依存関係インストール
pip install -e ".[dev]"
```

### Pre-commit フックインストール
```bash
# pre-commit インストール
pip install pre-commit

# フックインストール
pre-commit install

# 全ファイルチェック（初回実行時）
pre-commit run --all-files
```

### IDE 設定
```json
// .vscode/settings.json（VS Code 推奨）
{
    "python.defaultInterpreterPath": "./.venv/bin/python",
    "python.linting.enabled": true,
    "python.linting.ruffEnabled": true,
    "python.formatting.provider": "ruff",
    "editor.formatOnSave": true,
    "editor.codeActionsOnSave": {
        "source.fixAll.ruff": true
    },
    "python.testing.pytestEnabled": true,
    "python.testing.pytestArgs": [
        "--cov=agentflow",
        "--cov-report=html"
    ]
}
```

---

## 🔄 開発ワークフロー

### ブランチ戦略
```bash
# 機能ブランチ作成
git checkout -b feature/your-feature-name

# バグ修正ブランチ作成
git checkout -b fix/issue-number-description

# リファクタリングブランチ作成
git checkout -b refactor/component-name

# ドキュメントブランチ作成
git checkout -b docs/update-guide-name
```

### コミット規約
```bash
# コミットメッセージ形式
git commit -m "feat: 新しいワークフローエンジンを実装

- 非同期タスク実行をサポート
- エラーハンドリングを改善
- パフォーマンス監視を追加

Closes #123"

# コミットタイプ
feat:     新機能
fix:      バグ修正
docs:     ドキュメント変更
test:     テスト追加・変更
refactor: コードリファクタリング
perf:     パフォーマンス改善
chore:    メンテナンスタスク
ci:       CI/CD 変更
```

### 品質チェック実行
```bash
# 全チェック実行（推奨）
./check.sh all

# 個別チェック
./check.sh format    # コードフォーマット
./check.sh lint      # リントチェック
./check.sh type      # 型チェック
./check.sh test      # テスト実行
./check.sh test-cov  # カバレッジ付きテスト

# Windows PowerShell
.\check.ps1 all

# Windows CMD
check.bat all
```

---

## 💻 コーディング規約

### 必須遵守事項
- **型アノテーション 100% カバレッジ**
- **Google スタイル Docstring**
- **async/await パターン使用**
- **SOLID 原則遵守**
- **テスト駆動開発**

### コード変更時のチェックリスト
```python
# ✅ 新機能実装時のチェックリスト
class NewFeatureImplementation:
    """新しい機能を実装する際のチェックポイント。"""

    def __init__(self):
        self.requirements_gathered = False  # 要件定義完了
        self.design_reviewed = False        # 設計レビュー済み
        self.tests_written = False          # テスト先行実装
        self.code_implemented = False       # コード実装完了
        self.docs_updated = False           # ドキュメント更新
        self.integration_tested = False     # 統合テスト完了

    async def implement_feature(self, feature_spec: dict) -> dict:
        """機能を段階的に実装。

        1. 要件分析と設計
        2. テスト駆動開発（TDD）
        3. コード実装とリファクタリング
        4. 統合テスト
        5. ドキュメント更新
        """
        # 実装プロセス...
        pass
```

### テスト実装パターン
```python
# ✅ TDD サイクル実装例
import pytest
from agentflow.workflows import WorkflowEngine

class TestWorkflowEngine:
    """WorkflowEngine のテストスイート。"""

    @pytest.fixture
    def engine(self) -> WorkflowEngine:
        """テスト用エンジンインスタンス。"""
        return WorkflowEngine()

    def test_workflow_creation(self, engine: WorkflowEngine):
        """ワークフロー作成機能をテスト。"""
        # Given: ワークフロー定義
        workflow_def = {
            "name": "test-workflow",
            "steps": [{"id": "step1", "action": "log"}]
        }

        # When: ワークフロー作成
        workflow = engine.create_workflow(workflow_def)

        # Then: 作成されたワークフローを検証
        assert workflow.name == "test-workflow"
        assert len(workflow.steps) == 1

    async def test_workflow_execution(self, engine: WorkflowEngine):
        """ワークフロー実行機能をテスト。"""
        # Given: 作成されたワークフロー
        workflow = engine.create_workflow({
            "name": "test-workflow",
            "steps": [{"id": "step1", "action": "log", "message": "Hello"}]
        })

        # When: ワークフロー実行
        result = await engine.execute_workflow(workflow.id, {})

        # Then: 実行結果を検証
        assert result["status"] == "completed"
        assert "logs" in result
```

---

## 🔄 プルリクエストプロセス

### PR 作成前の準備
```bash
# 最新のメインブランチを取得
git checkout main
git pull upstream main

# 機能ブランチを最新化
git checkout feature/your-feature
git rebase main

# 競合が発生した場合の解決
# 1. 競合を解決
# 2. git add <resolved-files>
# 3. git rebase --continue
# 4. テスト実行で検証
```

### PR 説明テンプレート
```markdown
## 🎯 概要
[変更の簡単な説明 - 何を実装/修正したか]

## 🔧 変更内容
### 実装した機能
- [ ] 新しい Agent クラスを実装
- [ ] ワークフロー実行ロジックを改善
- [ ] エラーハンドリングを強化

### 技術的変更
- [ ] 非同期処理を導入
- [ ] 型アノテーションを追加
- [ ] テストカバレッジを向上

## 🧪 テスト
- [ ] ユニットテストを追加/更新
- [ ] 統合テストを追加/更新
- [ ] 手動テストを実施
- [ ] パフォーマンステストを実施

## 📚 ドキュメント
- [ ] コードドキュメントを更新
- [ ] README を更新（必要な場合）
- [ ] API ドキュメントを更新

## 🔍 レビューポイント
- [ ] セキュリティ上の懸念はないか
- [ ] パフォーマンスに影響はないか
- [ ] 後方互換性を維持しているか
- [ ] エラーハンドリングは適切か

## 🔗 関連 Issue
Closes #123
Related to #456

## 🚀 デプロイ・移行
[必要な移行作業やデプロイ時の注意事項]

---
**チェックリスト確認済み**: [ ] コード品質 | [ ] テスト | [ ] ドキュメント | [ ] セキュリティ
```

### PR サイズガイドライン
```python
# ✅ 小さな PR を推奨
class PRSizeGuidelines:
    """PR サイズの目安。"""

    RECOMMENDED = {
        "lines_changed": "< 200",      # 変更行数
        "files_changed": "< 10",       # 変更ファイル数
        "review_time": "< 30min",      # レビュー時間
        "merge_time": "< 1day"         # マージまで時間
    }

    MAXIMUM = {
        "lines_changed": "< 1000",     # 最大変更行数
        "files_changed": "< 50",       # 最大変更ファイル数
    }

    # 大規模変更の場合は分割を検討
    LARGE_CHANGE_STRATEGIES = [
        "機能ごとに別 PR 作成",
        "段階的ロールアウト",
        "フィーチャートグル使用",
        "バックワードコンパチビリティ維持"
    ]
```

---

## 👀 レビュープロセス

### レビュアーの役割
```python
class CodeReviewer:
    """コードレビュアーのチェックポイント。"""

    CHECKPOINTS = {
        "機能的正確性": [
            "要件を満たしているか",
            "エッジケースを処理しているか",
            "エラーハンドリングが適切か"
        ],

        "コード品質": [
            "SOLID原則を遵守しているか",
            "型安全性が確保されているか",
            "パフォーマンスが最適化されているか",
            "セキュリティベストプラクティスを守っているか"
        ],

        "テスト品質": [
            "テストカバレッジが十分か",
            "重要なパスがテストされているか",
            "モック/スタブが適切に使用されているか"
        ],

        "保守性": [
            "コードが読みやすいか",
            "ドキュメントが充実しているか",
            "技術的負債を増やしていないか"
        ]
    }

    def review_pr(self, pr_number: int) -> ReviewResult:
        """PR をレビューする。"""
        # 自動チェック結果確認
        # 手動コードレビュー実施
        # テスト実行確認
        # ドキュメント確認
        pass
```

### レビューコメントテンプレート
```markdown
## 🔍 レビューコメント

### ✅ 良い点
- [明確で読みやすいコード構造]
- [適切なエラーハンドリング]
- [包括的なテストカバレッジ]

### 💡 改善提案
**ファイル**: `agentflow/workflows/engine.py:45`
```python
# 現在のコード
if user.role == "admin":

# 提案
if user.has_permission("admin_action"):
```
**理由**: 権限チェックをメソッド化することで、再利用性と保守性が向上します。

### ❓ 確認事項
1. **セキュリティ**: このエンドポイントは認証が必要か？
2. **パフォーマンス**: 大量データ処理時にメモリリークの可能性はないか？
3. **互換性**: 既存の API 利用者に影響はないか？

### 🚫 修正必須事項
1. **重大なバグ**: `null` ポインタ参照の可能性あり
2. **セキュリティ脆弱性**: SQL インジェクションのリスク
3. **型エラー**: 戻り値の型アノテーションが不正確

### 🏷️ ラベル提案
- `bug-fix` - バグ修正
- `enhancement` - 機能改善
- `documentation` - ドキュメント更新
- `breaking-change` - 破壊的変更
```

### レビュアー割り当て
```python
# レビュアー割り当てルール
REVIEWER_ASSIGNMENT = {
    "backend_core": ["@core-team-member1", "@core-team-member2"],
    "frontend_ui": ["@ui-team-member1", "@ui-team-member2"],
    "security": ["@security-officer"],
    "performance": ["@performance-engineer"],
    "documentation": ["@tech-writer"],
}

# 自動割り当てロジック
def assign_reviewers(pr_labels: list[str], pr_size: str) -> list[str]:
    """PR に適したレビュアーを割り当てる。"""
    reviewers = []

    # ラベルに基づく割り当て
    for label in pr_labels:
        if label in REVIEWER_ASSIGNMENT:
            reviewers.extend(REVIEWER_ASSIGNMENT[label])

    # 大規模 PR の場合、上級レビュアーを追加
    if pr_size == "large":
        reviewers.append("@senior-reviewer")

    # 重複除去
    return list(set(reviewers))
```

---

## 🚀 リリースプロセス

### リリース準備
```bash
# リリースブランチ作成
git checkout -b release/v1.2.3

# バージョン更新
echo "1.2.3" > agentflow/__init__.py

# 変更履歴更新
# CHANGELOG.md に新しいバージョンのエントリを追加

# コミットとタグ作成
git add .
git commit -m "Release v1.2.3"
git tag -a v1.2.3 -m "Release version 1.2.3"

# プッシュ
git push origin release/v1.2.3
git push origin v1.2.3
```

### GitHub Release 作成
```markdown
## Release v1.2.3

### 🎉 新機能
- 新しいワークフローエンジンを実装
- 非同期タスク実行をサポート
- パフォーマンス監視機能を追加

### 🐛 バグ修正
- メモリリーク問題を修正
- エラーハンドリングを改善
- 型チェックエラーを解消

### 📚 ドキュメント
- API リファレンスを更新
- インストールガイドを改善
- チュートリアルを追加

### 🔄 内部変更
- 依存関係を更新
- CI/CD パイプラインを改善
- テストカバレッジを向上

### 🙏 コントリビューター
@contributor1, @contributor2, @contributor3

### 📦 インストール
```bash
pip install agentflow==1.2.3
```

### 🔗 移行ガイド
[破壊的変更がある場合は移行ガイドへのリンク]

---
**チェックサム**: `sha256:abc123...`
```

### リリース後フォローアップ
```python
class ReleaseFollowup:
    """リリース後のフォローアップタスク。"""

    async def post_release_tasks(self, version: str):
        """リリース後のタスクを実行。"""
        tasks = [
            self._update_docker_images(version),
            self._deploy_to_staging(version),
            self._run_integration_tests(),
            self._monitor_error_rates(),
            self._send_release_notifications(),
            self._update_documentation_sites(),
        ]

        for task in tasks:
            try:
                await task
                print(f"✅ {task.__name__} completed")
            except Exception as e:
                print(f"❌ {task.__name__} failed: {e}")
                # 失敗したタスクを記録して後で対応

    async def _monitor_post_release_health(self, version: str, days: int = 7):
        """リリース後の健全性を監視。"""
        # エラーレート監視
        # パフォーマンス監視
        # ユーザーからのフィードバック収集
        pass
```

---

## 📞 サポートと連絡

### コントリビューションの始め方
1. **リポジトリを探索**: 既存の Issue と PR を確認
2. **質問する**: GitHub Discussions で質問
3. **小さな変更から始める**: ドキュメント修正やテスト追加
4. **メンターを探す**: 経験豊富なコントリビューターに相談

### コミュニケーションガイドライン
- **敬意を払う**: すべてのコントリビューターに敬意を持って接する
- **建設的なフィードバック**: 改善点を具体的に指摘
- **オープンな議論**: 技術的決定を透明性を持って行う
- **感謝の表明**: コントリビューションに感謝する

### コミュニティリソース
- **GitHub Issues**: バグ報告と機能リクエスト
- **GitHub Discussions**: 一般的な議論と質問
- **Discord/Slack**: リアルタイムコミュニケーション
- **メーリングリスト**: 重要な発表と更新

---

## 🎉 コントリビューションの表彰

### コントリビューター表彰制度
```python
class ContributorRecognition:
    """コントリビューター表彰システム。"""

    BADGES = {
        "first-pr": "最初のプルリクエスト",
        "bug-hunter": "10件以上のバグ修正",
        "feature-champion": "5件以上の機能実装",
        "reviewer": "20件以上のレビュー",
        "mentor": "新規コントリビューターの指導",
        "documentation-hero": "包括的なドキュメント貢献",
        "security-guardian": "セキュリティ関連貢献",
    }

    MILESTONES = {
        1: "Welcome Contributor 🎉",
        10: "Active Contributor ⭐",
        50: "Core Contributor 🏆",
        100: "Legendary Contributor 👑",
    }

    def award_badges(self, contributor: str, achievements: list[str]):
        """コントリビューターにバッジを授与。"""
        for achievement in achievements:
            if achievement in self.BADGES:
                print(f"🏆 {contributor} earned: {self.BADGES[achievement]}")

    def check_milestones(self, contributor: str, contribution_count: int):
        """マイルストーン達成をチェック。"""
        for count, title in self.MILESTONES.items():
            if contribution_count == count:
                print(f"🎊 {contributor} reached milestone: {title}")
```

---

## 📚 関連ドキュメント

- [**開発規約**](../../../CONTRIBUTING.md) - 開発環境セットアップ
- [**コーディング規約**](coding-standards.md) - コード品質基準
- [**テスト規約**](testing-standards.md) - テスト作成ガイドライン
- [**リリース管理**](release-management.md) - リリースプロセス

---

**あなたのコントリビューションが AgentFlow をより良くします！** 🚀

*最終更新: 2025-01-19 | バージョン: 1.0.0*