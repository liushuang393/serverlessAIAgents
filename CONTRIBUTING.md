# AgentFlow への貢献

AgentFlow プロジェクトへの貢献を歓迎します！このガイドでは、プロジェクトへの貢献方法を説明します。

## 📋 目次

1. [行動規範](#行動規範)
2. [開発環境のセットアップ](#開発環境のセットアップ)
3. [開発ワークフロー](#開発ワークフロー)
4. [コーディング規約](#コーディング規約)
5. [テスト](#テスト)
6. [ドキュメント](#ドキュメント)
7. [プルリクエスト](#プルリクエスト)

## 行動規範

AgentFlow プロジェクトは、すべての貢献者に対して敬意と包括性を持って接することを期待しています。不適切な行動を発見した場合は、<team@agentflow.dev> までご報告ください。

## 開発環境のセットアップ

### 前提条件

- Python 3.13 以上
- Git
- Python の async/await の基本的な理解
- 型ヒントの知識

### 開発環境のセットアップ

```bash
# リポジトリをフォークしてクローン
git clone https://github.com/YOUR_USERNAME/serverlessAIAgents.git
cd serverlessAIAgents

# 仮想環境を作成
python -m venv .venv
source .venv/bin/activate  # Windows: .venv\Scripts\activate

# 開発用依存関係をインストール
pip install -e ".[dev]"

# pre-commit フックをインストール
pre-commit install
```

## 開発ワークフロー

### 1. ブランチを作成

```bash
# 機能ブランチを作成
git checkout -b feature/your-feature-name

# またはバグ修正ブランチ
git checkout -b fix/issue-number-description
```

### 2. 変更を加える

開発規約に厳密に従ってください：

- 型アノテーション付きコードを記述 (100% カバレッジ)
- Docstring を追加 (Google スタイル)
- 新機能にはテストを記述
- 関数は小さく、焦点を絞る
- I/O 操作には async/await を使用

### 3. 品質チェックを実行

```bash
# コードをフォーマット
ruff format .

# コードをリント
ruff check .

# 型チェック
mypy agentflow

# テストを実行
pytest

# カバレッジを確認
pytest --cov=agentflow --cov-report=term-missing
```

### 4. 変更をコミット

```bash
# 変更をステージング
git add .

# 説明的なメッセージでコミット
git commit -m "feat: 新機能 X を追加"

# またはバグ修正の場合
git commit -m "fix: issue #123 を解決"
```

**コミットメッセージ形式**:

- `feat:` - 新機能
- `fix:` - バグ修正
- `docs:` - ドキュメント変更
- `test:` - テスト追加または変更
- `refactor:` - コードリファクタリング
- `perf:` - パフォーマンス改善
- `chore:` - メンテナンスタスク

### 5. プッシュして PR を作成

```bash
# フォークにプッシュ
git push origin feature/your-feature-name

# GitHub で Pull Request を作成
```

## コーディング規約

### 型アノテーション

**必須**: すべての関数に完全な型アノテーションが必要です。

```python
# ✅ 良い例
async def process_data(
    input_data: dict[str, Any],
    *,
    timeout: float = 30.0,
) -> dict[str, Any]:
    ...

# ❌ 悪い例
async def process_data(input_data, timeout=30.0):
    ...
```

### Docstring

**必須**: すべての公開関数、クラス、メソッドに Docstring が必要です。

```python
def execute_workflow(
    workflow_id: str,
    inputs: dict[str, Any],
) -> ExecutionResult:
    """指定された入力でワークフローを実行.

    Args:
        workflow_id: ワークフローの一意識別子.
        inputs: ワークフローの入力パラメーター.

    Returns:
        ステータスと出力を含む ExecutionResult.

    Raises:
        WorkflowNotFoundError: ワークフローが存在しない場合.

    Example:
        >>> result = execute_workflow("my-workflow", {"key": "value"})
        >>> print(result.status)
        success
    """
    ...
```

### エラーハンドリング

**必須**: 具体的な例外を使用し、bare `except` は使用しない。

```python
# ✅ 良い例
try:
    result = await execute_workflow(workflow_id)
except WorkflowNotFoundError:
    logger.error(f"ワークフローが見つかりません: {workflow_id}")
    raise
except TimeoutError:
    logger.warning(f"ワークフローがタイムアウトしました: {workflow_id}")
    return default_result

# ❌ 悪い例
try:
    result = await execute_workflow(workflow_id)
except Exception:  # 広すぎる!
    pass
```

### Async/Await

**必須**: すべての I/O 操作に async/await を使用。

```python
# ✅ 良い例
async def load_config(path: str) -> dict[str, Any]:
    async with aiofiles.open(path) as f:
        content = await f.read()
    return yaml.safe_load(content)

# ❌ 悪い例
def load_config(path: str) -> dict[str, Any]:
    with open(path) as f:  # ブロッキング!
        content = f.read()
    return yaml.safe_load(content)
```

## テスト

### テスト構造

```
tests/
├── unit/           # ユニットテスト (高速、独立)
├── integration/    # 統合テスト (低速、依存関係あり)
└── conftest.py     # 共有フィクスチャ
```

### テストの記述

```python
import pytest
from agentflow.core.engine import AgentFlowEngine

class TestAgentFlowEngine:
    """AgentFlowEngine のテストスイート."""

    @pytest.fixture
    def engine(self) -> AgentFlowEngine:
        """テストエンジンインスタンスを作成."""
        return AgentFlowEngine()

    async def test_execute_workflow_success(
        self,
        engine: AgentFlowEngine,
    ) -> None:
        """ワークフロー実行の成功をテスト."""
        # Arrange
        workflow = create_test_workflow()
        engine.register_workflow(workflow)

        # Act
        result = await engine.execute(workflow.workflow_id, {})

        # Assert
        assert result.status == "success"
        assert result.error is None
```

### テストカバレッジ

- **最小**: 80% コードカバレッジ
- **目標**: 90%+ コードカバレッジ
- 実行: `pytest --cov=agentflow --cov-report=html`

## ドキュメント

### コードドキュメント

- すべての公開 API に Docstring が必要
- Google スタイルの Docstring を使用
- Docstring に例を含める

### README の更新

ユーザー向け機能に影響する変更の場合:

- README.md を更新
- 該当する場合は例を追加
- 機能リストを更新

## プルリクエストプロセス

### 提出前のチェック

- [ ] すべてのテストが通過 (`pytest`)
- [ ] コードがフォーマット済み (`ruff format .`)
- [ ] リントエラーなし (`ruff check .`)
- [ ] 型エラーなし (`mypy agentflow`)
- [ ] カバレッジ ≥ 80% (`pytest --cov`)
- [ ] ドキュメント更新済み
- [ ] CHANGELOG.md 更新済み (該当する場合)

### PR 説明テンプレート

```markdown
## 説明

変更の簡単な説明

## 変更の種類

- [ ] バグ修正
- [ ] 新機能
- [ ] 破壊的変更
- [ ] ドキュメント更新

## テスト

- [ ] ユニットテスト追加/更新
- [ ] 統合テスト追加/更新
- [ ] すべてのテストが通過

## チェックリスト

- [ ] コードがスタイルガイドラインに従っている
- [ ] セルフレビュー完了
- [ ] ドキュメント更新済み
- [ ] 新しい警告なし
```

### レビュープロセス

1. 自動チェックが通過する必要があります (CI/CD)
2. 少なくとも1人のメンテナーの承認が必要
3. すべてのレビューコメントに対応
4. マージコンフリクトなし

## 質問がありますか？

- バグ報告や機能リクエストは [GitHub Issues](https://github.com/liushuang393/serverlessAIAgents/issues) を開いてください
- 質問は [GitHub Discussions](https://github.com/liushuang393/serverlessAIAgents/discussions) を開始してください
- プライベートな問い合わせは <115070984+liushuang393@users.noreply.github.com> にメールしてください

AgentFlow への貢献ありがとうございます！ 🎉
