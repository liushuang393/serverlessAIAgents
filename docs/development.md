# 開発ガイド

AgentFlow プロジェクトへの貢献方法を説明します。

## 📋 目次

1. [開発環境のセットアップ](#開発環境のセットアップ)
2. [開発ワークフロー](#開発ワークフロー)
3. [コーディング規約](#コーディング規約)
4. [テストの書き方](#テストの書き方)
5. [プルリクエストの作成](#プルリクエストの作成)

---

## 開発環境のセットアップ

### 前提条件

- Python 3.13 以上
- Git
- pip

### セットアップ手順

```bash
# 1. リポジトリをフォーク
# GitHub で Fork ボタンをクリック

# 2. クローン
git clone https://github.com/YOUR_USERNAME/serverlessAIAgents.git
cd serverlessAIAgents

# 3. 仮想環境を作成
python -m venv .venv

# 4. 仮想環境を有効化
# Windows:
.venv\Scripts\activate
# macOS/Linux:
source .venv/bin/activate

# 5. 開発用依存関係をインストール
pip install -e ".[dev]"

# 6. pre-commit フックをインストール
pre-commit install
```

---

## 開発ワークフロー

### 1. ブランチを作成

```bash
# 機能追加の場合
git checkout -b feature/your-feature-name

# バグ修正の場合
git checkout -b fix/issue-number-description
```

### 2. コードを変更

- コーディング規約に従ってコードを記述
- 必要に応じてテストを追加
- ドキュメントを更新

### 3. 品質チェック

```bash
# コードをフォーマット
ruff format .

# リントチェック
ruff check .

# 型チェック
mypy agentflow

# テストを実行
pytest tests/ -v

# カバレッジを確認
pytest tests/ --cov=agentflow --cov-report=term-missing
```

### 4. コミット

```bash
# 変更をステージング
git add .

# コミット（コミットメッセージ規約に従う）
git commit -m "feat: 新機能を追加"
```

**コミットメッセージ規約**:
- `feat:` - 新機能
- `fix:` - バグ修正
- `docs:` - ドキュメント変更
- `test:` - テスト追加/変更
- `refactor:` - リファクタリング
- `perf:` - パフォーマンス改善
- `chore:` - その他の変更

### 5. プッシュ

```bash
# フォークにプッシュ
git push origin feature/your-feature-name
```

### 6. プルリクエストを作成

GitHub で Pull Request を作成します。

---

## コーディング規約

### 型アノテーション

**必須**: すべての関数に型アノテーションを付ける

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

**必須**: すべての公開関数、クラス、メソッドに Docstring を付ける

```python
def execute_workflow(
    workflow_id: str,
    inputs: dict[str, Any],
) -> ExecutionResult:
    """ワークフローを実行.
    
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

**必須**: 具体的な例外を使用

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
except Exception:  # 広すぎる
    pass
```

### 非同期処理

**必須**: すべての I/O 操作に async/await を使用

```python
# ✅ 良い例
async def load_config(path: str) -> dict[str, Any]:
    async with aiofiles.open(path) as f:
        content = await f.read()
    return yaml.safe_load(content)

# ❌ 悪い例
def load_config(path: str) -> dict[str, Any]:
    with open(path) as f:  # ブロッキング
        content = f.read()
    return yaml.safe_load(content)
```

---

## テストの書き方

### テスト構造

```
tests/
├── unit/           # ユニットテスト（高速、独立）
├── integration/    # 統合テスト（低速、依存関係あり）
└── conftest.py     # 共有フィクスチャ
```

### ユニットテスト

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

```bash
# カバレッジレポートを生成
pytest tests/ --cov=agentflow --cov-report=html

# ブラウザで確認
open htmlcov/index.html
```

---

## プルリクエストの作成

### 提出前のチェックリスト

- [ ] すべてのテストが通過 (`pytest`)
- [ ] コードがフォーマット済み (`ruff format .`)
- [ ] リントエラーなし (`ruff check .`)
- [ ] 型エラーなし (`mypy agentflow`)
- [ ] カバレッジ ≥ 80% (`pytest --cov`)
- [ ] ドキュメント更新済み
- [ ] CHANGELOG.md 更新済み（該当する場合）

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

1. 自動チェックが通過する必要があります（CI/CD）
2. 少なくとも 1 人のメンテナーの承認が必要
3. すべてのレビューコメントに対応
4. マージコンフリクトなし

---

## 質問がありますか？

- バグ報告や機能リクエストは [GitHub Issues](https://github.com/liushuang393/serverlessAIAgents/issues) を開いてください
- 質問は [GitHub Discussions](https://github.com/liushuang393/serverlessAIAgents/discussions) を開始してください
- プライベートな問い合わせは 115070984+liushuang393@users.noreply.github.com にメールしてください

---

詳細な貢献ガイドラインは [CONTRIBUTING.md](../CONTRIBUTING.md) を参照してください。

