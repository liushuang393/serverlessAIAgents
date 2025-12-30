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
- Conda（推奨）または Python venv
- Python の async/await の基本的な理解
- 型ヒントの知識

### ステップ 1: リポジトリのクローン

```bash
# リポジトリをフォークしてクローン
git clone https://github.com/YOUR_USERNAME/serverlessAIAgents.git
cd serverlessAIAgents

# アップストリームリポジトリを追加（オプション）
git remote add upstream https://github.com/liushuang393/serverlessAIAgents.git
```

### ステップ 2: Conda 環境のセットアップ（推奨）

```bash
# Conda 環境を作成
conda env create -f environment.yml

# 環境をアクティベート
conda activate agentflow

# 開発用依存関係をインストール
pip install -e ".[dev]"
```

### ステップ 3: 仮想環境のセットアップ（Conda を使用しない場合）

```bash
# 仮想環境を作成
python -m venv .venv

# 環境をアクティベート
# Linux/Mac:
source .venv/bin/activate
# Windows:
.venv\Scripts\activate

# 開発用依存関係をインストール
pip install -e ".[dev]"
```

### ステップ 4: Pre-commit フックのインストール

```bash
# pre-commit フックをインストール
pre-commit install

# 動作確認（全ファイルをチェック）
pre-commit run --all-files
```

### ステップ 5: インストール確認

```bash
# AgentFlow がインストールされているか確認
agentflow --version

# Python でインポート確認
python -c "import agentflow; print(agentflow.__version__)"
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

**推奨方法**: `check.sh`（Linux/Mac）または `check.ps1`/`check.bat`（Windows）を使用

```bash
# Linux/Mac: すべてのチェックを実行
./check.sh all

# Windows PowerShell: すべてのチェックを実行
.\check.ps1 all

# Windows CMD: すべてのチェックを実行
check.bat all
```

**個別のチェック**:

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

# HTML カバレッジレポートを生成
pytest --cov=agentflow --cov-report=html
# htmlcov/index.html を開いて確認
```

**check スクリプトのコマンド一覧**:

| コマンド | 説明 |
|---------|------|
| `all` | すべてのチェックを実行 |
| `format` | コードを自動フォーマット |
| `lint` | リントチェック |
| `type-check` | 型チェック |
| `test` | テストを実行 |
| `test-cov` | カバレッジ付きテスト |
| `pre-commit` | Pre-commit を実行 |
| `clean` | 一時ファイルを削除 |

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

### テストの実行

**基本的な実行**:

```bash
# すべてのテストを実行
pytest

# 特定のテストファイルを実行
pytest tests/unit/test_agent_block.py

# 特定のテスト関数を実行
pytest tests/unit/test_agent_block.py::test_agent_initialization

# 詳細な出力で実行
pytest -v

# 失敗時にデバッガーを起動
pytest --pdb
```

**カバレッジ付きテスト**:

```bash
# カバレッジレポートを表示
pytest --cov=agentflow --cov-report=term-missing

# HTML カバレッジレポートを生成
pytest --cov=agentflow --cov-report=html
# htmlcov/index.html をブラウザで開く

# カバレッジの最小値を設定
pytest --cov=agentflow --cov-fail-under=80
```

**並列実行**（高速化）:

```bash
# pytest-xdist を使用（インストールが必要）
pytest -n auto  # CPU コア数に応じて並列実行
```

### テスト構造

```
tests/
├── unit/           # ユニットテスト (高速、独立)
├── integration/    # 統合テスト (低速、依存関係あり)
└── conftest.py     # 共有フィクスチャ
```

### テストの記述ガイドライン

- **テスト名は明確に**: `test_関数名_シナリオ` の形式
- **1つのテストで1つのことをテスト**: 単一責任の原則
- **Arrange-Act-Assert パターン**: 準備、実行、検証を明確に分離
- **フィクスチャを使用**: 共通のセットアップは `conftest.py` に

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

### ステップ 1: ローカルでの準備

1. **最新のコードを取得**:

```bash
# メインブランチに切り替え
git checkout main

# アップストリームから最新を取得
git fetch upstream
git merge upstream/main

# または
git pull upstream main
```

2. **ブランチを作成**:

```bash
# 機能ブランチを作成
git checkout -b feature/your-feature-name

# またはバグ修正ブランチ
git checkout -b fix/issue-number-description
```

3. **変更を加える**:

- コードを編集
- テストを追加/更新
- ドキュメントを更新

### ステップ 2: 提出前のチェック

**必須チェックリスト**:

- [ ] すべてのテストが通過 (`pytest` または `./check.sh test`)
- [ ] コードがフォーマット済み (`ruff format .` または `./check.sh format`)
- [ ] リントエラーなし (`ruff check .` または `./check.sh lint`)
- [ ] 型エラーなし (`mypy agentflow` または `./check.sh type-check`)
- [ ] カバレッジ ≥ 80% (`pytest --cov` または `./check.sh test-cov`)
- [ ] Pre-commit チェック通過 (`pre-commit run --all-files` または `./check.sh pre-commit`)
- [ ] ドキュメント更新済み
- [ ] CHANGELOG.md 更新済み (該当する場合)

**推奨**: `check.sh all`（または `check.ps1 all`/`check.bat all`）を実行して、すべてのチェックを一度に確認

### ステップ 3: コミットとプッシュ

```bash
# 変更をステージング
git add .

# コミット（pre-commit フックが自動実行される）
git commit -m "feat: 新機能 X を追加"

# フォークにプッシュ
git push origin feature/your-feature-name
```

**注意**: Pre-commit フックが失敗した場合は、エラーを修正してから再度コミットしてください。

### ステップ 4: Pull Request の作成

1. GitHub で Pull Request を作成
2. PR 説明テンプレートに従って記入
3. 関連する Issue をリンク（該当する場合）

### ステップ 5: CI/CD チェック

- GitHub Actions が自動的にチェックを実行します
- すべてのチェックが通過する必要があります
- 失敗した場合は、ローカルで再確認して修正してください

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

## よくある質問

### Q: ローカルでテストが失敗する

**A**: 以下を確認してください：

```bash
# 環境がアクティベートされているか確認
conda activate agentflow  # または source .venv/bin/activate

# 依存関係がインストールされているか確認
pip install -e ".[dev]"

# テストを再実行
pytest
```

### Q: Pre-commit フックが遅い

**A**: 初回実行時は依存関係をダウンロードするため遅いです。2回目以降は速くなります。

```bash
# キャッシュをクリア
pre-commit clean

# 再インストール
pre-commit install --install-hooks
```

### Q: コミット時に自動チェックをスキップしたい

**A**: 緊急時のみ `--no-verify` を使用してください（**非推奨**）。

```bash
# 自動チェックをスキップ（非推奨）
git commit --no-verify -m "message"
```

**推奨**: エラーを修正してからコミットしてください。

### Q: 型エラーが解決できない

**A**: 以下を試してください：

```bash
# 型チェックの詳細を確認
mypy agentflow --show-error-codes

# 特定のエラーを無視（一時的）
# type: ignore[error-code] を追加
```

### Q: カバレッジが 80% を下回る

**A**: 新しいコードにはテストを追加してください。

```bash
# カバレッジレポートを確認
pytest --cov=agentflow --cov-report=html
# htmlcov/index.html で未カバー箇所を確認

# テストを追加
# tests/unit/ または tests/integration/ に追加
```

## サポート

- **バグ報告や機能リクエスト**: [GitHub Issues](https://github.com/liushuang393/serverlessAIAgents/issues) を開いてください
- **質問**: [GitHub Discussions](https://github.com/liushuang393/serverlessAIAgents/discussions) を開始してください
- **プライベートな問い合わせ**: <115070984+liushuang393@users.noreply.github.com> にメールしてください

## 関連ドキュメント

- [開発規範](docs/DEVELOPMENT_STANDARDS_JA.md) - 詳細なコーディング規約
- [アーキテクチャ](docs/architecture.md) - システム設計の理解
- [API リファレンス](docs/api.md) - API の詳細

---

AgentFlow への貢献ありがとうございます！ 🎉

あなたの貢献が、AgentFlow をより良いフレームワークにします。
