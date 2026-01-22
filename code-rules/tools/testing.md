# テスト規約

> **バージョン**: 1.0.0
> **適用範囲**: AgentFlow Framework 全テストコード
> **最終更新**: 2025-01-19

## 📋 目次

1. [テスト原則](#テスト原則)
2. [テスト構造](#テスト構造)
3. [テスト命名](#テスト命名)
4. [テストパターン](#テストパターン)
5. [モックとスタブ](#モックとスタブ)
6. [テスト実行](#テスト実行)
7. [カバレッジ要件](#カバレッジ要件)

---

## 🎯 テスト原則

### テスト駆動開発 (TDD)
```python
# ✅ TDD サイクル: Red → Green → Refactor

# 1. Red: 失敗するテストを書く
def test_user_creation_validates_email():
    """ユーザ作成時にメールアドレスが検証されることをテスト。"""
    service = UserService()

    # 無効なメールアドレスで例外が発生することを期待
    with pytest.raises(ValueError, match="Invalid email"):
        await service.create_user({"email": "invalid-email"})

# 2. Green: テストを通す最小限の実装
class UserService:
    async def create_user(self, data: dict) -> User:
        email = data.get("email", "")
        if "@" not in email:
            raise ValueError("Invalid email")
        return User(email=email)

# 3. Refactor: コードを改善（テストは通ったまま）
class UserService:
    def __init__(self, validator: EmailValidator | None = None):
        self.validator = validator or EmailValidator()

    async def create_user(self, data: dict) -> User:
        email = data.get("email", "")
        if not self.validator.is_valid(email):
            raise ValueError("Invalid email")
        return User(email=email)
```

### テストの品質基準
- **信頼性**: 同じ条件で常に同じ結果
- **保守性**: 変更に対する耐性
- **高速性**: 迅速なフィードバック
- **独立性**: 他のテストに依存しない

---

## 🏗️ テスト構造

### ディレクトリ構造
```
tests/
├── unit/                    # ユニットテスト（高速、独立）
│   ├── test_agent_block.py
│   ├── test_workflow_engine.py
│   └── test_providers.py
├── integration/             # 統合テスト（低速、依存関係あり）
│   ├── test_workflow_execution.py
│   ├── test_agent_coordination.py
│   └── test_database_integration.py
├── e2e/                     # エンドツーエンドテスト
│   ├── test_complete_workflow.py
│   └── test_user_journey.py
├── fixtures/                # テストフィクスチャ
│   ├── sample_workflows.py
│   └── mock_agents.py
├── conftest.py             # pytest 設定と共有フィクスチャ
└── utils/                  # テストユーティリティ
    ├── assertion_helpers.py
    └── test_data_factory.py
```

### テストファイル命名
```python
# ✅ 正しい: test_ で始まり、テスト対象を明確に
tests/unit/test_agent_block.py          # AgentBlock のユニットテスト
tests/unit/test_workflow_engine.py      # WorkflowEngine のユニットテスト
tests/integration/test_database_integration.py  # DB統合テスト

# ❌ 間違い: 命名が不明確
tests/unit/agent_tests.py              # 何のテストか不明
tests/feature/workflow.py              # テストファイルとして不明確
```

---

## 📝 テスト命名

### テスト関数命名
```python
# ✅ 正しい: test_ + メソッド名 + _ + シナリオ
def test_execute_workflow_success():
    """正常系のワークフロー実行をテスト。"""

def test_execute_workflow_with_invalid_input():
    """無効な入力でのワークフロー実行をテスト。"""

def test_execute_workflow_timeout():
    """タイムアウト時のワークフロー実行をテスト。"""

def test_create_user_with_duplicate_email():
    """重複メールアドレスでのユーザー作成をテスト。"""

# ❌ 間違い: 命名が不明確または冗長
def test_execute():                    # 何をテストするか不明
def test_workflow_execution_success(): # test_execute_workflow_success で十分
def test_user_creation():             # 具体的なシナリオを指定
```

### テストクラス命名
```python
# ✅ 正しい: Test + クラス名
class TestAgentBlock:
    """AgentBlock のテストスイート。"""

class TestWorkflowEngine:
    """WorkflowEngine のテストスイート。"""

class TestUserService:
    """UserService のテストスイート。"""

# パラメータ化テストの場合
class TestEmailValidation:
    """メールアドレス検証のテストスイート。"""

    @pytest.mark.parametrize("email,expected", [
        ("user@example.com", True),
        ("invalid-email", False),
        ("", False),
    ])
    def test_email_validation(self, email: str, expected: bool):
        validator = EmailValidator()
        assert validator.is_valid(email) == expected
```

---

## 🧪 テストパターン

### Arrange-Act-Assert パターン
```python
class TestWorkflowEngine:
    """WorkflowEngine のテスト。"""

    @pytest.fixture
    def engine(self) -> WorkflowEngine:
        """テスト用の WorkflowEngine インスタンス。"""
        return WorkflowEngine()

    async def test_execute_workflow_success(
        self,
        engine: WorkflowEngine
    ) -> None:
        """正常系のワークフロー実行をテスト。"""
        # Arrange: テストデータの準備
        workflow = create_test_workflow()
        inputs = {"param1": "value1"}

        # Act: テスト対象の実行
        result = await engine.execute(workflow, inputs)

        # Assert: 結果の検証
        assert result["status"] == "success"
        assert "execution_time" in result
        assert result["execution_time"] > 0
```

### Given-When-Then パターン
```python
async def test_user_registration_process():
    """
    ユーザー登録プロセスのテスト。

    Given: 有効なユーザー情報
    When: ユーザー登録を実行
    Then: ユーザーが作成され、確認メールが送信される
    """
    # Given: 有効なユーザー情報
    user_data = {
        "email": "test@example.com",
        "name": "Test User",
        "password": "secure_password"
    }
    service = UserService()

    # When: ユーザー登録を実行
    user = await service.register_user(user_data)

    # Then: ユーザーが作成され、確認メールが送信される
    assert user.email == user_data["email"]
    assert user.name == user_data["name"]
    assert user.is_active is False  # メール確認待ち

    # メール送信が呼ばれたことを検証
    # (モックを使用)
```

### テーブル駆動テスト
```python
class TestInputValidation:
    """入力検証のテスト。"""

    @pytest.mark.parametrize("input_value,expected_valid", [
        # (入力値, 期待される結果)
        ("valid@email.com", True),
        ("invalid-email", False),
        ("", False),
        ("a@b.c", True),
        ("user@subdomain.domain.com", True),
        ("user@", False),
        ("@domain.com", False),
    ])
    def test_email_validation(self, input_value: str, expected_valid: bool):
        """メールアドレス検証をテーブル駆動でテスト。"""
        validator = EmailValidator()
        assert validator.is_valid(input_value) == expected_valid

    @pytest.mark.parametrize("password,expected_score", [
        ("weak", 1),
        ("password123", 2),
        ("StrongPass123!", 5),
        ("", 0),
    ])
    def test_password_strength(self, password: str, expected_score: int):
        """パスワード強度チェックをテスト。"""
        checker = PasswordStrengthChecker()
        assert checker.get_score(password) == expected_score
```

---

## 🤖 モックとスタブ

### ユニットテストでのモック
```python
from unittest.mock import AsyncMock, MagicMock

class TestAgentCoordinator:
    """AgentCoordinator のテスト。"""

    async def test_coordinate_agents_with_mocked_dependencies(self):
        """モックを使用したエージェント調整テスト。"""
        # 依存関係のモック作成
        mock_agent1 = AsyncMock()
        mock_agent1.execute.return_value = {"result": "agent1_output"}

        mock_agent2 = AsyncMock()
        mock_agent2.execute.return_value = {"result": "agent2_output"}

        mock_monitor = MagicMock()
        mock_monitor.start_monitoring.return_value = None

        # テスト対象の作成
        coordinator = AgentCoordinator(
            agents=[mock_agent1, mock_agent2],
            monitor=mock_monitor
        )

        # 実行
        task = {"type": "parallel_execution"}
        result = await coordinator.execute(task)

        # 検証
        mock_agent1.execute.assert_called_once_with(task)
        mock_agent2.execute.assert_called_once_with(task)
        mock_monitor.start_monitoring.assert_called_once()

        assert result["results"] == ["agent1_output", "agent2_output"]
```

### 外部サービスのリモック
```python
import pytest
from httpx import AsyncClient
from respx import MockRouter

class TestAPIService:
    """API サービス統合テスト。"""

    @pytest.fixture
    def mock_router(self):
        """HTTP リクエストをモックするルーター。"""
        with MockRouter() as router:
            # GET /api/users エンドポイントのモック
            router.get("https://api.example.com/users").respond(
                json={
                    "users": [
                        {"id": 1, "name": "User 1"},
                        {"id": 2, "name": "User 2"}
                    ]
                }
            )

            # POST /api/users エンドポイントのモック
            router.post("https://api.example.com/users").respond(
                status_code=201,
                json={"id": 3, "name": "New User"}
            )

            yield router

    async def test_fetch_users(self, mock_router: MockRouter):
        """ユーザー取得 API のテスト。"""
        service = APIService(base_url="https://api.example.com")

        users = await service.fetch_users()

        assert len(users) == 2
        assert users[0]["name"] == "User 1"

    async def test_create_user(self, mock_router: MockRouter):
        """ユーザー作成 API のテスト。"""
        service = APIService(base_url="https://api.example.com")

        new_user = await service.create_user({"name": "New User"})

        assert new_user["id"] == 3
        assert new_user["name"] == "New User"
```

### フィクスチャの使用
```python
# conftest.py
import pytest
from agentflow.core import WorkflowEngine
from agentflow.agents import SimpleAgent

@pytest.fixture
def sample_workflow():
    """テスト用のサンプルワークフロー。"""
    return {
        "id": "test-workflow",
        "name": "Test Workflow",
        "steps": [
            {"id": "step1", "agent": "validator", "inputs": ["data"]},
            {"id": "step2", "agent": "processor", "inputs": ["validated_data"]}
        ]
    }

@pytest.fixture
async def workflow_engine():
    """テスト用の WorkflowEngine インスタンス。"""
    engine = WorkflowEngine()
    await engine.initialize()
    yield engine
    await engine.cleanup()

@pytest.fixture
def mock_agent():
    """モックエージェント。"""
    agent = AsyncMock(spec=SimpleAgent)
    agent.execute.return_value = {"status": "success", "result": "mocked"}
    return agent

# テストでの使用
class TestWorkflowExecution:
    async def test_workflow_execution_with_fixtures(
        self,
        workflow_engine: WorkflowEngine,
        sample_workflow: dict,
        mock_agent: AsyncMock
    ):
        """フィクスチャを使用したワークフロー実行テスト。"""
        # モックエージェントをエンジンに登録
        await workflow_engine.register_agent("validator", mock_agent)

        # ワークフロー実行
        result = await workflow_engine.execute_workflow(sample_workflow)

        # 検証
        assert result["status"] == "success"
        mock_agent.execute.assert_called()
```

---

## ▶️ テスト実行

### pytest 設定
```python
# pytest.ini または pyproject.toml
[tool:pytest]
testpaths = ["tests"]
python_files = ["test_*.py", "*_test.py"]
python_classes = ["Test*"]
python_functions = ["test_*"]
addopts = [
    "--strict-markers",
    "--strict-config",
    "--verbose",
    "--tb=short",
    "--cov=agentflow",
    "--cov-report=term-missing",
    "--cov-report=html",
    "--cov-fail-under=80",
]
markers = [
    "unit: ユニットテスト",
    "integration: 統合テスト",
    "e2e: エンドツーエンドテスト",
    "slow: 遅いテスト",
    "external: 外部依存関係が必要",
]
```

### テスト実行コマンド
```bash
# 全テスト実行
pytest

# 特定のテストファイル実行
pytest tests/unit/test_agent_block.py

# 特定のテスト関数実行
pytest tests/unit/test_agent_block.py::TestAgentBlock::test_initialization

# マーカー付きテスト実行
pytest -m "unit and not slow"

# カバレッジ付き実行
pytest --cov=agentflow --cov-report=html

# 並列実行（pytest-xdist が必要）
pytest -n auto

# 失敗時デバッガー起動
pytest --pdb

# 詳細出力
pytest -v -s
```

### CI/CD 統合
```yaml
# .github/workflows/test.yml
name: Test
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Set up Python
        uses: actions/setup-python@v4
        with:
          python-version: "3.13"

      - name: Install dependencies
        run: |
          python -m pip install --upgrade pip
          pip install -e ".[dev]"

      - name: Run tests with coverage
        run: |
          pytest --cov=agentflow --cov-report=xml

      - name: Upload coverage to Codecov
        uses: codecov/codecov-action@v3
        with:
          file: ./coverage.xml
```

---

## 📊 カバレッジ要件

### カバレッジ目標
- **ユニットテスト**: ≥ 80% コードカバレッジ
- **統合テスト**: すべてのコアワークフローをカバー
- **エンドツーエンドテスト**: 主要なユーザーシナリオをカバー

### カバレッジ除外
```toml
# pyproject.toml
[tool.coverage.run]
source = ["agentflow"]
omit = [
    "*/tests/*",
    "*/migrations/*",
    "*/venv/*",
    "*/__pycache__/*",
    "agentflow/cli/__main__.py",  # CLI エントリーポイント
]

[tool.coverage.report]
exclude_lines = [
    "pragma: no cover",
    "def __repr__",
    "raise AssertionError",
    "raise NotImplementedError",
    "if __name__ == .__main__.:",
    "class .*\bProtocol\):",
    "@(abc\.)?abstractmethod",
]
```

### カバレッジレポート分析
```python
# カバレッジレポートの確認
pytest --cov=agentflow --cov-report=html
# htmlcov/index.html をブラウザで開く

# 未カバー行の確認
pytest --cov=agentflow --cov-report=term-missing

# カバレッジ閾値チェック
pytest --cov=agentflow --cov-fail-under=80
```

### カバレッジ改善戦略
```python
# 1. テストされていないコードの特定
pytest --cov=agentflow --cov-report=html
# 未カバーの行を確認

# 2. テストケースの追加
def test_edge_case_not_covered():
    """未カバーだったエッジケースのテスト。"""
    # テストコード

# 3. 不要コードの削除またはリファクタリング
# 未カバーコードが不要であれば削除
# またはテストを追加してカバレッジを上げる

# 4. カバレッジレポートの再確認
pytest --cov=agentflow --cov-report=term-missing
```

---

## 🔧 テストユーティリティ

### アサーション補助関数
```python
# tests/utils/assertion_helpers.py
from typing import Any, Dict

def assert_workflow_result(result: Dict[str, Any]) -> None:
    """ワークフロー結果の共通アサーション。"""
    assert isinstance(result, dict)
    assert "status" in result
    assert result["status"] in ["success", "failed", "running"]
    if result["status"] == "success":
        assert "result" in result
        assert "execution_time" in result

def assert_user_data(user: Dict[str, Any]) -> None:
    """ユーザーデータの共通アサーション。"""
    assert isinstance(user, dict)
    assert "id" in user
    assert "email" in user
    assert "name" in user
    assert "@" in user["email"]  # 基本的なメール形式チェック
```

### テストデータファクトリ
```python
# tests/utils/test_data_factory.py
from typing import Dict, Any
import uuid

def create_test_user(overrides: Dict[str, Any] = None) -> Dict[str, Any]:
    """テスト用ユーザー作成。"""
    user = {
        "id": str(uuid.uuid4()),
        "email": f"test_{uuid.uuid4().hex[:8]}@example.com",
        "name": "Test User",
        "is_active": True,
        "created_at": "2024-01-01T00:00:00Z"
    }
    if overrides:
        user.update(overrides)
    return user

def create_test_workflow(name: str = "test-workflow") -> Dict[str, Any]:
    """テスト用ワークフロー作成。"""
    return {
        "id": str(uuid.uuid4()),
        "name": name,
        "description": f"Test workflow: {name}",
        "steps": [
            {
                "id": "step1",
                "agent": "validator",
                "inputs": ["data"],
                "outputs": ["validated_data"]
            }
        ],
        "created_at": "2024-01-01T00:00:00Z"
    }
```

---

## ✅ テスト品質チェック

### テストコード品質チェックリスト
- [ ] テスト関数名が明確でシナリオを表現している
- [ ] Arrange-Act-Assert パターンが使用されている
- [ ] モック/スタブが適切に使用されている
- [ ] テストが独立していて他のテストに依存しない
- [ ] エッジケースとエラーハンドリングがテストされている
- [ ] テストデータが現実的で有効
- [ ] 非同期コードが適切にテストされている

### テスト実行チェックリスト
- [ ] `pytest` で全テストが通る
- [ ] カバレッジが 80% 以上
- [ ] テストが高速に実行される（ユニットテスト < 100ms）
- [ ] 外部依存関係のモックが適切
- [ ] CI/CD でテストが自動実行される

---

## 📚 関連ドキュメント

- [**コーディング規約**](coding-standards.md) - コード品質基準
- [**アーキテクチャ設計**](architecture-guidelines.md) - 設計原則
- [**コードレビューチェックリスト**](code-review-guidelines.md) - レビュー基準
- [**CI/CDパイプライン**](../../../.github/workflows/test.yml) - 自動テスト設定

---

**優れたテストは、コードの信頼性と保守性を保証します。** 🧪

*最終更新: 2025-01-19 | バージョン: 1.0.0*