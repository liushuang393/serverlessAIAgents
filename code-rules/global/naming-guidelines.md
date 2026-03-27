# 命名規約

> **バージョン**: 1.0.0
> **適用範囲**: AgentFlow Framework 全コードベース
> **最終更新**: 2025-01-19

## 📋 目次

1. [一般原則](#一般原則)
2. [変数と関数](#変数と関数)
3. [クラスと型](#クラスと型)
4. [モジュールとパッケージ](#モジュールとパッケージ)
5. [定数](#定数)
6. [特殊パターン](#特殊パターン)
7. [命名例](#命名例)

---

## 🎯 一般原則

### 明確性優先

- **意図が明確**: 名前から用途と役割がわかる
- **英語使用**: すべて英語で記述（日本語禁止）
- **一貫性**: プロジェクト全体で統一された命名

### 長さのバランス

- **適切な長さ**: 短すぎず長すぎない
- **略語の制限**: 一般的な略語のみ使用
- **完全スペル優先**: 略語より完全スペル

### 構造的一貫性

```python
# ✅ 良い例: 関連する概念の命名パターン
class UserService:
    def get_user(self, user_id: str) -> User: ...
    def create_user(self, user_data: dict) -> User: ...
    def update_user(self, user_id: str, user_data: dict) -> User: ...

# ❌ 悪い例: 不統一な命名
class UserSvc:
    def fetchUser(self, id: str) -> User: ...
    def add_user(self, data: dict) -> User: ...
    def modifyUser(self, id: str, data: dict) -> User: ...
```

---

## 🐍 変数と関数

### 変数命名

```python
# ✅ 正しい: snake_case
user_name = "john_doe"
user_data = {"name": "John", "age": 30}
is_active = True
max_retries = 3

# ❌ 間違い: camelCase や PascalCase
userName = "john_doe"      # ❌
UserData = {...}           # ❌
isActive = True            # ❌

# ✅ 特殊変数
_ = "unused_variable"      # 慣用的に未使用変数
__version__ = "1.0.0"      # 特殊メソッド/属性
```

### 関数命名

```python
# ✅ 正しい: snake_case、動詞 + 目的語
def get_user(user_id: str) -> User:
    """指定されたユーザーを取得する。"""

def create_workflow(name: str, config: dict) -> Workflow:
    """新しいワークフローを作成する。"""

def validate_input(data: dict) -> bool:
    """入力データを検証する。"""

def process_batch(items: list[dict]) -> list[dict]:
    """アイテムのバッチ処理を実行する。"""

# ❌ 間違い: 動詞なし、または PascalCase
def user(user_id: str) -> User:           # ❌ 動詞なし
def UserData(data: dict) -> User:         # ❌ PascalCase
def processData(items: list) -> list:     # ❌ PascalCase
```

### ブール変数と関数

```python
# ✅ 正しい: is/has/can/should で始まる
is_active = True
has_permission = False
can_execute = True
should_retry = False

def is_valid_user(user_id: str) -> bool:
    """ユーザーが有効かどうかを確認する。"""

def has_required_permissions(user: User, action: str) -> bool:
    """必要な権限を持っているか確認する。"""

def can_process_request(request: dict) -> bool:
    """リクエストを処理可能か確認する。"""
```

---

## 🏗️ クラスと型

### クラス命名

```python
# ✅ 正しい: PascalCase、各単語の先頭大文字
class UserService:
    """ユーザー関連のサービスクラス。"""

class WorkflowEngine:
    """ワークフロー実行エンジン。"""

class AgentCoordinator:
    """エージェント調整クラス。"""

class DatabaseConnectionPool:
    """データベース接続プール。"""

# ❌ 間違い: snake_case や不適切な略語
class user_service:        # ❌ snake_case
class WFEngine:           # ❌ 略語
class agentCoord:         # ❌ 略語 + 混在
```

### 抽象基底クラス

```python
# ✅ 正しい: Abstract/Base で始まる
class AbstractAgent:
    """エージェントの抽象基底クラス。"""

class BaseRepository:
    """リポジトリの基底クラス。"""

class AbstractWorkflowProcessor:
    """ワークフロー処理の抽象クラス。"""
```

### 例外クラス

```python
# ✅ 正しい: Error/Exception で終わる
class ValidationError(Exception):
    """検証エラー。"""

class WorkflowNotFoundError(Exception):
    """ワークフローが見つからないエラー。"""

class AgentExecutionError(Exception):
    """エージェント実行エラー。"""
```

### プロトコルとインターフェース

```python
# ✅ 正しい: Protocol で終わる
from typing import Protocol

class AgentProtocol(Protocol):
    """エージェントプロトコル。"""

class StorageProtocol(Protocol):
    """ストレージプロトコル。"""

class MessageHandlerProtocol(Protocol):
    """メッセージハンドラープロトコル。"""
```

---

## 📦 モジュールとパッケージ

### モジュール命名

```python
# ✅ 正しい: snake_case、機能名
agentflow/
├── core/
│   ├── engine.py         # エンジン実装
│   ├── config.py         # 設定管理
│   └── exceptions.py     # 例外定義
├── services/
│   ├── user_service.py   # ユーザーサービス
│   ├── workflow_service.py  # ワークフローサービス
│   └── agent_service.py  # エージェントサービス
└── utils/
    ├── validation.py     # 検証ユーティリティ
    └── formatting.py     # フォーマットユーティリティ

# ❌ 間違い: PascalCase や略語
agentflow/
├── Core/                 # ❌ PascalCase
│   ├── Engine.py         # ❌ PascalCase
├── svcs/                 # ❌ 略語
│   ├── usr_svc.py        # ❌ 略語
└── utils/
    ├── ValidationUtils.py # ❌ PascalCase
```

### パッケージ命名

```python
# ✅ 正しい: 小文字、機能グループ
agentflow/
├── agents/               # エージェント関連
├── workflows/            # ワークフロー関連
├── protocols/            # プロトコル実装
├── providers/            # プロバイダー実装
└── integrations/         # 外部統合
```

### テストモジュール

```python
# ✅ 正しい: test_ で始まる
tests/
├── unit/
│   ├── test_engine.py
│   ├── test_config.py
│   └── test_services.py
├── integration/
│   ├── test_workflow_execution.py
│   └── test_agent_coordination.py
└── conftest.py
```

---

## 🔢 定数

### 定数命名

```python
# ✅ 正しい: UPPER_SNAKE_CASE
MAX_RETRIES = 3
DEFAULT_TIMEOUT = 30.0
DATABASE_URL = "postgresql://localhost/db"
API_VERSION = "v1.0"

# 設定定数
class Config:
    MAX_WORKFLOW_DEPTH = 10
    DEFAULT_AGENT_TIMEOUT = 60.0
    SUPPORTED_PROTOCOLS = ["mcp", "a2a", "ag-ui", "a2ui"]

# ❌ 間違い: 小文字や camelCase
max_retries = 3           # ❌ 小文字
defaultTimeout = 30.0     # ❌ camelCase
database_url = "..."      # ❌ 小文字
```

### 列挙型

```python
from enum import Enum

# ✅ 正しい: PascalCase のクラス名 + UPPER_SNAKE_CASE の値
class WorkflowStatus(Enum):
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"

class AgentType(Enum):
    SIMPLE = "simple"
    COORDINATOR = "coordinator"
    SUPERVISOR = "supervisor"
```

---

## 🎭 特殊パターン

### 非公開メンバー

```python
class AgentCoordinator:
    def __init__(self) -> None:
        self._internal_state = {}    # 保護メンバー
        self.__private_data = []     # プライベートメンバー

    def _validate_config(self) -> bool:  # 保護メソッド
        """内部設定検証。"""
        return True

    def __cleanup_resources(self) -> None:  # プライベートメソッド
        """リソースクリーンアップ。"""
        pass
```

### ファクトリー関数

```python
# ✅ 正しい: create/make で始まる
def create_agent(agent_type: str, config: dict) -> Agent:
    """指定されたタイプのエージェントを作成する。"""

def make_workflow_engine(config: dict) -> WorkflowEngine:
    """ワークフローエンジンを作成する。"""

# ❌ 間違い: ファクトリー関数がクラスに見える
def Agent(agent_type: str, config: dict) -> Agent:  # ❌
    ...
```

### コールバックとハンドラー

```python
# ✅ 正しい: 目的 + Handler/Callback/Listener
def workflow_complete_handler(workflow_id: str, result: dict) -> None:
    """ワークフロー完了ハンドラー。"""

def error_callback(error: Exception, context: dict) -> None:
    """エラーコールバック。"""

def message_listener(message: dict, sender: str) -> None:
    """メッセージリスナー。"""
```

### 型変数

```python
from typing import TypeVar, Generic

# ✅ 正しい: 単一大文字または短い名前
T = TypeVar("T")                          # 一般的な型
R = TypeVar("R")                          # 戻り値型
K = TypeVar("K")                          # キー型
V = TypeVar("V")                          # 値型

class Repository(Generic[T]):
    """ジェネリックリポジトリ。"""
```

---

## 📝 命名例

### サービスクラス

```python
class UserManagementService:
    """ユーザー管理サービス。"""

    def __init__(self, database_url: str) -> None:
        self.database_url = database_url
        self._connection_pool = None

    def get_user_by_id(self, user_id: str) -> User | None:
        """IDでユーザーを取得する。"""
        return self._execute_query(
            "SELECT * FROM users WHERE id = %s",
            (user_id,)
        )

    def create_new_user(self, user_data: dict[str, Any]) -> User:
        """新しいユーザーを作成する。"""
        return self._execute_insert(
            "INSERT INTO users (name, email) VALUES (%s, %s)",
            (user_data["name"], user_data["email"])
        )

    def _execute_query(self, query: str, params: tuple) -> Any:
        """内部クエリ実行。"""
        # 実装...
        pass

    def _execute_insert(self, query: str, params: tuple) -> Any:
        """内部挿入実行。"""
        # 実装...
        pass
```

### 設定クラス

```python
class ApplicationConfig:
    """アプリケーション設定。"""

    # データベース設定
    DATABASE_HOST = "localhost"
    DATABASE_PORT = 5432
    DATABASE_NAME = "agentflow"
    DATABASE_USER = "agentflow_user"

    # API設定
    API_HOST = "0.0.0.0"
    API_PORT = 8000
    API_VERSION = "v1.0"

    # ワークフロー設定
    MAX_WORKFLOW_DEPTH = 10
    DEFAULT_WORKFLOW_TIMEOUT = 300.0
    SUPPORTED_WORKFLOW_FORMATS = ["json", "yaml"]

    @classmethod
    def get_database_url(cls) -> str:
        """データベースURLを生成する。"""
        return (
            f"postgresql://{cls.DATABASE_USER}@{cls.DATABASE_HOST}:"
            f"{cls.DATABASE_PORT}/{cls.DATABASE_NAME}"
        )
```

### エージェント実装

```python
class DocumentAnalysisAgent:
    """文書分析エージェント。"""

    def __init__(self, model_name: str = "gpt-4") -> None:
        self.model_name = model_name
        self._analysis_history: list[dict] = []

    async def analyze_document(
        self,
        document_content: str,
        analysis_type: str = "summary"
    ) -> dict[str, Any]:
        """文書を分析する。"""
        # 分析ロジック...
        result = await self._perform_analysis(document_content, analysis_type)

        # 履歴を記録
        self._record_analysis(document_content, result)

        return result

    async def _perform_analysis(
        self,
        content: str,
        analysis_type: str
    ) -> dict[str, Any]:
        """内部分析実行。"""
        # LLM呼び出し...
        pass

    def _record_analysis(
        self,
        content: str,
        result: dict[str, Any]
    ) -> None:
        """分析履歴を記録する。"""
        self._analysis_history.append({
            "timestamp": datetime.now(),
            "content_length": len(content),
            "result": result
        })
```

---

## 🚫 禁止パターン

### 一般的な間違い

```python
# ❌ 略語の乱用
usr_mgr = UserManager()        # ❌
wf_exec = WorkflowExecutor()   # ❌

# ❌ 型情報のない変数名
data = get_user_data()         # ❌
result = process()             # ❌

# ❌ 誤解を招く名前
temp = get_permanent_data()    # ❌
user_list = get_single_user()  # ❌
```

### 言語混在禁止

```python
# ❌ 日本語変数名（完全に禁止）
ユーザー名 = "john_doe"         # ❌
データ処理 = process_data()     # ❌

# ❌ 日本語コメントのみ
def get_user(): pass           # ❌（英語コメント必須）
```

---

## 📚 関連ドキュメント

- [**コーディング規約**](coding-standards.md) - コードフォーマットと品質基準
- [**アーキテクチャ設計**](architecture-guidelines.md) - クラス設計と構造化
- [**ドキュメント規約**](documentation-standards.md) - コメントとドキュメント作成
- [**テスト規約**](testing-standards.md) - テストコードの命名

---

**明確で一貫性のある命名により、コードの可読性と保守性を向上させます。** 🎯

_最終更新: 2025-01-19 | バージョン: 1.0.0_
