# アーキテクチャ設計ガイドライン

> **バージョン**: 1.0.0
> **適用範囲**: AgentFlow Framework 全アーキテクチャ設計
> **最終更新**: 2025-01-19

## 📋 目次

1. [AgentFlow 8層アーキテクチャ](#agentflow-8層アーキテクチャ)
2. [SOLID 原則](#solid-原則)
3. [依存性管理](#依存性管理)
4. [非同期設計パターン](#非同期設計パターン)
5. [プロトコル設計](#プロトコル設計)
6. [データフロー設計](#データフロー設計)
7. [3 Studio + Kernel 境界規約](#3-studio--kernel-境界規約)

---

## 🏗️ AgentFlow 8層アーキテクチャ

### アーキテクチャ概要
AgentFlow は明確な責任分離に基づく8層アーキテクチャを採用：

```
┌─────────────────────────────────────────────────────────┐
│                    📱 アプリケーション層                  │
│  ┌─────────────────────────────────────────────────────┐ │
│  │ decision_governance_engine, market_trend_monitor   │ │
│  │ code_migration_assistant, custom_apps               │ │
│  └─────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────┘
                                  │
┌─────────────────────────────────────────────────────────┐
│                   🎨 UI層                                │
│  ┌─────────────────────────────────────────────────────┐ │
│  │ Studio UI, A2UI, AG-UI                             │ │
│  │ ビジュアルエディタ、宣言式UI、リアルタイムイベント      │ │
│  └─────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────┘
                                  │
┌─────────────────────────────────────────────────────────┐
│               🔄 フロー層 - 3つの開発方式                  │
│  ┌─────────────────────────────────────────────────────┐ │
│  │ @agent (推奨), create_flow, AgentCoordinator       │ │
│  │ 簡単→複雑の開発方式選択                              │ │
│  └─────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────┘
                                  │
┌─────────────────────────────────────────────────────────┐
│                   🤖 Agent層                             │
│  ┌─────────────────────────────────────────────────────┐ │
│  │ AgentBlock基底クラス, @agentデコレータ, カスタム実装 │ │
│  │ Agent実装の3つのアプローチ                           │ │
│  └─────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────┘
                                  │
┌─────────────────────────────────────────────────────────┐
│                   🛠️ ツール層                             │
│  ┌─────────────────────────────────────────────────────┐ │
│  │ @tool, MCP Tools, Skills Engine, Built-in Skills   │ │
│  │ 外部ツール統合と自動進化システム                      │ │
│  └─────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────┘
                                  │
┌─────────────────────────────────────────────────────────┐
│               🔌 Provider層 - 統一アクセス                 │
│  ┌─────────────────────────────────────────────────────┐ │
│  │ LLMProvider, DataProvider, EventProvider           │ │
│  │ ToolProvider: 約定優先の統一インターフェース         │ │
│  └─────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────┘
                                  │
┌─────────────────────────────────────────────────────────┐
│               🌐 プロトコル層 - 4プロトコル                 │
│  ┌─────────────────────────────────────────────────────┐ │
│  │ MCP, A2A, AG-UI, A2UI                              │ │
│  │ 標準プロトコルの統合インターフェース                  │ │
│  └─────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────┘
                                  │
┌─────────────────────────────────────────────────────────┐
│                   💾 インフラ層                           │
│  ┌─────────────────────────────────────────────────────┐ │
│  │ LLM Services, Database, Vector DB, Cache           │ │
│  │ 外部サービス・データストア                          │ │
│  └─────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────┘
```

### 各層の責任

| 層 | 主要責任 | 設計原則 |
|----|---------|---------|
| **アプリケーション層** | ビジネスロジック、ユーザー要件 | ビジネス駆動、ドメイン分離 |
| **UI層** | ユーザーインターフェース、視覚化 | ユーザビリティ、アクセシビリティ |
| **フロー層** | ワークフロー調整、エージェント協調 | 宣言的プログラミング、複雑さ管理 |
| **Agent層** | Agent実装、意思決定ロジック | 単一責任、テスト容易性 |
| **ツール層** | 外部ツール統合、能力拡張 | プラグインアーキテクチャ、自動進化 |
| **Provider層** | リソース抽象化、統一アクセス | 依存性逆転、インターフェース分離 |
| **プロトコル層** | 通信標準、相互運用性 | 標準準拠、拡張性 |
| **インフラ層** | データ永続化、外部サービス | 疎結合、設定駆動 |

---

## 3 Studio + Kernel 境界規約

### 目的

- 外部向けは `Migration Studio` / `Enterprise FAQ Studio` / `Computer Assistant Studio` の 3 主線で提供する。
- Kernel は長期安定 API（ロード・実行・ポリシー・監査・実行文脈）のみを保持する。
- それ以外の能力はプラグインとして実装する。

### 必須ルール

1. コア不変要素以外の機能を `agentflow` コアへ直接追加しない。
2. 副作用を持つ操作（ファイル書き込み・ネットワーク送信・コマンド実行・ブラウザ制御）は、必ずポリシーと監査を経由する。
3. Business 面ではプロトコル詳細（MCP/A2A/stream）を UI と API から隠蔽する。
4. `app_config.json` には下記分類を明示する。
   - `product_line`: `migration|faq|assistant|framework`
   - `surface_profile`: `business|developer|operator`
   - `audit_profile`: `business|developer`

### ドキュメント運用規約

- 対外説明は `docs/external/` に集約し、成果物中心で記述する。
- 対内実装は `docs/internal/` に集約し、手順・設計思想・注意事項を記述する。
- `docs/index.md` を文書入口の単一ソースとして維持する。

### Platform API 契約規約

- Product API は `/api/studios/*` を使用する。
- Framework App 管理 API は `/api/studios/framework/apps/*` を使用する。
- 旧 `/api/apps/*` への新規記述・再導入は禁止する。

### 監査方針

- `audit_profile=developer` は従来のプロトコル面チェックを維持。
- `audit_profile=business` は A2A/stream/MCP surface の強制チェックを無効化し、安全基線のみを維持。

---

## 🏛️ SOLID 原則

### 1. 単一責任原則 (Single Responsibility)
```python
# ✅ 正しい: 各クラスが単一の責任のみを持つ
class UserValidator:
    """ユーザー入力検証のみを担当。"""
    def validate_email(self, email: str) -> bool: ...
    def validate_password(self, password: str) -> bool: ...

class UserRepository:
    """ユーザーデータ永続化のみを担当。"""
    async def save(self, user: User) -> None: ...
    async def find_by_id(self, user_id: str) -> User | None: ...

class UserService:
    """ユーザー関連ビジネスロジックのみを担当。"""
    def __init__(self, validator: UserValidator, repo: UserRepository): ...
    async def create_user(self, user_data: dict) -> User: ...

# ❌ 間違い: 単一クラスが複数の責任を持つ
class UserManager:
    """検証・永続化・ビジネスロジックを全て担当（悪い例）"""
    def validate_email(self, email: str) -> bool: ...
    async def save_to_db(self, user: User) -> None: ...
    async def create_user(self, user_data: dict) -> User: ...
    def send_welcome_email(self, user: User) -> None: ...  # 通知も担当？
```

### 2. 開放閉鎖原則 (Open/Closed)
```python
# ✅ 正しい: 拡張に開いて、修正に閉じる
from abc import ABC, abstractmethod
from typing import Protocol

class NotificationService(Protocol):
    """通知サービスのプロトコル。"""
    async def send(self, recipient: str, message: str) -> None: ...

class EmailNotificationService:
    """メール通知の実装。"""
    async def send(self, recipient: str, message: str) -> None:
        # メール送信ロジック
        pass

class SMSNotificationService:
    """SMS通知の実装。"""
    async def send(self, recipient: str, message: str) -> None:
        # SMS送信ロジック
        pass

class UserService:
    """通知サービスを注入可能。"""
    def __init__(self, notification: NotificationService): ...
```

### 3. リスコフの置換原則 (Liskov Substitution)
```python
# ✅ 正しい: サブクラスは親クラスを置き換え可能
class BaseAgent(ABC):
    """エージェント基底クラス。"""
    @abstractmethod
    async def execute(self, task: dict) -> dict:
        """タスクを実行し、結果を返す。"""
        pass

class SimpleAgent(BaseAgent):
    """シンプルなエージェント実装。"""
    async def execute(self, task: dict) -> dict:
        # 基本的なタスク実行
        return {"result": "completed"}

class AdvancedAgent(BaseAgent):
    """高度なエージェント実装。"""
    async def execute(self, task: dict) -> dict:
        # 高度なタスク実行（SimpleAgent の振る舞いを維持）
        result = await super().execute(task)
        result["advanced_feature"] = True
        return result

# どちらも同じインターフェースで使用可能
async def run_agent(agent: BaseAgent, task: dict) -> dict:
    return await agent.execute(task)
```

### 4. インターフェース分離原則 (Interface Segregation)
```python
# ✅ 正しい: 小さく焦点を絞ったインターフェース
from typing import Protocol

class ReadableRepository(Protocol):
    """読み取り操作のみ。"""
    async def find_by_id(self, id: str) -> object | None: ...
    async def find_all(self) -> list[object]: ...

class WritableRepository(Protocol):
    """書き込み操作のみ。"""
    async def save(self, entity: object) -> None: ...
    async def delete(self, id: str) -> None: ...

class UserRepository(ReadableRepository, WritableRepository):
    """ユーザー操作の完全なインターフェース。"""
    pass

# 使用側では必要なインターフェースのみ依存
class UserQueryService:
    def __init__(self, repo: ReadableRepository):  # 読み取りのみ必要
        self.repo = repo
```

### 5. 依存性逆転原則 (Dependency Inversion)
```python
# ✅ 正しい: 具象クラスではなく抽象に依存
from typing import Protocol

class Storage(Protocol):
    """ストレージ抽象インターフェース。"""
    async def save(self, key: str, data: bytes) -> None: ...
    async def load(self, key: str) -> bytes | None: ...

class FileStorage:
    """ファイルストレージ実装。"""
    async def save(self, key: str, data: bytes) -> None: ...
    async def load(self, key: str) -> bytes | None: ...

class DatabaseStorage:
    """データベースストレージ実装。"""
    async def save(self, key: str, data: bytes) -> None: ...
    async def load(self, key: str) -> bytes | None: ...

class DataService:
    """ストレージの実装ではなく抽象に依存。"""
    def __init__(self, storage: Storage):  # プロトコルに依存
        self.storage = storage
```

---

## 🔗 依存性管理

### 依存性注入パターン
```python
# ✅ 正しい: コンストラクタ注入
class WorkflowEngine:
    def __init__(
        self,
        agent_coordinator: AgentCoordinator,
        workflow_validator: WorkflowValidator,
        execution_monitor: ExecutionMonitor,
        logger: logging.Logger | None = None,
    ) -> None:
        self.coordinator = agent_coordinator
        self.validator = workflow_validator
        self.monitor = execution_monitor
        self.logger = logger or logging.getLogger(__name__)

# ファクトリ関数での依存性解決
def create_workflow_engine(config: dict) -> WorkflowEngine:
    """依存性を解決して WorkflowEngine を作成。"""
    coordinator = AgentCoordinator()
    validator = WorkflowValidator()
    monitor = ExecutionMonitor()
    logger = logging.getLogger("workflow")

    return WorkflowEngine(coordinator, validator, monitor, logger)
```

### 循環依存の回避
```python
# ❌ 間違い: 直接インポートによる循環依存
# module_a.py
from module_b import ClassB

class ClassA:
    def __init__(self):
        self.b = ClassB()

# module_b.py
from module_a import ClassA

class ClassB:
    def __init__(self):
        self.a = ClassA()

# ✅ 正しい: TYPE_CHECKING を使用した遅延インポート
# module_a.py
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from module_b import ClassB

class ClassA:
    def __init__(self):
        self._b: "ClassB | None" = None

    def set_dependency(self, b: "ClassB") -> None:
        self._b = b

# module_b.py
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from module_a import ClassA

class ClassB:
    def __init__(self):
        self._a: "ClassA | None" = None

    def set_dependency(self, a: "ClassA") -> None:
        self._a = a
```

---

## ⚡ 非同期設計パターン

### 非同期コンテキストマネージャー
```python
# ✅ 正しい: リソース管理のためのコンテキストマネージャー
class DatabaseConnection:
    def __init__(self, connection_string: str):
        self.connection_string = connection_string
        self._connection = None

    async def __aenter__(self):
        self._connection = await self._create_connection()
        return self._connection

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        if self._connection:
            await self._connection.close()

# 使用例
async def process_user_data(user_id: str) -> dict:
    async with DatabaseConnection(DB_URL) as conn:
        user_data = await conn.fetch_user(user_id)
        return await process_data(user_data)
```

### 非同期ジェネレーター
```python
# ✅ 正しい: ストリーミング処理のための非同期ジェネレーター
async def stream_workflow_events(
    workflow_id: str,
) -> AsyncGenerator[WorkflowEvent, None]:
    """ワークフローイベントをストリーミングする。"""
    async with self._event_subscription(workflow_id) as events:
        async for event in events:
            yield WorkflowEvent(
                type=event["type"],
                data=event["data"],
                timestamp=datetime.now(),
            )

# 使用例
async def monitor_workflow(workflow_id: str) -> None:
    async for event in stream_workflow_events(workflow_id):
        print(f"Event: {event.type} - {event.data}")
        if event.type == "completed":
            break
```

### 非同期ファクトリーパターン
```python
# ✅ 正しい: 非同期初期化が必要なオブジェクトのファクトリー
class LLMService:
    def __init__(self, api_key: str, model: str):
        self.api_key = api_key
        self.model = model
        self._client = None

    @classmethod
    async def create(cls, api_key: str, model: str) -> "LLMService":
        """非同期初期化を行うファクトリーメソッド。"""
        instance = cls(api_key, model)
        instance._client = await instance._initialize_client()
        return instance

    async def _initialize_client(self):
        """クライアントの非同期初期化。"""
        # モデルロード、接続確立などの重い処理
        await asyncio.sleep(1)  # シミュレーション
        return MockClient()

# 使用例
async def main():
    service = await LLMService.create(API_KEY, "gpt-4")
    # service は完全に初期化済み
```

---

## 🌐 プロトコル設計

### プロトコル抽象化
```python
# ✅ 正しい: プロトコルベースの設計
from typing import Protocol

class CommunicationProtocol(Protocol):
    """通信プロトコルの抽象インターフェース。"""
    async def send_message(self, message: dict) -> dict: ...
    async def receive_message(self) -> dict: ...
    async def close(self) -> None: ...

class MCPProtocol:
    """MCP プロトコル実装。"""
    async def send_message(self, message: dict) -> dict:
        # MCP 固有のロジック
        return await self._mcp_send(message)

class A2AProtocol:
    """A2A プロトコル実装。"""
    async def send_message(self, message: dict) -> dict:
        # A2A 固有のロジック
        return await self._a2a_send(message)

class ProtocolManager:
    """プロトコル管理クラス。"""
    def __init__(self):
        self._protocols: dict[str, CommunicationProtocol] = {}

    def register_protocol(self, name: str, protocol: CommunicationProtocol):
        self._protocols[name] = protocol

    async def send_via_protocol(self, protocol_name: str, message: dict) -> dict:
        protocol = self._protocols.get(protocol_name)
        if not protocol:
            raise ValueError(f"Unknown protocol: {protocol_name}")
        return await protocol.send_message(message)
```

### プロトコル自動適応
```python
# ✅ 正しい: 自動プロトコル変換
class ProtocolAdapter:
    """プロトコル間変換アダプター。"""

    @staticmethod
    async def adapt_message(
        message: dict,
        from_protocol: str,
        to_protocol: str,
    ) -> dict:
        """メッセージを異なるプロトコル形式に変換。"""
        adapter = ProtocolAdapter._get_adapter(from_protocol, to_protocol)
        return await adapter.convert(message)

    @staticmethod
    def _get_adapter(from_proto: str, to_proto: str):
        """適切なアダプターを取得。"""
        adapters = {
            ("mcp", "a2a"): MCPToA2AAdapter(),
            ("a2a", "ag-ui"): A2AToAGUIAdapter(),
            # 他の変換ルール...
        }
        return adapters.get((from_proto, to_proto), IdentityAdapter())
```

---

## 📊 データフロー設計

### 一方向データフロー
```python
# ✅ 正しい: Flux/Redux パターン風の一方向フロー
from typing import Callable, Any
from dataclasses import dataclass

@dataclass
class Action:
    """状態変更アクション。"""
    type: str
    payload: dict[str, Any]

@dataclass
class State:
    """アプリケーション状態。"""
    workflow_status: str = "idle"
    current_agent: str | None = None
    results: list[dict] = None

    def __post_init__(self):
        if self.results is None:
            self.results = []

class WorkflowStore:
    """一方向データフローストア。"""

    def __init__(self):
        self._state = State()
        self._listeners: list[Callable[[State], None]] = []

    def get_state(self) -> State:
        """現在の状態を取得。"""
        return self._state

    def dispatch(self, action: Action) -> None:
        """アクションをディスパッチし、状態を更新。"""
        self._state = self._reduce(self._state, action)
        self._notify_listeners()

    def subscribe(self, listener: Callable[[State], None]) -> Callable[[], None]:
        """状態変更リスナーを登録。"""
        self._listeners.append(listener)
        return lambda: self._listeners.remove(listener)

    def _reduce(self, state: State, action: Action) -> State:
        """状態遷移ロジック。"""
        if action.type == "START_WORKFLOW":
            return State(
                workflow_status="running",
                current_agent=action.payload.get("agent_id"),
                results=state.results.copy(),
            )
        elif action.type == "COMPLETE_WORKFLOW":
            return State(
                workflow_status="completed",
                current_agent=None,
                results=action.payload.get("results", state.results),
            )
        return state

    def _notify_listeners(self) -> None:
        """すべてのリスナーに通知。"""
        for listener in self._listeners:
            listener(self._state)
```

### イミュータブルデータ構造
```python
# ✅ 正しい: イミュータブルなデータ更新
from dataclasses import dataclass, replace
from typing import NamedTuple

class WorkflowConfig(NamedTuple):
    """イミュータブルなワークフロー設定。"""
    name: str
    timeout: float = 30.0
    max_retries: int = 3
    agents: tuple[str, ...] = ()

    def with_timeout(self, timeout: float) -> "WorkflowConfig":
        """タイムアウトを変更した新しい設定を作成。"""
        return self._replace(timeout=timeout)

    def with_agent(self, agent_id: str) -> "WorkflowConfig":
        """エージェントを追加した新しい設定を作成。"""
        return self._replace(agents=self.agents + (agent_id,))

# 使用例
config = WorkflowConfig(name="user-onboarding")
updated_config = config.with_timeout(60.0).with_agent("validator")
# 元の config は変更されない
```

---

## 🔍 アーキテクチャ品質チェック

### 設計品質チェックリスト
- [ ] SOLID原則が適切に適用されている
- [ ] 各層の責任分離が明確
- [ ] 依存性注入が使用されている
- [ ] 循環依存が存在しない
- [ ] 非同期パターンが適切に使用されている
- [ ] プロトコル抽象化が実装されている
- [ ] データフローが一方向
- [ ] イミュータブルなデータ構造が使用されている

### アーキテクチャレビューポイント
- **スケーラビリティ**: 拡張時の影響範囲
- **保守性**: 変更の容易さ
- **テスト容易性**: ユニットテストの書きやすさ
- **パフォーマンス**: 非同期処理の効率性
- **セキュリティ**: 入力検証と権限管理

---

## 📚 関連ドキュメント

- [**コーディング規約**](coding-standards.md) - コード品質基準
- [**命名規約**](naming-conventions.md) - 命名規則
- [**テスト規約**](testing-standards.md) - テスト設計原則
- [**アーキテクチャドキュメント**](../../../docs/architecture.md) - 詳細なアーキテクチャ説明

---

**優れたアーキテクチャは、変化に強いソフトウェアを実現します。** 🏗️

*最終更新: 2025-01-19 | バージョン: 1.0.0*
