# AI Blocks API リファレンス

このドキュメントは、AI Blocksライブラリの全APIの詳細な説明を提供します。

## 目次

- [コアコンポーネント](#コアコンポーネント)
  - [Memory](#memory)
  - [Thread](#thread)
  - [Tool](#tool)
  - [Parser](#parser)
  - [Chunker](#chunker)
  - [Router](#router)
  - [Evaluator](#evaluator)
- [アーキテクチャパターン](#アーキテクチャパターン)
  - [AugmentedLLM](#augmentedllm)
- [データモデル](#データモデル)
- [ユーティリティ](#ユーティリティ)

## コアコンポーネント

### Memory

長期記憶のためのコンポーネント。ベクトル検索を使用して類似性に基づく情報検索を提供します。

#### MemoryInterface

```python
class MemoryInterface(ABC):
    """長期記憶のための抽象インターフェース"""

    @abstractmethod
    async def store(self, content: str, metadata: Dict[str, Any] = None) -> str:
        """コンテンツを記憶に保存する"""
        pass

    @abstractmethod
    async def search(self, query: str, limit: int = 10, threshold: float = None) -> List[MemoryItem]:
        """類似性検索を実行する"""
        pass

    @abstractmethod
    async def get(self, memory_id: str) -> Optional[MemoryItem]:
        """IDで記憶を取得する"""
        pass

    @abstractmethod
    async def delete(self, memory_id: str) -> bool:
        """記憶を削除する"""
        pass

    @abstractmethod
    async def clear(self) -> None:
        """全ての記憶を削除する"""
        pass

    @abstractmethod
    async def count(self) -> int:
        """記憶の総数を取得する"""
        pass
```

#### VectorMemory

```python
class VectorMemory(MemoryInterface):
    """ベクトル検索ベースの記憶実装"""

    def __init__(
        self,
        vector_store: Any = None,
        embedder: Any = None,
        max_items: int = None,
        similarity_threshold: float = None
    ):
        """
        Args:
            vector_store: ベクトルストア（Noneの場合はインメモリストアを使用）
            embedder: 埋め込みモデル（Noneの場合はモックを使用）
            max_items: 記憶の最大アイテム数
            similarity_threshold: デフォルトの類似度閾値
        """
```

**使用例:**

```python
from ai_blocks.core.memory import VectorMemory

# 初期化
memory = VectorMemory(max_items=1000, similarity_threshold=0.7)

# 記憶を保存
memory_id = await memory.store("Pythonは汎用プログラミング言語です", {"type": "knowledge"})

# 検索
results = await memory.search("プログラミング", limit=5)

# 取得
item = await memory.get(memory_id)

# 削除
await memory.delete(memory_id)
```

### Thread

会話履歴と状態管理のためのコンポーネント。

#### ThreadInterface

```python
class ThreadInterface(ABC):
    """会話履歴と状態管理のための抽象インターフェース"""

    @abstractmethod
    async def add_message(self, message: Message) -> None:
        """メッセージを履歴に追加する"""
        pass

    @abstractmethod
    async def get_history(self, limit: int = None) -> List[Message]:
        """履歴を取得する"""
        pass

    @abstractmethod
    async def get_state(self) -> Dict[str, Any]:
        """現在の状態を取得する"""
        pass

    @abstractmethod
    async def update_state(self, state: Dict[str, Any]) -> None:
        """状態を更新する"""
        pass

    @abstractmethod
    async def clear(self) -> None:
        """履歴と状態をクリアする"""
        pass

    @abstractmethod
    async def get_thread_id(self) -> str:
        """スレッドIDを取得する"""
        pass
```

#### SimpleThread

```python
class SimpleThread(ThreadInterface):
    """シンプルなインメモリスレッド実装"""

    def __init__(self, thread_id: str = None, max_history: int = 100):
        """
        Args:
            thread_id: スレッドID（Noneの場合は自動生成）
            max_history: 保持する履歴の最大数
        """
```

**使用例:**

```python
from ai_blocks.core.thread import SimpleThread
from ai_blocks.core.models import Message, MessageRole

# 初期化
thread = SimpleThread(max_history=50)

# メッセージを追加
message = Message(role=MessageRole.USER, content="こんにちは")
await thread.add_message(message)

# 履歴を取得
history = await thread.get_history(limit=10)

# 状態を更新
await thread.update_state({"user_name": "田中", "context": "greeting"})

# 状態を取得
state = await thread.get_state()
```

### Tool

外部API/関数実行のためのコンポーネント。

#### ToolInterface

```python
class ToolInterface(ABC):
    """外部API/関数実行のための抽象インターフェース"""

    @abstractmethod
    async def execute(self, tool_name: str, parameters: Dict[str, Any]) -> ToolResult:
        """ツールを実行する"""
        pass

    @abstractmethod
    def get_available_tools(self) -> List[ToolDefinition]:
        """利用可能なツール一覧を取得する"""
        pass

    @abstractmethod
    def register_tool(self, tool: ToolDefinition) -> None:
        """新しいツールを登録する"""
        pass

    @abstractmethod
    def unregister_tool(self, tool_name: str) -> bool:
        """ツールの登録を解除する"""
        pass
```

#### ToolManager

```python
class ToolManager(ToolInterface):
    """ツール管理の具体実装"""

    def __init__(self, timeout: float = None):
        """
        Args:
            timeout: ツール実行のタイムアウト時間（秒）
        """
```

#### toolデコレータ

```python
def tool(name: str = None, description: str = "", parameters: Dict[str, Any] = None):
    """関数をツールとして登録するためのデコレータ"""
```

**使用例:**

```python
from ai_blocks.core.tool import ToolManager, tool

# ツールマネージャーを初期化
tools = ToolManager(timeout=30.0)

# カスタムツールを定義
@tool(name="greet", description="挨拶を生成する")
def create_greeting(name: str, time: str = "day") -> str:
    return f"こんにちは、{name}さん！"

# ツールを登録
tools.register_function(create_greeting)

# ツールを実行
result = await tools.execute("greet", {"name": "山田"})
print(result.result)  # "こんにちは、山田さん！"
```

### Parser

多種ドキュメントをテキストに変換するためのコンポーネント。

#### ParserInterface

```python
class ParserInterface(ABC):
    """ドキュメント変換のための抽象インターフェース"""

    @abstractmethod
    async def parse(self, content: bytes, content_type: str, metadata: Dict[str, Any] = None) -> ParsedDocument:
        """ドキュメントをテキストに変換する"""
        pass

    @abstractmethod
    def supports(self, content_type: str) -> bool:
        """指定されたコンテンツタイプをサポートするか"""
        pass
```

#### MultiParser

```python
class MultiParser(ParserInterface):
    """複数のパーサーを統合したパーサー"""

    def __init__(self, parsers: List[ParserInterface] = None):
        """
        Args:
            parsers: 使用するパーサーのリスト（Noneの場合はデフォルトパーサーを使用）
        """
```

**使用例:**

```python
from ai_blocks.core.parser import MultiParser

# パーサーを初期化
parser = MultiParser()

# ドキュメントをパース
with open("document.pdf", "rb") as f:
    content = f.read()

parsed_doc = await parser.parse(content, "application/pdf")
print(parsed_doc.text)
```

### Chunker

長文分割と前処理のためのコンポーネント。

#### ChunkerInterface

```python
class ChunkerInterface(ABC):
    """長文分割と前処理のための抽象インターフェース"""

    @abstractmethod
    async def chunk(
        self,
        text: str,
        chunk_size: int = 1000,
        overlap: int = 200,
        metadata: Dict[str, Any] = None
    ) -> List[TextChunk]:
        """テキストをチャンクに分割する"""
        pass

    @abstractmethod
    async def merge_chunks(self, chunks: List[TextChunk]) -> str:
        """チャンクを結合する"""
        pass
```

#### SmartChunker

```python
class SmartChunker(ChunkerInterface):
    """スマートなチャンカー（複数の戦略を組み合わせ）"""

    def __init__(self):
        """スマートなチャンカーを初期化する"""
```

**使用例:**

```python
from ai_blocks.core.chunker import SmartChunker

# チャンカーを初期化
chunker = SmartChunker()

# テキストを分割
long_text = "..." # 長いテキスト
chunks = await chunker.chunk(long_text, chunk_size=500, overlap=50)

# チャンクを結合
merged_text = await chunker.merge_chunks(chunks)
```

### Router

インテント判定とAgent/Tool振り分けのためのコンポーネント。

#### RouterInterface

```python
class RouterInterface(ABC):
    """インテント判定とAgent/Tool振り分けのための抽象インターフェース"""

    @abstractmethod
    async def route(self, input_text: str, context: Dict[str, Any] = None) -> RouteResult:
        """入力に基づいて適切なAgent/Toolに振り分ける"""
        pass

    @abstractmethod
    def register_route(self, route: RouteDefinition) -> None:
        """新しいルートを登録する"""
        pass

    @abstractmethod
    def unregister_route(self, pattern: str) -> bool:
        """ルートの登録を解除する"""
        pass

    @abstractmethod
    def get_routes(self) -> List[RouteDefinition]:
        """登録されているルートの一覧を取得する"""
        pass
```

#### RuleBasedRouter

```python
class RuleBasedRouter(RouterInterface):
    """ルールベースのルーター"""

    def __init__(self, default_target: str = "default"):
        """
        Args:
            default_target: デフォルトのターゲット
        """
```

**使用例:**

```python
from ai_blocks.core.router import RuleBasedRouter, RouteDefinition, RouteType

# ルーターを初期化
router = RuleBasedRouter(default_target="general")

# カスタムルートを登録
route = RouteDefinition(
    pattern="weather",
    target="weather_agent",
    priority=90,
    conditions={"type": RouteType.KEYWORD, "keywords": ["天気", "weather"]}
)
router.register_route(route)

# ルーティング
result = await router.route("今日の天気はどうですか？")
print(result.target)  # "weather_agent"
```

### Evaluator

出力品質チェックと改善指示のためのコンポーネント。

#### EvaluatorInterface

```python
class EvaluatorInterface(ABC):
    """出力品質チェックと改善指示のための抽象インターフェース"""

    @abstractmethod
    async def evaluate(
        self,
        output: str,
        criteria: List[str],
        context: Dict[str, Any] = None
    ) -> EvaluationResult:
        """出力を評価する"""
        pass

    @abstractmethod
    async def suggest_improvements(
        self,
        output: str,
        evaluation: EvaluationResult,
        context: Dict[str, Any] = None
    ) -> List[str]:
        """改善提案を生成する"""
        pass
```

#### RuleBasedEvaluator

```python
class RuleBasedEvaluator(EvaluatorInterface):
    """ルールベースの評価器"""

    def __init__(self, passing_threshold: float = 0.7):
        """
        Args:
            passing_threshold: 合格とする閾値（0.0-1.0）
        """
```

**使用例:**

```python
from ai_blocks.core.evaluator import RuleBasedEvaluator

# 評価器を初期化
evaluator = RuleBasedEvaluator(passing_threshold=0.7)

# 出力を評価
output = "これは明確で有用な回答です。"
criteria = ["clarity", "relevance", "safety"]

evaluation = await evaluator.evaluate(output, criteria)
print(f"スコア: {evaluation.score}")
print(f"合格: {evaluation.passed}")

# 改善提案
suggestions = await evaluator.suggest_improvements(output, evaluation)
```

## アーキテクチャパターン

### AugmentedLLM

LLM + Memory + Tool を組み合わせた基本的なエージェントパターン。

```python
class AugmentedLLM(Agent):
    """LLMにMemoryとToolを組み合わせた基本的なエージェント"""

    def __init__(
        self,
        llm_provider: Any,
        memory: MemoryInterface = None,
        tool_manager: ToolInterface = None,
        name: str = "AugmentedLLM",
        config: Dict[str, Any] = None
    ):
        """
        Args:
            llm_provider: LLMプロバイダー
            memory: メモリコンポーネント
            tool_manager: ツールマネージャー
            name: エージェント名
            config: 設定辞書
        """

    async def process(self, input_text: str, context: Dict[str, Any] = None) -> str:
        """入力を処理して応答を生成する"""
        pass

    async def add_knowledge(self, knowledge: str, metadata: Dict[str, Any] = None) -> str:
        """知識を記憶に追加する"""
        pass

    async def get_status(self) -> Dict[str, Any]:
        """エージェントの状態を取得する"""
        pass
```

**使用例:**

```python
from ai_blocks.architectures.augmented_llm import AugmentedLLM
from ai_blocks.core.memory import VectorMemory
from ai_blocks.core.tool import ToolManager

# コンポーネントを初期化
memory = VectorMemory()
tools = ToolManager()
llm = YourLLMProvider()  # 実際のLLMプロバイダー

# エージェントを作成
agent = AugmentedLLM(
    llm_provider=llm,
    memory=memory,
    tool_manager=tools,
    config={
        "max_memory_items": 10,
        "enable_tool_use": True
    }
)

# 知識を追加
await agent.add_knowledge("Pythonは汎用プログラミング言語です。")

# 質問に回答
response = await agent.process("Pythonについて教えて")
```

## データモデル

### Message

```python
class Message(BaseModel):
    """メッセージモデル"""
    role: MessageRole
    content: str
    timestamp: datetime
    metadata: Dict[str, Any] = {}
```

### MemoryItem

```python
class MemoryItem(BaseModel):
    """記憶アイテムモデル"""
    id: str
    content: str
    metadata: Dict[str, Any]
    similarity_score: Optional[float] = None
    created_at: datetime
```

### ToolResult

```python
class ToolResult(BaseModel):
    """ツール実行結果モデル"""
    success: bool
    result: Any
    error_message: Optional[str] = None
    execution_time: float
    metadata: Dict[str, Any] = {}
```

### EvaluationResult

```python
class EvaluationResult(BaseModel):
    """評価結果モデル"""
    score: float
    criteria_scores: Dict[str, float]
    feedback: str
    passed: bool
    suggestions: List[str] = []
```

## ユーティリティ

### ロギング

```python
from ai_blocks.utils.logging import get_logger

logger = get_logger(__name__)
logger.info("ログメッセージ")
```

### 設定管理

```python
from ai_blocks.config import get_settings

settings = get_settings()
print(settings.log_level)
```

---

このAPIリファレンスは、AI Blocksライブラリの主要なコンポーネントとその使用方法を説明しています。より詳細な情報については、各コンポーネントのソースコードとdocstringを参照してください。
