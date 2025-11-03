"""
AI Blocks - サーバーレスAIエージェント基盤

このライブラリは「積木（レゴ）方式」でAIエージェントを構築するための
軽量で柔軟なコンポーネントを提供します。

主要コンポーネント:
- Memory: ベクトル検索を伴う長期記憶
- Thread: 会話履歴と状態管理
- Tool: 外部API/関数実行
- Parser: 多種ドキュメントのテキスト変換
- Chunker: 長文分割と前処理
- Router: インテント判定とAgent/Tool振り分け
- Evaluator: 出力品質チェックと改善指示

アーキテクチャパターン:
- Augmented LLM: LLM+Memory+Tool
- Prompt Chaining: 専用Agentの直列化
- Agent Router: 入力内容で最適Agentに振り分け
- Parallel Agents: 複数Agentの非同期並列実行
- Orchestrator-Worker: タスク分割と統合
- Evaluator-Optimizer: 自己評価による再生成
- Tool Calling: 外部システム連携
- Memory-centric: ドキュメントQAなど知識特化
"""

__version__ = "0.1.0"
__author__ = "AI Blocks Team"
__email__ = "contact@aiblocks.dev"

# アーキテクチャパターンのインポート
from .architectures import (
    AgentRouter,
    AugmentedLLM,
    DataProcessingWorker,
    EvaluatorOptimizer,
    FunctionCallingAgent,
    LLMWorker,
    MemoryCentricAgent,
    OptimizationStrategy,
    OrchestratorWorker,
    ParallelAgents,
    PromptChain,
    SearchStrategy,
    ToolCallingAgent,
    WorkerAgent,
)

# コアコンポーネントのインポート
from .core import (
    ChunkerInterface,
    EvaluatorInterface,
    MemoryInterface,
    ParserInterface,
    RouterInterface,
    ThreadInterface,
    ToolInterface,
)

# データモデルのインポート
from .core.models import (
    EvaluationResult,
    MemoryItem,
    Message,
    MessageRole,
    ParsedDocument,
    RouteDefinition,
    RouteResult,
    TextChunk,
    ToolDefinition,
    ToolResult,
)

__all__ = [
    # バージョン情報
    "__version__",
    "__author__",
    "__email__",
    # コアインターフェース
    "MemoryInterface",
    "ThreadInterface",
    "ToolInterface",
    "ParserInterface",
    "ChunkerInterface",
    "RouterInterface",
    "EvaluatorInterface",
    # データモデル
    "Message",
    "MessageRole",
    "MemoryItem",
    "ToolResult",
    "ToolDefinition",
    "ParsedDocument",
    "TextChunk",
    "RouteResult",
    "RouteDefinition",
    "EvaluationResult",
    # アーキテクチャパターン
    "AugmentedLLM",
    "PromptChain",
    "AgentRouter",
    "ParallelAgents",
    "OrchestratorWorker",
    "WorkerAgent",
    "LLMWorker",
    "DataProcessingWorker",
    "EvaluatorOptimizer",
    "OptimizationStrategy",
    "ToolCallingAgent",
    "FunctionCallingAgent",
    "MemoryCentricAgent",
    "SearchStrategy",
]
