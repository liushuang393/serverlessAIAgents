"""プロンプトレイヤーシステム.

6層プロンプト設計に基づくプロンプト合成フレームワーク。

使用例（単発タスク）:
    >>> from kernel.prompts import (
    ...     PromptAssembler,
    ...     PromptLayerSet,
    ...     CoreSystemLayer,
    ...     TaskSystemLayer,
    ...     RuntimeContextLayer,
    ...     PromptPattern,
    ... )
    >>>
    >>> layers = PromptLayerSet(
    ...     core_system=CoreSystemLayer(role="分析エージェント"),
    ...     task_system=TaskSystemLayer(goal="データ分析"),
    ...     runtime_context=RuntimeContextLayer(user_request="売上を分析して"),
    ... )
    >>> assembler = PromptAssembler()
    >>> result = assembler.assemble(layers, PromptPattern.SINGLE_TASK)
    >>> print(result.system_prompt)

使用例（パターン自動選択）:
    >>> from kernel.prompts import select_pattern
    >>> pattern = select_pattern(has_tools=True, turn_count=3)
    >>> result = assembler.assemble(layers, pattern)
"""

# モデル
# アセンブラー
from kernel.prompts.assembler import PromptAssembler

# ビルダー
from kernel.prompts.builders import (
    ConversationStateBuilder,
    CoreSystemBuilder,
    MemoryProfileBuilder,
    RuntimeContextBuilder,
    TaskSystemBuilder,
    ToolEnvironmentBuilder,
)
from kernel.prompts.models import (
    LAYER_CONVERSATION_STATE,
    LAYER_CORE_SYSTEM,
    LAYER_MEMORY_PROFILE,
    LAYER_ORDER,
    LAYER_RUNTIME_CONTEXT,
    LAYER_TASK_SYSTEM,
    LAYER_TOOL_ENVIRONMENT,
    AssembledPrompt,
    ConversationStateLayer,
    CoreSystemLayer,
    LayerPriority,
    MemoryProfileLayer,
    PromptLayerSet,
    PromptPattern,
    RuntimeContextLayer,
    TaskSystemLayer,
    ToolDescription,
    ToolEnvironmentLayer,
)

# パターン
from kernel.prompts.patterns import (
    PATTERN_REGISTRY,
    PatternConfig,
    get_pattern_config,
    select_pattern,
)

# 品質チェック
from kernel.prompts.quality import PromptQualityChecker, QualityReport


__all__ = [
    # 定数
    "LAYER_CONVERSATION_STATE",
    "LAYER_CORE_SYSTEM",
    "LAYER_MEMORY_PROFILE",
    "LAYER_ORDER",
    "LAYER_RUNTIME_CONTEXT",
    "LAYER_TASK_SYSTEM",
    "LAYER_TOOL_ENVIRONMENT",
    # パターン
    "PATTERN_REGISTRY",
    # モデル
    "AssembledPrompt",
    # ビルダー
    "ConversationStateBuilder",
    "ConversationStateLayer",
    "CoreSystemBuilder",
    "CoreSystemLayer",
    "LayerPriority",
    "MemoryProfileBuilder",
    "MemoryProfileLayer",
    "PatternConfig",
    # アセンブラー
    "PromptAssembler",
    "PromptLayerSet",
    "PromptPattern",
    # 品質
    "PromptQualityChecker",
    "QualityReport",
    "RuntimeContextBuilder",
    "RuntimeContextLayer",
    "TaskSystemBuilder",
    "TaskSystemLayer",
    "ToolDescription",
    "ToolEnvironmentBuilder",
    "ToolEnvironmentLayer",
    "get_pattern_config",
    "select_pattern",
]
