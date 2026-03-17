"""マルチLLMプロバイダーモジュール.

Agent別モデル割り当て、動的ルーティング、コスト最適化機能を提供。

使用例:
    >>> from infrastructure.providers.llm import MultiLLMRouter, ModelSelectionCriteria
    >>> router = MultiLLMRouter(criteria=ModelSelectionCriteria(priority="cost"))
    >>> provider = await router.select_provider(task_type="code_generation")
"""

from infrastructure.providers.llm.cost_optimizer import CostBudget, CostOptimizer, CostSummary
from infrastructure.providers.llm.model_selector import (
    ModelSelectionCriteria,
    ModelSelector,
    TaskType,
)
from infrastructure.providers.llm.multi_llm_router import (
    AgentModelMapping,
    MultiLLMRouter,
    RouterConfig,
)


__all__ = [
    "AgentModelMapping",
    "CostBudget",
    # コスト最適化
    "CostOptimizer",
    "CostSummary",
    "ModelSelectionCriteria",
    # モデル選択
    "ModelSelector",
    # マルチLLMルーター
    "MultiLLMRouter",
    "RouterConfig",
    "TaskType",
]
