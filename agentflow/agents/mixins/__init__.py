"""agentflow.agents.mixins - Agent 能力を付与する Mixin クラス群.

各 Mixin はそれぞれの能力（RAG、Skill など）を AgentBlock に付与する。
複数の Mixin を組み合わせて使うことができる。

使用例:
    >>> from agentflow.agents.mixins import RAGCapableMixin
    >>> from agentflow.core.agent_block import AgentBlock
    >>>
    >>> class MyAgent(RAGCapableMixin, AgentBlock):
    ...     ...
"""

from agentflow.agents.mixins.rag_mixin import RAGCapableMixin


__all__ = ["RAGCapableMixin"]
