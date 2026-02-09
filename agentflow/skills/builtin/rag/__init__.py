"""RAG Skill - 検索増強生成（Retrieval-Augmented Generation）.

このモジュールは、知識ベースと LLM を統合した RAG 機能を提供します。
SKILL.md と Python 実装が同じディレクトリに配置されています。

Example:
    >>> from agentflow.skills.builtin.rag import RAGSkill, RAGConfig
    >>> rag = RAGSkill(config=RAGConfig(top_k=5))
    >>> await rag.start()
    >>> result = await rag.query("AgentFlow とは？")
"""

from agentflow.skills.builtin.rag.rag import RAGConfig, RAGResult, RAGSkill


__all__ = [
    "RAGConfig",
    "RAGResult",
    "RAGSkill",
]

