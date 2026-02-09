"""FAQ System Skill.

知識ベース管理、検索、回答生成を提供。

Example:
    >>> from agentflow.skills.builtin.faq_system import FAQManager
    >>> faq = FAQManager()
    >>> await faq.add_question("How to reset password?", "Go to settings...")
    >>> answer = await faq.search("password reset")
"""

from agentflow.skills.builtin.faq_system.manager import (
    FAQEntry,
    FAQManager,
    SearchResult,
)


__all__ = [
    "FAQEntry",
    "FAQManager",
    "SearchResult",
]
