"""Layer 1 tool provider の互換 facade.

旧来の `infrastructure.llm.providers.tool_provider` 参照を、
正規の `infrastructure.providers.tool_provider` へ委譲する。
"""

from infrastructure.providers.tool_provider import *  # noqa: F403
from infrastructure.providers.tool_provider import _tool_registry  # noqa: F401

