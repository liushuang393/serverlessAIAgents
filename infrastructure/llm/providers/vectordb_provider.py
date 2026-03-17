"""Layer 1 vectordb provider の互換 facade.

旧来の `infrastructure.llm.providers.vectordb_provider` 参照を、
正規の `infrastructure.providers.vectordb_provider` へ委譲する。
"""

from infrastructure.providers.vectordb_provider import *  # noqa: F401,F403

