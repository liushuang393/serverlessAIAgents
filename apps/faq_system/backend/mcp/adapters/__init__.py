"""第三者フレームワーク用アダプター パッケージ.

LlamaIndex / LangChain 等の外部フレームワークを
RetrievalBackend として統合するためのアダプターを提供。

使用時の注意:
    各アダプターは対応パッケージのインストールが必要。
    インストールされていない場合、initialize() で ImportError が発生。
"""

from apps.faq_system.backend.mcp.adapters.base import ThirdPartyAdapter

__all__ = ["ThirdPartyAdapter"]

