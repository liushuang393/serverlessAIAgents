"""agentflow.bootstrap - 積木/プラグインモード + 動的能力自動接続.

contracts.* を書くだけで RAG/Skills/MCP が自動接続される
AppCapabilityBootstrapper と CapabilityBundle を提供する。

使用例:
    >>> import os
    >>> from agentflow.bootstrap import AppCapabilityBootstrapper, CapabilityBundle
    >>>
    >>> # lifespan で起動
    >>> bundle, bootstrapper = await AppCapabilityBootstrapper.build(
    ...     app_name="faq_system",
    ...     platform_url=os.environ.get("PLATFORM_URL"),
    ... )
    >>>
    >>> # RAG 有効なら RAGPipeline、無効なら None
    >>> if bundle.has_rag():
    ...     docs = await bundle.rag_engine.search("質問テキスト")
    >>>
    >>> # app shutdown 時
    >>> await bootstrapper.shutdown()

モジュール構成:
    capability_bundle.py: CapabilityBundle データクラス
    app_bootstrapper.py: AppCapabilityBootstrapper コア
    rag_builder.py: RAGContractConfig → RAGPipeline ファクトリ
    skill_builder.py: SkillsContractConfig → SkillGateway ファクトリ
    config_watcher.py: Platform SSE 購読 → ホットリロード
"""

from agentflow.bootstrap.app_bootstrapper import AppCapabilityBootstrapper
from agentflow.bootstrap.capability_bundle import CapabilityBundle
from agentflow.bootstrap.config_watcher import ConfigWatcher
from agentflow.bootstrap.rag_builder import RagBootstrapConfig, build_rag_engine
from agentflow.bootstrap.skill_builder import SkillsBootstrapConfig, build_skill_gateway


__all__ = [
    "AppCapabilityBootstrapper",
    "CapabilityBundle",
    "ConfigWatcher",
    "RagBootstrapConfig",
    "SkillsBootstrapConfig",
    "build_rag_engine",
    "build_skill_gateway",
]
