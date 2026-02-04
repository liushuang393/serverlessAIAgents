# -*- coding: utf-8 -*-
"""Platform App - AgentFlow Platform Application.

Platform は AgentFlow の統合プラットフォームアプリケーションです。
Gallery、一键发布、多租户Dashboard をサポートします。

アーキテクチャ:
    - Gallery: Agent/App/Component の検索・発見
    - ComponentLibrary: 共通コンポーネントライブラリ
    - PublishOrchestrator: 一键发布フロー
    - TenantDashboard: 多租户ダッシュボード

使用例:
    >>> from apps.platform import PlatformEngine
    >>>
    >>> engine = PlatformEngine()
    >>> # Gallery検索
    >>> results = await engine.search_gallery("PDF processor")
    >>> # コンポーネント登録
    >>> await engine.register_component(component)
    >>> # 一键发布
    >>> await engine.publish(agent, target="docker")
"""

from apps.platform.engine import PlatformEngine
from apps.platform.services.component_library import (
    ComponentLibrary,
    ComponentType,
    ComponentVisibility,
    ComponentEntry,
)
from apps.platform.services.gallery_service import GalleryService
from apps.platform.services.publish_orchestrator import PublishOrchestrator

__version__ = "1.0.0"
__author__ = "AgentFlow Team"

__all__ = [
    "PlatformEngine",
    "ComponentLibrary",
    "ComponentType",
    "ComponentVisibility",
    "ComponentEntry",
    "GalleryService",
    "PublishOrchestrator",
]
