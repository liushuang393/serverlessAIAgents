"""Platform App Schemas.

Pydantic モデル定義。
"""

from apps.platform.schemas.app_config_schemas import (
    AgentInfo,
    AgentBlueprintConfig,
    AuthContractConfig,
    AppConfig,
    BlueprintConfig,
    ContractsConfig,
    DependenciesConfig,
    EntryPointsConfig,
    PortsConfig,
    PluginBindingConfig,
    RAGContractConfig,
    ReleaseContractConfig,
    SkillsContractConfig,
    VisibilityConfig,
)
from apps.platform.schemas.capability_schemas import (
    CAPABILITY_DOMAINS,
    CanonicalCapability,
    CapabilityAggregate,
)
from apps.platform.schemas.component_schemas import (
    ComponentCreateRequest,
    ComponentListResponse,
    ComponentResponse,
    ComponentUpdateRequest,
)
from apps.platform.schemas.gallery_schemas import (
    GalleryFilter,
    GalleryItem,
    GalleryItemType,
    GallerySearchRequest,
    GallerySearchResponse,
)
from apps.platform.schemas.publish_schemas import (
    PublishRequest,
    PublishResponse,
    PublishStatus,
    PublishTarget,
)
from apps.platform.schemas.rag_schemas import (
    RAGConfigPatchRequest,
    RAGDataSourceInput,
)
from apps.platform.schemas.provisioning_schemas import (
    AgentBlueprintInput,
    AppCreateRequest,
    AppCreateResponse,
    MCPLazyLoadingPatchRequest,
    MCPServerUpsertRequest,
    PortConflictItem,
    PortConflictReport,
)
from apps.platform.schemas.plugin_manifest_schemas import (
    PluginCompatibility,
    PluginManifest,
    PluginSignature,
)


__all__ = [
    # AppConfig
    "AgentInfo",
    "AgentBlueprintConfig",
    "AuthContractConfig",
    "AppConfig",
    "BlueprintConfig",
    "ContractsConfig",
    "DependenciesConfig",
    "EntryPointsConfig",
    "PortsConfig",
    "PluginBindingConfig",
    "RAGContractConfig",
    "ReleaseContractConfig",
    "SkillsContractConfig",
    "VisibilityConfig",
    # Capability
    "CAPABILITY_DOMAINS",
    "CanonicalCapability",
    "CapabilityAggregate",
    # Component
    "ComponentCreateRequest",
    "ComponentListResponse",
    "ComponentResponse",
    "ComponentUpdateRequest",
    # Gallery
    "GalleryFilter",
    "GalleryItem",
    "GalleryItemType",
    "GallerySearchRequest",
    "GallerySearchResponse",
    # Publish
    "PublishRequest",
    "PublishResponse",
    "PublishStatus",
    "PublishTarget",
    # RAG 管理
    "RAGConfigPatchRequest",
    "RAGDataSourceInput",
    # Provisioning
    "AgentBlueprintInput",
    "AppCreateRequest",
    "AppCreateResponse",
    "MCPLazyLoadingPatchRequest",
    "MCPServerUpsertRequest",
    "PortConflictItem",
    "PortConflictReport",
    # Plugin Manifest
    "PluginCompatibility",
    "PluginManifest",
    "PluginSignature",
]
