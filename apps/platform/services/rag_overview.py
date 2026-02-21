"""RAG Overview Service — RAG 機能概要・App別設定管理."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from apps.platform.services.app_discovery import AppDiscoveryService


_CHUNK_STRATEGIES: list[dict[str, str]] = [
    {
        "name": "recursive",
        "label": "再帰的分割",
        "description": "LangChain 式の再帰的文字分割。汎用性が高い。",
    },
    {
        "name": "semantic",
        "label": "意味ベース分割",
        "description": "埋め込みベクトルの類似度でグループ化。精度重視。",
    },
    {
        "name": "sentence",
        "label": "文単位分割",
        "description": "文境界で分割。自然な区切りを維持。",
    },
    {
        "name": "token",
        "label": "トークンベース分割",
        "description": "トークン数ベースで均等分割。LLM コンテキスト制御向け。",
    },
    {
        "name": "markdown",
        "label": "Markdown ヘッダー分割",
        "description": "Markdown のヘッダー構造で分割。技術文書向け。",
    },
]

_RERANKERS: list[dict[str, str]] = [
    {
        "name": "cohere",
        "label": "Cohere Reranker",
        "description": "Cohere API によるリランキング。高精度。",
    },
    {
        "name": "cross_encoder",
        "label": "CrossEncoder",
        "description": "ローカル CrossEncoder モデルでリランキング。",
    },
    {
        "name": "bm25",
        "label": "BM25",
        "description": "BM25 スコアによるリランキング。外部依存なし。",
    },
    {
        "name": "none",
        "label": "なし",
        "description": "リランキングを行わない。",
    },
]

_RETRIEVAL_METHODS: list[dict[str, str]] = [
    {
        "name": "hybrid",
        "label": "Hybrid",
        "description": "ベクトル + キーワードのハイブリッド検索。",
    },
    {"name": "vector", "label": "Vector", "description": "ベクトル類似検索のみ。低遅延。"},
    {
        "name": "keyword",
        "label": "Keyword",
        "description": "キーワード/BM25 中心。説明可能性重視。",
    },
    {
        "name": "multi_query",
        "label": "Multi Query",
        "description": "クエリ拡張で取りこぼしを抑える方式。",
    },
]

_RAG_PATTERNS: list[dict[str, Any]] = [
    {
        "name": "balanced_knowledge",
        "label": "Balanced Knowledge",
        "description": "社内ドキュメント向けの標準構成。精度とコストのバランスを重視。",
        "config": {
            "chunk_strategy": "recursive",
            "chunk_size": 800,
            "chunk_overlap": 120,
            "retrieval_method": "hybrid",
            "reranker": "cross_encoder",
            "top_k": 6,
            "score_threshold": 0.2,
        },
    },
    {
        "name": "faq_precision",
        "label": "FAQ Precision",
        "description": "FAQ/問い合わせ応答向け。短文分割 + rerank で高精度化。",
        "config": {
            "chunk_strategy": "sentence",
            "chunk_size": 500,
            "chunk_overlap": 80,
            "retrieval_method": "hybrid",
            "reranker": "cohere",
            "top_k": 8,
            "score_threshold": 0.25,
        },
    },
    {
        "name": "news_realtime",
        "label": "News Realtime",
        "description": "ニュース/市場監視向け。速度重視の軽量設定。",
        "config": {
            "chunk_strategy": "token",
            "chunk_size": 700,
            "chunk_overlap": 60,
            "retrieval_method": "vector",
            "reranker": "none",
            "top_k": 4,
            "score_threshold": 0.1,
        },
    },
    {
        "name": "long_doc_reasoning",
        "label": "Long Doc Reasoning",
        "description": "規約・契約書など長文向け。構造分割と多段検索を優先。",
        "config": {
            "chunk_strategy": "markdown",
            "chunk_size": 1200,
            "chunk_overlap": 180,
            "retrieval_method": "multi_query",
            "reranker": "cross_encoder",
            "top_k": 10,
            "score_threshold": 0.3,
        },
    },
]

_VECTOR_PROVIDERS = {"qdrant", "pinecone", "weaviate", "pgvector", "milvus"}


class RAGOverviewService:
    """RAG 機能概要 + App別 RAG 設定サービス."""

    def __init__(self, discovery: AppDiscoveryService) -> None:
        self._discovery = discovery

    def get_overview(self) -> dict[str, Any]:
        apps = self.apps_using_rag()
        return {
            "description": (
                "AgentFlow RAG は、データソース取り込み・分割・検索・再ランクを App 単位で統一管理するための機能です。"
            ),
            "chunk_strategies": _CHUNK_STRATEGIES,
            "rerankers": _RERANKERS,
            "retrieval_methods": _RETRIEVAL_METHODS,
            "patterns": self.list_patterns(),
            "apps_using_rag": apps,
            "stats": {
                "total_strategies": len(_CHUNK_STRATEGIES),
                "total_rerankers": len(_RERANKERS),
                "total_apps_using_rag": len(apps),
            },
        }

    def list_strategies(self) -> list[dict[str, str]]:
        return list(_CHUNK_STRATEGIES)

    def list_rerankers(self) -> list[dict[str, str]]:
        return list(_RERANKERS)

    def list_retrieval_methods(self) -> list[dict[str, str]]:
        return list(_RETRIEVAL_METHODS)

    def list_patterns(self) -> list[dict[str, Any]]:
        return list(_RAG_PATTERNS)

    def list_app_configs(self) -> list[dict[str, Any]]:
        return [self._extract_app_rag(config) for config in self._discovery.list_apps()]

    def get_app_config(self, app_name: str) -> dict[str, Any]:
        config = self._discovery.get_app(app_name)
        if config is None:
            msg = f"App not found: {app_name}"
            raise KeyError(msg)
        return self._extract_app_rag(config)

    def update_app_config(self, app_name: str, patch: dict[str, Any]) -> dict[str, Any]:
        config = self._discovery.get_app(app_name)
        if config is None:
            msg = f"App not found: {app_name}"
            raise KeyError(msg)

        current = self._extract_app_rag(config)["rag"]
        pattern_name = patch.get("pattern")
        pattern_config: dict[str, Any] = {}
        if pattern_name:
            pattern_config = self._resolve_pattern(pattern_name)["config"]

        merged = {
            **current,
            **pattern_config,
            **patch,
        }
        rag_enabled = bool(merged.get("enabled", False))
        vector_provider = merged.get("vector_provider")
        if rag_enabled and not vector_provider:
            vector_provider = "qdrant"
            merged["vector_provider"] = vector_provider

        vector_collection = merged.get("vector_collection")
        if rag_enabled and not vector_collection:
            vector_collection = f"{app_name}_knowledge"
            merged["vector_collection"] = vector_collection

        contract_rag = {
            "enabled": rag_enabled,
            "pattern": merged.get("pattern"),
            "provider": vector_provider if rag_enabled else None,
            "collections": [vector_collection] if rag_enabled and vector_collection else [],
            "data_sources": merged.get("data_sources", []),
            "chunk_strategy": merged.get("chunk_strategy", "recursive"),
            "chunk_size": merged.get("chunk_size", 800),
            "chunk_overlap": merged.get("chunk_overlap", 120),
            "retrieval_method": merged.get("retrieval_method", "hybrid"),
            "embedding_model": merged.get("embedding_model"),
            "rerank_model": merged.get("reranker"),
            "default_top_k": merged.get("top_k", 5),
            "score_threshold": merged.get("score_threshold"),
            "indexing_schedule": merged.get("indexing_schedule"),
        }

        services_rag = {
            "enabled": rag_enabled,
            "pattern": merged.get("pattern"),
            "chunking": {
                "strategy": merged.get("chunk_strategy", "recursive"),
                "size": merged.get("chunk_size", 800),
                "overlap": merged.get("chunk_overlap", 120),
            },
            "retrieval": {
                "method": merged.get("retrieval_method", "hybrid"),
                "reranker": merged.get("reranker"),
                "top_k": merged.get("top_k", 5),
                "score_threshold": merged.get("score_threshold"),
            },
            "data_sources": merged.get("data_sources", []),
            "indexing_schedule": merged.get("indexing_schedule"),
        }

        services_vector_db = {
            "provider": vector_provider if rag_enabled else None,
            "url": merged.get("vector_url"),
            "collection": vector_collection,
        }

        external = set(config.dependencies.external)
        if rag_enabled:
            external.add("vector_store")
            if vector_provider and vector_provider in _VECTOR_PROVIDERS:
                external.add(vector_provider)
        else:
            external.discard("vector_store")

        tags = set(config.tags)
        if rag_enabled:
            tags.add("rag")
        else:
            tags.discard("rag")

        updated = self._discovery.update_app_config(
            app_name,
            {
                "contracts": {"rag": contract_rag},
                "services": {
                    "rag": services_rag,
                    "vector_db": services_vector_db,
                },
                "dependencies": {"external": sorted(external)},
                "tags": sorted(tags),
            },
        )
        return self._extract_app_rag(updated)

    def apps_using_rag(self) -> list[dict[str, Any]]:
        result: list[dict[str, Any]] = []
        for app in self.list_app_configs():
            rag = app["rag"]
            if not rag["enabled"]:
                continue
            details: list[str] = []
            if rag["vector_provider"]:
                details.append(f"provider: {rag['vector_provider']}")
            if rag["vector_collection"]:
                details.append(f"collection: {rag['vector_collection']}")
            details.append(f"retrieval: {rag['retrieval_method']}")
            if rag["reranker"]:
                details.append(f"rerank: {rag['reranker']}")
            if rag["data_sources"]:
                details.append(f"sources: {len(rag['data_sources'])}")
            result.append(
                {
                    "app_name": app["app_name"],
                    "display_name": app["display_name"],
                    "icon": app["icon"],
                    "rag_details": details,
                },
            )
        return result

    def stats(self) -> dict[str, Any]:
        return {
            "total_strategies": len(_CHUNK_STRATEGIES),
            "total_rerankers": len(_RERANKERS),
            "total_apps_using_rag": len(self.apps_using_rag()),
        }

    def _resolve_pattern(self, name: str) -> dict[str, Any]:
        for pattern in _RAG_PATTERNS:
            if pattern["name"] == name:
                return pattern
        msg = f"Unknown RAG pattern: {name}"
        raise ValueError(msg)

    def _extract_app_rag(self, app_config: Any) -> dict[str, Any]:
        raw_config = self._discovery.get_raw_config(app_config.name) or {}
        raw_contract_rag = (
            raw_config.get("contracts", {}).get("rag", {})
            if isinstance(raw_config.get("contracts"), dict)
            and isinstance(raw_config.get("contracts", {}).get("rag"), dict)
            else {}
        )

        rag_contract = app_config.contracts.rag
        services = app_config.services if isinstance(app_config.services, dict) else {}
        rag_service = services.get("rag", {}) if isinstance(services.get("rag"), dict) else {}
        vector_service = services.get("vector_db", {}) if isinstance(services.get("vector_db"), dict) else {}

        chunking = rag_service.get("chunking", {}) if isinstance(rag_service.get("chunking"), dict) else {}
        retrieval = rag_service.get("retrieval", {}) if isinstance(rag_service.get("retrieval"), dict) else {}
        service_collections = rag_service.get("collections", [])
        collection = (
            rag_contract.collections[0]
            if rag_contract.collections
            else (
                service_collections[0]
                if isinstance(service_collections, list) and service_collections
                else vector_service.get("collection")
            )
        )
        has_rag_agent = any(
            "rag" in capability.lower() for agent in app_config.agents for capability in agent.capabilities
        )
        enabled = self._resolve_enabled(
            raw_contract_rag=raw_contract_rag,
            contract_enabled=rag_contract.enabled,
            rag_service=rag_service,
            has_rag_agent=has_rag_agent,
        )

        data_sources = self._select(
            raw_contract_rag,
            "data_sources",
            rag_contract.data_sources,
            rag_service.get("data_sources"),
            [],
        )

        return {
            "app_name": app_config.name,
            "display_name": app_config.display_name,
            "icon": app_config.icon,
            "config_path": str(self._discovery.get_config_path(app_config.name) or ""),
            "rag": {
                "enabled": enabled,
                "pattern": self._select(
                    raw_contract_rag,
                    "pattern",
                    rag_contract.pattern,
                    rag_service.get("pattern"),
                    None,
                ),
                "vector_provider": self._select(
                    raw_contract_rag,
                    "provider",
                    rag_contract.provider,
                    vector_service.get("provider"),
                    None,
                ),
                "vector_url": vector_service.get("url"),
                "vector_collection": self._select_collection(
                    raw_contract_rag,
                    rag_contract.collections,
                    collection,
                ),
                "embedding_model": self._select(
                    raw_contract_rag,
                    "embedding_model",
                    rag_contract.embedding_model,
                    rag_service.get("embedding_model"),
                    None,
                ),
                "chunk_strategy": self._select(
                    raw_contract_rag,
                    "chunk_strategy",
                    rag_contract.chunk_strategy,
                    chunking.get("strategy"),
                    "recursive",
                ),
                "chunk_size": self._select(
                    raw_contract_rag,
                    "chunk_size",
                    rag_contract.chunk_size,
                    chunking.get("size"),
                    800,
                ),
                "chunk_overlap": self._select(
                    raw_contract_rag,
                    "chunk_overlap",
                    rag_contract.chunk_overlap,
                    chunking.get("overlap"),
                    120,
                ),
                "retrieval_method": self._select(
                    raw_contract_rag,
                    "retrieval_method",
                    rag_contract.retrieval_method,
                    retrieval.get("method"),
                    "hybrid",
                ),
                "reranker": self._select(
                    raw_contract_rag,
                    "rerank_model",
                    rag_contract.rerank_model,
                    retrieval.get("reranker"),
                    None,
                ),
                "top_k": self._select(
                    raw_contract_rag,
                    "default_top_k",
                    rag_contract.default_top_k,
                    retrieval.get("top_k"),
                    5,
                ),
                "score_threshold": self._select(
                    raw_contract_rag,
                    "score_threshold",
                    rag_contract.score_threshold,
                    retrieval.get("score_threshold"),
                    None,
                ),
                "indexing_schedule": self._select(
                    raw_contract_rag,
                    "indexing_schedule",
                    rag_contract.indexing_schedule,
                    rag_service.get("indexing_schedule"),
                    None,
                ),
                "data_sources": data_sources,
            },
        }

    @staticmethod
    def _resolve_enabled(
        *,
        raw_contract_rag: dict[str, Any],
        contract_enabled: bool,
        rag_service: dict[str, Any],
        has_rag_agent: bool,
    ) -> bool:
        """Resolve RAG enablement with priority contracts > services > inference."""
        if "enabled" in raw_contract_rag:
            return bool(contract_enabled)
        if "enabled" in rag_service:
            return bool(rag_service.get("enabled"))
        if rag_service:
            return True
        return has_rag_agent

    @staticmethod
    def _select(
        raw_contract_rag: dict[str, Any],
        contract_key: str,
        contract_value: Any,
        service_value: Any,
        default: Any,
    ) -> Any:
        """Pick value by priority contracts > services > default."""
        if contract_key in raw_contract_rag and contract_value is not None:
            return contract_value
        if service_value is not None:
            return service_value
        return default

    @staticmethod
    def _select_collection(
        raw_contract_rag: dict[str, Any],
        contract_collections: list[str],
        fallback_collection: Any,
    ) -> Any:
        """Pick vector collection with contract priority."""
        if "collections" in raw_contract_rag and contract_collections:
            return contract_collections[0]
        return fallback_collection
