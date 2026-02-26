"""FAQ RAG/SQL runtime config loader.

app_config.json の RAG/SQL 関連設定を統一的に解決する。
未設定時は機能を無効化し、サービス全体は継続動作させる。
"""

from __future__ import annotations

import json
import logging
import os
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any


logger = logging.getLogger(__name__)

_APP_CONFIG_PATH = Path(__file__).resolve().parents[2] / "app_config.json"


@dataclass(slots=True)
class RAGDataSourceConfig:
    """RAG data source resolved config."""

    source_id: str
    source_type: str
    uri: str
    label: str
    enabled: bool
    schedule: str | None
    options: dict[str, Any] = field(default_factory=dict)


@dataclass(slots=True)
class RAGRuntimeConfig:
    """Resolved runtime config for FAQ app."""

    app_name: str
    config_path: Path
    rag_enabled: bool
    sql_enabled: bool
    rag_collection: str
    rag_chunk_strategy: str
    rag_reranker: str
    rag_top_k: int
    indexing_schedule: str | None
    data_sources: list[RAGDataSourceConfig]
    sql_schema: dict[str, list[str]]
    sql_dialect: str
    database_url: str | None
    database_kind: str | None

    @property
    def hybrid_enabled(self) -> bool:
        """RAG+SQL 並行経路の有効可否."""
        return self.rag_enabled and self.sql_enabled


def load_rag_runtime_config(config_path: Path | None = None) -> RAGRuntimeConfig:
    """Load runtime config from app_config.json."""
    target = config_path or _APP_CONFIG_PATH
    payload = _load_json(target)

    app_name = _as_non_empty_str(payload.get("name")) or "faq_system"
    contracts = _as_dict(payload.get("contracts"))
    services = _as_dict(payload.get("services"))
    runtime = _as_dict(payload.get("runtime"))

    rag_contract = _as_dict(contracts.get("rag"))
    rag_service = _as_dict(services.get("rag"))
    sql_service = _as_dict(services.get("sql"))

    runtime_urls = _as_dict(runtime.get("urls"))
    runtime_database = _as_dict(runtime.get("database"))

    rag_collection = (
        _first_collection_name(rag_contract.get("collections"))
        or _first_collection_name(rag_service.get("collections"))
        or _as_non_empty_str(_as_dict(services.get("vector_db")).get("collection"))
        or _as_non_empty_str(os.getenv("RAG_COLLECTION"))
        or "faq_knowledge"
    )

    raw_data_sources = _pick_with_contract_priority(
        contract=rag_contract,
        key="data_sources",
        service=rag_service,
        default=[],
    )
    rag_enabled = _resolve_feature_flag(
        contract=rag_contract,
        service=rag_service,
        fallback=bool(rag_service) or bool(rag_contract) or bool(raw_data_sources),
    )
    data_sources = _normalize_data_sources(raw_data_sources)

    indexing_schedule = _as_non_empty_str(
        _pick_with_contract_priority(
            contract=rag_contract,
            key="indexing_schedule",
            service=rag_service,
            default=None,
        )
    )

    sql_schema = _normalize_schema(sql_service.get("schema"))
    sql_dialect = (
        _as_non_empty_str(sql_service.get("dialect"))
        or _as_non_empty_str(runtime_database.get("kind"))
        or "postgresql"
    )
    database_url = (
        _as_non_empty_str(runtime_database.get("url"))
        or _as_non_empty_str(runtime_urls.get("database"))
    )
    database_kind = (
        _as_non_empty_str(runtime_database.get("kind"))
        or _infer_database_kind(database_url)
    )

    sql_enabled = _resolve_sql_enabled(
        sql_service=sql_service,
        database_url=database_url,
        database_kind=database_kind,
        sql_schema=sql_schema,
    )

    chunk_strategy = _as_non_empty_str(
        _pick_with_contract_priority(
            contract=rag_contract,
            key="chunk_strategy",
            service=_as_dict(rag_service.get("chunking")),
            service_key="strategy",
            default="recursive",
        )
    ) or "recursive"
    reranker = _as_non_empty_str(
        _pick_with_contract_priority(
            contract=rag_contract,
            key="rerank_model",
            service=_as_dict(rag_service.get("retrieval")),
            service_key="reranker",
            default="bm25",
        )
    ) or "bm25"
    top_k = _coerce_int(
        _pick_with_contract_priority(
            contract=rag_contract,
            key="default_top_k",
            service=_as_dict(rag_service.get("retrieval")),
            service_key="top_k",
            default=5,
        ),
        default=5,
        min_value=1,
        max_value=50,
    )

    return RAGRuntimeConfig(
        app_name=app_name,
        config_path=target,
        rag_enabled=rag_enabled,
        sql_enabled=sql_enabled,
        rag_collection=rag_collection,
        rag_chunk_strategy=chunk_strategy,
        rag_reranker=reranker,
        rag_top_k=top_k,
        indexing_schedule=indexing_schedule,
        data_sources=data_sources,
        sql_schema=sql_schema,
        sql_dialect=sql_dialect,
        database_url=database_url,
        database_kind=database_kind,
    )


def sync_runtime_env(config: RAGRuntimeConfig) -> None:
    """Sync resolved config to environment variables (setdefault only)."""
    os.environ.setdefault("RAG_COLLECTION", config.rag_collection)
    os.environ.setdefault("FAQ_ENABLE_RAG", "1" if config.rag_enabled else "0")
    os.environ.setdefault("FAQ_ENABLE_SQL", "1" if config.sql_enabled else "0")
    if config.database_url:
        os.environ.setdefault("FAQ_DATABASE_URL", config.database_url)
    if config.sql_schema and "DB_SCHEMA" not in os.environ:
        os.environ["DB_SCHEMA"] = json.dumps(config.sql_schema, ensure_ascii=False)


def _load_json(path: Path) -> dict[str, Any]:
    if not path.is_file():
        return {}
    try:
        return json.loads(path.read_text("utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        logger.warning("Failed to parse app_config.json: %s", exc)
        return {}


def _as_dict(value: Any) -> dict[str, Any]:
    return value if isinstance(value, dict) else {}


def _as_non_empty_str(value: Any) -> str | None:
    if value is None:
        return None
    text = str(value).strip()
    return text or None


def _as_bool(value: Any) -> bool | None:
    if isinstance(value, bool):
        return value
    if isinstance(value, str):
        lowered = value.strip().lower()
        if lowered in {"1", "true", "yes", "on"}:
            return True
        if lowered in {"0", "false", "no", "off"}:
            return False
    return None


def _first_collection_name(values: Any) -> str | None:
    if not isinstance(values, list):
        return None
    for item in values:
        text = _as_non_empty_str(item)
        if text:
            return text
    return None


def _resolve_feature_flag(
    *,
    contract: dict[str, Any],
    service: dict[str, Any],
    fallback: bool,
) -> bool:
    if "enabled" in contract:
        resolved = _as_bool(contract.get("enabled"))
        if resolved is not None:
            return resolved
    if "enabled" in service:
        resolved = _as_bool(service.get("enabled"))
        if resolved is not None:
            return resolved
    return fallback


def _pick_with_contract_priority(
    *,
    contract: dict[str, Any],
    key: str,
    service: dict[str, Any],
    default: Any,
    service_key: str | None = None,
) -> Any:
    if key in contract and contract.get(key) is not None:
        return contract.get(key)
    target_key = service_key or key
    if target_key in service and service.get(target_key) is not None:
        return service.get(target_key)
    return default


def _normalize_schema(value: Any) -> dict[str, list[str]]:
    if not isinstance(value, dict):
        return {}
    result: dict[str, list[str]] = {}
    for table, columns in value.items():
        table_name = _as_non_empty_str(table)
        if not table_name:
            continue
        if isinstance(columns, list):
            normalized_columns = [col for col in (_as_non_empty_str(item) for item in columns) if col]
            if normalized_columns:
                result[table_name] = normalized_columns
    return result


def _normalize_data_sources(value: Any) -> list[RAGDataSourceConfig]:
    if not isinstance(value, list):
        return []

    result: list[RAGDataSourceConfig] = []
    for index, source in enumerate(value):
        if not isinstance(source, dict):
            continue
        source_type = (_as_non_empty_str(source.get("type")) or "web").lower()
        uri = _as_non_empty_str(source.get("uri")) or ""
        label = _as_non_empty_str(source.get("label")) or ""
        schedule = _as_non_empty_str(source.get("schedule"))
        options = source.get("options")
        source_options = options if isinstance(options, dict) else {}

        enabled_flag = _as_bool(source.get("enabled"))
        enabled = enabled_flag if enabled_flag is not None else True
        source_id = (
            _as_non_empty_str(source.get("id"))
            or _as_non_empty_str(source_options.get("source_id"))
            or f"source-{index + 1}"
        )

        result.append(
            RAGDataSourceConfig(
                source_id=source_id,
                source_type=source_type,
                uri=uri,
                label=label,
                enabled=enabled,
                schedule=schedule,
                options=source_options,
            )
        )
    return result


def _resolve_sql_enabled(
    *,
    sql_service: dict[str, Any],
    database_url: str | None,
    database_kind: str | None,
    sql_schema: dict[str, list[str]],
) -> bool:
    if "enabled" in sql_service:
        resolved = _as_bool(sql_service.get("enabled"))
        if resolved is not None:
            return resolved

    if not sql_service:
        return False

    if database_url:
        return True
    if database_kind:
        return True
    if sql_schema:
        return True
    return False


def _infer_database_kind(database_url: str | None) -> str | None:
    url = _as_non_empty_str(database_url)
    if not url:
        return None
    lowered = url.lower()
    if lowered.startswith("postgres"):
        return "postgresql"
    if lowered.startswith("mysql"):
        return "mysql"
    if lowered.startswith("sqlite"):
        return "sqlite"
    if lowered.startswith("mssql") or lowered.startswith("sqlserver"):
        return "mssql"
    scheme = lowered.split("://", 1)[0]
    if "+" in scheme:
        scheme = scheme.split("+", 1)[0]
    return scheme or None


def _coerce_int(value: Any, *, default: int, min_value: int, max_value: int) -> int:
    try:
        parsed = int(value)
    except (TypeError, ValueError):
        return default
    return max(min_value, min(max_value, parsed))
