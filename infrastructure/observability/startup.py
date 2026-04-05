"""起動時情報ログモジュール."""

from __future__ import annotations

import json
import logging
from collections import defaultdict
from copy import deepcopy
from pathlib import Path
from typing import Any

from contracts.app.contracts import LLMContractConfig, LLMContractModality, LLMContractModelRef
from shared.config.manifest import load_app_manifest_dict


logger = logging.getLogger("bizcore.startup")
_MODALITY_ORDER: dict[LLMContractModality, int] = {
    "text": 0,
    "embedding": 1,
    "image": 2,
    "speech_to_text": 3,
    "text_to_speech": 4,
}


def _mask_secret(value: str | None, visible_chars: int = 4) -> str:
    """機密情報をマスク.

    Args:
        value: マスク対象の文字列
        visible_chars: 表示する末尾文字数

    Returns:
        マスクされた文字列
    """
    if not value:
        return "Not configured"
    if len(value) <= visible_chars:
        return "***"
    return f"***{value[-visible_chars:]}"


def _mask_url(url: str | None) -> str:
    """URL内の機密情報をマスク."""
    if not url:
        return "Not configured"
    if "@" in url:
        parts = url.split("@")
        return f"***@{parts[-1]}"
    if len(url) > 40:
        return url[:30] + "..."
    return url


def _normalize_value(value: Any, *, fallback: str = "Not configured") -> str:
    text = str(value).strip() if value is not None else ""
    return text or fallback


def _merge_nested_dict(base: dict[str, Any], override: dict[str, Any]) -> dict[str, Any]:
    """None 以外の値のみを反映して辞書を再帰マージする."""
    merged = deepcopy(base)
    for key, value in override.items():
        if value is None:
            continue
        if isinstance(value, dict) and isinstance(merged.get(key), dict):
            merged[key] = _merge_nested_dict(merged[key], value)
            continue
        merged[key] = deepcopy(value)
    return merged


def _detect_database_backend(url: str | None) -> str:
    """接続 URL から DB backend を推定する."""
    normalized = str(url or "").strip().lower()
    if not normalized:
        return "memory"
    if normalized.startswith("sqlite"):
        return "sqlite"
    if normalized.startswith(("postgresql", "postgres")):
        return "postgresql"
    if normalized.startswith("mysql"):
        return "mysql"
    if normalized.startswith("mssql"):
        return "mssql"
    if normalized.startswith("oracle"):
        return "oracle"
    return "unknown"


def _load_manifest_dict(app_config_path: str | Path | None) -> dict[str, Any]:
    """app_config.json を正規化 dict として読み込む."""
    if app_config_path is None:
        return {}

    resolved_path = Path(app_config_path).resolve()
    if not resolved_path.is_file():
        return {}

    try:
        return load_app_manifest_dict(resolved_path)
    except (OSError, ValueError, json.JSONDecodeError):
        return {}


def _load_app_llm_contract(app_config_path: str | Path | None) -> tuple[str, LLMContractConfig] | None:
    """app_config.json から contracts.llm をロードする."""
    manifest = _load_manifest_dict(app_config_path)
    contracts = manifest.get("contracts")
    if not isinstance(contracts, dict):
        return None

    llm_contracts = contracts.get("llm")
    if not isinstance(llm_contracts, dict):
        return None

    try:
        config = LLMContractConfig.model_validate(llm_contracts)
    except Exception:
        return None

    fallback_app_name = "unknown"
    if app_config_path is not None:
        fallback_app_name = Path(app_config_path).resolve().parent.name
    app_name = str(manifest.get("name") or fallback_app_name).strip() or "unknown"
    return app_name, config


def _build_model_lookup() -> dict[str, dict[str, Any]]:
    """gateway config から model_id / alias lookup を構築する."""
    from infrastructure.llm.gateway.config import load_gateway_config

    config = load_gateway_config()
    provider_bases = {provider.name: provider.api_base for provider in config.providers}
    engine_bases = {engine.name: engine.base_url for engine in config.inference_engines}
    lookup: dict[str, dict[str, Any]] = {}
    for model in config.models:
        resolved_api_base = model.api_base or engine_bases.get(str(model.engine or "").strip().lower())
        if not resolved_api_base:
            resolved_api_base = provider_bases.get(model.provider)
        payload = {
            "alias": model.alias,
            "model_id": str(model.model_id or model.alias),
            "provider": model.provider,
            "model": model.model,
            "model_type": model.model_type,
            "api_base": resolved_api_base,
        }
        lookup[model.alias] = payload
        lookup[str(model.model_id or model.alias)] = payload
    return lookup


def _resolve_gateway_model(
    model_ref: LLMContractModelRef, *, model_lookup: dict[str, dict[str, Any]]
) -> dict[str, Any] | None:
    """contracts.llm の model ref を gateway model へ解決する."""
    return model_lookup.get(model_ref.model_id)


def _collect_contract_refs(contracts: LLMContractConfig) -> list[LLMContractModelRef]:
    """defaults / agent overrides / extra refs を 1 つの一覧に集約する."""
    refs: list[LLMContractModelRef] = []

    for modality in _MODALITY_ORDER:
        ref = contracts.defaults.get(modality)
        if ref is not None:
            refs.append(ref)

    for binding in contracts.agent_overrides.values():
        for modality in _MODALITY_ORDER:
            ref = binding.get(modality)
            if ref is not None:
                refs.append(ref)

    refs.extend(contracts.extra_model_refs)
    return refs


def _resolve_supported_models(app_config_path: str | Path | None) -> list[dict[str, Any]]:
    """app 単位で使用可能な model refs を実モデルへ解決する."""
    loaded = _load_app_llm_contract(app_config_path)
    if loaded is None:
        return []

    app_name, contracts = loaded
    if not contracts.enabled:
        return []

    try:
        model_lookup = _build_model_lookup()
    except Exception:
        logger.warning("[LLM] Failed to load gateway config for supported models", exc_info=True)
        model_lookup = {}

    deduped: dict[tuple[str, str, str], LLMContractModelRef] = {}
    for ref in _collect_contract_refs(contracts):
        key = (ref.model_type, ref.provider, ref.model_id)
        deduped.setdefault(key, ref)

    resolved_entries: list[dict[str, Any]] = []
    for ref in sorted(
        deduped.values(),
        key=lambda item: (_MODALITY_ORDER.get(item.model_type, 99), item.model_id, item.provider),
    ):
        resolved = _resolve_gateway_model(ref, model_lookup=model_lookup)
        if resolved is None:
            logger.warning(
                "[LLM] Unresolved app model ref: app=%s modality=%s provider=%s model_id=%s",
                app_name,
                ref.model_type,
                ref.provider,
                ref.model_id,
            )
            resolved_entries.append(
                {
                    "model_type": ref.model_type,
                    "model_id": ref.model_id,
                    "provider": None,
                    "model": None,
                    "alias": None,
                    "api_base": None,
                    "resolved": False,
                }
            )
            continue

        resolved_entries.append(
            {
                "model_type": ref.model_type,
                "model_id": ref.model_id,
                "provider": resolved["provider"],
                "model": resolved["model"],
                "alias": resolved["alias"],
                "api_base": resolved["api_base"],
                "resolved": True,
            }
        )

    return resolved_entries


def _resolve_default_contract_model(
    app_config_path: str | Path | None,
    *,
    modality: LLMContractModality,
) -> dict[str, Any] | None:
    """contracts.llm.defaults の指定 modality を実モデルへ解決する."""
    loaded = _load_app_llm_contract(app_config_path)
    if loaded is None:
        return None

    _app_name, contracts = loaded
    if not contracts.enabled:
        return None

    ref = contracts.defaults.get(modality)
    if ref is None:
        return None

    try:
        model_lookup = _build_model_lookup()
    except Exception:
        logger.warning("[LLM] Failed to load gateway config for default contract model", exc_info=True)
        return None
    return _resolve_gateway_model(ref, model_lookup=model_lookup)


def _collect_app_config_overrides(app_config_path: str | Path | None) -> dict[str, Any]:
    """app_config.json から startup summary 用のローカル値を抽出する."""
    manifest = _load_manifest_dict(app_config_path)
    if not manifest:
        return {}

    overrides: dict[str, Any] = {}
    runtime = manifest.get("runtime")
    if isinstance(runtime, dict):
        database = runtime.get("database")
        if isinstance(database, dict):
            db_url = str(database.get("url") or "").strip() or None
            db_backend = str(database.get("kind") or database.get("backend") or "").strip() or None
            if db_url or db_backend:
                overrides["db"] = {
                    "backend": db_backend or _detect_database_backend(db_url),
                    "url": db_url,
                }

    text_model = _resolve_default_contract_model(app_config_path, modality="text")
    if text_model is not None:
        overrides["llm"] = {
            "provider": text_model["provider"],
            "model": text_model["model"],
            "base_url": text_model["api_base"],
        }

    embedding_model = _resolve_default_contract_model(app_config_path, modality="embedding")
    if embedding_model is not None:
        overrides["embedding"] = {"model": embedding_model["model"]}

    return overrides


def _collect_global_summary() -> dict[str, Any]:
    """gateway/settings からグローバル既定値を収集する."""
    from infrastructure.config import get_settings

    settings = get_settings()

    llm_provider = "unknown"
    llm_model = "unknown"
    llm_base_url: str | None = None
    try:
        from infrastructure.llm.gateway.config import load_gateway_config

        gw_config = load_gateway_config()
        provider_bases = {provider.name: provider.api_base for provider in gw_config.providers}
        engine_bases = {engine.name: engine.base_url for engine in gw_config.inference_engines}
        default_role = gw_config.gateway.default_role
        default_alias = gw_config.registry.get(default_role)
        if default_alias:
            for model in gw_config.models:
                if model.alias == default_alias:
                    llm_provider = model.provider
                    llm_model = model.model
                    llm_base_url = model.api_base or engine_bases.get(str(model.engine or "").strip().lower())
                    if not llm_base_url:
                        llm_base_url = provider_bases.get(model.provider)
                    break
    except Exception:
        llm_config = settings.get_active_llm_config()
        llm_provider = str(llm_config.get("provider", "unknown"))
        llm_model = str(llm_config.get("model", "unknown"))
        llm_base_url = llm_config.get("base_url")

    db_config = settings.get_db_config()
    vectordb_config = settings.get_vectordb_config()

    embedding_model = settings.openai_embedding_model
    try:
        from infrastructure.llm.gateway.config import load_gateway_config as _load_gw

        gw_config = _load_gw()
        for model in gw_config.models:
            if model.alias == "platform_embedding_default" and model.model:
                embedding_model = model.model
                break
    except Exception:
        pass

    return {
        "llm": {
            "provider": llm_provider,
            "model": llm_model,
            "base_url": llm_base_url,
            "supported_models": [],
        },
        "db": {
            "backend": db_config.get("backend"),
            "url": db_config.get("url"),
        },
        "vectordb": {
            "backend": vectordb_config.get("backend"),
            "path": vectordb_config.get("persist_dir"),
            "collection": vectordb_config.get("collection"),
            "index": vectordb_config.get("index"),
        },
        "embedding": {
            "model": embedding_model,
        },
    }


def collect_startup_summary(
    *,
    app_config_path: str | Path | None = None,
    runtime_overrides: dict[str, Any] | None = None,
) -> dict[str, Any]:
    """起動ログに必要な設定サマリーを収集する."""
    summary = _collect_global_summary()
    summary = _merge_nested_dict(summary, _collect_app_config_overrides(app_config_path))
    if runtime_overrides:
        summary = _merge_nested_dict(summary, runtime_overrides)

    summary["llm"]["supported_models"] = _resolve_supported_models(app_config_path)
    return summary


def _log_supported_models(supported_models: list[dict[str, Any]]) -> None:
    """app-supported model refs を固定フォーマットで出力する."""
    if not supported_models:
        logger.info("[LLM] Supported Models (App): None declared")
        return

    grouped: dict[str, list[dict[str, Any]]] = defaultdict(list)
    for item in supported_models:
        grouped[str(item.get("model_type", "unknown"))].append(item)

    logger.info("[LLM] Supported Models (App):")
    for modality in _MODALITY_ORDER:
        for item in grouped.get(modality, []):
            model_id = _normalize_value(item.get("model_id"))
            provider = item.get("provider")
            model = item.get("model")
            if not item.get("resolved"):
                logger.info("  - %s: %s -> unresolved", modality, model_id)
                continue
            logger.info(
                "  - %s: %s -> %s / %s", modality, model_id, _normalize_value(provider), _normalize_value(model)
            )


def log_startup_info(
    app_name: str = "BizCore Application",
    extra_info: dict[str, Any] | None = None,
    *,
    app_config_path: str | Path | None = None,
    runtime_overrides: dict[str, Any] | None = None,
) -> dict[str, Any]:
    """起動時情報をログ出力.

    Args:
        app_name: アプリケーション名
        extra_info: 追加情報（agents, skills, rag_sources等）
        app_config_path: app_config.json のパス
        runtime_overrides: app から渡すローカル runtime 上書き

    Returns:
        設定情報辞書（プログラムで使用可能）
    """
    info = collect_startup_summary(
        app_config_path=app_config_path,
        runtime_overrides=runtime_overrides,
    )

    # ヘッダー
    logger.info("=" * 60)
    logger.info(f"{app_name} - 起動情報")
    logger.info("=" * 60)

    llm_info = info.get("llm", {}) if isinstance(info.get("llm"), dict) else {}
    db_info = info.get("db", {}) if isinstance(info.get("db"), dict) else {}
    vectordb_info = info.get("vectordb", {}) if isinstance(info.get("vectordb"), dict) else {}
    embedding_info = info.get("embedding", {}) if isinstance(info.get("embedding"), dict) else {}

    logger.info("[LLM] Active Provider: %s", _normalize_value(llm_info.get("provider"), fallback="unknown"))
    logger.info("[LLM] Active Model: %s", _normalize_value(llm_info.get("model"), fallback="unknown"))
    logger.info("[LLM] Base URL: %s", _mask_url(llm_info.get("base_url")))
    _log_supported_models(
        llm_info.get("supported_models", []) if isinstance(llm_info.get("supported_models"), list) else []
    )

    logger.info("[DB] Backend: %s", _normalize_value(db_info.get("backend")))
    logger.info("[DB] URL: %s", _mask_url(db_info.get("url")))

    logger.info("[VectorDB] Backend: %s", _normalize_value(vectordb_info.get("backend")))
    logger.info("[VectorDB] Path: %s", _normalize_value(vectordb_info.get("path")))
    logger.info("[VectorDB] Collection: %s", _normalize_value(vectordb_info.get("collection")))
    logger.info("[VectorDB] Index: %s", _normalize_value(vectordb_info.get("index")))

    logger.info("[Embedding] Model: %s", _normalize_value(embedding_info.get("model")))

    # 追加情報
    if extra_info:
        info["extra"] = extra_info
        if "agents" in extra_info:
            agents = extra_info["agents"]
            logger.info(f"[Agents] 登録数: {len(agents)}")
            for agent in agents[:10]:
                logger.info(f"  - {agent}")
            if len(agents) > 10:
                logger.info(f"  ... and {len(agents) - 10} more")

        if "skills" in extra_info:
            skills = extra_info["skills"]
            logger.info(f"[Skills] 登録数: {len(skills)}")
            for skill in skills[:5]:
                logger.info(f"  - {skill}")

        if "version" in extra_info:
            logger.info(f"[Version] {extra_info['version']}")

    logger.info("=" * 60)

    return info
