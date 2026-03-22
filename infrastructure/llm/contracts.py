"""Platform 主導 LLM 契約の解決ユーティリティ.

`apps/*/app_config.json` の `contracts.llm` を読み取り、
実行中アプリが使用可能な model_id / modality を解決する。
"""

from __future__ import annotations

import inspect
import json
import os
import re
from functools import lru_cache
from pathlib import Path
from typing import Literal

from pydantic import BaseModel, Field, field_validator

from infrastructure.llm.gateway import load_gateway_config


LLMContractModality = Literal[
    "text",
    "embedding",
    "image",
    "speech_to_text",
    "text_to_speech",
]

_MODALITY_VALUES: tuple[LLMContractModality, ...] = (
    "text",
    "embedding",
    "image",
    "speech_to_text",
    "text_to_speech",
)

_APP_PATH_PATTERN = re.compile(r"[/\\]apps[/\\]([a-zA-Z0-9_]+)[/\\]")
_DEFAULT_APPS_DIR = "apps"


class LLMContractResolutionError(RuntimeError):
    """LLM 契約解決に失敗した場合の例外."""


class LLMContractModelRef(BaseModel):
    """Platform catalog 上の model_id 参照."""

    provider: str = Field(..., min_length=1)
    model_id: str = Field(..., min_length=1)
    model_type: LLMContractModality = "text"

    @field_validator("provider", "model_id")
    @classmethod
    def normalize_lower(cls, value: str) -> str:
        return value.strip().lower()


class LLMContractBinding(BaseModel):
    """modality 単位の既定 binding."""

    text: LLMContractModelRef | None = None
    embedding: LLMContractModelRef | None = None
    image: LLMContractModelRef | None = None
    speech_to_text: LLMContractModelRef | None = None
    text_to_speech: LLMContractModelRef | None = None

    def get(self, modality: LLMContractModality) -> LLMContractModelRef | None:
        """指定 modality の参照を返す."""
        return getattr(self, modality)

    def defined_modalities(self) -> list[LLMContractModality]:
        """定義済み modality 一覧."""
        defined: list[LLMContractModality] = []
        for modality in _MODALITY_VALUES:
            if self.get(modality) is not None:
                defined.append(modality)
        return defined


class LLMContractsConfig(BaseModel):
    """app_config.json の contracts.llm."""

    enabled: bool = True
    defaults: LLMContractBinding = Field(default_factory=LLMContractBinding)
    agent_overrides: dict[str, LLMContractBinding] = Field(default_factory=dict)
    allowed_modalities: list[LLMContractModality] = Field(default_factory=list)
    extra_model_refs: list[LLMContractModelRef] = Field(default_factory=list)

    @field_validator("agent_overrides", mode="before")
    @classmethod
    def normalize_agent_overrides(cls, value: object) -> object:
        if not isinstance(value, dict):
            return value
        return {str(key).strip(): item for key, item in value.items() if str(key).strip()}

    def is_modality_allowed(self, modality: LLMContractModality) -> bool:
        """modality 使用可否."""
        if self.allowed_modalities:
            return modality in self.allowed_modalities
        return modality in self.defaults.defined_modalities()

    def resolve_ref(
        self,
        *,
        modality: LLMContractModality,
        agent_name: str | None = None,
    ) -> LLMContractModelRef | None:
        """agent override を考慮して model ref を解決."""
        if agent_name:
            binding = self.agent_overrides.get(agent_name)
            if binding is not None and binding.get(modality) is not None:
                return binding.get(modality)
        return self.defaults.get(modality)


def detect_calling_app_name() -> str | None:
    """呼び出しスタックから app 名を推定する."""
    from contracts.runtime.context import get_runtime_context

    runtime_context = get_runtime_context()
    if runtime_context is not None:
        metadata_app_name = str(runtime_context.metadata.get("app_name", "")).strip()
        if metadata_app_name:
            return metadata_app_name

    env_app_name = os.getenv("AGENTFLOW_APP_NAME", "").strip()
    if env_app_name:
        return env_app_name

    for frame_info in inspect.stack(context=0):
        filename = frame_info.filename
        match = _APP_PATH_PATTERN.search(filename)
        if match is None:
            continue
        app_name = match.group(1).strip()
        if app_name and app_name != "platform":
            return app_name
    return None


def detect_calling_agent_name() -> str | None:
    """呼び出しスタックから agent クラス名を推定する."""
    from contracts.runtime.context import get_runtime_context

    runtime_context = get_runtime_context()
    if runtime_context is not None:
        metadata_agent_name = str(runtime_context.metadata.get("agent_name", "")).strip()
        if metadata_agent_name:
            return metadata_agent_name

    env_agent_name = os.getenv("AGENTFLOW_AGENT_NAME", "").strip()
    if env_agent_name:
        return env_agent_name

    for frame_info in inspect.stack(context=0):
        local_self = frame_info.frame.f_locals.get("self")
        if local_self is None:
            continue
        class_name = getattr(local_self.__class__, "__name__", "").strip()
        if class_name:
            return class_name
    return None


def resolve_app_config_path(app_name: str, *, apps_dir: str | None = None) -> Path | None:
    """app_config.json のパスを解決する."""
    normalized = app_name.strip()
    if not normalized:
        return None

    candidates: list[Path] = []
    if apps_dir:
        candidates.append(Path(apps_dir) / normalized / "app_config.json")

    cwd = Path.cwd()
    candidates.extend(
        [
            cwd / _DEFAULT_APPS_DIR / normalized / "app_config.json",
            cwd / "app_config.json",
            Path(__file__).resolve().parents[2] / _DEFAULT_APPS_DIR / normalized / "app_config.json",
        ]
    )

    for path in candidates:
        if path.is_file():
            return path
    return None


@lru_cache(maxsize=128)
def _load_contracts_cached(config_path: str, mtime_ns: int) -> LLMContractsConfig | None:
    """mtime を含むキーで contracts.llm をキャッシュロードする."""
    path = Path(config_path)
    raw = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(raw, dict):
        return None
    contracts = raw.get("contracts")
    if not isinstance(contracts, dict):
        return None
    llm_contracts = contracts.get("llm")
    if not isinstance(llm_contracts, dict):
        return None
    return LLMContractsConfig.model_validate(llm_contracts)


def load_app_llm_contracts(
    app_name: str,
    *,
    apps_dir: str | None = None,
) -> LLMContractsConfig | None:
    """指定 app の contracts.llm を読み込む."""
    config_path = resolve_app_config_path(app_name, apps_dir=apps_dir)
    if config_path is None:
        return None
    stat = config_path.stat()
    return _load_contracts_cached(str(config_path), stat.st_mtime_ns)


def resolve_contract_model_ref(
    *,
    modality: LLMContractModality,
    app_name: str | None = None,
    agent_name: str | None = None,
) -> tuple[str, LLMContractModelRef] | None:
    """app/agent の contracts.llm から model ref を解決する."""
    resolved_app_name = app_name or detect_calling_app_name()
    if resolved_app_name is None:
        return None

    contracts = load_app_llm_contracts(resolved_app_name)
    if contracts is None or not contracts.enabled:
        return None

    if not contracts.is_modality_allowed(modality):
        msg = (
            f"App '{resolved_app_name}' では modality '{modality}' を許可していません。"
            " Platform の contracts.llm.allowed_modalities を確認してください。"
        )
        raise LLMContractResolutionError(msg)

    resolved_agent_name = agent_name or detect_calling_agent_name()
    ref = contracts.resolve_ref(modality=modality, agent_name=resolved_agent_name)
    if ref is None:
        msg = f"App '{resolved_app_name}' の contracts.llm に modality '{modality}' の既定 model_id がありません。"
        raise LLMContractResolutionError(msg)
    return resolved_app_name, ref


def resolve_contract_model_alias(
    *,
    modality: LLMContractModality,
    app_name: str | None = None,
    agent_name: str | None = None,
    gateway_config_path: Path | None = None,
) -> str | None:
    """contracts.llm の model_id から gateway alias を解決する."""
    resolved = resolve_contract_model_ref(
        modality=modality,
        app_name=app_name,
        agent_name=agent_name,
    )
    if resolved is None:
        return None

    _resolved_app_name, model_ref = resolved
    config = load_gateway_config(gateway_config_path)
    for model in config.models:
        if model_ref.model_id in (model.model_id, model.alias):
            return model.alias

    msg = (
        f"contracts.llm が参照する model_id '{model_ref.model_id}' は "
        "Platform catalog / gateway config に存在しません。"
    )
    raise LLMContractResolutionError(msg)


def resolve_known_model_ids(gateway_config_path: Path | None = None) -> set[str]:
    """gateway config に登録された model_id 一覧を返す."""
    config = load_gateway_config(gateway_config_path)
    ids = {model.alias for model in config.models}
    ids.update(model.model_id for model in config.models)
    return ids


def resolve_known_providers(gateway_config_path: Path | None = None) -> set[str]:
    """gateway config に登録された provider 一覧を返す."""
    config = load_gateway_config(gateway_config_path)
    return {provider.name for provider in config.providers}


__all__ = [
    "LLMContractBinding",
    "LLMContractModality",
    "LLMContractModelRef",
    "LLMContractResolutionError",
    "LLMContractsConfig",
    "detect_calling_agent_name",
    "detect_calling_app_name",
    "load_app_llm_contracts",
    "resolve_app_config_path",
    "resolve_contract_model_alias",
    "resolve_contract_model_ref",
    "resolve_known_model_ids",
    "resolve_known_providers",
]
