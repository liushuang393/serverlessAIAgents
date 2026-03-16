"""agentflow.llm.contracts 後方互換スタブ. 実体は infrastructure.llm.contracts."""

from infrastructure.llm.contracts import (  # noqa: F401
    LLMContractBinding,
    LLMContractModelRef,
    LLMContractResolutionError,
    LLMContractsConfig,
    detect_calling_agent_name,
    detect_calling_app_name,
    load_app_llm_contracts,
    resolve_contract_model_alias,
    resolve_contract_model_ref,
    resolve_known_model_ids,
    resolve_known_providers,
)

__all__ = [
    "LLMContractBinding",
    "LLMContractModelRef",
    "LLMContractResolutionError",
    "LLMContractsConfig",
    "detect_calling_agent_name",
    "detect_calling_app_name",
    "load_app_llm_contracts",
    "resolve_contract_model_alias",
    "resolve_contract_model_ref",
    "resolve_known_model_ids",
    "resolve_known_providers",
]
