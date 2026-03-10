"""contracts.llm 解決のユニットテスト."""

from __future__ import annotations

import json
from typing import TYPE_CHECKING

import pytest

from agentflow.llm.contracts import (
    LLMContractResolutionError,
    load_app_llm_contracts,
    resolve_contract_model_alias,
    resolve_contract_model_ref,
)
from agentflow.llm.gateway import load_gateway_config


if TYPE_CHECKING:
    from pathlib import Path


def _write_app_config(config_path: Path, payload: dict[str, object]) -> None:
    config_path.parent.mkdir(parents=True, exist_ok=True)
    config_path.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")


def test_load_app_llm_contracts_reads_defaults(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.chdir(tmp_path)
    app_config = tmp_path / "apps" / "faq_system" / "app_config.json"
    _write_app_config(
        app_config,
        {
            "contracts": {
                "llm": {
                    "enabled": True,
                    "defaults": {
                        "text": {"provider": "openai", "model_id": "platform_text_default", "model_type": "text"},
                        "embedding": {
                            "provider": "openai",
                            "model_id": "platform_embedding_default",
                            "model_type": "embedding",
                        },
                    },
                    "agent_overrides": {
                        "MigratorAgent": {
                            "text": {"provider": "openai", "model_id": "coding_openai", "model_type": "text"}
                        }
                    },
                    "allowed_modalities": ["text", "embedding"],
                    "extra_model_refs": [],
                }
            }
        },
    )

    contracts = load_app_llm_contracts("faq_system")

    assert contracts is not None
    assert contracts.defaults.text is not None
    assert contracts.defaults.text.model_id == "platform_text_default"
    assert contracts.agent_overrides["MigratorAgent"].text is not None


def test_resolve_contract_model_alias_prefers_agent_override(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    monkeypatch.chdir(tmp_path)
    app_config = tmp_path / "apps" / "code_migration_assistant" / "app_config.json"
    gateway_path = tmp_path / ".agentflow" / "llm_gateway.yaml"
    load_gateway_config(gateway_path)
    _write_app_config(
        app_config,
        {
            "contracts": {
                "llm": {
                    "enabled": True,
                    "defaults": {
                        "text": {"provider": "openai", "model_id": "platform_text_default", "model_type": "text"}
                    },
                    "agent_overrides": {
                        "MigratorAgent": {
                            "text": {"provider": "openai", "model_id": "coding_openai", "model_type": "text"}
                        }
                    },
                    "allowed_modalities": ["text"],
                    "extra_model_refs": [],
                }
            }
        },
    )

    default_alias = resolve_contract_model_alias(
        modality="text",
        app_name="code_migration_assistant",
        gateway_config_path=gateway_path,
    )
    override_alias = resolve_contract_model_alias(
        modality="text",
        app_name="code_migration_assistant",
        agent_name="MigratorAgent",
        gateway_config_path=gateway_path,
    )

    assert default_alias == "platform_text_default"
    assert override_alias == "coding_openai"


def test_resolve_contract_model_ref_rejects_disallowed_modality(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    monkeypatch.chdir(tmp_path)
    app_config = tmp_path / "apps" / "faq_system" / "app_config.json"
    _write_app_config(
        app_config,
        {
            "contracts": {
                "llm": {
                    "enabled": True,
                    "defaults": {
                        "text": {"provider": "openai", "model_id": "platform_text_default", "model_type": "text"}
                    },
                    "agent_overrides": {},
                    "allowed_modalities": ["text"],
                    "extra_model_refs": [],
                }
            }
        },
    )

    with pytest.raises(LLMContractResolutionError, match="modality 'image'"):
        resolve_contract_model_ref(modality="image", app_name="faq_system")
