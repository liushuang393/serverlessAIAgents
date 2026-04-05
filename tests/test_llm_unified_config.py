"""LLM 統一設定管理のテスト.

.bizcore/llm_gateway.yaml と app_config.json による
フレームワーク→App 継承、プロバイダー切替、Ollama エンジン統合を検証する。
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

import pytest

from contracts.app.contracts import (
    LLMContractBinding,
    LLMContractConfig,
    LLMContractModelRef,
)
from control_plane.schemas.llm_management_schemas import (
    LLMBackendKind,
    LLMCatalogProvider,
    LLMProviderKind,
)
from control_plane.services.llm_management_catalog import LLMCatalogService
from control_plane.services.llm_management_validator import (
    LLMConfigValidator,
    provider_default_api_base,
    provider_default_api_key_env,
)
from infrastructure.llm.contracts import (
    load_app_llm_contracts,
    resolve_contract_model_ref,
)
from infrastructure.llm.gateway import (
    InferenceEngineConfig,
    LLMGatewayConfig,
    ModelConfig,
    ProviderConfig,
    load_gateway_config,
    save_gateway_config,
)


# ---------------------------------------------------------------------------
# 1. Enum 整合性テスト
# ---------------------------------------------------------------------------


class TestEnumConsistency:
    """Provider / Backend Enum が正しく分離されていること."""

    def test_ollama_not_in_provider_kind(self) -> None:
        """Ollama は Provider ではなく Backend に属する."""
        provider_values = {e.value for e in LLMProviderKind}
        assert "ollama" not in provider_values

    def test_ollama_in_backend_kind(self) -> None:
        """Ollama は Backend に含まれる."""
        backend_values = {e.value for e in LLMBackendKind}
        assert "ollama" in backend_values

    def test_huggingface_in_provider_kind(self) -> None:
        """HuggingFace はプロバイダーに含まれる."""
        provider_values = {e.value for e in LLMProviderKind}
        assert "huggingface" in provider_values

    def test_all_engines_in_backend_kind(self) -> None:
        """vllm, sglang, tgi, ollama がすべて Backend に含まれる."""
        backend_values = {e.value for e in LLMBackendKind}
        for engine in ("vllm", "sglang", "tgi", "ollama"):
            assert engine in backend_values, f"{engine} が LLMBackendKind にない"


# ---------------------------------------------------------------------------
# 2. 後方互換マイグレーションテスト
# ---------------------------------------------------------------------------


class TestBackwardCompatibility:
    """provider='ollama' → provider='local', engine='ollama' への自動変換."""

    def test_canonical_provider_map_ollama_to_local(self) -> None:
        """'ollama' が 'local' に正規化される."""
        v = LLMConfigValidator()
        assert v.canonical_provider_name("ollama") == "local"

    def test_canonical_provider_map_gemini_to_google(self) -> None:
        """'gemini' が 'google' に正規化される."""
        v = LLMConfigValidator()
        assert v.canonical_provider_name("gemini") == "google"

    def test_model_migration_ollama_provider(self) -> None:
        """provider='ollama' のモデルが自動的に engine='ollama' を取得."""
        v = LLMConfigValidator()
        model = ModelConfig(
            alias="test_ollama", provider="ollama", model="llama3.3:70b"
        )
        migrated = v._canonicalize_models([model])
        assert migrated[0].provider == "local"
        assert migrated[0].engine == "ollama"

    def test_model_migration_preserves_existing_engine(self) -> None:
        """既に engine が設定済みのモデルは engine を上書きしない."""
        v = LLMConfigValidator()
        model = ModelConfig(
            alias="test_local",
            provider="local",
            model="qwen2.5",
            engine="vllm",
        )
        migrated = v._canonicalize_models([model])
        assert migrated[0].provider == "local"
        assert migrated[0].engine == "vllm"

    def test_non_ollama_provider_unchanged(self) -> None:
        """'openai' 等のプロバイダーはそのまま."""
        v = LLMConfigValidator()
        model = ModelConfig(
            alias="test_openai", provider="openai", model="gpt-4o"
        )
        migrated = v._canonicalize_models([model])
        assert migrated[0].provider == "openai"
        assert migrated[0].engine is None


# ---------------------------------------------------------------------------
# 3. デフォルト設定テスト
# ---------------------------------------------------------------------------


class TestDefaultGatewayConfig:
    """デフォルト gateway 設定の正確性."""

    def test_ollama_engine_in_defaults(self) -> None:
        """Ollama エンジンがデフォルト設定に含まれる."""
        from infrastructure.llm.gateway.config import _default_gateway_config

        config = _default_gateway_config()
        engine_names = {e.name for e in config.inference_engines}
        assert "ollama" in engine_names

    def test_ollama_engine_config(self) -> None:
        """Ollama エンジンの設定値が正しい."""
        from infrastructure.llm.gateway.config import _default_gateway_config

        config = _default_gateway_config()
        ollama = next(e for e in config.inference_engines if e.name == "ollama")
        assert ollama.engine_type == "ollama"
        assert ollama.base_url == "http://127.0.0.1:11434"
        assert ollama.health_path == "/"
        assert ollama.model_list_path == "/api/tags"
        assert ollama.deployment_mode == "native"

    def test_huggingface_provider_in_defaults(self) -> None:
        """HuggingFace プロバイダーがデフォルト設定に含まれる."""
        from infrastructure.llm.gateway.config import _default_gateway_config

        config = _default_gateway_config()
        provider_names = {p.name for p in config.providers}
        assert "huggingface" in provider_names

    def test_default_models_use_valid_providers(self) -> None:
        """デフォルトモデルが有効なプロバイダーを参照している."""
        from infrastructure.llm.gateway.config import _default_gateway_config

        config = _default_gateway_config()
        provider_names = {p.name for p in config.providers}
        for model in config.models:
            assert model.provider in provider_names, (
                f"モデル '{model.alias}' のプロバイダー '{model.provider}' が未定義"
            )

    def test_platform_text_default_exists(self) -> None:
        """platform_text_default alias がデフォルト設定に存在する."""
        from infrastructure.llm.gateway.config import _default_gateway_config

        config = _default_gateway_config()
        aliases = {m.alias for m in config.models}
        assert "platform_text_default" in aliases


# ---------------------------------------------------------------------------
# 4. プロバイダー設定テスト
# ---------------------------------------------------------------------------


class TestProviderDefaults:
    """各プロバイダーのデフォルト API キー・ベース URL が正しい."""

    @pytest.mark.parametrize(
        ("provider", "expected_env"),
        [
            ("openai", "OPENAI_API_KEY"),
            ("anthropic", "ANTHROPIC_API_KEY"),
            ("google", "GEMINI_API_KEY"),
            ("huggingface", "HF_TOKEN"),
            ("azure_openai", "AZURE_OPENAI_API_KEY"),
            ("openrouter", "OPENROUTER_API_KEY"),
            ("deepseek", "DEEPSEEK_API_KEY"),
        ],
    )
    def test_provider_api_key_env(self, provider: str, expected_env: str) -> None:
        assert provider_default_api_key_env(provider) == expected_env

    def test_local_provider_no_api_key(self) -> None:
        """local プロバイダーは API キー不要."""
        assert provider_default_api_key_env("local") is None

    @pytest.mark.parametrize(
        ("provider", "expected_substring"),
        [
            ("openai", "openai.com"),
            ("anthropic", "anthropic.com"),
            ("google", "googleapis.com"),
            ("huggingface", "huggingface.co"),
            ("local", "127.0.0.1"),
        ],
    )
    def test_provider_api_base(self, provider: str, expected_substring: str) -> None:
        base = provider_default_api_base(provider)
        assert base is not None
        assert expected_substring in base


# ---------------------------------------------------------------------------
# 5. カタログテスト
# ---------------------------------------------------------------------------


class TestCatalog:
    """カタログ生成の正確性."""

    def test_ollama_in_backends(self) -> None:
        """Ollama がバックエンドカタログに含まれる."""
        catalog = LLMCatalogService().build()
        backend_names = [b.name.value for b in catalog.backends]
        assert "ollama" in backend_names

    def test_ollama_not_in_providers(self) -> None:
        """Ollama がプロバイダーカタログに含まれない."""
        catalog = LLMCatalogService().build()
        provider_names = [p.name.value for p in catalog.providers]
        assert "ollama" not in provider_names

    def test_local_provider_has_category(self) -> None:
        """local プロバイダーに category='local' が設定されている."""
        catalog = LLMCatalogService().build()
        local = next(
            (p for p in catalog.providers if p.name.value == "local"), None
        )
        assert local is not None
        assert local.category == "local"

    def test_local_provider_has_engines(self) -> None:
        """local プロバイダーに全ローカルエンジンが紐付いている."""
        catalog = LLMCatalogService().build()
        local = next(p for p in catalog.providers if p.name.value == "local")
        engine_values = {e.value for e in local.local_engines}
        assert engine_values >= {"vllm", "sglang", "tgi", "ollama"}

    def test_cloud_providers_have_cloud_category(self) -> None:
        """クラウドプロバイダーに category='cloud' が設定されている."""
        catalog = LLMCatalogService().build()
        for p in catalog.providers:
            if p.name.value in {"openai", "anthropic", "google"}:
                assert p.category == "cloud", (
                    f"{p.name.value} の category が 'cloud' ではない"
                )


# ---------------------------------------------------------------------------
# 6. コントラクト継承テスト
# ---------------------------------------------------------------------------


class TestContractResolution:
    """app_config.json の contracts.llm 解決ロジック."""

    def test_default_binding_resolution(self) -> None:
        """defaults から model ref を解決できる."""
        config = LLMContractConfig(
            enabled=True,
            defaults=LLMContractBinding(
                text=LLMContractModelRef(
                    provider="openai",
                    model_id="platform_text_default",
                )
            ),
        )
        ref = config.resolve_ref(modality="text")
        assert ref is not None
        assert ref.model_id == "platform_text_default"

    def test_agent_override_takes_precedence(self) -> None:
        """agent_overrides が defaults より優先される."""
        config = LLMContractConfig(
            enabled=True,
            defaults=LLMContractBinding(
                text=LLMContractModelRef(
                    provider="openai",
                    model_id="platform_text_default",
                )
            ),
            agent_overrides={
                "CodeAgent": LLMContractBinding(
                    text=LLMContractModelRef(
                        provider="openai",
                        model_id="coding_openai",
                    )
                )
            },
        )
        # Agent 指定あり → override
        ref = config.resolve_ref(modality="text", agent_name="CodeAgent")
        assert ref is not None
        assert ref.model_id == "coding_openai"

        # Agent 指定なし → defaults
        ref_default = config.resolve_ref(modality="text")
        assert ref_default is not None
        assert ref_default.model_id == "platform_text_default"

    def test_unknown_agent_falls_back_to_defaults(self) -> None:
        """未定義 Agent 名は defaults にフォールバック."""
        config = LLMContractConfig(
            enabled=True,
            defaults=LLMContractBinding(
                text=LLMContractModelRef(
                    provider="openai",
                    model_id="platform_text_default",
                )
            ),
            agent_overrides={
                "SpecialAgent": LLMContractBinding(
                    text=LLMContractModelRef(
                        provider="anthropic",
                        model_id="reasoning_claude",
                    )
                )
            },
        )
        ref = config.resolve_ref(modality="text", agent_name="OtherAgent")
        assert ref is not None
        assert ref.model_id == "platform_text_default"

    def test_modality_not_allowed(self) -> None:
        """allowed_modalities に含まれない modality は拒否."""
        config = LLMContractConfig(
            enabled=True,
            defaults=LLMContractBinding(
                text=LLMContractModelRef(
                    provider="openai",
                    model_id="platform_text_default",
                )
            ),
            allowed_modalities=["text"],
        )
        assert config.is_modality_allowed("text") is True
        assert config.is_modality_allowed("image") is False


# ---------------------------------------------------------------------------
# 7. Gateway 設定の保存・読み込みラウンドトリップ
# ---------------------------------------------------------------------------


class TestGatewayConfigPersistence:
    """gateway 設定の YAML 保存・読み込み."""

    def test_save_and_load_roundtrip(self, tmp_path: Path) -> None:
        """保存した設定を読み込んで同一内容が復元される."""
        config = LLMGatewayConfig(
            providers=[
                ProviderConfig(name="openai", api_base="https://api.openai.com/v1"),
                ProviderConfig(name="local", api_base="http://127.0.0.1:18001"),
            ],
            inference_engines=[
                InferenceEngineConfig(
                    name="ollama",
                    engine_type="ollama",
                    base_url="http://127.0.0.1:11434",
                    health_path="/",
                    deployment_mode="native",
                ),
            ],
            models=[
                ModelConfig(
                    alias="test_model",
                    provider="openai",
                    model="gpt-4o-mini",
                ),
                ModelConfig(
                    alias="local_ollama",
                    provider="local",
                    model="llama3.3:70b",
                    engine="ollama",
                ),
            ],
        )

        config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"
        save_gateway_config(config, config_path)
        loaded = load_gateway_config(config_path)

        assert len(loaded.providers) >= 2
        provider_names = {p.name for p in loaded.providers}
        assert "openai" in provider_names
        assert "local" in provider_names

        engine_names = {e.name for e in loaded.inference_engines}
        assert "ollama" in engine_names

        model_aliases = {m.alias for m in loaded.models}
        assert "test_model" in model_aliases
        assert "local_ollama" in model_aliases

    def test_provider_switch_preserves_alias(self, tmp_path: Path) -> None:
        """プロバイダーを切り替えてもエイリアスが保持される."""
        config = LLMGatewayConfig(
            providers=[
                ProviderConfig(name="openai", api_base="https://api.openai.com/v1"),
                ProviderConfig(name="anthropic", api_base="https://api.anthropic.com"),
            ],
            models=[
                ModelConfig(
                    alias="platform_text_default",
                    provider="openai",
                    model="gpt-4o-mini",
                ),
            ],
        )
        config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"
        save_gateway_config(config, config_path)

        # プロバイダーを Anthropic に切り替え
        loaded = load_gateway_config(config_path)
        for model in loaded.models:
            if model.alias == "platform_text_default":
                model.provider = "anthropic"
                model.model = "claude-haiku-4-5"
        save_gateway_config(loaded, config_path)

        reloaded = load_gateway_config(config_path)
        ptd = next(m for m in reloaded.models if m.alias == "platform_text_default")
        assert ptd.provider == "anthropic"
        assert ptd.model == "claude-haiku-4-5"


# ---------------------------------------------------------------------------
# 8. App 契約ファイル検証テスト
# ---------------------------------------------------------------------------


class TestAppConfigContracts:
    """実際の app_config.json が有効な gateway alias を参照しているか."""

    @staticmethod
    def _load_app_contract(app_name: str) -> dict[str, Any] | None:
        """app_config.json から contracts.llm を読み込む."""
        app_dir = Path(__file__).resolve().parents[1] / "apps" / app_name
        config_path = app_dir / "app_config.json"
        if not config_path.is_file():
            return None
        raw = json.loads(config_path.read_text(encoding="utf-8"))
        contracts = raw.get("contracts", {})
        return contracts.get("llm")

    @staticmethod
    def _gateway_model_ids() -> set[str]:
        """gateway config から有効な model_id / alias 一覧を取得."""
        from infrastructure.llm.gateway.config import _default_gateway_config

        config = _default_gateway_config()
        ids: set[str] = set()
        for m in config.models:
            ids.add(m.alias)
            if m.model_id is not None:
                ids.add(m.model_id)
        return ids

    @pytest.mark.parametrize(
        "app_name",
        [
            "faq_system",
            "messaging_hub",
            "decision_governance_engine",
            "market_trend_monitor",
            "design_skills_engine",
            "orchestration_guardian",
            "code_migration_assistant",
        ],
    )
    def test_app_contract_refs_valid(self, app_name: str) -> None:
        """各 App の contracts.llm が有効な gateway alias を参照している."""
        llm_contract = self._load_app_contract(app_name)
        if llm_contract is None:
            pytest.skip(f"{app_name} に contracts.llm がない")
        if not llm_contract.get("enabled", True):
            pytest.skip(f"{app_name} の LLM 契約が無効")

        valid_ids = self._gateway_model_ids()

        # defaults の検証
        defaults = llm_contract.get("defaults", {})
        for modality, binding in defaults.items():
            model_id = binding.get("model_id", "")
            assert model_id in valid_ids, (
                f"{app_name}: defaults.{modality}.model_id='{model_id}' "
                f"が gateway に存在しない（有効: {sorted(valid_ids)}）"
            )

        # agent_overrides の検証
        overrides = llm_contract.get("agent_overrides", {})
        for agent_name, bindings in overrides.items():
            for modality, binding in bindings.items():
                model_id = binding.get("model_id", "")
                assert model_id in valid_ids, (
                    f"{app_name}: agent_overrides.{agent_name}.{modality}"
                    f".model_id='{model_id}' が gateway に存在しない"
                )


# ---------------------------------------------------------------------------
# 9. Validator 統合テスト
# ---------------------------------------------------------------------------


class TestValidatorIntegration:
    """LLMConfigValidator のクロスフィールド検証."""

    def test_valid_config_passes(self) -> None:
        """正常な設定はバリデーションを通過する."""
        config = LLMGatewayConfig(
            providers=[
                ProviderConfig(name="openai"),
                ProviderConfig(name="local"),
            ],
            inference_engines=[
                InferenceEngineConfig(
                    name="ollama",
                    engine_type="ollama",
                    base_url="http://127.0.0.1:11434",
                ),
            ],
            models=[
                ModelConfig(
                    alias="default",
                    provider="openai",
                    model="gpt-4o-mini",
                    enabled=True,
                ),
                ModelConfig(
                    alias="local_llama",
                    provider="local",
                    model="llama3.3:70b",
                    engine="ollama",
                    enabled=True,
                ),
            ],
            registry={"reasoning": "default", "local": "local_llama"},
        )
        v = LLMConfigValidator()
        errors = v.validate(config)
        assert errors == [], f"バリデーションエラー: {errors}"

    def test_invalid_engine_reference_detected(self) -> None:
        """存在しないエンジンを参照するモデルはエラーになる."""
        config = LLMGatewayConfig(
            providers=[ProviderConfig(name="local")],
            models=[
                ModelConfig(
                    alias="bad",
                    provider="local",
                    model="test",
                    engine="nonexistent_engine",
                    enabled=True,
                ),
            ],
        )
        v = LLMConfigValidator()
        errors = v.validate(config)
        assert any("nonexistent_engine" in e for e in errors)

    def test_duplicate_port_detected(self) -> None:
        """同一ポートのエンジンはエラーになる."""
        config = LLMGatewayConfig(
            providers=[ProviderConfig(name="local")],
            inference_engines=[
                InferenceEngineConfig(
                    name="engine1",
                    engine_type="vllm",
                    base_url="http://127.0.0.1:18001",
                    host_port=18001,
                ),
                InferenceEngineConfig(
                    name="engine2",
                    engine_type="sglang",
                    base_url="http://127.0.0.1:18001",
                    host_port=18001,
                ),
            ],
        )
        v = LLMConfigValidator()
        errors = v.validate(config)
        assert any("host_port" in e for e in errors)
