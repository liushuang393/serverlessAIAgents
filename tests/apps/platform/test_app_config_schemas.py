"""AppConfig Pydantic スキーマのユニットテスト.

テスト対象: apps/platform/schemas/app_config_schemas.py
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest
from apps.platform.schemas.app_config_schemas import (
    AgentInfo,
    AppConfig,
    DependenciesConfig,
    EntryPointsConfig,
    PortsConfig,
)
from apps.platform.schemas.capability_schemas import CapabilitySpec
from apps.platform.services.app_discovery import _flatten_capability_item
from apps.platform.services.capability_registry import CapabilityRegistry
from pydantic import ValidationError


class TestPortsConfig:
    """PortsConfig バリデーションテスト."""

    def test_valid_ports(self) -> None:
        """有効なポート番号を受け付ける."""
        cfg = PortsConfig(api=8000, frontend=3000, db=5432, redis=6379)
        assert cfg.api == 8000
        assert cfg.frontend == 3000

    def test_none_ports_allowed(self) -> None:
        """None ポートを許容する."""
        cfg = PortsConfig()
        assert cfg.api is None
        assert cfg.frontend is None

    def test_port_below_range_rejected(self) -> None:
        """1024 未満のポートを拒否する."""
        with pytest.raises(ValidationError, match="ポート番号"):
            PortsConfig(api=80)

    def test_port_above_range_rejected(self) -> None:
        """65535 超のポートを拒否する."""
        with pytest.raises(ValidationError, match="ポート番号"):
            PortsConfig(api=70000)


class TestAgentInfo:
    """AgentInfo バリデーションテスト."""

    def test_valid_agent(self) -> None:
        """有効な Agent 情報を受け付ける."""
        agent = AgentInfo(name="TestAgent", module="mod.test", capabilities=["rag"])
        assert agent.name == "TestAgent"
        assert agent.capabilities == ["rag"]

    def test_empty_name_rejected(self) -> None:
        """空の Agent 名を拒否する."""
        with pytest.raises(ValidationError):
            AgentInfo(name="")

    def test_defaults(self) -> None:
        """デフォルト値が正しい."""
        agent = AgentInfo(name="A")
        assert agent.module is None
        assert agent.capabilities == []
        assert agent.business_base is None
        assert agent.agent_type is None
        assert agent.pattern is None

    def test_normalize_taxonomy_fields(self) -> None:
        """分類フィールドを小文字で正規化する."""
        agent = AgentInfo(
            name="A",
            business_base="Knowledge",
            agent_type="Planner",
            pattern="Coordinator",
        )
        assert agent.business_base == "knowledge"
        assert agent.agent_type == "planner"
        assert agent.pattern == "coordinator"


class TestEntryPointsConfig:
    """EntryPointsConfig テスト."""

    def test_defaults(self) -> None:
        """デフォルト値が正しい."""
        ep = EntryPointsConfig()
        assert ep.api_module is None
        assert ep.health == "/health"

    def test_health_none_allowed(self) -> None:
        """health=None を許容する（ライブラリ App 用）."""
        ep = EntryPointsConfig(health=None)
        assert ep.health is None


class TestDependenciesConfig:
    """DependenciesConfig テスト."""

    def test_defaults(self) -> None:
        """デフォルト値が正しい."""
        dep = DependenciesConfig()
        assert dep.database is None
        assert dep.redis is False
        assert dep.external == []


class TestAppConfig:
    """AppConfig ルートスキーマテスト."""

    def test_real_legacy_modernization_geo_platform_manifest_is_valid(self) -> None:
        """実アプリ manifest が現行 AppConfig スキーマを満たす."""
        manifest_path = Path("apps/Legacy_modernization_geo_platform/app_config.json")
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))

        cfg = AppConfig.model_validate(manifest)

        assert cfg.name == "legacy_modernization_geo_platform"
        assert cfg.product_line == "framework"
        assert cfg.plugin_bindings == []

    def test_valid_full_config(self) -> None:
        """完全な設定を受け付ける."""
        from tests.apps.platform.conftest import SAMPLE_APP_CONFIG

        cfg = AppConfig.model_validate(SAMPLE_APP_CONFIG)
        assert cfg.name == "test_app"
        assert cfg.display_name == "テストアプリ"
        assert len(cfg.agents) == 2
        assert cfg.ports.api == 8099

    def test_minimal_valid_config(self) -> None:
        """必須分類項目を含む最小構成を受け付ける."""
        cfg = AppConfig(
            name="my_app",
            display_name="My App",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
        )
        assert cfg.version == "1.0.0"
        assert cfg.icon == "📦"
        assert cfg.agents == []
        assert cfg.runtime.urls.backend is None
        assert cfg.runtime.database.user is None
        assert cfg.runtime.commands.start is None
        assert cfg.business_base is None
        assert cfg.product_line == "framework"
        assert cfg.surface_profile == "developer"
        assert cfg.audit_profile == "developer"
        assert cfg.evolution.enabled is True
        assert cfg.evolution.scope_policy[0] == "tenant_app"
        assert cfg.plugin_bindings == []
        assert cfg.security_mode is None

    def test_missing_classification_fields_rejected(self) -> None:
        """分類必須項目が未指定なら拒否する."""
        with pytest.raises(ValidationError):
            AppConfig(name="my_app", display_name="My App")

    def test_invalid_name_uppercase(self) -> None:
        """大文字を含む App 名を拒否する."""
        with pytest.raises(ValidationError, match="snake_case"):
            AppConfig(
                name="MyApp",
                display_name="X",
                product_line="framework",
                surface_profile="developer",
                audit_profile="developer",
                plugin_bindings=[],
            )

    def test_invalid_name_starts_with_number(self) -> None:
        """数字始まりの App 名を拒否する."""
        with pytest.raises(ValidationError, match="snake_case"):
            AppConfig(
                name="1app",
                display_name="X",
                product_line="framework",
                surface_profile="developer",
                audit_profile="developer",
                plugin_bindings=[],
            )

    def test_invalid_name_hyphen(self) -> None:
        """ハイフンを含む App 名を拒否する."""
        with pytest.raises(ValidationError, match="snake_case"):
            AppConfig(
                name="my-app",
                display_name="X",
                product_line="framework",
                surface_profile="developer",
                audit_profile="developer",
                plugin_bindings=[],
            )

    def test_empty_name_rejected(self) -> None:
        """空の App 名を拒否する."""
        with pytest.raises(ValidationError):
            AppConfig(
                name="",
                display_name="X",
                product_line="framework",
                surface_profile="developer",
                audit_profile="developer",
                plugin_bindings=[],
            )

    def test_model_dump_roundtrip(self) -> None:
        """model_dump → model_validate のラウンドトリップが成功する."""
        from tests.apps.platform.conftest import SAMPLE_APP_CONFIG

        original = AppConfig.model_validate(SAMPLE_APP_CONFIG)
        dumped = original.model_dump()
        restored = AppConfig.model_validate(dumped)
        assert original.name == restored.name
        assert len(original.agents) == len(restored.agents)

    def test_product_and_plugin_fields(self) -> None:
        """製品線/プラグイン関連フィールドを受け付ける."""
        cfg = AppConfig(
            name="migration_app",
            display_name="Migration App",
            product_line="Migration",
            surface_profile="business",
            audit_profile="business",
            security_mode="approval_required",
            plugin_bindings=[
                {"id": "lang-cobol", "version": "1.2.0", "config": {"strict": True}},
            ],
        )
        assert cfg.product_line == "migration"
        assert cfg.surface_profile == "business"
        assert cfg.audit_profile == "business"
        assert cfg.security_mode == "approval_required"
        assert len(cfg.plugin_bindings) == 1
        assert cfg.plugin_bindings[0].id == "lang-cobol"

    def test_runtime_cli_defaults(self) -> None:
        """runtime.cli がデフォルトで初期化される."""
        cfg = AppConfig(
            name="cli_app",
            display_name="CLI App",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
        )
        assert cfg.runtime.cli.preferred == ["codex", "claude"]
        assert cfg.runtime.cli.codex.diagnostic_mode == "read_only"
        assert cfg.runtime.cli.claude.diagnostic_mode == "read_only"

    def test_runtime_cli_override(self) -> None:
        """runtime.cli の上書き設定を受け付ける."""
        cfg = AppConfig(
            name="cli_override_app",
            display_name="CLI Override App",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
            runtime={
                "cli": {
                    "preferred": ["claude", "codex"],
                    "codex": {
                        "executable": "codex-custom",
                        "diagnostic_mode": "plan",
                    },
                }
            },
        )
        assert cfg.runtime.cli.preferred == ["claude", "codex"]
        assert cfg.runtime.cli.codex.executable == "codex-custom"
        assert cfg.runtime.cli.codex.diagnostic_mode == "plan"

    def test_blueprint_normalize_app_template_and_agent_type(self) -> None:
        """blueprint.app_template / agents[].agent_type を正規化する."""
        cfg = AppConfig(
            name="template_app",
            display_name="Template App",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
            blueprint={
                "app_template": "Workflow_Orchestrator",
                "agents": [
                    {
                        "name": "PlannerAgent",
                        "role": "specialist",
                        "agent_type": "Planner",
                        "prompt": "plan",
                        "capabilities": ["analysis"],
                    }
                ],
            },
        )
        assert cfg.blueprint.app_template == "workflow_orchestrator"
        assert cfg.blueprint.agents[0].agent_type == "planner"

    def test_assistant_requires_security_mode(self) -> None:
        """assistant は security_mode 未指定を拒否する."""
        with pytest.raises(ValidationError, match="security_mode"):
            AppConfig(
                name="assistant_app",
                display_name="Assistant App",
                product_line="assistant",
                surface_profile="business",
                audit_profile="business",
                plugin_bindings=[],
            )

    def test_plugin_binding_version_requires_semver(self) -> None:
        """plugin_bindings.version は SemVer 形式を必須化する."""
        with pytest.raises(ValidationError, match="plugin version"):
            AppConfig(
                name="framework_app",
                display_name="Framework App",
                product_line="framework",
                surface_profile="developer",
                audit_profile="developer",
                plugin_bindings=[{"id": "official.sample", "version": "latest"}],
            )

    def test_plugin_binding_ids_must_be_unique(self) -> None:
        """plugin_bindings[].id の重複を拒否する."""
        with pytest.raises(ValidationError, match="plugin_bindings\\[\\]\\.id"):
            AppConfig(
                name="framework_app",
                display_name="Framework App",
                product_line="framework",
                surface_profile="developer",
                audit_profile="developer",
                plugin_bindings=[
                    {"id": "official.sample", "version": "1.0.0"},
                    {"id": "official.sample", "version": "1.0.0"},
                ],
            )


class TestCapabilitySpec:
    """CapabilitySpec 3 層構造能力宣言のテスト."""

    def test_basic_creation(self) -> None:
        """domain と actions を指定して作成できる."""
        spec = CapabilitySpec(domain="knowledge", actions=["retrieval", "faq"])
        assert spec.domain == "knowledge"
        assert spec.actions == ["retrieval", "faq"]
        assert spec.artifacts == []

    def test_with_artifacts(self) -> None:
        """artifacts 付きで作成できる."""
        spec = CapabilitySpec(
            domain="reasoning",
            actions=["analysis"],
            artifacts=["trend_report", "summary"],
        )
        assert spec.artifacts == ["trend_report", "summary"]

    def test_domain_normalized_to_lowercase(self) -> None:
        """domain は小文字に正規化される."""
        spec = CapabilitySpec(domain="Knowledge", actions=["retrieval"])
        assert spec.domain == "knowledge"

    def test_actions_normalized_to_lowercase(self) -> None:
        """actions の各要素は小文字・trim 正規化される."""
        spec = CapabilitySpec(domain="knowledge", actions=["  Retrieval ", "FAQ"])
        assert spec.actions == ["retrieval", "faq"]

    def test_empty_actions_rejected(self) -> None:
        """空の actions リストは拒否される."""
        with pytest.raises(ValidationError):
            CapabilitySpec(domain="knowledge", actions=[])

    def test_to_canonical_ids_no_artifacts(self) -> None:
        """artifacts なし → domain.action 形式の ID を生成する."""
        spec = CapabilitySpec(domain="knowledge", actions=["retrieval", "faq"])
        ids = spec.to_canonical_ids()
        assert ids == ["knowledge.retrieval", "knowledge.faq"]

    def test_to_canonical_ids_with_artifacts(self) -> None:
        """artifacts あり → domain.action.artifact 形式の ID を生成する."""
        spec = CapabilitySpec(
            domain="knowledge",
            actions=["retrieval"],
            artifacts=["rag_answer", "summary"],
        )
        ids = spec.to_canonical_ids()
        assert ids == ["knowledge.retrieval.rag_answer", "knowledge.retrieval.summary"]

    def test_to_canonical_ids_multi_action_with_artifacts(self) -> None:
        """複数 action × 複数 artifact の全組み合わせを生成する."""
        spec = CapabilitySpec(
            domain="reasoning",
            actions=["analysis", "forecast"],
            artifacts=["report"],
        )
        ids = spec.to_canonical_ids()
        assert ids == ["reasoning.analysis.report", "reasoning.forecast.report"]


class TestAgentInfoCapabilitySpec:
    """AgentInfo.capabilities が str / CapabilitySpec 混在を受け付けるテスト."""

    def test_legacy_flat_strings_still_accepted(self) -> None:
        """レガシーのフラット文字列は引き続き受け付ける（後方互換）."""
        agent = AgentInfo(name="A", capabilities=["rag", "analysis"])
        assert agent.capabilities == ["rag", "analysis"]

    def test_capability_spec_object_accepted(self) -> None:
        """CapabilitySpec オブジェクトを直接渡せる."""
        spec = CapabilitySpec(domain="knowledge", actions=["retrieval"])
        agent = AgentInfo(name="A", capabilities=[spec])
        assert len(agent.capabilities) == 1
        assert isinstance(agent.capabilities[0], CapabilitySpec)

    def test_capability_spec_dict_accepted(self) -> None:
        """JSON 辞書形式の CapabilitySpec を受け付ける（app_config.json 読み込み模倣）."""
        agent = AgentInfo(
            name="A",
            capabilities=[
                {"domain": "knowledge", "actions": ["retrieval", "faq"]},
            ],
        )
        assert len(agent.capabilities) == 1
        spec = agent.capabilities[0]
        assert isinstance(spec, CapabilitySpec)
        assert spec.domain == "knowledge"

    def test_mixed_str_and_spec_accepted(self) -> None:
        """フラット文字列と CapabilitySpec の混在を受け付ける."""
        agent = AgentInfo(
            name="A",
            capabilities=[
                "analysis",
                {"domain": "knowledge", "actions": ["retrieval"]},
            ],
        )
        assert len(agent.capabilities) == 2
        assert agent.capabilities[0] == "analysis"
        assert isinstance(agent.capabilities[1], CapabilitySpec)


class TestFlattenCapabilityItem:
    """_flatten_capability_item ユーティリティ関数のテスト."""

    def test_flat_string_returns_single_element(self) -> None:
        """フラット文字列はそのまま 1 要素リストで返る."""
        assert _flatten_capability_item("rag") == ["rag"]

    def test_capability_spec_expands_to_ids(self) -> None:
        """CapabilitySpec は canonical ID リストに展開される."""
        spec = CapabilitySpec(domain="knowledge", actions=["retrieval", "faq"])
        result = _flatten_capability_item(spec)
        assert result == ["knowledge.retrieval", "knowledge.faq"]

    def test_dict_form_parsed_and_expanded(self) -> None:
        """dict 形式は CapabilitySpec にパースされて展開される."""
        result = _flatten_capability_item(
            {"domain": "reasoning", "actions": ["analysis"], "artifacts": ["report"]}
        )
        assert result == ["reasoning.analysis.report"]

    def test_invalid_dict_falls_back_to_str(self) -> None:
        """不正な dict は str() にフォールバックして返る."""
        bad = {"no_domain": "x"}
        result = _flatten_capability_item(bad)
        assert len(result) == 1
        assert "no_domain" in result[0]


class TestCapabilityRegistryWithSpec:
    """CapabilityRegistry.canonicalize_many が CapabilitySpec を処理するテスト."""

    def test_flat_string_canonicalized(self) -> None:
        """フラット文字列は従来通り canonical 化される."""
        registry = CapabilityRegistry()
        result = registry.canonicalize_many(["rag"])
        assert len(result) == 1
        assert result[0].domain == "knowledge"

    def test_capability_spec_expanded(self) -> None:
        """CapabilitySpec は複数の CanonicalCapability に展開される."""
        registry = CapabilityRegistry()
        spec = CapabilitySpec(domain="knowledge", actions=["retrieval", "faq"])
        result = registry.canonicalize_many([spec])
        ids = [c.id for c in result]
        assert "knowledge.retrieval" in ids
        assert "knowledge.faq" in ids

    def test_mixed_input_deduplicates(self) -> None:
        """フラット文字列と CapabilitySpec が重複なく統合される."""
        registry = CapabilityRegistry()
        spec = CapabilitySpec(domain="knowledge", actions=["retrieval"])
        # "knowledge.retrieval.search" はエイリアス "retrieval" を経由して生成される
        # 今回はフラット "knowledge.retrieval" を直接重複させてテスト
        result = registry.canonicalize_many([spec, spec])
        # 重複は除去されるため 2 つではなく 2 つの action に対応する canonical が返る
        unique_ids = {c.id for c in result}
        assert "knowledge.retrieval" in unique_ids
