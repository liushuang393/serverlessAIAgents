"""AppConfig Pydantic スキーマのユニットテスト.

テスト対象: apps/platform/schemas/app_config_schemas.py
"""

from __future__ import annotations

import pytest
from apps.platform.schemas.app_config_schemas import (
    AgentInfo,
    AppConfig,
    DependenciesConfig,
    EntryPointsConfig,
    PortsConfig,
)
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
        assert agent.pattern is None

    def test_normalize_taxonomy_fields(self) -> None:
        """分類フィールドを小文字で正規化する."""
        agent = AgentInfo(name="A", business_base="Knowledge", pattern="Coordinator")
        assert agent.business_base == "knowledge"
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
