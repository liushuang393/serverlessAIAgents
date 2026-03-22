"""shared.config public export tests."""

from __future__ import annotations


def test_shared_config_exports_get_settings() -> None:
    """`from shared.config import get_settings` should stay valid."""
    from shared.config import (
        AgentFlowSettings,
        get_settings,
        load_app_manifest_dict,
        load_app_manifest_dict_payload,
        load_app_manifest_dict_text,
        load_app_manifest_payload,
        load_app_manifest_text,
    )

    settings = get_settings()

    assert settings is not None
    assert isinstance(settings, AgentFlowSettings)
    assert load_app_manifest_text.__name__ == "load_app_manifest_text"
    assert load_app_manifest_payload.__name__ == "load_app_manifest_payload"
    assert load_app_manifest_dict.__name__ == "load_app_manifest_dict"
    assert load_app_manifest_dict_text.__name__ == "load_app_manifest_dict_text"
    assert load_app_manifest_dict_payload.__name__ == "load_app_manifest_dict_payload"
