"""shared.config public export tests."""

from __future__ import annotations


def test_shared_config_exports_get_settings() -> None:
    """`from shared.config import get_settings` should stay valid."""
    from shared.config import AgentFlowSettings
    from shared.config import get_settings
    from shared.config import load_app_manifest_dict
    from shared.config import load_app_manifest_dict_payload
    from shared.config import load_app_manifest_dict_text
    from shared.config import load_app_manifest_payload
    from shared.config import load_app_manifest_text

    settings = get_settings()

    assert settings is not None
    assert isinstance(settings, AgentFlowSettings)
    assert load_app_manifest_text.__name__ == "load_app_manifest_text"
    assert load_app_manifest_payload.__name__ == "load_app_manifest_payload"
    assert load_app_manifest_dict.__name__ == "load_app_manifest_dict"
    assert load_app_manifest_dict_text.__name__ == "load_app_manifest_dict_text"
    assert load_app_manifest_dict_payload.__name__ == "load_app_manifest_dict_payload"
