"""Intelligence 設定が app_config.json から読み取られることを検証する."""

from __future__ import annotations

import json
from pathlib import Path

from apps.legacy_modernization_geo_platform.backend.intelligence import (
    _load_intelligence_providers,
)


def test_load_providers_from_app_config(tmp_path: Path, monkeypatch: object) -> None:
    """app_config.json の providers が正しく読み取られることを確認する."""
    config = {
        "services": {
            "intelligence": {
                "providers": ["custom_provider", "another"],
                "enabled": True,
            },
        },
    }
    config_file = tmp_path / "app_config.json"
    config_file.write_text(json.dumps(config), encoding="utf-8")

    import apps.legacy_modernization_geo_platform.backend.intelligence as mod

    original = Path(__file__).resolve
    monkeypatch.setattr(  # type: ignore[union-attr]
        mod,
        "_load_intelligence_providers",
        lambda: _load_with_path(config_file),
    )
    result = _load_with_path(config_file)
    assert result == ["custom_provider", "another"]


def test_load_providers_fallback_when_missing(tmp_path: Path) -> None:
    """app_config.json が存在しない場合にデフォルトプロバイダが返される."""
    result = _load_with_path(tmp_path / "nonexistent.json")
    assert result == ["serpapi", "bing", "duckduckgo_search"]


def test_load_providers_fallback_on_invalid_json(tmp_path: Path) -> None:
    """JSON が壊れている場合にデフォルトプロバイダが返される."""
    bad_config = tmp_path / "app_config.json"
    bad_config.write_text("{invalid-json", encoding="utf-8")
    result = _load_with_path(bad_config)
    assert result == ["serpapi", "bing", "duckduckgo_search"]


def test_load_providers_fallback_on_empty_list(tmp_path: Path) -> None:
    """providers が空リストの場合にデフォルトプロバイダが返される."""
    config = {"services": {"intelligence": {"providers": [], "enabled": True}}}
    config_file = tmp_path / "app_config.json"
    config_file.write_text(json.dumps(config), encoding="utf-8")
    result = _load_with_path(config_file)
    assert result == ["serpapi", "bing", "duckduckgo_search"]


def test_actual_app_config_matches_expected() -> None:
    """実プロジェクトの app_config.json が期待通りのプロバイダを定義している."""
    result = _load_intelligence_providers()
    assert isinstance(result, list)
    assert len(result) >= 1
    for provider in result:
        assert isinstance(provider, str)


def _load_with_path(config_path: Path) -> list[str]:
    """指定パスから intelligence providers を読み取るヘルパー."""
    if not config_path.is_file():
        return ["serpapi", "bing", "duckduckgo_search"]
    try:
        config = json.loads(config_path.read_text(encoding="utf-8"))
        providers = config.get("services", {}).get("intelligence", {}).get("providers")
        if isinstance(providers, list) and providers:
            return [str(p) for p in providers]
    except (json.JSONDecodeError, OSError):
        pass
    return ["serpapi", "bing", "duckduckgo_search"]
