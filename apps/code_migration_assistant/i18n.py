"""i18n Utility for Code Migration Assistant."""

import json
from pathlib import Path
from typing import Any


class I18n:
    def __init__(self, locale: str = "ja"):
        self.locale = locale
        self.translations: dict[str, Any] = {}
        self._load_translations()

    def _load_translations(self):
        locale_path = Path(__file__).parent / "locales" / f"{self.locale}.json"
        if locale_path.exists():
            with open(locale_path, encoding="utf-8") as f:
                self.translations = json.load(f)

    def t(self, key: str, default: str | None = None) -> str:
        """Translate a key."""
        parts = key.split(".")
        val = self.translations
        for p in parts:
            if isinstance(val, dict) and p in val:
                val = val[p]
            else:
                return default or key
        return str(val)


_i18n_instances: dict[str, I18n] = {}

def get_i18n(locale: str = "ja") -> I18n:
    if locale not in _i18n_instances:
        _i18n_instances[locale] = I18n(locale)
    return _i18n_instances[locale]
