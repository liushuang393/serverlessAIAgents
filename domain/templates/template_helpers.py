"""Local helper entrypoints for domain template packages.

Domain templates should import parsing helpers from this module instead of
reaching into ``shared.utils`` directly. That keeps fixes localized when the
response parsing contract changes.
"""

from __future__ import annotations

from typing import Any

from shared.utils import extract_json


def extract_json_payload(raw_response: str) -> dict[str, Any] | None:
    """Parse a model response into a dict for domain template consumers."""
    payload = extract_json(raw_response)
    return payload if isinstance(payload, dict) else None


__all__ = ["extract_json_payload"]
