"""Web ドメイン制御."""

from __future__ import annotations

from urllib.parse import urlparse


def _extract_host(url: str) -> str:
    parsed = urlparse(url)
    return parsed.netloc.split(":")[0].lower()


def is_domain_allowed(url: str, allowed_domains: list[str] | None, blocked_domains: list[str] | None) -> bool:
    """ドメイン許可判定."""
    host = _extract_host(url)
    if not host:
        return False

    blocked = [d.lower().strip() for d in (blocked_domains or []) if d.strip()]
    if any(host == domain or host.endswith(f".{domain}") for domain in blocked):
        return False

    allowed = [d.lower().strip() for d in (allowed_domains or []) if d.strip()]
    if not allowed:
        return True
    return any(host == domain or host.endswith(f".{domain}") for domain in allowed)


def filter_allowed_urls(
    urls: list[str],
    allowed_domains: list[str] | None,
    blocked_domains: list[str] | None,
) -> list[str]:
    """許可ドメインに基づいて URL をフィルタする."""
    return [url for url in urls if is_domain_allowed(url, allowed_domains, blocked_domains)]
