#!/usr/bin/env python3
"""通用网页正文提取脚本.

三段階フォールバック:
  1. Scrapling + html2text（反アンチクローラー、微信公众号対応）
  2. requests + html2text（軽量フォールバック）

用法（CLI）：
  python3 fetch.py <url> [max_chars]

用法（SkillRuntime）：
  result = fetch({"url": "https://...", "max_chars": 30000})
"""

import re
import sys

import html2text

from shared.web.markdown_pipeline import finalize_markdown


# --- scrapling の遅延インポート（依存不足時にフォールバック可能にする）---
_HAS_SCRAPLING = False
try:
    from scrapling.fetchers import Fetcher

    _HAS_SCRAPLING = True
except (ImportError, ModuleNotFoundError):
    pass


# ---------------------------------------------------------------------------
# 共通ユーティリティ
# ---------------------------------------------------------------------------


def fix_lazy_images(html_raw):
    """data-src 懒加载を src に昇格（微信公众号等）."""
    return re.sub(
        r'<img([^>]*?)\sdata-src="([^"]+)"([^>]*?)>',
        lambda m: f'<img{m.group(1)} src="{m.group(2)}"{m.group(3)}>',
        html_raw,
    )


def _make_converter():
    """html2text コンバータを生成."""
    h = html2text.HTML2Text()
    h.ignore_links = False
    h.ignore_images = False
    h.body_width = 0
    h.unicode_snob = True
    return h


def _clean(md, max_chars):
    """Markdown をクリーンアップ."""
    md = re.sub(r"\n{3,}", "\n\n", md).strip()
    return finalize_markdown(md, max_chars=max_chars)


# ---------------------------------------------------------------------------
# Strategy 1: Scrapling
# ---------------------------------------------------------------------------


def scrapling_fetch(url, max_chars=30000):
    """Scrapling + html2text で抽出."""
    page = Fetcher(auto_match=False).get(
        url,
        headers={"Referer": "https://www.google.com/search?q=site"},
    )

    h = _make_converter()

    if "mp.weixin.qq.com" in url:
        selectors = ["div#js_content", "div.rich_media_content"]
    else:
        selectors = [
            "article",
            "main",
            ".post-content",
            ".entry-content",
            ".article-body",
            '[class*="body"]',
            '[class*="content"]',
            '[class*="article"]',
        ]

    for selector in selectors:
        els = page.css(selector)
        if els:
            html_raw = fix_lazy_images(els[0].html_content)
            md = _clean(h.handle(html_raw), max_chars)
            if len(md) > 300:
                return md, f"scrapling:{selector}"

    html_raw = fix_lazy_images(page.html_content)
    return _clean(h.handle(html_raw), max_chars), "scrapling:body(fallback)"


# ---------------------------------------------------------------------------
# Strategy 2: requests + html2text（軽量フォールバック）
# ---------------------------------------------------------------------------


def requests_fetch(url, max_chars=30000):
    """requests + html2text で抽出（scrapling 不可時のフォールバック）."""
    import requests

    headers = {
        "User-Agent": (
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
            "AppleWebKit/537.36 (KHTML, like Gecko) "
            "Chrome/120.0.0.0 Safari/537.36"
        ),
        "Accept": "text/html,application/xhtml+xml",
        "Accept-Language": "zh-CN,zh;q=0.9,ja;q=0.8,en;q=0.7",
    }
    resp = requests.get(url, headers=headers, timeout=15)
    resp.encoding = "utf-8"

    html_raw = fix_lazy_images(resp.text)
    h = _make_converter()
    return _clean(h.handle(html_raw), max_chars), "requests"


# ---------------------------------------------------------------------------
# Public API（SkillRuntime から呼ばれる）
# ---------------------------------------------------------------------------


def fetch(input_data):
    """SkillRuntime 用エントリポイント.

    Args:
        input_data: {"url": str, "max_chars": int} または URL 文字列

    Returns:
        {"markdown": str, "method": str, "chars": int}
    """
    if isinstance(input_data, str):
        url, max_chars = input_data, 30000
    else:
        url = input_data.get("url", "")
        max_chars = input_data.get("max_chars", 30000)

    if not url:
        return {"error": "url is required", "markdown": "", "method": "none", "chars": 0}

    errors = []

    # Strategy 1: Scrapling
    if _HAS_SCRAPLING:
        try:
            md, method = scrapling_fetch(url, max_chars)
            if md and len(md) > 100:
                return {"markdown": md, "method": method, "chars": len(md)}
        except Exception as e:
            errors.append(f"scrapling: {e}")

    # Strategy 2: requests
    try:
        md, method = requests_fetch(url, max_chars)
        if md and len(md) > 100:
            return {"markdown": md, "method": method, "chars": len(md)}
    except Exception as e:
        errors.append(f"requests: {e}")

    return {
        "error": f"全戦略失敗: {'; '.join(errors)}",
        "markdown": "",
        "method": "none",
        "chars": 0,
    }


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("用法: python3 fetch.py <url> [max_chars]", file=sys.stderr)
        sys.exit(1)

    url = sys.argv[1]
    max_chars = int(sys.argv[2]) if len(sys.argv) > 2 else 30000

    result = fetch({"url": url, "max_chars": max_chars})
    if result.get("error"):
        print(f"Error: {result['error']}", file=sys.stderr)
        sys.exit(1)
    print(result["markdown"])
