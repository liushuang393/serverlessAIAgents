"""Static publishing utilities for generated GEO pages."""

from __future__ import annotations

import html
import json
from datetime import datetime, timezone
from pathlib import Path

from apps.legacy_modernization_geo_platform.backend.schemas import (
    ArtifactMeta,
    ContentDraftArtifact,
    PublishManifest,
    PublishedPageRecord,
    html_language_code,
    normalize_content_language,
)
from apps.legacy_modernization_geo_platform.backend.settings import GeoPlatformSettings


_PUBLIC_COPY: dict[str, dict[str, str]] = {
    "ja": {
        "eyebrow": "Legacy Modernization GEO",
        "guide_title": "導入ガイド",
        "faq_title": "よくある質問",
        "next_title": "次のアクション",
        "next_body": "無料診断から、対象システム・段階移行方針・投資対効果の仮説を整理します。",
    },
    "en": {
        "eyebrow": "Legacy Modernization GEO",
        "guide_title": "Implementation Guide",
        "faq_title": "Frequently Asked Questions",
        "next_title": "Next Action",
        "next_body": (
            "Start with a free assessment to align target systems, phased migration policy, and ROI hypotheses."
        ),
    },
    "zh": {
        "eyebrow": "Legacy Modernization GEO",
        "guide_title": "实施指南",
        "faq_title": "常见问题",
        "next_title": "下一步行动",
        "next_body": "从免费诊断开始，梳理目标系统、分阶段迁移策略与投资回报假设。",
    },
}


class GeoPublisher:
    """Render and store static HTML pages plus discovery files."""

    def __init__(self, settings: GeoPlatformSettings) -> None:
        """Initialize the publisher with output directories."""
        self._settings = settings

    def publish(self, task_id: str, draft: ContentDraftArtifact) -> PublishManifest:
        """Render pages and discovery metadata to disk."""
        target_language = normalize_content_language(draft.target_language)
        task_root = self._settings.published_dir / task_id
        pages_dir = task_root / "pages"
        pages_dir.mkdir(parents=True, exist_ok=True)

        records: list[PublishedPageRecord] = []
        for page in draft.pages:
            html_path = pages_dir / f"{page.slug}.html"
            html_path.write_text(self._render_page(page, target_language), encoding="utf-8")
            page_url = f"{self._settings.public_base_url}/geo/pages/{page.slug}"
            records.append(
                PublishedPageRecord(
                    slug=page.slug,
                    title=page.title,
                    page_url=page_url,
                    html_path=str(html_path),
                ),
            )

        sitemap_path = self._settings.published_dir / "sitemap.xml"
        sitemap_path.write_text(self._render_sitemap(records), encoding="utf-8")
        ai_feed_path = self._settings.published_dir / "ai-feed.json"
        ai_feed_path.write_text(self._render_ai_feed(records, draft), encoding="utf-8")

        return PublishManifest(
            meta=ArtifactMeta(task_id=task_id, trace_id=f"{task_id}:publish", stage="publish"),
            pages=records,
            sitemap_url=f"{self._settings.public_base_url}/geo/sitemap.xml",
            ai_feed_url=f"{self._settings.public_base_url}/geo/ai-feed.json",
            evidence=[{"page_url": item.page_url, "title": item.title} for item in records],
        )

    def _render_page(self, page: object, language: str) -> str:
        """Render a public GEO page with JSON-LD embedded."""
        normalized_language = normalize_content_language(language)
        copy = _PUBLIC_COPY[normalized_language]
        title = html.escape(getattr(page, "title"))
        summary = html.escape(getattr(page, "summary"))
        cta = html.escape(getattr(page, "cta"))
        body_markdown = getattr(page, "body_markdown")
        faq_entries = list(getattr(page, "faq_entries"))
        json_ld = json.dumps(getattr(page, "json_ld"), ensure_ascii=False, indent=2)
        body_html = "".join(
            f"<p>{html.escape(line)}</p>" for line in str(body_markdown).splitlines() if line.strip()
        )
        faq_html = "".join(
            (
                "<details class=\"faq-item\">"
                f"<summary>{html.escape(entry.question)}</summary>"
                f"<p>{html.escape(entry.answer)}</p>"
                "</details>"
            )
            for entry in faq_entries
        )
        return f"""<!DOCTYPE html>
<html lang="{html_language_code(normalized_language)}">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title>{title}</title>
    <meta name="description" content="{summary}" />
    <script type="application/ld+json">{json_ld}</script>
    <style>
      :root {{
        color-scheme: light;
        --bg: #f7f3ec;
        --ink: #172033;
        --muted: #5f6778;
        --line: rgba(23, 32, 51, 0.12);
        --card: rgba(255, 255, 255, 0.86);
        --accent: #ab3b2f;
      }}
      body {{
        margin: 0;
        font-family: "Space Grotesk", "Noto Sans JP", sans-serif;
        background:
          radial-gradient(circle at top left, rgba(171, 59, 47, 0.14), transparent 28rem),
          linear-gradient(180deg, #fffaf3 0%, var(--bg) 60%, #efe7dc 100%);
        color: var(--ink);
      }}
      main {{
        max-width: 58rem;
        margin: 0 auto;
        padding: 5rem 1.5rem 6rem;
      }}
      .hero, .section, .faq {{
        background: var(--card);
        border: 1px solid var(--line);
        border-radius: 1.5rem;
        box-shadow: 0 18px 48px rgba(23, 32, 51, 0.08);
        padding: 1.75rem;
        margin-bottom: 1.5rem;
      }}
      h1, h2 {{
        font-family: "IBM Plex Serif", "Noto Serif JP", serif;
      }}
      .eyebrow {{
        display: inline-block;
        margin-bottom: 0.8rem;
        color: var(--accent);
        font-size: 0.78rem;
        letter-spacing: 0.16em;
        text-transform: uppercase;
      }}
      .cta {{
        display: inline-flex;
        margin-top: 1rem;
        align-items: center;
        justify-content: center;
        min-height: 2.75rem;
        padding: 0 1.2rem;
        border-radius: 999px;
        color: #fff;
        background: var(--accent);
        text-decoration: none;
        font-weight: 700;
      }}
      .faq-item + .faq-item {{
        margin-top: 0.8rem;
      }}
      p {{
        line-height: 1.7;
        color: var(--muted);
      }}
    </style>
  </head>
  <body>
    <main>
      <section class="hero">
        <span class="eyebrow">{html.escape(copy["eyebrow"])}</span>
        <h1 data-testid="public-title">{title}</h1>
        <p>{summary}</p>
        <a class="cta" data-testid="public-hero-cta" href="#contact">{cta}</a>
      </section>
      <section class="section">
        <h2>{html.escape(copy["guide_title"])}</h2>
        {body_html}
      </section>
      <section class="faq" data-testid="public-faq">
        <h2>{html.escape(copy["faq_title"])}</h2>
        {faq_html}
      </section>
      <section class="section" id="contact">
        <h2>{html.escape(copy["next_title"])}</h2>
        <p>{html.escape(copy["next_body"])}</p>
        <a class="cta" data-testid="public-contact-cta" href="mailto:modernization@example.com">{cta}</a>
      </section>
    </main>
  </body>
</html>
"""

    def _render_sitemap(self, records: list[PublishedPageRecord]) -> str:
        """Build a minimal sitemap.xml document."""
        now = datetime.now(timezone.utc).date().isoformat()
        url_entries = "".join(
            f"<url><loc>{html.escape(record.page_url)}</loc><lastmod>{now}</lastmod></url>"
            for record in records
        )
        return (
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">"
            f"{url_entries}"
            "</urlset>"
        )

    def _render_ai_feed(
        self,
        records: list[PublishedPageRecord],
        draft: ContentDraftArtifact,
    ) -> str:
        """Build a simple AI-feed JSON document."""
        items = []
        for record in records:
            page = next((item for item in draft.pages if item.slug == record.slug), None)
            if page is None:
                continue
            items.append(
                {
                    "slug": record.slug,
                    "title": record.title,
                    "url": record.page_url,
                    "summary": page.summary,
                    "language": draft.target_language,
                    "faq": [entry.model_dump(mode="json") for entry in page.faq_entries],
                },
            )
        return json.dumps(
            {
                "generated_at": datetime.now(timezone.utc).isoformat(),
                "items": items,
            },
            ensure_ascii=False,
            indent=2,
        )
