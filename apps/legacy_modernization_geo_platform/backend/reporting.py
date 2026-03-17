"""Report assembly for GEO execution runs."""

from __future__ import annotations

from typing import Any

from apps.legacy_modernization_geo_platform.backend.schemas import (
    AccountSignalArtifact,
    CampaignReport,
    ArtifactMeta,
    GeoExecuteRequest,
    GeoQAReport,
    PublishManifest,
)


def build_campaign_report(
    *,
    task_id: str,
    request: GeoExecuteRequest,
    signal_artifact: AccountSignalArtifact,
    qa_report: GeoQAReport,
    publish_manifest: PublishManifest,
    provider_status: dict[str, Any],
) -> tuple[CampaignReport, str, dict[str, Any]]:
    """Build both the persisted report artifact and response summary."""
    published_urls = [page.page_url for page in publish_manifest.pages]
    highlights = [
        f"対象業界: {', '.join(request.targets.industries or ['general'])}",
        f"対象技術: {', '.join(request.targets.legacy_stacks or ['legacy modernization'])}",
        f"一次プロバイダ: {provider_status.get('primary_provider', 'none')}",
        f"公開ページ数: {len(publish_manifest.pages)}",
        f"リスクレベル: {qa_report.risk_level}",
    ]
    warnings = list(signal_artifact.unknowns)
    warnings.extend(qa_report.issues)
    markdown_lines = [
        "# Legacy Modernization GEO Platform Report",
        "",
        f"- Task ID: `{task_id}`",
        f"- Campaign: `{request.campaign_name}`",
        f"- Package: `{request.package}`",
        f"- Published pages: {len(publish_manifest.pages)}",
        f"- QA risk: {qa_report.risk_level}",
        "",
        "## Highlights",
        "",
    ]
    markdown_lines.extend(f"- {item}" for item in highlights)
    markdown_lines.extend(["", "## Published URLs", ""])
    markdown_lines.extend(f"- {url}" for url in published_urls or ["- none"])
    markdown_lines.extend(["", "## Warnings", ""])
    markdown_lines.extend(f"- {item}" for item in warnings or ["- none"])
    markdown = "\n".join(markdown_lines)
    summary = {
        "campaign_name": request.campaign_name,
        "package": request.package,
        "published_urls": published_urls,
        "risk_level": qa_report.risk_level,
        "primary_provider": provider_status.get("primary_provider", "none"),
        "warning_count": len(warnings),
    }
    artifact = CampaignReport(
        meta=ArtifactMeta(task_id=task_id, trace_id=f"{task_id}:report", stage="report"),
        summary="Legacy modernization demand capture run complete.",
        highlights=highlights,
        warnings=warnings,
        published_urls=published_urls,
        evidence=[{"url": url} for url in published_urls],
    )
    return artifact, markdown, summary
