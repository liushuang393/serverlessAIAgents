"""Report assembly for GEO execution runs."""

from __future__ import annotations

from typing import Any

from apps.legacy_modernization_geo_platform.backend.schemas import (
    AccountSignalArtifact,
    ArtifactMeta,
    CampaignReport,
    GeoExecuteRequest,
    GeoQAReport,
    PublishManifest,
    normalize_content_language,
)


_REPORT_COPY: dict[str, dict[str, str]] = {
    "ja": {
        "title": "Legacy Modernization GEO プラットフォームレポート",
        "label_task": "Task ID",
        "label_campaign": "キャンペーン",
        "label_package": "パッケージ",
        "label_published_pages": "公開ページ数",
        "label_qa_risk": "QA リスク",
        "section_highlights": "ハイライト",
        "section_urls": "公開 URL",
        "section_warnings": "注意点",
        "highlight_industries": "対象業界",
        "highlight_stacks": "対象技術",
        "highlight_provider": "一次プロバイダ",
        "highlight_page_count": "公開ページ数",
        "highlight_risk": "リスクレベル",
        "none": "なし",
        "summary": "旧システム刷新向け需要捕捉ランが完了しました。",
    },
    "en": {
        "title": "Legacy Modernization GEO Platform Report",
        "label_task": "Task ID",
        "label_campaign": "Campaign",
        "label_package": "Package",
        "label_published_pages": "Published pages",
        "label_qa_risk": "QA risk",
        "section_highlights": "Highlights",
        "section_urls": "Published URLs",
        "section_warnings": "Warnings",
        "highlight_industries": "Target industries",
        "highlight_stacks": "Target stacks",
        "highlight_provider": "Primary provider",
        "highlight_page_count": "Published page count",
        "highlight_risk": "Risk level",
        "none": "none",
        "summary": "Legacy modernization demand-capture run completed.",
    },
    "zh": {
        "title": "Legacy Modernization GEO 平台报告",
        "label_task": "任务 ID",
        "label_campaign": "活动",
        "label_package": "套餐",
        "label_published_pages": "已发布页面",
        "label_qa_risk": "QA 风险",
        "section_highlights": "关键摘要",
        "section_urls": "发布链接",
        "section_warnings": "注意事项",
        "highlight_industries": "目标行业",
        "highlight_stacks": "目标技术栈",
        "highlight_provider": "主数据源",
        "highlight_page_count": "发布页数量",
        "highlight_risk": "风险等级",
        "none": "无",
        "summary": "面向旧系统现代化的需求捕获任务已完成。",
    },
}


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
    language = normalize_content_language((request.inputs.content_languages or ["ja"])[0])
    copy = _REPORT_COPY[language]
    published_urls = [page.page_url for page in publish_manifest.pages]
    highlights = [
        f"{copy['highlight_industries']}: {', '.join(request.targets.industries or ['general'])}",
        f"{copy['highlight_stacks']}: {', '.join(request.targets.legacy_stacks or ['legacy modernization'])}",
        f"{copy['highlight_provider']}: {provider_status.get('primary_provider', copy['none'])}",
        f"{copy['highlight_page_count']}: {len(publish_manifest.pages)}",
        f"{copy['highlight_risk']}: {qa_report.risk_level}",
    ]
    warnings = list(signal_artifact.unknowns)
    warnings.extend(qa_report.issues)
    markdown_lines = [
        f"# {copy['title']}",
        "",
        f"- {copy['label_task']}: `{task_id}`",
        f"- {copy['label_campaign']}: `{request.campaign_name}`",
        f"- {copy['label_package']}: `{request.package}`",
        f"- {copy['label_published_pages']}: {len(publish_manifest.pages)}",
        f"- {copy['label_qa_risk']}: {qa_report.risk_level}",
        "",
        f"## {copy['section_highlights']}",
        "",
    ]
    markdown_lines.extend(f"- {item}" for item in highlights)
    markdown_lines.extend(["", f"## {copy['section_urls']}", ""])
    markdown_lines.extend(f"- {url}" for url in published_urls or [f"- {copy['none']}"])
    markdown_lines.extend(["", f"## {copy['section_warnings']}", ""])
    markdown_lines.extend(f"- {item}" for item in warnings or [f"- {copy['none']}"])
    markdown = "\n".join(markdown_lines)
    summary = {
        "campaign_name": request.campaign_name,
        "package": request.package,
        "published_urls": published_urls,
        "risk_level": qa_report.risk_level,
        "primary_provider": provider_status.get("primary_provider", copy["none"]),
        "language": language,
        "warning_count": len(warnings),
    }
    artifact = CampaignReport(
        meta=ArtifactMeta(task_id=task_id, trace_id=f"{task_id}:report", stage="report"),
        summary=copy["summary"],
        highlights=highlights,
        warnings=warnings,
        published_urls=published_urls,
        evidence=[{"url": url} for url in published_urls],
    )
    return artifact, markdown, summary
