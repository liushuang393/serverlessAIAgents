"""ContentDraftAgent - コンテンツドラフト生成."""

from __future__ import annotations

from typing import Any

from apps.legacy_modernization_geo_platform.agents._models import (
    ContentDraftInput,
    ContentDraftOutput,
)
from apps.legacy_modernization_geo_platform.backend.schemas import (
    ArtifactMeta,
    ContentDraftArtifact,
    ContentDraftPage,
    FAQEntry,
    normalize_content_language,
    schema_language_code,
)

from kernel.agents.resilient_agent import ResilientAgent


_COPY: dict[str, dict[str, str]] = {
    "ja": {
        "default_question": "段階移行は可能か",
        "evidence_prefix": "根拠",
        "sections.background": "課題の背景",
        "sections.approach": "段階移行の考え方",
        "sections.comparison": "比較観点",
        "sections.evidence": "根拠サマリー",
        "sections.semantics": "業務意味の維持ポイント",
        "sections.next": "次の一手",
        "paragraph.background": "旧システム刷新では、保守要員不足・調査難易度・一括刷新リスクが同時に発生します。",
        "paragraph.approach": "既存資産を可視化し、業務ルールを抽出してから API・バッチ・画面を順次刷新します。",
        "paragraph.comparison": "全面刷新と比較すると、段階刷新は業務停止リスクを抑えつつ投資判断を分割できます。",
        "paragraph.next": "{question} を起点に、対象資産の棚卸しと優先順位付けから着手します。",
        "faq.1.answer": "段階移行は現行業務の安定運用と並行して実施できます。",
        "faq.2.question": "一括刷新との違いは何ですか",
        "faq.2.answer": "投資とリスクを工程ごとに分割できる点です。",
        "faq.3.question": "どこから調査を始めるべきですか",
        "faq.3.answer": "高頻度バッチ、帳票、外部連携を優先して可視化します。",
        "summary_pattern": "{stack} の刷新を、診断・業務ルール抽出・段階移行で進めるための実務ガイド。",
    },
    "en": {
        "default_question": "Can we migrate in phases?",
        "evidence_prefix": "Evidence",
        "sections.background": "Background",
        "sections.approach": "Phased Migration Approach",
        "sections.comparison": "Comparison Lens",
        "sections.evidence": "Evidence Summary",
        "sections.semantics": "Business Semantics Guardrails",
        "sections.next": "Next Step",
        "paragraph.background": (
            "Legacy modernization often combines talent shortage, discovery complexity, and big-bang delivery risk."
        ),
        "paragraph.approach": (
            "Visualize the current assets, extract business rules, and modernize APIs, batch jobs, and UI step by step."
        ),
        "paragraph.comparison": (
            "Compared with full replacement, phased modernization reduces outage risk and splits investment decisions."
        ),
        "paragraph.next": "Start from \"{question}\" and prioritize asset inventory and sequencing.",
        "faq.1.answer": "Phased migration can run alongside stable day-to-day operations.",
        "faq.2.question": "How is this different from a full rewrite?",
        "faq.2.answer": "Investment and risk can be controlled milestone by milestone.",
        "faq.3.question": "Where should investigation start?",
        "faq.3.answer": "Start with high-frequency batch jobs, reports, and external integrations.",
        "summary_pattern": (
            "A practical guide to modernize {stack} with diagnostics, business-rule extraction, and phased migration."
        ),
    },
    "zh": {
        "default_question": "能否进行分阶段迁移？",
        "evidence_prefix": "依据",
        "sections.background": "问题背景",
        "sections.approach": "分阶段迁移思路",
        "sections.comparison": "对比视角",
        "sections.evidence": "依据摘要",
        "sections.semantics": "业务语义保持要点",
        "sections.next": "下一步动作",
        "paragraph.background": "旧系统现代化通常同时面临维护人才不足、调研复杂度高与一次性重构风险。",
        "paragraph.approach": "先完成资产可视化与业务规则提取，再按 API、批处理、页面逐步迁移。",
        "paragraph.comparison": "相比一次性重构，分阶段迁移可以降低业务中断风险并拆分投资决策。",
        "paragraph.next": "以“{question}”为起点，先做资产盘点与优先级排序。",
        "faq.1.answer": "分阶段迁移可以与现行业务稳定运行并行推进。",
        "faq.2.question": "与一次性重构有什么区别？",
        "faq.2.answer": "可以按阶段分摊投入，并逐段控制风险。",
        "faq.3.question": "应该从哪里开始调研？",
        "faq.3.answer": "优先梳理高频批处理、报表与外部系统集成链路。",
        "summary_pattern": "面向 {stack} 现代化的实务指南：诊断、业务规则提取与分阶段迁移。",
    },
}


class ContentDraftAgent(ResilientAgent[ContentDraftInput, ContentDraftOutput]):
    """決定論的ページドラフトを生成."""

    name = "ContentDraft"
    timeout_seconds = 60
    max_retries = 1
    enable_code_execution = False

    async def process(self, input_data: ContentDraftInput) -> ContentDraftOutput:
        request = input_data.request
        task_id = input_data.task_id
        blueprint = input_data.blueprint
        evidence_matrix = input_data.evidence_matrix
        legacy_semantics = input_data.legacy_semantics
        target_language = normalize_content_language(blueprint.target_language)
        copy = _COPY[target_language]
        pages: list[ContentDraftPage] = []
        question = (
            blueprint.pages[0].primary_question if blueprint.pages else copy["default_question"]
        )
        evidence_lines = [
            f"- {copy['evidence_prefix']}: {entry.title} / {entry.publisher} / {entry.source_url}"
            for entry in evidence_matrix.entries[:4]
        ]
        semantics_lines = [f"- {item}" for item in legacy_semantics.business_rules]
        for page in blueprint.pages:
            body_markdown = "\n".join(
                [
                    f"# {page.title}",
                    "",
                    f"## {copy['sections.background']}",
                    copy["paragraph.background"],
                    "",
                    f"## {copy['sections.approach']}",
                    copy["paragraph.approach"],
                    "",
                    f"## {copy['sections.comparison']}",
                    copy["paragraph.comparison"],
                    "",
                    f"## {copy['sections.evidence']}",
                    *evidence_lines,
                    "",
                    f"## {copy['sections.semantics']}",
                    *semantics_lines,
                    "",
                    f"## {copy['sections.next']}",
                    copy["paragraph.next"].format(question=question),
                ],
            )
            faq_entries = [
                FAQEntry(
                    question=question,
                    answer=copy["faq.1.answer"],
                ),
                FAQEntry(
                    question=copy["faq.2.question"],
                    answer=copy["faq.2.answer"],
                ),
                FAQEntry(
                    question=copy["faq.3.question"],
                    answer=copy["faq.3.answer"],
                ),
            ]
            json_ld = {
                "@context": "https://schema.org",
                "@type": "FAQPage",
                "inLanguage": schema_language_code(target_language),
                "mainEntity": [
                    {
                        "@type": "Question",
                        "name": entry.question,
                        "acceptedAnswer": {
                            "@type": "Answer",
                            "text": entry.answer,
                        },
                    }
                    for entry in faq_entries
                ],
            }
            pages.append(
                ContentDraftPage(
                    slug=page.slug,
                    title=page.title,
                    summary=copy["summary_pattern"].format(
                        stack=(request.targets.legacy_stacks or ["legacy"])[0],
                    ),
                    body_markdown=body_markdown,
                    cta=page.cta,
                    faq_entries=faq_entries,
                    json_ld=json_ld,
                ),
            )
        artifact = ContentDraftArtifact(
            meta=ArtifactMeta(
                task_id=task_id,
                trace_id=f"{task_id}:content",
                stage="content_composition",
            ),
            pages=pages,
            target_language=target_language,
            evidence=[
                item.model_dump(mode="json")
                for item in evidence_matrix.entries[:6]
            ],
            unknowns=list(evidence_matrix.unknowns),
        )
        return ContentDraftOutput(artifact=artifact)

    def _parse_input(self, input_data: dict[str, Any]) -> ContentDraftInput:
        return ContentDraftInput.model_validate(input_data)
