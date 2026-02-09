"""メンテナンス支援Agent.

保守担当者向けの変更作業支援Agent。
仕様差分・影響分析・成果物自動生成を実現。

設計原則:
- 仕様差分の自動抽出
- 影響範囲の網羅的分析
- 成果物のカバレッジ提示

使用例:
    >>> from apps.faq_system.backend.agents import MaintenanceAgent
    >>>
    >>> agent = MaintenanceAgent()
    >>> result = await agent.analyze_diff(old_doc, new_doc)
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass, field
from datetime import datetime
from difflib import unified_diff
from typing import Any

from pydantic import BaseModel, Field

from agentflow.core import ResilientAgent


logger = logging.getLogger(__name__)


# =============================================================================
# 型定義
# =============================================================================


class DiffCategory(str):
    """差分カテゴリ."""

    INTERFACE = "interface"
    FIELD = "field"
    RULE = "rule"
    EXCEPTION = "exception"
    FORMAT = "format"
    OTHER = "other"


class DiffSeverity(str):
    """差分深刻度."""

    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"


@dataclass
class DiffResult:
    """差分結果."""

    category: str
    severity: str
    old_value: str
    new_value: str
    location: str
    description: str
    impact_note: str = ""


@dataclass
class ImpactAnalysis:
    """影響分析結果."""

    affected_modules: list[str] = field(default_factory=list)
    affected_apis: list[str] = field(default_factory=list)
    affected_tables: list[str] = field(default_factory=list)
    affected_tests: list[str] = field(default_factory=list)
    affected_runbooks: list[str] = field(default_factory=list)
    affected_faqs: list[str] = field(default_factory=list)
    coverage_info: str = ""
    confidence: float = 0.8


@dataclass
class MaintenanceConfig:
    """メンテナンス支援設定."""

    # 差分抽出設定
    diff_categories: list[str] = field(default_factory=lambda: [
        DiffCategory.INTERFACE,
        DiffCategory.FIELD,
        DiffCategory.RULE,
        DiffCategory.EXCEPTION,
    ])
    ignore_whitespace: bool = True
    min_change_significance: float = 0.1

    # 影響分析設定
    enable_module_analysis: bool = True
    enable_api_analysis: bool = True
    enable_db_analysis: bool = True
    enable_test_analysis: bool = True

    # 成果物生成設定
    generate_release_note: bool = True
    generate_faq_draft: bool = True
    generate_training_notice: bool = True
    generate_test_checklist: bool = True

    # カバレッジ提示必須
    require_coverage_disclosure: bool = True


class MaintenanceResponse(BaseModel):
    """メンテナンス支援レスポンス."""

    action: str = ""
    diffs: list[dict[str, Any]] = Field(default_factory=list)
    impact: dict[str, Any] = Field(default_factory=dict)
    deliverables: dict[str, str] = Field(default_factory=dict)
    coverage_info: str = ""
    execution_time_ms: float = 0
    error: str = ""


# =============================================================================
# メンテナンス支援Agent
# =============================================================================


class MaintenanceAgent(ResilientAgent):
    """メンテナンス支援Agent.

    特徴:
    - 仕様差分の自動抽出
    - 影響範囲分析
    - 成果物自動生成
    - カバレッジ提示
    """

    name = "MaintenanceAgent"

    def __init__(
        self,
        config: MaintenanceConfig | None = None,
    ) -> None:
        """初期化."""
        super().__init__()
        self._config = config or MaintenanceConfig()
        self._logger = logging.getLogger(self.name)

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """Agent 実行."""
        start_time = datetime.now()
        action = input_data.get("action", "diff")

        try:
            if action == "diff":
                result = await self._handle_diff(input_data)
            elif action == "impact":
                result = await self._handle_impact(input_data)
            elif action == "generate":
                result = await self._handle_generate(input_data)
            elif action == "full":
                result = await self._handle_full_analysis(input_data)
            else:
                return MaintenanceResponse(
                    error=f"Unknown action: {action}"
                ).model_dump()

            result.action = action
            result.execution_time_ms = (
                datetime.now() - start_time
            ).total_seconds() * 1000

            return result.model_dump()

        except Exception as e:
            self._logger.exception("MaintenanceAgent エラー: %s", e)
            return MaintenanceResponse(error=str(e)).model_dump()

    async def _handle_diff(
        self, input_data: dict[str, Any]
    ) -> MaintenanceResponse:
        """差分分析."""
        old_doc = input_data.get("old_document", "")
        new_doc = input_data.get("new_document", "")

        if not old_doc or not new_doc:
            return MaintenanceResponse(
                error="old_document と new_document が必要です"
            )

        diffs = await self.analyze_diff(old_doc, new_doc)

        return MaintenanceResponse(
            diffs=[self._diff_to_dict(d) for d in diffs],
        )

    async def _handle_impact(
        self, input_data: dict[str, Any]
    ) -> MaintenanceResponse:
        """影響分析."""
        diffs = input_data.get("diffs", [])

        if not diffs:
            return MaintenanceResponse(
                error="diffs が必要です"
            )

        diff_results = [
            DiffResult(**d) if isinstance(d, dict) else d
            for d in diffs
        ]

        impact = await self.analyze_impact(diff_results)

        return MaintenanceResponse(
            impact=self._impact_to_dict(impact),
            coverage_info=impact.coverage_info,
        )

    async def _handle_generate(
        self, input_data: dict[str, Any]
    ) -> MaintenanceResponse:
        """成果物生成."""
        diffs = input_data.get("diffs", [])
        impact = input_data.get("impact", {})

        diff_results = [
            DiffResult(**d) if isinstance(d, dict) else d
            for d in diffs
        ]

        impact_result = ImpactAnalysis(**impact) if impact else ImpactAnalysis()

        deliverables = await self.generate_deliverables(diff_results, impact_result)

        return MaintenanceResponse(
            deliverables=deliverables,
            coverage_info=impact_result.coverage_info,
        )

    async def _handle_full_analysis(
        self, input_data: dict[str, Any]
    ) -> MaintenanceResponse:
        """完全分析（差分→影響→成果物）."""
        old_doc = input_data.get("old_document", "")
        new_doc = input_data.get("new_document", "")

        if not old_doc or not new_doc:
            return MaintenanceResponse(
                error="old_document と new_document が必要です"
            )

        # 1. 差分分析
        diffs = await self.analyze_diff(old_doc, new_doc)

        # 2. 影響分析
        impact = await self.analyze_impact(diffs)

        # 3. 成果物生成
        deliverables = await self.generate_deliverables(diffs, impact)

        return MaintenanceResponse(
            diffs=[self._diff_to_dict(d) for d in diffs],
            impact=self._impact_to_dict(impact),
            deliverables=deliverables,
            coverage_info=impact.coverage_info,
        )

    async def analyze_diff(
        self, old_doc: str, new_doc: str
    ) -> list[DiffResult]:
        """仕様差分分析.

        Args:
            old_doc: 旧ドキュメント
            new_doc: 新ドキュメント

        Returns:
            差分結果リスト
        """
        diffs = []

        # 1. 行単位の差分
        old_lines = old_doc.splitlines()
        new_lines = new_doc.splitlines()

        diff_lines = list(unified_diff(
            old_lines, new_lines,
            fromfile="old", tofile="new",
            lineterm="",
        ))

        # 2. 差分を分類
        current_section = ""
        for line in diff_lines:
            if line.startswith("@@"):
                match = re.search(r"@@ -(\d+)", line)
                if match:
                    current_section = f"Line {match.group(1)}"
            elif line.startswith("-") and not line.startswith("---"):
                old_value = line[1:]
                # カテゴリを推定
                category = self._classify_change(old_value)
                severity = self._estimate_severity(old_value, category)

                diffs.append(DiffResult(
                    category=category,
                    severity=severity,
                    old_value=old_value,
                    new_value="",
                    location=current_section,
                    description=f"削除: {old_value[:50]}...",
                ))
            elif line.startswith("+") and not line.startswith("+++"):
                new_value = line[1:]
                category = self._classify_change(new_value)
                severity = self._estimate_severity(new_value, category)

                diffs.append(DiffResult(
                    category=category,
                    severity=severity,
                    old_value="",
                    new_value=new_value,
                    location=current_section,
                    description=f"追加: {new_value[:50]}...",
                ))

        return diffs

    async def analyze_impact(
        self, diffs: list[DiffResult]
    ) -> ImpactAnalysis:
        """影響範囲分析.

        Args:
            diffs: 差分結果リスト

        Returns:
            影響分析結果
        """
        impact = ImpactAnalysis()
        analyzed_sources = []

        for diff in diffs:
            # インターフェース変更
            if diff.category == DiffCategory.INTERFACE:
                impact.affected_apis.append(
                    f"API変更の可能性: {diff.description}"
                )

            # フィールド変更
            if diff.category == DiffCategory.FIELD:
                impact.affected_tables.append(
                    f"DB変更の可能性: {diff.description}"
                )

            # ルール変更
            if diff.category == DiffCategory.RULE:
                impact.affected_modules.append(
                    f"ビジネスロジック変更: {diff.description}"
                )
                impact.affected_tests.append(
                    f"テスト更新必要: {diff.description}"
                )

            analyzed_sources.append(diff.location)

        # カバレッジ情報
        impact.coverage_info = (
            f"本分析は以下に基づいています:\n"
            f"- 分析対象: {len(diffs)}件の変更点\n"
            f"- 分析範囲: {', '.join(set(analyzed_sources)) or '全体'}\n"
            f"- 信頼度: {impact.confidence:.0%}\n"
            f"\n⚠️ 網羅的な影響調査には追加のコード分析が必要です。"
        )

        return impact

    async def generate_deliverables(
        self,
        diffs: list[DiffResult],
        impact: ImpactAnalysis,
    ) -> dict[str, str]:
        """変更成果物自動生成.

        Args:
            diffs: 差分結果
            impact: 影響分析

        Returns:
            成果物辞書
        """
        deliverables = {}

        # 1. Release Note
        if self._config.generate_release_note:
            deliverables["release_note"] = self._generate_release_note(
                diffs, impact
            )

        # 2. FAQ更新草案
        if self._config.generate_faq_draft:
            deliverables["faq_draft"] = self._generate_faq_draft(diffs)

        # 3. 研修通知草案
        if self._config.generate_training_notice:
            deliverables["training_notice"] = self._generate_training_notice(
                diffs
            )

        # 4. テスト観点リスト
        if self._config.generate_test_checklist:
            deliverables["test_checklist"] = self._generate_test_checklist(
                diffs, impact
            )

        # カバレッジ提示を追加
        if self._config.require_coverage_disclosure:
            for key in deliverables:
                deliverables[key] += f"\n\n---\n{impact.coverage_info}"

        return deliverables

    def _classify_change(self, text: str) -> str:
        """変更をカテゴリ分類."""
        text_lower = text.lower()

        if any(k in text_lower for k in ["api", "interface", "endpoint", "method"]):
            return DiffCategory.INTERFACE
        if any(k in text_lower for k in ["field", "column", "parameter", "attribute"]):
            return DiffCategory.FIELD
        if any(k in text_lower for k in ["rule", "condition", "if", "when", "must"]):
            return DiffCategory.RULE
        if any(k in text_lower for k in ["error", "exception", "fail"]):
            return DiffCategory.EXCEPTION

        return DiffCategory.OTHER

    def _estimate_severity(self, text: str, category: str) -> str:
        """深刻度を推定."""
        if category in [DiffCategory.INTERFACE, DiffCategory.RULE]:
            return DiffSeverity.HIGH
        if category == DiffCategory.FIELD:
            return DiffSeverity.MEDIUM

        return DiffSeverity.LOW

    def _generate_release_note(
        self, diffs: list[DiffResult], impact: ImpactAnalysis
    ) -> str:
        """Release Note 生成."""
        lines = [
            "# Release Note",
            "",
            f"更新日: {datetime.now().strftime('%Y-%m-%d')}",
            "",
            "## 変更概要",
            "",
        ]

        high_severity = [d for d in diffs if d.severity == DiffSeverity.HIGH]
        if high_severity:
            lines.append("### ⚠️ 重要な変更")
            for d in high_severity:
                lines.append(f"- {d.description}")
            lines.append("")

        lines.append("### 変更一覧")
        for d in diffs[:10]:
            lines.append(f"- [{d.category}] {d.description}")

        lines.extend([
            "",
            "## 影響範囲",
            "",
        ])

        if impact.affected_apis:
            lines.append("### API")
            for api in impact.affected_apis:
                lines.append(f"- {api}")

        if impact.affected_modules:
            lines.append("### モジュール")
            for module in impact.affected_modules:
                lines.append(f"- {module}")

        return "\n".join(lines)

    def _generate_faq_draft(self, diffs: list[DiffResult]) -> str:
        """FAQ更新草案生成."""
        lines = [
            "# FAQ更新草案",
            "",
            "以下の変更に基づき、FAQの更新をご検討ください。",
            "",
        ]

        for i, d in enumerate(diffs[:5], 1):
            lines.extend([
                f"## Q{i}: {d.category}に関する変更",
                "",
                "**A**: [回答を記入]",
                "",
                f"参考: {d.description}",
                "",
            ])

        return "\n".join(lines)

    def _generate_training_notice(self, diffs: list[DiffResult]) -> str:
        """研修通知草案生成."""
        return f"""# 研修通知

## 変更内容のお知らせ

今回の更新により、以下の変更がありました。
関係者の皆様は内容をご確認ください。

### 主な変更点

{chr(10).join(f'- {d.description}' for d in diffs[:5])}

### 対象者
- [対象部門を記入]

### 研修日程
- [日程を記入]
"""

    def _generate_test_checklist(
        self, diffs: list[DiffResult], impact: ImpactAnalysis
    ) -> str:
        """テスト観点リスト生成."""
        lines = [
            "# テスト観点リスト",
            "",
            "以下の観点でテストを実施してください。",
            "",
            "## 必須テスト項目",
            "",
        ]

        for i, d in enumerate(diffs, 1):
            status = "[ ]"
            lines.append(f"{i}. {status} {d.category}: {d.description}")

        if impact.affected_tests:
            lines.extend([
                "",
                "## 関連テストケース",
                "",
            ])
            for test in impact.affected_tests:
                lines.append(f"- {test}")

        return "\n".join(lines)

    def _diff_to_dict(self, diff: DiffResult) -> dict[str, Any]:
        """DiffResult を dict に変換."""
        return {
            "category": diff.category,
            "severity": diff.severity,
            "old_value": diff.old_value,
            "new_value": diff.new_value,
            "location": diff.location,
            "description": diff.description,
            "impact_note": diff.impact_note,
        }

    def _impact_to_dict(self, impact: ImpactAnalysis) -> dict[str, Any]:
        """ImpactAnalysis を dict に変換."""
        return {
            "affected_modules": impact.affected_modules,
            "affected_apis": impact.affected_apis,
            "affected_tables": impact.affected_tables,
            "affected_tests": impact.affected_tests,
            "affected_runbooks": impact.affected_runbooks,
            "coverage_info": impact.coverage_info,
            "confidence": impact.confidence,
        }


__all__ = [
    "DiffResult",
    "ImpactAnalysis",
    "MaintenanceAgent",
    "MaintenanceConfig",
    "MaintenanceResponse",
]
