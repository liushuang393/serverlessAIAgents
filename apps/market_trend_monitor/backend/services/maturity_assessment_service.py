"""技術成熟度評価サービス.

技術の成熟度をGartner Hype Cycleにマッピングし、
COBOL→Java移行関連技術の採用リスクを評価します。
"""

from __future__ import annotations

import json
import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import TYPE_CHECKING, Any

from agentflow import get_llm


if TYPE_CHECKING:
    from apps.market_trend_monitor.backend.models import Evidence

from agentflow import get_llm


class MaturityPhase(str, Enum):
    """技術成熟度フェーズ (Gartner Hype Cycle)."""

    INNOVATION_TRIGGER = "innovation_trigger"
    PEAK_OF_EXPECTATIONS = "peak_of_expectations"
    TROUGH_OF_DISILLUSIONMENT = "trough_of_disillusionment"
    SLOPE_OF_ENLIGHTENMENT = "slope_of_enlightenment"
    PLATEAU_OF_PRODUCTIVITY = "plateau_of_productivity"


@dataclass
class TechnologyMaturity:
    """技術成熟度データモデル."""

    technology: str
    phase: MaturityPhase
    confidence: float = 0.5
    evidence_count: int = 0
    adoption_risk: float = 0.5
    time_to_plateau: str = "2-5 years"
    assessed_at: datetime = field(default_factory=datetime.now)
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "technology": self.technology,
            "phase": self.phase.value,
            "confidence": self.confidence,
            "evidence_count": self.evidence_count,
            "adoption_risk": self.adoption_risk,
            "time_to_plateau": self.time_to_plateau,
            "assessed_at": self.assessed_at.isoformat(),
            "metadata": self.metadata,
        }


# デフォルトの追跡技術
DEFAULT_TECHNOLOGIES: list[str] = [
    "AI Code Conversion",
    "LLM-assisted Migration",
    "Automated Testing for Migration",
    "Cloud-native Java",
    "Microservices Architecture",
    "DevOps for Legacy",
    "Low-code Migration",
    "Containerization",
]

# フェーズごとの採用リスク目安
PHASE_RISK_MAP: dict[MaturityPhase, float] = {
    MaturityPhase.INNOVATION_TRIGGER: 0.9,
    MaturityPhase.PEAK_OF_EXPECTATIONS: 0.7,
    MaturityPhase.TROUGH_OF_DISILLUSIONMENT: 0.5,
    MaturityPhase.SLOPE_OF_ENLIGHTENMENT: 0.3,
    MaturityPhase.PLATEAU_OF_PRODUCTIVITY: 0.1,
}

# フェーズごとのプラトー到達予測
PHASE_TIME_MAP: dict[MaturityPhase, str] = {
    MaturityPhase.INNOVATION_TRIGGER: "5-10 years",
    MaturityPhase.PEAK_OF_EXPECTATIONS: "2-5 years",
    MaturityPhase.TROUGH_OF_DISILLUSIONMENT: "2-5 years",
    MaturityPhase.SLOPE_OF_ENLIGHTENMENT: "< 2 years",
    MaturityPhase.PLATEAU_OF_PRODUCTIVITY: "Already there",
}


class MaturityAssessmentService:
    """技術成熟度評価サービス.

    - Gartner Hype Cycle マッピング
    - COBOL→Java移行関連技術の成熟度追跡
    - 技術採用リスク評価
    """

    def __init__(
        self,
        *,
        llm: Any | None = None,
        technologies: list[str] | None = None,
    ) -> None:
        """初期化."""
        self._logger = logging.getLogger(self.__class__.__name__)
        self._llm = llm
        self._technologies = technologies or DEFAULT_TECHNOLOGIES
        self._assessments: dict[str, TechnologyMaturity] = {}

    def _get_llm(self) -> Any:
        """LLMインスタンスを取得."""
        if self._llm is None:
            self._llm = get_llm(temperature=0.3)
        return self._llm

    async def assess_technology(
        self,
        technology: str,
        evidences: list[Evidence],
    ) -> TechnologyMaturity:
        """技術の成熟度を評価.

        Args:
            technology: 評価対象技術名
            evidences: 関連証拠リスト

        Returns:
            技術成熟度評価結果
        """
        evidence_texts = [f"{e.title}: {e.extracted_data.get('content', '')[:100]}" for e in evidences[:10]]

        try:
            llm = self._get_llm()
            prompt = (
                f"Assess the maturity of '{technology}' technology "
                "in the COBOL to Java migration market.\n\n"
                "Evidence:\n" + "\n".join(evidence_texts) + "\n\n"
                "Maturity phases:\n"
                "- innovation_trigger: Very new, experimental\n"
                "- peak_of_expectations: High hype, limited real adoption\n"
                "- trough_of_disillusionment: Reality sets in, some failures\n"
                "- slope_of_enlightenment: Best practices emerging\n"
                "- plateau_of_productivity: Mainstream adoption\n\n"
                "Return JSON: {"
                '"phase": "...", "confidence": 0.0-1.0'
                "}\n\nJSON:"
            )
            response = await llm.chat([{"role": "user", "content": prompt}])
            raw = response if isinstance(response, str) else str(response)
            analysis = self._parse_assessment(raw)

            phase_str = analysis.get("phase", "innovation_trigger")
            try:
                phase = MaturityPhase(phase_str)
            except ValueError:
                phase = MaturityPhase.INNOVATION_TRIGGER

            confidence = float(analysis.get("confidence", 0.5))

        except Exception as e:
            self._logger.warning("技術成熟度評価失敗 %s: %s", technology, e)
            # Phase 10: 多因子ヒューリスティックを優先使用
            if evidences:
                phase = self._estimate_phase_from_evidence(evidences)
            else:
                phase = self._estimate_phase_from_evidence_count(0)
            confidence = 0.3

        maturity = TechnologyMaturity(
            technology=technology,
            phase=phase,
            confidence=confidence,
            evidence_count=len(evidences),
            adoption_risk=PHASE_RISK_MAP.get(phase, 0.5),
            time_to_plateau=PHASE_TIME_MAP.get(phase, "2-5 years"),
            metadata={"evidence_titles": [e.title for e in evidences[:5]]},
        )

        self._assessments[technology] = maturity
        return maturity

    async def get_maturity_landscape(self) -> list[TechnologyMaturity]:
        """全技術の成熟度ランドスケープを取得.

        Returns:
            技術成熟度リスト（フェーズ順）
        """
        phase_order = list(MaturityPhase)
        return sorted(
            self._assessments.values(),
            key=lambda m: phase_order.index(m.phase),
        )

    def get_assessment(self, technology: str) -> TechnologyMaturity | None:
        """技術評価結果を取得."""
        return self._assessments.get(technology)

    def list_assessments(self) -> list[TechnologyMaturity]:
        """全評価結果を取得."""
        return list(self._assessments.values())

    @staticmethod
    def _estimate_phase_from_evidence(
        evidences: list[Evidence],
    ) -> MaturityPhase:
        """多因子ヒューリスティックでフェーズを推定（フォールバック用）.

        Phase 10修正: 記事数のみではなく以下の複合指標を使用:
        - 情報源の多様性（何種類のソースがあるか）
        - 証拠の期間分布（最初と最後の証拠間の日数）
        - 信頼度の平均値
        """
        if not evidences:
            return MaturityPhase.INNOVATION_TRIGGER

        # 情報源多様性スコア (0-1)
        source_types = {e.source_type.value for e in evidences}
        source_diversity = min(len(source_types) / 4.0, 1.0)

        # 期間分布スコア (0-1): 長期間にわたる証拠ほど成熟
        dates = sorted(e.collected_at for e in evidences)
        span_days = (dates[-1] - dates[0]).days if len(dates) > 1 else 0
        span_score = min(span_days / 180.0, 1.0)

        # 信頼度平均
        avg_reliability = sum(e.reliability_score for e in evidences) / len(evidences)

        # 総合スコア (0-1)
        maturity_score = source_diversity * 0.3 + span_score * 0.4 + avg_reliability * 0.3

        if maturity_score >= 0.8:
            return MaturityPhase.PLATEAU_OF_PRODUCTIVITY
        if maturity_score >= 0.6:
            return MaturityPhase.SLOPE_OF_ENLIGHTENMENT
        if maturity_score >= 0.4:
            return MaturityPhase.TROUGH_OF_DISILLUSIONMENT
        if maturity_score >= 0.2:
            return MaturityPhase.PEAK_OF_EXPECTATIONS
        return MaturityPhase.INNOVATION_TRIGGER

    @staticmethod
    def _estimate_phase_from_evidence_count(count: int) -> MaturityPhase:
        """証拠数のみからフェーズを推定（最小フォールバック）."""
        if count >= 20:
            return MaturityPhase.SLOPE_OF_ENLIGHTENMENT
        if count >= 10:
            return MaturityPhase.TROUGH_OF_DISILLUSIONMENT
        if count >= 5:
            return MaturityPhase.PEAK_OF_EXPECTATIONS
        if count >= 2:
            return MaturityPhase.PEAK_OF_EXPECTATIONS
        return MaturityPhase.INNOVATION_TRIGGER

    @staticmethod
    def _parse_assessment(raw: str) -> dict[str, Any]:
        """LLMレスポンスをパース."""
        try:
            start = raw.find("{")
            end = raw.rfind("}")
            if start != -1 and end != -1 and end > start:
                return json.loads(raw[start : end + 1])
        except (json.JSONDecodeError, ValueError):
            pass
        return {}
