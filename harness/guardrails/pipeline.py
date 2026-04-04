"""ガードレール合成パイプライン - 複数ガードレールの直列実行.

Guardrail プロトコルで統一し、pre_check / post_check を提供する。
CRITICAL 重大度の不合格で短絡終了（ショートサーキット）する。
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from enum import StrEnum
from typing import Any, Protocol, runtime_checkable


_logger = logging.getLogger(__name__)


class Severity(StrEnum):
    """ガードレール検査の重大度."""

    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"


@dataclass(frozen=True)
class GuardrailResult:
    """個別ガードレールの検査結果."""

    passed: bool
    guardrail_name: str = ""
    reason: str = ""
    severity: Severity = Severity.LOW


@dataclass
class PipelineResult:
    """パイプライン全体の集約結果."""

    results: list[GuardrailResult] = field(default_factory=list)
    short_circuited: bool = False

    @property
    def all_passed(self) -> bool:
        """全ガードレールが合格したか."""
        return all(r.passed for r in self.results)

    @property
    def failures(self) -> list[GuardrailResult]:
        """不合格の結果のみ返す."""
        return [r for r in self.results if not r.passed]

    def summary(self) -> dict[str, Any]:
        """集約サマリーを dict で返す."""
        return {
            "total": len(self.results),
            "passed": sum(1 for r in self.results if r.passed),
            "failed": len(self.failures),
            "short_circuited": self.short_circuited,
            "failures": [
                {
                    "guardrail": r.guardrail_name,
                    "reason": r.reason,
                    "severity": r.severity.value,
                }
                for r in self.failures
            ],
        }


@runtime_checkable
class Guardrail(Protocol):
    """ガードレール統一プロトコル."""

    @property
    def name(self) -> str:
        """ガードレール名."""
        ...

    async def check(
        self,
        input_data: dict[str, Any],
        output_data: dict[str, Any] | None = None,
    ) -> GuardrailResult:
        """検査を実行する."""
        ...


class GuardrailPipeline:
    """ガードレール合成パイプライン.

    複数のガードレールを順番に実行し、集約結果を返す。
    CRITICAL 重大度の不合格が発生した場合は即座に短絡終了する。
    """

    def __init__(self, guardrails: list[Guardrail]) -> None:
        self._guardrails = list(guardrails)

    async def run_pre_check(self, input_data: dict[str, Any]) -> PipelineResult:
        """入力に対して全ガードレールを実行する."""
        return await self._run(input_data, output_data=None)

    async def run_post_check(
        self,
        input_data: dict[str, Any],
        output_data: dict[str, Any],
    ) -> PipelineResult:
        """入力と出力の両方に対して全ガードレールを実行する."""
        return await self._run(input_data, output_data=output_data)

    async def _run(
        self,
        input_data: dict[str, Any],
        output_data: dict[str, Any] | None,
    ) -> PipelineResult:
        """ガードレールを順次実行する（CRITICAL 不合格で短絡終了）."""
        pipeline_result = PipelineResult()

        for guardrail in self._guardrails:
            try:
                result = await guardrail.check(input_data, output_data)
            except Exception:
                _logger.exception("ガードレール '%s' の実行中にエラーが発生", guardrail.name)
                # エラー時は安全側に倒す（不合格扱い）
                result = GuardrailResult(
                    passed=False,
                    guardrail_name=guardrail.name,
                    reason=f"ガードレール '{guardrail.name}' の実行中に例外が発生",
                    severity=Severity.HIGH,
                )

            pipeline_result.results.append(result)

            # CRITICAL かつ不合格の場合は短絡終了
            if not result.passed and result.severity == Severity.CRITICAL:
                pipeline_result.short_circuited = True
                _logger.warning(
                    "CRITICAL 違反により短絡終了: guardrail=%s, reason=%s",
                    result.guardrail_name,
                    result.reason,
                )
                break

        return pipeline_result
