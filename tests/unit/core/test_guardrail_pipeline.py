"""GuardrailPipeline のテスト."""

from __future__ import annotations

import pytest

from harness.guardrails.pipeline import (
    Guardrail,
    GuardrailPipeline,
    GuardrailResult,
    Severity,
)


# ============================================================
# テスト用 Guardrail
# ============================================================


class PassGuardrail:
    """常に通過する Guardrail."""

    @property
    def name(self) -> str:
        return "PassGuardrail"

    async def check(
        self,
        input_data: dict,
        output_data: dict | None = None,
    ) -> GuardrailResult:
        return GuardrailResult(
            passed=True,
            guardrail_name=self.name,
            reason="",
            severity=Severity.LOW,
        )


class FailGuardrail:
    """常に失敗する Guardrail."""

    def __init__(self, severity: Severity = Severity.HIGH) -> None:
        self._severity = severity

    @property
    def name(self) -> str:
        return "FailGuardrail"

    async def check(
        self,
        input_data: dict,
        output_data: dict | None = None,
    ) -> GuardrailResult:
        return GuardrailResult(
            passed=False,
            guardrail_name=self.name,
            reason="テスト用失敗",
            severity=self._severity,
        )


class CriticalGuardrail:
    """CRITICAL 失敗を返す Guardrail."""

    @property
    def name(self) -> str:
        return "CriticalGuardrail"

    async def check(
        self,
        input_data: dict,
        output_data: dict | None = None,
    ) -> GuardrailResult:
        return GuardrailResult(
            passed=False,
            guardrail_name=self.name,
            reason="危険な入力を検出",
            severity=Severity.CRITICAL,
        )


# ============================================================
# テスト
# ============================================================


class TestGuardrailPipeline:
    """GuardrailPipeline のテスト."""

    @pytest.mark.asyncio
    async def test_all_pass(self) -> None:
        """全 Guardrail が通過する場合."""
        pipeline = GuardrailPipeline([PassGuardrail(), PassGuardrail()])
        result = await pipeline.run_pre_check({"question": "hello"})
        assert result.all_passed is True
        assert len(result.failures) == 0

    @pytest.mark.asyncio
    async def test_one_failure(self) -> None:
        """1つの Guardrail が失敗する場合."""
        pipeline = GuardrailPipeline([PassGuardrail(), FailGuardrail()])
        result = await pipeline.run_pre_check({"question": "bad"})
        assert result.all_passed is False
        assert len(result.failures) == 1

    @pytest.mark.asyncio
    async def test_critical_short_circuit(self) -> None:
        """CRITICAL 失敗で短絡評価されるか."""
        pipeline = GuardrailPipeline(
            [
                CriticalGuardrail(),
                PassGuardrail(),  # CRITICAL 後なので実行されない
            ]
        )
        result = await pipeline.run_pre_check({"question": "danger"})
        assert result.all_passed is False
        # CRITICAL で短絡 → results は 1 つだけ
        assert len(result.results) == 1

    @pytest.mark.asyncio
    async def test_post_check(self) -> None:
        """run_post_check が output_data を渡すか."""
        pipeline = GuardrailPipeline([PassGuardrail()])
        result = await pipeline.run_post_check(
            {"question": "hello"},
            {"answer": "world"},
        )
        assert result.all_passed is True

    @pytest.mark.asyncio
    async def test_protocol_compliance(self) -> None:
        """テスト用 Guardrail が Protocol を満たすか."""
        assert isinstance(PassGuardrail(), Guardrail)
        assert isinstance(FailGuardrail(), Guardrail)
        assert isinstance(CriticalGuardrail(), Guardrail)

    @pytest.mark.asyncio
    async def test_empty_pipeline(self) -> None:
        """空のパイプラインが全通過するか."""
        pipeline = GuardrailPipeline([])
        result = await pipeline.run_pre_check({"question": "hello"})
        assert result.all_passed is True
