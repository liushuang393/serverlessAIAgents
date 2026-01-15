# -*- coding: utf-8 -*-
"""安全機能ミックスイン - Apps統合用.

Engine/Service/Agentに安全機能を付加するミックスインクラス。
- 入力検査（プロンプト注入、PII検出）
- 出力検査（幻覚検出、機密情報漏洩）
- 推論監視

使用例:
    >>> from agentflow.security.safety_mixin import SafetyMixin
    >>>
    >>> class MyEngine(BaseEngine, SafetyMixin):
    ...     async def _execute(self, inputs):
    ...         # 入力検査
    ...         input_check = await self.check_input_safety(inputs.get("query", ""))
    ...         if not input_check.is_safe:
    ...             return {"error": "入力が安全でありません"}
    ...         
    ...         # 処理実行
    ...         result = await self._process(inputs)
    ...         
    ...         # 出力検査
    ...         output_check = await self.check_output_safety(result.get("response", ""))
    ...         if output_check.needs_review:
    ...             result["_safety_warning"] = output_check.warnings
    ...         
    ...         return result
"""

from __future__ import annotations

import logging
from typing import Any

from agentflow.security.ai_safety_guard import (
    AISafetyGuard,
    GuardConfig,
    InputCheckResult,
    OutputCheckResult,
    SafetyLevel,
)
from agentflow.security.data_sanitizer import DataSanitizer, SanitizerConfig
from agentflow.security.hallucination_detector import HallucinationDetector, DetectionConfig
from agentflow.security.reasoning_monitor import ReasoningMonitor, MonitorConfig

_logger = logging.getLogger(__name__)


class SafetyMixin:
    """安全機能ミックスイン.

    Engine, Service, Agent クラスに安全機能を付加する。
    AISafetyGuard を内部で使用し、統一されたインターフェースを提供。

    Attributes:
        _safety_guard: AISafetyGuard インスタンス
        _safety_enabled: 安全機能の有効/無効フラグ

    使用例:
        >>> class MyAgent(BaseAgent, SafetyMixin):
        ...     def __init__(self):
        ...         super().__init__()
        ...         self.init_safety()
    """

    _safety_guard: AISafetyGuard | None = None
    _safety_enabled: bool = True

    def init_safety(
        self,
        config: GuardConfig | None = None,
        enabled: bool = True,
    ) -> None:
        """安全機能を初期化.

        Args:
            config: AISafetyGuard設定
            enabled: 安全機能の有効化フラグ
        """
        self._safety_enabled = enabled
        if enabled:
            self._safety_guard = AISafetyGuard(config=config)
            _logger.info("安全機能を初期化しました")

    @property
    def safety_enabled(self) -> bool:
        """安全機能が有効かどうか."""
        return self._safety_enabled and self._safety_guard is not None

    async def check_input_safety(
        self,
        text: str,
        context: dict[str, Any] | None = None,
    ) -> InputCheckResult:
        """入力の安全性を検査.

        Args:
            text: 検査対象テキスト
            context: 追加コンテキスト

        Returns:
            入力検査結果
        """
        if not self.safety_enabled:
            return InputCheckResult(is_safe=True, sanitized_input=text)

        try:
            result = await self._safety_guard.check_input(text, context)  # type: ignore
            if not result.is_safe:
                _logger.warning(
                    "入力安全検査: 問題検出 [level=%s, threats=%d]",
                    result.safety_level.value,
                    len(result.threats),
                )
            return result
        except Exception as e:
            _logger.error("入力安全検査エラー: %s", e)
            # エラー時は安全側に倒す
            return InputCheckResult(
                is_safe=False,
                safety_level=SafetyLevel.WARNING,
                sanitized_input=text,
                warnings=[f"安全検査エラー: {e}"],
            )

    async def check_output_safety(
        self,
        text: str,
        context: dict[str, Any] | None = None,
    ) -> OutputCheckResult:
        """出力の安全性を検査.

        Args:
            text: 検査対象テキスト（LLM出力等）
            context: 追加コンテキスト（ソース情報等）

        Returns:
            出力検査結果
        """
        if not self.safety_enabled:
            from agentflow.security.ai_safety_guard import OutputCheckResult
            return OutputCheckResult(
                is_safe=True,
                needs_review=False,
                safety_level=SafetyLevel.SAFE,
            )

        try:
            result = await self._safety_guard.check_output(text, context)  # type: ignore
            if result.needs_review:
                _logger.warning(
                    "出力安全検査: 要確認 [score=%.2f, issues=%d]",
                    result.hallucination_result.confidence_score
                    if result.hallucination_result
                    else 0.0,
                    len(result.issues) if result.issues else 0,
                )
            return result
        except Exception as e:
            _logger.error("出力安全検査エラー: %s", e)
            from agentflow.security.ai_safety_guard import OutputCheckResult
            return OutputCheckResult(
                is_safe=True,
                needs_review=True,
                safety_level=SafetyLevel.WARNING,
                warnings=[f"安全検査エラー: {e}"],
            )

    def sanitize_text(self, text: str) -> str:
        """テキストを脱敏処理.

        PIIや機密情報をマスキングする。

        Args:
            text: 処理対象テキスト

        Returns:
            脱敏後のテキスト
        """
        if not self.safety_enabled:
            return text

        try:
            sanitizer = DataSanitizer()
            result = sanitizer.sanitize(text)
            return result.sanitized_text
        except Exception as e:
            _logger.error("テキスト脱敏エラー: %s", e)
            return text


__all__ = ["SafetyMixin"]

