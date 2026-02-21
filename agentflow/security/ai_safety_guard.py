"""AI安全防护门面 - 统一的AI弱点防护接口.

整合所有AI安全防护功能:
- 幻觉检测
- 推理监控
- 数据脱敏
- 提示注入防护

提供简单易用的统一接口，适用于各种AI应用场景。

使用例:
    >>> from agentflow.security.ai_safety_guard import AISafetyGuard
    >>>
    >>> guard = AISafetyGuard()
    >>>
    >>> # 检查用户输入
    >>> input_result = await guard.check_input(user_input)
    >>> if not input_result.is_safe:
    ...     print("输入不安全")
    >>>
    >>> # 检查LLM输出
    >>> output_result = await guard.check_output(llm_output, context)
    >>> if output_result.needs_review:
    ...     print("需要人工复核")
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field

from agentflow.security.data_sanitizer import (
    DataSanitizer,
    SanitizerConfig,
)
from agentflow.security.hallucination_detector import (
    DetectionConfig,
    HallucinationDetector,
)
from agentflow.security.reasoning_monitor import (
    MonitorConfig,
    MonitorResult,
    ReasoningMonitor,
    ReasoningStep,
)


class SafetyLevel(str, Enum):
    """安全级别."""

    SAFE = "safe"  # 安全
    WARNING = "warning"  # 警告
    DANGER = "danger"  # 危险
    BLOCKED = "blocked"  # 已阻止


@dataclass
class InputCheckResult:
    """输入检查结果.

    Attributes:
        is_safe: 是否安全
        safety_level: 安全级别
        sanitized_input: 脱敏后的输入
        threats: 检测到的威胁
        warnings: 警告信息
    """

    is_safe: bool = True
    safety_level: SafetyLevel = SafetyLevel.SAFE
    sanitized_input: str = ""
    threats: list[dict[str, Any]] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    checked_at: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "is_safe": self.is_safe,
            "safety_level": self.safety_level.value,
            "sanitized_input": self.sanitized_input,
            "threats_count": len(self.threats),
            "warnings": self.warnings,
            "checked_at": self.checked_at.isoformat(),
        }


@dataclass
class OutputCheckResult:
    """输出检查结果.

    Attributes:
        is_reliable: 是否可靠
        confidence_score: 可信度评分
        needs_review: 是否需要人工复核
        issues: 检测到的问题
        sanitized_output: 脱敏后的输出
    """

    is_reliable: bool = True
    confidence_score: float = 1.0
    needs_review: bool = False
    issues: list[dict[str, Any]] = field(default_factory=list)
    sanitized_output: str = ""
    checked_at: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "is_reliable": self.is_reliable,
            "confidence_score": self.confidence_score,
            "needs_review": self.needs_review,
            "issues_count": len(self.issues),
            "checked_at": self.checked_at.isoformat(),
        }


class GuardConfig(BaseModel):
    """防护配置."""

    # 是否启用各类检查
    enable_hallucination_check: bool = Field(default=True)
    enable_injection_check: bool = Field(default=True)
    enable_pii_sanitization: bool = Field(default=True)

    # 阈值设置
    confidence_threshold: float = Field(default=0.7, ge=0.0, le=1.0)
    human_review_threshold: float = Field(default=0.5, ge=0.0, le=1.0)

    # 行为设置
    block_dangerous_input: bool = Field(default=True)
    sanitize_output: bool = Field(default=True)

    # 严格模式
    strict_mode: bool = Field(default=False)


class AISafetyGuard:
    """AI安全防护门面.

    提供统一的AI安全防护接口，整合幻觉检测、推理监控和数据脱敏。

    核心功能:
    1. 输入检查：检测提示注入、越狱攻击、敏感信息
    2. 输出检查：检测幻觉、事实错误、敏感信息泄露
    3. 推理监控：监控多步推理过程的稳定性
    4. 数据脱敏：自动脱敏敏感信息

    Example:
        >>> guard = AISafetyGuard()
        >>>
        >>> # 检查用户输入
        >>> input_result = await guard.check_input(user_input)
        >>> if not input_result.is_safe:
        ...     return "输入包含安全威胁"
        >>>
        >>> # 使用脱敏后的输入调用LLM
        >>> llm_output = await llm.generate(input_result.sanitized_input)
        >>>
        >>> # 检查LLM输出
        >>> output_result = await guard.check_output(llm_output)
        >>> if output_result.needs_review:
        ...     await notify_human_reviewer(output_result)
    """

    def __init__(
        self,
        config: GuardConfig | None = None,
        llm_client: Any = None,
    ) -> None:
        """初始化.

        Args:
            config: 防护配置
            llm_client: LLM客户端（用于辅助验证）
        """
        self._config = config or GuardConfig()
        self._llm = llm_client

        # 初始化各组件
        self._hallucination_detector = HallucinationDetector(
            config=DetectionConfig(
                confidence_threshold=self._config.confidence_threshold,
                human_review_threshold=self._config.human_review_threshold,
                strict_mode=self._config.strict_mode,
            ),
            llm_client=llm_client,
        )

        self._sanitizer = DataSanitizer(
            config=SanitizerConfig(
                detect_prompt_injection=self._config.enable_injection_check,
                detect_pii=self._config.enable_pii_sanitization,
                block_injection=self._config.block_dangerous_input,
                strict_mode=self._config.strict_mode,
            )
        )

        # 推理监控器（按需创建）
        self._reasoning_monitors: dict[str, ReasoningMonitor] = {}

        self._logger = logging.getLogger(__name__)

    async def check_input(
        self,
        user_input: str,
        context: str | None = None,
    ) -> InputCheckResult:
        """检查用户输入.

        Args:
            user_input: 用户输入
            context: 上下文信息

        Returns:
            InputCheckResult
        """
        result = InputCheckResult(sanitized_input=user_input)

        # 1. 检测提示注入
        if self._config.enable_injection_check:
            injection_threats = self._sanitizer.check_prompt_injection(user_input)
            jailbreak_threats = self._sanitizer.check_jailbreak(user_input)

            all_threats = injection_threats + jailbreak_threats

            if all_threats:
                result.threats = [t.to_dict() for t in all_threats]

                # 根据威胁严重程度设置安全级别
                max_severity = max(t.severity for t in all_threats)

                if max_severity >= 0.8:
                    result.safety_level = SafetyLevel.BLOCKED
                    result.is_safe = False
                    if self._config.block_dangerous_input:
                        result.sanitized_input = "[输入已屏蔽]"
                elif max_severity >= 0.5:
                    result.safety_level = SafetyLevel.DANGER
                    result.is_safe = False
                else:
                    result.safety_level = SafetyLevel.WARNING
                    result.warnings.append("检测到潜在威胁，请谨慎处理")

        # 2. PII脱敏
        if self._config.enable_pii_sanitization and result.is_safe:
            sanitize_result = self._sanitizer.sanitize(user_input)
            result.sanitized_input = sanitize_result.sanitized_text

            if sanitize_result.detections:
                result.warnings.append(f"检测到 {len(sanitize_result.detections)} 处敏感信息，已脱敏")

        return result

    async def check_output(
        self,
        llm_output: str,
        context: str | None = None,
        ground_truth: str | None = None,
    ) -> OutputCheckResult:
        """检查LLM输出.

        Args:
            llm_output: LLM输出
            context: 上下文信息
            ground_truth: 真实信息（如果已知）

        Returns:
            OutputCheckResult
        """
        result = OutputCheckResult(sanitized_output=llm_output)

        # 1. 幻觉检测
        if self._config.enable_hallucination_check:
            detection_result = await self._hallucination_detector.check(
                output=llm_output,
                context=context,
                ground_truth=ground_truth,
            )

            result.confidence_score = detection_result.confidence_score
            result.is_reliable = detection_result.is_reliable
            result.needs_review = detection_result.needs_human_review
            result.issues = [i.to_dict() for i in detection_result.issues]

        # 2. 输出脱敏
        if self._config.sanitize_output:
            sanitize_result = self._sanitizer.sanitize(llm_output)
            result.sanitized_output = sanitize_result.sanitized_text

            # 审计输出
            audit_result = self._sanitizer.audit_output(llm_output, context)
            if not audit_result["is_safe"]:
                result.issues.extend(audit_result.get("threats", []))
                result.needs_review = True

        return result

    def create_reasoning_monitor(
        self,
        session_id: str,
        goal: str,
        constraints: list[str] | None = None,
    ) -> ReasoningMonitor:
        """创建推理监控器.

        Args:
            session_id: 会话ID
            goal: 原始目标
            constraints: 约束条件

        Returns:
            ReasoningMonitor
        """
        monitor = ReasoningMonitor(
            original_goal=goal,
            config=MonitorConfig(
                auto_correction=True,
                max_correction_attempts=3,
            ),
            llm_client=self._llm,
        )

        if constraints:
            for constraint in constraints:
                monitor.add_constraint(constraint)

        self._reasoning_monitors[session_id] = monitor
        return monitor

    def get_reasoning_monitor(self, session_id: str) -> ReasoningMonitor | None:
        """获取推理监控器.

        Args:
            session_id: 会话ID

        Returns:
            ReasoningMonitor or None
        """
        return self._reasoning_monitors.get(session_id)

    async def check_reasoning_step(
        self,
        session_id: str,
        step: ReasoningStep,
    ) -> MonitorResult:
        """检查推理步骤.

        Args:
            session_id: 会话ID
            step: 推理步骤

        Returns:
            MonitorResult
        """
        monitor = self._reasoning_monitors.get(session_id)
        if not monitor:
            msg = f"未找到会话 {session_id} 的推理监控器"
            raise ValueError(msg)

        return monitor.check_step(step)

    async def full_check(
        self,
        user_input: str,
        llm_output: str,
        context: str | None = None,
    ) -> dict[str, Any]:
        """完整检查（输入+输出）.

        Args:
            user_input: 用户输入
            llm_output: LLM输出
            context: 上下文信息

        Returns:
            完整检查结果
        """
        input_result = await self.check_input(user_input, context)
        output_result = await self.check_output(llm_output, context)

        return {
            "input_check": input_result.to_dict(),
            "output_check": output_result.to_dict(),
            "overall_safe": input_result.is_safe and output_result.is_reliable,
            "needs_review": output_result.needs_review,
            "checked_at": datetime.now().isoformat(),
        }

    def get_stats(self) -> dict[str, Any]:
        """获取统计信息."""
        return {
            "active_monitors": len(self._reasoning_monitors),
            "config": {
                "hallucination_check": self._config.enable_hallucination_check,
                "injection_check": self._config.enable_injection_check,
                "pii_sanitization": self._config.enable_pii_sanitization,
                "strict_mode": self._config.strict_mode,
            },
        }
