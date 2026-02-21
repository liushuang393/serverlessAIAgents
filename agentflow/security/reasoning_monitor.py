"""推理监控器 - 多步推理稳定性保障.

解决LLM在多步推理中的不稳定问题:
- 目标偏离检测
- 推理链完整性检查
- 自动回滚与重规划
- 状态一致性验证

参考文献:
- Q* Framework for LLM Reasoning (2024)
- Chain-of-Thought Prompting (2022)
- ReAct: Synergizing Reasoning and Acting (2023)

使用例:
    >>> from agentflow.security.reasoning_monitor import ReasoningMonitor
    >>>
    >>> monitor = ReasoningMonitor(original_goal="分析销售数据")
    >>>
    >>> # 在每个推理步骤后检查
    >>> for step in reasoning_steps:
    ...     result = monitor.check_step(step)
    ...     if result.needs_correction:
    ...         corrected = await monitor.suggest_correction()
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class DeviationType(str, Enum):
    """偏离类型."""

    GOAL_DRIFT = "goal_drift"  # 目标偏离
    CONTEXT_LOSS = "context_loss"  # 上下文丢失
    CONSTRAINT_VIOLATION = "constraint_violation"  # 约束违反
    LOGICAL_ERROR = "logical_error"  # 逻辑错误
    TOOL_MISUSE = "tool_misuse"  # 工具误用
    INFINITE_LOOP = "infinite_loop"  # 无限循环
    PREMATURE_END = "premature_end"  # 过早结束


class ReasoningState(str, Enum):
    """推理状态."""

    ON_TRACK = "on_track"  # 正常进行
    MINOR_DEVIATION = "minor"  # 轻微偏离
    MAJOR_DEVIATION = "major"  # 严重偏离
    FAILED = "failed"  # 失败
    RECOVERED = "recovered"  # 已恢复


@dataclass
class ReasoningStep:
    """推理步骤.

    Attributes:
        step_id: 步骤ID
        action: 执行的动作
        thought: 推理思考
        result: 执行结果
        tool_calls: 工具调用
    """

    step_id: int
    action: str
    thought: str = ""
    result: Any = None
    tool_calls: list[dict[str, Any]] = field(default_factory=list)
    timestamp: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "step_id": self.step_id,
            "action": self.action,
            "thought": self.thought,
            "result": str(self.result)[:200] if self.result else None,
            "tool_calls": self.tool_calls,
            "timestamp": self.timestamp.isoformat(),
        }


@dataclass
class DeviationReport:
    """偏离报告.

    Attributes:
        deviation_type: 偏离类型
        severity: 严重程度（0.0-1.0）
        description: 描述
        affected_step: 受影响的步骤
        suggestion: 修正建议
    """

    deviation_type: DeviationType
    severity: float
    description: str
    affected_step: int | None = None
    suggestion: str = ""

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "type": self.deviation_type.value,
            "severity": self.severity,
            "description": self.description,
            "affected_step": self.affected_step,
            "suggestion": self.suggestion,
        }


@dataclass
class MonitorResult:
    """监控结果.

    Attributes:
        state: 当前推理状态
        deviations: 检测到的偏离
        needs_correction: 是否需要修正
        recommended_action: 推荐操作
        progress: 进度估计
    """

    state: ReasoningState = ReasoningState.ON_TRACK
    deviations: list[DeviationReport] = field(default_factory=list)
    needs_correction: bool = False
    recommended_action: str = "continue"
    progress: float = 0.0
    checked_at: datetime = field(default_factory=datetime.now)

    @property
    def is_healthy(self) -> bool:
        """是否健康."""
        return self.state in (ReasoningState.ON_TRACK, ReasoningState.RECOVERED)

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "state": self.state.value,
            "is_healthy": self.is_healthy,
            "needs_correction": self.needs_correction,
            "recommended_action": self.recommended_action,
            "progress": self.progress,
            "deviations": [d.to_dict() for d in self.deviations],
        }


class MonitorConfig(BaseModel):
    """监控配置."""

    # 最大步骤数
    max_steps: int = Field(default=50, ge=1)
    # 偏离阈值
    deviation_threshold: float = Field(default=0.3, ge=0.0, le=1.0)
    # 重复检测窗口
    loop_detection_window: int = Field(default=5, ge=2)
    # 相似度阈值（用于循环检测）
    similarity_threshold: float = Field(default=0.8, ge=0.0, le=1.0)
    # 是否启用自动修正
    auto_correction: bool = Field(default=True)
    # 最大修正尝试次数
    max_correction_attempts: int = Field(default=3, ge=1)


class ReasoningMonitor:
    """推理监控器.

    监控多步推理过程，检测偏离并提供修正建议。

    核心功能:
    1. 目标一致性检测：确保每步都朝向原始目标
    2. 上下文完整性：检测上下文丢失
    3. 循环检测：识别无限循环
    4. 约束验证：检查是否违反预设约束

    Example:
        >>> monitor = ReasoningMonitor("生成销售报告")
        >>> monitor.add_constraint("不要访问外部API")
        >>>
        >>> step = ReasoningStep(1, "分析数据", "需要先查看数据结构")
        >>> result = monitor.check_step(step)
    """

    def __init__(
        self,
        original_goal: str,
        config: MonitorConfig | None = None,
        llm_client: Any = None,
    ) -> None:
        """初始化.

        Args:
            original_goal: 原始目标
            config: 监控配置
            llm_client: LLM客户端（用于语义分析）
        """
        self._goal = original_goal
        self._config = config or MonitorConfig()
        self._llm = llm_client

        # 推理历史
        self._steps: list[ReasoningStep] = []
        self._checkpoints: list[int] = []  # 检查点位置

        # 约束
        self._constraints: list[str] = []

        # 状态
        self._current_state = ReasoningState.ON_TRACK
        self._correction_attempts = 0

        self._logger = logging.getLogger(__name__)

    def add_constraint(self, constraint: str) -> None:
        """添加约束.

        Args:
            constraint: 约束描述
        """
        self._constraints.append(constraint)

    def add_checkpoint(self) -> int:
        """添加检查点.

        Returns:
            检查点ID（当前步骤数）
        """
        checkpoint_id = len(self._steps)
        self._checkpoints.append(checkpoint_id)
        self._logger.info(f"添加检查点: {checkpoint_id}")
        return checkpoint_id

    def check_step(self, step: ReasoningStep) -> MonitorResult:
        """检查推理步骤.

        Args:
            step: 推理步骤

        Returns:
            MonitorResult
        """
        self._steps.append(step)

        result = MonitorResult(
            state=self._current_state,
            progress=len(self._steps) / self._config.max_steps,
        )

        deviations: list[DeviationReport] = []

        # 1. 检查步骤数限制
        if len(self._steps) > self._config.max_steps:
            deviations.append(
                DeviationReport(
                    deviation_type=DeviationType.PREMATURE_END,
                    severity=0.8,
                    description="超过最大步骤数限制",
                    affected_step=step.step_id,
                    suggestion="建议终止或重新规划",
                )
            )

        # 2. 检查循环
        loop_deviation = self._detect_loop()
        if loop_deviation:
            deviations.append(loop_deviation)

        # 3. 检查目标偏离
        goal_deviation = self._check_goal_alignment(step)
        if goal_deviation:
            deviations.append(goal_deviation)

        # 4. 检查约束违反
        constraint_deviations = self._check_constraints(step)
        deviations.extend(constraint_deviations)

        # 5. 检查上下文丢失
        context_deviation = self._check_context_loss(step)
        if context_deviation:
            deviations.append(context_deviation)

        # 更新结果
        result.deviations = deviations

        if deviations:
            max_severity = max(d.severity for d in deviations)

            if max_severity >= 0.7:
                result.state = ReasoningState.MAJOR_DEVIATION
                result.needs_correction = True
                result.recommended_action = "rollback_and_replan"
            elif max_severity >= 0.3:
                result.state = ReasoningState.MINOR_DEVIATION
                result.needs_correction = self._config.auto_correction
                result.recommended_action = "adjust"
            else:
                result.state = ReasoningState.ON_TRACK
                result.recommended_action = "continue"

        self._current_state = result.state
        return result

    def _detect_loop(self) -> DeviationReport | None:
        """检测循环."""
        if len(self._steps) < self._config.loop_detection_window:
            return None

        recent = self._steps[-self._config.loop_detection_window :]
        actions = [s.action for s in recent]

        # 检查完全相同的动作
        if len(set(actions)) == 1:
            return DeviationReport(
                deviation_type=DeviationType.INFINITE_LOOP,
                severity=0.9,
                description=f"检测到重复动作: {actions[0]}",
                affected_step=recent[-1].step_id,
                suggestion="建议回滚到上一个检查点或修改策略",
            )

        # 检查动作模式重复（如 A-B-A-B）
        half = len(actions) // 2
        if actions[:half] == actions[half:]:
            return DeviationReport(
                deviation_type=DeviationType.INFINITE_LOOP,
                severity=0.7,
                description="检测到动作模式循环",
                affected_step=recent[-1].step_id,
                suggestion="建议打破循环模式",
            )

        return None

    def _check_goal_alignment(self, step: ReasoningStep) -> DeviationReport | None:
        """检查目标对齐."""
        # 简单的关键词检查
        goal_keywords = set(self._goal.lower().split())
        step_keywords = set(f"{step.action} {step.thought}".lower().split())

        # 计算重叠度
        overlap = len(goal_keywords & step_keywords)
        alignment = overlap / len(goal_keywords) if goal_keywords else 1.0

        if alignment < 0.1:
            return DeviationReport(
                deviation_type=DeviationType.GOAL_DRIFT,
                severity=0.6,
                description="当前步骤可能偏离原始目标",
                affected_step=step.step_id,
                suggestion=f"原始目标: {self._goal}",
            )

        return None

    def _check_constraints(self, step: ReasoningStep) -> list[DeviationReport]:
        """检查约束违反."""
        deviations = []

        for constraint in self._constraints:
            # 简单的关键词匹配
            constraint_lower = constraint.lower()
            step_text = f"{step.action} {step.thought}".lower()

            # 检查禁止词
            if "不要" in constraint or "禁止" in constraint:
                # 提取被禁止的动作
                forbidden_keywords = constraint_lower.replace("不要", "").replace("禁止", "").split()
                for kw in forbidden_keywords:
                    if kw in step_text:
                        deviations.append(
                            DeviationReport(
                                deviation_type=DeviationType.CONSTRAINT_VIOLATION,
                                severity=0.8,
                                description=f"违反约束: {constraint}",
                                affected_step=step.step_id,
                                suggestion="请修改操作以遵守约束",
                            )
                        )

        return deviations

    def _check_context_loss(self, step: ReasoningStep) -> DeviationReport | None:
        """检查上下文丢失."""
        if len(self._steps) < 3:
            return None

        # 检查是否引用了之前的步骤
        recent_actions = [s.action for s in self._steps[-5:-1]]
        current_thought = step.thought.lower()

        # 如果思考中没有引用最近的动作，可能丢失了上下文
        has_reference = any(action.lower()[:10] in current_thought for action in recent_actions)

        if not has_reference and len(self._steps) > 5:
            return DeviationReport(
                deviation_type=DeviationType.CONTEXT_LOSS,
                severity=0.4,
                description="可能丢失了之前步骤的上下文",
                affected_step=step.step_id,
                suggestion="建议在推理中引用之前的步骤结果",
            )

        return None

    def rollback_to_checkpoint(self, checkpoint_id: int | None = None) -> list[ReasoningStep]:
        """回滚到检查点.

        Args:
            checkpoint_id: 检查点ID（None则回滚到最近的检查点）

        Returns:
            被回滚的步骤
        """
        if not self._checkpoints:
            self._logger.warning("没有可用的检查点")
            return []

        target = checkpoint_id if checkpoint_id is not None else self._checkpoints[-1]

        if target >= len(self._steps):
            return []

        rolled_back = self._steps[target:]
        self._steps = self._steps[:target]

        self._logger.info(f"回滚到检查点 {target}，移除 {len(rolled_back)} 个步骤")
        self._current_state = ReasoningState.RECOVERED

        return rolled_back

    async def suggest_correction(
        self,
        deviation: DeviationReport | None = None,
    ) -> str:
        """建议修正方案.

        Args:
            deviation: 需要修正的偏离

        Returns:
            修正建议
        """
        if not deviation:
            # 获取最严重的偏离
            if not self._steps:
                return "无需修正"
            result = self.check_step(self._steps[-1])
            if not result.deviations:
                return "无需修正"
            deviation = max(result.deviations, key=lambda d: d.severity)

        # 使用LLM生成修正建议
        if self._llm:
            prompt = f"""推理任务出现偏离，请提供修正建议:

原始目标: {self._goal}
偏离类型: {deviation.deviation_type.value}
偏离描述: {deviation.description}
当前步骤数: {len(self._steps)}

请提供简洁的修正建议（1-2句话）:"""

            try:
                response = await self._llm.generate(prompt)
                if isinstance(response, dict):
                    return str(response.get("content", str(response)))
                return str(response)
            except Exception as e:
                self._logger.warning(f"LLM建议生成失败: {e}")

        # 默认建议
        suggestions = {
            DeviationType.GOAL_DRIFT: f"请重新聚焦于原始目标: {self._goal}",
            DeviationType.CONTEXT_LOSS: "请回顾之前的步骤结果并在推理中引用",
            DeviationType.INFINITE_LOOP: "建议采用不同的方法或回滚到检查点",
            DeviationType.CONSTRAINT_VIOLATION: "请修改操作以遵守约束条件",
            DeviationType.TOOL_MISUSE: "请检查工具使用是否正确",
            DeviationType.PREMATURE_END: "建议简化任务或分阶段执行",
        }

        return suggestions.get(deviation.deviation_type, deviation.suggestion)

    def get_summary(self) -> dict[str, Any]:
        """获取监控摘要."""
        return {
            "goal": self._goal,
            "total_steps": len(self._steps),
            "checkpoints": len(self._checkpoints),
            "current_state": self._current_state.value,
            "constraints": self._constraints,
            "correction_attempts": self._correction_attempts,
        }

    def get_steps(self) -> list[ReasoningStep]:
        """获取所有步骤."""
        return self._steps.copy()
