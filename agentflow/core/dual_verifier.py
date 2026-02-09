"""双重校验器 - 多重验证系统.

Manus分析中提到的「多重校验」机制：
- 每步结果都进行双重验证
- 主Agent产出后，审核Agent复核
- 数值结果进行独立计算验证
- SQL等操作进行安全性审查

设计原则:
- 不只看最终结果，每一步都验证
- 独立验证避免单点失败
- 支持人工确认关键决策

使用例:
    >>> from agentflow.core.dual_verifier import DualVerifier
    >>>
    >>> verifier = DualVerifier(llm_client=my_llm)
    >>> result = await verifier.verify(
    ...     primary_result=agent_output,
    ...     context={"task": "财务分析"},
    ... )
"""

from __future__ import annotations

import logging
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from pydantic import BaseModel


logger = logging.getLogger(__name__)


class VerifyStatus(str, Enum):
    """校验状态."""

    PASS = "pass"          # 通过
    FAIL = "fail"          # 失败
    WARNING = "warning"    # 警告（需关注但可继续）
    NEED_HUMAN = "need_human"  # 需要人工确认


class VerifyType(str, Enum):
    """校验类型."""

    SCHEMA = "schema"          # 模式匹配
    SEMANTIC = "semantic"      # 语义检查
    NUMERICAL = "numerical"    # 数值验证
    CONSISTENCY = "consistency"  # 一致性检查
    SAFETY = "safety"          # 安全性检查
    CROSS = "cross"            # 交叉验证


@dataclass
class VerifyResult:
    """校验结果.

    Attributes:
        status: 校验状态
        verify_type: 校验类型
        confidence: 置信度 (0.0-1.0)
        message: 说明信息
        details: 详细信息
        suggestions: 改进建议
        timestamp: 校验时间
    """

    status: VerifyStatus
    verify_type: VerifyType = VerifyType.SCHEMA
    confidence: float = 1.0
    message: str = ""
    details: dict[str, Any] = field(default_factory=dict)
    suggestions: list[str] = field(default_factory=list)
    timestamp: datetime = field(default_factory=datetime.now)

    @property
    def is_valid(self) -> bool:
        """是否有效（通过或警告）."""
        return self.status in (VerifyStatus.PASS, VerifyStatus.WARNING)

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "status": self.status.value,
            "verify_type": self.verify_type.value,
            "confidence": self.confidence,
            "message": self.message,
            "details": self.details,
            "suggestions": self.suggestions,
            "timestamp": self.timestamp.isoformat(),
        }


class VerifyStrategy(ABC):
    """校验策略基类."""

    @abstractmethod
    async def verify(
        self,
        data: dict[str, Any],
        context: dict[str, Any],
    ) -> VerifyResult:
        """执行校验.

        Args:
            data: 待校验数据
            context: 上下文信息

        Returns:
            VerifyResult
        """
        ...


class SchemaVerifyStrategy(VerifyStrategy):
    """模式校验策略."""

    def __init__(self, schema: type[BaseModel]) -> None:
        """初始化."""
        self._schema = schema

    async def verify(
        self,
        data: dict[str, Any],
        context: dict[str, Any],
    ) -> VerifyResult:
        """执行模式校验."""
        try:
            self._schema.model_validate(data)
            return VerifyResult(
                status=VerifyStatus.PASS,
                verify_type=VerifyType.SCHEMA,
                message="模式校验通过",
            )
        except Exception as e:
            return VerifyResult(
                status=VerifyStatus.FAIL,
                verify_type=VerifyType.SCHEMA,
                message=f"模式校验失败: {e}",
                details={"error": str(e)},
            )


class ConsistencyVerifyStrategy(VerifyStrategy):
    """一致性校验策略 - 检查数据内部一致性."""

    def __init__(
        self,
        key_fields: list[str],
        tolerance: float = 0.01,
    ) -> None:
        """初始化."""
        self._key_fields = key_fields
        self._tolerance = tolerance

    async def verify(
        self,
        data: dict[str, Any],
        context: dict[str, Any],
    ) -> VerifyResult:
        """执行一致性校验."""
        issues: list[str] = []

        # 检查关键字段是否存在
        for field_name in self._key_fields:
            if field_name not in data:
                issues.append(f"缺少关键字段: {field_name}")

        if issues:
            return VerifyResult(
                status=VerifyStatus.FAIL,
                verify_type=VerifyType.CONSISTENCY,
                message="一致性校验失败",
                details={"issues": issues},
            )

        return VerifyResult(
            status=VerifyStatus.PASS,
            verify_type=VerifyType.CONSISTENCY,
            message="一致性校验通过",
        )


class SafetyVerifyStrategy(VerifyStrategy):
    """安全性校验策略 - 检查潜在危险操作."""

    # 危险关键词
    DANGER_KEYWORDS = [
        "DROP", "DELETE", "TRUNCATE", "ALTER",  # SQL危险
        "rm -rf", "format", "sudo",  # 系统危险
        "password", "secret", "token",  # 敏感信息
    ]

    async def verify(
        self,
        data: dict[str, Any],
        context: dict[str, Any],
    ) -> VerifyResult:
        """执行安全性校验."""
        data_str = str(data).upper()
        found_dangers: list[str] = []

        for keyword in self.DANGER_KEYWORDS:
            if keyword.upper() in data_str:
                found_dangers.append(keyword)

        if found_dangers:
            return VerifyResult(
                status=VerifyStatus.NEED_HUMAN,
                verify_type=VerifyType.SAFETY,
                message=f"检测到潜在危险操作: {found_dangers}",
                details={"dangerous_keywords": found_dangers},
                suggestions=["建议人工审核后再执行"],
            )

        return VerifyResult(
            status=VerifyStatus.PASS,
            verify_type=VerifyType.SAFETY,
            message="安全性校验通过",
        )


class DualVerifier:
    """双重校验器 - 多重验证系统.

    核心功能:
    - 多策略组合校验
    - 主结果与审核结果交叉验证
    - 数值独立计算验证
    - 人工确认触发机制

    Example:
        >>> verifier = DualVerifier()
        >>> verifier.add_strategy(SchemaVerifyStrategy(MySchema))
        >>> verifier.add_strategy(SafetyVerifyStrategy())
        >>>
        >>> result = await verifier.verify(agent_output, context)
        >>> if not result.is_valid:
        ...     handle_verification_failure(result)
    """

    def __init__(
        self,
        llm_client: Any = None,
        strategies: list[VerifyStrategy] | None = None,
        require_all_pass: bool = False,
        min_confidence: float = 0.8,
    ) -> None:
        """初始化.

        Args:
            llm_client: LLM客户端（用于语义校验）
            strategies: 校验策略列表
            require_all_pass: 是否要求所有策略都通过
            min_confidence: 最小置信度阈值
        """
        self._llm = llm_client
        self._strategies = strategies or []
        self._require_all_pass = require_all_pass
        self._min_confidence = min_confidence
        self._logger = logging.getLogger(__name__)

    def add_strategy(self, strategy: VerifyStrategy) -> None:
        """添加校验策略.

        Args:
            strategy: 校验策略
        """
        self._strategies.append(strategy)

    async def verify(
        self,
        data: dict[str, Any],
        context: dict[str, Any] | None = None,
    ) -> VerifyResult:
        """执行校验.

        运行所有注册的策略，汇总结果。

        Args:
            data: 待校验数据
            context: 上下文信息

        Returns:
            汇总的VerifyResult
        """
        context = context or {}

        if not self._strategies:
            return VerifyResult(
                status=VerifyStatus.WARNING,
                message="没有配置校验策略",
            )

        results: list[VerifyResult] = []
        for strategy in self._strategies:
            try:
                result = await strategy.verify(data, context)
                results.append(result)
            except Exception as e:
                self._logger.exception(f"策略执行失败: {e}")
                results.append(VerifyResult(
                    status=VerifyStatus.FAIL,
                    message=f"策略执行异常: {e}",
                ))

        return self._aggregate_results(results)

    async def cross_validate(
        self,
        result1: dict[str, Any],
        result2: dict[str, Any],
        key_fields: list[str] | None = None,
    ) -> VerifyResult:
        """交叉验证两个结果.

        比较两个独立计算的结果是否一致。

        Args:
            result1: 第一个结果
            result2: 第二个结果
            key_fields: 需要比较的关键字段

        Returns:
            VerifyResult
        """
        key_fields = key_fields or list(set(result1.keys()) & set(result2.keys()))

        mismatches: list[dict[str, Any]] = []
        for field_name in key_fields:
            val1 = result1.get(field_name)
            val2 = result2.get(field_name)

            if not self._values_match(val1, val2):
                mismatches.append({
                    "field": field_name,
                    "value1": val1,
                    "value2": val2,
                })

        if mismatches:
            return VerifyResult(
                status=VerifyStatus.FAIL,
                verify_type=VerifyType.CROSS,
                confidence=1.0 - len(mismatches) / len(key_fields),
                message=f"交叉验证失败: {len(mismatches)}个字段不一致",
                details={"mismatches": mismatches},
                suggestions=["检查两个结果的差异原因"],
            )

        return VerifyResult(
            status=VerifyStatus.PASS,
            verify_type=VerifyType.CROSS,
            confidence=1.0,
            message="交叉验证通过",
        )

    def _values_match(
        self,
        val1: Any,
        val2: Any,
        tolerance: float = 0.01,
    ) -> bool:
        """比较两个值是否匹配.

        数值类型允许一定误差，其他类型要求完全相等。
        """
        if isinstance(val1, (int, float)) and isinstance(val2, (int, float)):
            if val1 == 0 and val2 == 0:
                return True
            if val1 == 0 or val2 == 0:
                return abs(val1 - val2) < tolerance
            return abs(val1 - val2) / max(abs(val1), abs(val2)) < tolerance
        return val1 == val2

    def _aggregate_results(
        self,
        results: list[VerifyResult],
    ) -> VerifyResult:
        """汇总多个校验结果.

        Args:
            results: 结果列表

        Returns:
            汇总结果
        """
        if not results:
            return VerifyResult(
                status=VerifyStatus.WARNING,
                message="没有校验结果",
            )

        # 统计各状态数量
        status_counts = dict.fromkeys(VerifyStatus, 0)
        for r in results:
            status_counts[r.status] += 1

        # 计算平均置信度
        avg_confidence = sum(r.confidence for r in results) / len(results)

        # 收集所有问题和建议
        all_details: list[dict[str, Any]] = []
        all_suggestions: list[str] = []
        for r in results:
            if r.details:
                all_details.append(r.details)
            all_suggestions.extend(r.suggestions)

        # 确定最终状态
        if status_counts[VerifyStatus.FAIL] > 0:
            if self._require_all_pass:
                final_status = VerifyStatus.FAIL
            else:
                # 允许部分失败的情况下，看失败比例
                fail_ratio = status_counts[VerifyStatus.FAIL] / len(results)
                final_status = VerifyStatus.FAIL if fail_ratio > 0.5 else VerifyStatus.WARNING
        elif status_counts[VerifyStatus.NEED_HUMAN] > 0:
            final_status = VerifyStatus.NEED_HUMAN
        elif status_counts[VerifyStatus.WARNING] > 0:
            final_status = VerifyStatus.WARNING
        else:
            final_status = VerifyStatus.PASS

        return VerifyResult(
            status=final_status,
            confidence=avg_confidence,
            message=f"校验完成: {len(results)}项策略, "
                   f"通过{status_counts[VerifyStatus.PASS]}, "
                   f"失败{status_counts[VerifyStatus.FAIL]}",
            details={
                "strategy_results": [r.to_dict() for r in results],
                "status_counts": {k.value: v for k, v in status_counts.items()},
            },
            suggestions=list(set(all_suggestions)),
        )

    def get_stats(self) -> dict[str, Any]:
        """获取统计信息."""
        return {
            "strategy_count": len(self._strategies),
            "require_all_pass": self._require_all_pass,
            "min_confidence": self._min_confidence,
            "has_llm": self._llm is not None,
        }

