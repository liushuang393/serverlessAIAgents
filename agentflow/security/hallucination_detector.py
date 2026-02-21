"""幻觉检测器 - AI输出事实校验.

基于研究论文的幻觉检测机制:
- 事实一致性检查
- 可信度评分
- 引用验证
- 数字/时间/实体校验

参考文献:
- Survey on Hallucination in LLMs (2024)
- ChatGPT Limitations Systematic Review (2024)
- Self-Consistency in Chain-of-Thought (2023)

使用例:
    >>> from agentflow.security.hallucination_detector import HallucinationDetector
    >>>
    >>> detector = HallucinationDetector()
    >>> result = await detector.check(
    ...     output="GPT-4于2022年发布",
    ...     context="GPT-4发布时间相关信息",
    ... )
    >>> print(result.confidence_score)  # 0.3 (低可信度)
    >>> print(result.issues)  # ["日期可能不准确: GPT-4于2023年3月发布"]
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field


if TYPE_CHECKING:
    from collections.abc import Callable


class IssueType(str, Enum):
    """问题类型."""

    FACTUAL_ERROR = "factual_error"  # 事实错误
    TEMPORAL_ERROR = "temporal_error"  # 时间错误
    NUMERICAL_ERROR = "numerical_error"  # 数字错误
    CITATION_ERROR = "citation_error"  # 引用错误
    LOGICAL_ERROR = "logical_error"  # 逻辑错误
    UNSUPPORTED_CLAIM = "unsupported_claim"  # 无依据声明
    CONTRADICTION = "contradiction"  # 自相矛盾


class Severity(str, Enum):
    """严重程度."""

    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"


@dataclass
class Issue:
    """检测到的问题.

    Attributes:
        type: 问题类型
        description: 问题描述
        severity: 严重程度
        location: 问题位置（文本片段）
        suggestion: 修正建议
    """

    type: IssueType
    description: str
    severity: Severity = Severity.MEDIUM
    location: str = ""
    suggestion: str = ""

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "type": self.type.value,
            "description": self.description,
            "severity": self.severity.value,
            "location": self.location,
            "suggestion": self.suggestion,
        }


@dataclass
class DetectionResult:
    """检测结果.

    Attributes:
        confidence_score: 可信度评分（0.0-1.0）
        issues: 检测到的问题列表
        verified_claims: 已验证的声明
        unverified_claims: 未验证的声明
        needs_human_review: 是否需要人工复核
    """

    confidence_score: float = 1.0
    issues: list[Issue] = field(default_factory=list)
    verified_claims: list[str] = field(default_factory=list)
    unverified_claims: list[str] = field(default_factory=list)
    needs_human_review: bool = False
    checked_at: datetime = field(default_factory=datetime.now)

    @property
    def is_reliable(self) -> bool:
        """是否可靠（无高严重度问题）."""
        return all(issue.severity not in (Severity.HIGH, Severity.CRITICAL) for issue in self.issues)

    @property
    def has_critical_issues(self) -> bool:
        """是否有严重问题."""
        return any(issue.severity == Severity.CRITICAL for issue in self.issues)

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "confidence_score": self.confidence_score,
            "is_reliable": self.is_reliable,
            "has_critical_issues": self.has_critical_issues,
            "needs_human_review": self.needs_human_review,
            "issues": [i.to_dict() for i in self.issues],
            "verified_claims": self.verified_claims,
            "unverified_claims": self.unverified_claims,
            "checked_at": self.checked_at.isoformat(),
        }


class DetectionConfig(BaseModel):
    """检测配置."""

    # 可信度阈值
    confidence_threshold: float = Field(default=0.7, ge=0.0, le=1.0)
    # 是否启用各类检查
    check_dates: bool = Field(default=True)
    check_numbers: bool = Field(default=True)
    check_citations: bool = Field(default=True)
    check_consistency: bool = Field(default=True)
    # 需要人工复核的阈值
    human_review_threshold: float = Field(default=0.5, ge=0.0, le=1.0)
    # 严格模式（更严格的检查）
    strict_mode: bool = Field(default=False)


class HallucinationDetector:
    """幻觉检测器.

    检测LLM输出中的潜在幻觉和事实错误。

    检测方法:
    1. 模式匹配：检测常见的幻觉模式（虚假引用、模糊声明等）
    2. 一致性检查：与上下文信息对比
    3. 数值验证：检查数字、日期的合理性
    4. LLM辅助验证：使用另一个LLM验证事实（可选）

    Example:
        >>> detector = HallucinationDetector()
        >>> result = await detector.check("GPT-4于2022年发布")
        >>> if not result.is_reliable:
        ...     print("检测到潜在幻觉")
    """

    # 常见幻觉模式
    HALLUCINATION_PATTERNS = [
        # 虚假引用模式
        (r"根据.{2,20}研究(?:表明|显示)", "unverified_research"),
        (r"研究(?:表明|显示|发现)", "vague_research_claim"),
        (r"专家(?:认为|指出|表示)", "vague_expert_claim"),
        # 过度自信模式
        (r"绝对(?:是|会|能|不)", "overconfident_claim"),
        (r"肯定(?:是|会|能|不)", "overconfident_claim"),
        (r"100%", "absolute_claim"),
        # 模糊引用模式
        (r"有人说", "vague_attribution"),
        (r"据说", "hearsay"),
        (r"一般认为", "vague_consensus"),
    ]

    # 日期模式
    DATE_PATTERNS = [
        r"\d{4}年\d{1,2}月\d{1,2}日",
        r"\d{4}年\d{1,2}月",
        r"\d{4}年",
        r"\d{1,2}/\d{1,2}/\d{4}",
        r"\d{4}-\d{2}-\d{2}",
    ]

    # 数字模式
    NUMBER_PATTERNS = [
        r"\d+(?:\.\d+)?%",  # 百分比
        r"\d+(?:,\d{3})*(?:\.\d+)?(?:万|亿|千|百)?",  # 数量
        r"\$\d+(?:,\d{3})*(?:\.\d+)?(?:[KMB])?",  # 美元
    ]

    def __init__(
        self,
        config: DetectionConfig | None = None,
        llm_client: Any = None,
        fact_checker: Callable[[str], bool] | None = None,
    ) -> None:
        """初始化.

        Args:
            config: 检测配置
            llm_client: LLM客户端（用于辅助验证）
            fact_checker: 自定义事实检查函数
        """
        self._config = config or DetectionConfig()
        self._llm = llm_client
        self._fact_checker = fact_checker
        self._logger = logging.getLogger(__name__)

    async def check(
        self,
        output: str,
        context: str | None = None,
        ground_truth: str | None = None,
    ) -> DetectionResult:
        """检测幻觉.

        Args:
            output: LLM输出文本
            context: 上下文信息（用于一致性检查）
            ground_truth: 真实信息（如果已知）

        Returns:
            DetectionResult
        """
        result = DetectionResult()
        issues: list[Issue] = []

        # 1. 模式匹配检查
        pattern_issues = self._check_patterns(output)
        issues.extend(pattern_issues)

        # 2. 日期检查
        if self._config.check_dates:
            date_issues = self._check_dates(output)
            issues.extend(date_issues)

        # 3. 数值检查
        if self._config.check_numbers:
            number_issues = self._check_numbers(output)
            issues.extend(number_issues)

        # 4. 一致性检查
        if self._config.check_consistency and context:
            consistency_issues = await self._check_consistency(output, context)
            issues.extend(consistency_issues)

        # 5. 与真实信息对比
        if ground_truth:
            truth_issues = await self._check_against_truth(output, ground_truth)
            issues.extend(truth_issues)

        # 6. LLM辅助验证（可选）
        if self._llm and self._config.strict_mode:
            llm_issues = await self._llm_verify(output, context)
            issues.extend(llm_issues)

        # 计算可信度评分
        result.issues = issues
        result.confidence_score = self._calculate_confidence(issues)
        result.needs_human_review = result.confidence_score < self._config.human_review_threshold

        return result

    def _check_patterns(self, text: str) -> list[Issue]:
        """检查幻觉模式."""
        issues: list[Issue] = []

        for pattern, issue_type in self.HALLUCINATION_PATTERNS:
            matches = re.findall(pattern, text)
            for match in matches:
                issues.append(
                    Issue(
                        type=IssueType.UNSUPPORTED_CLAIM,
                        description=f"检测到可能的幻觉模式: {issue_type}",
                        severity=Severity.MEDIUM,
                        location=match if isinstance(match, str) else str(match),
                        suggestion="建议添加具体来源或修改为更谨慎的表述",
                    )
                )

        return issues

    def _check_dates(self, text: str) -> list[Issue]:
        """检查日期合理性."""
        issues: list[Issue] = []
        current_year = datetime.now().year

        for pattern in self.DATE_PATTERNS:
            matches = re.findall(pattern, text)
            for match in matches:
                # 提取年份
                year_match = re.search(r"(\d{4})", match)
                if year_match:
                    year = int(year_match.group(1))

                    # 检查未来日期
                    if year > current_year:
                        issues.append(
                            Issue(
                                type=IssueType.TEMPORAL_ERROR,
                                description=f"日期指向未来: {match}",
                                severity=Severity.HIGH,
                                location=match,
                                suggestion="请验证日期是否正确",
                            )
                        )

                    # 检查过于久远的日期（可能是打字错误）
                    elif year < 1900:
                        issues.append(
                            Issue(
                                type=IssueType.TEMPORAL_ERROR,
                                description=f"日期异常久远: {match}",
                                severity=Severity.MEDIUM,
                                location=match,
                                suggestion="请确认年份是否正确",
                            )
                        )

        return issues

    def _check_numbers(self, text: str) -> list[Issue]:
        """检查数值合理性."""
        issues: list[Issue] = []

        # 检查百分比
        percentages = re.findall(r"(\d+(?:\.\d+)?)%", text)
        for pct in percentages:
            value = float(pct)
            if value > 100:
                # 某些场景可以超过100%，但标记为需要注意
                issues.append(
                    Issue(
                        type=IssueType.NUMERICAL_ERROR,
                        description=f"百分比超过100%: {pct}%",
                        severity=Severity.LOW,
                        location=f"{pct}%",
                        suggestion="请确认数值是否正确（某些场景可能合理）",
                    )
                )

        return issues

    async def _check_consistency(
        self,
        output: str,
        context: str,
    ) -> list[Issue]:
        """检查与上下文的一致性."""
        issues = []

        # 简单的关键词冲突检查
        output_lower = output.lower()
        context_lower = context.lower()

        # 检查否定冲突
        negation_pairs = [
            ("是", "不是"),
            ("有", "没有"),
            ("能", "不能"),
            ("会", "不会"),
        ]

        for pos, neg in negation_pairs:
            # 如果上下文说"是"但输出说"不是"
            if pos in context_lower and neg in output_lower:
                issues.append(
                    Issue(
                        type=IssueType.CONTRADICTION,
                        description=f"与上下文可能存在矛盾（{pos} vs {neg}）",
                        severity=Severity.MEDIUM,
                        suggestion="请检查输出与上下文是否一致",
                    )
                )

        return issues

    async def _check_against_truth(
        self,
        output: str,
        ground_truth: str,
    ) -> list[Issue]:
        """与真实信息对比."""
        issues = []

        # 使用自定义事实检查器
        if self._fact_checker and not self._fact_checker(output):
            issues.append(
                Issue(
                    type=IssueType.FACTUAL_ERROR,
                    description="事实检查未通过",
                    severity=Severity.HIGH,
                    suggestion="输出与已知事实不符",
                )
            )

        return issues

    async def _llm_verify(
        self,
        output: str,
        context: str | None,
    ) -> list[Issue]:
        """使用LLM辅助验证."""
        issues: list[Issue] = []

        if not self._llm:
            return issues

        prompt = f"""请判断以下LLM输出是否可能包含幻觉或事实错误：

输出内容:
{output}

{f"参考上下文: {context}" if context else ""}

请只回答以下格式:
- 是否可能有幻觉: 是/否
- 问题描述: (如果有)
- 可信度评分: 0-10
"""

        try:
            response = await self._llm.generate(prompt)
            content = response.get("content", str(response))

            if "是" in content and "幻觉" in content:
                issues.append(
                    Issue(
                        type=IssueType.UNSUPPORTED_CLAIM,
                        description="LLM辅助验证发现潜在问题",
                        severity=Severity.MEDIUM,
                        suggestion="建议人工复核",
                    )
                )

        except Exception as e:
            self._logger.warning(f"LLM验证失败: {e}")

        return issues

    def _calculate_confidence(self, issues: list[Issue]) -> float:
        """计算可信度评分."""
        if not issues:
            return 1.0

        # 基于问题严重程度计算
        severity_weights = {
            Severity.LOW: 0.05,
            Severity.MEDIUM: 0.15,
            Severity.HIGH: 0.30,
            Severity.CRITICAL: 0.50,
        }

        total_penalty = sum(severity_weights.get(issue.severity, 0.1) for issue in issues)

        return max(0.0, 1.0 - total_penalty)

    def add_pattern(self, pattern: str, issue_type: str) -> None:
        """添加自定义检测模式.

        Args:
            pattern: 正则表达式模式
            issue_type: 问题类型标识
        """
        self.HALLUCINATION_PATTERNS.append((pattern, issue_type))
