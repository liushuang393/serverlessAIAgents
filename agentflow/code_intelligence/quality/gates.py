"""QualityGate - 品質ゲート.

コード変換の品質を検証するゲートを定義します。

使用例:
    >>> runner = QualityGateRunner()
    >>> runner.add_gate(SyntaxValidator())
    >>> runner.add_gate(SemanticValidator())
    >>> report = await runner.run_all(source, target, context)
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import UTC, datetime
from enum import Enum
from typing import Any


class QualityLevel(str, Enum):
    """品質レベル."""

    CRITICAL = "critical"
    ERROR = "error"
    WARNING = "warning"
    INFO = "info"


@dataclass
class QualityIssue:
    """品質問題.

    Attributes:
        level: 重大度
        message: メッセージ
        location: 場所（ファイル:行）
        rule: 違反したルール
        suggestion: 修正提案
    """

    level: QualityLevel
    message: str
    location: str = ""
    rule: str = ""
    suggestion: str = ""

    def to_dict(self) -> dict[str, Any]:
        return {
            "level": self.level.value,
            "message": self.message,
            "location": self.location,
            "rule": self.rule,
            "suggestion": self.suggestion,
        }


@dataclass
class GateResult:
    """ゲート結果.

    Attributes:
        gate_name: ゲート名
        passed: 通過したか
        issues: 検出された問題
        duration_ms: 実行時間
    """

    gate_name: str
    passed: bool
    issues: list[QualityIssue] = field(default_factory=list)
    duration_ms: float = 0.0
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        return {
            "gate_name": self.gate_name,
            "passed": self.passed,
            "issues": [i.to_dict() for i in self.issues],
            "duration_ms": self.duration_ms,
            "metadata": self.metadata,
        }


@dataclass
class QualityReport:
    """品質レポート.

    Attributes:
        passed: 全ゲート通過したか
        gate_results: 各ゲートの結果
        total_issues: 総問題数
        critical_count: クリティカル数
        error_count: エラー数
        warning_count: 警告数
        score: 品質スコア (0-100)
    """

    passed: bool
    gate_results: list[GateResult] = field(default_factory=list)
    total_issues: int = 0
    critical_count: int = 0
    error_count: int = 0
    warning_count: int = 0
    score: float = 0.0
    duration_ms: float = 0.0
    generated_at: datetime = field(default_factory=lambda: datetime.now(UTC))

    def to_dict(self) -> dict[str, Any]:
        return {
            "passed": self.passed,
            "gate_results": [g.to_dict() for g in self.gate_results],
            "total_issues": self.total_issues,
            "critical_count": self.critical_count,
            "error_count": self.error_count,
            "warning_count": self.warning_count,
            "score": self.score,
            "duration_ms": self.duration_ms,
            "generated_at": self.generated_at.isoformat(),
        }


class QualityGate(ABC):
    """品質ゲート基底クラス.

    変換結果の品質を検証します。
    """

    @property
    @abstractmethod
    def name(self) -> str:
        """ゲート名."""

    @property
    def description(self) -> str:
        """ゲート説明."""
        return ""

    @property
    def is_blocking(self) -> bool:
        """ブロッキングゲートか（失敗時に全体を失敗にするか）."""
        return True

    @abstractmethod
    async def validate(
        self,
        source_code: str,
        target_code: str,
        context: dict[str, Any],
    ) -> GateResult:
        """検証を実行.

        Args:
            source_code: ソースコード
            target_code: ターゲットコード
            context: コンテキスト

        Returns:
            ゲート結果
        """


class SyntaxValidator(QualityGate):
    """構文検証ゲート."""

    @property
    def name(self) -> str:
        return "syntax_validator"

    @property
    def description(self) -> str:
        return "Validates target code syntax"

    async def validate(
        self,
        source_code: str,
        target_code: str,
        context: dict[str, Any],
    ) -> GateResult:
        import time

        start = time.time()
        issues = []

        # 基本的な構文チェック
        if not target_code.strip():
            issues.append(
                QualityIssue(
                    level=QualityLevel.CRITICAL,
                    message="Target code is empty",
                    rule="non_empty_output",
                )
            )

        # バランスチェック
        brackets = {"(": ")", "{": "}", "[": "]"}
        stack = []
        for i, char in enumerate(target_code):
            if char in brackets:
                stack.append((char, i))
            elif char in brackets.values():
                if not stack:
                    issues.append(
                        QualityIssue(
                            level=QualityLevel.ERROR,
                            message=f"Unmatched closing bracket '{char}'",
                            location=f"char:{i}",
                            rule="balanced_brackets",
                        )
                    )
                else:
                    open_bracket, _ = stack.pop()
                    if brackets[open_bracket] != char:
                        issues.append(
                            QualityIssue(
                                level=QualityLevel.ERROR,
                                message=f"Mismatched brackets: '{open_bracket}' and '{char}'",
                                location=f"char:{i}",
                                rule="balanced_brackets",
                            )
                        )

        if stack:
            for open_bracket, pos in stack:
                issues.append(
                    QualityIssue(
                        level=QualityLevel.ERROR,
                        message=f"Unmatched opening bracket '{open_bracket}'",
                        location=f"char:{pos}",
                        rule="balanced_brackets",
                    )
                )

        duration = (time.time() - start) * 1000
        passed = not any(i.level in [QualityLevel.CRITICAL, QualityLevel.ERROR] for i in issues)

        return GateResult(
            gate_name=self.name,
            passed=passed,
            issues=issues,
            duration_ms=duration,
        )


class SemanticValidator(QualityGate):
    """意味検証ゲート."""

    @property
    def name(self) -> str:
        return "semantic_validator"

    @property
    def description(self) -> str:
        return "Validates semantic equivalence"

    @property
    def is_blocking(self) -> bool:
        return False  # 警告のみ

    async def validate(
        self,
        source_code: str,
        target_code: str,
        context: dict[str, Any],
    ) -> GateResult:
        import time

        start = time.time()
        issues = []

        # 簡易的なチェック（本格実装では AST 比較を行う）
        source_lines = len(source_code.splitlines())
        target_lines = len(target_code.splitlines())

        if target_lines < source_lines * 0.3:
            issues.append(
                QualityIssue(
                    level=QualityLevel.WARNING,
                    message="Target code is significantly shorter than source",
                    rule="code_length_ratio",
                    suggestion="Verify that all functionality is preserved",
                )
            )

        duration = (time.time() - start) * 1000
        return GateResult(
            gate_name=self.name,
            passed=True,  # 警告のみなので常に pass
            issues=issues,
            duration_ms=duration,
        )


class QualityGateRunner:
    """品質ゲートランナー.

    複数のゲートを順に実行し、レポートを生成します。
    """

    def __init__(self) -> None:
        self._gates: list[QualityGate] = []

    def add_gate(self, gate: QualityGate) -> None:
        """ゲートを追加."""
        self._gates.append(gate)

    def remove_gate(self, gate_name: str) -> bool:
        """ゲートを削除."""
        for i, gate in enumerate(self._gates):
            if gate.name == gate_name:
                del self._gates[i]
                return True
        return False

    async def run_all(
        self,
        source_code: str,
        target_code: str,
        context: dict[str, Any] | None = None,
    ) -> QualityReport:
        """全ゲートを実行.

        Args:
            source_code: ソースコード
            target_code: ターゲットコード
            context: コンテキスト

        Returns:
            品質レポート
        """
        import time

        start = time.time()
        context = context or {}

        results: list[GateResult] = []
        all_passed = True

        for gate in self._gates:
            result = await gate.validate(source_code, target_code, context)
            results.append(result)

            if not result.passed and gate.is_blocking:
                all_passed = False

        # 統計を集計
        total_issues = sum(len(r.issues) for r in results)
        critical_count = sum(1 for r in results for i in r.issues if i.level == QualityLevel.CRITICAL)
        error_count = sum(1 for r in results for i in r.issues if i.level == QualityLevel.ERROR)
        warning_count = sum(1 for r in results for i in r.issues if i.level == QualityLevel.WARNING)

        # スコア計算
        score = 100.0
        score -= critical_count * 30
        score -= error_count * 10
        score -= warning_count * 2
        score = max(0.0, score)

        duration = (time.time() - start) * 1000

        return QualityReport(
            passed=all_passed,
            gate_results=results,
            total_issues=total_issues,
            critical_count=critical_count,
            error_count=error_count,
            warning_count=warning_count,
            score=score,
            duration_ms=duration,
        )


__all__ = [
    "GateResult",
    "QualityGate",
    "QualityGateRunner",
    "QualityIssue",
    "QualityLevel",
    "QualityReport",
    "SemanticValidator",
    "SyntaxValidator",
]
