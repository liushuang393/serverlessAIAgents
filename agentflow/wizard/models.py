# -*- coding: utf-8 -*-
"""Wizard データモデル.

AgentWizard システムで使用するデータ構造を定義します。
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime, UTC
from enum import Enum
from typing import Any


class GapType(str, Enum):
    """能力缺口タイプ."""

    SKILL = "skill"  # 技能缺口
    TOOL = "tool"  # 工具缺口
    AGENT = "agent"  # Agent 缺口
    PATTERN = "pattern"  # パターン缺口
    KNOWLEDGE = "knowledge"  # 知识缺口


class EngineType(str, Enum):
    """Engine タイプ."""

    SIMPLE = "simple"  # SimpleEngine
    PIPELINE = "pipeline"  # PipelineEngine
    GATE = "gate"  # GateEngine
    RAG = "rag"  # RAGEngine


class ValidationStatus(str, Enum):
    """検証ステータス."""

    PENDING = "pending"
    PASSED = "passed"
    FAILED = "failed"
    WARNING = "warning"


@dataclass
class WizardConfig:
    """Wizard 設定.

    Attributes:
        auto_test: 自動テストを有効にするか
        auto_publish: 自動発布を有効にするか
        min_confidence: 最小信頼度
        max_retries: 最大リトライ回数
        enable_learning: 学習を有効にするか
    """

    auto_test: bool = True
    auto_publish: bool = False
    min_confidence: float = 0.7
    max_retries: int = 3
    enable_learning: bool = True
    test_coverage_threshold: float = 0.8
    quality_threshold: float = 0.7


@dataclass
class AgentSpec:
    """Agent 仕様.

    自然言語記述から生成された Agent の仕様。

    Attributes:
        name: Agent 名
        description: 説明
        capabilities: 能力リスト
        required_skills: 必要な Skill
        required_tools: 必要な Tool
        system_prompt: システムプロンプト
        engine_type: Engine タイプ
        confidence: 生成信頼度
    """

    name: str
    description: str
    capabilities: list[str] = field(default_factory=list)
    required_skills: list[str] = field(default_factory=list)
    required_tools: list[str] = field(default_factory=list)
    system_prompt: str = ""
    engine_type: EngineType = EngineType.SIMPLE
    confidence: float = 0.0
    input_schema: dict[str, Any] = field(default_factory=dict)
    output_schema: dict[str, Any] = field(default_factory=dict)
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "name": self.name,
            "description": self.description,
            "capabilities": self.capabilities,
            "required_skills": self.required_skills,
            "required_tools": self.required_tools,
            "system_prompt": self.system_prompt,
            "engine_type": self.engine_type.value,
            "confidence": self.confidence,
            "input_schema": self.input_schema,
            "output_schema": self.output_schema,
            "metadata": self.metadata,
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "AgentSpec":
        """辞書から作成."""
        return cls(
            name=data["name"],
            description=data.get("description", ""),
            capabilities=data.get("capabilities", []),
            required_skills=data.get("required_skills", []),
            required_tools=data.get("required_tools", []),
            system_prompt=data.get("system_prompt", ""),
            engine_type=EngineType(data.get("engine_type", "simple")),
            confidence=data.get("confidence", 0.0),
            input_schema=data.get("input_schema", {}),
            output_schema=data.get("output_schema", {}),
            metadata=data.get("metadata", {}),
        )


@dataclass
class CapabilityGap:
    """能力缺口.

    システムに不足している能力を表します。

    Attributes:
        gap_id: 缺口ID
        gap_type: 缺口タイプ
        description: 説明
        frequency: 発生頻度
        severity: 重大度 (0.0-1.0)
        suggested_solution: 提案解決策
        auto_fixable: 自動修正可能か
        source: 検出ソース
        detected_at: 検出日時
    """

    gap_id: str
    gap_type: GapType
    description: str
    frequency: int = 1
    severity: float = 0.5
    suggested_solution: str = ""
    auto_fixable: bool = False
    source: str = ""
    detected_at: datetime = field(default_factory=lambda: datetime.now(UTC))
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "gap_id": self.gap_id,
            "gap_type": self.gap_type.value,
            "description": self.description,
            "frequency": self.frequency,
            "severity": self.severity,
            "suggested_solution": self.suggested_solution,
            "auto_fixable": self.auto_fixable,
            "source": self.source,
            "detected_at": self.detected_at.isoformat(),
            "metadata": self.metadata,
        }


@dataclass
class GapAnalysis:
    """缺口分析結果.

    Attributes:
        gaps: 検出された缺口リスト
        total_count: 総缺口数
        auto_fixable_count: 自動修正可能な缺口数
        priority_gaps: 優先度の高い缺口
        recommendations: 推奨アクション
    """

    gaps: list[CapabilityGap] = field(default_factory=list)
    total_count: int = 0
    auto_fixable_count: int = 0
    priority_gaps: list[CapabilityGap] = field(default_factory=list)
    recommendations: list[str] = field(default_factory=list)
    analyzed_at: datetime = field(default_factory=lambda: datetime.now(UTC))

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "gaps": [g.to_dict() for g in self.gaps],
            "total_count": self.total_count,
            "auto_fixable_count": self.auto_fixable_count,
            "priority_gaps": [g.to_dict() for g in self.priority_gaps],
            "recommendations": self.recommendations,
            "analyzed_at": self.analyzed_at.isoformat(),
        }


@dataclass
class TestCase:
    """テストケース.

    Attributes:
        name: テスト名
        description: 説明
        input_data: 入力データ
        expected_output: 期待出力
        assertions: アサーション
        tags: タグ
    """

    name: str
    description: str = ""
    input_data: dict[str, Any] = field(default_factory=dict)
    expected_output: dict[str, Any] = field(default_factory=dict)
    assertions: list[str] = field(default_factory=list)
    tags: list[str] = field(default_factory=list)
    timeout_seconds: int = 30


@dataclass
class TestResult:
    """テスト結果.

    Attributes:
        test_name: テスト名
        passed: 合格したか
        duration_ms: 実行時間（ミリ秒）
        error: エラーメッセージ
        output: 実際の出力
        assertions_passed: 通過したアサーション数
        assertions_failed: 失敗したアサーション数
    """

    test_name: str
    passed: bool
    duration_ms: float = 0.0
    error: str | None = None
    output: dict[str, Any] = field(default_factory=dict)
    assertions_passed: int = 0
    assertions_failed: int = 0
    details: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "test_name": self.test_name,
            "passed": self.passed,
            "duration_ms": self.duration_ms,
            "error": self.error,
            "output": self.output,
            "assertions_passed": self.assertions_passed,
            "assertions_failed": self.assertions_failed,
            "details": self.details,
        }


@dataclass
class TestSuiteResult:
    """テストスイート結果.

    Attributes:
        total_tests: 総テスト数
        passed_tests: 合格テスト数
        failed_tests: 失敗テスト数
        results: 個別テスト結果
        coverage: カバレッジ率
        duration_ms: 総実行時間
    """

    total_tests: int = 0
    passed_tests: int = 0
    failed_tests: int = 0
    results: list[TestResult] = field(default_factory=list)
    coverage: float = 0.0
    duration_ms: float = 0.0

    @property
    def success_rate(self) -> float:
        """成功率を計算."""
        if self.total_tests == 0:
            return 0.0
        return self.passed_tests / self.total_tests

    @property
    def all_passed(self) -> bool:
        """全テスト合格かどうか."""
        return self.failed_tests == 0 and self.total_tests > 0

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "total_tests": self.total_tests,
            "passed_tests": self.passed_tests,
            "failed_tests": self.failed_tests,
            "results": [r.to_dict() for r in self.results],
            "coverage": self.coverage,
            "duration_ms": self.duration_ms,
            "success_rate": self.success_rate,
            "all_passed": self.all_passed,
        }


@dataclass
class ValidationResult:
    """検証結果.

    Attributes:
        valid: 検証合格か
        status: ステータス
        errors: エラーリスト
        warnings: 警告リスト
        suggestions: 改善提案
    """

    valid: bool
    status: ValidationStatus = ValidationStatus.PENDING
    errors: list[str] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    suggestions: list[str] = field(default_factory=list)
    score: float = 0.0
    details: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "valid": self.valid,
            "status": self.status.value,
            "errors": self.errors,
            "warnings": self.warnings,
            "suggestions": self.suggestions,
            "score": self.score,
            "details": self.details,
        }


@dataclass
class SynthesisResult:
    """合成結果.

    SystemSynthesizer が生成したシステム全体の結果。

    Attributes:
        success: 成功したか
        agents: 生成された Agent 仕様
        flows: 生成された Flow 定義
        skills: 生成された Skill
        tests: 生成されたテスト
        validation: 検証結果
    """

    success: bool
    agents: list[AgentSpec] = field(default_factory=list)
    flows: list[dict[str, Any]] = field(default_factory=list)
    skills: list[dict[str, Any]] = field(default_factory=list)
    tests: list[TestCase] = field(default_factory=list)
    validation: ValidationResult | None = None
    error: str | None = None
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "success": self.success,
            "agents": [a.to_dict() for a in self.agents],
            "flows": self.flows,
            "skills": self.skills,
            "tests": [
                {
                    "name": t.name,
                    "description": t.description,
                    "input_data": t.input_data,
                    "expected_output": t.expected_output,
                }
                for t in self.tests
            ],
            "validation": self.validation.to_dict() if self.validation else None,
            "error": self.error,
            "metadata": self.metadata,
        }


@dataclass
class WizardResult:
    """Wizard 実行結果.

    AgentWizard.create_from_description() の結果。

    Attributes:
        success: 成功したか
        agent_spec: 生成された Agent 仕様
        generated_code: 生成されたコード
        tests_passed: テスト合格したか
        published: 発布されたか
        publish_id: 発布ID
    """

    success: bool
    agent_spec: AgentSpec | None = None
    generated_code: str | None = None
    test_results: TestSuiteResult | None = None
    validation: ValidationResult | None = None
    published: bool = False
    publish_id: str | None = None
    error: str | None = None
    duration_ms: float = 0.0
    metadata: dict[str, Any] = field(default_factory=dict)

    @property
    def tests_passed(self) -> bool:
        """テストが全て合格したか."""
        return self.test_results is not None and self.test_results.all_passed

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "success": self.success,
            "agent_spec": self.agent_spec.to_dict() if self.agent_spec else None,
            "generated_code": self.generated_code,
            "test_results": self.test_results.to_dict() if self.test_results else None,
            "validation": self.validation.to_dict() if self.validation else None,
            "tests_passed": self.tests_passed,
            "published": self.published,
            "publish_id": self.publish_id,
            "error": self.error,
            "duration_ms": self.duration_ms,
            "metadata": self.metadata,
        }


__all__ = [
    "GapType",
    "EngineType",
    "ValidationStatus",
    "WizardConfig",
    "AgentSpec",
    "CapabilityGap",
    "GapAnalysis",
    "TestCase",
    "TestResult",
    "TestSuiteResult",
    "ValidationResult",
    "SynthesisResult",
    "WizardResult",
]
