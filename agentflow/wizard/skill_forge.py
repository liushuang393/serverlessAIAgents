# -*- coding: utf-8 -*-
"""SkillForge - 技能锻造.

Skill のテスト合成、依存解析、組み立てを行います。

使用例:
    >>> forge = SkillForge()
    >>> result = await forge.forge_skill(skill_spec)
    >>> if result.valid:
    ...     print(f"Skill forged: {result.skill.name}")
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import Any

from agentflow.providers import get_llm
from agentflow.skills.base import Skill
from agentflow.skills.validator import SkillValidator
from agentflow.wizard.models import (
    TestCase,
    TestResult,
    TestSuiteResult,
    ValidationResult,
    ValidationStatus,
)
from agentflow.wizard.test_synthesizer import TestSynthesizer


@dataclass
class ForgeResult:
    """Forge 結果.

    Attributes:
        success: 成功したか
        skill: 生成された Skill
        tests: 生成されたテスト
        test_results: テスト結果
        validation: 検証結果
        dependencies: 依存 Skill
    """

    success: bool
    skill: Skill | None = None
    tests: list[TestCase] = field(default_factory=list)
    test_results: TestSuiteResult | None = None
    validation: ValidationResult | None = None
    dependencies: list[str] = field(default_factory=list)
    error: str | None = None


@dataclass
class SkillSpec:
    """Skill 仕様.

    Attributes:
        name: Skill 名
        description: 説明
        trigger_patterns: トリガーパターン
        instructions: 実行指示
        examples: 使用例
        dependencies: 依存 Skill
    """

    name: str
    description: str
    trigger_patterns: list[str] = field(default_factory=list)
    instructions: str = ""
    examples: list[str] = field(default_factory=list)
    dependencies: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)


class SkillForge:
    """技能锻造.

    Skill のテスト合成、依存解析、組み立てを行います。

    機能:
    - テスト合成: Skill 仕様からテストを自動生成
    - 依存解析: Skill 間の依存関係を解析
    - 組み立て: 複数の Skill を組み合わせ
    - 検証: 品質チェック
    """

    def __init__(
        self,
        llm_client: Any = None,
        skill_validator: SkillValidator | None = None,
        test_synthesizer: TestSynthesizer | None = None,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLM クライアント
            skill_validator: Skill 検証器
            test_synthesizer: テスト合成器
        """
        self._llm = llm_client or get_llm()
        self._validator = skill_validator or SkillValidator()
        self._test_synthesizer = test_synthesizer or TestSynthesizer(llm_client=self._llm)
        self._logger = logging.getLogger(__name__)

        # 依存グラフ
        self._dependency_graph: dict[str, list[str]] = {}

    async def forge_skill(
        self,
        spec: SkillSpec,
        *,
        auto_test: bool = True,
        resolve_dependencies: bool = True,
    ) -> ForgeResult:
        """Skill を锻造.

        Args:
            spec: Skill 仕様
            auto_test: 自動テストを実行するか
            resolve_dependencies: 依存を解決するか

        Returns:
            Forge 結果
        """
        self._logger.info(f"Forging skill: {spec.name}")

        try:
            # 1. 依存解析
            dependencies = []
            if resolve_dependencies:
                dependencies = await self._resolve_dependencies(spec)

            # 2. Skill を生成
            skill = await self._create_skill(spec)

            # 3. 検証
            val_result = self._validator.validate(skill)
            validation = ValidationResult(
                valid=val_result.valid,
                status=ValidationStatus.PASSED if val_result.valid else ValidationStatus.FAILED,
                errors=val_result.errors,
                warnings=val_result.warnings,
                score=val_result.score if hasattr(val_result, "score") else 0.8,
            )

            # 4. テスト生成と実行
            tests = []
            test_results = None
            if auto_test:
                tests = await self._synthesize_skill_tests(spec)
                test_results = await self._run_skill_tests(skill, tests)

            # 5. 依存グラフを更新
            self._dependency_graph[spec.name] = dependencies

            return ForgeResult(
                success=validation.valid,
                skill=skill,
                tests=tests,
                test_results=test_results,
                validation=validation,
                dependencies=dependencies,
            )

        except Exception as e:
            self._logger.exception(f"Forge failed: {e}")
            return ForgeResult(
                success=False,
                error=str(e),
            )

    async def _resolve_dependencies(self, spec: SkillSpec) -> list[str]:
        """依存を解析.

        Args:
            spec: Skill 仕様

        Returns:
            依存 Skill 名リスト
        """
        dependencies = list(spec.dependencies)

        # instructions から依存を推定
        if spec.instructions:
            prompt = f"""以下の Skill 指示から、必要となる可能性のある他の Skill を特定してください。

Skill 名: {spec.name}
指示: {spec.instructions}

依存する可能性のある Skill 名をカンマ区切りで回答してください。
なければ "none" と回答してください。"""

            response = await self._llm.generate(prompt)
            content = response.content if hasattr(response, "content") else str(response)

            if content.lower().strip() != "none":
                inferred = [s.strip() for s in content.split(",") if s.strip()]
                for dep in inferred:
                    if dep not in dependencies and dep != spec.name:
                        dependencies.append(dep)

        return dependencies

    async def _create_skill(self, spec: SkillSpec) -> Skill:
        """Skill を作成.

        Args:
            spec: Skill 仕様

        Returns:
            作成された Skill
        """
        return Skill(
            name=spec.name,
            trigger_patterns=spec.trigger_patterns,
            instructions=spec.instructions,
            examples=spec.examples,
            metadata={
                "description": spec.description,
                "forged": True,
                **spec.metadata,
            },
        )

    async def _synthesize_skill_tests(self, spec: SkillSpec) -> list[TestCase]:
        """Skill テストを合成.

        Args:
            spec: Skill 仕様

        Returns:
            テストケースリスト
        """
        tests = []

        # トリガーパターンごとのテスト
        for i, pattern in enumerate(spec.trigger_patterns[:3]):
            tests.append(TestCase(
                name=f"test_{spec.name}_trigger_{i+1}",
                description=f"Test trigger pattern: {pattern}",
                input_data={"query": pattern},
                expected_output={"matched": True},
                assertions=[
                    "result is not None",
                    "result.get('matched', False) == True",
                ],
                tags=["trigger", "pattern"],
            ))

        # 例に基づくテスト
        for i, example in enumerate(spec.examples[:2]):
            tests.append(TestCase(
                name=f"test_{spec.name}_example_{i+1}",
                description=f"Test example: {example[:50]}...",
                input_data={"query": example},
                expected_output={},
                assertions=[
                    "result is not None",
                ],
                tags=["example"],
            ))

        # エッジケース
        tests.append(TestCase(
            name=f"test_{spec.name}_no_match",
            description="Test with non-matching input",
            input_data={"query": "completely unrelated random text xyz123"},
            expected_output={"matched": False},
            assertions=[],
            tags=["edge", "no_match"],
        ))

        return tests

    async def _run_skill_tests(
        self,
        skill: Skill,
        tests: list[TestCase],
    ) -> TestSuiteResult:
        """Skill テストを実行.

        Args:
            skill: Skill
            tests: テストケース

        Returns:
            テスト結果
        """
        results = []
        total_duration = 0.0

        for test in tests:
            import time
            start = time.time()

            try:
                # Skill マッチングをシミュレート
                query = test.input_data.get("query", "")
                matched = any(
                    pattern.lower() in query.lower()
                    for pattern in skill.trigger_patterns
                )

                passed = True
                for assertion in test.assertions:
                    try:
                        result = {"matched": matched}
                        # 簡易的なアサーション評価
                        if "matched" in assertion and "True" in assertion:
                            passed = passed and matched
                        elif "matched" in assertion and "False" in assertion:
                            passed = passed and not matched
                    except Exception:
                        passed = False
                        break

                duration = (time.time() - start) * 1000
                total_duration += duration

                results.append(TestResult(
                    test_name=test.name,
                    passed=passed,
                    duration_ms=duration,
                    output={"matched": matched},
                    assertions_passed=len(test.assertions) if passed else 0,
                    assertions_failed=0 if passed else len(test.assertions),
                ))

            except Exception as e:
                results.append(TestResult(
                    test_name=test.name,
                    passed=False,
                    error=str(e),
                ))

        passed_count = sum(1 for r in results if r.passed)

        return TestSuiteResult(
            total_tests=len(results),
            passed_tests=passed_count,
            failed_tests=len(results) - passed_count,
            results=results,
            coverage=0.8,  # シミュレーション
            duration_ms=total_duration,
        )

    async def compose_skills(
        self,
        skills: list[SkillSpec],
        composition_name: str,
    ) -> ForgeResult:
        """複数の Skill を組み合わせ.

        Args:
            skills: Skill 仕様リスト
            composition_name: 組み合わせ名

        Returns:
            Forge 結果
        """
        self._logger.info(f"Composing {len(skills)} skills into '{composition_name}'")

        # 依存関係を解析
        all_dependencies = []
        for spec in skills:
            deps = await self._resolve_dependencies(spec)
            all_dependencies.extend(deps)

        # 組み合わせ Skill を作成
        combined_patterns = []
        combined_instructions = []
        combined_examples = []

        for spec in skills:
            combined_patterns.extend(spec.trigger_patterns)
            combined_instructions.append(f"## {spec.name}\n{spec.instructions}")
            combined_examples.extend(spec.examples)

        combined_spec = SkillSpec(
            name=composition_name,
            description=f"Composed skill from: {', '.join(s.name for s in skills)}",
            trigger_patterns=list(set(combined_patterns)),
            instructions="\n\n".join(combined_instructions),
            examples=combined_examples[:5],
            dependencies=list(set(all_dependencies)),
        )

        return await self.forge_skill(combined_spec)

    def get_dependency_graph(self) -> dict[str, list[str]]:
        """依存グラフを取得.

        Returns:
            依存グラフ（Skill名 → 依存Skill名リスト）
        """
        return dict(self._dependency_graph)

    def find_circular_dependencies(self) -> list[list[str]]:
        """循環依存を検出.

        Returns:
            循環依存のパスリスト
        """
        cycles = []
        visited = set()
        rec_stack = set()

        def dfs(node: str, path: list[str]) -> None:
            visited.add(node)
            rec_stack.add(node)
            path.append(node)

            for neighbor in self._dependency_graph.get(node, []):
                if neighbor not in visited:
                    dfs(neighbor, path.copy())
                elif neighbor in rec_stack:
                    # 循環を検出
                    cycle_start = path.index(neighbor)
                    cycle = path[cycle_start:] + [neighbor]
                    cycles.append(cycle)

            rec_stack.remove(node)

        for node in self._dependency_graph:
            if node not in visited:
                dfs(node, [])

        return cycles


__all__ = ["SkillForge", "SkillSpec", "ForgeResult"]
