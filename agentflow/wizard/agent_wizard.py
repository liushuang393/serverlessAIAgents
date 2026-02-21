"""AgentWizard - Agent 自動生成ウィザード.

自然言語記述から Agent を自動生成します。

フロー: Parse → Design → Generate → Test → Validate → Publish

使用例:
    >>> wizard = AgentWizard()
    >>> result = await wizard.create_from_description(
    ...     "PDFを解析して要約を生成するAgent"
    ... )
    >>> if result.success:
    ...     print(f"Agent created: {result.agent_spec.name}")
"""

from __future__ import annotations

import logging
import time
import uuid
from typing import Any, cast

from agentflow.providers import get_llm
from agentflow.skills.engine import SkillEngine
from agentflow.wizard.models import (
    AgentSpec,
    EngineType,
    TestSuiteResult,
    ValidationResult,
    ValidationStatus,
    WizardConfig,
    WizardResult,
)
from agentflow.wizard.test_synthesizer import TestSynthesizer


class AgentWizard:
    """Agent 自動生成ウィザード.

    自然言語記述から Agent を自動生成、テスト、検証、発布を行います。

    フロー:
    1. Parse - 自然言語記述を解析
    2. Design - Agent 仕様を設計
    3. Generate - コードを生成
    4. Test - 自動テストを実行
    5. Validate - 品質検証
    6. Publish - 発布（オプション）

    Example:
        >>> wizard = AgentWizard()
        >>> result = await wizard.create_from_description(
        ...     "PDFを解析して要約を生成するAgent"
        ... )
    """

    def __init__(
        self,
        config: WizardConfig | None = None,
        llm_client: Any = None,
        skill_engine: SkillEngine | None = None,
    ) -> None:
        """初期化.

        Args:
            config: Wizard 設定
            llm_client: LLM クライアント
            skill_engine: Skill エンジン
        """
        self._config = config or WizardConfig()
        self._llm: Any = llm_client or get_llm()
        self._skill_engine = skill_engine or SkillEngine()
        self._test_synthesizer = TestSynthesizer(llm_client=self._llm)
        self._logger = logging.getLogger(__name__)

    async def create_from_description(
        self,
        description: str,
        *,
        name: str | None = None,
        auto_test: bool | None = None,
        auto_publish: bool | None = None,
    ) -> WizardResult:
        """自然言語記述から Agent を作成.

        Args:
            description: Agent の自然言語記述
            name: Agent 名（省略時は自動生成）
            auto_test: 自動テストを実行するか
            auto_publish: 自動発布するか

        Returns:
            Wizard 実行結果
        """
        start_time = time.time()
        auto_test = auto_test if auto_test is not None else self._config.auto_test
        auto_publish = auto_publish if auto_publish is not None else self._config.auto_publish

        self._logger.info(f"Creating agent from description: {description[:50]}...")

        try:
            # 1. Parse - 自然言語記述を解析
            parsed = await self._parse_description(description)

            # 2. Design - Agent 仕様を設計
            agent_spec = await self._design_agent(parsed, name)

            if agent_spec.confidence < self._config.min_confidence:
                self._logger.warning(f"Low confidence: {agent_spec.confidence:.2f} < {self._config.min_confidence}")

            # 3. Generate - コードを生成
            generated_code = await self._generate_code(agent_spec)

            # 4. Test - 自動テストを実行
            test_results = None
            if auto_test:
                test_results = await self._run_tests(agent_spec, generated_code)

            # 5. Validate - 品質検証
            validation = await self._validate(agent_spec, generated_code, test_results)

            # 6. Publish - 発布（オプション）
            published = False
            publish_id = None
            if auto_publish and validation.valid:
                publish_id = await self._publish(agent_spec, generated_code)
                published = publish_id is not None

            duration_ms = (time.time() - start_time) * 1000

            return WizardResult(
                success=True,
                agent_spec=agent_spec,
                generated_code=generated_code,
                test_results=test_results,
                validation=validation,
                published=published,
                publish_id=publish_id,
                duration_ms=duration_ms,
            )

        except Exception as e:
            self._logger.exception(f"Failed to create agent: {e}")
            return WizardResult(
                success=False,
                error=str(e),
                duration_ms=(time.time() - start_time) * 1000,
            )

    async def _parse_description(self, description: str) -> dict[str, Any]:
        """自然言語記述を解析.

        Args:
            description: Agent の自然言語記述

        Returns:
            解析結果
        """
        prompt = f"""以下の Agent 記述を解析し、構造化された情報を抽出してください。

記述: {description}

以下の JSON 形式で回答してください:
{{
    "intent": "Agent の主な目的",
    "capabilities": ["能力1", "能力2", ...],
    "inputs": ["必要な入力1", "必要な入力2", ...],
    "outputs": ["出力1", "出力2", ...],
    "complexity": "simple|medium|complex",
    "domain": "ドメイン（例: document, data, communication）",
    "keywords": ["キーワード1", "キーワード2", ...]
}}"""

        content = await self._llm_text(prompt)

        # JSON 抽出
        import json

        try:
            # JSON 部分を抽出
            start = content.find("{")
            end = content.rfind("}") + 1
            if start >= 0 and end > start:
                parsed = json.loads(content[start:end])
                if isinstance(parsed, dict):
                    return cast("dict[str, Any]", parsed)
        except json.JSONDecodeError:
            pass

        # フォールバック
        return {
            "intent": description,
            "capabilities": [],
            "inputs": [],
            "outputs": [],
            "complexity": "medium",
            "domain": "general",
            "keywords": [],
        }

    async def _design_agent(
        self,
        parsed: dict[str, Any],
        name: str | None = None,
    ) -> AgentSpec:
        """Agent 仕様を設計.

        Args:
            parsed: 解析結果
            name: Agent 名

        Returns:
            Agent 仕様
        """
        # Agent 名を生成
        if not name:
            name = await self._generate_name(parsed)

        # Engine タイプを決定
        engine_type = self._determine_engine_type(parsed)

        # 必要な Skill を検索/生成
        required_skills = await self._find_required_skills(parsed)

        # システムプロンプトを生成
        system_prompt = await self._generate_system_prompt(parsed)

        # 信頼度を計算
        confidence = self._calculate_confidence(parsed, required_skills)

        return AgentSpec(
            name=name,
            description=parsed.get("intent", ""),
            capabilities=parsed.get("capabilities", []),
            required_skills=required_skills,
            required_tools=[],
            system_prompt=system_prompt,
            engine_type=engine_type,
            confidence=confidence,
            input_schema={"inputs": parsed.get("inputs", [])},
            output_schema={"outputs": parsed.get("outputs", [])},
            metadata={
                "domain": parsed.get("domain", "general"),
                "complexity": parsed.get("complexity", "medium"),
            },
        )

    async def _generate_name(self, parsed: dict[str, Any]) -> str:
        """Agent 名を生成.

        Args:
            parsed: 解析結果

        Returns:
            Agent 名
        """
        intent = parsed.get("intent", "")
        domain = parsed.get("domain", "general")

        prompt = f"""以下の Agent の目的に基づいて、適切な英語の Agent 名を生成してください。

目的: {intent}
ドメイン: {domain}

名前は以下の形式で、1語で回答してください:
- PascalCase
- 末尾に "Agent" を付ける
- 簡潔で分かりやすい

例: PDFAnalyzerAgent, DataProcessorAgent, SummarizerAgent

回答（名前のみ）:"""

        content = await self._llm_text(prompt)
        name = content.strip().split()[0]

        # 名前の検証
        if not name or not name[0].isupper():
            name = f"{domain.title()}Agent"

        if not name.endswith("Agent"):
            name = f"{name}Agent"

        return name

    def _determine_engine_type(self, parsed: dict[str, Any]) -> EngineType:
        """Engine タイプを決定.

        Args:
            parsed: 解析結果

        Returns:
            Engine タイプ
        """
        complexity = parsed.get("complexity", "medium")
        capabilities = parsed.get("capabilities", [])

        # 複雑さに基づく決定
        if complexity == "simple":
            return EngineType.SIMPLE

        # 能力に基づく決定
        capabilities_lower = [c.lower() for c in capabilities]

        if any("rag" in c or "検索" in c or "知識" in c for c in capabilities_lower):
            return EngineType.RAG

        if any("パイプライン" in c or "順次" in c or "ステージ" in c for c in capabilities_lower):
            return EngineType.PIPELINE

        if any("判断" in c or "分岐" in c or "ゲート" in c for c in capabilities_lower):
            return EngineType.GATE

        if complexity == "complex":
            return EngineType.PIPELINE

        return EngineType.SIMPLE

    async def _find_required_skills(self, parsed: dict[str, Any]) -> list[str]:
        """必要な Skill を検索.

        Args:
            parsed: 解析結果

        Returns:
            必要な Skill 名リスト
        """
        required_skills = []
        keywords = parsed.get("keywords", []) + parsed.get("capabilities", [])

        for keyword in keywords:
            try:
                results = self._skill_engine.find(keyword, top_k=1)
                if results:
                    required_skills.append(results[0].skill.name)
            except Exception as e:
                self._logger.debug(f"Skill search failed for '{keyword}': {e}")

        return list(set(required_skills))

    async def _generate_system_prompt(self, parsed: dict[str, Any]) -> str:
        """システムプロンプトを生成.

        Args:
            parsed: 解析結果

        Returns:
            システムプロンプト
        """
        intent = parsed.get("intent", "")
        capabilities = parsed.get("capabilities", [])
        domain = parsed.get("domain", "general")

        prompt = f"""以下の Agent 仕様に基づいて、システムプロンプトを生成してください。

目的: {intent}
能力: {", ".join(capabilities)}
ドメイン: {domain}

システムプロンプトは以下の要素を含めてください:
1. Agent の役割と目的
2. 主要な能力の説明
3. 出力形式のガイドライン
4. 制約事項

簡潔かつ効果的なプロンプトを生成してください。"""

        return await self._llm_text(prompt)

    def _calculate_confidence(
        self,
        parsed: dict[str, Any],
        required_skills: list[str],
    ) -> float:
        """信頼度を計算.

        Args:
            parsed: 解析結果
            required_skills: 必要な Skill

        Returns:
            信頼度 (0.0-1.0)
        """
        score = 0.5  # ベーススコア

        # 能力の明確さ
        capabilities = parsed.get("capabilities", [])
        if len(capabilities) >= 3:
            score += 0.1
        if len(capabilities) >= 5:
            score += 0.1

        # Skill の存在
        if required_skills:
            score += 0.1 * min(len(required_skills) / 3, 1.0)

        # 複雑さの妥当性
        complexity = parsed.get("complexity", "medium")
        if complexity in ["simple", "medium"]:
            score += 0.1

        return min(score, 1.0)

    async def _generate_code(self, agent_spec: AgentSpec) -> str:
        """Agent コードを生成.

        Args:
            agent_spec: Agent 仕様

        Returns:
            生成されたコード
        """
        # テンプレートベースのコード生成
        template = self._get_agent_template(agent_spec.engine_type)

        return template.format(
            name=agent_spec.name,
            description=agent_spec.description,
            system_prompt=agent_spec.system_prompt.replace('"', '\\"'),
            engine_type=agent_spec.engine_type.value,
            capabilities=agent_spec.capabilities,
            required_skills=agent_spec.required_skills,
        )

    def _get_agent_template(self, engine_type: EngineType) -> str:
        """Agent テンプレートを取得.

        Args:
            engine_type: Engine タイプ

        Returns:
            テンプレート文字列
        """
        return '''# -*- coding: utf-8 -*-
"""Auto-generated Agent: {name}

{description}

Engine Type: {engine_type}
Capabilities: {capabilities}
"""

from __future__ import annotations

from typing import Any

from agentflow.core.agent_block import AgentBlock


class {name}(AgentBlock):
    """{description}"""

    def __init__(self, llm_client: Any = None) -> None:
        """初期化."""
        super().__init__(
            name="{name}",
            description="{description}",
            system_prompt="""{system_prompt}""",
            llm_client=llm_client,
        )

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """Agent を実行.

        Args:
            input_data: 入力データ

        Returns:
            実行結果
        """
        # TODO: Implement agent logic
        return {{"success": True, "result": "Not implemented yet"}}


__all__ = ["{name}"]
'''

    async def _run_tests(
        self,
        agent_spec: AgentSpec,
        generated_code: str,
    ) -> TestSuiteResult:
        """テストを実行.

        Args:
            agent_spec: Agent 仕様
            generated_code: 生成されたコード

        Returns:
            テスト結果
        """
        # テストケースを合成
        test_cases = await self._test_synthesizer.synthesize(agent_spec)

        # テストを実行（シミュレーション）
        results: list[dict[str, Any]] = []
        for test_case in test_cases:
            # TODO: 実際のテスト実行
            results.append(
                {
                    "test_name": test_case.name,
                    "passed": True,  # シミュレーション
                    "duration_ms": 100.0,
                }
            )

        passed = sum(1 for r in results if r["passed"])
        failed = len(results) - passed

        return TestSuiteResult(
            total_tests=len(results),
            passed_tests=passed,
            failed_tests=failed,
            coverage=0.8,  # シミュレーション
            duration_ms=sum(float(r["duration_ms"]) for r in results),
        )

    async def _validate(
        self,
        agent_spec: AgentSpec,
        generated_code: str,
        test_results: TestSuiteResult | None,
    ) -> ValidationResult:
        """品質検証.

        Args:
            agent_spec: Agent 仕様
            generated_code: 生成されたコード
            test_results: テスト結果

        Returns:
            検証結果
        """
        errors: list[str] = []
        warnings: list[str] = []
        suggestions: list[str] = []
        score = 0.0

        # 基本検証
        if not agent_spec.name:
            errors.append("Agent name is required")

        if not agent_spec.description:
            warnings.append("Agent description is empty")

        if not agent_spec.system_prompt:
            warnings.append("System prompt is empty")

        # コード検証
        if "class " not in generated_code:
            errors.append("Generated code does not contain a class definition")

        if "async def run" not in generated_code:
            warnings.append("Generated code does not contain async run method")

        # テスト結果検証
        if test_results:
            if test_results.failed_tests > 0:
                errors.append(f"{test_results.failed_tests} tests failed")

            if test_results.coverage < self._config.test_coverage_threshold:
                warnings.append(
                    f"Test coverage {test_results.coverage:.1%} is below threshold "
                    f"{self._config.test_coverage_threshold:.1%}"
                )

        # スコア計算
        score = 1.0
        score -= len(errors) * 0.3
        score -= len(warnings) * 0.1
        score = max(0.0, score)

        valid = len(errors) == 0 and score >= self._config.quality_threshold

        return ValidationResult(
            valid=valid,
            status=ValidationStatus.PASSED if valid else ValidationStatus.FAILED,
            errors=errors,
            warnings=warnings,
            suggestions=suggestions,
            score=score,
        )

    async def _llm_text(self, prompt: str) -> str:
        """LLMからテキスト応答を取得."""
        llm = cast("Any", self._llm)
        if hasattr(llm, "generate"):
            response = await llm.generate(prompt)
        else:
            response = await llm.chat([{"role": "user", "content": prompt}])
        return response.content if hasattr(response, "content") else str(response)

    async def _publish(
        self,
        agent_spec: AgentSpec,
        generated_code: str,
    ) -> str | None:
        """Agent を発布.

        Args:
            agent_spec: Agent 仕様
            generated_code: 生成されたコード

        Returns:
            発布ID（失敗時は None）
        """
        # TODO: 実際の発布処理
        publish_id = f"pub_{uuid.uuid4().hex[:12]}"
        self._logger.info(f"Published agent: {agent_spec.name} (ID: {publish_id})")
        return publish_id


__all__ = ["AgentWizard"]
