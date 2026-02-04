# -*- coding: utf-8 -*-
"""SystemSynthesizer - システム自動合成.

能力缺口リストから完整なシステム（Agents + Flows + Skills + Tests）を生成します。

使用例:
    >>> synthesizer = SystemSynthesizer()
    >>> result = await synthesizer.synthesize(gap_analysis.gaps)
    >>> for agent in result.agents:
    ...     print(f"Generated: {agent.name}")
"""

from __future__ import annotations

import logging
from typing import Any

from agentflow.providers import get_llm
from agentflow.wizard.models import (
    AgentSpec,
    CapabilityGap,
    EngineType,
    GapType,
    SynthesisResult,
    TestCase,
    ValidationResult,
    ValidationStatus,
)
from agentflow.wizard.test_synthesizer import TestSynthesizer


class SystemSynthesizer:
    """システム自動合成.

    能力缺口から完整なシステムを自動生成します。

    生成物:
    - Agents: 能力を実装する Agent
    - Flows: Agent を連携する Flow
    - Skills: 再利用可能な Skill
    - Tests: 生成物の検証テスト
    """

    def __init__(
        self,
        llm_client: Any = None,
        test_synthesizer: TestSynthesizer | None = None,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLM クライアント
            test_synthesizer: テスト合成器
        """
        self._llm = llm_client or get_llm()
        self._test_synthesizer = test_synthesizer or TestSynthesizer(llm_client=self._llm)
        self._logger = logging.getLogger(__name__)

    async def synthesize(
        self,
        gaps: list[CapabilityGap],
        *,
        generate_tests: bool = True,
        validate: bool = True,
    ) -> SynthesisResult:
        """システムを合成.

        Args:
            gaps: 能力缺口リスト
            generate_tests: テストを生成するか
            validate: 検証を行うか

        Returns:
            合成結果
        """
        self._logger.info(f"Synthesizing system from {len(gaps)} gaps")

        try:
            agents: list[AgentSpec] = []
            flows: list[dict[str, Any]] = []
            skills: list[dict[str, Any]] = []
            tests: list[TestCase] = []

            # 缺口タイプごとに処理
            for gap in gaps:
                if gap.gap_type == GapType.SKILL:
                    skill = await self._synthesize_skill(gap)
                    skills.append(skill)

                elif gap.gap_type == GapType.TOOL:
                    # Tool は Skill として実装
                    skill = await self._synthesize_tool_as_skill(gap)
                    skills.append(skill)

                elif gap.gap_type == GapType.AGENT:
                    agent = await self._synthesize_agent(gap)
                    agents.append(agent)

                elif gap.gap_type == GapType.PATTERN:
                    flow = await self._synthesize_flow(gap)
                    flows.append(flow)

                elif gap.gap_type == GapType.KNOWLEDGE:
                    # Knowledge は RAG Agent として実装
                    agent = await self._synthesize_rag_agent(gap)
                    agents.append(agent)

            # テスト生成
            if generate_tests:
                for agent in agents:
                    agent_tests = await self._test_synthesizer.synthesize(agent)
                    tests.extend(agent_tests)

            # 検証
            validation = None
            if validate:
                validation = self._validate_synthesis(agents, flows, skills)

            return SynthesisResult(
                success=True,
                agents=agents,
                flows=flows,
                skills=skills,
                tests=tests,
                validation=validation,
            )

        except Exception as e:
            self._logger.exception(f"Synthesis failed: {e}")
            return SynthesisResult(
                success=False,
                error=str(e),
            )

    async def _synthesize_skill(self, gap: CapabilityGap) -> dict[str, Any]:
        """Skill を合成.

        Args:
            gap: 能力缺口

        Returns:
            Skill 定義
        """
        prompt = f"""以下の能力缺口を解決する Skill を設計してください。

缺口: {gap.description}
提案解決策: {gap.suggested_solution}

以下の JSON 形式で Skill 定義を回答してください:
{{
    "name": "skill_name",
    "description": "Skill の説明",
    "trigger_patterns": ["パターン1", "パターン2"],
    "instructions": "実行指示",
    "examples": ["例1", "例2"]
}}"""

        response = await self._llm.generate(prompt)
        content = response.content if hasattr(response, "content") else str(response)

        # JSON 抽出
        import json
        try:
            start = content.find("{")
            end = content.rfind("}") + 1
            if start >= 0 and end > start:
                skill = json.loads(content[start:end])
                skill["source_gap_id"] = gap.gap_id
                return skill
        except json.JSONDecodeError:
            pass

        # フォールバック
        return {
            "name": f"skill_{gap.gap_id}",
            "description": gap.description,
            "trigger_patterns": [],
            "instructions": gap.suggested_solution,
            "source_gap_id": gap.gap_id,
        }

    async def _synthesize_tool_as_skill(self, gap: CapabilityGap) -> dict[str, Any]:
        """Tool を Skill として合成.

        Args:
            gap: 能力缺口

        Returns:
            Skill 定義（Tool として使用）
        """
        skill = await self._synthesize_skill(gap)
        skill["is_tool"] = True
        return skill

    async def _synthesize_agent(self, gap: CapabilityGap) -> AgentSpec:
        """Agent を合成.

        Args:
            gap: 能力缺口

        Returns:
            Agent 仕様
        """
        prompt = f"""以下の能力缺口を解決する Agent を設計してください。

缺口: {gap.description}
提案解決策: {gap.suggested_solution}

以下の JSON 形式で Agent 仕様を回答してください:
{{
    "name": "AgentName",
    "description": "Agent の説明",
    "capabilities": ["能力1", "能力2"],
    "system_prompt": "システムプロンプト",
    "engine_type": "simple|pipeline|gate|rag"
}}"""

        response = await self._llm.generate(prompt)
        content = response.content if hasattr(response, "content") else str(response)

        # JSON 抽出
        import json
        try:
            start = content.find("{")
            end = content.rfind("}") + 1
            if start >= 0 and end > start:
                data = json.loads(content[start:end])
                return AgentSpec(
                    name=data.get("name", f"SynthesizedAgent_{gap.gap_id}"),
                    description=data.get("description", gap.description),
                    capabilities=data.get("capabilities", []),
                    system_prompt=data.get("system_prompt", ""),
                    engine_type=EngineType(data.get("engine_type", "simple")),
                    confidence=0.7,
                    metadata={"source_gap_id": gap.gap_id},
                )
        except (json.JSONDecodeError, ValueError):
            pass

        # フォールバック
        return AgentSpec(
            name=f"SynthesizedAgent_{gap.gap_id[:8]}",
            description=gap.description,
            capabilities=[gap.suggested_solution],
            system_prompt=f"You are an agent designed to: {gap.description}",
            engine_type=EngineType.SIMPLE,
            confidence=0.5,
            metadata={"source_gap_id": gap.gap_id},
        )

    async def _synthesize_flow(self, gap: CapabilityGap) -> dict[str, Any]:
        """Flow を合成.

        Args:
            gap: 能力缺口

        Returns:
            Flow 定義
        """
        prompt = f"""以下の能力缺口を解決する Flow を設計してください。

缺口: {gap.description}
提案解決策: {gap.suggested_solution}

以下の JSON 形式で Flow 定義を回答してください:
{{
    "name": "flow_name",
    "description": "Flow の説明",
    "nodes": [
        {{"id": "node1", "type": "agent", "name": "AgentName"}}
    ],
    "edges": [
        {{"from": "node1", "to": "node2"}}
    ]
}}"""

        response = await self._llm.generate(prompt)
        content = response.content if hasattr(response, "content") else str(response)

        # JSON 抽出
        import json
        try:
            start = content.find("{")
            end = content.rfind("}") + 1
            if start >= 0 and end > start:
                flow = json.loads(content[start:end])
                flow["source_gap_id"] = gap.gap_id
                return flow
        except json.JSONDecodeError:
            pass

        # フォールバック
        return {
            "name": f"flow_{gap.gap_id}",
            "description": gap.description,
            "nodes": [],
            "edges": [],
            "source_gap_id": gap.gap_id,
        }

    async def _synthesize_rag_agent(self, gap: CapabilityGap) -> AgentSpec:
        """RAG Agent を合成.

        Args:
            gap: 能力缺口

        Returns:
            Agent 仕様
        """
        agent = await self._synthesize_agent(gap)
        agent.engine_type = EngineType.RAG
        agent.metadata["requires_knowledge_base"] = True
        return agent

    def _validate_synthesis(
        self,
        agents: list[AgentSpec],
        flows: list[dict[str, Any]],
        skills: list[dict[str, Any]],
    ) -> ValidationResult:
        """合成結果を検証.

        Args:
            agents: Agent リスト
            flows: Flow リスト
            skills: Skill リスト

        Returns:
            検証結果
        """
        errors = []
        warnings = []
        suggestions = []

        # Agent 検証
        for agent in agents:
            if not agent.name:
                errors.append(f"Agent has no name")
            if not agent.description:
                warnings.append(f"Agent '{agent.name}' has no description")
            if not agent.system_prompt:
                warnings.append(f"Agent '{agent.name}' has no system prompt")

        # Flow 検証
        for flow in flows:
            if not flow.get("name"):
                errors.append("Flow has no name")
            if not flow.get("nodes"):
                warnings.append(f"Flow '{flow.get('name', 'unknown')}' has no nodes")

        # Skill 検証
        for skill in skills:
            if not skill.get("name"):
                errors.append("Skill has no name")
            if not skill.get("instructions"):
                warnings.append(f"Skill '{skill.get('name', 'unknown')}' has no instructions")

        # スコア計算
        total_items = len(agents) + len(flows) + len(skills)
        score = 1.0
        if total_items > 0:
            score -= (len(errors) / total_items) * 0.5
            score -= (len(warnings) / total_items) * 0.2
        score = max(0.0, score)

        valid = len(errors) == 0

        return ValidationResult(
            valid=valid,
            status=ValidationStatus.PASSED if valid else ValidationStatus.FAILED,
            errors=errors,
            warnings=warnings,
            suggestions=suggestions,
            score=score,
            details={
                "agents_count": len(agents),
                "flows_count": len(flows),
                "skills_count": len(skills),
            },
        )


__all__ = ["SystemSynthesizer"]
