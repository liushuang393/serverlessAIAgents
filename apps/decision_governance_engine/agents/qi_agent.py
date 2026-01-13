# -*- coding: utf-8 -*-
"""QiAgent - 技術実装Agent（器）.

実行計画を技術的な実装方針に変換する。
LLM必須。LLMなしの場合はエラーを返す。
"""

import logging
from typing import Any

from agentflow import ResilientAgent
from apps.decision_governance_engine.schemas.agent_schemas import (
    DomainSpecificTechnology,
    GeographicConsideration,
    Implementation,
    QiInput,
    QiOutput,
    RegulatoryConsideration,
    ShuOutput,
)


class QiAgent(ResilientAgent[QiInput, QiOutput]):
    """技術実装Agent.

    職責:
    - 技術実装方針の策定
    - ドメイン固有技術の明示
    - 規制対応事項の明示
    - 地理的考慮事項の明示
    """

    name = "QiAgent"
    temperature = 0.6

    def __init__(self, llm_client: Any = None) -> None:
        """初期化."""
        super().__init__(llm_client)
        self._logger = logging.getLogger(self.name)

    SYSTEM_PROMPT = """あなたはQiAgent（器）です。
実行計画を具体的な技術実装方針に変換します。

【出力形式】JSON形式で出力:
{
    "implementations": [{"component": "名", "technology": "技術", "estimated_effort": "工数", "risks": ["リスク"]}],
    "tool_recommendations": ["ツール"],
    "integration_points": ["統合ポイント"],
    "technical_debt_warnings": ["警告"],
    "domain_technologies": [{"technology_name": "名", "category": "分類", "why_required": "理由", "alternatives": ["代替"]}],
    "regulatory_considerations": [{"region": "地域", "regulation": "規制", "requirement": "要件", "implementation_impact": "影響"}],
    "geographic_considerations": [{"region": "地域", "latency_requirement": "遅延要件", "infrastructure_need": "インフラ要件"}]
}"""

    def _parse_input(self, input_data: dict[str, Any]) -> QiInput:
        """入力をパース."""
        if "shu_result" in input_data and isinstance(input_data["shu_result"], dict):
            input_data["shu_result"] = ShuOutput(**input_data["shu_result"])
        return QiInput(**input_data)

    async def process(self, input_data: QiInput) -> QiOutput:
        """技術実装方針を策定（LLM必須）."""
        if not self._llm:
            raise RuntimeError("QiAgent requires LLM client")

        shu_result = input_data.shu_result
        tech_constraints = input_data.tech_constraints

        phases_info = "\n".join(
            f"Phase {p.phase_number}: {p.name} ({p.duration}) - {', '.join(p.actions)}"
            for p in shu_result.phases
        )

        user_prompt = f"""【実行フェーズ】
{phases_info}

【技術制約】{', '.join(tech_constraints) if tech_constraints else "特になし"}

上記を技術実装方針に変換してください。JSON形式で出力。"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{user_prompt}")

        # デバッグログ
        self._logger.debug(f"QiAgent LLM response length: {len(response) if response else 0}")
        if not response or not response.strip():
            self._logger.warning("QiAgent: LLM returned empty response")
            # 空レスポンスの場合はデフォルト出力を返す
            return self._create_default_output(shu_result)

        from agentflow.utils import extract_json
        data = extract_json(response)
        if data is None:
            self._logger.warning(f"QiAgent: Failed to extract JSON. Response preview: {response[:500]}")
            # JSONパース失敗時もデフォルト出力を返す
            return self._create_default_output(shu_result)

        return QiOutput(
            implementations=[Implementation(**i) for i in data.get("implementations", [])],
            tool_recommendations=data.get("tool_recommendations", []),
            integration_points=data.get("integration_points", []),
            technical_debt_warnings=data.get("technical_debt_warnings", []),
            domain_technologies=[
                DomainSpecificTechnology(**dt) for dt in data.get("domain_technologies", [])
            ],
            regulatory_considerations=[
                RegulatoryConsideration(**rc) for rc in data.get("regulatory_considerations", [])
            ],
            geographic_considerations=[
                GeographicConsideration(**gc) for gc in data.get("geographic_considerations", [])
            ],
        )

    def _create_default_output(self, shu_result: ShuOutput) -> QiOutput:
        """デフォルト出力を生成（LLMレスポンス失敗時のフォールバック）.

        Args:
            shu_result: ShuAgent出力

        Returns:
            基本的なQiOutput
        """
        # フェーズから実装要素を推定
        implementations = []
        for phase in shu_result.phases[:3]:
            implementations.append(
                Implementation(
                    component=phase.name,
                    technology="要検討",
                    estimated_effort=phase.duration,
                    risks=["詳細検討が必要"],
                )
            )

        return QiOutput(
            implementations=implementations,
            tool_recommendations=["プロジェクト管理ツール", "コラボレーションツール"],
            integration_points=["既存システムとの連携"],
            technical_debt_warnings=["詳細な技術検討が必要です（LLM分析未完了）"],
            domain_technologies=[],
            regulatory_considerations=[],
            geographic_considerations=[],
        )
