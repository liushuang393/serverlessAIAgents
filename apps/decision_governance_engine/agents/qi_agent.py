"""QiAgent - 技術実装Agent（器）.

実行計画を技術的な実装方針に変換する。

注意:
    - 通常は LLM を利用して詳細化する。
    - ただし、テスト/ローカル環境などで LLM が未設定の場合でもパイプライン全体を
      破綻させないため、最低限のルールベース・フォールバックを返す。
"""

import logging
from typing import Any

from apps.decision_governance_engine.schemas.agent_schemas import (
    DomainSpecificTechnology,
    GeographicConsideration,
    Implementation,
    QiInput,
    QiOutput,
    RegulatoryConsideration,
    ShuOutput,
)

from agentflow import ResilientAgent
from agentflow.core.exceptions import AgentOutputValidationError


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

【あなたの責任】
1. 各フェーズに必要な技術要素を特定
2. ドメイン固有の技術（WebRTC, SFU等）を明示
3. 規制対応・地理的要因を考慮

【出力形式】必ずJSON形式で出力してください：
{
    "implementations": [  // 実装要素（必須）
        {"component": "コンポーネント名", "technology": "使用技術", "estimated_effort": "工数", "risks": ["リスク"]}
    ],
    "tool_recommendations": ["ツール1", "ツール2"],  // 推奨ツール
    "integration_points": ["統合ポイント1"],  // 統合ポイント
    "technical_debt_warnings": ["警告1"],  // 技術負債警告
    "domain_technologies": [  // ★最大5個まで
        {"technology_name": "技術名(80字以内)", "category": "分類(30字以内)", "why_required": "理由(100字以内)", "alternatives": ["代替1"]}
    ],
    "regulatory_considerations": [  // ★最大5個まで
        {"region": "地域(20字)", "regulation": "規制名(30字)", "requirement": "要件(50字)", "implementation_impact": "影響(50字以内)"}
    ],
    "geographic_considerations": [  // ★最大5個まで
        {"region": "地域(20字)", "latency_requirement": "遅延要件(30字)", "infrastructure_need": "インフラ(50字)"}
    ]
}

【重要な制約】
- domain_technologies: 最大5個まで
- regulatory_considerations: 最大5個まで、各フィールド文字数制限厳守
- geographic_considerations: 最大5個まで
- 必ずJSONのみを出力（説明文は不要）"""

    def _parse_input(self, input_data: dict[str, Any]) -> QiInput:
        """入力をパース."""
        if "shu_result" in input_data and isinstance(input_data["shu_result"], dict):
            input_data["shu_result"] = ShuOutput(**input_data["shu_result"])
        return QiInput(**input_data)

    async def process(self, input_data: QiInput) -> QiOutput:
        """技術実装方針を策定.

        I/O:
            - Input: ShuAgent の出力（フェーズ） + 技術制約
            - Output: 実装要素/推奨ツール/統合ポイント等

        注意:
            - LLM が利用できない場合は、最低限のデフォルト出力を返す（例外は投げない）。
        """

        shu_result = input_data.shu_result
        tech_constraints = input_data.tech_constraints

        if not self._llm:
            self._logger.warning("QiAgent: LLM not available, returning default output")
            return self._create_default_output(shu_result)


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
            self._logger.warning("QiAgent: LLM returned empty response - triggering retry")
            # 空レスポンスの場合はリトライをトリガー（デフォルトに頼らない）
            raise AgentOutputValidationError(
                agent_name=self.name,
                field_name="response",
                expected="non-empty JSON response",
                actual="empty response from LLM",
            )

        from agentflow.utils import extract_json
        data = extract_json(response)
        if data is None:
            self._logger.warning(f"QiAgent: Failed to extract JSON. Response preview: {response[:500]}")
            # JSONパース失敗時もリトライをトリガー
            raise AgentOutputValidationError(
                agent_name=self.name,
                field_name="response",
                expected="valid JSON",
                actual=f"invalid JSON: {response[:100]}...",
            )

        # 詳細ログ: 抽出されたJSON
        self._logger.debug(f"Extracted JSON: {data}")

        # リスト長を正規化（max_length制約を遵守）
        self._normalize_list_lengths(data)

        # 文字列長を正規化してからPydanticに渡す
        domain_techs = [
            self._normalize_domain_technology(dt)
            for dt in data.get("domain_technologies", [])[:5]
        ]
        regulatory = [
            self._normalize_regulatory_consideration(rc)
            for rc in data.get("regulatory_considerations", [])[:5]
        ]
        geographic = [
            self._normalize_geographic_consideration(gc)
            for gc in data.get("geographic_considerations", [])[:5]
        ]

        return QiOutput(
            implementations=[Implementation(**i) for i in data.get("implementations", [])],
            tool_recommendations=data.get("tool_recommendations", []),
            integration_points=data.get("integration_points", []),
            technical_debt_warnings=data.get("technical_debt_warnings", []),
            domain_technologies=[DomainSpecificTechnology(**dt) for dt in domain_techs],
            regulatory_considerations=[RegulatoryConsideration(**rc) for rc in regulatory],
            geographic_considerations=[GeographicConsideration(**gc) for gc in geographic],
        )

    def _normalize_domain_technology(self, dt: dict) -> dict:
        """DomainSpecificTechnologyの文字列長を正規化.

        Args:
            dt: ドメイン固有技術データ

        Returns:
            正規化されたデータ
        """
        if not isinstance(dt, dict):
            return dt
        return {
            "technology_name": str(dt.get("technology_name", ""))[:80],
            "category": str(dt.get("category", ""))[:30],
            "why_required": str(dt.get("why_required", ""))[:100],
            "alternatives": dt.get("alternatives", [])[:3],
        }

    def _normalize_regulatory_consideration(self, rc: dict) -> dict:
        """RegulatoryConsiderationの文字列長を正規化.

        Args:
            rc: 規制対応事項データ

        Returns:
            正規化されたデータ
        """
        if not isinstance(rc, dict):
            return rc
        return {
            "region": str(rc.get("region", ""))[:20],
            "regulation": str(rc.get("regulation", ""))[:30],
            "requirement": str(rc.get("requirement", ""))[:50],
            "implementation_impact": str(rc.get("implementation_impact", ""))[:50],
        }

    def _normalize_geographic_consideration(self, gc: dict) -> dict:
        """GeographicConsiderationの文字列長を正規化.

        Args:
            gc: 地理的考慮事項データ

        Returns:
            正規化されたデータ
        """
        if not isinstance(gc, dict):
            return gc
        return {
            "region": str(gc.get("region", ""))[:20],
            "latency_requirement": str(gc.get("latency_requirement", ""))[:30],
            "infrastructure_need": str(gc.get("infrastructure_need", ""))[:50],
        }

    def _normalize_list_lengths(self, data: dict) -> None:
        """リスト長を正規化（max_length制約を遵守）.

        Args:
            data: 抽出されたJSONデータ
        """
        # domain_technologies (max 5)
        domain_tech = data.get("domain_technologies", [])
        if isinstance(domain_tech, list) and len(domain_tech) > 5:
            self._logger.warning(f"domain_technologies has {len(domain_tech)} items (max 5), truncating")
            data["domain_technologies"] = domain_tech[:5]

        # regulatory_considerations (max 5)
        regulatory = data.get("regulatory_considerations", [])
        if isinstance(regulatory, list) and len(regulatory) > 5:
            self._logger.warning(f"regulatory_considerations has {len(regulatory)} items (max 5), truncating")
            data["regulatory_considerations"] = regulatory[:5]

        # geographic_considerations (max 5)
        geographic = data.get("geographic_considerations", [])
        if isinstance(geographic, list) and len(geographic) > 5:
            self._logger.warning(f"geographic_considerations has {len(geographic)} items (max 5), truncating")
            data["geographic_considerations"] = geographic[:5]

        # domain_technologies内の alternatives (max 3)
        for i, dt in enumerate(data.get("domain_technologies", [])[:5]):
            if isinstance(dt, dict):
                alts = dt.get("alternatives", [])
                if isinstance(alts, list) and len(alts) > 3:
                    self._logger.warning(f"domain_technologies[{i}].alternatives has {len(alts)} items (max 3), truncating")
                    dt["alternatives"] = alts[:3]

    def validate_output(self, output: QiOutput) -> bool:
        """出力検証.

        Args:
            output: QiAgent出力

        Returns:
            検証結果
        """
        # implementations が空でないか
        if not output.implementations:
            self._logger.warning("Validation warning: implementations is empty")
            # 警告のみ、通過させる

        return True

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
