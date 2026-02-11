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
    ArchitectureComponent,
    DomainSpecificTechnology,
    ExpansionStage,
    GeographicConsideration,
    Implementation,
    ImplementationStep,
    MinimalLogging,
    PoCMinimalArchitecture,
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

    SYSTEM_PROMPT = """あなたはQiAgent（器）v3.1です。
器Agentは「提案（技術実装）」であり、レビューではない。
出力では「警告」「指摘」「修正提案」などの文体を禁止し、
前提が整ったものとして「何を使うか」「どう組むか」「注意点」を提案する。

【必須の出力要件（v3.1）】
1) 最初に「PoC最小アーキテクチャ」を1つだけ提案する（箱と矢印で説明）：
   - コンポーネント一覧（各コンポーネントの名前・役割・PoCで使う技術）
   - データフロー説明（コンポーネント間の流れ）
   - ログ/計測（相関ID、タイムスタンプ）を最小セットで組み込む
   ※WebRTC/SFU/WORMなどは"必要条件が満たされたら追加"として別枠に退避

2) 次に「拡張アーキテクチャ」を段階別に提示する：
   - 多人数/多拠点になったら → SFU追加
   - 監査要件が厳格になったら → WORM追加
   - 音声合成や話者分離 → 品質改善フェーズで追加
   それぞれ導入条件（閾値）を明記する

3) 実装は「手順」で出す（Step1〜StepN）：
   - 最短で動くもの → 計測 → 安定化 → 統制強化
   - 各Stepに注意点と、よくある詰まりポイントの回避策を添える

4) 規制・地理はPoCでは最小化し、将来項目は「スケール時の追加要件」として隔離する。

【文体ルール】
- "警告/指摘/修正提案"は禁止。実装提案の語彙（推奨/手順/注意点/条件）に統一。
- 「採用/不採用」より「PoCではこれを使う」「スケールでこれを足す」の提案型で書く。

【出力形式】必ずJSON形式で出力してください：
{
    "poc_minimal_architecture": {
        "components": [
            {"name": "コンポーネント名", "purpose": "役割", "technology_choice": "PoCで使う技術", "notes": "注意点"}
        ],
        "data_flow_description": "コンポーネント間のデータフロー説明（矢印）",
        "minimal_logging": {
            "correlation_id_strategy": "相関ID戦略",
            "timestamp_points": ["計測ポイント1", "計測ポイント2"],
            "storage": "ログ保存先"
        },
        "deferred_components": ["PoCでは使わないが将来追加するもの"]
    },
    "expansion_stages": [
        {"stage_name": "段階名", "introduction_condition": "導入条件/閾値", "added_components": ["追加コンポーネント"], "rationale": "追加理由"}
    ],
    "implementation_steps": [
        {"step_number": 1, "objective": "目標", "tasks": ["作業1"], "notes": ["注意点"], "common_pitfalls": ["詰まりポイントと回避策"]}
    ],
    "future_scale_requirements": ["将来スケール時の追加要件"],
    "implementations": [
        {"component": "コンポーネント名", "technology": "使用技術", "estimated_effort": "工数", "risks": ["リスク"]}
    ],
    "tool_recommendations": ["ツール1"],
    "integration_points": ["統合ポイント1"],
    "domain_technologies": [
        {"technology_name": "技術名", "category": "分類", "why_required": "理由", "alternatives": ["代替"]}
    ]
}"""

    def _parse_input(self, input_data: dict[str, Any]) -> QiInput:
        """入力をパース."""
        if "shu_result" in input_data and isinstance(input_data["shu_result"], dict):
            input_data["shu_result"] = ShuOutput(**input_data["shu_result"])
        return QiInput(**input_data)

    async def process(self, input_data: QiInput) -> QiOutput:
        """技術実装方針を策定（v3.1: 提案モード）.

        I/O:
            - Input: ShuAgent の出力（フェーズ） + 技術制約
            - Output: PoC最小アーキテクチャ/拡張段階/実装手順 + 従来フィールド

        注意:
            - LLM が利用できない場合は、最低限のデフォルト出力を返す（例外は投げない）。
        """

        shu_result = input_data.shu_result
        tech_constraints = input_data.tech_constraints

        if not self._llm:
            self._logger.warning("QiAgent: LLM not available, returning default output")
            return self._create_default_output(shu_result)

        # v3.1: フェーズ情報（2段ロケットがあればそれを優先）
        phases_info = ""
        if shu_result.two_stage_rocket:
            rocket = shu_result.two_stage_rocket
            phases_info = f"【Stage1】{rocket.stage1_minimal_pipeline.stage_name}\n"
            phases_info += f"  目標: {rocket.stage1_minimal_pipeline.objective}\n"
            for p in rocket.stage1_minimal_pipeline.phases:
                phases_info += f"  Phase {p.phase_number}: {p.name} ({p.duration}) - {', '.join(p.tasks)}\n"
            phases_info += f"【Stage2】{rocket.stage2_governance.stage_name}\n"
            phases_info += f"  目標: {rocket.stage2_governance.objective}\n"
            for p in rocket.stage2_governance.phases:
                phases_info += f"  Phase {p.phase_number}: {p.name} ({p.duration}) - {', '.join(p.tasks)}\n"
        else:
            phases_info = "\n".join(
                f"Phase {p.phase_number}: {p.name} ({p.duration}) - {', '.join(p.actions)}"
                for p in shu_result.phases
            )

        # v3.1: PoC DoD情報
        dod_info = ""
        if shu_result.poc_definition_of_done:
            dod = shu_result.poc_definition_of_done
            dod_info = "\n【PoC完成定義】\n"
            dod_info += f"  体験条件: {', '.join(dod.experience_conditions)}\n"
            dod_info += f"  フォールバック: {dod.fallback_strategy}\n"

        user_prompt = f"""{phases_info}
{dod_info}
【技術制約】{', '.join(tech_constraints) if tech_constraints else "特になし"}

上記の実行計画を技術実装に落とし込む。提案口調で書くこと。

【v3.1 必須出力】
1. poc_minimal_architecture: PoC最小アーキテクチャ（箱と矢印 + 最小ログ）
2. expansion_stages: 拡張アーキテクチャ（導入条件付き、段階別）
3. implementation_steps: 実装手順（Step1〜StepN、注意点と詰まりポイント付き）
4. future_scale_requirements: 将来スケール要件（PoC外に隔離）
5. implementations: 実装要素（コンポーネント/技術/工数/リスク）

JSON形式で出力してください。"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{user_prompt}")

        self._logger.debug(f"QiAgent LLM response length: {len(response) if response else 0}")
        if not response or not response.strip():
            self._logger.warning("QiAgent: LLM returned empty response - triggering retry")
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
            raise AgentOutputValidationError(
                agent_name=self.name,
                field_name="response",
                expected="valid JSON",
                actual=f"invalid JSON: {response[:100]}...",
            )

        self._logger.debug(f"Extracted JSON keys: {list(data.keys())}")

        # リスト長を正規化（max_length制約を遵守）
        self._normalize_list_lengths(data)

        # v3.0: 文字列長を正規化してからPydanticに渡す
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

        # v3.1: PoC最小アーキテクチャをパース
        poc_arch = self._parse_poc_minimal_architecture(data.get("poc_minimal_architecture", {}))

        # v3.1: 拡張アーキテクチャ段階をパース
        expansion = self._parse_expansion_stages(data.get("expansion_stages", []))

        # v3.1: 実装手順をパース
        impl_steps = self._parse_implementation_steps(data.get("implementation_steps", []))

        # v3.1: 将来スケール要件
        future_reqs = [str(r)[:100] for r in data.get("future_scale_requirements", [])[:10]]

        return QiOutput(
            implementations=[Implementation(**i) for i in data.get("implementations", [])],
            tool_recommendations=data.get("tool_recommendations", []),
            integration_points=data.get("integration_points", []),
            technical_debt_warnings=data.get("technical_debt_warnings", []),
            domain_technologies=[DomainSpecificTechnology(**dt) for dt in domain_techs],
            regulatory_considerations=[RegulatoryConsideration(**rc) for rc in regulatory],
            geographic_considerations=[GeographicConsideration(**gc) for gc in geographic],
            poc_minimal_architecture=poc_arch,
            expansion_stages=expansion,
            implementation_steps=impl_steps,
            future_scale_requirements=future_reqs,
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
        """デフォルト出力を生成（v3.1: 提案モード対応）.

        Args:
            shu_result: ShuAgent出力

        Returns:
            v3.1対応のQiOutput
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

        # v3.1: デフォルトPoC最小アーキテクチャ
        poc_arch = self._generate_default_poc_architecture()

        # v3.1: デフォルト拡張アーキテクチャ段階
        expansion = self._generate_default_expansion_stages()

        # v3.1: デフォルト実装手順
        impl_steps = self._generate_default_implementation_steps()

        return QiOutput(
            implementations=implementations,
            tool_recommendations=["開発環境ツール", "CI/CDパイプライン"],
            integration_points=["既存システムとの連携"],
            technical_debt_warnings=[],
            domain_technologies=[],
            regulatory_considerations=[],
            geographic_considerations=[],
            poc_minimal_architecture=poc_arch,
            expansion_stages=expansion,
            implementation_steps=impl_steps,
            future_scale_requirements=[
                "規制対応（対象地域拡大時に追加）",
                "地理的分散（マルチリージョン展開時に追加）",
                "高可用性構成（本番環境展開時に追加）",
            ],
        )

    # --- v3.1 ヘルパーメソッド ---

    def _parse_poc_minimal_architecture(self, data: dict | None) -> PoCMinimalArchitecture | None:
        """LLM出力からPoC最小アーキテクチャをパースする."""
        if not data or not isinstance(data, dict):
            return None
        try:
            components = []
            for c in data.get("components", [])[:10]:
                if isinstance(c, dict):
                    components.append(ArchitectureComponent(
                        name=str(c.get("name", ""))[:50],
                        purpose=str(c.get("purpose", ""))[:80],
                        technology_choice=str(c.get("technology_choice", ""))[:80],
                        notes=str(c.get("notes", ""))[:100],
                    ))
            if not components:
                return None

            # 最小ログ設定をパース
            ml_data = data.get("minimal_logging", {})
            minimal_logging = None
            if ml_data and isinstance(ml_data, dict):
                minimal_logging = MinimalLogging(
                    correlation_id_strategy=str(ml_data.get("correlation_id_strategy", "UUID"))[:100],
                    timestamp_points=ml_data.get("timestamp_points", [])[:5],
                    storage=str(ml_data.get("storage", ""))[:80],
                )

            return PoCMinimalArchitecture(
                components=components,
                data_flow_description=str(data.get("data_flow_description", ""))[:300],
                minimal_logging=minimal_logging,
                deferred_components=data.get("deferred_components", [])[:10],
            )
        except Exception as e:
            self._logger.warning(f"PoC architecture parse failed: {e}")
            return None

    def _parse_expansion_stages(self, raw_stages: list) -> list[ExpansionStage]:
        """LLM出力から拡張アーキテクチャ段階をパースする."""
        if not raw_stages or not isinstance(raw_stages, list):
            return []
        stages = []
        for s in raw_stages[:5]:
            if not isinstance(s, dict):
                continue
            try:
                added = s.get("added_components", [])
                if not added:
                    continue
                stages.append(ExpansionStage(
                    stage_name=str(s.get("stage_name", ""))[:50],
                    introduction_condition=str(s.get("introduction_condition", ""))[:100],
                    added_components=[str(c)[:50] for c in added[:5]],
                    rationale=str(s.get("rationale", ""))[:150],
                ))
            except Exception as e:
                self._logger.warning(f"ExpansionStage parse failed: {e}")
        return stages

    def _parse_implementation_steps(self, raw_steps: list) -> list[ImplementationStep]:
        """LLM出力から実装手順をパースする."""
        if not raw_steps or not isinstance(raw_steps, list):
            return []
        steps = []
        for s in raw_steps[:10]:
            if not isinstance(s, dict):
                continue
            try:
                tasks = s.get("tasks", [])
                if not tasks:
                    continue
                steps.append(ImplementationStep(
                    step_number=int(s.get("step_number", len(steps) + 1)),
                    objective=str(s.get("objective", ""))[:80],
                    tasks=[str(t)[:80] for t in tasks[:5]],
                    notes=[str(n)[:100] for n in s.get("notes", [])[:3]],
                    common_pitfalls=[str(p)[:100] for p in s.get("common_pitfalls", [])[:3]],
                ))
            except Exception as e:
                self._logger.warning(f"ImplementationStep parse failed: {e}")
        return steps

    def _generate_default_poc_architecture(self) -> PoCMinimalArchitecture:
        """デフォルトPoC最小アーキテクチャを生成する（v3.1）."""
        return PoCMinimalArchitecture(
            components=[
                ArchitectureComponent(
                    name="入力取得", purpose="ユーザー入力を受け付ける",
                    technology_choice="要選定", notes="最小構成で開始する",
                ),
                ArchitectureComponent(
                    name="コア処理", purpose="主要なビジネスロジックを実行する",
                    technology_choice="要選定", notes="最小機能に限定する",
                ),
                ArchitectureComponent(
                    name="出力配信", purpose="処理結果をユーザーに提供する",
                    technology_choice="要選定", notes="基本的な表示のみ",
                ),
            ],
            data_flow_description="入力取得 → コア処理 → 出力配信（最小限のパイプライン）",
            minimal_logging=MinimalLogging(
                correlation_id_strategy="リクエスト毎にUUIDを発行する",
                timestamp_points=["入力受付時", "処理完了時", "出力完了時"],
                storage="ローカルファイルまたはCloudWatch Logs",
            ),
            deferred_components=["高可用性構成", "マルチリージョン対応", "高度な監査ログ"],
        )

    def _generate_default_expansion_stages(self) -> list[ExpansionStage]:
        """デフォルト拡張アーキテクチャ段階を生成する（v3.1）."""
        return [
            ExpansionStage(
                stage_name="スケールアウト対応",
                introduction_condition="同時接続ユーザーが増加した場合",
                added_components=["ロードバランサー", "水平スケーリング"],
                rationale="単一インスタンスでは処理しきれない負荷に対応するため",
            ),
            ExpansionStage(
                stage_name="監査・コンプライアンス強化",
                introduction_condition="監査要件が厳格化した場合",
                added_components=["改竄防止ログ", "アクセス制御強化"],
                rationale="規制要件を満たすために証跡の完全性を確保するため",
            ),
            ExpansionStage(
                stage_name="品質改善",
                introduction_condition="基本機能が安定稼働した後",
                added_components=["品質監視", "パフォーマンス最適化"],
                rationale="ユーザー体験を向上させるため",
            ),
        ]

    def _generate_default_implementation_steps(self) -> list[ImplementationStep]:
        """デフォルト実装手順を生成する（v3.1）."""
        return [
            ImplementationStep(
                step_number=1,
                objective="最短で動くものを作る",
                tasks=["技術選定", "開発環境構築", "コア機能の最小実装"],
                notes=["完璧を目指さず動作を優先する"],
                common_pitfalls=["技術選定に時間をかけすぎる → 2案に絞って比較で決める"],
            ),
            ImplementationStep(
                step_number=2,
                objective="計測環境を整える",
                tasks=["相関IDの実装", "タイムスタンプ計測", "基本メトリクス収集"],
                notes=["ログは最小限から始める"],
                common_pitfalls=["過剰なログ設計 → 相関IDとタイムスタンプのみで開始する"],
            ),
            ImplementationStep(
                step_number=3,
                objective="安定化と品質向上",
                tasks=["エラーハンドリング強化", "パフォーマンス計測", "テスト追加"],
                notes=["計測データに基づいて改善する"],
                common_pitfalls=["全部直そうとする → 計測で見つかった上位3件に集中する"],
            ),
            ImplementationStep(
                step_number=4,
                objective="統制を段階的に強化する",
                tasks=["監査ログ実装", "権限管理導入", "データ保持ポリシー適用"],
                notes=["Stage1の成果を壊さないよう注意する"],
                common_pitfalls=["最初から完全な統制を入れる → 最低限から段階的に追加する"],
            ),
        ]
