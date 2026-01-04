# -*- coding: utf-8 -*-
"""QiAgent - 技術実装Agent（器）v3.0.

実行計画を技術的な実装方針に変換する。
RAG使用許可（technical_docs, compliance）。
v3.0: ドメイン固有技術・規制対応・地理的考慮を追加。

Skills統合: RAGSkillで技術ドキュメント・コンプライアンス情報を参照。

【器層の核心】
「Python/FastAPI」は汎用技術。
器層は「WebRTC / SFU / TURN/STUN」のようなドメイン固有技術を明示すること。
また、規制対応（GDPR, 中国サイバーセキュリティ法）や地理的考慮（レイテンシ, PoP）を
具体的に記述すること。
"""

import json
import logging
from typing import Any

from agentflow.skills.rag import RAGConfig, RAGSkill

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
    """技術実装Agent v3.0（ドメイン固有技術・規制対応）.

    職責:
    - 技術実装方針の策定
    - ツール推奨
    - 統合ポイントの特定
    - 技術負債警告
    - **NEW: ドメイン固有技術の明示（具体名詞必須）**
    - **NEW: 規制対応事項の明示（地域別）**
    - **NEW: 地理的考慮事項の明示（レイテンシ・PoP）**

    【器層の核心】
    「Python/FastAPI」は汎用技術で、どの問題にも当てはまる。
    器層は以下を明示すること:
    - ドメイン固有技術（WebRTC, SFU, TURN/STUN等）
    - 規制対応（GDPR, 中国サイバーセキュリティ法等）
    - 地理的考慮（レイテンシ, リージョン, PoP等）

    禁止:
    - 抽象的提案
    - スコープ拡大
    - 升维思考（問題を大きくしない）
    - **汎用技術のみの列挙**

    許可:
    - RAG使用可（technical_docs, compliance）
    """

    name = "QiAgent"
    max_tokens = 1500  # 増加（新フィールド追加のため）
    temperature = 0.6

    # RAG使用許可
    USE_RAG = True
    RAG_SOURCES = ["technical_docs", "compliance"]

    # v3.0: ドメインキーワードと関連技術のマッピング
    DOMAIN_TECHNOLOGY_MAP: dict[str, list[dict[str, str]]] = {
        "会議": [
            {"name": "WebRTC", "category": "プロトコル", "why": "ブラウザベースのリアルタイム通信"},
            {"name": "SFU/MCU", "category": "サーバー", "why": "多人数会議のメディア配信"},
            {"name": "TURN/STUN", "category": "NAT越え", "why": "ファイアウォール越えの接続確立"},
        ],
        "音声": [
            {"name": "Opus", "category": "コーデック", "why": "低遅延音声圧縮"},
            {"name": "WebRTC", "category": "プロトコル", "why": "リアルタイム音声伝送"},
            {"name": "Jitter Buffer", "category": "品質制御", "why": "パケットロス・遅延変動対策"},
        ],
        "国際": [
            {"name": "CDN/Edge", "category": "配信", "why": "地理的に分散したユーザーへの低遅延配信"},
            {"name": "Data Residency", "category": "データ主権", "why": "地域別データ滞留要件"},
            {"name": "Multi-Region", "category": "インフラ", "why": "グローバル展開時の冗長性"},
        ],
    }

    # v3.0: 地域別規制マッピング
    REGIONAL_REGULATIONS: dict[str, dict[str, str]] = {
        "EU": {
            "regulation": "GDPR",
            "requirement": "個人データの域外移転制限",
            "impact": "EUリージョンでのデータ処理・保存が必要",
        },
        "中国": {
            "regulation": "中国サイバーセキュリティ法",
            "requirement": "重要データの国内保存義務",
            "impact": "中国国内のデータセンター設置が必要",
        },
        "米国": {
            "regulation": "CLOUD Act",
            "requirement": "米国企業へのデータ開示義務",
            "impact": "データ主権と米国法の衝突に注意",
        },
        "日本": {
            "regulation": "個人情報保護法",
            "requirement": "第三者提供・越境移転の同意取得",
            "impact": "プライバシーポリシーの明示が必要",
        },
    }

    def __init__(self, llm_client: Any = None) -> None:
        """初期化."""
        super().__init__(llm_client)
        self._logger = logging.getLogger(self.name)

        # RAGスキル初期化（技術ドキュメント向け）
        # 設定は routers/config.py から動的に適用される
        self._rag: RAGSkill | None = None
        self._rag_config = RAGConfig(
            top_k=3,
            min_similarity=0.4,
            system_prompt="技術実装の参考情報を提供します。",
            context_template="技術参考:\n{context}\n\n実装対象: {query}",
        )

    async def initialize_rag(self) -> None:
        """RAGスキルを初期化・開始.

        routers/config.py の設定に基づいてRAGを初期化。
        use_rag=False の場合は初期化をスキップ。
        """
        from apps.decision_governance_engine.agents.rag_config_helper import (
            initialize_agent_rag,
        )

        if self._rag is None:
            self._rag = await initialize_agent_rag(self, "qi", self._rag_config)
            if self._rag:
                await self._load_technical_docs()
                self._logger.info("RAG Skill initialized for QiAgent")

    async def _load_technical_docs(self) -> None:
        """技術ドキュメントをRAGに登録."""
        if not self._rag:
            return
        docs = [
            "FastAPIは非同期Webフレームワーク。Pydanticによる型検証とOpenAPI自動生成が特徴。",
            "Reactはコンポーネントベースのフロントエンドライブラリ。TypeScriptとの組み合わせが推奨。",
            "Docker化により環境差分を排除。CI/CDパイプラインとの統合が容易になる。",
            "AWS Lambdaはサーバーレス実行環境。コールドスタートに注意が必要。",
            "セキュリティでは認証（JWT）と認可（RBAC）の分離が重要。OWASP Top 10を参照。",
        ]
        for doc in docs:
            await self._rag.add_document(doc, topic="technical_docs")

    SYSTEM_PROMPT = """あなたはQiAgent（器）v3.0です。
実行計画を具体的な技術実装方針に変換します。

【器層の核心原則】
「Python/FastAPI」「React/TypeScript」は汎用技術で、どの問題にも当てはまる。
器層は以下を明示すること:
1. ドメイン固有技術（WebRTC, SFU, Opus, TURN/STUN等）
2. 規制対応（GDPR, 中国サイバーセキュリティ法等）
3. 地理的考慮（レイテンシ, リージョン, PoP等）

【具体名詞必須】
× 「リアルタイム通信基盤」（抽象的）
○ 「WebRTC + mediasoup SFU + Opus コーデック」（具体的）

× 「コンプライアンス対応」（抽象的）
○ 「GDPR Article 44準拠のデータ移転メカニズム」（具体的）

【禁止事項】
- 抽象的な提案は禁止
- スコープを拡大してはいけない
- 汎用技術のみの列挙は禁止

【出力形式】
必ず以下のJSON形式で出力してください：
{
    "implementations": [
        {
            "component": "コンポーネント名",
            "technology": "使用技術（具体名詞）",
            "estimated_effort": "見積もり工数",
            "risks": ["技術リスク1"]
        }
    ],
    "tool_recommendations": ["ツール1", "ツール2"],
    "integration_points": ["統合ポイント1"],
    "technical_debt_warnings": ["技術負債警告1"],
    "domain_technologies": [
        {
            "technology_name": "WebRTC",
            "category": "プロトコル",
            "why_required": "ブラウザベースのリアルタイム通信に必須",
            "alternatives": ["SIP", "RTMP"]
        }
    ],
    "regulatory_considerations": [
        {
            "region": "EU",
            "regulation": "GDPR",
            "requirement": "個人データの域外移転制限",
            "implementation_impact": "EUリージョンでのデータ処理・保存が必要"
        }
    ],
    "geographic_considerations": [
        {
            "region": "アジア太平洋",
            "latency_requirement": "200ms以下",
            "infrastructure_need": "東京・シンガポールリージョンにPoP設置"
        }
    ]
}"""

    def _parse_input(self, input_data: dict[str, Any]) -> QiInput:
        """入力をパース."""
        if "shu_result" in input_data and isinstance(input_data["shu_result"], dict):
            input_data["shu_result"] = ShuOutput(**input_data["shu_result"])
        return QiInput(**input_data)

    async def process(self, input_data: QiInput) -> QiOutput:
        """技術実装方針を策定."""
        shu_result = input_data.shu_result
        tech_constraints = input_data.tech_constraints

        if self._llm:
            return await self._plan_with_llm(shu_result, tech_constraints)

        return self._plan_rule_based(shu_result, tech_constraints)

    async def _plan_with_llm(
        self,
        shu_result: ShuOutput,
        tech_constraints: list[str],
    ) -> QiOutput:
        """LLMを使用した技術計画（v3.0: ドメイン固有技術対応）."""
        phases_info = "\n".join(
            f"Phase {p.phase_number}: {p.name} ({p.duration})\n  Actions: {', '.join(p.actions)}"
            for p in shu_result.phases
        )

        # v3.0: 文脈特化行動から技術要件を抽出
        context_actions_info = ""
        if shu_result.context_specific_actions:
            context_actions_info = "\n【文脈特化行動】\n"
            for csa in shu_result.context_specific_actions:
                context_actions_info += f"- {csa.action}\n"

        # v3.0: 単一検証ポイントを取得
        validation_info = ""
        if shu_result.single_validation_point:
            svp = shu_result.single_validation_point
            validation_info = f"\n【検証ポイント】\n- {svp.validation_target} (成功基準: {svp.success_criteria})\n"

        # RAGから技術参考情報を取得
        rag_context = ""
        if self._rag and self.USE_RAG:
            try:
                actions_str = " ".join(
                    action for p in shu_result.phases for action in p.actions[:2]
                )
                rag_result = await self._rag.query(actions_str, topic="technical_docs")
                if rag_result.sources:
                    rag_context = f"\n\n【参考: 技術ドキュメント】\n{rag_result.context_used}"
                    self._logger.info(f"RAG retrieved {len(rag_result.sources)} sources")
            except Exception as e:
                self._logger.warning(f"RAG query failed: {e}")

        user_prompt = f"""【実行フェーズ】
{phases_info}
{context_actions_info}
{validation_info}

【技術制約】{', '.join(tech_constraints) if tech_constraints else "特になし"}{rag_context}

上記を技術実装方針に変換してください。

【重要】以下を必ず含めること:
1. domain_technologies: ドメイン固有技術（具体名詞必須、抽象化禁止）
2. regulatory_considerations: 対象地域の規制対応事項
3. geographic_considerations: 地理的考慮事項（レイテンシ、PoP等）

JSON形式で出力してください。"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{user_prompt}")

        try:
            # JSON部分を抽出してパース（堅牢な抽出）
            from agentflow.utils import extract_json
            data = extract_json(response)
            if data is None:
                raise json.JSONDecodeError("No valid JSON found", response, 0)

            implementations = [Implementation(**i) for i in data.get("implementations", [])]
            if not implementations:
                implementations = self._generate_default_implementations(shu_result)

            # v3.0: ドメイン固有技術をパース
            domain_technologies = []
            for dt in data.get("domain_technologies", [])[:5]:
                domain_technologies.append(DomainSpecificTechnology(
                    technology_name=dt.get("technology_name", "")[:30],
                    category=dt.get("category", "")[:20],
                    why_required=dt.get("why_required", "")[:50],
                    alternatives=dt.get("alternatives", [])[:3],
                ))

            # v3.0: 規制対応事項をパース
            regulatory_considerations = []
            for rc in data.get("regulatory_considerations", [])[:5]:
                regulatory_considerations.append(RegulatoryConsideration(
                    region=rc.get("region", "")[:20],
                    regulation=rc.get("regulation", "")[:30],
                    requirement=rc.get("requirement", "")[:50],
                    implementation_impact=rc.get("implementation_impact", "")[:50],
                ))

            # v3.0: 地理的考慮事項をパース
            geographic_considerations = []
            for gc in data.get("geographic_considerations", [])[:5]:
                geographic_considerations.append(GeographicConsideration(
                    region=gc.get("region", "")[:20],
                    latency_requirement=gc.get("latency_requirement", "")[:30],
                    infrastructure_need=gc.get("infrastructure_need", "")[:50],
                ))

            return QiOutput(
                implementations=implementations,
                tool_recommendations=data.get("tool_recommendations", []),
                integration_points=data.get("integration_points", []),
                technical_debt_warnings=data.get("technical_debt_warnings", []),
                domain_technologies=domain_technologies,
                regulatory_considerations=regulatory_considerations,
                geographic_considerations=geographic_considerations,
            )
        except json.JSONDecodeError as e:
            self._logger.warning(f"LLM response parse failed: {e}")
            return self._plan_rule_based(shu_result, tech_constraints)

    def _plan_rule_based(
        self,
        shu_result: ShuOutput,
        tech_constraints: list[str],
    ) -> QiOutput:
        """ルールベース技術計画（v3.0: ドメイン固有技術対応）."""
        implementations = self._generate_default_implementations(shu_result)

        # v3.0: ドメイン固有技術を推定
        domain_technologies = self._infer_domain_technologies(shu_result, tech_constraints)

        # v3.0: 規制対応事項を生成（デフォルト）
        regulatory_considerations = self._generate_default_regulatory_considerations(tech_constraints)

        # v3.0: 地理的考慮事項を生成（デフォルト）
        geographic_considerations = self._generate_default_geographic_considerations(tech_constraints)

        return QiOutput(
            implementations=implementations,
            tool_recommendations=[
                "プロジェクト管理: Jira / Linear",
                "コミュニケーション: Slack",
                "ドキュメント: Notion / Confluence",
            ],
            integration_points=[
                "既存システムとのAPI連携",
                "データベース移行",
                "認証・認可システム統合",
            ],
            technical_debt_warnings=[
                "短期間開発によるテストカバレッジ低下リスク",
                "ドキュメント不足による属人化リスク",
            ],
            domain_technologies=domain_technologies,
            regulatory_considerations=regulatory_considerations,
            geographic_considerations=geographic_considerations,
        )

    def _infer_domain_technologies(
        self, shu_result: ShuOutput, tech_constraints: list[str]
    ) -> list[DomainSpecificTechnology]:
        """ドメイン固有技術を推定（v3.0）."""
        technologies = []

        # フェーズのアクションからドメインキーワードを検出
        all_actions = " ".join(
            " ".join(p.actions) for p in shu_result.phases
        )

        # 文脈特化行動からも検出
        if shu_result.context_specific_actions:
            all_actions += " " + " ".join(
                csa.action for csa in shu_result.context_specific_actions
            )

        for keyword, tech_list in self.DOMAIN_TECHNOLOGY_MAP.items():
            if keyword in all_actions or keyword in " ".join(tech_constraints):
                for tech in tech_list[:2]:
                    technologies.append(DomainSpecificTechnology(
                        technology_name=tech["name"],
                        category=tech["category"],
                        why_required=tech["why"],
                        alternatives=[],
                    ))

        # 最低限の技術を追加
        if not technologies:
            technologies.append(DomainSpecificTechnology(
                technology_name="要検討",
                category="未分類",
                why_required="ドメイン固有技術の調査が必要",
                alternatives=[],
            ))

        return technologies[:5]

    def _generate_default_regulatory_considerations(
        self, tech_constraints: list[str]
    ) -> list[RegulatoryConsideration]:
        """デフォルトの規制対応事項を生成（v3.0）."""
        considerations = []

        # 制約からキーワードを検出
        constraints_str = " ".join(tech_constraints)

        for region, reg_info in self.REGIONAL_REGULATIONS.items():
            # 地域名または規制名が制約に含まれているか
            if region in constraints_str or reg_info["regulation"] in constraints_str:
                considerations.append(RegulatoryConsideration(
                    region=region,
                    regulation=reg_info["regulation"],
                    requirement=reg_info["requirement"],
                    implementation_impact=reg_info["impact"],
                ))

        # 国際/グローバルが含まれていれば主要地域を追加
        if "国際" in constraints_str or "グローバル" in constraints_str:
            for region in ["EU", "中国", "米国"]:
                if region not in [c.region for c in considerations]:
                    reg_info = self.REGIONAL_REGULATIONS[region]
                    considerations.append(RegulatoryConsideration(
                        region=region,
                        regulation=reg_info["regulation"],
                        requirement=reg_info["requirement"],
                        implementation_impact=reg_info["impact"],
                    ))

        return considerations[:5]

    def _generate_default_geographic_considerations(
        self, tech_constraints: list[str]
    ) -> list[GeographicConsideration]:
        """デフォルトの地理的考慮事項を生成（v3.0）."""
        considerations = []

        constraints_str = " ".join(tech_constraints)

        # 国際/グローバルが含まれていれば主要地域を追加
        if "国際" in constraints_str or "グローバル" in constraints_str:
            considerations = [
                GeographicConsideration(
                    region="アジア太平洋",
                    latency_requirement="200ms以下",
                    infrastructure_need="東京・シンガポールリージョンにPoP設置",
                ),
                GeographicConsideration(
                    region="欧州",
                    latency_requirement="150ms以下",
                    infrastructure_need="フランクフルト・ロンドンリージョン",
                ),
                GeographicConsideration(
                    region="北米",
                    latency_requirement="100ms以下",
                    infrastructure_need="バージニア・オレゴンリージョン",
                ),
            ]

        return considerations[:5]

    def _generate_default_implementations(self, shu_result: ShuOutput) -> list[Implementation]:
        """デフォルト実装要素を生成."""
        return [
            Implementation(
                component="コアシステム",
                technology="Python / FastAPI",
                estimated_effort="2人月",
                risks=["技術選定の妥当性", "スケーラビリティ"],
            ),
            Implementation(
                component="フロントエンド",
                technology="React / TypeScript",
                estimated_effort="1.5人月",
                risks=["UX設計の不確実性"],
            ),
            Implementation(
                component="インフラ",
                technology="AWS / Docker",
                estimated_effort="0.5人月",
                risks=["コスト見積もり精度"],
            ),
        ]

