# -*- coding: utf-8 -*-
"""DaoAgent - 本質判定Agent（道）v3.0.

問題の本質を抽出し、因果齿轮で構造化し、死穴（禁忌）を明らかにする。
v3.0: 制約主導型分析・本質導出プロセスの可視化を追加。

RAG使用禁止。
"""

import json
import logging
from typing import Any

from agentflow import ResilientAgent
from apps.decision_governance_engine.schemas.agent_schemas import (
    CausalGear,
    DaoInput,
    DaoOutput,
    DeathTrap,
    DeathTrapSeverity,
    EssenceDerivation,
    ExistingAlternative,
    LeverageLevel,
    ProblemNatureType,
    ProblemType,
)


class DaoAgent(ResilientAgent[DaoInput, DaoOutput]):
    """本質判定Agent v3.0（制約主導型分析対応）.

    職責:
    - 問題タイプの分類
    - 一文での本質抽出
    - 不可変制約の特定
    - 隠れた前提の発見
    - 因果齿轮の構造化
    - 死穴（禁忌）の特定
    - **NEW: 問題の「存在理由」を追問**
    - **NEW: 既存解が使えない理由を明確化**
    - **NEW: 制約主導型問題の検出**

    禁止事項:
    - 解決策の提示
    - 行動の推奨
    - 楽観的予測
    - **テンプレートによる本質のすり替え**
    """

    name = "DaoAgent"
    max_tokens = 1200  # 出力増加に伴い拡張
    temperature = 0.3  # 低め＝安定判断

    # RAG使用禁止
    USE_RAG = False

    def __init__(self, llm_client: Any = None) -> None:
        """初期化."""
        super().__init__(llm_client)
        self._logger = logging.getLogger(self.name)

    # 問題タイプ判定キーワード
    PROBLEM_TYPE_KEYWORDS: dict[ProblemType, list[str]] = {
        ProblemType.RESOURCE_ALLOCATION: ["予算", "リソース", "配分", "投資", "人員"],
        ProblemType.TIMING_DECISION: ["いつ", "タイミング", "時期", "着手", "開始"],
        ProblemType.TRADE_OFF: ["どちら", "AとB", "選択", "両立", "トレードオフ"],
        ProblemType.RISK_ASSESSMENT: ["リスク", "危険", "懸念", "不確実"],
        ProblemType.STRATEGY_DIRECTION: ["戦略", "方針", "方向性", "ビジョン", "中長期"],
    }

    # v3.0: 制約主導型検出キーワード
    CONSTRAINT_DRIVEN_KEYWORDS: list[str] = [
        "構築", "開発", "自社", "独自", "内製",
        "国際", "グローバル", "規制", "法規制", "コンプライアンス",
        "データ主権", "セキュリティ", "機密",
    ]

    # v3.0: 既存代替手段の一般例
    COMMON_ALTERNATIVES: dict[str, list[str]] = {
        "会議": ["Zoom", "Teams", "Google Meet", "Webex"],
        "ストレージ": ["AWS S3", "Google Cloud Storage", "Azure Blob"],
        "CRM": ["Salesforce", "HubSpot", "Zoho"],
        "ERP": ["SAP", "Oracle", "NetSuite"],
        "コミュニケーション": ["Slack", "Teams", "Discord"],
    }

    # 死穴テンプレート（問題タイプ別）
    DEATH_TRAP_TEMPLATES: dict[ProblemType, list[dict[str, str]]] = {
        ProblemType.TRADE_OFF: [
            {
                "action": "両方を中途半端に実行する",
                "reason": "リソースの分散により両方とも失敗するリスク",
                "severity": "SEVERE",
            },
            {
                "action": "検証せずに一方にフルコミットする",
                "reason": "前提が間違っていた場合の損失が甚大",
                "severity": "FATAL",
            },
        ],
        ProblemType.RESOURCE_ALLOCATION: [
            {
                "action": "全リソースを一点に集中投入する",
                "reason": "その一点が失敗した場合の回復が困難",
                "severity": "SEVERE",
            },
            {
                "action": "キャッシュフローを無視してリソース配分する",
                "reason": "運転資金枯渇による事業継続リスク",
                "severity": "FATAL",
            },
        ],
        ProblemType.TIMING_DECISION: [
            {
                "action": "市場検証なしに本格着手する",
                "reason": "タイミングの妥当性が未確認のまま投資",
                "severity": "SEVERE",
            },
            {
                "action": "競合の動きを無視して遅延する",
                "reason": "市場機会の喪失、先行者優位の損失",
                "severity": "SEVERE",
            },
        ],
        ProblemType.RISK_ASSESSMENT: [
            {
                "action": "最悪ケースを想定せずに進める",
                "reason": "想定外の事態への対応不能",
                "severity": "SEVERE",
            },
            {
                "action": "リスクを過小評価して楽観的に進める",
                "reason": "後から発覚した場合のダメージが拡大",
                "severity": "FATAL",
            },
        ],
        ProblemType.STRATEGY_DIRECTION: [
            {
                "action": "戦略を決めずにリソース配分を開始する",
                "reason": "方向転換時に全投資が無駄になる",
                "severity": "FATAL",
            },
            {
                "action": "ステークホルダーの合意なしに進める",
                "reason": "実行段階での抵抗、キーパーソン離脱",
                "severity": "SEVERE",
            },
        ],
    }

    SYSTEM_PROMPT = """あなたはDaoAgent（道）v3.0です。
問題の本質を見抜き、構造化された分析を提供します。

【最重要原則】
「本質」とは「問題カテゴリ」ではありません。
「中長期的な方向性の決定」は本質ではなく、単なる分類です。

本質とは「なぜこの問題が存在するのか」への一刀です。

例:
× 「中長期的な方向性の決定」（カテゴリ）
○ 「既存のグローバル標準が当社の法規制・データ主権要件を満たせない」（本質）

【本質導出の3ステップ】
1. 表面的な問題: ユーザーが言ったことをそのまま
2. 一段深い理由: なぜそれが問題なのか
3. 根本的な制約: これがなければ問題ではない

【既存解が使えない理由の追問】
「XXを構築したい」という問題には必ず問う:
- なぜ既存のサービス（Zoom, AWS, Salesforce等）を使わないのか？
- 使えない理由は何か？（法規制？コスト？技術的制約？）

【問題の本質的性質】
- TECHNICAL_LIMITATION: 技術的に解決可能な問題
- INVESTMENT_DECISION: ROI/リソース配分の判断
- CONSTRAINT_DRIVEN: 既存解が使えない制約主導型（最も深い）
- STRATEGIC_CHOICE: 方向性・ビジョンの選択
- REGULATORY_COMPLIANCE: 規制対応が主因の問題
- MARKET_TIMING: タイミングが本質の問題

【禁止事項】
- 解決策を提示してはいけません
- 行動を推奨してはいけません
- テンプレート的な本質（「XXの決定」等）は禁止

【出力形式】
必ず以下のJSON形式で出力してください：
{
    "problem_type": "TRADE_OFF" | "RESOURCE_ALLOCATION" | "TIMING_DECISION" | "RISK_ASSESSMENT" | "STRATEGY_DIRECTION",
    "problem_nature": "TECHNICAL_LIMITATION" | "INVESTMENT_DECISION" | "CONSTRAINT_DRIVEN" | "STRATEGIC_CHOICE" | "REGULATORY_COMPLIANCE" | "MARKET_TIMING",
    "essence_derivation": {
        "surface_problem": "表面的な問題（ユーザーが言ったこと）",
        "underlying_why": "なぜそれが問題なのか（一段深い理由）",
        "root_constraint": "根本的な制約（これがなければ問題ではない）",
        "essence_statement": "本質の一文（非これ不可）"
    },
    "essence": "問題の本質を一文で（50字以内、カテゴリ禁止）",
    "existing_alternatives": [
        {
            "name": "既存解の名称（例: Zoom）",
            "why_not_viable": "なぜこの企業では使えないか",
            "specific_constraint": "具体的な制約"
        }
    ],
    "immutable_constraints": ["変えられない制約1", "制約2", ...],
    "hidden_assumptions": ["暗黙の前提1", "前提2", ...],
    "causal_gears": [
        {
            "gear_id": 1,
            "name": "齿轮名（20字以内）",
            "description": "説明（100字以内）",
            "drives": [2, 3],
            "driven_by": [],
            "leverage": "HIGH"
        }
    ],
    "bottleneck_gear": 1,
    "death_traps": [
        {
            "action": "禁止行動",
            "reason": "なぜ致命的か",
            "severity": "FATAL"
        }
    ]
}"""

    def _parse_input(self, input_data: dict[str, Any]) -> DaoInput:
        """入力をパース."""
        return DaoInput(**input_data)

    async def process(self, input_data: DaoInput) -> DaoOutput:
        """本質分析を実行."""
        question = input_data.question
        constraints = input_data.constraints

        # Step 1: 問題タイプを推定
        problem_type = self._infer_problem_type(question)

        # Step 2: LLMで詳細分析（LLMがある場合）
        if self._llm:
            return await self._analyze_with_llm(question, constraints, problem_type)

        # Step 3: LLMなしの場合はルールベース分析
        return self._analyze_rule_based(question, constraints, problem_type)

    def _infer_problem_type(self, question: str) -> ProblemType:
        """問題タイプを推定."""
        for ptype, keywords in self.PROBLEM_TYPE_KEYWORDS.items():
            if any(kw in question for kw in keywords):
                return ptype
        return ProblemType.STRATEGY_DIRECTION  # デフォルト

    async def _analyze_with_llm(
        self,
        question: str,
        constraints: list[str],
        inferred_type: ProblemType,
    ) -> DaoOutput:
        """LLMを使用した分析（v3.0: 制約主導型分析対応）."""
        # v3.0: 制約主導型問題かどうかを検出
        is_constraint_driven = self._detect_constraint_driven(question)

        # v3.0: 既存代替手段を推定
        potential_alternatives = self._infer_existing_alternatives(question)

        user_prompt = f"""【問題】
{question}

【現実制約】
{chr(10).join(f"- {c}" for c in constraints) if constraints else "特になし"}

【推定された問題タイプ】
{inferred_type.value}

【制約主導型問題の可能性】
{is_constraint_driven}

【考えられる既存代替手段】
{', '.join(potential_alternatives) if potential_alternatives else "不明"}

上記の問題を分析し、JSON形式で出力してください。

【最重要】
1. 「essence」はカテゴリ（「XXの決定」）ではなく、問題の存在理由を一文で
2. 「existing_alternatives」で既存解が使えない理由を明確に
3. 「essence_derivation」で思考プロセスを可視化"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{user_prompt}")

        try:
            # JSON部分を抽出してパース
            json_match = response[response.find("{"):response.rfind("}") + 1]
            data = json.loads(json_match)

            # v3.0: 本質導出プロセスをパース
            essence_derivation = None
            if "essence_derivation" in data:
                ed = data["essence_derivation"]
                essence_derivation = EssenceDerivation(
                    surface_problem=ed.get("surface_problem", "")[:50],
                    underlying_why=ed.get("underlying_why", "")[:100],
                    root_constraint=ed.get("root_constraint", "")[:100],
                    essence_statement=ed.get("essence_statement", "")[:50],
                )

            # v3.0: 問題の本質的性質をパース
            problem_nature = None
            if "problem_nature" in data:
                try:
                    problem_nature = ProblemNatureType(data["problem_nature"])
                except ValueError:
                    problem_nature = ProblemNatureType.STRATEGIC_CHOICE

            # v3.0: 既存代替手段をパース
            existing_alternatives = []
            for alt_data in data.get("existing_alternatives", [])[:3]:
                existing_alternatives.append(ExistingAlternative(
                    name=alt_data.get("name", "")[:30],
                    why_not_viable=alt_data.get("why_not_viable", "")[:100],
                    specific_constraint=alt_data.get("specific_constraint", "")[:50],
                ))

            # 因果齿轮をパース
            causal_gears = []
            for gear_data in data.get("causal_gears", [])[:5]:
                causal_gears.append(CausalGear(
                    gear_id=gear_data.get("gear_id", 1),
                    name=gear_data.get("name", "")[:20],
                    description=gear_data.get("description", "")[:100],
                    drives=gear_data.get("drives", []),
                    driven_by=gear_data.get("driven_by", []),
                    leverage=LeverageLevel(gear_data.get("leverage", "MEDIUM")),
                ))

            # 死穴をパース
            death_traps = []
            for trap_data in data.get("death_traps", [])[:3]:
                death_traps.append(DeathTrap(
                    action=trap_data.get("action", ""),
                    reason=trap_data.get("reason", ""),
                    severity=DeathTrapSeverity(trap_data.get("severity", "SEVERE")),
                ))

            # v3.0: 本質の検証（テンプレート的な回答を拒否）
            essence = data.get("essence", "")[:50]
            if self._is_template_essence(essence):
                self._logger.warning(f"Template essence detected: {essence}")
                # 本質導出プロセスからessenceを再構成
                if essence_derivation:
                    essence = essence_derivation.essence_statement

            return DaoOutput(
                problem_type=ProblemType(data.get("problem_type", inferred_type.value)),
                essence=essence,
                immutable_constraints=data.get("immutable_constraints", [])[:5],
                hidden_assumptions=data.get("hidden_assumptions", [])[:3],
                essence_derivation=essence_derivation,
                problem_nature=problem_nature,
                existing_alternatives=existing_alternatives,
                causal_gears=causal_gears,
                bottleneck_gear=data.get("bottleneck_gear"),
                death_traps=death_traps,
            )
        except (json.JSONDecodeError, ValueError) as e:
            self._logger.warning(f"LLM response parse failed: {e}")
            # パース失敗時はルールベースにフォールバック
            return self._analyze_rule_based(question, constraints, inferred_type)

    def _detect_constraint_driven(self, question: str) -> str:
        """制約主導型問題かどうかを検出."""
        matches = [kw for kw in self.CONSTRAINT_DRIVEN_KEYWORDS if kw in question]
        if len(matches) >= 2:
            return f"高い（キーワード: {', '.join(matches[:3])}）"
        if len(matches) == 1:
            return f"中程度（キーワード: {matches[0]}）"
        return "低い"

    def _infer_existing_alternatives(self, question: str) -> list[str]:
        """問題に対する既存代替手段を推定."""
        alternatives = []
        for category, alts in self.COMMON_ALTERNATIVES.items():
            if category in question:
                alternatives.extend(alts[:2])
        return alternatives[:4]

    def _is_template_essence(self, essence: str) -> bool:
        """本質がテンプレート的かどうかを判定."""
        template_patterns = [
            "の決定", "の判断", "の選択", "の評価",
            "方向性", "方針", "戦略",
            "最適な", "効果的な", "適切な",
        ]
        return any(pattern in essence for pattern in template_patterns)

    def _analyze_rule_based(
        self,
        question: str,
        constraints: list[str],
        problem_type: ProblemType,
    ) -> DaoOutput:
        """ルールベース分析（LLMなし）v3.0対応."""
        # v3.0: 問題の本質的性質を判定
        problem_nature = self._infer_problem_nature(question, constraints)

        # v3.0: 本質導出プロセスを構築
        essence_derivation = self._derive_essence(question, problem_type, constraints)

        # 本質の抽出（v3.0: 導出プロセスから）
        essence = essence_derivation.essence_statement

        # v3.0: 既存代替手段を分析
        existing_alternatives = self._analyze_existing_alternatives(question, constraints)

        # 不可変制約
        immutable = constraints[:5] if constraints else ["制約情報なし"]

        # 隠れた前提（一般的なもの）
        hidden = [
            "現状の市場環境が継続する",
            "チームの能力が維持される",
            "資金調達に大きな変化がない",
        ][:3]

        # 因果齿轮を生成
        causal_gears = self._generate_default_gears(problem_type)

        # 瓶颈齿轮を特定
        bottleneck = self._identify_bottleneck(causal_gears)

        # 死穴を生成
        death_traps = self._generate_death_traps(problem_type, constraints)

        return DaoOutput(
            problem_type=problem_type,
            essence=essence[:50],
            immutable_constraints=immutable,
            hidden_assumptions=hidden,
            essence_derivation=essence_derivation,
            problem_nature=problem_nature,
            existing_alternatives=existing_alternatives,
            causal_gears=causal_gears,
            bottleneck_gear=bottleneck,
            death_traps=death_traps,
        )

    def _infer_problem_nature(self, question: str, constraints: list[str]) -> ProblemNatureType:
        """問題の本質的性質を推定（v3.0）."""
        # 制約主導型のキーワードチェック
        constraint_keywords = ["規制", "法規", "コンプライアンス", "データ主権", "セキュリティ", "機密"]
        if any(kw in question for kw in constraint_keywords) or any(kw in " ".join(constraints) for kw in constraint_keywords):
            return ProblemNatureType.CONSTRAINT_DRIVEN

        # 規制対応
        regulatory_keywords = ["GDPR", "個人情報", "プライバシー", "監査"]
        if any(kw in question for kw in regulatory_keywords):
            return ProblemNatureType.REGULATORY_COMPLIANCE

        # タイミング
        timing_keywords = ["いつ", "タイミング", "時期", "競合", "先行"]
        if any(kw in question for kw in timing_keywords):
            return ProblemNatureType.MARKET_TIMING

        # 投資判断
        investment_keywords = ["投資", "予算", "ROI", "コスト"]
        if any(kw in question for kw in investment_keywords):
            return ProblemNatureType.INVESTMENT_DECISION

        # 技術的問題
        tech_keywords = ["技術", "実装", "アーキテクチャ", "スケール"]
        if any(kw in question for kw in tech_keywords):
            return ProblemNatureType.TECHNICAL_LIMITATION

        return ProblemNatureType.STRATEGIC_CHOICE

    def _derive_essence(
        self, question: str, problem_type: ProblemType, constraints: list[str]
    ) -> EssenceDerivation:
        """本質導出プロセスを構築（v3.0）."""
        # 表面的な問題
        surface = question[:50] if len(question) > 50 else question

        # 一段深い理由（問題タイプから推定）
        underlying_templates = {
            ProblemType.TRADE_OFF: "複数の選択肢があり、どれを選んでも何かを失う状況",
            ProblemType.RESOURCE_ALLOCATION: "限られたリソースをどう配分するかで結果が大きく変わる",
            ProblemType.TIMING_DECISION: "実行タイミングが成否を分ける重要な変数",
            ProblemType.RISK_ASSESSMENT: "不確実性が高く、リスクの評価が判断を左右する",
            ProblemType.STRATEGY_DIRECTION: "長期的な方向性が組織の将来を決定づける",
        }
        underlying = underlying_templates.get(problem_type, "判断が必要な状況")

        # 根本的な制約
        if constraints:
            root_constraint = f"制約条件: {', '.join(constraints[:2])}"
        else:
            root_constraint = "制約条件が不明確なため、問題の本質が曖昧"

        # 本質の一文（制約主導型かどうかで分岐）
        is_constraint_driven = self._detect_constraint_driven(question)
        if "高い" in is_constraint_driven:
            essence_statement = "既存の標準解が自社の固有制約を満たせない"
        else:
            essence_statement = self._extract_essence(question, problem_type)

        return EssenceDerivation(
            surface_problem=surface,
            underlying_why=underlying,
            root_constraint=root_constraint,
            essence_statement=essence_statement[:50],
        )

    def _analyze_existing_alternatives(
        self, question: str, constraints: list[str]
    ) -> list[ExistingAlternative]:
        """既存代替手段を分析（v3.0）."""
        alternatives = []

        # 問題から関連する代替手段を推定
        for category, alts in self.COMMON_ALTERNATIVES.items():
            if category in question:
                # 代替手段ごとに「使えない理由」を推定
                for alt in alts[:2]:
                    # 制約から使えない理由を推定
                    constraint_reason = "不明"
                    if any("規制" in c or "法規" in c for c in constraints):
                        constraint_reason = "法規制の要件を満たせない可能性"
                    elif any("セキュリティ" in c or "機密" in c for c in constraints):
                        constraint_reason = "セキュリティ要件を満たせない可能性"
                    elif any("国際" in question or "グローバル" in question for _ in [1]):
                        constraint_reason = "地域別のデータ主権要件への対応が困難"

                    alternatives.append(ExistingAlternative(
                        name=alt,
                        why_not_viable=f"標準サービスでは{constraint_reason}",
                        specific_constraint=constraint_reason[:50],
                    ))

        return alternatives[:3]

    def _extract_essence(self, question: str, problem_type: ProblemType) -> str:
        """本質を一文で抽出."""
        essence_templates = {
            ProblemType.TRADE_OFF: "複数の選択肢間の最適なバランス判断",
            ProblemType.RESOURCE_ALLOCATION: "限られたリソースの最適配分判断",
            ProblemType.TIMING_DECISION: "実行タイミングの最適化判断",
            ProblemType.RISK_ASSESSMENT: "リスクと機会のバランス評価",
            ProblemType.STRATEGY_DIRECTION: "中長期的な方向性の決定",
        }
        return essence_templates.get(problem_type, "意思決定が必要な課題")

    def _generate_default_gears(self, problem_type: ProblemType) -> list[CausalGear]:
        """デフォルトの因果齿轮を生成."""
        # 共通の齿轮構造
        base_gears = [
            CausalGear(
                gear_id=1,
                name="外部環境",
                description="市場、競合、規制など外部の変数",
                drives=[3, 4],
                driven_by=[],
                leverage=LeverageLevel.HIGH,
            ),
            CausalGear(
                gear_id=2,
                name="内部リソース",
                description="予算、人員、技術力など内部の資源",
                drives=[4, 5],
                driven_by=[],
                leverage=LeverageLevel.MEDIUM,
            ),
            CausalGear(
                gear_id=3,
                name="戦略的方向性",
                description="どの方向に進むかの大きな判断",
                drives=[4],
                driven_by=[1],
                leverage=LeverageLevel.HIGH,
            ),
            CausalGear(
                gear_id=4,
                name="実行計画",
                description="具体的にどう実行するかの計画",
                drives=[5],
                driven_by=[1, 2, 3],
                leverage=LeverageLevel.MEDIUM,
            ),
            CausalGear(
                gear_id=5,
                name="組織/チーム",
                description="実行を担うチームの能力と士気",
                drives=[],
                driven_by=[2, 4],
                leverage=LeverageLevel.LOW,
            ),
        ]

        # 問題タイプに応じて調整
        if problem_type == ProblemType.TIMING_DECISION:
            base_gears[0].name = "市場タイミング"
            base_gears[0].description = "市場の成熟度、競合の動き、顧客の準備度"

        return base_gears

    def _identify_bottleneck(self, gears: list[CausalGear]) -> int:
        """瓶颈齿轮を特定."""
        # レバレッジがHIGHで、他から駆動されている齿轮を瓶颈とする
        for gear in gears:
            if gear.leverage == LeverageLevel.HIGH and gear.driven_by:
                return gear.gear_id

        # なければ最初のHIGH齿轮
        for gear in gears:
            if gear.leverage == LeverageLevel.HIGH:
                return gear.gear_id

        return 1  # デフォルト

    def _generate_death_traps(
        self, problem_type: ProblemType, constraints: list[str]
    ) -> list[DeathTrap]:
        """死穴を生成."""
        traps = []

        # 問題タイプに応じたテンプレートを取得
        templates = self.DEATH_TRAP_TEMPLATES.get(problem_type, [])
        for template in templates[:2]:
            traps.append(DeathTrap(
                action=template["action"],
                reason=template["reason"],
                severity=DeathTrapSeverity(template["severity"]),
            ))

        # 制約に基づいた死穴を追加
        if any("予算" in c or "円" in c for c in constraints):
            traps.append(DeathTrap(
                action="キャッシュフローを無視した投資判断",
                reason="運転資金の枯渇は事業継続を不可能にする",
                severity=DeathTrapSeverity.FATAL,
            ))

        return traps[:3]

    def validate_output(self, output: DaoOutput) -> bool:
        """出力検証（v3.0: 浅い出力を拒否）."""
        # 本質が空でないか
        if not output.essence:
            self._logger.warning("Validation failed: essence is empty")
            return False

        # v3.0: テンプレート的な本質を警告
        if self._is_template_essence(output.essence):
            self._logger.warning(
                f"Validation warning: template-like essence detected: {output.essence}"
            )
            # 警告のみ、通過させる（段階的移行のため）

        # v3.0: 本質導出プロセスがあるか確認
        if not output.essence_derivation:
            self._logger.warning("Validation warning: essence_derivation is missing")
            # 警告のみ、通過させる

        # v3.0: 問題の本質的性質があるか確認
        if not output.problem_nature:
            self._logger.warning("Validation warning: problem_nature is missing")
            # 警告のみ、通過させる

        # 因果齿轮が3-5個あるか
        if not (3 <= len(output.causal_gears) <= 5):
            self._logger.warning(
                f"Validation warning: causal_gears count is {len(output.causal_gears)}"
            )
            # 警告のみ、通過させる

        # 死穴があるか
        if not output.death_traps:
            self._logger.warning("Validation warning: death_traps is empty")
            # 警告のみ、通過させる

        return True
