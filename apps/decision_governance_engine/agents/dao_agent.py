"""DaoAgent - 本質判定Agent（道）v3.1.

問題の本質を抽出し、因果齿轮で構造化し、死穴（禁忌）を明らかにする。
v3.0: 制約主導型分析・本質導出プロセスの可視化を追加。
v3.1: 制約境界条件、解空間3案比較、定量指標化、監査証拠チェックリスト、セルフチェック追加。

RAG使用禁止。
"""

import json
import logging
from typing import Any

from apps.decision_governance_engine.schemas.agent_schemas import (
    AuditEvidenceItem,
    CausalGear,
    ConstraintBoundary,
    DaoInput,
    DaoOutput,
    DeathTrap,
    DeathTrapSeverity,
    EssenceDerivation,
    ExistingAlternative,
    LeverageLevel,
    ProblemNatureType,
    ProblemType,
    QuantifiedMetric,
    SelfCheckResult,
    SelfCheckStatus,
    SolutionRoute,
)

from agentflow import ResilientAgent
from agentflow.core.exceptions import AgentOutputValidationError


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
    # timeout_seconds, max_retries, max_tokens は ResilientAgent のデフォルト値を使用
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

    SYSTEM_PROMPT = """あなたはDaoAgent（道）v3.1です。
問題の本質を見抜き、構造化された分析を提供します。

【最重要原則】
「本質」とは「問題カテゴリ」ではありません。
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
- なぜ既存サービスを使わないのか？
- 使えない理由は何か？（法規制？コスト？技術的制約？）

【v3.1 改修方針（厳守）】

■ 方針1: 制約は「境界条件」で定義する
制約を単語で終わらせない。各制約に以下をセットで定義する:
- 判定条件: 何を超えたら違反か
- 違反例: 具体的な違反パターン
- 例外: 許容される逸脱条件

■ 方針2: 結論前に成立ルートを最低3案提示して比較する
「既存SaaSと衝突」等に早期収束しない。結論前に以下を行う:
- 置換ルート（既存を完全に置き換える）
- 旁路ルート（プラグイン/プロキシ/ローカル代理）
- アプライアンスルート（専用機器/隔離環境）
- その他の成立形
各ルートの実現可能性とトレードオフを明示すること。

■ 方針3: 要請ではなく指標（数値or段階）＋優先順位に落とす
「低遅延」「高品質」等の曖昧な要請を禁止する。以下を明示する:
- E2E遅延目標（例: 200ms以下）
- 品質定義（用途別）
- 優先順位（1=最優先）
三角トレードオフが未解決ならdeath_trapsにFATALとして登録する。

■ 方針4: 監査可能性は証拠チェックリストとして出す
「監査可能にする必要」で終わらせない。具体的な証拠項目を列挙する:
- データフロー図、ログ保持方針、削除証跡、権限設計
- モデル更新手順、テレメトリ等

■ 方針5: 最後にセルフチェックを必須実行する
分析完了後、以下の観点で自己検証する:
- 境界未定義の制約はないか
- 漏れた選択肢はないか
- 曖昧な指標はないか
- 制約衝突はないか
- 証拠不足はないか
1つでもFATALな不足があればoverall_statusをFATALとする。

【問題の本質的性質】
- TECHNICAL_LIMITATION / INVESTMENT_DECISION / CONSTRAINT_DRIVEN
- STRATEGIC_CHOICE / REGULATORY_COMPLIANCE / MARKET_TIMING

【禁止事項】
- 解決策を提示してはいけません
- 行動を推奨してはいけません
- テンプレート的な本質（「XXの決定」等）は禁止

【出力形式】必ず以下のJSON形式で出力:
{
    "problem_type": "TRADE_OFF|RESOURCE_ALLOCATION|TIMING_DECISION|RISK_ASSESSMENT|STRATEGY_DIRECTION",
    "problem_nature": "TECHNICAL_LIMITATION|INVESTMENT_DECISION|CONSTRAINT_DRIVEN|STRATEGIC_CHOICE|REGULATORY_COMPLIANCE|MARKET_TIMING",
    "essence_derivation": {
        "surface_problem": "表面的な問題",
        "underlying_why": "一段深い理由",
        "root_constraint": "根本的な制約",
        "essence_statement": "本質の一文"
    },
    "essence": "問題の本質を一文で（50字以内）",
    "existing_alternatives": [{"name": "既存解", "why_not_viable": "使えない理由", "specific_constraint": "制約"}],
    "immutable_constraints": ["制約1", "制約2"],
    "constraint_boundaries": [
        {"constraint_name": "制約名", "definition": "判定条件", "violation_example": "違反例", "exceptions": "例外"}
    ],
    "solution_routes": [
        {"route_type": "置換|旁路|アプライアンス|...", "description": "説明", "viability": "実現可能性", "tradeoffs": ["トレードオフ"]}
    ],
    "quantified_metrics": [
        {"metric_name": "指標名", "target_value": "目標値", "priority": 1, "tradeoff_note": "注記"}
    ],
    "audit_evidence_checklist": [
        {"category": "カテゴリ", "required_evidence": "必要証拠", "verification_method": "確認方法"}
    ],
    "hidden_assumptions": ["前提1", "前提2"],
    "causal_gears": [{"gear_id": 1, "name": "名", "description": "説明", "drives": [2], "driven_by": [], "leverage": "HIGH"}],
    "bottleneck_gear": 1,
    "death_traps": [{"action": "禁止行動", "reason": "理由", "severity": "FATAL"}],
    "self_check": {
        "boundary_undefined": ["未定義の制約"],
        "missing_alternatives": ["漏れた選択肢"],
        "ambiguous_metrics": ["曖昧な指標"],
        "constraint_conflicts": ["制約衝突"],
        "evidence_gaps": ["証拠不足"],
        "overall_status": "PASS|WARNING|FATAL"
    }
}

【重要な制約】
- constraint_boundaries: immutable_constraintsの各制約に対応させること
- solution_routes: 最低3個（3種以上のルートを比較）
- quantified_metrics: 曖昧な要請は必ず数値/段階化する
- audit_evidence_checklist: 監査関連の制約がある場合は必須
- self_check: 必ず出力すること。未解決トレードオフがあればFATAL
- hidden_assumptions: 最大3個 / death_traps: 最大3個
- existing_alternatives: 最大3個 / causal_gears: 3〜5個"""

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

【v3.1必須事項】
1. 「essence」はカテゴリ（「XXの決定」）ではなく、問題の存在理由を一文で
2. 「existing_alternatives」で既存解が使えない理由を明確に
3. 「constraint_boundaries」で各制約の判定条件・違反例・例外を定義
4. 「solution_routes」で最低3つの成立ルートを比較（置換/旁路/アプライアンス等）
5. 「quantified_metrics」で曖昧な要請を数値/段階＋優先順位に変換
6. 「audit_evidence_checklist」で監査証拠を具体的に列挙
7. 「self_check」で分析の自己検証を必ず実行"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{user_prompt}")

        # 詳細ログ: LLM生出力を記録（デバッグ用）
        self._logger.debug(f"LLM raw response (first 500 chars): {response[:500] if response else 'EMPTY'}")

        try:
            # JSON部分を抽出してパース（堅牢な抽出）
            from agentflow.utils import extract_json
            data = extract_json(response)

            if data is None:
                self._logger.error(f"JSON extraction failed. Raw response: {response[:1000]}")
                msg = "No valid JSON found"
                raise json.JSONDecodeError(msg, response, 0)

            # 詳細ログ: 抽出されたJSON
            self._logger.debug(f"Extracted JSON: {data}")

            # 業務ロジック検証（Pydantic検証前）- 失敗時はリトライをトリガー
            self._validate_llm_output_fields(data)

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

            # v3.1: 制約境界条件をパース
            constraint_boundaries = []
            for cb_data in data.get("constraint_boundaries", [])[:5]:
                constraint_boundaries.append(ConstraintBoundary(
                    constraint_name=cb_data.get("constraint_name", "")[:30],
                    definition=cb_data.get("definition", "")[:100],
                    violation_example=cb_data.get("violation_example", "")[:100],
                    exceptions=cb_data.get("exceptions", "")[:100],
                ))

            # v3.1: 成立ルートをパース
            solution_routes = []
            for sr_data in data.get("solution_routes", [])[:5]:
                solution_routes.append(SolutionRoute(
                    route_type=sr_data.get("route_type", "")[:20],
                    description=sr_data.get("description", "")[:100],
                    viability=sr_data.get("viability", "")[:50],
                    tradeoffs=sr_data.get("tradeoffs", [])[:3],
                ))

            # v3.1: 定量指標をパース
            quantified_metrics = []
            for qm_data in data.get("quantified_metrics", [])[:5]:
                quantified_metrics.append(QuantifiedMetric(
                    metric_name=qm_data.get("metric_name", "")[:30],
                    target_value=qm_data.get("target_value", "")[:50],
                    priority=min(max(qm_data.get("priority", 5), 1), 10),
                    tradeoff_note=qm_data.get("tradeoff_note", "")[:100],
                ))

            # v3.1: 監査証拠チェックリストをパース
            audit_evidence = []
            for ae_data in data.get("audit_evidence_checklist", [])[:8]:
                audit_evidence.append(AuditEvidenceItem(
                    category=ae_data.get("category", "")[:30],
                    required_evidence=ae_data.get("required_evidence", "")[:100],
                    verification_method=ae_data.get("verification_method", "")[:100],
                ))

            # v3.1: セルフチェック結果をパース
            self_check = None
            sc_data = data.get("self_check")
            if isinstance(sc_data, dict):
                try:
                    overall = SelfCheckStatus(sc_data.get("overall_status", "WARNING"))
                except ValueError:
                    overall = SelfCheckStatus.WARNING
                self_check = SelfCheckResult(
                    boundary_undefined=sc_data.get("boundary_undefined", []),
                    missing_alternatives=sc_data.get("missing_alternatives", []),
                    ambiguous_metrics=sc_data.get("ambiguous_metrics", []),
                    constraint_conflicts=sc_data.get("constraint_conflicts", []),
                    evidence_gaps=sc_data.get("evidence_gaps", []),
                    overall_status=overall,
                )

            # 本質の検証（テンプレート的な回答を拒否）
            essence = data.get("essence", "")[:50]
            if self._is_template_essence(essence):
                self._logger.warning(f"Template essence detected: {essence}")
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
                constraint_boundaries=constraint_boundaries,
                solution_routes=solution_routes,
                quantified_metrics=quantified_metrics,
                audit_evidence_checklist=audit_evidence,
                self_check=self_check,
            )
        except ValueError as e:
            self._logger.warning(f"LLM response parse failed: {e}")
            # パース失敗時はルールベースにフォールバック
            return self._analyze_rule_based(question, constraints, inferred_type)

    def _validate_llm_output_fields(self, data: dict[str, Any]) -> None:
        """LLM出力の業務ロジック検証（Pydantic検証前）.

        重要なフィールドが空の場合はAgentOutputValidationErrorを発生させ、
        リトライをトリガーする。

        Args:
            data: 抽出されたJSONデータ

        Raises:
            AgentOutputValidationError: 必須フィールドが空の場合
        """
        # essence の検証（必須かつ非空）
        essence = data.get("essence")
        if not essence or not isinstance(essence, str) or not essence.strip():
            self._logger.warning(f"LLM returned empty essence. Full data: {data}")
            raise AgentOutputValidationError(
                agent_name=self.name,
                field_name="essence",
                expected="non-empty string (max 50 chars)",
                actual=f"empty or invalid: {essence}",
            )

        # immutable_constraints の検証（リストであること）
        immutable = data.get("immutable_constraints")
        if immutable is not None and not isinstance(immutable, list):
            self._logger.warning(f"LLM returned invalid immutable_constraints: {immutable}")
            data["immutable_constraints"] = []

        # causal_gears のリスト長を検証・正規化（max 5）
        causal_gears = data.get("causal_gears", [])
        if isinstance(causal_gears, list) and len(causal_gears) > 5:
            self._logger.warning(f"causal_gears has {len(causal_gears)} items (max 5), truncating")
            data["causal_gears"] = causal_gears[:5]

        # death_traps のリスト長を検証・正規化（max 3）
        death_traps = data.get("death_traps", [])
        if isinstance(death_traps, list) and len(death_traps) > 3:
            self._logger.warning(f"death_traps has {len(death_traps)} items (max 3), truncating")
            data["death_traps"] = death_traps[:3]

        # existing_alternatives のリスト長を検証・正規化（max 3）
        existing_alts = data.get("existing_alternatives", [])
        if isinstance(existing_alts, list) and len(existing_alts) > 3:
            self._logger.warning(f"existing_alternatives has {len(existing_alts)} items (max 3), truncating")
            data["existing_alternatives"] = existing_alts[:3]

        # hidden_assumptions のリスト長を検証・正規化（max 3）
        hidden = data.get("hidden_assumptions", [])
        if isinstance(hidden, list) and len(hidden) > 3:
            self._logger.warning(f"hidden_assumptions has {len(hidden)} items (max 3), truncating")
            data["hidden_assumptions"] = hidden[:3]

        # v3.1: constraint_boundaries のリスト長を検証・正規化（max 5）
        cb = data.get("constraint_boundaries", [])
        if isinstance(cb, list) and len(cb) > 5:
            self._logger.warning(f"constraint_boundaries has {len(cb)} items (max 5), truncating")
            data["constraint_boundaries"] = cb[:5]

        # v3.1: solution_routes の検証（最低3個推奨）
        sr = data.get("solution_routes", [])
        if isinstance(sr, list) and len(sr) < 3:
            self._logger.warning(f"solution_routes has {len(sr)} items (min 3 recommended)")

        # v3.1: quantified_metrics のリスト長を検証・正規化（max 5）
        qm = data.get("quantified_metrics", [])
        if isinstance(qm, list) and len(qm) > 5:
            self._logger.warning(f"quantified_metrics has {len(qm)} items (max 5), truncating")
            data["quantified_metrics"] = qm[:5]

        # v3.1: audit_evidence_checklist のリスト長を検証・正規化（max 8）
        ae = data.get("audit_evidence_checklist", [])
        if isinstance(ae, list) and len(ae) > 8:
            self._logger.warning(f"audit_evidence_checklist has {len(ae)} items (max 8), truncating")
            data["audit_evidence_checklist"] = ae[:8]

        # v3.1: self_check の検証（必須）
        sc = data.get("self_check")
        if not sc or not isinstance(sc, dict):
            self._logger.warning("self_check is missing or invalid")

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
        """ルールベース分析（LLMなし）v3.1対応."""
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

        # v3.1: 制約境界条件のデフォルト生成
        constraint_boundaries = self._generate_default_constraint_boundaries(immutable)

        # v3.1: 成立ルートのデフォルト生成
        solution_routes = self._generate_default_solution_routes(problem_type)

        # v3.1: 定量指標のデフォルト生成
        quantified_metrics = self._generate_default_quantified_metrics(problem_type)

        # v3.1: 監査証拠チェックリストのデフォルト生成
        audit_evidence = self._generate_default_audit_evidence(constraints)

        # v3.1: セルフチェック結果のデフォルト生成（ルールベースなので常にWARNING）
        self_check = SelfCheckResult(
            boundary_undefined=["ルールベース分析のため境界条件は概算"],
            missing_alternatives=["LLM分析でより詳細な選択肢を検討可能"],
            ambiguous_metrics=["定量指標はLLM分析で精緻化が必要"],
            constraint_conflicts=[],
            evidence_gaps=["ルールベース分析のため証拠の網羅性は限定的"],
            overall_status=SelfCheckStatus.WARNING,
        )

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
            constraint_boundaries=constraint_boundaries,
            solution_routes=solution_routes,
            quantified_metrics=quantified_metrics,
            audit_evidence_checklist=audit_evidence,
            self_check=self_check,
        )

    def _generate_default_constraint_boundaries(
        self, immutable_constraints: list[str],
    ) -> list[ConstraintBoundary]:
        """制約境界条件のデフォルト生成（v3.1ルールベース用）."""
        boundaries = []
        for c in immutable_constraints[:5]:
            boundaries.append(ConstraintBoundary(
                constraint_name=c[:30],
                definition=f"「{c[:20]}」に違反する行為全般"[:100],
                violation_example=f"「{c[:20]}」を無視した設計・運用"[:100],
                exceptions="経営層の明示的承認がある場合"[:100],
            ))
        return boundaries

    def _generate_default_solution_routes(
        self, problem_type: ProblemType,
    ) -> list[SolutionRoute]:
        """成立ルートのデフォルト生成（v3.1ルールベース用）."""
        return [
            SolutionRoute(
                route_type="置換",
                description="既存ソリューションを完全に置き換える",
                viability="要詳細検討",
                tradeoffs=["導入コスト大", "移行リスク"],
            ),
            SolutionRoute(
                route_type="旁路",
                description="プラグイン/プロキシで既存に追加機能",
                viability="中程度",
                tradeoffs=["機能制限の可能性", "依存関係増加"],
            ),
            SolutionRoute(
                route_type="アプライアンス",
                description="専用機器/隔離環境で要件を分離充足",
                viability="要コスト検証",
                tradeoffs=["運用負荷増", "統合の複雑さ"],
            ),
        ]

    def _generate_default_quantified_metrics(
        self, problem_type: ProblemType,
    ) -> list[QuantifiedMetric]:
        """定量指標のデフォルト生成（v3.1ルールベース用）."""
        return [
            QuantifiedMetric(
                metric_name="導入コスト",
                target_value="要算出",
                priority=1,
                tradeoff_note="品質・スピードとのバランスが必要",
            ),
            QuantifiedMetric(
                metric_name="導入期間",
                target_value="要算出",
                priority=2,
                tradeoff_note="コストと品質に影響",
            ),
        ]

    def _generate_default_audit_evidence(
        self, constraints: list[str],
    ) -> list[AuditEvidenceItem]:
        """監査証拠チェックリストのデフォルト生成（v3.1ルールベース用）."""
        evidence = [
            AuditEvidenceItem(
                category="データフロー",
                required_evidence="データフロー図（入力→処理→出力→保存）",
                verification_method="設計レビューで図面を確認",
            ),
            AuditEvidenceItem(
                category="ログ保持",
                required_evidence="ログ保持方針（期間・形式・アクセス権）",
                verification_method="運用手順書に記載を確認",
            ),
        ]
        # 制約にセキュリティ関連があれば追加
        security_keywords = ["セキュリティ", "機密", "暗号", "認証", "権限"]
        if any(kw in " ".join(constraints) for kw in security_keywords):
            evidence.append(AuditEvidenceItem(
                category="権限設計",
                required_evidence="アクセス制御マトリクス",
                verification_method="権限設定の実機確認",
            ))
        return evidence

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

        # v3.1: 成立ルートが3個以上あるか
        if len(output.solution_routes) < 3:
            self._logger.warning(
                f"Validation warning: solution_routes has {len(output.solution_routes)} items (min 3)"
            )
            # 警告のみ、通過させる

        # v3.1: セルフチェックが存在するか
        if not output.self_check:
            self._logger.warning("Validation warning: self_check is missing")
            # 警告のみ、通過させる
        elif output.self_check.overall_status == SelfCheckStatus.FATAL:
            self._logger.warning("Validation warning: self_check overall_status is FATAL")
            # 警告のみ、通過させる（FATALでもレポートは出力する）

        # v3.1: 制約境界条件の存在チェック
        if not output.constraint_boundaries:
            self._logger.warning("Validation warning: constraint_boundaries is empty")

        return True
