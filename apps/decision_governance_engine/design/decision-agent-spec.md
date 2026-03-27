# 企業級決策Agent平台 完整技術仕様書

## 1. プロジェクト概要

### 1.1 コアミッション（不可変）

```
複雑な問題 → 正確な意思決定 → 実行可能なプラン
このプロセスをシステムとして固定化した【企業級決策Agent平台】
```

### 1.2 核心キーワード定義

| キーワード | 意味                         | アンチパターン        |
| ---------- | ---------------------------- | --------------------- |
| 決策       | 構造化された意思決定出力     | ❌ 雑談・自由回答     |
| 企業級     | 責任者向け、署名可能な結論   | ❌ 個人向けアドバイス |
| システム   | 再現可能・追跡可能なプロセス | ❌ 人依存・属人化     |

### 1.3 絶対にやらないこと（ネガティブスコープ）

- ❌ チャットボット的な自由会話
- ❌ 複数人への同時意思決定支援
- ❌ 全自動意思決定（人間の承認なし）
- ❌ コンサルティングサービスの販売
- ❌ 哲学的議論・抽象的アドバイス

---

## 2. システムアーキテクチャ

### 2.1 全体構成図

```
┌─────────────────────────────────────────────────────────────┐
│                        Frontend (Next.js)                    │
│  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────────────┐ │
│  │ 入力画面 │→│ 進捗表示 │→│ 結果表示 │→│ レポートExport  │ │
│  └─────────┘  └─────────┘  └─────────┘  └─────────────────┘ │
└────────────────────────┬────────────────────────────────────┘
                         │ REST API / WebSocket
┌────────────────────────▼────────────────────────────────────┐
│                     Backend (FastAPI)                        │
│  ┌──────────────────────────────────────────────────────┐   │
│  │                 Decision Flow Engine                  │   │
│  │  ┌────────┐  ┌────────┐  ┌────────┐  ┌────────┐     │   │
│  │  │DaoAgent│→│FaAgent │→│ShuAgent│→│QiAgent │      │   │
│  │  └────┬───┘  └────────┘  └────────┘  └────┬───┘     │   │
│  │       │                                    │          │   │
│  │       └──────────┐            ┌───────────┘          │   │
│  │                  ▼            ▼                       │   │
│  │              ┌────────────────────┐                  │   │
│  │              │   ReviewAgent      │ ← 全層検証       │   │
│  │              └────────────────────┘                  │   │
│  └──────────────────────────────────────────────────────┘   │
│                              │                               │
│  ┌───────────────┐  ┌───────▼───────┐  ┌────────────────┐  │
│  │  LLM Gateway  │  │ State Machine │  │ RAG Service    │  │
│  │  (抽象化層)   │  │ (状態管理)    │  │ (Shu/Qi専用)   │  │
│  └───────────────┘  └───────────────┘  └────────────────┘  │
└─────────────────────────────────────────────────────────────┘
                         │
         ┌───────────────┼───────────────┐
         ▼               ▼               ▼
    ┌─────────┐    ┌─────────┐    ┌─────────┐
    │PostgreSQL│    │  Redis  │    │  S3/Minio│
    │(履歴/結果)│    │(セッション)│   │(レポート)│
    └─────────┘    └─────────┘    └─────────┘
```

### 2.2 Agent間データフロー（状態機械）

```
           ┌─────────────────────────────────────────────┐
           │              State Machine Flow             │
           │                                             │
           │  INPUT → [PENDING] → [DAO] → [FA] → [SHU]  │
           │                         │       │      │    │
           │                         ▼       ▼      ▼    │
           │  REVIEW ←──────────────────────────────────│
           │    │                                        │
           │    ├─ PASS → [QI] → [FINAL] → OUTPUT      │
           │    │                                        │
           │    └─ FAIL → [REVISION] → 該当Agent再実行 │
           │                                             │
           └─────────────────────────────────────────────┘

状態遷移ルール（厳格）:
- 順方向のみ許可（DaoからFaを飛ばしてShuは不可）
- ReviewAgentのみ逆方向遷移を発行可能
- 各状態での最大滞在時間: 30秒
- 最大リトライ回数: 各Agent 2回まで
```

---

## 3. Agent詳細設計

### 3.1 共通インターフェース規約

```python
from abc import ABC, abstractmethod
from pydantic import BaseModel
from typing import Generic, TypeVar

InputT = TypeVar('InputT', bound=BaseModel)
OutputT = TypeVar('OutputT', bound=BaseModel)

class BaseAgent(ABC, Generic[InputT, OutputT]):
    """全Agentの基底クラス"""

    # 必須メタデータ
    name: str                    # Agent識別名
    max_tokens: int              # 出力トークン上限
    temperature: float           # LLM温度パラメータ
    timeout_seconds: int = 30    # タイムアウト

    @abstractmethod
    def run(self, input: InputT, context: DecisionContext) -> OutputT:
        """
        実行メソッド
        - 入力は明示的に型定義
        - 出力は必ず構造化
        - 暗黙の状態共有禁止
        """
        pass

    @abstractmethod
    def validate_output(self, output: OutputT) -> bool:
        """出力の自己検証"""
        pass
```

### 3.2 各Agent仕様

#### 【GatekeeperAgent】入口検証Agent（最優先実行）

```python
"""
=================================================================
入口検証Agent - 不適格な問題を門前払い
=================================================================
このAgentはDaoAgentの前に必ず実行され、問題の適格性を判断する。
不適格な問題は即座に拒否し、後続Agentには渡さない。
"""

class QuestionCategory(str, Enum):
    """問題カテゴリ分類"""

    # ✅ 受理可能（決策系）
    STRATEGIC_DECISION = "strategic_decision"      # 戦略的意思決定
    RESOURCE_ALLOCATION = "resource_allocation"    # リソース配分
    TRADE_OFF_CHOICE = "trade_off_choice"          # トレードオフ選択
    TIMING_JUDGMENT = "timing_judgment"            # タイミング判断
    RISK_EVALUATION = "risk_evaluation"            # リスク評価
    PRIORITY_SETTING = "priority_setting"          # 優先順位設定
    GO_NOGO_DECISION = "go_nogo_decision"          # Go/No-Go判断

    # ❌ 拒否（非決策系）
    GENERAL_KNOWLEDGE = "general_knowledge"        # 一般知識質問
    TECHNICAL_HOWTO = "technical_howto"            # 技術的How-to
    SYSTEM_INQUIRY = "system_inquiry"              # システム自体への質問
    CASUAL_CHAT = "casual_chat"                    # 雑談
    FACTUAL_LOOKUP = "factual_lookup"              # 事実確認
    OPINION_REQUEST = "opinion_request"            # 意見・感想求め
    CREATIVE_REQUEST = "creative_request"          # 創作依頼

class GatekeeperInput(BaseModel):
    raw_question: str

class GatekeeperOutput(BaseModel):
    is_acceptable: bool                    # 受理可否
    category: QuestionCategory             # 分類結果
    confidence: float                      # 判定確信度 (0.0-1.0)
    rejection_reason: str | None           # 拒否理由（拒否時のみ）
    rejection_message: str | None          # ユーザー向けメッセージ
    suggested_rephrase: str | None         # 言い換え提案（境界ケース）

class GatekeeperAgent(BaseAgent[GatekeeperInput, GatekeeperOutput]):
    name = "GatekeeperAgent"
    max_tokens = 300
    temperature = 0.1  # 極めて低い＝一貫した判定

    # ========================================
    # 受理条件（これらを全て満たす必要）
    # ========================================
    ACCEPTANCE_CRITERIA = [
        "意思決定・判断を求めている",
        "複数の選択肢や方向性が存在する",
        "ビジネス・組織・プロジェクトに関連する",
        "回答者（決策者）が行動を起こせる",
        "正解が一意に定まらない（判断が必要）",
    ]

    # ========================================
    # 即座に拒否するパターン（正規表現）
    # ========================================
    INSTANT_REJECT_PATTERNS = [
        # 天気・時刻
        r"(天気|気温|weather|何時|today)",
        # システム自体への質問
        r"(このシステム|このAI|どうやって作|仕組み|how.*work|how.*built)",
        # 一般知識
        r"(とは何|what is|意味|定義|explain what)",
        # 計算・変換
        r"(計算して|convert|換算|translate)",
        # 雑談
        r"(こんにちは|hello|hi|元気|調子|ありがとう|thank)",
        # コード生成
        r"(コード.*書いて|write.*code|プログラム.*作成)",
        # 創作
        r"(物語|story|poem|詩|小説|作文)",
        # 事実確認
        r"(誰が|いつ|どこで|who is|when did|where is).*\?$",
    ]

    # ========================================
    # 拒否時の定型メッセージ
    # ========================================
    REJECTION_MESSAGES = {
        QuestionCategory.GENERAL_KNOWLEDGE: {
            "reason": "一般的な知識・情報の質問です",
            "message": "このシステムは企業の意思決定支援に特化しています。一般的な質問にはお答えできません。",
            "suggest": "意思決定が必要な形に言い換えてください。例：「○○を導入すべきか判断したい」"
        },
        QuestionCategory.TECHNICAL_HOWTO: {
            "reason": "技術的なHow-to質問です",
            "message": "技術的な実装方法の質問にはお答えできません。",
            "suggest": "技術選定の意思決定として言い換えてください。例：「AとBのどちらの技術を採用すべきか」"
        },
        QuestionCategory.SYSTEM_INQUIRY: {
            "reason": "システム自体に関する質問です",
            "message": "このシステムの仕組みや実装についてはお答えできません。",
            "suggest": None
        },
        QuestionCategory.CASUAL_CHAT: {
            "reason": "雑談・挨拶です",
            "message": "このシステムは雑談には対応していません。意思決定が必要な問題を入力してください。",
            "suggest": None
        },
        QuestionCategory.FACTUAL_LOOKUP: {
            "reason": "事実確認の質問です",
            "message": "事実や情報の検索にはお答えできません。",
            "suggest": "その事実を踏まえた意思決定として言い換えてください。"
        },
        QuestionCategory.OPINION_REQUEST: {
            "reason": "意見・感想を求める質問です",
            "message": "個人的な意見や感想の提供は行っていません。",
            "suggest": "客観的な判断基準に基づく意思決定として言い換えてください。"
        },
        QuestionCategory.CREATIVE_REQUEST: {
            "reason": "創作・コンテンツ生成の依頼です",
            "message": "文章や創作物の生成には対応していません。",
            "suggest": None
        },
    }

    # ========================================
    # 境界ケースの判定ガイドライン
    # ========================================
    BOUNDARY_GUIDELINES = """
    【境界ケースの判定基準】

    ◆ 受理する：
    - 「AとBどちらを選ぶべきか」→ トレードオフ選択
    - 「いつ○○すべきか」→ タイミング判断
    - 「○○に投資すべきか」→ リソース配分
    - 「○○を続けるべきか中止すべきか」→ Go/No-Go

    ◆ 拒否する：
    - 「○○とは何ですか」→ 定義質問
    - 「○○の方法を教えて」→ How-to
    - 「○○についてどう思いますか」→ 意見要求
    - 「○○を作って」→ 生成依頼

    ◆ 確認が必要（confidence < 0.7）：
    - 「○○について」で終わる曖昧な質問
    - 主語や目的が不明確な質問
    → suggested_rephraseを提供
    """
```

#### 【DaoAgent】本質判定Agent

```python
class DaoInput(BaseModel):
    question: str                           # 原始質問（Gatekeeper通過済み）
    constraints: list[str]                  # 現実制約
    stakeholders: list[str]                 # 関係者
    gatekeeper_result: GatekeeperOutput     # 入口検証結果（参照用）

class DaoOutput(BaseModel):
    problem_type: Literal[
        "RESOURCE_ALLOCATION",    # リソース配分問題
        "TIMING_DECISION",        # タイミング判断
        "TRADE_OFF",              # トレードオフ選択
        "RISK_ASSESSMENT",        # リスク評価
        "STRATEGY_DIRECTION"      # 戦略方向性
    ]
    essence: str                  # 一文での本質（50字以内）
    immutable_constraints: list[str]  # 不可変制約（max 5）
    hidden_assumptions: list[str]     # 隠れた前提（max 3）

class DaoAgent(BaseAgent[DaoInput, DaoOutput]):
    name = "DaoAgent"
    max_tokens = 500
    temperature = 0.3  # 低め＝安定判断

    # 禁止事項
    FORBIDDEN = [
        "解決策の提示",
        "行動の推奨",
        "楽観的予測",
    ]
```

#### 【FaAgent】戦略選定Agent

```python
class FaInput(BaseModel):
    dao_result: DaoOutput
    available_resources: dict     # 利用可能リソース
    time_horizon: str             # 時間軸

class PathOption(BaseModel):
    path_id: str
    name: str                     # パス名（10字以内）
    description: str              # 説明（100字以内）
    pros: list[str]               # メリット（max 3）
    cons: list[str]               # デメリット（max 3）
    success_probability: float    # 成功確率（0.0-1.0）

class FaOutput(BaseModel):
    recommended_paths: list[PathOption]  # 1-2個のみ
    rejected_paths: list[PathOption]     # 明示的に不推奨
    decision_criteria: list[str]         # 判断基準

class FaAgent(BaseAgent[FaInput, FaOutput]):
    name = "FaAgent"
    max_tokens = 800
    temperature = 0.4

    # 制約
    MAX_RECOMMENDED_PATHS = 2
    MUST_INCLUDE_REJECTED = True  # 不推奨は必須
```

#### 【ShuAgent】実行計画Agent

```python
class ShuInput(BaseModel):
    fa_result: FaOutput
    selected_path_id: str         # 選択されたパス

class ActionPhase(BaseModel):
    phase_number: int
    name: str
    duration: str                 # "2週間", "1ヶ月" など
    actions: list[str]            # 具体的行動（max 5）
    deliverables: list[str]       # 成果物
    success_criteria: list[str]   # 完了条件

class ShuOutput(BaseModel):
    phases: list[ActionPhase]     # 3-5フェーズ
    first_action: str             # 「最初の一歩」（明日できること）
    dependencies: list[str]       # 前提条件

class ShuAgent(BaseAgent[ShuInput, ShuOutput]):
    name = "ShuAgent"
    max_tokens = 1000
    temperature = 0.5

    # RAG使用許可
    USE_RAG = True
    RAG_SOURCES = ["industry_practices", "case_studies"]
```

#### 【QiAgent】技術実装Agent

```python
class QiInput(BaseModel):
    shu_result: ShuOutput
    tech_constraints: list[str]   # 技術制約

class Implementation(BaseModel):
    component: str
    technology: str
    estimated_effort: str
    risks: list[str]

class QiOutput(BaseModel):
    implementations: list[Implementation]
    tool_recommendations: list[str]
    integration_points: list[str]
    technical_debt_warnings: list[str]

class QiAgent(BaseAgent[QiInput, QiOutput]):
    name = "QiAgent"
    max_tokens = 1200
    temperature = 0.6

    # 禁止
    FORBIDDEN = [
        "抽象的提案",
        "升维思考",      # 問題を大きくしない
        "スコープ拡大",
    ]

    # RAG使用許可
    USE_RAG = True
    RAG_SOURCES = ["technical_docs", "compliance"]
```

#### 【ReviewAgent】検証Agent（最重要）

```python
class ReviewInput(BaseModel):
    dao_result: DaoOutput
    fa_result: FaOutput
    shu_result: ShuOutput
    qi_result: QiOutput

class ReviewFinding(BaseModel):
    severity: Literal["CRITICAL", "WARNING", "INFO"]
    category: Literal[
        "LOGIC_FLAW",           # 論理的欠陥
        "OVER_OPTIMISM",        # 過度な楽観
        "RESPONSIBILITY_GAP",   # 責任の空白
        "RESOURCE_MISMATCH",    # リソース不整合
        "TIMELINE_UNREALISTIC"  # 非現実的スケジュール
    ]
    description: str
    affected_agent: str
    suggested_revision: str

class ReviewOutput(BaseModel):
    overall_verdict: Literal["PASS", "REVISE", "REJECT"]
    findings: list[ReviewFinding]
    confidence_score: float       # 0.0-1.0
    final_warnings: list[str]     # 最終警告

class ReviewAgent(BaseAgent[ReviewInput, ReviewOutput]):
    name = "ReviewAgent"
    max_tokens = 1500
    temperature = 0.7  # やや高め＝多角的視点

    # 必須チェック項目
    MANDATORY_CHECKS = [
        "責任者が明確か",
        "最悪ケースの想定があるか",
        "撤退条件が定義されているか",
        "最初の一歩が明日実行可能か",
    ]
```

---

## 4. データモデル（Schema）

### 4.1 入力Schema

```python
class DecisionRequest(BaseModel):
    """システム入力の最上位Schema"""

    # 必須
    question: str = Field(
        ...,
        min_length=10,
        max_length=2000,
        description="解決したい問題・意思決定事項"
    )

    # 制約情報
    constraints: ConstraintSet

    # メタ情報
    requester: RequesterInfo

    # オプション
    additional_context: str | None = None
    attachments: list[str] | None = None  # S3 URLs

class ConstraintSet(BaseModel):
    budget: BudgetConstraint | None
    timeline: TimelineConstraint | None
    human_resources: list[str] = []
    technical: list[str] = []
    regulatory: list[str] = []

class RequesterInfo(BaseModel):
    role: Literal["FOUNDER", "CEO", "EXECUTIVE", "MANAGER"]
    decision_authority: bool  # 最終決定権があるか
    organization_size: Literal["STARTUP", "SMB", "ENTERPRISE"]
```

### 4.2 出力Schema

```python
class DecisionReport(BaseModel):
    """最終出力レポート"""

    # メタ情報
    report_id: str
    created_at: datetime
    version: str = "1.0"

    # 各層の結果
    dao: DaoOutput
    fa: FaOutput
    shu: ShuOutput
    qi: QiOutput
    review: ReviewOutput

    # エグゼクティブサマリー（自動生成）
    executive_summary: ExecutiveSummary

    # 署名欄
    signature_required: bool = True

class ExecutiveSummary(BaseModel):
    one_line_decision: str        # 一文結論（30字以内）
    recommended_action: str       # 推奨アクション
    key_risks: list[str]          # 主要リスク（max 3）
    first_step: str               # 最初の一歩
    estimated_impact: str         # 期待効果
```

---

## 5. フロントエンド設計

### 5.1 デザインコンセプト

```
テーマ: "Executive Decision Console"
- ダークモード基調（集中力向上）
- ミニマリスト（情報過多を避ける）
- プログレス可視化（安心感）
- プロフェッショナル感（信頼性）
```

### 5.2 カラーパレット

```css
:root {
  /* Primary */
  --bg-primary: #0a0a0f; /* 深い黒 */
  --bg-secondary: #12121a; /* カード背景 */
  --bg-elevated: #1a1a24; /* 浮き上がり */

  /* Accent */
  --accent-primary: #6366f1; /* インディゴ */
  --accent-secondary: #818cf8;
  --accent-glow: rgba(99, 102, 241, 0.2);

  /* Semantic */
  --success: #10b981; /* エメラルド */
  --warning: #f59e0b; /* アンバー */
  --danger: #ef4444; /* レッド */
  --info: #3b82f6; /* ブルー */

  /* Text */
  --text-primary: #f8fafc;
  --text-secondary: #94a3b8;
  --text-muted: #475569;

  /* Border */
  --border-subtle: rgba(255, 255, 255, 0.06);
  --border-accent: rgba(99, 102, 241, 0.3);
}
```

### 5.3 画面構成

#### 画面1: 入力画面（Decision Input）

```
┌─────────────────────────────────────────────────────────────────┐
│  [Logo]  Decision Agent Platform              [User] [Settings] │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   ┌─────────────────────────────────────────────────────────┐  │
│   │                                                         │  │
│   │   あなたの意思決定課題を入力してください                 │  │
│   │                                                         │  │
│   │   ┌─────────────────────────────────────────────────┐   │  │
│   │   │                                                 │   │  │
│   │   │  [テキストエリア - 問題・課題の記述]            │   │  │
│   │   │                                                 │   │  │
│   │   │  placeholder: 例）新規事業の方向性について、     │   │  │
│   │   │  A案とB案のどちらを選ぶべきか決断できない...     │   │  │
│   │   │                                                 │   │  │
│   │   └─────────────────────────────────────────────────┘   │  │
│   │                                                         │  │
│   │   制約条件（任意）                                       │  │
│   │   ┌──────────┐ ┌──────────┐ ┌──────────┐             │  │
│   │   │💰 予算   │ │⏱️ 期限   │ │👥 人員   │             │  │
│   │   │ ¥___M   │ │ ___ヶ月  │ │ ___名    │             │  │
│   │   └──────────┘ └──────────┘ └──────────┘             │  │
│   │                                                         │  │
│   │   ┌──────────┐ ┌──────────┐                           │  │
│   │   │🔧 技術   │ │📋 規制   │  [+ 制約を追加]          │  │
│   │   │ タグ入力 │ │ タグ入力 │                           │  │
│   │   └──────────┘ └──────────┘                           │  │
│   │                                                         │  │
│   │           ┌─────────────────────────────┐              │  │
│   │           │   ▶ 決策分析を開始する       │              │  │
│   │           └─────────────────────────────┘              │  │
│   │                                                         │  │
│   └─────────────────────────────────────────────────────────┘  │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### 画面2: 処理中画面（Processing View）

```
┌─────────────────────────────────────────────────────────────────┐
│  [Logo]  Decision Agent Platform              [User] [Settings] │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   分析進行中...                                                 │
│                                                                 │
│   ┌─────────────────────────────────────────────────────────┐  │
│   │                                                         │  │
│   │   道 [████████████████████] ✓ 完了                      │  │
│   │   ├─ 問題タイプ: TRADE_OFF                              │  │
│   │   └─ 本質: 短期収益と長期成長のバランス判断             │  │
│   │                                                         │  │
│   │   法 [██████████░░░░░░░░░░] ⟳ 処理中...                 │  │
│   │   └─ 戦略オプションを評価中                             │  │
│   │                                                         │  │
│   │   術 [░░░░░░░░░░░░░░░░░░░░] ○ 待機中                    │  │
│   │                                                         │  │
│   │   器 [░░░░░░░░░░░░░░░░░░░░] ○ 待機中                    │  │
│   │                                                         │  │
│   │   ─────────────────────────────────────                 │  │
│   │   検証 [░░░░░░░░░░░░░░░░░░░░] ○ 最終検証待ち            │  │
│   │                                                         │  │
│   └─────────────────────────────────────────────────────────┘  │
│                                                                 │
│   💡 ヒント: 各段階で深層分析を行っています。                  │
│      通常2-3分で完了します。                                   │
│                                                                 │
│   [キャンセル]                                                  │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### 画面3: 結果画面（Decision Report）

```
┌─────────────────────────────────────────────────────────────────┐
│  [Logo]  Decision Agent Platform              [User] [Settings] │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌───────────────────────────────────────────────────────────┐ │
│  │ EXECUTIVE SUMMARY                              信頼度: 87% │ │
│  │                                                            │ │
│  │ ┌────────────────────────────────────────────────────────┐│ │
│  │ │ 💡 結論                                                ││ │
│  │ │ B案を選択し、6ヶ月以内にMVP検証を実施すべき           ││ │
│  │ └────────────────────────────────────────────────────────┘│ │
│  │                                                            │ │
│  │ 🎯 最初の一歩（明日実行可能）                             │ │
│  │ → プロダクトチームとの30分MTGを設定する                   │ │
│  │                                                            │ │
│  │ ⚠️ 主要リスク                                             │ │
│  │ • 既存顧客の離反リスク                                    │ │
│  │ • 開発リソースの競合                                      │ │
│  │ • 市場タイミングの不確実性                                │ │
│  └───────────────────────────────────────────────────────────┘ │
│                                                                 │
│  ┌─────────┬─────────┬─────────┬─────────┐                    │
│  │   道    │   法    │   術    │   器    │  ← タブ切替       │
│  └─────────┴─────────┴─────────┴─────────┘                    │
│  ┌───────────────────────────────────────────────────────────┐ │
│  │                                                           │ │
│  │  [選択中タブの詳細内容がここに表示]                       │ │
│  │                                                           │ │
│  │  • 構造化された情報                                       │ │
│  │  • 視覚的なカード/リスト                                  │ │
│  │  • 展開可能なセクション                                   │ │
│  │                                                           │ │
│  └───────────────────────────────────────────────────────────┘ │
│                                                                 │
│  ┌─────────────────┐  ┌─────────────────┐  ┌────────────────┐ │
│  │ 📄 PDF出力      │  │ 📋 Notion連携   │  │ 🔄 再分析     │ │
│  └─────────────────┘  └─────────────────┘  └────────────────┘ │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 5.4 UIコンポーネント仕様

#### カードコンポーネント

```tsx
interface DecisionCardProps {
  type: "dao" | "fa" | "shu" | "qi" | "review";
  status: "pending" | "processing" | "completed" | "error";
  title: string;
  summary?: string;
  expandable: boolean;
  children?: React.ReactNode;
}

// 各タイプのアイコン・カラー
const typeConfig = {
  dao: { icon: "🎯", color: "indigo", label: "本質" },
  fa: { icon: "🛤️", color: "violet", label: "戦略" },
  shu: { icon: "📋", color: "blue", label: "計画" },
  qi: { icon: "🔧", color: "emerald", label: "実装" },
  review: { icon: "🔍", color: "amber", label: "検証" },
};
```

#### プログレスインジケーター

```tsx
interface AgentProgressProps {
  agents: {
    name: string;
    status: "waiting" | "running" | "completed" | "failed";
    progress: number; // 0-100
    message?: string;
  }[];
}
```

---

## 6. 技術スタック（確定）

### 6.1 バックエンド

```yaml
言語: Python 3.12+
フレームワーク: FastAPI
バリデーション: Pydantic v2
非同期: asyncio + anyio
状態機械: transitions / 自作FSM
LLM統合: litellm (抽象化層)
RAG: LlamaIndex (Shu/Qi専用)
DB: PostgreSQL 16 + pgvector
キャッシュ: Redis 7
タスクキュー: Celery / ARQ
```

### 6.2 フロントエンド

```yaml
フレームワーク: Next.js 14 (App Router)
言語: TypeScript 5.x
スタイリング: Tailwind CSS + shadcn/ui
状態管理: Zustand
データ取得: TanStack Query
WebSocket: Socket.io-client
アニメーション: Framer Motion
チャート: Recharts
PDF生成: @react-pdf/renderer
```

### 6.3 インフラ

```yaml
コンテナ: Docker + Docker Compose
オーケストレーション: Kubernetes (将来)
CI/CD: GitHub Actions
モニタリング: Prometheus + Grafana
ログ: Loki / CloudWatch
```

---

## 7. 開発制約・ルール（AI実装時の指針）

### 7.1 絶対に守るべきルール

```markdown
1. 【構造化出力の厳守】
   - 全てのAgent出力はPydanticモデルで定義
   - 自由文テキストは禁止
   - JSON Schema準拠

2. 【状態遷移の厳格化】
   - 定義された遷移以外は実行不可
   - スキップ禁止
   - タイムアウト必須

3. 【責任の明確化】
   - 各Agentは単一責任
   - 入出力は明示的に型定義
   - 暗黙の状態共有禁止

4. 【RAGの制限】
   - Dao/Faでは使用禁止
   - Shu/Qiでのみ許可
   - ソース明記必須

5. 【エラーハンドリング】
   - 全てのエラーは構造化
   - ユーザーへの影響を明記
   - リカバリー方法を提示
```

### 7.2 コード品質ルール

```python
# ✅ Good: 明示的な型、単一責任
class DaoAgent:
    def run(self, input: DaoInput, ctx: Context) -> DaoOutput:
        essence = self._extract_essence(input.question)
        constraints = self._identify_constraints(input)
        return DaoOutput(essence=essence, constraints=constraints)

# ❌ Bad: 暗黙の依存、曖昧な戻り値
class BadAgent:
    def run(self, data):
        result = self.llm.chat(data)  # 暗黙のLLM依存
        return result  # 型不明
```

### 7.3 プロンプト設計ルール

```markdown
各Agentのシステムプロンプトには以下を必ず含める:

1. 【役割定義】
   - あなたは○○Agentです
   - 唯一の責任は○○です

2. 【出力形式の強制】
   - 必ず以下のJSON形式で出力してください
   - 形式外の出力は無効です

3. 【禁止事項】
   - ○○について言及してはいけません
   - ○○の判断は次のAgentに委ねてください

4. 【例示】
   - 良い出力例
   - 悪い出力例
```

---

## 8. 実装ロードマップ

### Phase 1: MVP（2週間）

```
Week 1:
- [ ] プロジェクト構造構築
- [ ] DecisionContext schema定義
- [ ] 5 Agent の入出力schema定義
- [ ] DaoAgent実装（Prompt + Parser）

Week 2:
- [ ] Fa/Shu/Qi Agent実装
- [ ] ReviewAgent実装
- [ ] 状態機械実装
- [ ] 単体テスト
```

### Phase 2: 統合（2週間）

```
Week 3:
- [ ] API エンドポイント実装
- [ ] WebSocket進捗通知
- [ ] フロントエンド入力画面

Week 4:
- [ ] フロントエンド結果画面
- [ ] PDF出力機能
- [ ] E2Eテスト
```

### Phase 3: 検証（2週間）

```
Week 5-6:
- [ ] 実問題での検証（3ケース以上）
- [ ] プロンプトチューニング
- [ ] ReviewAgent強化
- [ ] ドキュメント整備
```

---

## 付録A: ディレクトリ構成

```
decision-agent-platform/
├── backend/
│   ├── app/
│   │   ├── __init__.py
│   │   ├── main.py              # FastAPI app
│   │   ├── config.py            # 設定
│   │   │
│   │   ├── agents/              # Agent実装
│   │   │   ├── __init__.py
│   │   │   ├── base.py          # BaseAgent
│   │   │   ├── dao.py
│   │   │   ├── fa.py
│   │   │   ├── shu.py
│   │   │   ├── qi.py
│   │   │   └── review.py
│   │   │
│   │   ├── schemas/             # Pydanticモデル
│   │   │   ├── __init__.py
│   │   │   ├── input.py
│   │   │   ├── output.py
│   │   │   └── internal.py
│   │   │
│   │   ├── flow/                # 状態機械
│   │   │   ├── __init__.py
│   │   │   ├── state_machine.py
│   │   │   └── transitions.py
│   │   │
│   │   ├── services/            # ビジネスロジック
│   │   │   ├── __init__.py
│   │   │   ├── decision.py
│   │   │   ├── llm_gateway.py
│   │   │   └── rag_service.py
│   │   │
│   │   ├── api/                 # APIルート
│   │   │   ├── __init__.py
│   │   │   ├── decisions.py
│   │   │   └── websocket.py
│   │   │
│   │   └── prompts/             # プロンプトテンプレート
│   │       ├── dao.txt
│   │       ├── fa.txt
│   │       ├── shu.txt
│   │       ├── qi.txt
│   │       └── review.txt
│   │
│   ├── tests/
│   ├── pyproject.toml
│   └── Dockerfile
│
├── frontend/
│   ├── src/
│   │   ├── app/
│   │   │   ├── page.tsx         # 入力画面
│   │   │   ├── processing/
│   │   │   │   └── page.tsx     # 処理中画面
│   │   │   └── report/
│   │   │       └── [id]/
│   │   │           └── page.tsx # 結果画面
│   │   │
│   │   ├── components/
│   │   │   ├── ui/              # shadcn/ui
│   │   │   ├── decision-input/
│   │   │   ├── agent-progress/
│   │   │   └── report-view/
│   │   │
│   │   ├── lib/
│   │   │   ├── api.ts
│   │   │   └── websocket.ts
│   │   │
│   │   └── styles/
│   │       └── globals.css
│   │
│   ├── package.json
│   └── Dockerfile
│
├── docker-compose.yml
└── README.md
```

---

## 付録B: API仕様

### POST /api/decisions

```yaml
Request:
  Content-Type: application/json
  Body: DecisionRequest

Response:
  201 Created
  Body:
    decision_id: string
    status: "ACCEPTED"
    websocket_url: string
```

### GET /api/decisions/{id}

```yaml
Response:
  200 OK
  Body: DecisionReport
```

### WebSocket /ws/decisions/{id}

```yaml
Messages (Server → Client):
  - type: "AGENT_START"
    agent: string

  - type: "AGENT_PROGRESS"
    agent: string
    progress: number
    message: string

  - type: "AGENT_COMPLETE"
    agent: string
    result: object

  - type: "DECISION_COMPLETE"
    report: DecisionReport

  - type: "ERROR"
    error: ErrorDetail
```
