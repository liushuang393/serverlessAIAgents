# Market Trend Monitor - 設計書

## 1. システム概要

### 目的
COBOL→Java移行、AI関連技術の市場動向を自動的に収集・分析し、**意思決定支援**を行うシステム。
AgentFlowフレームワークを活用した「情報収集→判断→行動」の意思決定支援システムとして設計。

### 主要機能
1. **データ収集**: 複数ソース（ニュース、GitHub、arXiv、RSS）から情報を自動収集
2. **トレンド分析**: キーワード抽出、トピック分類、重要度評価
3. **レポート生成**: 日次/週次レポートの自動生成
4. **リアルタイム通知**: 重要な変化を検知して即座に通知
5. **ダッシュボード**: トレンドグラフ、最新ニュース、分析結果の可視化

### 核心4機能（意思決定支援）
| 機能 | 説明 | 目的 |
|------|------|------|
| **Evidence Ledger** | 証拠台帳 | 全結論の追跡可能性確保 |
| **Signal Scoring** | 信号評価 | 統一的な信号強度評価（A/B/C/D） |
| **Red Team Analysis** | 反証分析 | バイアス軽減、失効条件明確化 |
| **Prediction Review** | 予測復盤 | 予測精度評価、学習フィードバック |

### ターゲットユーザー
- IT企業の技術戦略担当者
- レガシーシステム移行プロジェクトマネージャー
- AI技術導入を検討する意思決定者

---

## 2. アーキテクチャ設計

### システム構成図

```
┌──────────────────────────────────────────────────────────────────────────┐
│                         Frontend (React)                                  │
│  ┌────────────┐  ┌────────────┐  ┌────────────┐  ┌────────────┐         │
│  │ Dashboard  │  │ Evidence   │  │ Prediction │  │ Reports    │         │
│  │            │  │ Viewer     │  │ Tracker    │  │            │         │
│  └────────────┘  └────────────┘  └────────────┘  └────────────┘         │
└──────────────────────────────────────────────────────────────────────────┘
                                    │
                         REST API / WebSocket / SSE
                                    │
┌──────────────────────────────────────────────────────────────────────────┐
│                    Backend (AgentFlow Framework)                          │
│                                                                           │
│  ┌─────────────────────────────────────────────────────────────────────┐ │
│  │                    Multi-Agent Coordinator                           │ │
│  │  ┌─────────────────┐  ┌─────────────────┐                           │ │
│  │  │ Flow Controller │  │ ReflectionLoop  │                           │ │
│  │  └─────────────────┘  └─────────────────┘                           │ │
│  └─────────────────────────────────────────────────────────────────────┘ │
│                                                                           │
│  ┌─────────────────────── Agent Pipeline ──────────────────────────────┐ │
│  │                                                                      │ │
│  │  ┌──────────┐    ┌──────────┐    ┌──────────┐    ┌──────────┐      │ │
│  │  │Collector │ -> │Evidence  │ -> │Analyzer  │ -> │Signal    │      │ │
│  │  │Agent     │    │Ledger    │    │Agent     │    │Scorer    │      │ │
│  │  └──────────┘    └──────────┘    └──────────┘    └──────────┘      │ │
│  │                                        │                             │ │
│  │                                        v                             │ │
│  │  ┌──────────┐    ┌──────────┐    ┌──────────┐                      │ │
│  │  │Notifier  │ <- │Reporter  │ <- │Red Team  │                      │ │
│  │  │Agent     │    │Agent     │    │Agent     │                      │ │
│  │  └──────────┘    └──────────┘    └──────────┘                      │ │
│  │                                                                      │ │
│  └──────────────────────────────────────────────────────────────────────┘ │
│                                                                           │
│  ┌─────────────────────── Core Modules ────────────────────────────────┐ │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐              │ │
│  │  │ Evidence     │  │ Signal       │  │ Prediction   │              │ │
│  │  │ Ledger       │  │ Scoring      │  │ Review       │              │ │
│  │  │ (証拠台帳)   │  │ (信号評価)   │  │ (予測復盤)   │              │ │
│  │  └──────────────┘  └──────────────┘  └──────────────┘              │ │
│  └──────────────────────────────────────────────────────────────────────┘ │
│                                                                           │
│  ┌─────────────────────── Storage Layer ───────────────────────────────┐ │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐              │ │
│  │  │ MemoryManager│  │ Database     │  │ VectorStore  │              │ │
│  │  │ (3段階記憶)  │  │ (PostgreSQL) │  │ (検索)       │              │ │
│  │  └──────────────┘  └──────────────┘  └──────────────┘              │ │
│  └──────────────────────────────────────────────────────────────────────┘ │
└──────────────────────────────────────────────────────────────────────────┘
                                    │
                         External Data Sources
                                    │
┌──────────────────────────────────────────────────────────────────────────┐
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  ┌────────────┐  │
│  │ NewsAPI      │  │ GitHub API   │  │ arXiv API    │  │ RSS Feeds  │  │
│  └──────────────┘  └──────────────┘  └──────────────┘  └────────────┘  │
│                                                                          │
│  ┌──────────────────────────────────────────────────────────────────┐   │
│  │                    情報源台帳 (Source Registry)                   │   │
│  │  - 信頼度スコア管理  - 法的準拠チェック  - 利用規約管理           │   │
│  └──────────────────────────────────────────────────────────────────┘   │
└──────────────────────────────────────────────────────────────────────────┘
```

### 技術スタック

**バックエンド**:
- **フレームワーク**: AgentFlow (Python 3.13+)
- **パターン**: Multi-Agent Collaboration + Reflection Pattern
- **プロトコル**: AG-UI (フロントエンド通信), MCP (外部ツール連携)
- **Web API**: FastAPI
- **データベース**: SQLite (開発), PostgreSQL (本番)
- **記憶システム**: AgentFlow MemoryManager (3段階記憶)
- **スケジューラー**: APScheduler

**フロントエンド**:
- **フレームワーク**: React 18 + TypeScript
- **UI ライブラリ**: Material-UI (MUI)
- **状態管理**: Zustand
- **グラフ**: Recharts
- **通信**: Axios + WebSocket

---

## 3. エージェント設計

### 3.1 CollectorAgent (データ収集エージェント)

**役割**: 複数のソースから市場動向データを収集

**入力**:
```python
{
    "keywords": ["COBOL", "Java migration", "AI", "LLM"],
    "sources": ["news", "github", "arxiv", "rss"],
    "date_range": {"start": "2025-01-01", "end": "2025-01-18"}
}
```

**出力**:
```python
{
    "articles": [
        {
            "id": "uuid",
            "title": "記事タイトル",
            "url": "https://...",
            "source": "news",
            "published_at": "2025-01-18T10:00:00Z",
            "content": "記事本文...",
            "keywords": ["COBOL", "Java"]
        }
    ],
    "total_count": 150
}
```

**実装**:
- MCP プロトコルで外部 API と連携
- レート制限とエラーハンドリング
- キャッシュ機構で重複収集を防止

### 3.2 AnalyzerAgent (分析エージェント)

**役割**: 収集データを分析し、トレンドを抽出

**入力**: CollectorAgent の出力

**出力**:
```python
{
    "trends": [
        {
            "topic": "COBOL to Java Migration",
            "score": 0.85,
            "articles_count": 45,
            "keywords": ["modernization", "cloud", "microservices"],
            "sentiment": "positive",
            "growth_rate": 0.23
        }
    ],
    "summary": "今週はCOBOL移行案件が23%増加..."
}
```

**実装**:
- キーワード抽出（TF-IDF）
- トピック分類（LLM API 使用）
- センチメント分析
- トレンド計算（時系列分析）

### 3.3 ReporterAgent (レポート生成エージェント)

**役割**: 分析結果から読みやすいレポートを生成

**入力**: AnalyzerAgent の出力

**出力**:
```python
{
    "report_id": "uuid",
    "title": "週次市場動向レポート (2025-W03)",
    "sections": [
        {
            "title": "エグゼクティブサマリー",
            "content": "..."
        },
        {
            "title": "主要トレンド",
            "content": "...",
            "charts": [...]
        }
    ],
    "generated_at": "2025-01-18T12:00:00Z"
}
```

**実装**:
- LLM API でサマリー生成
- Markdown/HTML フォーマット
- グラフデータ生成

### 3.4 NotifierAgent (通知エージェント)

**役割**: 重要な変化を検知して通知

**入力**: AnalyzerAgent の出力

**出力**:
```python
{
    "notifications": [
        {
            "type": "alert",
            "priority": "high",
            "message": "COBOL移行案件が急増（+50%）",
            "timestamp": "2025-01-18T12:00:00Z"
        }
    ]
}
```

**実装**:
- 閾値ベースのアラート
- 通知チャネル（メール、Slack、WebSocket）
- 通知履歴管理
- MemoryManager との連携（重複通知防止）

### 3.5 EvidenceLedgerAgent (証拠台帳エージェント) 【NEW】

**役割**: 全ての結論に対する証拠の追跡可能性を確保

**入力**: CollectorAgent の出力（記事リスト）

**出力**:
```python
{
    "evidence_ids": ["uuid1", "uuid2", ...],
    "total_registered": 150,
    "sources_verified": 5
}
```

**機能**:
- 証拠の登録・管理
- 主張と証拠の紐付け
- 主張レベルの自動判定（Lead → Hypothesis → Finding → Conclusion）
- 証拠チェーンの構築

**主張レベル基準**:
| レベル | 必要証拠数 | 信頼度閾値 | 説明 |
|--------|-----------|-----------|------|
| LEAD | 1以上 | <0.5 | 初期手がかり |
| HYPOTHESIS | 2以上 | 0.5-0.7 | 仮説 |
| FINDING | 3以上 | 0.7-0.85 | 発見 |
| CONCLUSION | 5以上 | >0.85 | 結論 |

### 3.6 SignalScorerAgent (信号評価エージェント) 【NEW】

**役割**: 統一的な信号強度評価システム

**入力**: AnalyzerAgent の出力（トレンドリスト）

**出力**:
```python
{
    "signals": [
        {
            "trend_id": "uuid",
            "score": {
                "reliability": 0.8,
                "leading": 0.7,
                "relevance": 0.9,
                "actionability": 0.6,
                "convergence": 0.75
            },
            "grade": "A",
            "total": 4.2
        }
    ]
}
```

**5軸評価体系**:
| 軸 | 説明 | 評価基準 |
|---|------|---------|
| 信頼性 (Reliability) | 情報源の信頼度 | 一次情報=高、SNS=低 |
| 先行性 (Leading) | 市場先行度 | 公開前情報=高、事後報道=低 |
| 関連性 (Relevance) | ドメイン関連度 | キーワード一致率 |
| 実行可能性 (Actionability) | 行動可能性 | 具体的施策示唆=高 |
| 収束性 (Convergence) | 多ソース一致度 | 複数ソース裏付け=高 |

**信号グレード基準**:
| グレード | スコア範囲 | 意味 | 推奨アクション |
|---------|-----------|------|--------------|
| A (強信号) | ≥4.0 | 高確度で行動可能 | 即時対応を検討 |
| B (中信号) | 3.0-3.9 | 注視すべき兆候 | 追加調査を推奨 |
| C (弱信号) | 2.0-2.9 | 参考レベル | 継続モニタリング |
| D (雑音) | <2.0 | ノイズ | 無視可 |

### 3.7 RedTeamAgent (反証分析エージェント) 【NEW】

**役割**: 各トレンド判断に対する反対論点を自動生成し、バイアスを軽減

**入力**: AnalyzerAgent の主張（Claim）リスト

**出力**:
```python
{
    "analyses": [
        {
            "claim_id": "uuid",
            "counter_arguments": [
                {"argument": "反論内容", "strength": 0.7}
            ],
            "invalidation_conditions": [
                {"condition": "〇〇が発生したら無効", "probability": 0.3}
            ],
            "overall_uncertainty": 0.35,
            "recommendation": "継続監視を推奨"
        }
    ]
}
```

**機能**:
- 反論生成 (Counter-Arguments)
- 失効条件特定 (Invalidation Conditions)
- トリガー条件定義 (Trigger Conditions)
- 不確実性定量化 (Uncertainty Quantification)

---

## 4. API設計

### REST API エンドポイント

**基本エンドポイント**:
```
GET  /api/trends              # トレンド一覧取得
GET  /api/trends/{id}         # トレンド詳細取得
GET  /api/articles            # 記事一覧取得
GET  /api/reports             # レポート一覧取得
GET  /api/reports/{id}        # レポート詳細取得
POST /api/collect             # 手動データ収集トリガー
GET  /api/settings            # 設定取得
PUT  /api/settings            # 設定更新
```

**Evidence Ledger エンドポイント**:
```
GET  /api/evidence            # 証拠一覧取得
GET  /api/evidence/{id}       # 証拠詳細取得
GET  /api/claims              # 主張一覧取得
GET  /api/claims/{id}/chain   # 主張の証拠チェーン取得
POST /api/evidence            # 証拠登録
```

**Signal Scoring エンドポイント**:
```
GET  /api/signals             # 信号一覧取得
GET  /api/signals/{id}        # 信号詳細取得
GET  /api/signals/dashboard   # 信号ダッシュボード
```

**Prediction Review エンドポイント**:
```
GET  /api/predictions         # 予測一覧取得
GET  /api/predictions/{id}    # 予測詳細取得
POST /api/predictions/{id}/review  # 予測結果レビュー
GET  /api/predictions/accuracy     # 精度統計
```

**情報源管理エンドポイント**:
```
GET  /api/sources             # 情報源一覧取得
PUT  /api/sources/{id}        # 情報源設定更新
```

### WebSocket イベント

```
event: trend_update           # トレンド更新通知
event: collection_progress    # 収集進捗通知
event: analysis_complete      # 分析完了通知
event: alert                  # アラート通知
event: evidence_registered    # 証拠登録通知
event: signal_evaluated       # 信号評価完了通知
event: prediction_due         # 予測レビュー期限通知
```

---

## 5. データモデル

### 5.1 基本モデル

#### Article (記事)
```python
@dataclass
class Article:
    id: str
    title: str
    url: str
    source: str
    published_at: datetime
    content: str
    keywords: list[str]
    collected_at: datetime
```

#### Trend (トレンド)
```python
@dataclass
class Trend:
    id: str
    topic: str
    score: float
    articles_count: int
    keywords: list[str]
    sentiment: str
    growth_rate: float
    created_at: datetime
    signal_id: str | None  # 関連Signal ID
```

#### Report (レポート)
```python
@dataclass
class Report:
    id: str
    title: str
    sections: list[ReportSection]
    generated_at: datetime
    period: str
```

### 5.2 Evidence Ledger モデル

#### Evidence (証拠)
```python
class SourceType(Enum):
    NEWS = "news"
    GITHUB = "github"
    ARXIV = "arxiv"
    RSS = "rss"

@dataclass
class Evidence:
    id: str
    source_id: str                 # 情報源ID
    source_type: SourceType        # 情報源タイプ
    url: str
    title: str
    content_hash: str              # 重複検出用
    extracted_data: dict           # 抽出データ
    collected_at: datetime
    reliability_score: float       # 信頼度スコア (0-1)
```

#### Claim (主張)
```python
class ClaimLevel(Enum):
    LEAD = "lead"               # 初期手がかり
    HYPOTHESIS = "hypothesis"   # 仮説
    FINDING = "finding"         # 発見
    CONCLUSION = "conclusion"   # 結論

@dataclass
class Claim:
    id: str
    statement: str                 # 主張内容
    level: ClaimLevel              # 主張レベル
    confidence: float              # 信頼度 (0-1)
    evidence_ids: list[str]        # 根拠となる証拠ID
    created_at: datetime
    updated_at: datetime
```

### 5.3 Signal Scoring モデル

```python
class SignalGrade(Enum):
    A = "A"  # 強信号 (≥4.0)
    B = "B"  # 中信号 (3.0-3.9)
    C = "C"  # 弱信号 (2.0-2.9)
    D = "D"  # 雑音 (<2.0)

@dataclass
class SignalScore:
    reliability: float      # 信頼性 (0-1)
    leading: float          # 先行性 (0-1)
    relevance: float        # 関連性 (0-1)
    actionability: float    # 実行可能性 (0-1)
    convergence: float      # 収束性 (0-1)

    @property
    def total(self) -> float:
        return (self.reliability + self.leading + self.relevance +
                self.actionability + self.convergence)

@dataclass
class Signal:
    id: str
    trend_id: str
    score: SignalScore
    grade: SignalGrade
    evaluated_at: datetime
```

### 5.4 Red Team モデル

```python
@dataclass
class CounterArgument:
    argument: str
    strength: float         # 反論の強さ (0-1)

@dataclass
class InvalidationCondition:
    condition: str          # 失効条件
    probability: float      # 発生確率

@dataclass
class RedTeamAnalysis:
    id: str
    claim_id: str
    counter_arguments: list[CounterArgument]
    invalidation_conditions: list[InvalidationCondition]
    overall_uncertainty: float  # 総合不確実性 (0-1)
    recommendation: str
    analyzed_at: datetime
```

### 5.5 Prediction Review モデル

```python
class PredictionStatus(Enum):
    PENDING = "pending"     # 期限未到来
    CORRECT = "correct"     # 的中
    PARTIAL = "partial"     # 部分的中
    INCORRECT = "incorrect" # 外れ

@dataclass
class Prediction:
    id: str
    statement: str          # 予測内容
    target_date: date       # 予測対象日
    confidence: float       # 予測時信頼度
    claim_id: str | None    # 関連主張ID
    created_at: datetime
    status: PredictionStatus
    actual_outcome: str | None    # 実際の結果
    reviewed_at: datetime | None
```

---

## 6. ディレクトリ構造

```
apps/market_trend_monitor/
├── backend/
│   ├── agents/                    # エージェント実装
│   │   ├── __init__.py
│   │   ├── collector_agent.py     # データ収集
│   │   ├── analyzer_agent.py      # トレンド分析
│   │   ├── reporter_agent.py      # レポート生成
│   │   ├── notifier_agent.py      # 通知
│   │   ├── evidence_ledger_agent.py  # 証拠管理【NEW】
│   │   ├── signal_scorer_agent.py    # 信号評価【NEW】
│   │   └── red_team_agent.py         # 反証分析【NEW】
│   │
│   ├── api/
│   │   ├── __init__.py
│   │   ├── main.py
│   │   └── routes/                # ルート分割
│   │       ├── __init__.py
│   │       ├── trends.py
│   │       ├── evidence.py        # 【NEW】
│   │       ├── signals.py         # 【NEW】
│   │       ├── predictions.py     # 【NEW】
│   │       └── sources.py         # 【NEW】
│   │
│   ├── models/                    # データモデル
│   │   ├── __init__.py
│   │   ├── schemas.py             # 基本スキーマ
│   │   ├── agent_schemas.py       # エージェントI/O
│   │   ├── evidence.py            # 証拠モデル【NEW】
│   │   ├── signal.py              # 信号モデル【NEW】
│   │   ├── red_team.py            # Red Team モデル【NEW】
│   │   └── prediction.py          # 予測モデル【NEW】
│   │
│   ├── services/                  # ビジネスロジック層
│   │   ├── __init__.py
│   │   ├── evidence_service.py    # 【NEW】
│   │   ├── signal_service.py      # 【NEW】
│   │   ├── prediction_service.py  # 【NEW】
│   │   ├── source_registry.py     # 情報源管理【NEW】
│   │   └── scheduler.py
│   │
│   ├── integrations/              # 外部連携
│   │   ├── __init__.py
│   │   ├── news_api.py
│   │   ├── github_api.py          # 【NEW】
│   │   ├── arxiv_api.py           # 【NEW】
│   │   └── rss_fetcher.py         # 【NEW】
│   │
│   ├── config.py
│   ├── workflow.py
│   └── requirements.txt
│
├── configs/                       # 設定ファイル
│   ├── flows/
│   │   └── market_trend_monitor.yaml
│   └── sources/
│       └── source_registry.yaml   # 情報源台帳
│
├── frontend/                      # 将来実装
├── tests/
│   ├── __init__.py
│   ├── conftest.py
│   ├── test_agents.py
│   ├── test_workflow.py
│   ├── test_evidence.py           # 【NEW】
│   ├── test_signals.py            # 【NEW】
│   └── test_red_team.py           # 【NEW】
│
├── DESIGN.md
└── README.md
```

---

## 7. 実装計画

### Phase 1: データモデル拡張 & 基盤構築
- [x] エージェント基底クラス実装
- [x] Multi-Agent Coordinator 設定
- [x] 基本データモデル定義
- [ ] Evidence Ledger モデル実装
- [ ] Signal Scoring モデル実装
- [ ] Red Team モデル実装
- [ ] Prediction Review モデル実装
- [ ] Evidence Ledger Agent 実装
- [ ] Evidence Service 実装

### Phase 2: 信号評価 & 反証分析
- [ ] Signal Scorer Agent 実装
- [ ] Signal Service 実装
- [ ] Red Team Agent 実装
- [ ] 既存 CollectorAgent 拡張（Evidence連携）
- [ ] 既存 AnalyzerAgent 拡張（Signal連携）

### Phase 3: API拡張 & 予測復盤
- [ ] Evidence API エンドポイント実装
- [ ] Signal API エンドポイント実装
- [ ] Prediction Review 機能実装
- [ ] Prediction API エンドポイント実装

### Phase 4: 統合 & 外部連携
- [ ] GitHub API 連携実装
- [ ] arXiv API 連携実装
- [ ] RSS フィード連携実装
- [ ] 情報源台帳 (Source Registry) 実装

### Phase 5: テスト & 品質保証
- [ ] Evidence Ledger ユニットテスト
- [ ] Signal Scoring ユニットテスト
- [ ] Red Team ユニットテスト
- [ ] 統合テスト
- [ ] 予測精度評価テスト
