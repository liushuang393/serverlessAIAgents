# Market Trend Monitor - 設計書

## 1. システム概要

### 目的
COBOL→Java移行、AI関連技術の市場動向を自動的に収集・分析し、重要なトレンドをリアルタイムで監視するシステム。

### 主要機能
1. **データ収集**: 複数ソース（ニュース、技術記事、GitHub、論文）から情報を自動収集
2. **トレンド分析**: キーワード抽出、トピック分類、重要度評価
3. **レポート生成**: 日次/週次レポートの自動生成
4. **リアルタイム通知**: 重要な変化を検知して即座に通知
5. **ダッシュボード**: トレンドグラフ、最新ニュース、分析結果の可視化

### ターゲットユーザー
- IT企業の技術戦略担当者
- レガシーシステム移行プロジェクトマネージャー
- AI技術導入を検討する意思決定者

---

## 2. アーキテクチャ設計

### システム構成

```
┌─────────────────────────────────────────────────────────┐
│                    Frontend (React)                      │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  │
│  │ Dashboard    │  │ Settings     │  │ Reports      │  │
│  └──────────────┘  └──────────────┘  └──────────────┘  │
└─────────────────────────────────────────────────────────┘
                            │
                    REST API / WebSocket
                            │
┌─────────────────────────────────────────────────────────┐
│              Backend (AgentFlow Framework)               │
│  ┌──────────────────────────────────────────────────┐  │
│  │           Multi-Agent Coordinator                 │  │
│  └──────────────────────────────────────────────────┘  │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌───────┐  │
│  │Collector │→ │Analyzer  │→ │Reporter  │→ │Notifier│ │
│  │Agent     │  │Agent     │  │Agent     │  │Agent   │  │
│  └──────────┘  └──────────┘  └──────────┘  └───────┘  │
│                                                          │
│  ┌──────────────────────────────────────────────────┐  │
│  │           Shared Context & Storage                │  │
│  └──────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────┘
                            │
                    External Services
                            │
┌─────────────────────────────────────────────────────────┐
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌────────┐ │
│  │News API  │  │GitHub API│  │RSS Feeds │  │arXiv   │ │
│  └──────────┘  └──────────┘  └──────────┘  └────────┘ │
└─────────────────────────────────────────────────────────┘
```

### 技術スタック

**バックエンド**:
- **フレームワーク**: AgentFlow (Python 3.13+)
- **パターン**: Multi-Agent Collaboration (Sequential)
- **プロトコル**: AG-UI (フロントエンド通信), MCP (外部ツール連携)
- **Web API**: FastAPI
- **データベース**: SQLite (開発), PostgreSQL (本番)
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

---

## 4. API設計

### REST API エンドポイント

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

### WebSocket イベント

```
event: trend_update           # トレンド更新通知
event: collection_progress    # 収集進捗通知
event: analysis_complete      # 分析完了通知
event: alert                  # アラート通知
```

---

## 5. データモデル

### Article (記事)
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

### Trend (トレンド)
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
```

### Report (レポート)
```python
@dataclass
class Report:
    id: str
    title: str
    sections: list[ReportSection]
    generated_at: datetime
    period: str
```

---

## 6. ディレクトリ構造

```
apps/market-trend-monitor/
├── backend/
│   ├── agents/
│   │   ├── __init__.py
│   │   ├── collector_agent.py
│   │   ├── analyzer_agent.py
│   │   ├── reporter_agent.py
│   │   └── notifier_agent.py
│   ├── api/
│   │   ├── __init__.py
│   │   ├── main.py
│   │   ├── routes/
│   │   └── websocket.py
│   ├── models/
│   │   ├── __init__.py
│   │   └── schemas.py
│   ├── services/
│   │   ├── __init__.py
│   │   ├── scheduler.py
│   │   └── storage.py
│   ├── config.py
│   └── requirements.txt
├── frontend/
│   ├── src/
│   │   ├── components/
│   │   ├── pages/
│   │   ├── stores/
│   │   ├── api/
│   │   └── App.tsx
│   ├── package.json
│   └── tsconfig.json
├── tests/
│   ├── test_agents.py
│   └── test_api.py
├── DESIGN.md
└── README.md
```

---

## 7. 実装計画

### Phase 1: バックエンド基盤
- [ ] エージェント基底クラス実装
- [ ] Multi-Agent Coordinator 設定
- [ ] データモデル定義

### Phase 2: エージェント実装
- [ ] CollectorAgent 実装
- [ ] AnalyzerAgent 実装
- [ ] ReporterAgent 実装
- [ ] NotifierAgent 実装

### Phase 3: API実装
- [ ] FastAPI サーバー構築
- [ ] REST API エンドポイント実装
- [ ] WebSocket 実装

### Phase 4: フロントエンド実装
- [ ] React プロジェクト初期化
- [ ] ダッシュボード UI
- [ ] 設定画面
- [ ] レポート表示

### Phase 5: テスト & 統合
- [ ] ユニットテスト
- [ ] 統合テスト
- [ ] E2Eテスト

