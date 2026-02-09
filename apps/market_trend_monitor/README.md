# Market Trend Monitor

**市場動向監視システム** - COBOL→Java移行、AI関連技術の市場動向を自動収集・分析

## 概要

Market Trend Monitor は、AgentFlow フレームワークを使用した実用的なアプリケーション例です。Multi-Agent パターンで複数のエージェントが協調動作し、
市場動向を自動的に監視・分析します。

### 主要機能

- 📊 **データ収集**: 複数ソース（ニュース、GitHub、arXiv、RSS）から自動収集
- 🔍 **トレンド分析**: キーワード抽出、トピック分類、センチメント分析
- 📝 **レポート生成**: 日次/週次レポートの自動生成
- 🔔 **リアルタイム通知**: 重要な変化を即座に検知
- 📈 **ダッシュボード**: トレンドグラフ、最新ニュースの可視化

## アーキテクチャ

```
Frontend (React) ←→ REST API / WebSocket ←→ Backend (AgentFlow)
                                                    ↓
                                          Multi-Agent Coordinator
                                                    ↓
                        ┌───────────────┬───────────────┬───────────────┐
                        ↓               ↓               ↓               ↓
                  CollectorAgent  AnalyzerAgent  ReporterAgent  NotifierAgent
```

### エージェント構成

1. **CollectorAgent**: データ収集
2. **AnalyzerAgent**: トレンド分析
3. **ReporterAgent**: レポート生成
4. **NotifierAgent**: 通知送信

## セットアップ

### 前提条件

- Python 3.13+
- Node.js 18+ (フロントエンド用)
- AgentFlow フレームワーク

### バックエンドセットアップ

```bash
# 依存関係インストール
cd ./apps/market-trend-monitor/backend
pip install -r requirements.txt

# 環境変数設定（オプション）
export OPENAI_API_KEY=
export DATABASE_URL="sqlite:///./market_trend.db"
export LOG_LEVEL="INFO"

# サーバー起動
uvicorn apps.market_trend_monitor.backend.api.main:app --host 0.0.0.0 --port 8002 --reload
```

サーバーは `http://localhost:8002` で起動します。

### フロントエンドセットアップ

```bash
# 依存関係インストール
cd ./apps/market_trend_monitor/frontend
npm install

# 開発サーバー起動
npm run dev
```

フロントエンドは `http://localhost:3002` で起動します。

### API ドキュメント

起動後、以下の URL でドキュメントを確認できます:
- Swagger UI: http://localhost:8000/docs
- ReDoc: http://localhost:8000/redoc

## 使用方法

### 手動データ収集

```bash
curl -X POST http://localhost:8000/api/collect \
  -H "Content-Type: application/json" \
  -d '{
    "keywords": ["COBOL", "Java migration", "AI"],
    "sources": ["news", "github"]
  }'
```

### Python API

```python
from apps.market_trend_monitor.backend.workflow import workflow

# ワークフロー初期化
await workflow.initialize()

# 実行
result = await workflow.run({
    "keywords": ["COBOL", "Java migration"],
    "sources": ["news"]
})

# クリーンアップ
await workflow.cleanup()
```

## テスト

```bash
# ユニットテスト実行
cd apps/market-trend-monitor
pytest tests/ -v

# カバレッジ付き
pytest tests/ --cov=backend --cov-report=html
```

## 設定

設定は `backend/config.py` で管理されています:

```python
from apps.market_trend_monitor.backend.config import config

# 収集設定
config.collector.keywords = ["COBOL", "Java", "AI"]
config.collector.interval_seconds = 3600

# 分析設定
config.analyzer.llm_provider = "openai"
config.analyzer.llm_model = "gpt-4"

# 通知設定
config.notifier.alert_growth_rate_threshold = 0.3
```

## ディレクトリ構造

```
apps/market-trend-monitor/
├── backend/
│   ├── agents/              # エージェント実装
│   │   ├── collector_agent.py
│   │   ├── analyzer_agent.py
│   │   ├── reporter_agent.py
│   │   └── notifier_agent.py
│   ├── api/                 # FastAPI サーバー
│   │   └── main.py
│   ├── models/              # データモデル
│   │   └── schemas.py
│   ├── config.py            # 設定管理
│   ├── workflow.py          # ワークフロー定義
│   └── requirements.txt
├── frontend/                # React フロントエンド（未実装）
├── tests/                   # テスト
│   ├── test_agents.py
│   └── test_workflow.py
├── DESIGN.md                # 設計書
└── README.md                # このファイル
```


## ライセンス

MIT License

## 関連ドキュメント

- [設計書](DESIGN.md)
- [AgentFlow ドキュメント](../../../docs/)
