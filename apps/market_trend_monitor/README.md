# Market Trend Monitor

**市場動向監視システム** — COBOL→Java移行、AI関連技術の市場動向を自動収集・分析

## 概要

Market Trend Monitor は、AgentFlow フレームワークを使用した Multi-Agent アプリケーションです。
9 つのエージェントが協調動作し、市場動向を自動的に監視・分析します。

### 主要機能

- 📊 **データ収集**: 複数ソース（ニュース、GitHub、arXiv、RSS、Dev.to、StackOverflow）から自動収集
- 🔍 **トレンド分析**: キーワード抽出、トピッククラスタリング、センチメント分析
- 🛡️ **信頼性評価**: シグナルスコアリング、ベイズ信頼度、Red Team 検証
- 📝 **レポート生成**: 日次/週次レポートの自動生成（PDF エクスポート対応）
- 🔔 **リアルタイム通知**: 重要な変化を即座に検知
- 📈 **ダッシュボード**: React + TypeScript によるトレンド可視化

## アーキテクチャ

```
Frontend (React/Vite) ←→ REST API ←→ Backend (FastAPI + AgentFlow)
                                              ↓
                                    Flow (create_flow パイプライン)
                                              ↓
  ┌──────────┬──────────────┬──────────┬──────────────┬──────────┬──────────┬──────────┐
  ↓          ↓              ↓          ↓              ↓          ↓          ↓
Collector → EvidenceLedger → Analyzer → SignalScorer → Reporter → RedTeam → Notifier
```

### エージェント構成

| # | エージェント | 役割 |
|---|------------|------|
| 1 | **CollectorAgent** | 複数ソースからデータ収集 |
| 2 | **EvidenceLedgerAgent** | 証拠台帳の管理 |
| 3 | **AnalyzerAgent** | トレンド分析・センチメント分析 |
| 4 | **SignalScorerAgent** | シグナル信頼度スコアリング |
| 5 | **ReporterAgent** | レポート生成 |
| 6 | **RedTeamAgent** | 仮説の反証検証 |
| 7 | **NotifierAgent** | アラート通知 |
| 8 | **CompetitorTrackingAgent** | 競合動向追跡 |
| 9 | **PredictionReviewAgent** | 予測レビュー |

## セットアップ

### 前提条件

- Python 3.13+
- Node.js 18+（フロントエンド用）
- AgentFlow フレームワーク

### ポート設定（一元管理）

ポートとホストは `apps/market_trend_monitor/app_config.json` で一元管理します。

```json
{
  "api_host": "0.0.0.0",
  "api_port": 8002,
  "frontend_port": 3002
}
```

環境変数で上書きも可能です:

| 環境変数 | 説明 | デフォルト |
|---------|------|----------|
| `MARKET_TREND_MONITOR_API_HOST` | API ホスト | `app_config.json` の値 |
| `MARKET_TREND_MONITOR_API_PORT` | API ポート | `8002` |
| `MARKET_TREND_MONITOR_FRONTEND_PORT` | フロントエンドポート | `3002` |
| `OPENAI_API_KEY` | OpenAI API キー | — |
| `DATABASE_URL` | DB 接続先 | `sqlite:///./market_trend.db` |
| `LOG_LEVEL` | ログレベル | `INFO` |

## 起動方法

### バックエンド

```bash
# 依存関係インストール（初回のみ）
cd apps/market_trend_monitor/backend
pip install -r requirements.txt
```

#### ローカル開発（ホットリロード有効）

プロジェクトルートから実行します。
コード変更時に自動リロードされます（`apps/market_trend_monitor/` 配下を監視）。

```bash
python -m apps.market_trend_monitor.backend.api.main
```

#### 本番起動

```bash
uvicorn apps.market_trend_monitor.backend.api.main:app \
  --host 0.0.0.0 --port 8002
```

バックエンドは `http://localhost:8002` で起動します。

### フロントエンド

```bash
# 依存関係インストール（初回のみ）
cd apps/market_trend_monitor/frontend
npm install

# 開発サーバー起動（ホットリロード有効）
npm run dev
```

フロントエンドは `http://localhost:3002` で起動します。
Vite の HMR により、ソース変更は即座にブラウザへ反映されます。

### API ドキュメント

バックエンド起動後、以下の URL で確認できます:

- Swagger UI: http://localhost:8002/docs
- ReDoc: http://localhost:8002/redoc

## テスト

```bash
# バックエンド ユニットテスト
pytest apps/market_trend_monitor/tests/ -v

# カバレッジ付き
pytest apps/market_trend_monitor/tests/ --cov=apps/market_trend_monitor/backend --cov-report=html

# フロントエンド テスト
cd apps/market_trend_monitor/frontend
npm test
```

## ディレクトリ構造

```
apps/market_trend_monitor/
├── app_config.json              # ポート設定（一元管理）
├── backend/
│   ├── agents/                  # エージェント実装（9 エージェント）
│   ├── api/                     # FastAPI サーバー
│   │   ├── main.py              # エントリポイント
│   │   ├── state.py             # アプリケーション状態
│   │   └── routes/              # API ルーティング
│   ├── db/                      # データベース（SQLAlchemy）
│   ├── integrations/            # 外部 API 連携
│   ├── models/                  # データモデル・スキーマ
│   ├── services/                # ビジネスロジック
│   ├── config.py                # 設定管理
│   ├── workflow.py              # Flow パイプライン定義
│   ├── workflow_engine.py       # ワークフローエンジン
│   └── requirements.txt         # Python 依存関係
├── frontend/                    # React + Vite + TypeScript
│   ├── src/
│   │   ├── components/          # UI コンポーネント
│   │   ├── api/                 # API クライアント
│   │   ├── store/               # Zustand 状態管理
│   │   └── types/               # 型定義
│   ├── package.json
│   └── vite.config.ts
├── tests/                       # バックエンドテスト
├── DESIGN.md                    # 設計書
└── README.md                    # このファイル
```

## ライセンス

MIT License

## 関連ドキュメント

- [設計書](DESIGN.md)
