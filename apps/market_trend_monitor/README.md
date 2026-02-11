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

## 利用手順（まずはこの順番）

設計意図として、このシステムは「収集実行」を起点に各画面のデータが更新されます。  
初回は以下の順で操作すると迷いません。

1. `設定` 画面
   - 監視キーワードを設定
   - データソースを選択（news/github/arxiv/rss/stackoverflow/devto）
   - 収集期間を選択（`直近7日 / 30日 / 90日`）
   - 必要に応じて `競合ウォッチリスト` と `別名辞書` を設定して保存
2. `設定` 画面で `データ収集を実行`
   - ここが全画面データ更新のトリガーです
3. `証拠台帳` で収集記事の有無を確認
4. `シグナル` で信号スコア（A/B/C/D）を確認
5. `予測追跡` で `予測を生成`（必要時）し、期限到来後に `Review`
6. `競合分析` で `最新記事から自動発見` を実行
7. `レポート` で結果確認・必要なら PDF エクスポート

## データ生成タイミング（重要）

- `シグナル` データは、`データ収集を実行` したフロー内で `SignalScorer` が評価した時点で生成されます。
- `予測` は、収集後トレンドから自動生成、または `予測を生成` ボタンで生成されます。
- `Brier Score` はレビュー結果が1件以上入るまで `N/A` です。

## 画面にデータが出ないときの確認順

1. `設定` の収集期間が短すぎないか（まず `直近30日` 推奨）
2. `データ収集を実行` 後のメッセージで `記事件数/トレンド件数` が 0 でないか
3. `証拠台帳` にデータがあるか
4. 競合分析の場合は、ウォッチリストと別名辞書を保存済みか

## Signal 永続化（実装済み）

Signal は DB (`signals` テーブル) に保存され、API 起動時に再ロードされます。  
そのため、サーバー再起動後も過去の Signal を保持できます。

- 収集実行時: SignalScorer 評価後に upsert 保存
- API 起動時: 既存 Signal をメモリに復元
- 目的: 月次レビュー、顧客説明、時系列再現に対応

## 営業で使える訴求ポイント（差別化）

1. **Grounding Guard による根拠可視化**
   - 「何を根拠に判断したか」を証拠台帳で追跡可能
2. **競合発見の実務運用性**
   - 自動発見 + ウォッチリスト + 別名辞書で、現場の呼称揺れに対応
3. **予測の事後検証（Brier Score）**
   - 出しっぱなしではなく、予測の精度を継続評価できる

## 注意点（特に重要）

1. 収集を実行しない限り、シグナル/競合/予測の新規データは更新されません。
2. 収集期間が短いとデータゼロになりやすいため、初期運用は `直近30日` 以上を推奨します。
3. 競合分析は名称揺れの影響を受けるため、別名辞書（例: IBM / International Business Machines）を必ず整備してください。

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
