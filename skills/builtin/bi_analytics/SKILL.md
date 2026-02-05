# BI Analytics Skill

> AI Agent向けビジネスインテリジェンス分析スキル

## 概要

BI Analytics Skillは、データ分析、可視化、レポート生成を自動化するAI Agentスキルです。

## 機能

### 1. データ接続 (Connectors)

- **PostgreSQL**: 企業DB接続
- **MySQL**: レガシーシステム連携
- **BigQuery**: クラウドDWH
- **CSV/Excel**: ファイルインポート

### 2. 分析機能 (Analyzers)

- **統計分析**: 基本統計、相関、回帰
- **トレンド分析**: 時系列、季節性、予測
- **異常検出**: 外れ値、パターン異常

### 3. 可視化 (Visualizers)

- **チャート生成**: 棒グラフ、折れ線、散布図
- **ダッシュボード**: 複数指標の統合表示
- **レポート**: PDF/HTML出力

## 使用例

```python
from agentflow.skills.builtin.bi_analytics import BIAnalyzer, DataConnector

# データ接続
connector = DataConnector.from_url("postgres://...")
data = await connector.query("SELECT * FROM sales")

# 分析
analyzer = BIAnalyzer()
result = await analyzer.analyze(data, analysis_type="trend")

# 可視化
chart = await analyzer.visualize(result, chart_type="line")
```

## 設定

```yaml
bi_analytics:
  default_connector: postgres
  cache_ttl: 3600
  max_rows: 100000
```

## セキュリティ

- 接続情報は環境変数で管理
- クエリはサニタイズ処理
- テナント分離対応
