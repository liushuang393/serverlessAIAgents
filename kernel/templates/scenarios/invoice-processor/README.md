# {{ agent_name | title }}

{{ agent_description }}

## 概要

このエージェントは PDF 請求書を処理し、構造化されたデータを抽出します。

## 機能

- PDF ファイルの読み込み
{% if enable_ocr %}- OCR による文字認識 (対応言語: {{ supported_languages | join(', ') }}){% endif %}
- 請求書データの抽出
- {{ database_type | upper }} データベースへの保存
- JSON/CSV 形式での出力

## 使用方法

### インストール

```bash
# 依存関係をインストール
pip install -r requirements.txt
```

### 実行

```bash
# エージェントを実行
python agent.py
```

### API 経由での実行

```bash
# AgentFlow CLI を使用
agentflow run . --input '{"pdf_path": "invoice.pdf", "output_format": "json"}'
```

## 入力

- `pdf_path` (string, required): PDF ファイルのパス
- `output_format` (string, optional): 出力形式 (json, csv, database)

## 出力

- `invoice_data` (object): 抽出された請求書データ
  - `invoice_number` (string): 請求書番号
  - `date` (string): 日付
  - `total_amount` (number): 合計金額
  - `items` (array): 明細項目

## 設定

`agent.yaml` で以下の設定が可能です：

- `timeout`: タイムアウト時間（秒）
- `max_retries`: 最大リトライ回数
- `enable_logging`: ログ出力の有効化

## データベース設定

データベースタイプ: **{{ database_type }}**

{% if database_type == 'sqlite' %}
SQLite データベースは `invoices.db` に保存されます。
{% elif database_type == 'postgresql' %}
PostgreSQL 接続情報を環境変数で設定してください：
- `POSTGRES_HOST`
- `POSTGRES_PORT`
- `POSTGRES_USER`
- `POSTGRES_PASSWORD`
- `POSTGRES_DB`
{% elif database_type == 'mysql' %}
MySQL 接続情報を環境変数で設定してください：
- `MYSQL_HOST`
- `MYSQL_PORT`
- `MYSQL_USER`
- `MYSQL_PASSWORD`
- `MYSQL_DB`
{% endif %}

## プロトコル対応

このエージェントは以下のプロトコルに対応しています：

- **MCP**: Model Context Protocol
- **A2A**: Agent-to-Agent Protocol
- **AG-UI**: Agent-UI Protocol

## ライセンス

MIT License

## 作成者

{{ author }} <{{ email }}>

