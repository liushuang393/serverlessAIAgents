# {{ agent_name | title }}

{{ agent_description }}

## 概要

このエージェントはデータ処理パイプラインを提供します。

## 機能

- {{ input_format | upper }} 形式のデータ読み込み
{% if enable_validation %}- データ品質検証{% endif %}
{% if enable_transformation %}- データ変換{% endif %}
- {{ output_format | upper }} 形式でのデータ出力
- バッチ処理（1000行ずつ）

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
agentflow run . --input '{
  "input_path": "data.{{ input_format }}",
  "output_path": "output.{{ output_format }}",
  "transformations": ["normalize", "deduplicate"]
}'
```

## 入力

- `input_path` (string, required): 入力ファイルパス
- `output_path` (string, required): 出力ファイルパス
- `transformations` (array, optional): 適用する変換のリスト

## 出力

- `processed_data` (object): 処理統計
  - `rows_processed` (integer): 処理した行数
  - `rows_valid` (integer): 有効な行数
  - `rows_invalid` (integer): 無効な行数

## 設定

`agent.yaml` で以下の設定が可能です：

- `timeout`: タイムアウト時間（秒）
- `max_retries`: 最大リトライ回数
- `enable_logging`: ログ出力の有効化
- `batch_size`: バッチサイズ

## データ形式

- **入力形式**: {{ input_format | upper }}
- **出力形式**: {{ output_format | upper }}

## 変換

以下の変換がサポートされています：

- `normalize`: データの正規化
- `deduplicate`: 重複削除
- `filter`: フィルタリング
- `aggregate`: 集計
- `sort`: ソート

## プロトコル対応

このエージェントは以下のプロトコルに対応しています：

- **MCP**: Model Context Protocol
- **A2A**: Agent-to-Agent Protocol

## ライセンス

MIT License

## 作成者

{{ author }} <{{ email }}>

