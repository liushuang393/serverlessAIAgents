# {{ agent_name | title }}

{{ agent_description }}

## 概要

このエージェントは LLM を使用した対話型チャットボットです。

## 機能

- {{ llm_provider | upper }} {{ model_name }} を使用したテキスト生成
  {% if enable_memory %}- 会話履歴の保持（最大 {{ max_history_length }} ターン）{% endif %}
- ストリーミング応答
- セッション管理

## 使用方法

### インストール

```bash
# 依存関係をインストール
pip install -r requirements.txt
```

### 環境変数の設定

{% if llm_provider == 'openai' %}

```bash
export OPENAI_API_KEY=
```

{% elif llm_provider == 'anthropic' %}

```bash
export ANTHROPIC_API_KEY=
```

{% elif llm_provider == 'google' %}

```bash
export GOOGLE_API_KEY=
```

{% endif %}

### 実行

```bash
# エージェントを実行
python agent.py
```

### API 経由での実行

```bash
# AgentFlow CLI を使用
agentflow run . --input '{"message": "Hello!", "session_id": "user123"}'
```

## 入力

- `message` (string, required): ユーザーメッセージ
- `session_id` (string, optional): セッション ID（会話履歴用）

## 出力

- `response` (string): ボットの応答
- `metadata` (object): メタデータ
  - `model` (string): 使用したモデル
  - `tokens_used` (integer): 使用トークン数
  - `confidence` (number): 信頼度

## 設定

`agent.yaml` で以下の設定が可能です：

- `timeout`: タイムアウト時間（秒）
- `max_retries`: 最大リトライ回数
- `enable_logging`: ログ出力の有効化
- `streaming`: ストリーミング応答の有効化

## LLM 設定

- **Provider**: {{ llm_provider }}
- **Model**: {{ model_name }}
  {% if enable_memory %}- **Memory**: 有効（最大 {{ max_history_length }} ターン）{% else %}- **Memory**: 無効{% endif %}

## プロトコル対応

このエージェントは以下のプロトコルに対応しています：

- **MCP**: Model Context Protocol
- **A2A**: Agent-to-Agent Protocol
- **AG-UI**: Agent-UI Protocol（ストリーミング応答）

## ライセンス

MIT License

## 作成者

{{ author }} <{{ email }}>
