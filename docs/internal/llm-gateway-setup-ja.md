# AgentFlow LiteLLM Gateway 設定手順（日本語）

## 1. 概要

AgentFlow の LLM 呼び出しは、Provider SDK 直呼びを禁止し、内蔵 LiteLLM Gateway に統一します。  
実行時の正本設定は `.agentflow/llm_gateway.yaml` です。

- 対応 Provider: OpenAI / Anthropic Claude / Google Gemini / Local
- 対応 Inference Engine: vLLM / SGLang / TGI
- 呼び出し契約: `generate()` / `stream()` / `tool_call()`
- 秘密情報: `ENV` 優先、未設定時のみ `.env` を参照

## 2. 初期化

初回起動時、設定ファイルが存在しなければ自動生成されます。

```bash
python -c "from agentflow.llm.gateway import load_gateway_config; load_gateway_config()"
```

生成先:

- `.agentflow/llm_gateway.yaml`

## 3. 設定項目

### 3.1 Provider 設定

必須キー:

- `name`
- `api_base`
- `api_key_env`
- `models[]`
- `enabled`

### 3.2 Inference Engine 設定

必須キー:

- `engine_type` (`vllm|sglang|tgi`)
- `base_url`
- `health_path`
- `metrics_path`
- `model_list_path`
- `enabled`

### 3.3 Model Registry（role -> model_alias）

標準 role:

- `reasoning`
- `coding`
- `cheap`
- `local`

アプリ/Agent は provider 名を指定せず、`role` で呼び出します。

```python
response = await llm.generate(
    role="reasoning",
    prompt="..."
)
```

### 3.4 Routing Policy

- `priority`: `latency|cost|quality`
- `fallback_chain`
- `load_balance_strategy`: `round_robin|least_latency|random`
- `cost_budget`

## 4. ENV 優先運用

秘密情報は `api_key_env` で参照します。  
優先順位は `ENV > .env > unavailable` です。

例:

```bash
export OPENAI_API_KEY=...
export ANTHROPIC_API_KEY=...
export GEMINI_API_KEY=...
```

追加上書き:

```bash
export LLM_GATEWAY_PRIORITY=cost
export LLM_GATEWAY_LOAD_BALANCE=least_latency
export LLM_ROLE_REASONING_ALIAS=reasoning_claude
```

## 5. Platform での管理

UI:

- 左メニュー `LLM Management`（`Settings` の上）

API:

- `GET /api/studios/framework/llm/overview`
- `PUT /api/studios/framework/llm/providers`
- `PUT /api/studios/framework/llm/engines`
- `PUT /api/studios/framework/llm/models`
- `PUT /api/studios/framework/llm/registry`
- `PUT /api/studios/framework/llm/routing-policy`

## 6. トラブルシュート

### 6.1 Provider が unavailable

確認順:

1. `api_key_env` 名が正しいか
2. シェル環境変数が設定済みか
3. `.env` に値があるか

### 6.2 Local Engine が unavailable

確認順:

1. `base_url` 到達性
2. `health_path` 応答
3. `model_list_path` でモデル列挙可能か

### 6.3 フォールバック動作確認

`routing_policy.fallback_chain` を設定し、一次モデル停止時に二次モデルへ遷移するかを確認します。  
Platform の `cost_summary` と `providers_runtime` で結果を確認できます。
