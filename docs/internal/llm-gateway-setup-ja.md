# AgentFlow Platform LLM 管理設定手順

## 1. 目的

AgentFlow の LLM 呼び出しは、Provider SDK 直呼びではなく Platform 管理の Gateway を経由します。  
正本は `apps/platform` の `LLM Management` と `.agentflow/llm_gateway.yaml` です。

この文書の前提:

- Provider / Model / Engine は Platform が管理する
- app は `contracts.llm` で Platform catalog を参照する
- API Key は Platform 画面から暗号化保存できる
- local 推論 engine は Platform から Docker 配備する

## 2. 永続化と secret 解決順

- Platform DB 既定値: `sqlite+aiosqlite:///./apps/platform/data/platform.db`
- override: `PLATFORM_DATABASE_URL`
- 暗号化キー: `PLATFORM_SECRET_MASTER_KEY`
- runtime secret cache: `apps/platform/data/platform_runtime_cache.db`
- secret 解決順: `Platform 暗号化保存 > ENV > .env > unavailable`

注意:

- API 応答では平文 API Key を返しません
- 画面には `configured / masked / source` だけを表示します
- `PLATFORM_SECRET_MASTER_KEY` が未設定の状態では暗号化保存を利用できません

## 3. 初期セットアップ

```bash
cd <repo-root>
cp .env.example .env
cp apps/platform/.env.example apps/platform/.env
```

最低限設定する値:

```dotenv
PLATFORM_SECRET_MASTER_KEY=32bytes-or-passphrase
PLATFORM_DATABASE_URL=sqlite+aiosqlite:///./apps/platform/data/platform.db
PLATFORM_DB_AUTO_CREATE=true
```

起動:

```bash
conda activate agentflow
python -m apps.platform.main serve --port 8000

cd apps/platform/frontend
npm install
npm run dev
```

## 4. Provider 管理

Platform 画面の `LLM Management` で以下を管理します。

- Provider 名
- `api_base`
- `api_key_env`
- enabled / disabled
- 暗号化済み secret

UI で API Key を保存すると、DB に暗号化保存されます。  
保存済み secret がある場合、app 側で個別 `.env` を持たなくても runtime から利用できます。

ENV fallback を使う場合の例:

```bash
export OPENAI_API_KEY=...
export ANTHROPIC_API_KEY=...
export GEMINI_API_KEY=...
```

ただし、Platform 正本運用では「画面保存を優先し、ENV は fallback」と考えてください。

## 5. Model 管理

各 model は少なくとも次を持ちます。

- `alias`
- `model_id`
- `provider`
- `model`
- `model_type`
- `engine`
- `enabled`

`model_type` は以下を使用します。

- `text`
- `embedding`
- `image`
- `speech_to_text`
- `text_to_speech`

既定 catalog には次の標準 ID を含みます。

- `platform_text_default`
- `platform_embedding_default`
- `platform_image_default`
- `platform_speech_to_text_default`
- `platform_text_to_speech_default`
- `coding_openai`
- `local_vllm_default`

## 6. Engine 配備

Platform から `vLLM / SGLang / TGI` を配備できます。

Engine 設定項目:

- `deployment_mode`
- `docker_image`
- `served_model_name`
- `container_name`
- `host_port`
- `public_base_url`
- `gpu_enabled`
- `gpu_devices`
- `gpu_count`
- `extra_env`

配備時の動作:

1. `.agentflow/llm_backends/<engine-id>/docker-compose.yml` を生成
2. `docker compose up -d` を実行
3. 配備状態と公開 URL を DB に保存

停止時:

1. 同じ compose を使って `docker compose down`
2. 停止状態を DB に保存

## 7. app 側契約

各 app は `app_config.json` の `contracts.llm` を持ちます。

例:

```json
{
  "contracts": {
    "llm": {
      "enabled": true,
      "defaults": {
        "text": {
          "provider": "openai",
          "model_id": "platform_text_default",
          "model_type": "text"
        },
        "embedding": {
          "provider": "openai",
          "model_id": "platform_embedding_default",
          "model_type": "embedding"
        }
      },
      "agent_overrides": {
        "CodeTransformationAgent": {
          "text": {
            "provider": "openai",
            "model_id": "coding_openai",
            "model_type": "text"
          }
        }
      },
      "allowed_modalities": ["text", "embedding"],
      "extra_model_refs": [
        {
          "provider": "openai",
          "model_id": "coding_openai",
          "model_type": "text"
        }
      ]
    }
  }
}
```

運用ルール:

- app は provider 名や raw model 名を直接固定しない
- Platform catalog の `provider` と `model_id` を参照する
- modality を使う app は `allowed_modalities` に明示する
- Platform 未登録の `model_id` は scan / audit で失敗する

## 8. 互換 fallback

以下の env は互換性維持のため残しています。

- `LLM_PROVIDER`
- `OPENAI_MODEL`
- `ANTHROPIC_MODEL`
- `GOOGLE_MODEL`
- `OPENAI_EMBEDDING_MODEL`

ただし、Platform 管理 app の正本ではありません。  
新規設定では `contracts.llm` と Platform catalog を優先してください。

## 9. 主要 API

- `GET /api/studios/framework/llm/overview`
- `GET /api/studios/framework/llm/catalog`
- `GET /api/studios/framework/llm/providers/runtime`
- `GET /api/studios/framework/llm/engines/status`
- `PUT /api/studios/framework/llm/providers/{provider_name}/secret`
- `DELETE /api/studios/framework/llm/providers/{provider_name}/secret`
- `POST /api/studios/framework/llm/engines/{engine_name}/deploy`
- `POST /api/studios/framework/llm/engines/{engine_name}/stop`
- `POST /api/studios/framework/llm/preflight`
- `POST /api/studios/framework/llm/switch`
- `POST /api/studios/framework/llm/setup-and-switch`

## 10. トラブルシュート

### Provider が未設定

確認順:

1. Platform 画面で secret を保存済みか
2. `PLATFORM_SECRET_MASTER_KEY` が一致しているか
3. ENV / `.env` fallback があるか

### `/llm-management` が 404

確認順:

1. Platform backend を再起動する
2. `/openapi.json` に `/api/studios/framework/llm/*` があるか確認する
3. frontend proxy と backend port が一致しているか確認する

### local engine が利用不可

確認順:

1. `docker compose ps` で container 状態を確認する
2. `public_base_url` と `base_url` が正しいか確認する
3. `health_path` が対象 engine と一致しているか確認する

### app が model を解決できない

確認順:

1. app の `contracts.llm` が Platform catalog の `model_id` を参照しているか
2. `allowed_modalities` に対象 modality が含まれているか
3. agent override 名が実際の agent 名と一致しているか
