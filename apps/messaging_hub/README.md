# Messaging Hub - マルチプラットフォーム AI チャットボット

統一メッセージプラットフォームゲートウェイ。[moltbot](https://github.com/moltbot/moltbot) に類似した実装で、Telegram、Slack、Discord などのマルチプラットフォーム統合をサポート。

## Product Position

- `product_line`: `assistant`
- `surface_profile`: `business`
- `security_mode` 既定値: `approval_required`（明示指定なし時）

## 🚀 機能

- ✅ **マルチプラットフォーム対応**: Telegram, Slack, Discord
- ✅ **統一セッション管理**: クロスプラットフォームユーザーセッション追跡
- ✅ **リアルタイム同期**: WebSocket 双方向通信
- ✅ **AI Agent 統合**: AgentFlow マルチエージェント機能を活用
- ✅ **リッチテキスト対応**: Markdown, Embeds, Block Kit
- ✅ **松結合設計**: LLM プロバイダー自動検出

## 📋 アーキテクチャ

```
Message Platforms (Telegram/Slack/Discord)
           ↓
    Message Gateway (コアルーティング)
           ↓
    ChatBot Skill (セッション管理)
           ↓
    Agent/Coordinator (AI 処理)
           ↓
    WebSocket Hub → Frontend (Live Canvas)
```

## 🛠️ クイックスタート

### 1. 依存関係のインストール

```bash
# 基本依存関係
pip install -e ".[dev]"

# プラットフォーム依存関係（必要に応じてインストール）
pip install python-telegram-bot>=20.0  # Telegram
pip install slack-sdk>=3.0             # Slack
pip install discord.py>=2.0            # Discord
```

### 2. 環境変数の設定

```bash
# 設定ファイルをコピー
cp apps/messaging_hub/.env.example apps/messaging_hub/.env

# 設定を編集（最低1つの LLM + 1つのプラットフォームを設定）
vim apps/messaging_hub/.env
```

必須設定：
- **LLM Provider**: `OPENAI_API_KEY` または `ANTHROPIC_API_KEY`
- **最低1つのプラットフォーム**: `TELEGRAM_BOT_TOKEN` または `SLACK_BOT_TOKEN` または `DISCORD_BOT_TOKEN`

### 3. サービスの起動

```bash
# ローカル開発（ホットリロード有効）
# ポートは app_config.json から自動読み込み（8004）
python -m apps.messaging_hub.main --reload

# 本番起動（リロードなし）
python -m apps.messaging_hub.main
```

起動後のアクセス先：
- **API ドキュメント**: http://localhost:8004/docs
- **ヘルスチェック**: http://localhost:8004/health
- **WebSocket**: ws://localhost:8004/ws

## 🤖 プラットフォーム設定

### Telegram

1. **Bot の作成**:
   - [@BotFather](https://t.me/BotFather) にアクセス
   - `/newbot` を送信して新しい bot を作成
	   - Token を取得（BotFather が表示する文字列）

2. **Webhook の設定**（オプション、本番環境推奨）:
   ```bash
   curl -X POST https://api.telegram.org/bot<TOKEN>/setWebhook \
     -d url=https://your-domain.com/webhook/telegram
   ```

3. **またはポーリングモードを使用**（開発環境）:
   - コードは自動ポーリングをサポート、追加設定不要

### Slack

1. **Slack App の作成**:
   - https://api.slack.com/apps にアクセス
   - "Create New App" → "From scratch" をクリック

2. **OAuth & Permissions の設定**:
   - Bot Token Scopes を追加:
     - `chat:write`
     - `channels:read`
     - `im:read`
     - `users:read`
   - App を workspace にインストール
	   - Bot User OAuth Token をコピー（prefix: xoxb）

3. **Event Subscriptions の設定**:
   - Events を有効化
   - Request URL: `https://your-domain.com/webhook/slack`
   - bot events を購読:
     - `message.channels`
     - `message.im`

### Discord

1. **Discord Bot の作成**:
   - https://discord.com/developers/applications にアクセス
   - "New Application" をクリック
   - "Bot" タブに移動し、"Add Bot" をクリック

2. **Intents の設定**:
   - "Message Content Intent" を有効化
   - "Server Members Intent" を有効化

3. **Token の取得**:
   - Bot Token をコピー

4. **Bot をサーバーに招待**:
   - OAuth2 → URL Generator
   - Scopes: `bot`
   - Permissions: `Send Messages`, `Read Messages/View Channels`
   - 生成された URL をコピーしてアクセス

## 📡 API エンドポイント

### HTTP APIs

| エンドポイント | メソッド | 説明 |
|---------------|---------|------|
| `/` | GET | サービス情報 |
| `/health` | GET | ヘルスチェック + 統計 |
| `/platforms` | GET | 登録済みプラットフォーム一覧 |
| `/sessions` | GET | アクティブセッション一覧 |
| `/send` | POST | 直接メッセージ送信（管理用） |
| `/webhook/telegram` | POST | Telegram webhook |
| `/webhook/slack` | POST | Slack webhook |

### WebSocket

```javascript
// WebSocket に接続
const ws = new WebSocket('ws://localhost:8004/ws?client_id=user123');

// リアルタイムメッセージを受信
ws.onmessage = (event) => {
  const data = JSON.parse(event.data);
  console.log('Received:', data);
  // { type: 'assistant_message', session_id: '...', data: {...} }
};
```

## 🧪 テスト

### 手動テスト

1. **Telegram**: bot にメッセージを送信
   ```
   /start
   Hello, bot!
   ```

2. **Slack**: チャンネルまたは DM で bot を @メンション
   ```
   @YourBot hello
   ```

3. **Discord**: サーバーチャンネルでメッセージを送信
   ```
   !hello
   こんにちは、bot
   ```

### API を使用したテスト

```bash
# Telegram にメッセージを送信
curl -X POST http://localhost:8004/send \
  -H "Content-Type: application/json" \
  -d '{
    "platform": "telegram",
    "channel_id": "123456789",
    "text": "Hello from API!"
  }'

# アクティブセッションを確認
curl http://localhost:8004/sessions

# プラットフォーム状態を確認
curl http://localhost:8004/platforms
```

## 🎯 高度な使用方法

### カスタム Agent の追加

```python
from agentflow import ChatBotSkill
from agentflow.patterns.coordinator import AdaptiveCoordinator

# マルチエージェントコーディネーターを作成
coordinator = AdaptiveCoordinator(agents=[agent1, agent2])

# ChatBot に統合
chatbot = ChatBotSkill(coordinator=coordinator)

# ゲートウェイで使用
gateway = MessageGateway(hub, chatbot)
```

### RAG 機能の追加

```python
from agentflow.skills.rag import RAGSkill

# RAG skill を作成
rag = RAGSkill(knowledge_base_path="./data")

# ChatBot に統合
chatbot = ChatBotSkill(rag_skill=rag)
```

### セッションの永続化

```python
# ChatBotSkill を拡張してデータベースストレージを実装
from agentflow import get_db

class PersistentChatBot(ChatBotSkill):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.db = get_db()

    async def create_session(self, metadata=None):
        # データベースから読み込み
        # ...
```

## 📊 モニタリング

統計情報の確認：

```bash
curl http://localhost:8004/health
```

レスポンス：
```json
{
  "status": "healthy",
  "statistics": {
    "registered_channels": 3,
    "active_sessions": 15,
    "message_queues": 8,
    "platforms": ["telegram", "slack", "discord"]
  }
}
```

## 🔒 セキュリティ

1. **Webhook 検証**:
   - Slack: 署名を自動検証（`SLACK_SIGNING_SECRET` が必要）
   - Telegram: HTTPS + secret token の使用を推奨

2. **環境変数**:
   - `.env` ファイルをバージョン管理にコミットしないこと
   - 本番環境ではシークレット管理サービスを使用

3. **レート制限**:
   - レート制限の追加を検討（FastAPI middleware で実装可能）

## 🚢 デプロイ

### Docker

```dockerfile
FROM python:3.13-slim

WORKDIR /app
COPY . .

RUN pip install -e ".[dev]" && \
    pip install python-telegram-bot slack-sdk discord.py

# app_config.json のポート（8004）を自動使用
CMD ["python", "-m", "apps.messaging_hub.main"]
```

### 環境変数（本番環境）

```bash
# シークレット管理サービスを使用
# 例: AWS Secrets Manager / ECS Task Definition / GitHub Actions Secrets などで実行環境へ注入
```

## 🆚 Moltbot との比較

| 機能 | Moltbot | Messaging Hub |
|------|---------|---------------|
| プラットフォーム | 12+ (WhatsApp, iMessage 等) | 3 (拡張可能) |
| アーキテクチャ | Gateway 中心 | 8層クリーンアーキテクチャ |
| マルチエージェント | 基本ルーティング | 4パターン + 5エンジン |
| メモリ | 不明 | 3層システム |
| UI | Live Canvas | A2UI + React Studio |
| プロトコル | A2UI | MCP/A2A/AG-UI/A2UI/UCP |
| 音声 | ✅ (ElevenLabs) | 🔜 (計画中) |
| デバイスツール | ✅ (カメラ, 位置情報) | 🔜 (計画中) |
| ブラウザ制御 | ✅ (Playwright) | 🔜 (計画中) |

## 📝 ライセンス

MIT License - AgentFlow メイン README を参照

## 🤝 コントリビューション

貢献を歓迎します！AgentFlow のコントリビューションガイドに従ってください。

実装済み機能：

## 共有テスト env 自動生成

```bash
conda run -n agentflow python scripts/bootstrap_test_env.py --env-file .env
```

- `MESSAGING_HUB_API_KEY_ENV` / `MESSAGING_HUB_API_KEY` は上記で自動補完されます。
- 空値のみ補完し、既存の非空値は保持されます（`--force` 指定時のみ上書き）。

## 本番運用と多租户招待メール

- `contracts.auth` が有効なため、本番では API キーを Secret Manager から注入してください。
- 招待メールは機密最小化を徹底し、ログイン URL は別メールで送信してください。
- 詳細手順: `docs/internal/env-bootstrap-and-tenant-invite-security.md`
