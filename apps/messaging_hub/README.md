# Messaging Hub - マルチプラットフォーム AI チャットボット

<!-- README_REQUIRED_SECTIONS_START -->

## 機能概要

- Telegram / Slack / Discord を統合し、単一ボット基盤で運用可能。
- セッション管理とチャネル変換を標準化し、チャット処理を共通化。
- WebSocket 連携でリアルタイム配信とオペレーション監視を実現。

## 優位性

- マルチチャネル拡張時も中核ロジックを再利用でき、実装コストが低い。
- LLM/Agent 層を疎結合化し、プロバイダー切替に強い。
- 監査・セキュリティ運用を前提にした business 向け設計。

## 技術アーキテクチャ

- Channel Adapter → Message Gateway → ChatBot Skill → Agent 層の直列構成。
- FastAPI API と WebSocket Hub を同居させ、運用統制を単純化。
- AgentFlow マルチエージェント機能を応答生成・制御に適用。

## アプリケーション階層

- Channel Layer: 各メッセージプラットフォーム接続。
- Routing Layer: 受信正規化・意図分配・セッション統合。
- Agent Layer: 応答生成・ツール実行・調停。
- Delivery Layer: 返信送信・モニタリング・監査。
<!-- README_REQUIRED_SECTIONS_END -->

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

## 🛠️ 開発環境（インストール: 統一手順）

この app 単体ではなく、リポジトリ全体の開発環境をセットアップします。

```bash
cd <repo-root>
bash setup_dev.sh
```

手動で行う場合:

```bash
conda activate agentflow
pip install -e ".[dev,apps]"
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

### 4. 管理UI（独自チャット画面）

```bash
cd apps/messaging_hub/admin_ui
npm install
npm run dev
```

- 画面: `http://localhost:3001/conversations`
- 内容: `sr_chat` API 連携の会話一覧・履歴表示・送信（assistant応答取得）
- 主要API: `/api/sr_chat/conversations.list` `/api/sr_chat/conversations.history` `/api/sr_chat/chat.postMessage`
- デザイン: ガラス調 + 奥行き（3D感）を持つ独自 UI テーマを適用
- スキル運用: `/api/skills*` `/api/workflows*` と連携する Skills/Workflow 管理画面を提供

依存関係ポリシー（admin_ui）:

- 依存の組み合わせは `package.json` を正本にし、`npm install` 後に `npm run build` で整合性を検証する。
- `@typescript-eslint/typescript-estree` 配下の `minimatch` は `overrides` で `^10.2.1` に固定して脆弱性影響を回避する。
- `npm install` で `EAI_AGAIN` が出る場合は DNS/ネットワーク要因なので、接続復旧後に再実行する。

## 📦 本番ビルド/発布（Platform に統一）

```bash
conda activate agentflow
python -m apps.platform.main publish ./apps/messaging_hub --target docker
```

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

| エンドポイント           | メソッド | 説明                                                      |
| ------------------------ | -------- | --------------------------------------------------------- |
| `/`                      | GET      | サービス情報                                              |
| `/health`                | GET      | ヘルスチェック + 統計                                     |
| `/platforms`             | GET      | 登録済みプラットフォーム一覧                              |
| `/sessions`              | GET      | アクティブセッション一覧                                  |
| `/send`                  | POST     | 直接メッセージ送信（管理用）                              |
| `/webhook/telegram`      | POST     | Telegram webhook                                          |
| `/webhook/slack`         | POST     | Slack webhook                                             |
| `/assistant/process`     | POST     | 主管アシスタント処理（低信頼トラブル時は CLI 提案を返す） |
| `/assistant/cli/execute` | POST     | 提案済み CLI 調査の承認実行（`confirm=true` 必須）        |

### sr_chat APIs（独自チャット画面で利用）

| エンドポイント                       | メソッド | 説明                               |
| ------------------------------------ | -------- | ---------------------------------- |
| `/api/sr_chat/auth.test`             | POST     | 認証状態確認                       |
| `/api/sr_chat/conversations.list`    | GET      | 会話一覧                           |
| `/api/sr_chat/conversations.history` | GET      | 会話履歴                           |
| `/api/sr_chat/chat.postMessage`      | POST     | メッセージ送信 + assistant応答生成 |
| `/api/sr_chat/chat.update`           | POST     | メッセージ更新                     |
| `/api/sr_chat/files.upload`          | POST     | ファイルアップロード記録           |
| `/api/sr_chat/events.subscribe`      | POST     | イベント購読登録                   |

### WebSocket

```javascript
// WebSocket に接続
const ws = new WebSocket("ws://localhost:8004/ws?client_id=user123");

// リアルタイムメッセージを受信
ws.onmessage = (event) => {
  const data = JSON.parse(event.data);
  console.log("Received:", data);
  // { type: 'assistant_message', session_id: '...', data: {...} }
};
```

## 🧭 CLI 提案ルーティング（最小接続）

Messaging Hub は「わからないトラブルシュート要求」を直接断定せず、CLI 調査提案へルーティングする。

### 1) 提案生成

`POST /assistant/process`

- 条件: `unknown` または低信頼意図（< 0.45）かつトラブルシュート系キーワード検出
- 応答: `needs_cli_confirmation=true` と `cli_proposal` を返却

例:

```json
{
  "ok": true,
  "summary": "CLI 調査提案",
  "needs_cli_confirmation": true,
  "cli_proposal": {
    "proposal_id": "uuid",
    "tool_candidates": ["codex", "claude"],
    "mode": "read_only",
    "prompt": "Investigate this issue in read-only mode ...",
    "rationale": "intent confidence is low/unknown and troubleshooting intent is detected"
  }
}
```

### 2) 承認実行

`POST /assistant/cli/execute`

```json
{
  "proposal_id": "uuid",
  "confirm": true
}
```

実行仕様:

- `confirm=true` がない場合は拒否（`confirmation_required`）
- 実行は `read_only` 診断モード固定
- 戻り値は `tool/command/summary/raw_output/error` を含む
- 自動コード改変は行わない（提案と調査結果のみ返す）

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
4. **CLI 実行境界**:
   - `assistant/cli/execute` は提案ID + 明示確認の二段階を必須化
   - 既定モードは `read_only` で、環境調査と修復提案のみに限定

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

| 機能               | Moltbot                     | Messaging Hub             |
| ------------------ | --------------------------- | ------------------------- |
| プラットフォーム   | 12+ (WhatsApp, iMessage 等) | 3 (拡張可能)              |
| アーキテクチャ     | Gateway 中心                | 8層クリーンアーキテクチャ |
| マルチエージェント | 基本ルーティング            | 4パターン + 5エンジン     |
| メモリ             | 不明                        | 3層システム               |
| UI                 | Live Canvas                 | A2UI + React Studio       |
| プロトコル         | A2UI                        | MCP/A2A/AG-UI/A2UI/UCP    |
| 音声               | ✅ (ElevenLabs)             | 🔜 (計画中)               |
| デバイスツール     | ✅ (カメラ, 位置情報)       | 🔜 (計画中)               |
| ブラウザ制御       | ✅ (Playwright)             | 🔜 (計画中)               |

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
