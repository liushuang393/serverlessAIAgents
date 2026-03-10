# Messaging Hub - マルチプラットフォーム AI チャットボット

<!-- README_REQUIRED_SECTIONS_START -->

## 機能概要

Messaging Hub は、複数のメッセージプラットフォームを単一のゲートウェイで統合し、AI エージェントと人間オペレーターが連携して動作する **ビジネス向けマルチプラットフォーム AI チャットボット基盤**です。

主要な機能ブロック：

- **6 プラットフォーム統合**: Telegram・Slack・Discord・Microsoft Teams・WhatsApp・Signal に対応。共通のアダプターパターンで実装されており、環境変数にトークンを設定するだけで有効化できる。
- **統一セッション管理**: プラットフォームを横断してユーザーセッションを追跡。各会話の開始時刻・最終メッセージ・メッセージ数を一元管理する。
- **AI パーソナルアシスタント（`PersonalAssistantCoordinator`）**: 自然言語のタスク指示（例:「メールを整理して」「競合分析レポートを作成して」）を受け取り、意図を分類したうえで適切なスキルへ振り分け、summary・key_points・actions・risks を返す。
- **スキルシステム**: スキルの一覧・有効化・無効化・自然言語からの動的生成・実行が可能。実行はすべてトラッキングされ、成功/失敗・所要時間・コストが記録される。
- **承認ワークフロー（Human-in-the-Loop）**: ファイル整理などリスクの高い操作は、管理画面からの承認なしに実行されない。保留中・承認済み・却下の全履歴を永続化する。
- **実行トラッキング**: `RunStarted → StepStarted → ToolExecuted → EvidenceAdded → RunFinished` の標準イベントチェーンで、各スキル実行のライフサイクルを追跡・記録する。アーティファクト（生成物）とロールバックハンドルも保持する。
- **リアルタイム WebSocket ハブ**: 管理ダッシュボードとのリアルタイム同期。クライアントはチャンネル別にイベント購読し、メッセージ到着・実行状況の即時反映を受け取れる。
- **File Organizer**: ディレクトリ分析・重複ファイル検出・ルールベースのファイル整理を提供。`dry_run=false` の実際の整理実行には承認が必須。
- **sr_chat API**: 独自チャット UI 専用の会話管理 API。会話一覧・履歴・メッセージ投稿・更新・ファイルアップロード・エクスポートをサポートする。
- **会話エクスポート**: JSON・CSV・Markdown の 3 形式で会話履歴をエクスポート可能。

## 優位性

- **プラットフォーム無依存**: 6 プラットフォームへの対応を共通アダプター層で実現。新プラットフォーム追加時も中核ロジックの変更は不要。
- **承認ゲートによる安全運用**: 高リスク操作は必ず人間の承認を経る設計。`autonomous` / `approval_required` のセキュリティモードを `.env` で切り替え可能。
- **LLM プロバイダー疎結合**: OpenAI・Anthropic 等を `get_llm()` で抽象化し、プロバイダー切替時もアプリコードの変更が不要。
- **API キー自動ブートストラップ**: `.env` に API キーが設定されていれば、UI が `/api/admin-key` を通じて自動取得するため、初回セットアップでの手動入力が不要。
- **永続化と復元**: 承認リクエスト・実行イベントを起動時にストアから復元し、再起動後も運用状態を維持する。

## 技術アーキテクチャ

- **Channel Adapter**: 各プラットフォームの差異（Webhook 形式・Bot API 仕様）を吸収し、内部統一メッセージ形式に変換する。
- **Message Gateway**: 登録済みアダプターを管理し、受信メッセージのルーティングと送信を担う。プラットフォーム別のメッセージ数・最終活動時刻などの統計も保持する。
- **ChatBot Skill + PersonalAssistantCoordinator**: セッション管理と AI 応答生成の中核。意図ルーターが自然言語を分類し、テンプレートベースのタスクプランを構築する。
- **Skill Gateway + Approval Manager**: スキル呼び出しをインターセプトし、リスクレベルに応じて自動承認または人間承認へルーティングする。
- **Execution Tracker**: 実行イベントの開始・完了・エラーを追跡し、統計（成功率・平均所要時間・コスト）を提供する。
- **WebSocket Hub**: 管理ダッシュボードへのリアルタイムイベント配信を担う Pub/Sub ハブ。
- **FastAPI + Lifespan 管理**: アプリ起動時にストア初期化・承認状態復元・プラットフォーム登録・Discord バックグラウンドタスク起動を行う。シャットダウン時はすべてのリソースをクリーンアップする。

## アプリケーション階層

- **Channel Layer**: 各メッセージプラットフォームとの接続（Webhook / Bot API / 長時間接続）。
- **Gateway Layer**: メッセージの受信正規化・プラットフォーム間ルーティング・統計収集。
- **Session Layer**: ユーザーセッションの作成・追跡・会話履歴管理（sr_chat ストア）。
- **Intent Layer**: 自然言語の意図分類・テンプレートマッチング・タスクプラン構築。
- **Skill Layer**: スキル呼び出し・承認ゲート・実行トラッキング・アーティファクト記録。
- **Delivery Layer**: 返信送信・WebSocket ブロードキャスト・運用監視。
<!-- README_REQUIRED_SECTIONS_END -->

統一メッセージプラットフォームゲートウェイ。[moltbot](https://github.com/moltbot/moltbot) に類似した実装で、Telegram、Slack、Discord などのマルチプラットフォーム統合をサポート。

## Product Position

- `product_line`: `assistant`
- `surface_profile`: `business`
- `security_mode` 既定値: `approval_required`（明示指定なし時）

## 🚀 機能

### メッセージプラットフォーム統合

- ✅ **Telegram**: Webhook + Bot API。`/webhook/telegram` でメッセージ受信・応答送信
- ✅ **Slack**: イベント購読 + 署名検証。`/webhook/slack` で Bot Events を処理
- ✅ **Discord**: 長時間接続の Bot モード（バックグラウンドタスクで常時稼働）
- ✅ **Microsoft Teams**: Bot Framework Activity を処理（`TEAMS_APP_ID` / `TEAMS_APP_PASSWORD` で有効化）
- ✅ **WhatsApp**: Meta Business API 連携（Webhook 検証 + メッセージ処理）
- ✅ **Signal**: signal-cli-rest-api コールバックモードでの受信

### セッション・会話管理

- ✅ **統一セッション管理**: プラットフォーム横断でユーザーセッションを追跡。開始時刻・最終メッセージ・メッセージ数を一元管理
- ✅ **sr_chat 会話 API**: 独自チャット UI 専用の会話ストア。複数会話（`conversation_id`）を並行管理し、メッセージ投稿・更新・ファイルアップロードをサポート
- ✅ **会話エクスポート**: JSON・CSV・Markdown の 3 形式でエクスポート可能

### AI エージェント・スキル

- ✅ **PersonalAssistantCoordinator**: 自然言語タスク（メール整理・ファイル整理・調査・レポート作成など）を受け取り、意図分類 → スキル実行 → summary/key_points/actions/risks を返す
- ✅ **スキル管理 API**: スキルの一覧・有効化・無効化・自然言語からの動的生成・手動呼び出しが可能
- ✅ **ワークフロー管理**: 複数スキルを組み合わせたワークフローの一覧・状態管理
- ✅ **意図ルーター + テンプレート**: `email_organize` / `file_organize` / `system_optimize` / `research` / `competitor_analysis` / `report` など事前定義テンプレートによる高速タスクプラン構築

### 承認・安全運用（Human-in-the-Loop）

- ✅ **承認ワークフロー**: 高リスク操作（ファイル整理の実行など）は管理画面からの承認なしに実行されない。PENDING / APPROVED / REJECTED / AUTO_APPROVED の状態を永続化
- ✅ **承認 API**: 保留中の承認一覧取得・承認実行・拒否・承認履歴・統計の各エンドポイントを提供
- ✅ **セキュリティモード**: `approval_required`（既定）/ `autonomous` を `.env` の `SECURITY_MODE` で切り替え可能

### 実行トラッキング・監査

- ✅ **イベントライフサイクル記録**: `RunStarted → StepStarted → ToolExecuted → EvidenceAdded → RunFinished` の標準イベントチェーンで各スキル実行を追跡
- ✅ **アーティファクト管理**: スキル実行結果の生成物とロールバックハンドルをストアに保持
- ✅ **実行統計**: 成功率・平均所要時間・失敗理由・コスト推定値をリアルタイム集計

### File Organizer

- ✅ **ディレクトリ分析**: 指定パスの古いファイル・サイズ分布・種別統計を分析
- ✅ **重複ファイル検出**: コンテンツハッシュまたは名前ベースで重複を検出
- ✅ **ファイル整理実行**: ルールベースの整理を実行（`dry_run=true` でプレビュー可能。実際の実行は承認必須）

### 管理ダッシュボード（Admin UI）

- ✅ **リアルタイム同期**: WebSocket（`/ws`）経由で管理ダッシュボードへイベントをリアルタイム配信
- ✅ **API キー自動ブートストラップ**: `.env` に API キーが設定済みであれば UI が自動取得し、手動入力が不要
- ✅ **統合ダッシュボード**: 会話一覧・承認管理・実行タイムライン・File Organizer・スキル管理を統一 UI で提供

### その他

- ✅ **A2A AgentCard**: `/api/a2a/card` で Agent の能力・スキル情報を公開（他エージェントとの連携用）
- ✅ **CLI 診断提案**: 低信頼度のトラブルシュート要求に対し、読み取り専用の CLI 調査を提案し、確認後に実行
- ✅ **LLM プロバイダー疎結合**: `get_llm()` で OpenAI・Anthropic 等を自動検出・切り替え可能

## 📋 アーキテクチャ

```
Message Platforms
  Telegram / Slack / Discord / Teams / WhatsApp / Signal
           ↓  (Webhook / Bot API / 長時間接続)
    Channel Adapters  ─────────────────────────────
           ↓                                       |
    Message Gateway                          Admin REST API
    (ルーティング・統計収集)               /api/platforms, /api/sessions
           ↓                               /api/skills, /api/approvals
    ChatBot Skill                          /api/executions, /api/workflows
    (セッション管理・sr_chat ストア)       /api/file-organizer, etc.
           ↓                                       |
    PersonalAssistantCoordinator ─ Intent Router   |
    (意図分類 → タスクプラン構築)     ↓             |
           ↓                   Skill Templates     |
    Skill Gateway                                  |
    (スキル呼び出し・リスク評価)                   |
           ↓                                       |
    Approval Manager ─── ← 承認 API ──────────────┘
    (PENDING/APPROVED/REJECTED)
           ↓
    Execution Tracker
    (RunStarted→ToolExecuted→RunFinished)
           ↓
    WebSocket Hub ──→ Admin Dashboard (React UI)
    (リアルタイムイベント配信)    リアルタイム同期
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

- **Platform LLM 管理**: text / speech 系 model は `contracts.llm` で Platform catalog を参照
- **最低1つのプラットフォーム**: `TELEGRAM_BOT_TOKEN` または `SLACK_BOT_TOKEN` または `DISCORD_BOT_TOKEN`

補足:

- Provider / model / API Key の正本は `apps/platform` の `LLM Management` です。
- `OPENAI_API_KEY` / `ANTHROPIC_API_KEY` / `GEMINI_API_KEY` は Platform 未設定時の fallback としてのみ利用します。

### 3. サービスの起動

```bash
# ローカル開発（ホットリロード有効）
# ポートは app_config.json から自動読み込み（8004）
conda activate agentflow
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
- 拡張API: `/api/sr_chat/chat.update` `/api/sr_chat/files.upload` `/api/sr_chat/events.subscribe` `/api/sr_chat/export`
- デザイン: ガラス調 + 奥行き（3D感）を持つ独自 UI テーマを適用
- スキル運用: `/api/skills*` `/api/workflows*` と連携する Skills/Workflow 管理画面を提供
- 運用導線: 承認管理 `/approvals`、実行履歴 `/timeline`、File Organizer `/file-organizer` を含む統合ダッシュボード
- PWA インストール: ブラウザの「インストール」操作でローカルアプリ化可能（デスクトップ/Android/iOS）

PWA（インストール）手順:

- **Windows / macOS (Chrome/Edge)**:
  - `http://localhost:3001/` を開き、アドレスバー右側のインストールアイコンをクリック
  - または画面右上の「インストール」ボタンから実行
- **Android (Chrome)**:
  - ブラウザメニュー → `アプリをインストール` / `ホーム画面に追加`
- **iOS (Safari)**:
  - 共有メニュー → `ホーム画面に追加`
- 要件:
  - `localhost` または `https` 配信であること（開発時は `localhost` で可）
  - 初回アクセス後に manifest / service worker が読み込まれていること

依存関係ポリシー（admin_ui）:

- 依存の組み合わせは `package.json` を正本にし、`npm install` 後に `npm run build` で整合性を検証する。
- `@typescript-eslint/typescript-estree` 配下の `minimatch` は `overrides` で `^10.2.1` に固定して脆弱性影響を回避する。
- `npm install` で `EAI_AGAIN` が出る場合は DNS/ネットワーク要因なので、接続復旧後に再実行する。

## 📦 本番ビルド/発布（Platform に統一）

```bash
conda activate agentflow
python -m apps.platform.main publish ./apps/messaging_hub --target docker
```

発布時の補足（PWA）:

- `admin_ui` のビルド成果物に `manifest.webmanifest` / `sw.js` / `public/icons/*` が含まれる。
- 追加の publish 手順は不要（既存の `apps.platform.main publish` で同梱される）。

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
| `/api/sr_chat/export`                | GET      | sr_chat 履歴エクスポート           |

### File Organizer APIs（管理画面 `/file-organizer` で利用）

| エンドポイント                   | メソッド | 説明                                                     |
| -------------------------------- | -------- | -------------------------------------------------------- |
| `/api/file-organizer/analyze`    | POST     | ディレクトリ分析                                         |
| `/api/file-organizer/duplicates` | POST     | 重複ファイル検出                                         |
| `/api/file-organizer/organize`   | POST     | ファイル整理（`dry_run=false` は承認要求を返す場合あり） |

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

| 機能             | Moltbot                     | Messaging Hub                                                                |
| ---------------- | --------------------------- | ---------------------------------------------------------------------------- |
| プラットフォーム | 12+ (WhatsApp, iMessage 等) | 6 (Telegram/Slack/Discord/Teams/WhatsApp/Signal, アダプター追加可能)         |
| アーキテクチャ   | Gateway 中心                | Channel Adapter → Gateway → Skill → Approval → Tracking の多層構成           |
| AI エージェント  | 基本ルーティング            | PersonalAssistantCoordinator + Intent Router + Skill Gateway                 |
| 承認フロー       | 不明                        | ✅ Human-in-the-Loop 承認システム（PENDING/APPROVED/REJECTED を永続化）      |
| 実行トラッキング | 不明                        | ✅ イベントライフサイクル記録（RunStarted → RunFinished + アーティファクト） |
| スキル管理       | 不明                        | ✅ 一覧・有効化・無効化・自然言語生成・ワークフロー管理                      |
| ファイル操作     | 不明                        | ✅ File Organizer（分析・重複検出・整理、承認必須）                          |
| 会話ストア       | 不明                        | ✅ sr_chat API（複数会話並行管理・エクスポート対応）                         |
| セキュリティ     | 不明                        | ✅ `approval_required` / `autonomous` モード切替、API キー認証               |
| UI               | Live Canvas                 | Admin UI (React + Vite) + WebSocket リアルタイム同期                         |
| A2A 連携         | 不明                        | ✅ `/api/a2a/card` で AgentCard 公開（MCP/A2A 拡張 🔜）                      |
| 音声             | ✅ (ElevenLabs)             | 🔜 (計画中)                                                                  |
| デバイスツール   | ✅ (カメラ, 位置情報)       | 🔜 (計画中)                                                                  |
| ブラウザ制御     | ✅ (Playwright)             | 🔜 (計画中)                                                                  |

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
