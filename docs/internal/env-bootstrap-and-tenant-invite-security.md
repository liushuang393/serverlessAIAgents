# Env Bootstrap / Tenant 招待セキュリティ運用

このドキュメントは、8 app 共通の運用ルールを定義する。

## 1. 共有テスト環境の env 自動生成

手動で API キーやシークレットを作らず、以下を実行する。

```bash
conda run -n agentflow python scripts/bootstrap_test_env.py --env-file .env
```

既定動作:
- ルート `.env` を更新する
- `apps/*/.env.example` が存在する app では `apps/*/.env` も更新する
- 既存の非空値は保持し、空値のみ自動補完する（`--force` 指定時のみ上書き）

主な自動補完対象:
- `MESSAGING_HUB_API_KEY(_ENV)`
- `CODE_MIGRATION_API_KEY(_ENV)`
- `DESIGN_SKILLS_API_KEY(_ENV)`
- `JWT_SECRET_KEY`
- `FAQ_PROXY_AUTH_SHARED_SECRET`
- `POSTGRES_PASSWORD`
- `SESSION_SECRET`
- `LLM_PROVIDER=mock`（共通テスト向け）

## 2. 本番環境のシークレット設定手順

1. シークレットは Secrets Manager / Vault / KMS で作成し、Git に保存しない。
2. 本番デプロイ時に実行環境へ注入する（`.env` 配布は禁止）。
3. `app_config.json` の `contracts.auth` が `enabled=true` かつ `allow_anonymous=false` の app は、対応 API キーを必ず注入する。
4. DB 初期化前に以下を完了する。
   - マイグレーション適用
   - 最小権限ロール作成
   - 監査ログ保存先の有効化
5. キーは定期ローテーションし、失効手順を運用手順書に明記する。

## 3. DB 初期化とテナント初期化の原則

1. DB 初期化時に、テナントごとの分離情報（tenant_id, role, policy）を先に作成する。
2. 招待トークンはワンタイム・短寿命（推奨 10〜15 分）で発行する。
3. トークンは平文保存しない（ハッシュ化して保存する）。
4. 監査ログには `tenant_id` と `request_id` を残し、token / password / secret は記録しない。

## 4. 多租户招待メールの安全ルール（必須）

1. 1通目（招待通知メール）にはログイン URL を入れない。
2. 1通目には最小情報のみ記載する。
   - サービス名
   - 招待された事実
   - 有効期限
   - サポート連絡先
3. ログイン URL が必要な場合、2通目として分離送信する。
4. URL は以下を満たす。
   - ワンタイムトークンのみ（メールアドレス、tenant_id、内部IDを埋め込まない）
   - HTTPS 必須
   - 短寿命 + 1回使用で失効
5. 高リスク環境では、URL ではなく OTP/認可コード方式を優先する。

## 5. 参考運用フロー

1. `bootstrap_test_env.py` で検証環境を作成
2. app ごとに起動・疎通確認
3. 本番は Secret Manager 注入に切替
4. DB 初期化 + テナント初期化
5. 招待メールは「通知」と「URL」を分離送信

## 6. Platform 招待 API (実装済み)

- `POST /api/studios/framework/tenants/invitations`
  - 1通目の通知メールを送信
  - `auto_send_challenge=true` の場合は 2通目も送信
- `POST /api/studios/framework/tenants/invitations/{invitation_id}/challenge`
  - 2通目 (OTP またはリンク) を別送
- `POST /api/studios/framework/tenants/invitations/consume`
  - ワンタイム URL / OTP を検証して消費
- `GET /api/studios/framework/tenants/invitations/outbox` (開発限定)
  - `PLATFORM_INVITE_ENABLE_OUTBOX_ENDPOINT=true` のときのみ有効

### 関連環境変数

- `PLATFORM_INVITE_EMAIL_TRANSPORT` (`memory` / `smtp`)
- `PLATFORM_INVITE_SMTP_HOST`
- `PLATFORM_INVITE_SMTP_PORT`
- `PLATFORM_INVITE_SMTP_USERNAME`
- `PLATFORM_INVITE_SMTP_PASSWORD`
- `PLATFORM_INVITE_SMTP_STARTTLS`
- `PLATFORM_INVITE_FROM_EMAIL`
- `PLATFORM_INVITE_LOGIN_BASE_URL`
- `PLATFORM_INVITE_TTL_MINUTES`
- `PLATFORM_INVITE_SUPPORT_CONTACT`
- `PLATFORM_INVITE_TOKEN_PEPPER`
- `PLATFORM_INVITE_ENABLE_OUTBOX_ENDPOINT`
