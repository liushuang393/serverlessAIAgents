# AI-Blocks 本番リリースガイド

このディレクトリには、AI-Blocksプロジェクトの本番環境へのリリースに必要なスクリプトと設定ファイルが含まれています。

## 📁 ファイル構成

```
release/
├── deploy.sh          # Linux/macOS用リリーススクリプト
├── deploy.bat         # Windows用リリーススクリプト
└── README.md          # このファイル

プロジェクトルート/
├── docker-compose.prod.yml    # 本番用Docker Compose設定
├── .env.production.template   # 本番環境変数テンプレート
└── 本番リリース.md            # 詳細なリリース手順
```

## 🚀 クイックスタート

### 1. 環境設定

```bash
# 環境変数ファイルを作成
cp .env.production.template .env.production

# 実際の値を設定
nano .env.production
```

### 2. リリース実行

#### Linux/macOS の場合:
```bash
# スクリプトに実行権限を付与
chmod +x release/deploy.sh

# 完全リリース実行
./release/deploy.sh full-release

# バージョン指定リリース
./release/deploy.sh -v 0.2.0 full-release
```

#### Windows の場合:
```cmd
# 完全リリース実行
release\deploy.bat full-release

# バージョン指定リリース
release\deploy.bat /v 0.2.0 full-release
```

## 📋 利用可能なコマンド

### 基本コマンド

| コマンド | 説明 |
|----------|------|
| `build` | パッケージをビルド |
| `test` | 全テストを実行 |
| `docker-build` | Dockerイメージをビルド |
| `docker-deploy` | Docker本番デプロイ |
| `pypi-upload` | PyPIにアップロード |
| `full-release` | 完全リリース（全工程実行） |
| `health-check` | ヘルスチェック実行 |
| `rollback` | 前バージョンにロールバック |

### オプション

| オプション | 説明 |
|------------|------|
| `-v, /v VERSION` | 新しいバージョンを指定 |
| `-t, /test-only` | テスト環境のみ |
| `--skip-tests, /skip-tests` | テストをスキップ |
| `--force, /force` | 強制実行 |
| `-h, /help` | ヘルプを表示 |

## 🔧 段階的リリース手順

### 1. 開発完了後のテスト

```bash
# テストのみ実行
./release/deploy.sh test
```

### 2. パッケージビルド

```bash
# パッケージビルド
./release/deploy.sh build
```

### 3. Dockerイメージ作成

```bash
# Dockerイメージビルド
./release/deploy.sh docker-build
```

### 4. テスト環境デプロイ

```bash
# テスト環境のみリリース
./release/deploy.sh -t full-release
```

### 5. 本番環境デプロイ

```bash
# 本番環境リリース
./release/deploy.sh full-release
```

### 6. ヘルスチェック

```bash
# デプロイ後の確認
./release/deploy.sh health-check
```

## 🛡️ セキュリティ考慮事項

### 環境変数の管理

- `.env.production` ファイルは絶対にGitにコミットしないでください
- 本番用のAPIキーとパスワードは安全に管理してください
- 定期的にシークレットをローテーションしてください

### アクセス制御

- 本番環境へのアクセスは最小限の権限で行ってください
- デプロイスクリプトの実行権限を適切に設定してください
- ログファイルのアクセス権限を確認してください

## 📊 監視とログ

### 利用可能な監視ツール

- **Prometheus**: メトリクス収集 (http://localhost:9090)
- **Grafana**: ダッシュボード (http://localhost:3000)
- **Jaeger**: 分散トレーシング (http://localhost:16686)

### ログの確認

```bash
# アプリケーションログ
docker-compose -f docker-compose.prod.yml logs -f ai-blocks

# 全サービスのログ
docker-compose -f docker-compose.prod.yml logs -f

# 特定の時間範囲のログ
docker-compose -f docker-compose.prod.yml logs --since="2024-01-01T00:00:00" ai-blocks
```

## 🔄 ロールバック手順

問題が発生した場合のロールバック:

```bash
# 自動ロールバック
./release/deploy.sh rollback

# 手動ロールバック（特定バージョン）
docker tag aiblocks/ai-blocks:0.1.0 aiblocks/ai-blocks:latest
docker-compose -f docker-compose.prod.yml up -d
```

## 🚨 トラブルシューティング

### よくある問題と解決方法

#### 1. Docker コンテナが起動しない

```bash
# ログを確認
docker-compose -f docker-compose.prod.yml logs ai-blocks

# コンテナの状態確認
docker-compose -f docker-compose.prod.yml ps
```

#### 2. ヘルスチェックが失敗する

```bash
# 手動でヘルスチェック
curl http://localhost:8000/health

# アプリケーションの状態確認
docker exec -it ai-blocks-prod python -c "import ai_blocks; print('OK')"
```

#### 3. データベース接続エラー

```bash
# PostgreSQL接続確認
docker exec -it ai-blocks-postgres psql -U aiblocks -d aiblocks -c "SELECT 1;"

# Redis接続確認
docker exec -it ai-blocks-redis redis-cli ping
```

#### 4. パフォーマンス問題

```bash
# リソース使用量確認
docker stats

# メトリクス確認
curl http://localhost:9090/metrics
```

## 📞 サポート

問題が発生した場合は、以下の情報を含めてサポートに連絡してください:

1. エラーメッセージ
2. ログファイル
3. 実行したコマンド
4. 環境情報（OS、Dockerバージョンなど）

## 📝 変更履歴

- v0.1.0: 初期リリース
- 今後のバージョンアップ情報はここに記載されます
