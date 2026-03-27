# デプロイ規約

> **バージョン**: 1.0.0
> **適用範囲**: AgentFlow Framework 全デプロイメントプロセス
> **最終更新**: 2025-01-19

## 📋 目次

1. [デプロイ原則](#デプロイ原則)
2. [環境管理](#環境管理)
3. [コンテナ化](#コンテナ化)
4. [CI/CD パイプライン](#cicd-パイプライン)
5. [リリース管理](#リリース管理)
6. [ロールバック戦略](#ロールバック戦略)
7. [監視とログ](#監視とログ)

---

## 🎯 デプロイ原則

### 継続的デプロイメント

```yaml
# ✅ 正しい: 自動化されたデプロイプロセス
# .github/workflows/deploy.yml
name: Deploy to Production
on:
  push:
    branches: [main]
  workflow_dispatch:

jobs:
  deploy:
    if: github.ref == 'refs/heads/main'
    runs-on: ubuntu-latest
    environment: production

    steps:
      - name: Checkout code
        uses: actions/checkout@v3

      - name: Configure AWS credentials
        uses: aws-actions/configure-aws-credentials@v2
        with:
          aws-access-key-id: ${{ secrets.AWS_ACCESS_KEY_ID }}
          aws-secret-access-key: ${{ secrets.AWS_SECRET_ACCESS_KEY }}
          aws-region: us-east-1

      - name: Deploy to ECS
        run: |
          # 自動デプロイスクリプト実行
          ./scripts/deploy.sh production
```

### イミュータブルデプロイメント

```dockerfile
# ✅ 正しい: イミュータブルなコンテナイメージ
FROM python:3.13-slim

# セキュリティ更新
RUN apt-get update && apt-get upgrade -y && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# 作業ディレクトリ設定
WORKDIR /app

# 依存関係のコピーとインストール
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# アプリケーションコードのコピー
COPY agentflow/ ./agentflow/
COPY scripts/ ./scripts/

# 非rootユーザーでの実行
RUN useradd --create-home --shell /bin/bash app
RUN chown -R app:app /app
USER app

# ヘルスチェック
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:8000/health || exit 1

# 起動コマンド
CMD ["python", "-m", "agentflow.api"]
```

### ブルーグリーンデプロイメント

```bash
#!/bin/bash
# ✅ 正しい: ブルーグリーンデプロイメントスクリプト

set -e

ENVIRONMENT=$1
BLUE_SERVICE="agentflow-blue"
GREEN_SERVICE="agentflow-green"

# 現在のアクティブサービスを確認
ACTIVE_SERVICE=$(aws ecs describe-services \
    --cluster agentflow-cluster \
    --services $BLUE_SERVICE $GREEN_SERVICE \
    --query 'services[?runningCount>0].serviceName' \
    --output text)

if [ "$ACTIVE_SERVICE" = "$BLUE_SERVICE" ]; then
    TARGET_SERVICE=$GREEN_SERVICE
    STANDBY_SERVICE=$BLUE_SERVICE
else
    TARGET_SERVICE=$BLUE_SERVICE
    STANDBY_SERVICE=$GREEN_SERVICE
fi

echo "Deploying to $TARGET_SERVICE..."

# 新しいサービスを起動
aws ecs update-service \
    --cluster agentflow-cluster \
    --service $TARGET_SERVICE \
    --task-definition agentflow-$ENVIRONMENT \
    --desired-count 3

# ヘルスチェックを待機
echo "Waiting for health checks..."
aws ecs wait services-stable \
    --cluster agentflow-cluster \
    --services $TARGET_SERVICE

# トラフィックを切り替え
echo "Switching traffic..."
# Load Balancer のターゲットグループを更新
aws elbv2 modify-listener \
    --listener-arn $LISTENER_ARN \
    --default-actions Type=forward,TargetGroupArn=$TARGET_TG_ARN

# 古いサービスを停止
echo "Stopping old service..."
aws ecs update-service \
    --cluster agentflow-cluster \
    --service $STANDBY_SERVICE \
    --desired-count 0

echo "Deployment completed successfully!"
```

---

## 🌍 環境管理

### 環境設定管理

```python
# ✅ 正しい: 環境別設定管理
import os
from typing import Dict, Any
from pydantic import BaseSettings, Field

class BaseConfig(BaseSettings):
    """ベース設定クラス。"""

    # アプリケーション設定
    app_name: str = Field(default="AgentFlow")
    debug: bool = Field(default=False)
    log_level: str = Field(default="INFO")

    # データベース設定
    database_url: str = Field(...)

    # 外部サービス設定
    redis_url: str = Field(default="redis://localhost:6379")
    openai_api_key: str = Field(...)

    class Config:
        env_file = ".env"
        env_file_encoding = "utf-8"

class DevelopmentConfig(BaseConfig):
    """開発環境設定。"""

    debug: bool = True
    log_level: str = "DEBUG"

    # 開発環境のデフォルト値
    database_url: str = Field(default="postgresql://localhost/agentflow_dev")

class StagingConfig(BaseConfig):
    """ステージング環境設定。"""

    log_level: str = "WARNING"

    # ステージング環境のデフォルト値
    database_url: str = Field(default="postgresql://staging-db/agentflow_staging")

class ProductionConfig(BaseConfig):
    """本番環境設定。"""

    debug: bool = False
    log_level: str = "ERROR"

    # 本番環境では必須設定
    database_url: str = Field(...)

def get_config() -> BaseConfig:
    """環境に応じた設定を取得。"""
    env = os.getenv("ENVIRONMENT", "development").lower()

    config_map = {
        "development": DevelopmentConfig,
        "staging": StagingConfig,
        "production": ProductionConfig,
    }

    config_class = config_map.get(env, DevelopmentConfig)
    return config_class()

# 使用例
config = get_config()
print(f"Running in {config.app_name} mode")
```

### 環境変数の管理

```bash
# ✅ 正しい: .env ファイルの管理（バージョン管理対象外）
# .env.example（テンプレート）
APP_NAME=AgentFlow
DEBUG=false
LOG_LEVEL=INFO
DATABASE_URL=
REDIS_URL=redis://localhost:6379
OPENAI_API_KEY=
JWT_SECRET=

# .env.local（ローカル開発用、上書き）
DEBUG=true
LOG_LEVEL=DEBUG
DATABASE_URL=postgresql://localhost/agentflow_dev

# 本番環境では環境変数を直接設定
# AWS ECS タスク定義例
{
    "family": "agentflow-production",
    "containerDefinitions": [
        {
            "name": "agentflow",
            "image": "agentflow:latest",
            "environment": [
                {"name": "ENVIRONMENT", "value": "production"},
                {"name": "DATABASE_URL", "valueFrom": "arn:aws:secretsmanager:..."},
                {"name": "OPENAI_API_KEY", "valueFrom": "arn:aws:secretsmanager:..."}
            ],
            "secrets": [
                {
                    "name": "JWT_SECRET",
                    "valueFrom": "arn:aws:secretsmanager:region:account:secret:name"
                }
            ]
        }
    ]
}
```

### シークレット管理

```python
# ✅ 正しい: AWS Secrets Manager 統合
import boto3
from typing import Dict, Any
import json

class SecretsManager:
    """AWS Secrets Manager 統合クラス。"""

    def __init__(self, region_name: str = "us-east-1"):
        self.client = boto3.client("secretsmanager", region_name=region_name)

    async def get_secret(self, secret_name: str) -> Dict[str, Any]:
        """シークレットを取得。"""
        try:
            response = await self.client.get_secret_value(SecretId=secret_name)
            secret_string = response["SecretString"]
            return json.loads(secret_string)
        except Exception as e:
            logger.error(f"Failed to retrieve secret {secret_name}: {e}")
            raise

    async def get_database_credentials(self) -> Dict[str, str]:
        """データベース認証情報を取得。"""
        secrets = await self.get_secret("agentflow/database")
        return {
            "username": secrets["username"],
            "password": secrets["password"],
            "host": secrets["host"],
            "port": secrets["port"],
            "database": secrets["database"],
        }

class ConfigWithSecrets:
    """シークレットを含む設定管理。"""

    def __init__(self):
        self.secrets_manager = SecretsManager()

    async def load_config(self) -> Dict[str, Any]:
        """設定とシークレットを統合して読み込み。"""
        # 基本設定の読み込み
        config = get_config()

        # シークレットの読み込み
        if config.environment == "production":
            db_creds = await self.secrets_manager.get_database_credentials()
            api_keys = await self.secrets_manager.get_secret("agentflow/api-keys")

            # 設定に統合
            config.database_url = (
                f"postgresql://{db_creds['username']}:{db_creds['password']}"
                f"@{db_creds['host']}:{db_creds['port']}/{db_creds['database']}"
            )
            config.openai_api_key = api_keys["openai_api_key"]
            config.jwt_secret = api_keys["jwt_secret"]

        return config
```

---

## 🐳 コンテナ化

### Dockerfile のベストプラクティス

```dockerfile
# ✅ 正しい: マルチステージビルド
# Build stage
FROM python:3.13-slim as builder

WORKDIR /app

# 依存関係のインストール
COPY requirements.txt .
RUN pip install --user --no-cache-dir -r requirements.txt

# Production stage
FROM python:3.13-slim as production

# セキュリティ更新とクリーンアップ
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y curl && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# 非rootユーザー作成
RUN groupadd -r appuser && useradd -r -g appuser appuser

WORKDIR /app

# ビルド済み依存関係のコピー
COPY --from=builder /root/.local /home/appuser/.local
ENV PATH=/home/appuser/.local/bin:$PATH

# アプリケーションコードのコピー
COPY --chown=appuser:appuser agentflow/ ./agentflow/

# 権限設定
RUN chown -R appuser:appuser /app
USER appuser

# ポート公開
EXPOSE 8000

# ヘルスチェック
HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
    CMD curl -f http://localhost:8000/health || exit 1

# 起動コマンド
CMD ["python", "-m", "uvicorn", "agentflow.api:app", "--host", "0.0.0.0", "--port", "8000"]
```

### Docker Compose の設定

```yaml
# ✅ 正しい: マルチサービス構成
version: '3.8'

services:
  api:
    build:
      context: .
      dockerfile: Dockerfile
      target: production
    ports:
      - "8000:8000"
    environment:
      - ENVIRONMENT=development
      - DATABASE_URL=postgresql://user:pass@db:5432/agentflow
      - REDIS_URL=redis://redis:6379
    depends_on:
      db:
        condition: service_healthy
      redis:
        condition: service_healthy
    volumes:
      - ./logs:/app/logs
    restart: unless-stopped
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:8000/health"]
      interval: 30s
      timeout: 10s
      retries: 3

	  db:
	    image: postgres:15-alpine
	    environment:
	      - POSTGRES_DB=agentflow
	      - POSTGRES_USER=user
	      - POSTGRES_PASSWORD=
    volumes:
      - postgres_data:/var/lib/postgresql/data
      - ./scripts/init.sql:/docker-entrypoint-initdb.d/init.sql
    ports:
      - "5432:5432"
    restart: unless-stopped
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U user -d agentflow"]
      interval: 10s
      timeout: 5s
      retries: 5

  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"
    restart: unless-stopped
    healthcheck:
      test: ["CMD", "redis-cli", "ping"]
      interval: 10s
      timeout: 5s
      retries: 5

  worker:
    build:
      context: .
      dockerfile: Dockerfile.worker
    environment:
      - ENVIRONMENT=development
      - DATABASE_URL=postgresql://user:pass@db:5432/agentflow
      - REDIS_URL=redis://redis:6379
    depends_on:
      - db
      - redis
    restart: unless-stopped

volumes:
  postgres_data:

networks:
  default:
    driver: bridge
```

---

## 🔄 CI/CD パイプライン

### GitHub Actions ワークフロー

```yaml
# .github/workflows/ci-cd.yml
name: CI/CD Pipeline

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]

env:
  REGISTRY: ghcr.io
  IMAGE_NAME: ${{ github.repository }}

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Set up Python
        uses: actions/setup-python@v4
        with:
          python-version: "3.13"

      - name: Cache dependencies
        uses: actions/cache@v3
        with:
          path: ~/.cache/pip
          key: ${{ runner.os }}-pip-${{ hashFiles('**/requirements*.txt') }}

      - name: Install dependencies
        run: |
          python -m pip install --upgrade pip
          pip install -r requirements.txt -r requirements-dev.txt

      - name: Run tests
        run: |
          pytest --cov=agentflow --cov-report=xml

      - name: Upload coverage
        uses: codecov/codecov-action@v3
        with:
          file: ./coverage.xml

  build-and-push:
    needs: test
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'
    permissions:
      contents: read
      packages: write

    steps:
      - name: Checkout repository
        uses: actions/checkout@v3

      - name: Log in to Container Registry
        uses: docker/login-action@v2
        with:
          registry: ${{ env.REGISTRY }}
          username: ${{ github.actor }}
          password: ${{ secrets.GITHUB_TOKEN }}

      - name: Extract metadata
        id: meta
        uses: docker/metadata-action@v4
        with:
          images: ${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}
          tags: |
            type=ref,event=branch
            type=ref,event=pr
            type=sha
            type=raw,value=latest,enable={{is_default_branch}}

      - name: Build and push Docker image
        uses: docker/build-push-action@v4
        with:
          context: .
          push: true
          tags: ${{ steps.meta.outputs.tags }}
          labels: ${{ steps.meta.outputs.labels }}
          cache-from: type=gha
          cache-to: type=gha,mode=max

  deploy-staging:
    needs: build-and-push
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/develop'
    environment: staging

    steps:
      - name: Deploy to staging
        run: |
          echo "Deploying to staging environment"
          # ステージングデプロイスクリプト実行

  deploy-production:
    needs: build-and-push
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'
    environment: production

    steps:
      - name: Deploy to production
        run: |
          echo "Deploying to production environment"
          # 本番デプロイスクリプト実行
```

### アーキテクチャ検証ステージ (Architecture Validation)

**実行タイミング:** Lint 後、Type check 前
**ツール:**

- `python scripts/check_layer_boundaries.py` - 7層境界ルール
- `python scripts/check_no_direct_provider_calls.py` - プロバイダ隔離
- `python scripts/check_app_compliance.py` - App フレームワーク準拠
- `python scripts/check_rules_compliance.py` - ルール総合遵守

**ブロッキング条件:**

- レイヤー境界違反 > 25 (段階的に 0 へ削減)
- プロバイダ直接 import > 0 (即時ブロック)

**非ブロッキング (レポートのみ):**

- App コンプライアンス (改善追跡用)
- ルールコンプライアンス (トレンド監視)

### デプロイスクリプト

```bash
#!/bin/bash
# scripts/deploy.sh

set -e

ENVIRONMENT=$1
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_DIR="/backups/$TIMESTAMP"

echo "🚀 Deploying AgentFlow to $ENVIRONMENT environment..."

# 事前チェック
echo "📋 Running pre-deployment checks..."
./scripts/health-check.sh

# データベースバックアップ
echo "💾 Creating database backup..."
mkdir -p $BACKUP_DIR
pg_dump agentflow_db > $BACKUP_DIR/pre_deploy.sql

# ブルーグリーンデプロイメント
echo "🔄 Performing blue-green deployment..."
if [ "$ENVIRONMENT" = "production" ]; then
    ./scripts/blue-green-deploy.sh production
else
    ./scripts/rolling-deploy.sh $ENVIRONMENT
fi

# ヘルスチェック
echo "🏥 Running post-deployment health checks..."
timeout 300 ./scripts/wait-for-health.sh || {
    echo "❌ Health checks failed, initiating rollback..."
    ./scripts/rollback.sh $BACKUP_DIR
    exit 1
}

# クリーンアップ
echo "🧹 Cleaning up old deployments..."
./scripts/cleanup.sh

echo "✅ Deployment completed successfully!"
```

---

## 📦 リリース管理

### セマンティックバージョニング

```python
# ✅ 正しい: バージョン管理
# agentflow/__init__.py
__version__ = "1.2.3"  # MAJOR.MINOR.PATCH

# バージョン更新ルール:
# - MAJOR: 破壊的変更（APIの非互換）
# - MINOR: 新機能追加（後方互換）
# - PATCH: バグ修正（後方互換）

# リリーススクリプト例
def update_version(version_type: str):
    """バージョンを更新する。"""
    current = parse_version(__version__)

    if version_type == "major":
        new_version = f"{current.major + 1}.0.0"
    elif version_type == "minor":
        new_version = f"{current.major}.{current.minor + 1}.0"
    else:  # patch
        new_version = f"{current.major}.{current.minor}.{current.patch + 1}"

    # __init__.py を更新
    update_file("__init__.py", __version__, new_version)

    # 変更履歴更新
    update_changelog(new_version)

    # Git タグ作成
    run_command(f"git tag v{new_version}")
    run_command(f"git push origin v{new_version}")
```

### リリースノート生成

````python
# ✅ 正しい: 自動リリースノート生成
# scripts/generate-release-notes.py
import re
from pathlib import Path

def generate_release_notes(version: str) -> str:
    """リリースノートを生成。"""
    changelog_path = Path("CHANGELOG.md")
    content = changelog_path.read_text()

    # 指定バージョンの変更履歴を抽出
    pattern = rf"## \[{version}\].*?(?=## \[[0-9]+\.[0-9]+\.[0-9]+\]|$)"
    match = re.search(pattern, content, re.DOTALL)

    if not match:
        raise ValueError(f"Version {version} not found in changelog")

    notes = match.group(0).strip()

    # GitHub リリース形式に変換
    return f"""# Release {version}

{notes}

## Installation

```bash
pip install agentflow=={version}
````

## Docker

```bash
docker pull ghcr.io/liushuang393/serverlessaiaagents:{version}
```

"""

# 使用例

if **name** == "**main**":
notes = generate_release_notes("1.2.3")
print(notes)

````

### リリースチェックリスト
```markdown
# リリースチェックリスト v1.2.3

## 事前確認
- [ ] すべてのテストが通過
- [ ] カバレッジ 80% 以上
- [ ] セキュリティスキャン通過
- [ ] パフォーマンステスト通過
- [ ] CHANGELOG.md 更新済み
- [ ] ドキュメント更新済み
- [ ] 移行ガイド作成済み（破壊的変更の場合）

## リリース準備
- [ ] バージョン番号更新（agentflow/__init__.py）
- [ ] Git タグ作成（v1.2.3）
- [ ] Docker イメージビルド
- [ ] PyPI パッケージビルド

## リリース実行
- [ ] GitHub Release 作成
- [ ] PyPI パッケージ公開
- [ ] Docker イメージ公開
- [ ] ドキュメントサイト更新

## リリース後
- [ ] ステージング環境デプロイ
- [ ] 本番環境デプロイ（段階的）
- [ ] 監視ダッシュボード確認
- [ ] ユーザー通知（該当する場合）
- [ ] 問題発生時のロールバック準備
````

---

## 🔄 ロールバック戦略

### 自動ロールバック

```bash
#!/bin/bash
# scripts/rollback.sh

set -e

BACKUP_TIMESTAMP=$1
ENVIRONMENT=${2:-production}

echo "🔄 Rolling back to $BACKUP_TIMESTAMP in $ENVIRONMENT..."

# データベース復元
echo "💾 Restoring database..."
psql agentflow_db < /backups/$BACKUP_TIMESTAMP/pre_deploy.sql

# 以前のコンテナイメージに切り戻し
echo "🐳 Rolling back container image..."
PREVIOUS_IMAGE=$(get_previous_image_tag)
kubectl set image deployment/agentflow agentflow=$PREVIOUS_IMAGE

# 設定復元
echo "⚙️ Restoring configuration..."
cp /backups/$BACKUP_TIMESTAMP/config.yaml /etc/agentflow/config.yaml

# サービス再起動
echo "🔄 Restarting services..."
kubectl rollout restart deployment/agentflow

# ヘルスチェック
echo "🏥 Waiting for health checks..."
timeout 300 ./scripts/wait-for-health.sh

echo "✅ Rollback completed successfully!"
```

### 段階的ロールバック

```python
# ✅ 正しい: カナリアロールバック
class RollingRollback:
    """段階的ロールバッククラス。"""

    def __init__(self, k8s_client, deployment_name: str):
        self.k8s = k8s_client
        self.deployment_name = deployment_name

    async def gradual_rollback(self, steps: int = 5, delay: int = 60):
        """段階的にロールバックを実行。"""
        # 現在のレプリカ数を取得
        current_replicas = await self._get_current_replicas()

        # 段階的に古いバージョンに戻す
        old_image = await self._get_previous_image()

        for i in range(steps):
            # 新しいバージョンとの比率を計算
            new_ratio = (steps - i - 1) / steps
            old_ratio = (i + 1) / steps

            # トラフィックを調整
            await self._update_traffic_split(new_ratio, old_ratio)

            # ヘルスチェック
            if not await self._run_health_checks():
                # 問題が発生したら停止
                await self._emergency_rollback()
                raise RollbackFailedError("Health checks failed during rollback")

            # 待機
            await asyncio.sleep(delay)

        # 完全ロールバック
        await self._complete_rollback(old_image)

    async def _update_traffic_split(self, new_ratio: float, old_ratio: float):
        """トラフィック分割を更新。"""
        # Istio またはサービスメッシュ経由でトラフィック制御
        pass

    async def _run_health_checks(self) -> bool:
        """ヘルスチェックを実行。"""
        # エラー率、レスポンスタイム、成功率をチェック
        pass
```

---

## 📊 監視とログ

### アプリケーションメトリクス

```python
# ✅ 正しい: Prometheus メトリクス統合
from prometheus_client import Counter, Histogram, Gauge, generate_latest
from fastapi import FastAPI, Response
from fastapi.middleware.base import BaseHTTPMiddleware

app = FastAPI()

# メトリクス定義
REQUEST_COUNT = Counter(
    'http_requests_total',
    'Total HTTP requests',
    ['method', 'endpoint', 'status_code']
)

REQUEST_LATENCY = Histogram(
    'http_request_duration_seconds',
    'HTTP request latency',
    ['method', 'endpoint']
)

ACTIVE_CONNECTIONS = Gauge(
    'active_connections',
    'Number of active connections'
)

class MetricsMiddleware(BaseHTTPMiddleware):
    """メトリクス収集ミドルウェア。"""

    async def dispatch(self, request, call_next):
        method = request.method
        endpoint = request.url.path

        with REQUEST_LATENCY.labels(method=method, endpoint=endpoint).time():
            ACTIVE_CONNECTIONS.inc()
            try:
                response = await call_next(request)
                REQUEST_COUNT.labels(
                    method=method,
                    endpoint=endpoint,
                    status_code=response.status_code
                ).inc()
                return response
            finally:
                ACTIVE_CONNECTIONS.dec()

@app.get("/metrics")
async def metrics():
    """Prometheus メトリクスエンドポイント。"""
    return Response(
        generate_latest(),
        media_type="text/plain; charset=utf-8"
    )
```

### 構造化ログ

```python
# ✅ 正しい: JSON 形式の構造化ログ
import logging
import json
from typing import Dict, Any
from datetime import datetime

class StructuredLogger:
    """構造化ロガー。"""

    def __init__(self, name: str):
        self.logger = logging.getLogger(name)
        self.logger.setLevel(logging.INFO)

        # JSON フォーマッタ
        formatter = logging.Formatter(
            '{"timestamp": "%(asctime)s", "level": "%(levelname)s", '
            '"logger": "%(name)s", "message": "%(message)s"}'
        )

        # ハンドラー設定
        handler = logging.StreamHandler()
        handler.setFormatter(formatter)
        self.logger.addHandler(handler)

    def log_request(self, request_id: str, method: str, url: str, status: int, duration: float):
        """HTTP リクエストログ。"""
        self.logger.info(
            "Request processed",
            extra={
                "request_id": request_id,
                "method": method,
                "url": url,
                "status": status,
                "duration": duration,
                "event_type": "http_request"
            }
        )

    def log_error(self, error: Exception, context: Dict[str, Any]):
        """エラーログ。"""
        self.logger.error(
            f"Error occurred: {error}",
            extra={
                "error_type": type(error).__name__,
                "error_message": str(error),
                "context": context,
                "event_type": "error"
            }
        )

    def log_workflow_execution(self, workflow_id: str, status: str, metrics: Dict[str, Any]):
        """ワークフロー実行ログ。"""
        self.logger.info(
            f"Workflow {workflow_id} {status}",
            extra={
                "workflow_id": workflow_id,
                "status": status,
                "metrics": metrics,
                "event_type": "workflow_execution"
            }
        )

# 使用例
logger = StructuredLogger("agentflow.api")

# リクエストログ
logger.log_request(
    request_id="req-123",
    method="POST",
    url="/api/workflows",
    status=200,
    duration=0.145
)

# エラーログ
try:
    # 何らかの処理
    pass
except Exception as e:
    logger.log_error(e, {"user_id": "user123", "action": "create_workflow"})
```

### ログ集約と分析

```yaml
# ✅ 正しい: ELK Stack 設定例
# docker-compose.logging.yml
version: "3.8"

services:
  elasticsearch:
    image: elasticsearch:8.5.0
    environment:
      - discovery.type=single-node
      - xpack.security.enabled=false
    ports:
      - "9200:9200"
    volumes:
      - elasticsearch_data:/usr/share/elasticsearch/data

  logstash:
    image: logstash:8.5.0
    ports:
      - "5044:5044"
    volumes:
      - ./config/logstash.conf:/usr/share/logstash/pipeline/logstash.conf
    depends_on:
      - elasticsearch

  kibana:
    image: kibana:8.5.0
    ports:
      - "5601:5601"
    depends_on:
      - elasticsearch

volumes:
  elasticsearch_data:
```

---

## ✅ デプロイチェックリスト

### デプロイ前チェックリスト

- [ ] すべてのテストが通過
- [ ] カバレッジ 80% 以上
- [ ] セキュリティスキャン通過
- [ ] パフォーマンステスト通過
- [ ] ドキュメント更新済み
- [ ] 移行スクリプト作成済み
- [ ] ロールバックプラン策定済み

### デプロイ中チェックリスト

- [ ] データベースバックアップ取得
- [ ] ブルーグリーンデプロイメント実行
- [ ] ヘルスチェック通過
- [ ] 監視ダッシュボード確認
- [ ] ログ集約正常動作

### デプロイ後チェックリスト

- [ ] 機能テスト実行
- [ ] パフォーマンス監視
- [ ] エラーレート確認
- [ ] ユーザー影響評価
- [ ] ドキュメント更新

---

## 📚 関連ドキュメント

- [**CI/CD パイプライン**](ci-cd-pipeline.md) - 継続的インテグレーションの詳細
- [**監視アラート**](monitoring-alerting.md) - 運用監視のベストプラクティス
- [**セキュリティ規約**](security-standards.md) - 安全なデプロイメント
- [**リリース管理**](release-management.md) - バージョン管理とリリースプロセス

---

**信頼性が高く、安全なデプロイメントが成功の鍵です。** 🚀

_最終更新: 2025-01-19 | バージョン: 1.0.0_
