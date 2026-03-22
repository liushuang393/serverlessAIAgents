# Developer Studio (デベロッパースタジオ)

<!-- README_REQUIRED_SECTIONS_START -->

## 機能概要

- コード解析（code_intelligence）、コード生成（codegen）、Agent ウィザード（wizard）を提供する開発支援ツール。
- 独立したバックエンド API サーバーとして動作し、フロントエンドは持たない。
- FastAPI ベースの REST API で各機能を公開。

## 優位性

- 軽量な単一サーバー構成で即座に起動可能。
- コード解析・生成・Agent 構築を一箇所に集約し、開発者の作業効率を向上。
- AgentFlow プラットフォームの他 app と連携して拡張可能。

## 技術アーキテクチャ

- FastAPI ベースの REST API サーバー (Port 8011)。
- 3 つのサブモジュール構成: `code_intelligence` / `codegen` / `wizard`。
- Docker Compose による独立デプロイに対応。

## アプリケーション階層

- API Layer: `/health` ヘルスチェック、各機能エンドポイント。
- Code Intelligence: コード解析・依存関係分析。
- Codegen: AI を活用したコード生成。
- Wizard: Agent 構築ウィザード。

<!-- README_REQUIRED_SECTIONS_END -->

`product_line`: `framework` / `surface_profile`: `developer`

**※ 本 app はバックエンド専用サービスです（フロントエンドなし）。**

## 開発環境（インストール: 統一手順）

```bash
cd <repo-root>
bash setup_dev.sh
```

## ローカル起動

```bash
# バックエンド（ホットリロード付き）
conda run -n agentflow uvicorn apps.dev_studio.main:app --host 0.0.0.0 --port 8011 --reload
```

ヘルスチェック: `http://localhost:8011/health`

## Docker 起動

```bash
cd apps/dev_studio
docker compose up -d --build
```

停止:

```bash
docker compose down
```

## Platform 主導 Publish

```bash
conda run -n agentflow python -m control_plane.main publish ./apps/dev_studio --target docker
```

## テスト/静的チェック（統一スクリプト）

```bash
cd <repo-root>
./check.sh test
./check.sh lint
./check.sh type-check
```

## Endpoints

| エンドポイント | メソッド | 説明 |
| -------------- | -------- | ---- |
| `/` | GET | サービス情報 |
| `/health` | GET | ヘルスチェック |
