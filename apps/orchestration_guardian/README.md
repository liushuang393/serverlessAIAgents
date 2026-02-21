# Orchestration Guardian (オーケストレーション・ガーディアン)


<!-- README_REQUIRED_SECTIONS_START -->
## 機能概要
- オーケストレーション準備状況をチェックし、運用前の欠落を検出。
- プロトコル契約カバレッジを検証し、実装と宣言のずれを可視化。
- API でヘルス、チェックリスト、検証実行を提供。

## 優位性
- 軽量構成のため導入が早く、CI への組み込みが容易。
- 本番前のゲートとして利用でき、リリース品質を底上げできる。
- 検証観点を共通化し、app 間の監査基準をそろえられる。

## 技術アーキテクチャ
- FastAPI ベースの検証 API。
- ルール化したチェックリスト評価ロジック。
- Platform 発布フローに接続可能な最小構成。

## アプリケーション階層
- API Layer: `/api/health` `/api/checklist` `/api/verify`。
- Validation Layer: 契約チェック・準備状況評価。
- Reporting Layer: 判定結果の構造化出力。
- Integration Layer: CI/CD・Platform 連携。
<!-- README_REQUIRED_SECTIONS_END -->

オーケストレーション準備状況とプロトコル契約カバレッジを検証するための軽量 app です。

`product_line`: `framework` / `surface_profile`: `operator`

## 開発環境（インストール: 統一手順）

```bash
cd <repo-root>
bash setup_dev.sh
```

## 起動

```bash
conda activate agentflow
python -m apps.orchestration_guardian.main
```

## テスト/静的チェック（統一スクリプト）

```bash
cd <repo-root>
./check.sh test
./check.sh lint
./check.sh type-check
```

## 本番ビルド/発布（Platform に統一）

```bash
conda activate agentflow
python -m apps.platform.main publish ./apps/orchestration_guardian --target docker
```

## Endpoints

- **シークレット**: 本番環境のシークレット（機密情報）は Secret Manager 経由で注入されます。
- **テナント招待**: セキュリティのため、招待メールは「通知」と「ログインURL」の2通に分けて送信されます。
- **詳細ドキュメント**: 詳細は `docs/internal/env-bootstrap-and-tenant-invite-security.md` を参照してください。
