# Legacy Modernization GEO Platform

旧システム刷新向けの需要診断、GEO コンテンツ生成、承認、静的公開を 1 つの運用フローで扱う MVP アプリです。

## できること

- 対象業界、レガシースタック、地域を指定してキャンペーンを開始する
- 需要シグナル、質問マップ、証拠サマリーを確認する
- 草稿をレビューし、Rewrite をかける
- 承認または却下で公開判定を行う
- 公開ページ、`sitemap.xml`、`ai-feed.json` を生成する
- 実行レポートを確認する

## 利用手順

### 1. 環境変数を用意する

```bash
cp apps/legacy_modernization_geo_platform/.env.example apps/legacy_modernization_geo_platform/.env
```

ライブ検索を使う場合は、必要に応じて以下を設定してください。

- `SERPAPI_API_KEY`
- `BING_SEARCH_API_KEY`
- `BING_SEARCH_ENDPOINT`

開発・テスト用に固定データで動かす場合は、以下を設定します。

```bash
GEO_PLATFORM_USE_SAMPLE_INTELLIGENCE=1
```

### 2. バックエンドを起動する

```bash
conda run -n agentflow python apps/legacy_modernization_geo_platform/scripts/dev.py --reload
```

`scripts/dev.py` は `app_config.json` を既定値とし、`GEO_PLATFORM_HOST` / `GEO_PLATFORM_PORT` があればそれを優先して `uvicorn` を起動します。既定では API は `http://localhost:8100` です。

### 3. フロントエンドを起動する

```bash
cd apps/legacy_modernization_geo_platform/frontend
npm install
npm run dev
```

既定では Operator UI は `http://localhost:3100` で起動します。

### 4. 現実運用に近い操作順

1. `Campaign Console` でキャンペーン名、業界、レガシースタックを設定して開始する。
2. `Account Workspace` で需要シグナル、質問マップ、証拠サマリーを確認する。
3. `Content Studio` で草稿と QA リスクを確認し、必要なら Rewrite を実行する。
4. `Approval Center` で承認または却下を行う。
5. 承認後、`Report Center` で公開結果とレポートを確認する。
6. 公開ページ、FAQ JSON-LD、`sitemap.xml` を確認する。

## Docker で起動する

```bash
conda run -n agentflow python apps/legacy_modernization_geo_platform/scripts/compose.py publish
```

停止:

```bash
conda run -n agentflow python apps/legacy_modernization_geo_platform/scripts/compose.py stop
```

## テスト

バックエンド:

```bash
.venv/bin/python -m pytest apps/legacy_modernization_geo_platform/tests
```

フロントエンド:

```bash
cd apps/legacy_modernization_geo_platform/frontend
npm run test
npm run build
```

E2E:

```bash
cd apps/legacy_modernization_geo_platform/frontend
npx playwright test
```

## 注意点

### 検索プロバイダについて

- 本番想定では `SerpAPI -> Bing -> DuckDuckGo` の順で検索します。
- 自動テストと E2E は再現性を優先して `GEO_PLATFORM_USE_SAMPLE_INTELLIGENCE=1` を使っています。
- ライブプロバイダの結果品質は API キー、クエリ、地域によって変動します。

### 承認フローについて

- 比較表現、数値表現、引用準備率、不明点の残り方によって `MEDIUM` 以上のリスクになり、公開前承認が必要になります。
- 却下した場合、そのタスクは `failed` になり、公開は行われません。

### 公開ページについて

- 公開ページは SPA ではなく静的 HTML です。
- 生成物は `data/published/` 以下に保存されます。
- 現時点では主に単一ページの公開を想定しています。

### 現時点の制約

- 現在の CTA は `mailto:` であり、問い合わせフォーム、CRM 連携、UTM 計測、コンバージョン分析は未実装です。
- リード管理、提案ブリーフ生成、継続フォロー運用は未実装です。
- 多ページ公開、競合比較の深掘り、運用後の性能学習ループは次フェーズです。

## 関連ドキュメント

- 品質レポート: `apps/legacy_modernization_geo_platform/quality_report.md`
- 機械可読レポート: `apps/legacy_modernization_geo_platform/quality_report.json`
- 運用順 E2E 評価: `apps/legacy_modernization_geo_platform/operations_e2e_assessment.md`
