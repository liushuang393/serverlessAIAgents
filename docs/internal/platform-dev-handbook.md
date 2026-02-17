# Platform 開発ハンドブック

最終更新: 2026-02-17

## 1. 日次起動

1. `conda activate agentflow`
2. `python -m apps.platform.main serve --port 8000`
3. `cd apps/platform/frontend && npm run dev`

## 2. API 面

- Studio API: `/api/studios/*`
- Framework API: `/api/studios/framework/*`
- 旧 `/api/agents` などの経路は廃止済み

## 3. 変更時の基本手順

1. 変更対象ペルソナを決める（business / developer / operator）
2. Kernel 変更か Plugin 変更かを判定
3. スキーマ・API・テストを同時更新
4. `apps/*/app_config.json` を整合
5. 外部/内部ドキュメントを更新

## 4. app_config 必須項目

全 App で以下を明示する:

- `product_line`
- `surface_profile`
- `audit_profile`
- `plugin_bindings`

追加要件:

- `product_line=assistant` では `security_mode` 必須

## 5. 回帰観点

1. 旧 prefix が 404 であること
2. 新 prefix が 404 以外を返すこと
3. app_config バリデーションが strict であること
4. Studio の副作用ツールが plugin manifest で検証されること
