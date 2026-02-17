# 3 Studio + Kernel ガイド

## 1. 目的

- 顧客の認知負荷を下げる（4 ステップ導線）
- Kernel を長期安定化し、機能拡張はプラグインで実現する
- 監査可能な実行経路を維持する

## 2. 顧客導線（Business Surface）

1. テンプレートを選択
2. データソース・権限・リスクレベルを設定
3. 実行
4. 成果物を確認/ダウンロード

## 3. 3 つの製品主線

- `Migration Studio`
  - 成果物: リスク一覧、証拠付き評価、移行提案
- `Enterprise FAQ Studio`
  - 成果物: 引用付きFAQ、索引状態、運用レポート
- `Computer Assistant Studio`
  - 成果物: タスク実行ログ、記憶状態、セキュリティ監査ログ

## 4. Persona 分離

- `business`: 成果物中心。内部プロトコル非表示。
- `developer`: SDK/プラグイン開発中心。プロトコル可視。
- `operator`: ポリシー、隔離、監査、署名検証中心。

## 5. app_config 契約（追加フィールド）

- `product_line`: `migration|faq|assistant|framework`
- `surface_profile`: `business|developer|operator`
- `audit_profile`: `business|developer`
- `plugin_bindings`: バインド済みプラグイン
- `security_mode`: `read_only|approval_required|autonomous`（assistant 向け）

## 6. 監査ポリシー

- `audit_profile=developer`: 従来の stream/A2A/MCP 面チェックを維持
- `audit_profile=business`: A2A/stream/MCP の強制チェックを無効化し、安全基線を維持

## 7. Plugin Manifest 標準

`plugins/<plugin_id>/plugin_manifest.json` に以下を必須化する:

- `id`, `version`, `type`
- `capabilities`, `risk_tier`, `side_effects`
- `required_permissions`
- `signature`
- `compatibility`
- `tests_required`
