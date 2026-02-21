# Plugin 拡張ハンドブック

## 1. 追加手順

1. `plugins/<plugin_id>/plugin_manifest.json` を作成
2. `plugins/<plugin_id>/plugin_manifest.sig` を作成（sidecar 署名）
3. `plugins/trust_store.json` に公開鍵を登録
4. `compatibility.kernel` を `>=2.0.0` で定義
5. `compatibility.product_lines` を明示
6. `tests_required` を定義
7. 対応 App の `app_config.json` に `plugin_bindings` を追加

## 1.1 canonical JSON と sidecar 署名

- 署名対象は `plugin_manifest.json` 全体（sidecar 方式なので自己参照問題なし）
- canonical 化:
  - `sort_keys=True`
  - `separators=(",", ":")`
  - UTF-8
- `plugin_manifest.sig` は base64 文字列 1 行

## 1.2 trust store 運用

- 既定: `plugins/trust_store.json`
- 上書き: `AGENTFLOW_PLUGIN_TRUST_STORE`
- キー形式: `issuer -> key_id -> {algorithm, public_key_base64}`
- P1 は `ed25519` のみ対応

## 1.3 実行時ポリシー（P1）

- `AGENTFLOW_PLUGIN_SIGNATURE_ENFORCEMENT` 既定値は `warn`
- 署名検証失敗（missing_sig/missing_key/bad_signature/parse_error）は warning のみ
- product_line が strict でも P1 では deny しない

## 2. バージョン管理

1. 破壊的変更はメジャー更新
2. 非破壊機能追加はマイナー更新
3. 修正はパッチ更新

## 3. 審査観点

1. 署名情報の有無
2. 副作用と権限宣言の一致
3. Kernel 互換性
4. product_line 互換性
5. テスト要件の充足

## 4. 失敗パターン

- app 側 binding の version と manifest version が不一致
- Studio で `plugin_id` 未設定の副作用ツールを実行
- product_line 非対応 plugin を誤って利用
