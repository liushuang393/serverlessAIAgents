# Plugin Manifests

`plugins/<plugin_id>/plugin_manifest.json` は、実行時ガバナンスが参照する標準メタデータです。

## 必須項目

- `id`
- `version`
- `type`
- `capabilities`
- `risk_tier`
- `side_effects`
- `required_permissions`
- `signature`
- `compatibility`
- `tests_required`
- `plugin_manifest.sig`（sidecar 署名ファイル）

## 署名ファイル

- 配置先: `plugins/<plugin_id>/plugin_manifest.sig`
- 形式: `plugin_manifest.json` 全体を canonical JSON 化したバイト列に対する署名（base64）
- canonical JSON ルール:
  - `sort_keys=True`
  - `separators=(",", ":")`
  - UTF-8 バイト列

## 互換要件

- `compatibility.kernel` は `>=2.0.0` を指定する
- `compatibility.product_lines` で対象製品線を明示する

## Trust Store

- 既定パス: `plugins/trust_store.json`
- 上書き env: `AGENTFLOW_PLUGIN_TRUST_STORE`
- 形式:

```json
{
  "issuer-name": {
    "key-id": {
      "algorithm": "ed25519",
      "public_key_base64": "<base64-raw-public-key>"
    }
  }
}
```

## 実行時ポリシー（P1）

- `AGENTFLOW_PLUGIN_SIGNATURE_ENFORCEMENT` 既定値: `warn`
- 現在は `warn` 固定運用（署名不整合は全 product_line で warning。deny しない）

## 実行時判定

- Studio (`migration|faq|assistant`): manifest 不整合は `DENY`
- Framework (`framework`): manifest 不整合は `ALLOW + warning`
