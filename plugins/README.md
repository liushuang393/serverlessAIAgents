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

## 互換要件

- `compatibility.kernel` は `>=2.0.0` を指定する
- `compatibility.product_lines` で対象製品線を明示する

## 実行時判定

- Studio (`migration|faq|assistant`): manifest 不整合は `DENY`
- Framework (`framework`): manifest 不整合は `ALLOW + warning`
