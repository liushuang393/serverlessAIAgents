# Plugin 拡張ハンドブック

## 1. 追加手順

1. `plugins/<plugin_id>/plugin_manifest.json` を作成
2. `compatibility.kernel` を `>=2.0.0` で定義
3. `compatibility.product_lines` を明示
4. `tests_required` を定義
5. 対応 App の `app_config.json` に `plugin_bindings` を追加

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
