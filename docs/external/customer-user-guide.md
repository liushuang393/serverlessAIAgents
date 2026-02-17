# 顧客利用ガイド

## 1. 共通操作フロー

1. テンプレート選択
2. データ/権限設定
3. 実行
4. 成果物確認

## 2. Studio 別成果物

1. Migration Studio
- リスク一覧
- 証跡付き評価レポート
- 移行提案

2. Enterprise FAQ Studio
- 引用付き FAQ
- インデックス状態
- 運用レポート

3. Computer Assistant Studio
- タスク結果と実行ログ
- 管理可能メモリ状態
- セキュリティ監査記録

## 3. 役割ごとの可視性

1. `business`: 業務導線のみ
2. `developer`: 実装設定と拡張設定
3. `operator`: ポリシー、監査、実行環境管理

## 4. セキュリティ

1. 副作用操作は必ずポリシー/監査を経由する。
2. Assistant の既定モードは `approval_required`。
3. 高リスク操作は承認フロー対象。
