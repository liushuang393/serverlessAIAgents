## ステージ1: 差分固定
**目的**: `run_pipeline()` と `execute_stage_task()` の責務差分を、capability 実行・trace・成果物契約の観点で固定する。
**成功条件**: 追加テストで stage 実行経路の skill-first / capability trace 契約の欠落が再現する。
**タスク分解**:
- 対象コードと既存テストの把握
- 追加する回帰テストの設計
- 失敗を確認して差分を固定
**進捗状況**: 完了

## ステージ2: Runtime 共通化
**目的**: 両経路が同じ capability runner と stage 実行補助を使うようにする。
**成功条件**: stage 実行でも full pipeline でも trace/provider/decision/evidence の意味が揃う。
**タスク分解**:
- 共通ヘルパーの追加
- `execute_stage_task()` の capability 実行統一
- `run_pipeline()` の capability 実行統一
**進捗状況**: 完了

## ステージ3: 検証と仕上げ
**目的**: 追加テストと対象テストを通し、lint/型面の退行を確認する。
**成功条件**: 追加した回帰テストが通り、編集ファイルに新規 lint エラーがない。
**タスク分解**:
- 追加テストの再実行
- 関連 pytest 実行
- lints 確認と最終見直し
**進捗状況**: 完了
