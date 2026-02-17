# Tool / Skill / Plugin 実装ガイド

## 1. 分類

1. Tool Plugin: 実行最小単位
2. SkillPack Plugin: 複合能力パック
3. Protocol Adapter Plugin: 外部プロトコル接続
4. Channel Plugin: チャネル接続
5. Deploy/Codegen Plugin: 配備・生成
6. Runner/Sandbox Plugin: 実行環境

## 2. Tool 実装

1. `@tool` で登録する
2. 副作用ツールには `operation_type` を `write/delete/execute` で指定
3. `plugin_id` と `plugin_version` を設定
4. 必要に応じて `required_permissions` / `requires_approval` を設定

## 3. Skill 実装

1. `SKILL.md` と実行コードを同一パッケージに配置
2. 入力契約・出力契約・失敗時挙動を明記
3. 参照可能な Tool 一覧を明示

## 4. Plugin manifest 必須項目

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

## 5. 実行時ガバナンス

- Studio (`migration|faq|assistant`): 不整合は拒否
- Framework (`framework`): 不整合は警告許可
