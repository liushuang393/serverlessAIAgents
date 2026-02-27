# 権限モデル仕様（凍結版）

## モデル

権限単位は `agent` ではなく `skill + scope + resource`。

## Scope 一覧

- `fs.read`
- `fs.write`
- `doc.edit`
- `browser.read`
- `browser.act`
- `app.control`
- `net.http`
- `db.read`
- `db.write`
- `secrets.use`

## 判定構造

- Subject: `user_id`, `role`, `permissions`
- Resource: `type`, `target`, `sensitivity`
- Action: `read|write|execute|delete`
- Context: `app_name`, `product_line`, `tenant_id`, `trace_id`

## 承認モード

- 当回のみ
- 本タスク期間
- 永久許可（撤回可能）

## ポリシー結果

- `ALLOW`
- `DENY`
- `APPROVAL_REQUIRED`

## plugin 署名方針

- `dev`: `warn`
- `staging/prod`: `deny`

未検証署名 plugin は `staging/prod` で拒否する。
