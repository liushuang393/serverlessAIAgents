---
name: conversation-export
description: 会話エクスポート機能。会話履歴を JSON / CSV / Markdown 形式でエクスポート。
version: 1.0.0
triggers:
  - export conversation
  - 会話エクスポート
  - export chat
  - 履歴保存
  - conversation export
tags:
  - export
  - conversation
  - history
  - backup
examples:
  - "会話履歴をエクスポートして"
  - "チャット履歴をCSVで保存して"
  - "会話をMarkdownで出力して"
---

# Conversation Export Skill

会話履歴を様々な形式でエクスポートするスキル。

## 機能

- **JSON エクスポート**: 構造化されたJSON形式で出力
- **CSV エクスポート**: スプレッドシート用CSV形式で出力
- **Markdown エクスポート**: 読みやすいMarkdown形式で出力

## 使用例

```python
from agentflow.skills.builtin.conversation_export import ConversationExportSkill

exporter = ConversationExportSkill()

# JSON エクスポート
json_data = await exporter.export_json(messages)

# CSV エクスポート
csv_data = await exporter.export_csv(messages)

# Markdown エクスポート
md_data = await exporter.export_markdown(messages)
```

## 対応形式

- JSON
- CSV
- Markdown

