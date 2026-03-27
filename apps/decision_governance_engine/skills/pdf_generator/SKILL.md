---
name: pdf_generator
version: 1.0.0
description: DecisionReportを日本式ビジネス提案書フォーマット（PDF/HTML）で出力するスキル
author: Decision Governance Engine
tags:
  - export
  - pdf
  - html
  - report
triggers:
  - export pdf
  - generate report
  - download html
requirements:
  - reportlab
---

# PDF Generator Skill

## あなたの責任

DecisionReport オブジェクトを受け取り、日本式のビジネス提案書フォーマットで PDF または HTML を生成すること。

## 出力仕様

- **PDF**: ReportLab を使用し、CJK（日本語）に対応した A4 サイズのドキュメント。
- **HTML**: モダンな A2UI デザインに準拠したレスポンシブな HTML 文書。
- **署名欄**: 作成者と承認者の情報（部署・役職・氏名・日付）を正しく表示し、署名済みの場合は電子印鑑（赤丸）を視覚的に表示する。

## 改善ポイント (v3.2)

1. **空白ページの防止**: `PageBreak()` を呼び出す前に、そのページにコンテンツが存在するかをチェックする。
2. **スタイルの一貫性**: 画面（Dashboard/ReportPage）と出力内容のスタイル・項目を完全に一致させる。
3. **署名データの完全性**: `signed_data` が提供されている場合、それを最優先で署名欄に反映する。
