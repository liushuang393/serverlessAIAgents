---
name: pdf-export
description: MarkdownテキストをPDFに変換、出力する技術
version: 1.0.0
provides:
  - pdf-generation
  - formatted-reports
---

# PDF Export Skill

## 概要
Markdown形式で生成された各種レポート（設計書、報告書、監査ログ）を、顧客提出用のPDFファイルに変換します。

## 使用ツール
- `migration.export_pdf`: Markdownテキストと出力パスを指定して実行します。

## 指導
1. **内容の整形**: PDFに出力する前に、Markdownが正しく構造化されていることを確認してください。
2. **ファイル名**: `task_id_report_type.pdf` のような識別しやすい名前を推奨します。
3. **日本語対応**: 日本語文字列が含まれる場合、ツール側で日本語フォントが適用されるようになっています（環境依存）。
