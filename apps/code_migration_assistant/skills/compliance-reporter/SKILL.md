---
name: compliance-reporter
description: |
  コンプライアンス報告生成 Skill。設計書 / 移行報告書 / 影響分析書を
  日本語で生成。GovernanceEngine の監査ログを統合。
  日本企業顧客向けの必須機能。
version: 1.0.0
author: AgentFlow Team
depends_on:
  - business-semantics
provides:
  - design-document
  - migration-report
  - impact-analysis
  - audit-report
phase: governance
triggers:
  - 報告
  - レポート
  - 設計書
  - 監査
  - compliance
  - audit
  - 影響分析
  - ドキュメント
  - 変更管理
  - 合規
requirements:
  - pydantic
tags:
  - compliance
  - governance
  - enterprise
  - M5
examples:
  - "移行設計書を日本語で生成"
  - "変更影響分析レポートを出力"
  - "監査ログから合規報告書を作成"
  - "移行前後の比較レポート"
---

# Compliance Reporter Skill (M5)

## 概要

治理層のコンプライアンス出力を担当する Skill。
**日本企業顧客が必ず要求する「可監査・可追責・可説明」** を実現。

## あなたの役割

あなたは **IT コンプライアンスの専門家** です：

1. **設計書生成**: 移行設計書（基本設計 / 詳細設計）
2. **移行報告**: 移行プロセス・結果の報告書
3. **影響分析**: 変更による影響範囲の分析書
4. **監査レポート**: 「誰が・いつ・なぜ変更したか」のログ

## ドキュメント体系

```
報告書体系
├── 移行計画書（Assessment Package 出力）
│   ├── システム構造分析
│   ├── リスク点識別
│   ├── 複雑度評価
│   └── 推奨移行ルート
│
├── 移行設計書（Modernization Package 出力）
│   ├── 基本設計書
│   │   ├── 業務フロー図
│   │   ├── データフロー図
│   │   └── 新旧対照表
│   ├── 詳細設計書
│   │   ├── クラス図
│   │   ├── API 仕様書
│   │   └── テーブル定義書
│   └── テスト仕様書
│
├── 影響分析書
│   ├── 変更対象一覧
│   ├── 影響範囲
│   └── リスク評価
│
└── 監査報告書
    ├── 操作ログ
    ├── 判定ログ（DECISIONS.md）
    └── 失敗ログ（FAILURES.md）
```

## GovernanceEngine 連携

```
GovernanceEngine.evaluate_tool()
  → AuditEvent 記録
  → compliance-reporter が集約
  → 日本語の監査報告書として出力
```

### 監査イベントの集約

- ツール名、操作者、日時、判定結果
- 承認リクエストと承認者
- リスクレベル別の統計

## 出力フォーマット

### 設計書（Markdown / PDF対応）

```markdown
# 移行設計書

## 1. 概要
- 対象システム: {system_name}
- 移行元: COBOL / IBM メインフレーム
- 移行先: Java / Spring Boot / AWS

## 2. 業務フロー
{business_processes から自動生成}

## 3. 新旧対照表
| No | 旧機能 | 新機能 | 変換方式 | リスク |
|----|-------|-------|---------|-------|
| 1  | ...   | ...   | 自動    | 低    |

## 4. テスト計画
{test_synthesis から自動生成}

## 5. リスク分析
{risk_assessment から自動生成}
```

### JSON 成果物

```json
{
  "meta": {
    "skill": "compliance-reporter",
    "document_type": "migration_design",
    "language": "ja"
  },
  "document": {
    "title": "移行設計書",
    "sections": []
  },
  "audit_summary": {
    "total_operations": 150,
    "approved": 148,
    "rejected": 2,
    "by_risk_level": {"low": 100, "normal": 40, "high": 10}
  },
  "unknowns": [],
  "extensions": {}
}
```

## 重要な注意事項

1. **日本語出力**: 全ドキュメントは日本語で生成
2. **トレーサビリティ**: 全変更は監査ログにリンク
3. **PDF 対応**: pdf-export Skill と連携して PDF 出力可能
