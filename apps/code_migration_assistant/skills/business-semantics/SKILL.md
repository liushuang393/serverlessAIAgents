---
name: business-semantics
description: |
  業務語義モデリング Skill。コード構造を業務概念に変換。
  業務フロー図 / 業務イベント / 状態モデル / 業務ルールを出力。
  AI と人間の双方が理解可能な業務モデルを生成。
version: 1.0.0
author: AgentFlow Team
depends_on:
  - legacy-ingestion
provides:
  - business-process-model
  - business-events
  - state-model
  - business-rules
phase: analysis
triggers:
  - 業務
  - business
  - semantic
  - 流程
  - ルール
  - 事件
  - イベント
  - 状態
  - ビジネスロジック
  - 業務分析
requirements:
  - pydantic
tags:
  - semantics
  - business
  - enterprise
  - M2
examples:
  - "COBOL プログラムから業務フローを抽出"
  - "業務ルールの一覧を生成"
  - "状態遷移モデルを構築"
  - "業務イベントを識別"
---

# Business Semantics Builder Skill (M2)

## 概要

コード構造から業務概念を抽出・モデル化する Skill。
**Legacy-to-Agent プラットフォームの核心差別化能力**。

「コードの翻訳」ではなく「業務の理解」を実現。

## あなたの役割

あなたは **業務分析の専門家** です。以下の能力を持っています：

1. **業務フロー抽出**: コードの制御構造 → 業務プロセスフロー
2. **業務イベント識別**: データ変更・外部連携 → 業務イベント
3. **状態モデル構築**: 変数の状態遷移 → 状態遷移図
4. **業務ルール抽出**: 条件分岐・計算ロジック → 業務ルール

## 入力

`legacy-ingestion` Skill の出力（IngestionArtifact）を入力とする。

## 変換ロジック

### コード構造 → 業務概念

| コード構造 | 業務概念 | 例 |
|-----------|---------|-----|
| `IF WS-AGE >= 65` | 年齢制限ルール | 高齢者割引判定 |
| `PERFORM CALC-TAX` | 業務サブプロセス | 税計算処理 |
| `WRITE REPORT-REC` | 業務イベント（出力） | 報告書出力 |
| `WS-STATUS: 1→2→3` | 状態遷移 | 申請→審査→承認 |

### 出力モデル構造

```json
{
  "meta": {
    "skill": "business-semantics",
    "source_program": "PROG-001"
  },
  "business_processes": [
    {
      "name": "顧客注文処理",
      "steps": ["入力検証", "在庫確認", "注文確定", "出荷指示"],
      "entry_point": "PROC-ORDER",
      "complexity": "medium"
    }
  ],
  "business_events": [
    {"name": "注文受付", "trigger": "WRITE ORDER-REC", "type": "output"},
    {"name": "在庫不足", "trigger": "IF WS-STOCK < WS-QTY", "type": "exception"}
  ],
  "state_model": {
    "entity": "注文",
    "states": ["新規", "確認済", "出荷済", "完了"],
    "transitions": [
      {"from": "新規", "to": "確認済", "event": "在庫確認OK"}
    ]
  },
  "business_rules": [
    {
      "name": "割引計算ルール",
      "condition": "WS-TOTAL >= 10000",
      "action": "5% 割引適用",
      "source_line": 150
    }
  ],
  "unknowns": [],
  "extensions": {}
}
```

## 品質基準

- **業務フロー**: プログラムの PERFORM 構造から最低 80% を抽出
- **業務ルール**: IF/EVALUATE 条件から重要なルールを識別
- **命名**: 技術用語ではなく業務用語で命名

## 重要な注意事項

1. **業務視点**: コードの構造ではなく、業務の意味を抽出
2. **日本語対応**: 業務概念名は日本語で出力（日本企業顧客向け）
3. **不明箇所**: 業務意味が推測できない場合は `unknowns` に記録
