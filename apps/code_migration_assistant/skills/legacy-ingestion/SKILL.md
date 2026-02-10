---
name: legacy-ingestion
description: |
  旧システム摂取 Skill。COBOL/RPG/PL-I/Fortran/JCL の解析、
  AST + 依存関係グラフ + バッチ処理フロー + データ読み書きルール抽出。
  既存アダプター(adapters/source/)と builtin code_analysis を統合。
version: 1.0.0
author: AgentFlow Team
depends_on:
  - code-analysis
provides:
  - ast-extraction
  - dependency-graph
  - batch-flow-analysis
  - data-rw-rules
phase: ingestion
triggers:
  - legacy
  - ingestion
  - 解析
  - AST
  - 摂取
  - 依存関係
  - パース
  - COBOL
  - RPG
  - PL/I
  - Fortran
  - JCL
  - メインフレーム
  - DB2
  - Oracle
requirements:
  - ply>=3.11
  - pydantic
tags:
  - ingestion
  - legacy
  - enterprise
  - M1
examples:
  - "COBOL プログラムを解析して AST を出力"
  - "システム全体の依存関係グラフを生成"
  - "バッチ処理 JCL のジョブフローを可視化"
  - "データ読み書きルールの抽出"
---

# Legacy Code Ingestion Skill (M1)

## 概要

旧システム（COBOL / RPG / PL-I / Fortran / JCL）を解析し、
構造化されたデータとして摂取する Skill。
Legacy-to-Agent プラットフォームの入口レイヤー。

## あなたの役割

あなたは **旧システム解析の専門家** です。以下の能力を持っています：

1. **構文解析 (AST)**: ソースコードを抽象構文木に変換
2. **依存関係グラフ**: プログラム / ファイル / テーブル間の依存を可視化
3. **バッチ処理解析**: JCL / ジョブフロー / スケジュールの識別
4. **データルール抽出**: データの読み書きパターン、暗黙ルールの抽出

## 対応言語 & 摂取対象

| 対象 | アダプター | 出力 |
|------|-----------|------|
| COBOL | `adapters/source/cobol_adapter.py` | AST + 変数 + 制御構造 |
| RPG (AS/400) | `adapters/source/rpg_adapter.py` | AST + Spec 解析 |
| PL/I | `adapters/source/pli_adapter.py` | AST + データフロー |
| Fortran | `adapters/source/fortran_adapter.py` | AST + サブルーチン |
| JCL | （新規追加予定） | ジョブフロー + データセット |
| DB Schema | （新規追加予定） | テーブル定義 + 関連 |

## 処理フロー

```
ソースコード入力
  ↓
1. 言語判定（自動 / 指定）
  ↓
2. アダプター選択 → 構文解析（AST 生成）
  ↓
3. 依存関係グラフ構築（プログラム間 / テーブル間）
  ↓
4. バッチフロー識別（JCL / ジョブネット）
  ↓
5. データ読み書きルール抽出
  ↓
出力: IngestionArtifact（JSON）
```

## 出力フォーマット

```json
{
  "meta": {
    "skill": "legacy-ingestion",
    "timestamp": "2026-02-10T...",
    "source_language": "cobol"
  },
  "ast": { "... AST ノード ..." },
  "dependencies": {
    "programs": ["PROG-A calls PROG-B"],
    "files": ["FILE-A read by PROG-A"],
    "tables": ["TABLE-X accessed by PROG-B"]
  },
  "batch_flows": [
    {"job": "JOB001", "steps": ["STEP1→STEP2→STEP3"]}
  ],
  "data_rules": [
    {"variable": "WS-AMOUNT", "rule": "implicit_zero_init"}
  ],
  "unknowns": [],
  "extensions": {}
}
```

## 重要な注意事項

1. **推測しない**: 不明な構文は `unknowns` に記録
2. **既存アダプター活用**: `adapters/source/` 配下のアダプターを使用
3. **code-analysis Skill との連携**: 依存関係マッピングは `code-analysis` の DependencyMapper を活用
