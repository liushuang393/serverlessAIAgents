# Code Migration Assistant - コンポーネント詳細設計

## 📋 概要

各コンポーネントの詳細設計を定義します。入出力インターフェース、データ構造、エラーハンドリング、実装方針を明確化します。

---

## 🧩 1. COBOLParser（COBOL解析器）

### 職責
- COBOLソースコードの字句解析
- 構文解析とAST生成
- メタデータ抽出
- エラー検出と報告

### 入出力インターフェース

#### 入力
```python
{
    "cobol_code": str,           # COBOLソースコード（必須）
    "file_name": str,            # ファイル名（オプション）
    "encoding": str,             # 文字エンコーディング（デフォルト: "utf-8"）
    "parse_options": {           # 解析オプション（オプション）
        "strict_mode": bool,     # 厳格モード（デフォルト: False）
        "expand_copy": bool,     # COPY文を展開（デフォルト: True）
    }
}
```

#### 出力
```python
{
    "success": bool,             # 解析成功フラグ
    "ast": {                     # 抽象構文木
        "program_id": str,       # プログラムID
        "divisions": {           # 各DIVISIONの内容
            "identification": {...},
            "environment": {...},
            "data": {...},
            "procedure": {...},
        }
    },
    "metadata": {                # メタデータ
        "variables": [...],      # 変数定義リスト
        "procedures": [...],     # プロシージャリスト
        "file_controls": [...],  # ファイル制御リスト
    },
    "errors": [                  # エラーリスト
        {
            "type": str,         # エラータイプ
            "line": int,         # 行番号
            "column": int,       # 列番号
            "message": str,      # エラーメッセージ
        }
    ],
    "warnings": [...],           # 警告リスト（同じ構造）
}
```

### データ構造

#### AST構造
```python
class ASTNode:
    """AST基底クラス."""
    node_type: str              # ノードタイプ
    line_number: int            # 行番号
    column_number: int          # 列番号
    children: list[ASTNode]     # 子ノード

class ProgramNode(ASTNode):
    """プログラムノード."""
    program_id: str
    divisions: dict[str, DivisionNode]

class DataDivisionNode(ASTNode):
    """データ部ノード."""
    working_storage: list[VariableNode]
    file_section: list[FileNode]
    linkage_section: list[VariableNode]

class ProcedureDivisionNode(ASTNode):
    """手続き部ノード."""
    paragraphs: list[ParagraphNode]
    statements: list[StatementNode]
```

### エラーハンドリング

#### エラータイプ
1. **SyntaxError**: 構文エラー
2. **SemanticError**: 意味エラー（未定義変数など）
3. **WarningError**: 警告（非推奨構文など）

#### エラー処理方針
- **厳格モード**: エラーで即座に停止
- **寛容モード**: エラーを記録して継続（部分的なAST生成）

### 実装方針

#### Phase 1（MVP）
- IDENTIFICATION DIVISION解析
- DATA DIVISION解析（基本データ型のみ）
- PROCEDURE DIVISION解析（基本制御構造のみ）

#### サポートする構文
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. SAMPLE-PROGRAM.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-VAR PIC 9(5).
01 WS-NAME PIC X(20).

PROCEDURE DIVISION.
    DISPLAY "Hello".
    MOVE 100 TO WS-VAR.
    IF WS-VAR > 50
        DISPLAY "Large"
    ELSE
        DISPLAY "Small"
    END-IF.
    STOP RUN.
```

---

## 🧩 2. JavaGenerator（Java生成器）

### 職責
- ASTからJavaコード生成
- 命名規則の適用
- コメント生成
- コードフォーマット

### 入出力インターフェース

#### 入力
```python
{
    "ast": dict,                 # 抽象構文木（必須）
    "metadata": dict,            # メタデータ（必須）
    "generation_options": {      # 生成オプション（オプション）
        "class_name": str,       # クラス名（デフォルト: ASTから生成）
        "package_name": str,     # パッケージ名（デフォルト: "com.migration"）
        "style": str,            # コードスタイル（"standard", "spring", "jakarta"）
        "add_comments": bool,    # コメント追加（デフォルト: True）
        "add_javadoc": bool,     # Javadoc追加（デフォルト: True）
    }
}
```

#### 出力
```python
{
    "success": bool,             # 生成成功フラグ
    "java_code": str,            # Javaソースコード
    "class_name": str,           # 生成されたクラス名
    "package_name": str,         # パッケージ名
    "imports": list[str],        # import文リスト
    "report": {                  # 生成レポート
        "lines_of_code": int,    # コード行数
        "methods_count": int,    # メソッド数
        "fields_count": int,     # フィールド数
        "complexity": float,     # 複雑度スコア
    },
    "warnings": [                # 警告リスト
        {
            "type": str,         # 警告タイプ
            "message": str,      # 警告メッセージ
            "suggestion": str,   # 改善提案
        }
    ],
    "mappings": {                # COBOL→Javaマッピング
        "variables": {...},      # 変数マッピング
        "procedures": {...},     # プロシージャマッピング
    }
}
```

### データ型マッピング

#### COBOL → Java
```python
TYPE_MAPPING = {
    "PIC 9(n)": "int",           # 数値（整数）
    "PIC 9(n)V9(m)": "BigDecimal",  # 数値（小数）
    "PIC X(n)": "String",        # 文字列
    "PIC A(n)": "String",        # 英字
    "PIC S9(n)": "int",          # 符号付き整数
    "PIC S9(n)V9(m)": "BigDecimal",  # 符号付き小数
}
```

### 制御構造マッピング

#### COBOL → Java
```python
CONTROL_MAPPING = {
    "IF ... END-IF": "if (...) { ... }",
    "PERFORM ... TIMES": "for (int i = 0; i < n; i++) { ... }",
    "PERFORM UNTIL": "while (...) { ... }",
    "PERFORM VARYING": "for (...; ...; ...) { ... }",
    "EVALUATE": "switch (...) { ... }",
}
```

### 命名規則

#### クラス名
- COBOL PROGRAM-ID → Java Class Name
- 例: `SAMPLE-PROGRAM` → `SampleProgram`

#### メソッド名
- COBOL PARAGRAPH → Java Method
- 例: `CALCULATE-TOTAL` → `calculateTotal()`

#### 変数名
- COBOL変数 → Java変数
- 例: `WS-TOTAL-AMOUNT` → `totalAmount`

### 生成例

#### COBOL入力
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. CALCULATOR.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-NUM1 PIC 9(5).
01 WS-NUM2 PIC 9(5).
01 WS-RESULT PIC 9(10).

PROCEDURE DIVISION.
    MOVE 100 TO WS-NUM1.
    MOVE 200 TO WS-NUM2.
    ADD WS-NUM1 TO WS-NUM2 GIVING WS-RESULT.
    DISPLAY WS-RESULT.
    STOP RUN.
```

#### Java出力
```java
package com.migration;

/**
 * Migrated from COBOL program: CALCULATOR
 * Generated by Code Migration Assistant
 */
public class Calculator {
    // Working Storage Section
    private int num1;
    private int num2;
    private int result;
    
    /**
     * Main procedure
     */
    public void execute() {
        num1 = 100;
        num2 = 200;
        result = num1 + num2;
        System.out.println(result);
    }
    
    public static void main(String[] args) {
        Calculator calculator = new Calculator();
        calculator.execute();
    }
}
```

---

## 🧩 3. MigrationValidator（移行検証器）

### 職責
- 生成されたJavaコードの検証
- 意味的等価性チェック
- コンパイルエラー検出
- 品質スコア算出

### 入出力インターフェース

#### 入力
```python
{
    "cobol_code": str,           # 元のCOBOLコード（必須）
    "java_code": str,            # 生成されたJavaコード（必須）
    "ast": dict,                 # AST（必須）
    "metadata": dict,            # メタデータ（必須）
    "validation_options": {      # 検証オプション（オプション）
        "check_syntax": bool,    # 構文チェック（デフォルト: True）
        "check_semantics": bool, # 意味チェック（デフォルト: True）
        "check_style": bool,     # スタイルチェック（デフォルト: True）
        "strict_mode": bool,     # 厳格モード（デフォルト: False）
    }
}
```

#### 出力
```python
{
    "is_valid": bool,            # 検証結果（合格/不合格）
    "score": float,              # 品質スコア（0-100）
    "scores_breakdown": {        # スコア内訳
        "syntax": float,         # 構文正確性（0-30）
        "semantics": float,      # 意味的等価性（0-40）
        "style": float,          # コード品質（0-20）
        "performance": float,    # パフォーマンス（0-10）
    },
    "errors": [                  # エラーリスト
        {
            "type": str,         # エラータイプ
            "severity": str,     # 重要度（"critical", "major", "minor"）
            "message": str,      # エラーメッセージ
            "location": str,     # エラー箇所
        }
    ],
    "warnings": [...],           # 警告リスト（同じ構造）
    "suggestions": [             # 改善提案
        {
            "type": str,         # 提案タイプ
            "message": str,      # 提案内容
            "code_snippet": str, # 改善コード例
        }
    ],
    "feedback": str,             # 総合フィードバック
}
```

### 検証項目

#### 1. 構文正確性（30点）
- Javaコンパイルエラーなし: 20点
- 警告なし: 5点
- 命名規則遵守: 5点

#### 2. 意味的等価性（40点）
- データ型の一致: 15点
- ロジックの等価性: 20点
- エラーハンドリング: 5点

#### 3. コード品質（20点）
- Javaベストプラクティス: 10点
- コメント・Javadoc: 5点
- コードの可読性: 5点

#### 4. パフォーマンス（10点）
- 効率的なアルゴリズム: 5点
- メモリ使用量: 3点
- 最適化の余地: 2点

### エラータイプ

#### Critical（致命的）
- コンパイルエラー
- 意味的不一致
- データ損失の可能性

#### Major（重大）
- 警告
- 非推奨API使用
- パフォーマンス問題

#### Minor（軽微）
- スタイル違反
- コメント不足
- 命名規則違反

---

## 🔄 コンポーネント間のデータフロー

### 基本フロー

```
COBOLParser
    ↓ (AST + Metadata)
JavaGenerator
    ↓ (Java Code)
MigrationValidator
    ↓ (Validation Result)
[Reflection Loop]
    ↓ (Improved Java Code)
Final Output
```

### エラーハンドリングフロー

```
COBOLParser Error
    → Partial AST
    → JavaGenerator (Best Effort)
    → MigrationValidator (Report Issues)
    → User Feedback

JavaGenerator Error
    → Fallback Generation
    → MigrationValidator (Report Issues)
    → Reflection Loop (Improve)

MigrationValidator Error
    → Detailed Report
    → Suggestions
    → Reflection Loop (Fix)
```

---

## 📊 パフォーマンス目標

### 処理時間
- COBOLParser: < 1秒 / 1000行
- JavaGenerator: < 2秒 / 1000行
- MigrationValidator: < 1秒 / 1000行
- 全体（Reflection含む）: < 10秒 / 1000行

### メモリ使用量
- COBOLParser: < 100MB / 10000行
- JavaGenerator: < 200MB / 10000行
- MigrationValidator: < 100MB / 10000行

---

## 🎯 次のステップ

1. ✅ コンポーネント詳細設計完了
2. ⏭️ Reflection Pattern統合設計
3. ⏭️ Memory System統合設計
4. ⏭️ 実装開始

