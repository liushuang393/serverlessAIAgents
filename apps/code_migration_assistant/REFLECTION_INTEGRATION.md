# Code Migration Assistant - Reflection Pattern統合設計

## 📋 概要

Reflection Patternを使用して、生成されたJavaコードを自己評価・改善するシステムを設計します。

---

## 🎯 目標

### 主要目標
1. **品質向上**: 生成されたコードの品質を自動的に改善
2. **エラー修正**: 検証で見つかったエラーを自動修正
3. **反復改善**: 最大3回の反復で最適化
4. **学習機能**: 改善履歴をMemory Systemに記録

### 成功指標
- 初回生成スコア: 60-70点
- 最終スコア: 85点以上
- 改善率: 30%以上
- 反復回数: 平均2回以下

---

## 🔄 Reflection Workflow設計

### 全体フロー

```
┌─────────────────────────────────────────────────────────┐
│              Reflection Workflow                         │
├─────────────────────────────────────────────────────────┤
│                                                           │
│  Input: COBOL Code + AST + Metadata                      │
│     │                                                     │
│     ▼                                                     │
│  ┌──────────────────────────────────────┐               │
│  │  1. Generate (JavaGenerator)         │               │
│  │     - 初期Javaコード生成              │               │
│  │     - Memory Systemからパターン取得   │               │
│  └──────────────────────────────────────┘               │
│     │                                                     │
│     ▼                                                     │
│  ┌──────────────────────────────────────┐               │
│  │  2. Evaluate (MigrationValidator)    │               │
│  │     - 品質評価（0-100点）             │               │
│  │     - エラー・警告検出                │               │
│  │     - 改善提案生成                    │               │
│  └──────────────────────────────────────┘               │
│     │                                                     │
│     ▼                                                     │
│  ┌──────────────────────────────────────┐               │
│  │  3. Decision                          │               │
│  │     - スコア >= 85点 → 完了           │               │
│  │     - スコア < 85点 → 改善            │               │
│  │     - 反復回数 >= 3 → 終了            │               │
│  └──────────────────────────────────────┘               │
│     │                                                     │
│     ▼                                                     │
│  ┌──────────────────────────────────────┐               │
│  │  4. Improve (JavaGenerator)          │               │
│  │     - フィードバックに基づいて改善    │               │
│  │     - エラー修正                      │               │
│  │     - コード最適化                    │               │
│  └──────────────────────────────────────┘               │
│     │                                                     │
│     └─────────────────┐                                  │
│                       ▼                                  │
│                    繰り返し（最大3回）                    │
│                                                           │
│  Output: Final Java Code + Report + History              │
│                                                           │
└─────────────────────────────────────────────────────────┘
```

---

## 🧩 コンポーネント統合

### 1. Generator（JavaGenerator）

**役割:** Javaコード生成

**初回生成:**
```python
async def generate_initial(self, input_data: dict[str, Any]) -> dict[str, Any]:
    """初回生成.
    
    Args:
        input_data: {
            "ast": dict,
            "metadata": dict,
            "task": str,  # "COBOL→Java移行"
        }
    
    Returns:
        {
            "output": str,  # 生成されたJavaコード
            "task": str,
        }
    """
    # Memory Systemから類似パターンを取得
    patterns = await self._memory.recall(
        topic="migration_pattern",
        query=input_data["ast"]["program_id"],
        limit=5,
        min_similarity=0.7,
    )
    
    # パターンを参考にコード生成
    java_code = self._generate_with_patterns(
        input_data["ast"],
        input_data["metadata"],
        patterns,
    )
    
    return {
        "output": java_code,
        "task": input_data["task"],
    }
```

**改善生成:**
```python
async def generate_improved(self, input_data: dict[str, Any]) -> dict[str, Any]:
    """改善版生成.
    
    Args:
        input_data: {
            "output": str,        # 前回の出力
            "feedback": str,      # フィードバック
            "suggestions": list,  # 改善提案
            "task": str,
        }
    
    Returns:
        {
            "improved_output": str,  # 改善されたJavaコード
            "task": str,
        }
    """
    # フィードバックを解析
    issues = self._parse_feedback(input_data["feedback"])
    
    # 改善提案を適用
    improved_code = self._apply_suggestions(
        input_data["output"],
        input_data["suggestions"],
        issues,
    )
    
    return {
        "improved_output": improved_code,
        "task": input_data["task"],
    }
```

---

### 2. Evaluator（MigrationValidator）

**役割:** 品質評価

**評価実装:**
```python
async def evaluate(self, input_data: dict[str, Any]) -> dict[str, Any]:
    """品質評価.
    
    Args:
        input_data: {
            "output": str,       # 生成されたJavaコード
            "cobol_code": str,   # 元のCOBOLコード
            "ast": dict,
            "task": str,
        }
    
    Returns:
        {
            "is_acceptable": bool,  # 合格判定（>= 85点）
            "score": float,         # 総合スコア（0-100）
            "feedback": str,        # フィードバック
            "suggestions": list,    # 改善提案
            "output": str,          # 元の出力
        }
    """
    # 各項目を評価
    syntax_score = await self._check_syntax(input_data["output"])
    semantics_score = await self._check_semantics(
        input_data["cobol_code"],
        input_data["output"],
        input_data["ast"],
    )
    style_score = await self._check_style(input_data["output"])
    performance_score = await self._check_performance(input_data["output"])
    
    # 総合スコア計算
    total_score = (
        syntax_score +
        semantics_score +
        style_score +
        performance_score
    )
    
    # フィードバック生成
    feedback = self._generate_feedback(
        syntax_score,
        semantics_score,
        style_score,
        performance_score,
    )
    
    # 改善提案生成
    suggestions = self._generate_suggestions(
        input_data["output"],
        syntax_score,
        semantics_score,
        style_score,
        performance_score,
    )
    
    return {
        "is_acceptable": total_score >= 85.0,
        "score": total_score,
        "feedback": feedback,
        "suggestions": suggestions,
        "output": input_data["output"],
        "task": input_data["task"],
    }
```

---

### 3. Improver（JavaGenerator）

**役割:** コード改善

**改善戦略:**

#### 構文エラー修正
```python
def fix_syntax_errors(self, code: str, errors: list) -> str:
    """構文エラーを修正."""
    for error in errors:
        if error["type"] == "missing_semicolon":
            code = self._add_semicolon(code, error["line"])
        elif error["type"] == "unclosed_brace":
            code = self._close_brace(code, error["line"])
        # ... 他のエラータイプ
    return code
```

#### 意味的改善
```python
def improve_semantics(self, code: str, suggestions: list) -> str:
    """意味的改善."""
    for suggestion in suggestions:
        if suggestion["type"] == "data_type_mismatch":
            code = self._fix_data_type(code, suggestion)
        elif suggestion["type"] == "logic_error":
            code = self._fix_logic(code, suggestion)
        # ... 他の改善タイプ
    return code
```

#### スタイル改善
```python
def improve_style(self, code: str) -> str:
    """スタイル改善."""
    # 命名規則の適用
    code = self._apply_naming_conventions(code)
    
    # コメント追加
    code = self._add_comments(code)
    
    # フォーマット
    code = self._format_code(code)
    
    return code
```

---

## 📊 評価基準詳細

### 1. 構文正確性（30点）

#### チェック項目
- **コンパイルエラー（20点）**
  - エラーなし: 20点
  - 1-3個のエラー: 10点
  - 4個以上のエラー: 0点

- **警告（5点）**
  - 警告なし: 5点
  - 1-5個の警告: 3点
  - 6個以上の警告: 0点

- **命名規則（5点）**
  - 完全遵守: 5点
  - 部分的遵守: 3点
  - 未遵守: 0点

### 2. 意味的等価性（40点）

#### チェック項目
- **データ型の一致（15点）**
  - 全て一致: 15点
  - 80%以上一致: 10点
  - 80%未満: 5点

- **ロジックの等価性（20点）**
  - 完全等価: 20点
  - ほぼ等価: 15点
  - 部分的等価: 10点
  - 不一致: 0点

- **エラーハンドリング（5点）**
  - 適切: 5点
  - 部分的: 3点
  - 不足: 0点

### 3. コード品質（20点）

#### チェック項目
- **ベストプラクティス（10点）**
  - 完全遵守: 10点
  - 部分的遵守: 5点
  - 未遵守: 0点

- **コメント・Javadoc（5点）**
  - 十分: 5点
  - 部分的: 3点
  - 不足: 0点

- **可読性（5点）**
  - 高い: 5点
  - 中程度: 3点
  - 低い: 0点

### 4. パフォーマンス（10点）

#### チェック項目
- **アルゴリズム効率（5点）**
  - 最適: 5点
  - 良好: 3点
  - 改善の余地: 1点

- **メモリ使用量（3点）**
  - 最適: 3点
  - 良好: 2点
  - 改善の余地: 1点

- **最適化（2点）**
  - 十分: 2点
  - 部分的: 1点
  - 不足: 0点

---

## 🔄 Reflection Loop実装

### ReflectionLoop統合

```python
from agentflow.patterns import ReflectionLoop

# コンポーネント作成
generator = JavaGenerator(memory=memory_manager)
validator = MigrationValidator(llm_client=llm)
improver = generator  # GeneratorがImproverも兼ねる

# ReflectionLoop作成
loop = ReflectionLoop(
    generator=generator,
    reflector=validator,
    improver=improver,
    max_iterations=3,
)

# 実行
result = await loop.execute({
    "task": "COBOL→Java移行",
    "ast": ast,
    "metadata": metadata,
    "cobol_code": cobol_code,
})

# 結果
final_code = result["final_output"]
iterations = result["iterations"]
history = result["history"]
final_score = result["final_score"]
```

---

## 📈 改善履歴の記録

### 履歴データ構造

```python
{
    "iteration": int,           # 反復回数
    "score": float,             # スコア
    "is_acceptable": bool,      # 合格判定
    "feedback": str,            # フィードバック
    "suggestions": list,        # 改善提案
    "changes": list,            # 変更内容
    "timestamp": datetime,      # タイムスタンプ
}
```

### Memory Systemへの記録

```python
# 改善履歴を記憶
await memory.remember(
    content=f"Iteration {iteration}: Score {score} → {new_score}",
    topic="improvement_history",
    metadata={
        "program_id": program_id,
        "iteration": iteration,
        "score_improvement": new_score - score,
        "changes": changes,
    }
)
```

---

## 🎯 次のステップ

1. ✅ Reflection Pattern統合設計完了
2. ⏭️ Memory System統合設計
3. ⏭️ 実装開始

