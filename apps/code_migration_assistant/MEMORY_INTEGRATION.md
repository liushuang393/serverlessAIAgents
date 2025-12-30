# Code Migration Assistant - Memory System統合設計

## 📋 概要

Memory Systemを使用して、移行パターン、履歴、ベストプラクティスを記憶・活用するシステムを設計します。

---

## 🎯 目標

### 主要目標
1. **パターン学習**: 頻出する移行パターンを記憶
2. **履歴管理**: 過去の移行履歴を記録
3. **知識共有**: ベストプラクティスを蓄積
4. **品質向上**: 記憶を活用して生成品質を向上

### 成功指標
- パターン再利用率: 70%以上
- 履歴検索精度: 90%以上
- 生成品質向上: 20%以上
- 記憶検索速度: < 100ms

---

## 🧠 記憶の種類

### 1. 移行パターンライブラリ

**目的:** 頻出する移行パターンを記憶

#### 記憶内容
```python
{
    "content": str,              # パターン説明
    "cobol_pattern": str,        # COBOLパターン
    "java_pattern": str,         # Javaパターン
    "topic": "migration_pattern",
    "metadata": {
        "pattern_type": str,     # パターンタイプ
        "success_rate": float,   # 成功率
        "usage_count": int,      # 使用回数
        "avg_score": float,      # 平均スコア
        "examples": list,        # 使用例
    }
}
```

#### パターンタイプ
1. **データ型変換**
   - `PIC 9(n)` → `int`
   - `PIC X(n)` → `String`
   - `PIC 9(n)V9(m)` → `BigDecimal`

2. **制御構造変換**
   - `IF ... END-IF` → `if (...) { ... }`
   - `PERFORM ... TIMES` → `for (...) { ... }`
   - `PERFORM UNTIL` → `while (...) { ... }`

3. **データ構造変換**
   - `01 LEVEL` → `class`
   - `05 LEVEL` → `field`
   - `REDEFINES` → `union` or `inheritance`

4. **ファイルI/O変換**
   - `OPEN` → `FileInputStream`
   - `READ` → `BufferedReader.readLine()`
   - `WRITE` → `PrintWriter.println()`

#### 記憶例
```python
# パターンを記憶
await memory.remember(
    content="""
    COBOL Pattern: PERFORM VARYING counter FROM 1 BY 1 UNTIL counter > limit
    Java Pattern: for (int counter = 1; counter <= limit; counter++)
    """,
    topic="migration_pattern",
    metadata={
        "pattern_type": "PERFORM_VARYING",
        "success_rate": 0.95,
        "usage_count": 150,
        "avg_score": 88.5,
    }
)

# パターンを検索
patterns = await memory.recall(
    topic="migration_pattern",
    query="PERFORM VARYING",
    limit=5,
    min_similarity=0.7,
)
```

---

### 2. 移行履歴

**目的:** 過去の移行履歴を記録

#### 記憶内容
```python
{
    "content": str,              # 履歴サマリー
    "topic": "migration_history",
    "metadata": {
        "file_name": str,        # ファイル名
        "program_id": str,       # プログラムID
        "cobol_lines": int,      # COBOL行数
        "java_lines": int,       # Java行数
        "initial_score": float,  # 初回スコア
        "final_score": float,    # 最終スコア
        "iterations": int,       # 反復回数
        "patterns_used": list,   # 使用したパターン
        "errors_fixed": list,    # 修正したエラー
        "timestamp": datetime,   # タイムスタンプ
    }
}
```

#### 記憶例
```python
# 移行履歴を記憶
await memory.remember(
    content=f"""
    Migration: {program_id}
    Initial Score: {initial_score}
    Final Score: {final_score}
    Iterations: {iterations}
    Improvement: {final_score - initial_score}
    """,
    topic="migration_history",
    metadata={
        "file_name": file_name,
        "program_id": program_id,
        "cobol_lines": cobol_lines,
        "java_lines": java_lines,
        "initial_score": initial_score,
        "final_score": final_score,
        "iterations": iterations,
        "patterns_used": patterns_used,
        "timestamp": datetime.now(),
    }
)

# 類似の移行履歴を検索
similar_migrations = await memory.recall(
    topic="migration_history",
    query=program_id,
    limit=10,
    min_similarity=0.6,
)
```

---

### 3. ベストプラクティス

**目的:** Javaのベストプラクティスを記憶

#### 記憶内容
```python
{
    "content": str,              # ベストプラクティス説明
    "topic": "best_practice",
    "metadata": {
        "category": str,         # カテゴリ
        "priority": str,         # 優先度（high/medium/low）
        "applicable_to": list,   # 適用可能なパターン
        "examples": list,        # 例
    }
}
```

#### カテゴリ
1. **命名規則**
   - クラス名: PascalCase
   - メソッド名: camelCase
   - 定数: UPPER_SNAKE_CASE

2. **デザインパターン**
   - Singleton
   - Factory
   - Strategy

3. **エラーハンドリング**
   - try-catch-finally
   - カスタム例外
   - ログ記録

4. **パフォーマンス**
   - StringBuilder使用
   - ストリームAPI活用
   - 不要なオブジェクト生成回避

#### 記憶例
```python
# ベストプラクティスを記憶
await memory.remember(
    content="""
    Best Practice: Use StringBuilder for string concatenation in loops
    Reason: String is immutable, StringBuilder is more efficient
    Example: StringBuilder sb = new StringBuilder(); for (...) { sb.append(...); }
    """,
    topic="best_practice",
    metadata={
        "category": "performance",
        "priority": "high",
        "applicable_to": ["string_operations", "loops"],
    }
)

# ベストプラクティスを検索
practices = await memory.recall(
    topic="best_practice",
    query="string concatenation",
    limit=5,
)
```

---

## 🔄 Memory System統合フロー

### 生成時の活用

```
1. COBOLParser: AST生成
   ↓
2. Memory Recall: 類似パターン検索
   ├─ migration_pattern: 移行パターン
   ├─ migration_history: 類似の移行履歴
   └─ best_practice: 適用可能なベストプラクティス
   ↓
3. JavaGenerator: パターンを参考にコード生成
   ↓
4. Reflection Loop: 品質改善
   ↓
5. Memory Store: 結果を記憶
   ├─ migration_pattern: 新しいパターン
   ├─ migration_history: 移行履歴
   └─ improvement_history: 改善履歴
```

### 実装例

```python
class CodeMigrationAssistant:
    """Code Migration Assistant."""
    
    def __init__(self, memory: MemoryManager, llm: Any):
        self.memory = memory
        self.parser = COBOLParser()
        self.generator = JavaGenerator(memory=memory)
        self.validator = MigrationValidator(llm_client=llm)
    
    async def migrate(self, cobol_code: str) -> dict[str, Any]:
        """COBOL→Java移行."""
        # 1. 解析
        parse_result = await self.parser.run({"cobol_code": cobol_code})
        ast = parse_result["ast"]
        metadata = parse_result["metadata"]
        
        # 2. Memory Recall: 類似パターン検索
        patterns = await self.memory.recall(
            topic="migration_pattern",
            query=ast["program_id"],
            limit=10,
            min_similarity=0.7,
        )
        
        similar_migrations = await self.memory.recall(
            topic="migration_history",
            query=ast["program_id"],
            limit=5,
            min_similarity=0.6,
        )
        
        best_practices = await self.memory.recall(
            topic="best_practice",
            limit=20,
        )
        
        # 3. 生成（パターンを参考に）
        gen_result = await self.generator.run({
            "ast": ast,
            "metadata": metadata,
            "patterns": patterns,
            "similar_migrations": similar_migrations,
            "best_practices": best_practices,
        })
        
        # 4. Reflection Loop
        loop = ReflectionLoop(
            generator=self.generator,
            reflector=self.validator,
            improver=self.generator,
            max_iterations=3,
        )
        
        reflection_result = await loop.execute({
            "task": "COBOL→Java移行",
            "ast": ast,
            "metadata": metadata,
            "cobol_code": cobol_code,
            "initial_output": gen_result["java_code"],
        })
        
        # 5. Memory Store: 結果を記憶
        await self._store_results(
            ast,
            reflection_result,
            patterns,
        )
        
        return {
            "java_code": reflection_result["final_output"],
            "score": reflection_result["final_score"],
            "iterations": reflection_result["iterations"],
            "history": reflection_result["history"],
        }
    
    async def _store_results(
        self,
        ast: dict,
        result: dict,
        patterns_used: list,
    ) -> None:
        """結果をMemory Systemに記憶."""
        # 移行履歴を記憶
        await self.memory.remember(
            content=f"Migration: {ast['program_id']}, Score: {result['final_score']}",
            topic="migration_history",
            metadata={
                "program_id": ast["program_id"],
                "final_score": result["final_score"],
                "iterations": result["iterations"],
                "patterns_used": [p.id for p in patterns_used],
            }
        )
        
        # 新しいパターンを記憶（スコアが高い場合）
        if result["final_score"] >= 90.0:
            await self.memory.remember(
                content=f"High-quality pattern from {ast['program_id']}",
                topic="migration_pattern",
                metadata={
                    "pattern_type": "custom",
                    "success_rate": 1.0,
                    "avg_score": result["final_score"],
                }
            )
```

---

## 📊 Memory System設定

### MemoryManager設定

```python
from agentflow.memory import MemoryManager
from agentflow.memory.embeddings import OpenAIEmbeddings
from agentflow.memory.vector_db import QdrantDB

# 埋め込みエンジン
embeddings = OpenAIEmbeddings(
    api_key="your-api-key",
    model="text-embedding-3-small"
)

# ベクトルDB
vector_db = QdrantDB(
    host="localhost",
    port=6333,
    collection_name="code_migration_memories"
)

# MemoryManager
memory = MemoryManager(
    enable_vector_search=True,
    enable_importance_adjustment=True,
    embedding_engine=embeddings,
    vector_database=vector_db,
    token_threshold=2000,
    consolidation_interval=600,
)

await memory.start()
```

---

## 🎯 次のステップ

1. ✅ Memory System統合設計完了
2. ⏭️ 実装開始
   - COBOLParser実装
   - JavaGenerator実装
   - MigrationValidator実装
   - Reflection Workflow統合
   - Memory System統合

