# Code Migration Assistant - アーキテクチャ設計

## 📋 概要

Code Migration Assistantは、COBOL→Java移行を支援するAIエージェントシステムです。Reflection Pattern + Memory Systemを活用し、高品質な移行コードを生成します。

---

## 🎯 目標

### 主要目標
1. **高品質な移行**: COBOL→Javaの正確な変換
2. **自己改善**: Reflection Patternによる品質向上
3. **学習機能**: Memory Systemによるパターン学習
4. **実用性**: 実際のレガシーシステム移行に使用可能

### 成功指標
- 構文解析精度: 95%以上
- 意味的等価性: 90%以上
- Reflection改善率: 30%以上
- パターン再利用率: 70%以上

---

## 🏗️ システムアーキテクチャ

### 全体構成（MCP工具化アーキテクチャ）

```
┌─────────────────────────────────────────────────────────────┐
│         Code Migration Assistant (Orchestrator)              │
│                                                               │
│  ┌─────────────────────────────────────────────────────┐   │
│  │           Workflow Orchestration Logic               │   │
│  │  - MCP工具调用顺序管理                                │   │
│  │  - 数据流转控制                                       │   │
│  │  - 错误处理和重试                                     │   │
│  │  - 结果聚合                                           │   │
│  └─────────────────────────────────────────────────────┘   │
│                            │                                 │
│                            ▼                                 │
│  ┌─────────────────────────────────────────────────────┐   │
│  │              MCP Protocol Layer                      │   │
│  │  - 标准化接口调用                                     │   │
│  │  - JSON序列化/反序列化                                │   │
│  │  - 工具发现和版本管理                                 │   │
│  └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
                             │
        ┌────────────────────┼────────────────────┐
        ▼                    ▼                    ▼
┌──────────────┐    ┌──────────────┐    ┌──────────────┐
│  核心工具层   │    │  辅助工具层   │    │  基盤工具层   │
├──────────────┤    ├──────────────┤    ├──────────────┤
│ COBOLParser  │    │SyntaxChecker │    │ Reflection   │
│ JavaGenerator│    │StyleChecker  │    │ Memory       │
│CodeValidator │    │TestGenerator │    │ LLM Client   │
│              │    │Complexity    │    │              │
└──────────────┘    └──────────────┘    └──────────────┘
```

### 设计理念

**Code Migration Assistant = Orchestrator（编排器）**

- **不直接实现**: 不直接实现解析、生成、验证逻辑
- **MCP调用**: 通过MCP协议调用各种工具
- **流程管理**: 管理工具之间的数据流和调用顺序
- **错误处理**: 统一的错误处理和重试机制

**优势:**
1. **松耦合**: 各工具独立开发、测试、部署
2. **可复用**: 工具可以被多个应用使用
3. **可扩展**: 容易添加新工具，支持热插拔
4. **标准化**: 统一的MCP接口，易于集成
5. **分布式**: 工具可以部署在不同的服务器上

---

### MCP工具分层

#### 1. 核心工具层（Core Tools）
- **COBOLParser**: COBOL源代码解析
- **JavaGenerator**: Java代码生成
- **CodeValidator**: 代码验证和质量评估

#### 2. 辅助工具层（Auxiliary Tools）
- **SyntaxChecker**: 语法检查（Java编译检查）
- **StyleChecker**: 代码风格检查
- **TestGenerator**: 测试代码生成
- **ComplexityAnalyzer**: 代码复杂度分析

#### 3. 基盤工具层（Foundation Tools）
- **ReflectionPattern**: 反射模式编排
- **MemorySystem**: 记忆系统
- **LLMClient**: LLM调用

---

### 传统架构 vs MCP工具化架构

#### 传统架构（紧耦合）
```
Application
  ├─ COBOLParser (内部模块)
  ├─ JavaGenerator (内部模块)
  └─ CodeValidator (内部模块)
```
**问题**: 难以复用、难以扩展、难以测试

#### MCP工具化架构（松耦合）
```
Application (Orchestrator)
  │
  ├─ MCP Call → COBOLParser Tool
  ├─ MCP Call → JavaGenerator Tool
  └─ MCP Call → CodeValidator Tool
```
**优势**: 易于复用、易于扩展、易于测试

---

## 🔄 工具调用流程

### 基本流程

```
1. Orchestrator接收COBOL代码
   ↓
2. MCP Call: COBOLParser Tool
   ↓ (返回: AST + Metadata)
3. MCP Call: MemorySystem Tool (recall patterns)
   ↓ (返回: Patterns)
4. MCP Call: ReflectionPattern Tool
   ├─ 内部 MCP Call: JavaGenerator Tool (generate)
   ├─ 内部 MCP Call: CodeValidator Tool (evaluate)
   └─ 内部 MCP Call: JavaGenerator Tool (improve)
   ↓ (返回: Final Java Code + Score)
5. MCP Call: MemorySystem Tool (remember result)
   ↓
6. 返回最终结果给用户
```

### 详细流程

```
┌─────────────────────────────────────────────────────────────┐
│              Orchestrator Workflow                           │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  Input: COBOL Code                                           │
│     │                                                         │
│     ▼                                                         │
│  ┌──────────────────────────────────────┐                   │
│  │  Step 1: Parse COBOL                 │                   │
│  │  MCP Call: COBOLParser Tool          │                   │
│  └──────────────────────────────────────┘                   │
│     │ Output: AST + Metadata                                 │
│     ▼                                                         │
│  ┌──────────────────────────────────────┐                   │
│  │  Step 2: Recall Patterns             │                   │
│  │  MCP Call: MemorySystem Tool         │                   │
│  │  Operation: recall                   │                   │
│  └──────────────────────────────────────┘                   │
│     │ Output: Patterns + History                             │
│     ▼                                                         │
│  ┌──────────────────────────────────────┐                   │
│  │  Step 3: Reflection Loop             │                   │
│  │  MCP Call: ReflectionPattern Tool    │                   │
│  │    ├─ JavaGenerator (generate)       │                   │
│  │    ├─ CodeValidator (evaluate)       │                   │
│  │    └─ JavaGenerator (improve)        │                   │
│  └──────────────────────────────────────┘                   │
│     │ Output: Final Java Code + Score                        │
│     ▼                                                         │
│  ┌──────────────────────────────────────┐                   │
│  │  Step 4: Store Result                │                   │
│  │  MCP Call: MemorySystem Tool         │                   │
│  │  Operation: remember                 │                   │
│  └──────────────────────────────────────┘                   │
│     │                                                         │
│     ▼                                                         │
│  Output: Final Result                                        │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

---

## 🧩 コンポーネント詳細

### Orchestrator（编排器）

**职责:**
- 接收用户请求
- 调用MCP工具
- 管理数据流
- 处理错误和重试
- 聚合结果

**实现:**
```python
class CodeMigrationOrchestrator:
    """Code Migration Orchestrator."""

    def __init__(self, mcp_client: MCPClient):
        self.mcp = mcp_client

    async def migrate(self, cobol_code: str) -> dict:
        """COBOL→Java移行."""
        try:
            # Step 1: Parse COBOL
            parse_result = await self.mcp.call_tool(
                tool="cobol_parser",
                input={"cobol_code": cobol_code}
            )

            # Step 2: Recall Patterns
            patterns = await self.mcp.call_tool(
                tool="memory_system",
                operation="recall",
                input={"topic": "migration_pattern"}
            )

            # Step 3: Reflection Loop
            reflection_result = await self.mcp.call_tool(
                tool="reflection_pattern",
                input={
                    "generator_tool": "java_generator",
                    "evaluator_tool": "code_validator",
                    "improver_tool": "java_generator",
                    "initial_input": {
                        "ast": parse_result["ast"],
                        "patterns": patterns["memories"]
                    }
                }
            )

            # Step 4: Store Result
            await self.mcp.call_tool(
                tool="memory_system",
                operation="remember",
                input={
                    "content": f"Migration: {parse_result['ast']['program_id']}",
                    "topic": "migration_history",
                    "metadata": {
                        "score": reflection_result["final_score"]
                    }
                }
            )

            return {
                "java_code": reflection_result["final_output"],
                "score": reflection_result["final_score"],
                "iterations": reflection_result["iterations"]
            }

        except Exception as e:
            # 错误处理
            return {"error": str(e)}
```

---

## 📊 MCP工具接口标准

### 标准输入输出格式

**输入:**
```json
{
  "tool": "tool_name",
  "version": "1.0.0",
  "input": {
    // 工具特定的输入参数
  }
}
```

**输出:**
```json
{
  "success": true,
  "output": {
    // 工具特定的输出数据
  },
  "metadata": {
    "execution_time": 1.23,
    "tool_version": "1.0.0"
  },
  "errors": []
}
```

### 错误处理

**错误输出:**
```json
{
  "success": false,
  "error": {
    "code": "ERROR_CODE",
    "message": "Error message",
    "details": {}
  }
}
```

---

## 🎯 下一步

1. ✅ MCP工具化架构设计完成
2. ⏭️ 实现各个MCP工具
3. ⏭️ 实现Orchestrator
4. ⏭️ 集成测试

详细的MCP工具设计请参考: [MCP_TOOLS_DESIGN.md](./MCP_TOOLS_DESIGN.md)
│         │       Memory System              │                │
│         │  ┌────────┐  ┌────────┐  ┌────┐ │                │
│         │  │Pattern │  │History │  │Best│ │                │
│         │  │Library │  │Records │  │Prac│ │                │
│         │  └────────┘  └────────┘  └────┘ │                │
│         └──────────────────────────────────┘                │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

---

## 🧩 コンポーネント設計

### 1. COBOLParser（COBOL解析器）

**職責:**
- COBOLソースコードの字句解析
- 構文解析とAST生成
- エラー検出と報告

**入力:**
- COBOLソースコード（文字列）

**出力:**
- AST（Abstract Syntax Tree）
- メタデータ（変数定義、プロシージャ、データ構造）

**実装方針:**
```python
class COBOLParser(AgentBlock):
    """COBOL解析器."""
    
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """COBOLコードを解析.
        
        Args:
            input_data: {
                "cobol_code": str,  # COBOLソースコード
                "file_name": str,   # ファイル名（オプション）
            }
        
        Returns:
            {
                "ast": dict,           # 抽象構文木
                "metadata": dict,      # メタデータ
                "errors": list[str],   # エラーリスト
            }
        """
```

**主要機能:**
- IDENTIFICATION DIVISION解析
- DATA DIVISION解析（変数定義）
- PROCEDURE DIVISION解析（ロジック）
- COPY文の展開
- エラーハンドリング

---

### 2. JavaGenerator（Java生成器）

**職責:**
- ASTからJavaコード生成
- 命名規則の適用
- コメント生成

**入力:**
- AST（Abstract Syntax Tree）
- メタデータ

**出力:**
- Javaソースコード
- 生成レポート

**実装方針:**
```python
class JavaGenerator(AgentBlock):
    """Java生成器."""
    
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """Javaコードを生成.
        
        Args:
            input_data: {
                "ast": dict,        # 抽象構文木
                "metadata": dict,   # メタデータ
                "style": str,       # コードスタイル（オプション）
            }
        
        Returns:
            {
                "java_code": str,      # Javaソースコード
                "report": dict,        # 生成レポート
                "warnings": list[str], # 警告リスト
            }
        """
```

**主要機能:**
- クラス構造生成
- メソッド変換
- データ型マッピング（COBOL→Java）
- 制御構造変換（PERFORM→for/while）
- エラーハンドリング変換

---

### 3. MigrationValidator（移行検証器）

**職責:**
- 生成されたJavaコードの検証
- 意味的等価性チェック
- コンパイルエラー検出

**入力:**
- 元のCOBOLコード
- 生成されたJavaコード

**出力:**
- 検証結果
- エラー・警告リスト
- 改善提案

**実装方針:**
```python
class MigrationValidator(AgentBlock):
    """移行検証器."""
    
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """移行結果を検証.
        
        Args:
            input_data: {
                "cobol_code": str,  # 元のCOBOLコード
                "java_code": str,   # 生成されたJavaコード
                "ast": dict,        # AST
            }
        
        Returns:
            {
                "is_valid": bool,         # 検証結果
                "score": float,           # 品質スコア（0-100）
                "errors": list[str],      # エラーリスト
                "warnings": list[str],    # 警告リスト
                "suggestions": list[str], # 改善提案
            }
        """
```

**検証項目:**
- 構文エラー
- データ型の一致
- ロジックの等価性
- エラーハンドリング
- パフォーマンス考慮

---

## 🔄 Reflection Pattern統合

### Reflection Workflow

**目的:** 生成されたJavaコードを自己評価・改善

**フロー:**
```
1. Generate: JavaGenerator がコード生成
2. Evaluate: MigrationValidator が品質評価
3. Improve: JavaGenerator が改善版を生成
4. 繰り返し（最大3回）
```

**実装:**
```python
from agentflow.patterns import ReflectionWorkflow

# Reflection Workflow を作成
workflow = ReflectionWorkflow.create(
    workflow_id="code-migration-reflection",
    generator=java_generator,
    llm_client=llm,
    evaluation_criteria={
        "syntax": "構文エラーがないか",
        "semantics": "意味的に等価か",
        "style": "Javaのベストプラクティスに従っているか",
        "performance": "パフォーマンスが考慮されているか",
    },
    max_iterations=3,
    acceptance_threshold=85.0,
)
```

**評価基準:**
- 構文正確性: 30点
- 意味的等価性: 40点
- コード品質: 20点
- パフォーマンス: 10点

---

## 🧠 Memory System統合

### 記憶の種類

#### 1. 移行パターンライブラリ

**目的:** 頻出する移行パターンを記憶

**記憶内容:**
- COBOLパターン → Javaパターン
- 成功事例
- 失敗事例と対策

**使用例:**
```python
# パターンを記憶
await memory.remember(
    content=f"COBOL: {cobol_pattern}\nJava: {java_pattern}",
    topic="migration_pattern",
    metadata={
        "pattern_type": "PERFORM_LOOP",
        "success_rate": 0.95,
    }
)

# パターンを検索
patterns = await memory.recall(
    topic="migration_pattern",
    query="PERFORM VARYING",
    min_similarity=0.7,
)
```

#### 2. 移行履歴

**目的:** 過去の移行履歴を記録

**記憶内容:**
- 移行元ファイル
- 移行結果
- 品質スコア
- 改善履歴

**使用例:**
```python
# 移行履歴を記憶
await memory.remember(
    content=f"File: {file_name}\nScore: {score}\nIterations: {iterations}",
    topic="migration_history",
    metadata={
        "file_name": file_name,
        "score": score,
        "timestamp": datetime.now(),
    }
)
```

#### 3. ベストプラクティス

**目的:** Javaのベストプラクティスを記憶

**記憶内容:**
- 命名規則
- デザインパターン
- エラーハンドリング
- パフォーマンス最適化

---

## 📊 データフロー

### 基本フロー

```
1. Input: COBOLソースコード
   ↓
2. COBOLParser: AST生成
   ↓
3. Memory Recall: 類似パターン検索
   ↓
4. JavaGenerator: 初期Javaコード生成
   ↓
5. Reflection Loop:
   a. MigrationValidator: 品質評価
   b. JavaGenerator: 改善版生成
   c. 繰り返し（最大3回）
   ↓
6. Memory Store: パターン・履歴を記憶
   ↓
7. Output: 最終Javaコード + レポート
```

### エラーハンドリング

**パース失敗:**
- エラー箇所を特定
- 部分的な解析結果を返す
- ユーザーに修正を促す

**生成失敗:**
- フォールバック生成
- 警告を含むレポート
- 手動修正が必要な箇所を明示

**検証失敗:**
- 詳細なエラーレポート
- 改善提案
- 最大反復回数に達した場合の処理

---

## 🔧 技術スタック

### コア技術
- **AgentFlow**: エージェント基盤
- **Reflection Pattern**: 自己改善
- **Memory System**: パターン学習

### 外部ライブラリ（検討中）
- **PLY (Python Lex-Yacc)**: COBOL字句・構文解析
- **ANTLR**: 高度な構文解析
- **Black**: Javaコードフォーマット（Java版）

---

## 📈 拡張性

### Phase 1（MVP）
- 基本的なCOBOL構文のサポート
- 単純なデータ型変換
- 基本的な制御構造変換

### Phase 2（拡張）
- 複雑なCOBOL構文のサポート
- データベースアクセス変換
- ファイルI/O変換

### Phase 3（高度）
- マルチファイル移行
- 依存関係解析
- テストコード生成

---

## 🎯 次のステップ

1. ✅ アーキテクチャ設計完了
2. ⏭️ コンポーネント詳細設計
3. ⏭️ Reflection Pattern統合設計
4. ⏭️ Memory System統合設計
5. ⏭️ 実装開始

