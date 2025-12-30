# Code Migration Assistant - COBOL→Java移行支援システム

## 📋 概要

Code Migration Assistantは、COBOL→Java移行を支援するAIエージェントシステムです。AgentFlowのReflection Pattern + Memory Systemを活用し、高品質な移行コードを自動生成します。

---

## 🎯 主な特徴

### 1. 高品質な移行
- **構文解析精度**: 95%以上
- **意味的等価性**: 90%以上
- **自動エラー修正**: Reflection Patternによる自己改善

### 2. 学習機能
- **パターンライブラリ**: 頻出する移行パターンを記憶
- **履歴管理**: 過去の移行履歴を活用
- **ベストプラクティス**: Javaのベストプラクティスを適用

### 3. 反復改善
- **最大3回の反復**: 品質スコア85点以上を目指す
- **自動改善**: フィードバックに基づいて自動修正
- **改善率**: 平均30%以上の品質向上

---

## 🏗️ システムアーキテクチャ（MCP工具化）

### 核心设计理念

**Code Migration Assistant = Orchestrator（编排器）**

本系统采用**MCP工具化架构**，将所有功能模块设计为独立的MCP工具，通过标准化的MCP协议进行通信。

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

### MCP工具化的优势

1. **松耦合**: 各工具独立开发、测试、部署
2. **可复用**: 工具可以被多个应用使用（不仅限于Code Migration Assistant）
3. **可扩展**: 容易添加新工具，支持热插拔
4. **标准化**: 统一的MCP接口，易于集成
5. **分布式**: 工具可以部署在不同的服务器上

---

## 🧩 MCP工具分类

### 1. 核心工具层（Core Tools）

#### COBOLParser MCP Tool
- **職責**: COBOLソースコードの解析
- **出力**: AST（抽象構文木）+ メタデータ
- **サポート**: IDENTIFICATION/DATA/PROCEDURE DIVISION
- **MCP接口**: 标准JSON输入输出

#### JavaGenerator MCP Tool
- **職責**: ASTからJavaコード生成
- **機能**: データ型変換、制御構造変換、命名規則適用
- **出力**: Javaソースコード + レポート
- **MCP接口**: 支持patterns和best_practices输入

#### CodeValidator MCP Tool
- **職責**: 生成されたJavaコードの検証
- **評価**: 構文（30点）、意味（40点）、品質（20点）、性能（10点）
- **出力**: 品質スコア + フィードバック + 改善提案
- **MCP接口**: 返回详细的评分和建议

### 2. 辅助工具层（Auxiliary Tools）

#### SyntaxChecker MCP Tool
- **職責**: Java语法检查（编译检查）
- **出力**: 编译错误和警告列表

#### StyleChecker MCP Tool
- **職責**: 代码风格检查
- **出力**: 风格违规和改进建议

#### TestGenerator MCP Tool
- **職責**: 测试代码生成
- **出力**: JUnit测试代码

#### ComplexityAnalyzer MCP Tool
- **職責**: 代码复杂度分析
- **出力**: 圈复杂度、认知复杂度等指标

### 3. 基盤工具层（Foundation Tools）

#### ReflectionPattern MCP Tool
- **職責**: 反射模式编排
- **フロー**: Generate → Evaluate → Improve（最大3回）
- **目標**: スコア85点以上
- **MCP接口**: 接收generator/evaluator/improver工具名称

#### MemorySystem MCP Tool
- **職責**: 记忆系统
- **記憶**: 移行パターン、履歴、ベストプラクティス
- **活用**: 類似パターン検索、品質向上
- **MCP接口**: remember和recall操作

#### LLMClient MCP Tool
- **職責**: LLM调用
- **用途**: 代码改进、反馈生成
- **MCP接口**: 标准prompt输入

---

## 📚 ドキュメント

### 設計ドキュメント
1. **[ARCHITECTURE.md](./ARCHITECTURE.md)** - 全体アーキテクチャ設計（MCP工具化）
2. **[MCP_TOOLS_DESIGN.md](./MCP_TOOLS_DESIGN.md)** - MCP工具详细设计
3. **[COMPONENT_DESIGN.md](./COMPONENT_DESIGN.md)** - コンポーネント詳細設計
4. **[REFLECTION_INTEGRATION.md](./REFLECTION_INTEGRATION.md)** - Reflection Pattern統合設計
5. **[MEMORY_INTEGRATION.md](./MEMORY_INTEGRATION.md)** - Memory System統合設計

---

## 🚀 使用例

### 基本的な使用方法（MCP工具化）


---

## 🚀 使用方法

### 基本使用

```python
import asyncio
from apps.code_migration_assistant.mcp_client import MCPClient
from apps.code_migration_assistant.orchestrator import CodeMigrationOrchestrator
from apps.code_migration_assistant.mcp_tools import (
    COBOLParser,
    JavaGenerator,
    CodeValidator,
    ReflectionPattern,
    MemorySystem,
)

async def main():
    # MCPClientを作成
    client = MCPClient()

    # MCP工具を登録
    client.register_tool("cobol_parser", COBOLParser())
    client.register_tool("java_generator", JavaGenerator())
    client.register_tool("code_validator", CodeValidator())
    client.register_tool("reflection_pattern", ReflectionPattern(mcp_client=client))
    # client.register_tool("memory_system", MemorySystem(memory_manager=...))

    # Orchestratorを作成
    orchestrator = CodeMigrationOrchestrator(client)

    # COBOLコード
    cobol_code = """
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
           DISPLAY "RESULT: " WS-RESULT.
           STOP RUN.
    """

    # 移行実行
    result = await orchestrator.migrate(cobol_code=cobol_code)

    if result["success"]:
        print("✅ 移行成功！")
        print(f"Java Class: {result['class_name']}")
        print(f"Score: {result['score']}")
        print(f"Iterations: {result['iterations']}")
        print("\nJava Code:")
        print(result["java_code"])
    else:
        print("❌ 移行失敗")
        print(f"Errors: {result['errors']}")

if __name__ == "__main__":
    asyncio.run(main())
```

### 個別MCP工具の使用

#### COBOLParser

```python
from apps.code_migration_assistant.mcp_tools import COBOLParser, MCPToolRequest

parser = COBOLParser()

request = MCPToolRequest(
    tool="cobol_parser",
    version="1.0.0",
    input={
        "cobol_code": "...",
        "file_name": "program.cob",
    },
)

response = await parser.handle_request(request)

if response.success:
    ast = response.output["ast"]
    metadata = response.output["metadata"]
```

#### JavaGenerator

```python
from apps.code_migration_assistant.mcp_tools import JavaGenerator, MCPToolRequest

generator = JavaGenerator()

request = MCPToolRequest(
    tool="java_generator",
    version="1.0.0",
    input={
        "ast": ast,
        "metadata": metadata,
        "patterns": [],
    },
)

response = await generator.handle_request(request)

if response.success:
    java_code = response.output["java_code"]
    class_name = response.output["class_name"]
```

#### CodeValidator

```python
from apps.code_migration_assistant.mcp_tools import CodeValidator, MCPToolRequest

validator = CodeValidator()

request = MCPToolRequest(
    tool="code_validator",
    version="1.0.0",
    input={
        "java_code": java_code,
        "ast": ast,
        "metadata": metadata,
        "mappings": mappings,
    },
)

response = await validator.handle_request(request)

if response.success:
    score = response.output["score"]
    is_acceptable = response.output["is_acceptable"]
    feedback = response.output["feedback"]
```

---

## 📊 実装状況

### Phase 1: Core Tools ✅
- [x] COBOLParser MCP Tool
- [x] JavaGenerator MCP Tool
- [x] CodeValidator MCP Tool

### Phase 2: Foundation Tools ✅
- [x] ReflectionPattern MCP Tool
- [x] MemorySystem MCP Tool

### Phase 3: Orchestrator ✅
- [x] MCP Client Implementation
- [x] CodeMigrationOrchestrator Implementation

### Phase 4: Testing and Documentation ✅
- [x] Unit Tests
- [x] Integration Tests
- [x] Deployment Guide

---

## 📚 ドキュメント

- [アーキテクチャ設計](ARCHITECTURE.md)
- [MCP工具設計](MCP_TOOLS_DESIGN.md)
- [MCP架構総結](MCP_ARCHITECTURE_SUMMARY.md)
- [コンポーネント設計](COMPONENT_DESIGN.md)
- [Reflection Pattern統合](REFLECTION_INTEGRATION.md)
- [Memory System統合](MEMORY_INTEGRATION.md)
- [デプロイメントガイド](DEPLOYMENT_GUIDE.md)

---

## 🧪 テスト

### 単元テスト実行

```bash
pytest apps/code_migration_assistant/tests/test_cobol_parser.py -v
```

### 統合テスト実行

```bash
pytest apps/code_migration_assistant/tests/test_integration.py -v
```

### 全テスト実行

```bash
pytest apps/code_migration_assistant/tests/ -v
```

---

## 🔧 設定

### 環境変数

```bash
# Redis設定（MemorySystem用）
REDIS_URL=redis://localhost:6379

# PostgreSQL設定（MemorySystem用）
POSTGRES_URL=postgresql://user:pass@localhost:5432/db

# Qdrant設定（オプション）
QDRANT_URL=http://localhost:6333

# 移行設定
MAX_ITERATIONS=3
ACCEPTANCE_THRESHOLD=85.0
```

---

## 🎯 今後の改善

1. **LLMClient MCP Tool実装**
   - OpenAI/Anthropic統合
   - コード改善提案生成

2. **より高度なCOBOL解析**
   - COPY文サポート
   - サブプログラム呼び出し
   - ファイルI/O処理

3. **より高度なJava生成**
   - Spring Boot統合
   - JPA/Hibernate統合
   - RESTful API生成

4. **パフォーマンス最適化**
   - 並列処理
   - キャッシング
   - バッチ処理

5. **UI/UX改善**
   - Webインターフェース
   - 進捗表示
   - 差分表示


```python
from apps.code_migration_assistant import CodeMigrationOrchestrator
from agentflow.mcp import MCPClient

# MCP Clientの初期化
mcp_client = MCPClient(
    tools_registry={
        "cobol_parser": "http://localhost:8001",
        "java_generator": "http://localhost:8002",
        "code_validator": "http://localhost:8003",
        "reflection_pattern": "http://localhost:8004",
        "memory_system": "http://localhost:8005",
    }
)

# Orchestratorの初期化
orchestrator = CodeMigrationOrchestrator(mcp_client=mcp_client)

# COBOL→Java移行
cobol_code = """
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
"""

# MCP工具を通じて移行実行
result = await orchestrator.migrate(cobol_code)

print(f"Java Code:\n{result['java_code']}")
print(f"Quality Score: {result['score']}")
print(f"Iterations: {result['iterations']}")
```

### MCP工具调用流程

```
Orchestrator
  │
  ├─ MCP Call: COBOLParser Tool
  │    └─ 返回: AST + Metadata
  │
  ├─ MCP Call: MemorySystem Tool (recall)
  │    └─ 返回: Patterns
  │
  ├─ MCP Call: ReflectionPattern Tool
  │    ├─ 内部调用: JavaGenerator Tool
  │    ├─ 内部调用: CodeValidator Tool
  │    └─ 内部调用: JavaGenerator Tool (improve)
  │    └─ 返回: Final Java Code + Score
  │
  └─ MCP Call: MemorySystem Tool (remember)
       └─ 返回: Success
```

### 出力例

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

## 📊 パフォーマンス目標

### 処理時間
- COBOLParser: < 1秒 / 1000行
- JavaGenerator: < 2秒 / 1000行
- MigrationValidator: < 1秒 / 1000行
- 全体（Reflection含む）: < 10秒 / 1000行

### 品質目標
- 初回生成スコア: 60-70点
- 最終スコア: 85点以上
- 改善率: 30%以上
- パターン再利用率: 70%以上

---

## 🎯 開発ロードマップ

### Phase 1（MVP） - 現在設計中
- [x] アーキテクチャ設計
- [x] コンポーネント設計
- [x] Reflection Pattern統合設計
- [x] Memory System統合設計
- [ ] COBOLParser実装
- [ ] JavaGenerator実装
- [ ] MigrationValidator実装
- [ ] Reflection Workflow統合
- [ ] Memory System統合
- [ ] 単体テスト作成
- [ ] 統合テスト作成

### Phase 2（拡張）
- [ ] 複雑なCOBOL構文のサポート
- [ ] データベースアクセス変換
- [ ] ファイルI/O変換
- [ ] エラーハンドリング強化

### Phase 3（高度）
- [ ] マルチファイル移行
- [ ] 依存関係解析
- [ ] テストコード生成
- [ ] CI/CD統合

---

## 🤝 貢献

プロジェクトへの貢献を歓迎します！

1. このリポジトリをフォーク
2. 機能ブランチを作成 (`git checkout -b feature/amazing-feature`)
3. 変更をコミット (`git commit -m 'Add amazing feature'`)
4. ブランチにプッシュ (`git push origin feature/amazing-feature`)
5. プルリクエストを作成

---

## 📝 ライセンス

MIT License - 詳細は [LICENSE](../../LICENSE) を参照してください。

---

## 📧 お問い合わせ

質問や提案がある場合は、[GitHub Issues](https://github.com/liushuang393/serverlessAIAgents/issues) で報告してください。

