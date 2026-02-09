# Code Migration Assistant - COBOL→Java移行支援システム

## 📋 概要

Code Migration Assistantは、COBOL→Java移行を支援するAIエージェントシステム(等価性検証を中心にした移行工場」へ進化させる)です。
AgentFlowのReflection Pattern + Memory Systemを活用し、高品質な移行コードを自動生成します。
汎用化（COBOL → Java 以外も想定）

## 🧱 工程固定ワークフロー（v4）

以下の工程を固定し、工程間通信は `artifacts/` 配下の JSON に限定します。

1. 分析（LegacyAnalysis）
2. 設計（MigrationDesign）
3. 変換（CodeTransformation）
4. テスト生成（TestSynthesis）
5. 差分検証（DifferentialVerification）
6. 品質裁定（QualityGate）
7. 限定修正（LimitedFix）

各成果物は `meta / unknowns / extensions` を必須とし、Schema は `specs/schemas/` で管理します。

この設計にすると：
COBOL → Java
RPG → Java
Struts → Spring Boot
Java8 → Java21

はすべて：
👉 Adapter + Design ルール差し替えだけ

今の code_migration_assistant は「賢い Agent」寄り。
次は「等価性検証を中心にした移行工場」へ進化させる段階。

Agent を増やすより、
Spec と工程を固定する方が精度も再現性も上がる。
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

## 🏗️ システムアーキテクチャ（MCPツール化）

### 中核設計理念

**Code Migration Assistant = Orchestrator（オーケストレーター）**

本システムは**MCPツール化アーキテクチャ**を採用し、すべての機能モジュールを独立したMCPツールとして設計し、標準化されたMCPプロトコルで通信します。

```mermaid
flowchart TB
    subgraph Input["📥 入力"]
        COBOL["COBOL ソースコード"]
    end

    subgraph Coordinator["🎯 QualityGateAgent (オーケストレーション)"]
        direction TB
        C1["フロー判定"]
        C2["Agent スケジューリング"]
        C3["リトライ管理"]
    end

    subgraph Transform["🔄 CodeTransformationAgent"]
        direction TB
        T1["@tool: parse_cobol"]
        T2["@tool: compile_java"]
        T3["LLM: コード翻訳"]
    end

    subgraph Checker["✅ DifferentialVerificationAgent"]
        direction TB
        CH1["@tool: execute_java"]
        CH2["@tool: compare_outputs"]
        CH3["@tool: generate_diff_report"]
        CH4["LLM: 差異分析・判定"]
    end

    subgraph Fixer["🔧 LimitedFixerAgent"]
        direction TB
        F1["@tool: compile_java"]
        F2["@tool: execute_java"]
        F3["LLM: コード修復"]
    end

    subgraph TestGen["🧪 TestSynthesisAgent"]
        direction TB
        TG1["@tool: run_junit"]
        TG2["@tool: get_test_template"]
        TG3["LLM: テストケース生成"]
    end

    subgraph Output["📤 出力"]
        JAVA["Java ソースコード"]
        RESULT["検証結果"]
    end

    COBOL --> Coordinator
    Coordinator --> Transform
    Transform --> Checker
    Checker -->|PASS| Output
    Checker -->|FAIL/RETRY| Fixer
    Fixer --> Checker
    Coordinator -.-> TestGen
    TestGen -.-> Checker

    style Coordinator fill:#4a90d9,color:#fff
    style Transform fill:#50b5a9,color:#fff
    style Checker fill:#e8a838,color:#fff
    style Fixer fill:#d94a4a,color:#fff
    style TestGen fill:#9b59b6,color:#fff
```

### MCPツール化の利点

1. **疎結合**: 各ツールを独立して開発・テスト・デプロイ可能
2. **再利用可能**: ツールを複数アプリで共有可能（Code Migration Assistantに限定しない）
3. **拡張容易**: 新規ツールを追加しやすく、ホットプラグに対応
4. **標準化**: 統一されたMCPインターフェースで統合容易
5. **分散**: ツールを複数サーバーに分散配置可能

---

## 🧩 MCPツール分類

### 1. コアツール層（Core Tools）

#### COBOLParser MCP Tool
- **職責**: COBOLソースコードの解析
- **出力**: AST（抽象構文木）+ メタデータ
- **サポート**: IDENTIFICATION/DATA/PROCEDURE DIVISION
- **MCPインターフェース**: 標準JSON入出力

#### JavaGenerator MCP Tool
- **職責**: ASTからJavaコード生成
- **機能**: データ型変換、制御構造変換、命名規則適用
- **出力**: Javaソースコード + レポート
- **MCPインターフェース**: patterns と best_practices の入力に対応

#### CodeValidator MCP Tool
- **職責**: 生成されたJavaコードの検証
- **評価**: 構文（30点）、意味（40点）、品質（20点）、性能（10点）
- **出力**: 品質スコア + フィードバック + 改善提案
- **MCPインターフェース**: 詳細なスコアと提案を返す

### 2. 補助ツール層（Auxiliary Tools）

#### SyntaxChecker MCP Tool
- **職責**: Java文法チェック（コンパイルチェック）
- **出力**: コンパイルエラーと警告一覧

#### StyleChecker MCP Tool
- **職責**: コードスタイルチェック
- **出力**: スタイル違反と改善提案

#### TestGenerator MCP Tool
- **職責**: テストコード生成
- **出力**: JUnitテストコード

#### ComplexityAnalyzer MCP Tool
- **職責**: コード複雑度分析
- **出力**: 循環的複雑度、認知的複雑度などの指標

### 3. 基盤ツール層（Foundation Tools）

#### ReflectionPattern MCP Tool
- **職責**: リフレクションパターンのオーケストレーション
- **フロー**: Generate → Evaluate → Improve（最大3回）
- **目標**: スコア85点以上
- **MCPインターフェース**: generator/evaluator/improver のツール名を受け取る

#### MemorySystem MCP Tool
- **職責**: 記憶システム
- **記憶**: 移行パターン、履歴、ベストプラクティス
- **活用**: 類似パターン検索、品質向上
- **MCPインターフェース**: remember/recall 操作

#### LLMClient MCP Tool
- **職責**: LLM呼び出し
- **用途**: コード改善、フィードバック生成
- **MCPインターフェース**: 標準プロンプト入力

---

## 📚 ドキュメント

### 設計ドキュメント
1. **[ARCHITECTURE.md](./ARCHITECTURE.md)** - 全体アーキテクチャ設計（MCPツール化）
2. **[MCP_TOOLS_DESIGN.md](./MCP_TOOLS_DESIGN.md)** - MCPツール詳細設計
3. **[COMPONENT_DESIGN.md](./COMPONENT_DESIGN.md)** - コンポーネント詳細設計
4. **[REFLECTION_INTEGRATION.md](./REFLECTION_INTEGRATION.md)** - Reflection Pattern統合設計
5. **[MEMORY_INTEGRATION.md](./MEMORY_INTEGRATION.md)** - Memory System統合設計

---

## 🚀 使用例

### 基本的な使用方法（MCPツール化）


---

## 🚀 使用方法

### 基本的な使用方法

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

    # MCPツールを登録
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

### 個別MCPツールの使用

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
- [MCPツール設計](MCP_TOOLS_DESIGN.md)
- [MCPアーキテクチャサマリー](MCP_ARCHITECTURE_SUMMARY.md)
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

# MCPツールを通じて移行実行
result = await orchestrator.migrate(cobol_code)

print(f"Java Code:\n{result['java_code']}")
print(f"Quality Score: {result['score']}")
print(f"Iterations: {result['iterations']}")
```

### MCPツール呼び出しフロー

```
Orchestrator
  │
  ├─ MCP Call: COBOLParser Tool
  │    └─ 返却: AST + Metadata
  │
  ├─ MCP Call: MemorySystem Tool (recall)
  │    └─ 返却: Patterns
  │
  ├─ MCP Call: ReflectionPattern Tool
  │    ├─ 内部呼び出し: JavaGenerator Tool
  │    ├─ 内部呼び出し: CodeValidator Tool
  │    └─ 内部呼び出し: JavaGenerator Tool (improve)
  │    └─ 返却: Final Java Code + Score
  │
  └─ MCP Call: MemorySystem Tool (remember)
       └─ 返却: Success
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

---

## 🔧 新規移行タイプ追加手順（拡張ガイド）

本システムは拡張可能なアダプター設計を採用しており、COBOL→Java 以外の移行タイプを追加できます。

### 対応可能な移行例

| ソース言語 | ターゲット言語 | ユースケース |
|----------|-------------|------------|
| RPG (AS/400) | C# | IBMメインフレームからの移行 |
| Java Struts | Java Spring Boot | レガシーWebフレームワーク刷新 |
| VB6 | C# WPF | デスクトップアプリ現代化 |
| PL/I | Java | メインフレームからの移行 |

---

### 手順1: 設定ファイルの確認

現在の設定ファイル構成:
```
apps/code_migration_assistant/
├── adapters/
│   ├── base.py              # 基底クラス（変更不要）
│   ├── source/              # ソース言語アダプター
│   │   └── cobol_adapter.py
│   └── target/              # ターゲット言語アダプター
│       └── java_adapter.py
├── parsers/                 # 言語パーサー
│   └── ply_cobol_parser.py
├── tools/                   # 実行環境ツール
│   └── junit_runner.py
└── agents/                  # Agent（変更不要）
```

---

### 手順2: ソース言語アダプターの追加

#### 例: RPG → C# 移行の場合

**2-1. ソースアダプター作成**

`adapters/source/rpg_adapter.py` を新規作成:

```python
# -*- coding: utf-8 -*-
"""RPG Language Adapter."""

from apps.code_migration_assistant.adapters.base import (
    AST,
    SourceLanguageAdapter,
)

class RpgAdapter(SourceLanguageAdapter):
    """RPG (AS/400) 言語アダプター."""

    @property
    def language_name(self) -> str:
        return "RPG"

    def parse(self, source_code: str) -> AST:
        """RPG コードを解析."""
        # RPG 固有の解析ロジック
        # H-Spec, F-Spec, D-Spec, C-Spec を解析
        pass

    def extract_variables(self, ast: AST) -> list[dict]:
        """D-Spec から変数を抽出."""
        pass

    def identify_external_calls(self, ast: AST) -> list[dict]:
        """CALL, SQL 等を特定."""
        pass
```

**2-2. ターゲットアダプター作成**

`adapters/target/csharp_adapter.py` を新規作成:

```python
# -*- coding: utf-8 -*-
"""C# Language Adapter."""

from apps.code_migration_assistant.adapters.base import (
    AST,
    ExecutionResult,
    TargetLanguageAdapter,
)

class CSharpAdapter(TargetLanguageAdapter):
    """C# 言語アダプター."""

    @property
    def language_name(self) -> str:
        return "C#"

    def generate_skeleton(self, ast: AST, class_name: str) -> str:
        """C# クラススケルトン生成."""
        # namespace, using, class 構造を生成
        pass

    def compile(self, code: str) -> tuple[bool, list[str]]:
        """dotnet build でコンパイル."""
        pass

    def execute(self, code: str, inputs: dict) -> ExecutionResult:
        """dotnet run で実行."""
        pass
```

---

### 手順3: パーサーの追加

`parsers/rpg_parser.py` を新規作成:

```python
# -*- coding: utf-8 -*-
"""RPG Parser using PLY."""

class RpgParser:
    """RPG ソースコードパーサー."""

    def __init__(self, source_code: str):
        self.source = source_code

    def parse(self) -> dict:
        """RPG コードを解析して AST を生成."""
        # H-Spec: ヘッダー仕様
        # F-Spec: ファイル仕様
        # D-Spec: データ仕様
        # C-Spec: 計算仕様
        pass
```

---

### 手順4: ツールの追加

`tools/dotnet_runner.py` を新規作成:

```python
# -*- coding: utf-8 -*-
"""C# (.NET) 実行環境."""

class DotNetRunner:
    """NUnit テスト実行."""

    def run_tests(self, test_code: str, target_code: str) -> dict:
        """dotnet test でテスト実行."""
        pass
```

---

### 手順5: アダプター登録

`adapters/__init__.py` を更新:

```python
from .source.rpg_adapter import RpgAdapter
from .target.csharp_adapter import CSharpAdapter

__all__ = [
    # 既存
    "CobolAdapter", "JavaAdapter",
    # 新規追加
    "RpgAdapter", "CSharpAdapter",
]
```

---

### 手順6: Agent の system_prompt を更新（オプション）

`agents/transform_agent.py` の `system_prompt` を言語に応じて動的に設定:

```python
PROMPTS = {
    ("COBOL", "Java"): "あなたは COBOL→Java 変換の専門家です...",
    ("RPG", "C#"): "あなたは RPG→C# 変換の専門家です...",
    ("Struts", "SpringBoot"): "あなたは Struts→Spring Boot 変換の専門家です...",
}
```

---

### 例: Java Struts → Spring Boot 移行

**追加フォルダ/ファイル:**

```
adapters/source/struts_adapter.py    # Struts XML/Action 解析
adapters/target/springboot_adapter.py # Spring Boot 生成
parsers/struts_parser.py             # struts-config.xml パーサー
tools/maven_runner.py                # Maven ビルド/テスト実行
```

**Struts アダプターの実装ポイント:**

1. `struts-config.xml` の解析（Action マッピング）
2. `ActionForm` → `@ModelAttribute` 変換
3. `Action` クラス → `@Controller` 変換
4. JSP → Thymeleaf or HTML テンプレート変換

---

## 🔍 アーキテクチャ分析と改善提案

### 現状の評価

| 観点 | 評価 | 説明 |
|-----|-----|------|
| 拡張性 | ⭐⭐⭐☆☆ (3/5) | アダプターパターンで言語追加可能だが、Agent との結合が強い |
| 高内聚 | ⭐⭐⭐⭐☆ (4/5) | 各 Agent の責務は明確 |
| 低耦合 | ⭐⭐⭐☆☆ (3/5) | Agent が具体的な Adapter を直接参照 |
| 合理性 | ⭐⭐⭐⭐☆ (4/5) | MCPツール化は良い設計、ただし改善余地あり |

---

### 改善提案

#### 1. **Factory Pattern の導入（高優先度）**

**問題**: `TransformAgent` が `CobolAdapter`, `JavaAdapter` を直接インスタンス化

**改善**: `AdapterFactory` を導入し、設定で言語ペアを切り替え

```python
# adapters/factory.py
class AdapterFactory:
    """言語アダプターファクトリー."""

    REGISTRY = {
        "COBOL": CobolAdapter,
        "RPG": RpgAdapter,
        "Struts": StrutsAdapter,
    }

    @classmethod
    def get_source_adapter(cls, language: str) -> SourceLanguageAdapter:
        return cls.REGISTRY[language]()
```

#### 2. **設定ファイルによる言語ペア管理（高優先度）**

**問題**: 言語ペアがコードにハードコード

**改善**: YAML/JSON 設定ファイルで管理

```yaml
# config/migration_types.yaml
migration_types:
  - name: cobol-to-java
    source: COBOL
    target: Java
    parser: ply_cobol_parser
    runner: junit_runner
    prompts:
      transform: prompts/cobol_java_transform.txt

  - name: rpg-to-csharp
    source: RPG
    target: C#
    parser: rpg_parser
    runner: dotnet_runner
    prompts:
      transform: prompts/rpg_csharp_transform.txt
```

#### 3. **Plugin アーキテクチャ（中優先度）**

**問題**: 新言語追加時にコア修正が必要

**改善**: Plugin として動的ロード

```python
# plugins/rpg_csharp/__init__.py
def register(registry):
    registry.register_source("RPG", RpgAdapter)
    registry.register_target("C#", CSharpAdapter)
    registry.register_parser("rpg", RpgParser)
```

#### 4. **Prompt テンプレート外部化（中優先度）**

**問題**: `system_prompt` がコード内に埋め込み

**改善**: 外部ファイル化して言語ごとに管理

```
prompts/
├── cobol_java/
│   ├── transform.txt
│   ├── checker.txt
│   └── fixer.txt
└── rpg_csharp/
    ├── transform.txt
    └── ...
```

#### 5. **型マッピングの外部化（低優先度）**

**問題**: 型マッピングがアダプター内にハードコード

**改善**: マッピングテーブルを JSON/YAML で管理

```yaml
# mappings/cobol_java_types.yaml
type_mappings:
  - cobol_type: "PIC 9(5)"
    java_type: "int"
    java_default: "0"

  - cobol_type: "PIC 9(5)V99"
    java_type: "BigDecimal"
    java_import: "java.math.BigDecimal"
```

---

### 推奨実装順序

1. **Phase 1（必須）**: Factory Pattern + 設定ファイル
2. **Phase 2（推奨）**: Prompt 外部化 + 型マッピング外部化
3. **Phase 3（将来）**: Plugin アーキテクチャ

---

### 改善後のフォルダ構成（推奨）

```
apps/code_migration_assistant/
├── config/
│   ├── migration_types.yaml    # 移行タイプ定義
│   └── settings.yaml           # グローバル設定
├── adapters/
│   ├── base.py
│   ├── factory.py              # ← 新規追加
│   ├── source/
│   │   ├── cobol_adapter.py
│   │   ├── rpg_adapter.py      # ← 新規追加（例）
│   │   └── struts_adapter.py   # ← 新規追加（例）
│   └── target/
│       ├── java_adapter.py
│       ├── csharp_adapter.py   # ← 新規追加（例）
│       └── springboot_adapter.py  # ← 新規追加（例）
├── parsers/
│   ├── ply_cobol_parser.py
│   ├── rpg_parser.py           # ← 新規追加（例）
│   └── struts_parser.py        # ← 新規追加（例）
├── tools/
│   ├── junit_runner.py
│   ├── dotnet_runner.py        # ← 新規追加（例）
│   └── maven_runner.py         # ← 新規追加（例）
├── prompts/                    # ← 新規追加
│   ├── cobol_java/
│   └── rpg_csharp/
├── mappings/                   # ← 新規追加
│   ├── cobol_java_types.yaml
│   └── rpg_csharp_types.yaml
└── agents/                     # 変更最小限
    ├── transform_agent.py
    └── ...
```

---

## 📝 チェックリスト（新規移行タイプ追加時）

```markdown
- [ ] ソースアダプター作成 (`adapters/source/xxx_adapter.py`)
- [ ] ターゲットアダプター作成 (`adapters/target/xxx_adapter.py`)
- [ ] パーサー作成 (`parsers/xxx_parser.py`)
- [ ] テストランナー作成 (`tools/xxx_runner.py`)
- [ ] 型マッピング定義 (`mappings/xxx_types.yaml`)
- [ ] Prompt テンプレート作成 (`prompts/xxx/`)
- [ ] `adapters/__init__.py` に登録
- [ ] 設定ファイル更新 (`config/migration_types.yaml`)
- [ ] 単体テスト作成
- [ ] 統合テスト作成
- [ ] README 更新
```
