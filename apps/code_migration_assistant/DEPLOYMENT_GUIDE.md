# Code Migration Assistant - デプロイメントガイド

## 📋 目次

1. [概要](#概要)
2. [前提条件](#前提条件)
3. [MCP工具のデプロイ](#mcp工具のデプロイ)
4. [Orchestratorのデプロイ](#orchestratorのデプロイ)
5. [設定](#設定)
6. [テスト](#テスト)
7. [トラブルシューティング](#トラブルシューティング)

---

## 概要

Code Migration Assistantは、COBOL→Java移行を支援するMCPベースのシステムです。

**アーキテクチャ:**
```
┌─────────────────────────────────────────┐
│  CodeMigrationOrchestrator              │
│  (編排器)                                │
└─────────────────────────────────────────┘
            ↓ MCP Protocol
┌─────────────────────────────────────────┐
│  MCP Tools (独立サービス)                │
│  - COBOLParser                          │
│  - JavaGenerator                        │
│  - CodeValidator                        │
│  - ReflectionPattern                    │
│  - MemorySystem                         │
└─────────────────────────────────────────┘
```

---

## 前提条件

### システム要件
- Python 3.11+
- Redis 7.0+ (MemorySystem用)
- PostgreSQL 14+ (MemorySystem用)
- Qdrant 1.7+ (MemorySystem用、オプション)

### Pythonパッケージ
```bash
pip install -r requirements.txt
```

**主要パッケージ:**
- `pydantic>=2.0.0` - データ検証
- `redis>=5.0.0` - Redis接続
- `psycopg2-binary>=2.9.0` - PostgreSQL接続
- `pytest>=7.4.0` - テスト
- `pytest-asyncio>=0.21.0` - 非同期テスト

---

## MCP工具のデプロイ

### 1. COBOLParser

**機能:** COBOLソースコードを解析してASTとメタデータを生成

**デプロイ方法:**
```python
from apps.code_migration_assistant.mcp_tools import COBOLParser

# 工具を作成
parser = COBOLParser()

# MCPClientに登録
client.register_tool("cobol_parser", parser)
```

**設定:** なし

---

### 2. JavaGenerator

**機能:** ASTからJavaコードを生成

**デプロイ方法:**
```python
from apps.code_migration_assistant.mcp_tools import JavaGenerator

# 工具を作成
generator = JavaGenerator()

# MCPClientに登録
client.register_tool("java_generator", generator)
```

**設定:** なし

---

### 3. CodeValidator

**機能:** 生成されたJavaコードを検証

**デプロイ方法:**
```python
from apps.code_migration_assistant.mcp_tools import CodeValidator

# 工具を作成
validator = CodeValidator()

# MCPClientに登録
client.register_tool("code_validator", validator)
```

**設定:**
- `acceptance_threshold`: 受け入れ閾値（デフォルト: 85.0）

---

### 4. ReflectionPattern

**機能:** Generate → Evaluate → Improve ループを編排

**デプロイ方法:**
```python
from apps.code_migration_assistant.mcp_tools import ReflectionPattern

# 工具を作成（MCPClientを注入）
reflection = ReflectionPattern(mcp_client=client)

# MCPClientに登録
client.register_tool("reflection_pattern", reflection)
```

**設定:**
- `max_iterations`: 最大反復回数（デフォルト: 3）
- `acceptance_threshold`: 受け入れ閾値（デフォルト: 85.0）

---

### 5. MemorySystem

**機能:** 移行パターン、履歴、ベストプラクティスを記憶・想起

**デプロイ方法:**
```python
from agentflow.memory import MemoryManager
from apps.code_migration_assistant.mcp_tools import MemorySystem

# MemoryManagerを作成
memory_manager = MemoryManager(
    redis_url="redis://localhost:6379",
    postgres_url="postgresql://user:pass@localhost:5432/db",
)

# 工具を作成（MemoryManagerを注入）
memory = MemorySystem(memory_manager=memory_manager)

# MCPClientに登録
client.register_tool("memory_system", memory)
```

**設定:**
- Redis URL
- PostgreSQL URL
- Qdrant URL（オプション）

---

## Orchestratorのデプロイ

### 基本デプロイ

```python
from apps.code_migration_assistant.mcp_client import MCPClient
from apps.code_migration_assistant.orchestrator import CodeMigrationOrchestrator
from apps.code_migration_assistant.mcp_tools import (
    COBOLParser,
    JavaGenerator,
    CodeValidator,
    ReflectionPattern,
    MemorySystem,
)

# MCPClientを作成
client = MCPClient()

# MCP工具を登録
client.register_tool("cobol_parser", COBOLParser())
client.register_tool("java_generator", JavaGenerator())
client.register_tool("code_validator", CodeValidator())
client.register_tool("reflection_pattern", ReflectionPattern(mcp_client=client))
client.register_tool("memory_system", MemorySystem(memory_manager=memory_manager))

# Orchestratorを作成
orchestrator = CodeMigrationOrchestrator(client)

# 移行実行
result = await orchestrator.migrate(cobol_code="...")
```

---

## 設定

### 環境変数

```bash
# Redis設定
REDIS_URL=redis://localhost:6379

# PostgreSQL設定
POSTGRES_URL=postgresql://user:pass@localhost:5432/db

# Qdrant設定（オプション）
QDRANT_URL=http://localhost:6333

# 移行設定
MAX_ITERATIONS=3
ACCEPTANCE_THRESHOLD=85.0
```

---

## テスト

### 単元テスト

```bash
# 全テスト実行
pytest apps/code_migration_assistant/tests/

# 特定テスト実行
pytest apps/code_migration_assistant/tests/test_cobol_parser.py
```

### 統合テスト

```bash
pytest apps/code_migration_assistant/tests/test_integration.py
```

---

## トラブルシューティング

### 問題: MCP工具が見つからない

**原因:** 工具が登録されていない

**解決策:**
```python
# 工具が登録されているか確認
print(client.list_tools())

# 工具を登録
client.register_tool("tool_name", tool_instance)
```

### 問題: MemorySystemエラー

**原因:** Redis/PostgreSQLに接続できない

**解決策:**
```bash
# Redis起動確認
redis-cli ping

# PostgreSQL起動確認
psql -U user -d db -c "SELECT 1"
```

### 問題: 移行スコアが低い

**原因:** COBOLコードが複雑、またはパターンが不足

**解決策:**
- `max_iterations`を増やす
- `acceptance_threshold`を下げる
- MemorySystemにパターンを追加

