# AgentFlow Python 固有ルール

> **バージョン**: 1.0.0
> **適用範囲**: AgentFlow Python コードベース
> **最終更新**: 2026-01-19

## 📋 目次

1. [7コア層 + Apps外層遵守](#7コア層 + Apps外層遵守)
2. [プロトコル統合パターン](#プロトコル統合パターン)
3. [統一Provider使用](#統一provider使用)
4. [Agent開発パターン](#agent開発パターン)
5. [非同期I/O強制](#非同期io強制)
6. [型安全強化](#型安全強化)
7. [Skills自動進化対応](#skills自動進化対応)
8. [自動化チェック](#自動化チェック)

---

## 🏗️ 7コア層 + Apps外層遵守

### レイヤー境界の厳格遵守
AgentFlow の7コア層 + Apps外層における依存方向を厳守します。

```python
# ✅ 正しい依存方向（上位→下位）
# アプリケーション層 → フロー層 → Agent層 → ツール層 → Provider層

class DecisionEngine:  # アプリケーション層
    def __init__(self, flow_executor):  # フロー層に依存
        self.flow_executor = flow_executor

class FlowExecutor:  # フロー層
    def __init__(self, agent_registry):  # Agent層に依存
        self.agent_registry = agent_registry

# ❌ 禁止: 逆方向依存
class AgentBlock:  # Agent層
    def __init__(self, decision_engine):  # アプリケーション層に依存 - 禁止
        pass
```

### インターフェース経由の結合
層間結合は必ずインターフェースを経由します。

```python
# ✅ インターフェースによる結合
from agentflow.core.interfaces import ICodeGenerator

class PublishService:
    def __init__(self, code_generator: ICodeGenerator):  # インターフェース依存
        self.code_generator = code_generator

    async def generate_code(self, workflow) -> GeneratedCode:
        return await self.code_generator.generate(workflow)

# ❌ 禁止: 具象クラス直接依存
class PublishService:
    def __init__(self, code_generator: CodeGenerator):  # 具象クラス依存 - 禁止
        self.code_generator = code_generator
```

---

## 🌐 プロトコル統合パターン

### 4プロトコルの統一操作
MCP/A2A/AG-UI/A2UI の4プロトコルを統一APIで操作します。

```python
# ✅ 統一プロトコルAPIの使用
from agentflow import get_protocol_adapter

adapter = get_protocol_adapter()

# MCP ツール呼び出し
result = await adapter.call_tool("mcp", "search_database", {"query": "test"})

# A2A メッセージ送信
await adapter.send_message("a2a", agent_id, message)

# AG-UI イベント発行
await adapter.emit_event("ag-ui", "workflow_started", {"workflow_id": "wf-123"})

# A2UI コンポーネント更新
await adapter.update_component("a2ui", component_id, {"props": new_props})
```

### @auto_adapt デコレータ使用
プロトコル自動変換を活用します。

```python
# ✅ @auto_adapt の活用
from agentflow.protocols import auto_adapt

class MyAgent(AgentBlock):
    @auto_adapt
    async def run(self, inputs):
        """プロトコル自動変換."""
        # このメソッドは MCP, A2A, AG-UI, A2UI のいずれからも呼び出し可能
        return {"result": "processed"}

# 異なるプロトコルからの呼び出し
agent = MyAgent()

# MCP プロトコル経由
mcp_result = await agent.run_via_mcp(mcp_inputs)

# A2A プロトコル経由
a2a_result = await agent.run_via_a2a(a2a_message)

# 自動的にプロトコル変換される
```

---

## 🔌 統一Provider使用

### 松耦合Provider API
具体的なプロバイダー実装を知らずにサービスを使用します。

```python
# ✅ 統一Provider APIの使用
from agentflow import get_llm, get_db, get_vectordb, get_cache

# LLM（自動プロバイダー検出）
llm = get_llm(temperature=0.7)
response = await llm.chat([{"role": "user", "content": "Hello!"}])

# データベース（自動接続）
db = get_db()
users = await db.select("users", filters={"active": True})

# ベクトルDB（自動初期化）
vdb = get_vectordb()
results = await vdb.search("query text", top_k=5)

# キャッシュ（自動設定）
cache = get_cache()
await cache.set("key", "value", ttl=3600)
value = await cache.get("key")
```

### 環境変数ベース設定
環境変数から自動検出・設定を行います。

```bash
# .env 設定例
# LLM Providers
OPENAI_API_KEY=
ANTHROPIC_API_KEY=
GOOGLE_API_KEY=

# Databases
DATABASE_URL=
SUPABASE_URL=https://xxx.supabase.co
SUPABASE_KEY=

# Vector DB
PINECONE_API_KEY=
QDRANT_URL=http://localhost:6333

# Cache
REDIS_URL=redis://localhost:6379
```

```python
# ✅ 環境変数自動検出
# コード変更なしでプロバイダー切り替え可能
llm = get_llm()  # OPENAI_API_KEY があれば OpenAI、なければ Anthropic

db = get_db()    # DATABASE_URL が PostgreSQL なら PostgreSQL、
                 # SUPABASE_URL があれば Supabase

vdb = get_vectordb()  # PINECONE_API_KEY があれば Pinecone、
                     # QDRANT_URL があれば Qdrant
```

---

## 🤖 Agent開発パターン

### @agent デコレータ優先
最もシンプルなAgent定義方式を使用します。

```python
# ✅ @agent デコレータ優先
from agentflow import agent, tool

@agent
class QAAgent:
    """質問応答Agent - 設定ゼロ."""

    system_prompt = "あなたは親切なアシスタントです"

    @tool
    def search_database(self, query: str) -> list[dict]:
        """データベース検索."""
        # 実際の検索ロジック
        return []

    async def run(self, inputs: dict) -> dict:
        """メイン実行ロジック."""
        query = inputs.get("question", "")

        # LLM 呼び出し
        llm = get_llm()
        response = await llm.chat([
            {"role": "system", "content": self.system_prompt},
            {"role": "user", "content": query}
        ])

        # ツール使用
        search_results = self.search_database(query)

        return {
            "answer": response["content"],
            "sources": search_results
        }
```

### AgentBlock 基底クラス使用
複雑なAgentには基底クラスを使用します。

```python
# ✅ AgentBlock 基底クラス
from agentflow.agents import AgentBlock

class ComplexAgent(AgentBlock):
    """複雑な処理を行うAgent."""

    def __init__(self):
        super().__init__()
        self.llm = get_llm()
        self.db = get_db()

    async def run(self, inputs: dict) -> dict:
        """複雑な処理フロー."""
        # ステップ1: 入力検証
        validated = await self._validate_inputs(inputs)

        # ステップ2: データ取得
        data = await self.db.select("relevant_data", filters=validated)

        # ステップ3: AI処理
        analysis = await self.llm.chat([
            {"role": "system", "content": "データを分析してください"},
            {"role": "user", "content": str(data)}
        ])

        # ステップ4: 結果加工
        return await self._format_output(analysis, data)

    async def _validate_inputs(self, inputs: dict) -> dict:
        """入力検証."""
        # 検証ロジック
        pass

    async def _format_output(self, analysis: dict, data: list) -> dict:
        """出力フォーマット."""
        # フォーマットロジック
        pass
```

---

## ⚡ 非同期I/O強制

### 全てのI/Oを非同期化
ブロッキングI/Oを一切禁止します。

```python
# ✅ 非同期I/Oのみ
import aiofiles
import aiohttp

class DataLoader:
    async def load_yaml(self, path: str) -> dict:
        """YAMLファイル読み込み（非同期）."""
        async with aiofiles.open(path, 'r', encoding='utf-8') as f:
            content = await f.read()
        return yaml.safe_load(content)

    async def fetch_api(self, url: str) -> dict:
        """API呼び出し（非同期）."""
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                return await response.json()

# ❌ 禁止: 同期I/O
class BadDataLoader:
    def load_yaml(self, path: str) -> dict:  # 同期 - 禁止
        with open(path, 'r', encoding='utf-8') as f:  # ブロッキング
            content = f.read()
        return yaml.safe_load(content)
```

### async/await パターン強制
全てのメソッドを非同期化します。

```python
# ✅ async/await パターン
class AsyncService:
    async def process_data(self, data: dict) -> dict:
        """データ処理（非同期）."""
        # 並行処理
        tasks = [
            self._validate_data(data),
            self._enrich_data(data),
            self._save_to_cache(data)
        ]

        results = await asyncio.gather(*tasks)

        # 結果統合
        return {
            "validated": results[0],
            "enriched": results[1],
            "cached": results[2]
        }

    async def _validate_data(self, data: dict) -> bool:
        """データ検証."""
        # 検証ロジック
        pass

    async def _enrich_data(self, data: dict) -> dict:
        """データ拡充."""
        # 拡充ロジック
        pass

    async def _save_to_cache(self, data: dict) -> bool:
        """キャッシュ保存."""
        # 保存ロジック
        pass
```

---

## 🧬 型安全強化

### 100% 型アノテーション
全ての関数/メソッドに型アノテーションを付けます。

```python
# ✅ 完全な型アノテーション
from typing import Any, Dict, List, Optional, Union
from agentflow.core.types import WorkflowDefinition, GeneratedCode

class CodeGenerator:
    async def generate(
        self,
        workflow: WorkflowDefinition,
        output_type: str = "backend",
        *,
        template_dir: Optional[str] = None,
        validate: bool = True,
    ) -> GeneratedCode:
        """ワークフローからコードを生成.

        Args:
            workflow: 生成対象のワークフロー定義
            output_type: 出力タイプ（"backend" | "frontend" | "fullstack"）
            template_dir: テンプレートディレクトリ（オプション）
            validate: 入力検証を行うか

        Returns:
            生成されたコード

        Raises:
            ValidationError: ワークフロー検証失敗時
            TemplateError: テンプレート処理失敗時
        """
        pass
```

### Protocol 活用
動的構造にはProtocolを使用します。

```python
# ✅ Protocol の活用
from typing import Protocol

class Tool(Protocol):
    """ツールインターフェース."""

    async def run(self, payload: dict[str, Any]) -> dict[str, Any]:
        """ツール実行."""
        ...

class DatabaseTool(Protocol):
    """データベースツールインターフェース."""

    async def connect(self) -> None:
        """接続."""
        ...

    async def disconnect(self) -> None:
        """切断."""
        ...

    async def execute(self, query: str, params: Optional[dict] = None) -> list[dict]:
        """クエリ実行."""
        ...

# 使用例
async def execute_tool(tool: Tool, payload: dict[str, Any]) -> dict[str, Any]:
    """任意のツールを実行."""
    return await tool.run(payload)
```

---

## 🧠 Skills自動進化対応

### Skills エンジン統合
越用越厉害の自動進化システムに対応します。

```python
# ✅ Skills エンジン統合
from agentflow.skills import SkillEngine

class SkillAwareAgent(AgentBlock):
    """Skills自動進化対応Agent."""

    def __init__(self):
        super().__init__()
        self.skill_engine = SkillEngine(auto_learn=True)

    async def run(self, inputs: dict) -> dict:
        """Skills を活用した処理."""
        task = inputs.get("task", "")

        # 既存Skill検索・自動生成
        result = await self.skill_engine.resolve(task)

        if result.generated:
            # 新しいSkillが自動生成された
            logger.info(
                "new_skill_generated",
                skill_name=result.skill.name,
                task=task
            )

        # Skill実行
        output = await result.skill.run(inputs)

        return {"result": output, "used_skill": result.skill.name}
```

### Skill 定義パターン
再利用可能なSkillを定義します。

```python
# ✅ Skill 定義
from agentflow.skills import skill, SkillContext

@skill
class PDFExtractorSkill:
    """PDFテキスト抽出Skill."""

    name = "pdf_extractor"
    description = "PDFファイルからテキストを抽出します"

    async def can_handle(self, context: SkillContext) -> bool:
        """このSkillで処理可能か判定."""
        return (
            context.task_type == "document_processing" and
            context.file_extension == ".pdf"
        )

    async def run(self, context: SkillContext) -> dict[str, Any]:
        """PDF抽出実行."""
        file_path = context.inputs.get("file_path")

        # PDF処理ロジック
        text = await self._extract_text(file_path)

        return {
            "extracted_text": text,
            "page_count": len(text.split("\n\n")),
            "word_count": len(text.split())
        }

    async def _extract_text(self, file_path: str) -> str:
        """PDFからテキスト抽出."""
        # 実際のPDF処理
        pass
```

---

## ✅ 自動化チェック

### AgentFlow 固有検証スクリプト
```python
#!/usr/bin/env python3
# scripts/validate_agentflow_patterns.py

import ast
import sys
from pathlib import Path
from typing import List

class AgentFlowPatternValidator:
    """AgentFlow固有パターンを検証."""

    def __init__(self):
        self.violations: List[str] = []

    def validate_file(self, file_path: Path):
        """ファイルを検証."""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                tree = ast.parse(f.read())

            self._check_async_usage(tree, file_path)
            self._check_unified_providers(tree, file_path)
            self._check_agent_patterns(tree, file_path)
            self._check_layer_dependencies(tree, file_path)

        except Exception as e:
            self.violations.append(f"{file_path}: パースエラー - {e}")

    def _check_async_usage(self, tree: ast.AST, file_path: Path):
        """非同期使用をチェック."""
        for node in ast.walk(tree):
            if isinstance(node, ast.FunctionDef):
                # I/O 関連の関数は async であるべき
                if self._is_io_function(node.name):
                    if not isinstance(node, ast.AsyncFunctionDef):
                        self.violations.append(
                            f"{file_path}:{node.lineno}: "
                            f"I/O関数 {node.name} が非同期ではありません"
                        )

    def _is_io_function(self, func_name: str) -> bool:
        """I/O関連の関数か判定."""
        io_indicators = [
            'load', 'save', 'fetch', 'send', 'receive', 'connect',
            'execute', 'query', 'insert', 'update', 'delete',
            'generate', 'process', 'handle'
        ]
        return any(indicator in func_name.lower() for indicator in io_indicators)

    def _check_unified_providers(self, tree: ast.AST, file_path: Path):
        """統一Provider使用をチェック."""
        provider_imports = {
            'get_llm', 'get_db', 'get_vectordb', 'get_cache'
        }

        imported_providers = set()

        for node in ast.walk(tree):
            if isinstance(node, ast.ImportFrom):
                for alias in node.names:
                    if alias.name in provider_imports:
                        imported_providers.add(alias.name)

        # 直接プロバイダー使用をチェック
        for node in ast.walk(tree):
            if isinstance(node, ast.Call):
                if isinstance(node.func, ast.Attribute):
                    # obj.method() 形式
                    if isinstance(node.func.value, ast.Name):
                        obj_name = node.func.value.id
                        method_name = node.func.attr

                        # 直接プロバイダーインスタンス化の禁止
                        if self._is_direct_provider_usage(obj_name, method_name):
                            self.violations.append(
                                f"{file_path}:{node.lineno}: "
                                f"直接プロバイダー使用: {obj_name}.{method_name}"
                            )

    def _is_direct_provider_usage(self, obj_name: str, method_name: str) -> bool:
        """直接プロバイダー使用か判定."""
        direct_providers = {
            'OpenAI': ['chat', '__init__'],
            'Anthropic': ['chat', '__init__'],
            'SupabaseClient': ['table', '__init__'],
            'PineconeClient': ['index', '__init__'],
        }

        if obj_name in direct_providers:
            return method_name in direct_providers[obj_name]

        return False

    def _check_agent_patterns(self, tree: ast.AST, file_path: Path):
        """Agentパターンをチェック."""
        for node in ast.walk(tree):
            if isinstance(node, ast.ClassDef):
                # Agent クラスかチェック
                if any(base.id == 'AgentBlock' for base in node.bases if hasattr(base, 'id')):
                    self._check_agent_class(node, file_path)

    def _check_agent_class(self, node: ast.ClassDef, file_path: Path):
        """Agentクラスのパターンをチェック."""
        has_run_method = False
        has_async_run = False

        for item in node.body:
            if isinstance(item, ast.FunctionDef):
                if item.name == 'run':
                    has_run_method = True
                    if isinstance(item, ast.AsyncFunctionDef):
                        has_async_run = True

        if has_run_method and not has_async_run:
            self.violations.append(
                f"{file_path}:{node.lineno}: "
                f"Agentクラス {node.name} の run メソッドが非同期ではありません"
            )

    def _check_layer_dependencies(self, tree: ast.AST, file_path: Path):
        """レイヤー依存をチェック."""
        # 簡易的なチェック（完全な実装は別途）
        pass

    def validate_all_python_files(self):
        """全Pythonファイルを検証."""
        for py_file in Path("agentflow").rglob("*.py"):
            self.validate_file(py_file)

    def report(self) -> bool:
        """結果をレポート."""
        if self.violations:
            print("❌ AgentFlow パターン違反:")
            for violation in self.violations:
                print(f"  - {violation}")
            return False
        else:
            print("✅ AgentFlow パターン検証通過")
            return True

def main():
    validator = AgentFlowPatternValidator()
    validator.validate_all_python_files()
    success = validator.report()
    sys.exit(0 if success else 1)

if __name__ == "__main__":
    main()
```

### 7コア層 + Apps外層依存検証
```python
#!/usr/bin/env python3
# scripts/validate_layer_dependencies.py

import ast
import sys
from pathlib import Path
from typing import Dict, Set, List

class LayerDependencyValidator:
    """7コア層 + Apps外層の依存関係を検証."""

    # レイヤー定義と許可された依存
    LAYER_HIERARCHY = {
        "application": {"flows", "agents", "tools", "providers", "protocols", "infra"},
        "ui": {"flows", "agents", "tools", "providers", "protocols", "infra"},
        "flows": {"agents", "tools", "providers", "protocols", "infra"},
        "agents": {"tools", "providers", "protocols", "infra"},
        "tools": {"providers", "protocols", "infra"},
        "providers": {"protocols", "infra"},
        "protocols": {"infra"},
        "infra": set()  # インフラ層は何にも依存しない
    }

    # ファイルパスからレイヤーを判定するマッピング
    PATH_TO_LAYER = {
        "applications": "application",
        "uis": "ui",
        "flows": "flows",
        "agents": "agents",
        "tools": "tools",
        "providers": "providers",
        "protocols": "protocols",
        "core": "providers",  # core は providers 層
        "services": "application",  # services は application 層
        "infra": "infra",
    }

    def __init__(self):
        self.violations: List[str] = []

    def validate_file(self, file_path: Path):
        """ファイルを検証."""
        current_layer = self._get_layer_from_path(file_path)
        if not current_layer:
            return  # レイヤー外のファイルはスキップ

        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                tree = ast.parse(f.read())

            self._check_imports(tree, file_path, current_layer)

        except Exception as e:
            self.violations.append(f"{file_path}: パースエラー - {e}")

    def _get_layer_from_path(self, file_path: Path) -> str | None:
        """ファイルパスからレイヤーを判定."""
        parts = file_path.parts
        if "agentflow" in parts:
            idx = parts.index("agentflow")
            if idx + 1 < len(parts):
                layer_name = parts[idx + 1]
                return self.PATH_TO_LAYER.get(layer_name, layer_name)
        return None

    def _check_imports(self, tree: ast.AST, file_path: Path, current_layer: str):
        """インポートをチェック."""
        for node in ast.walk(tree):
            if isinstance(node, ast.ImportFrom):
                module = node.module or ""
                if module.startswith("agentflow."):
                    imported_layer = self._get_layer_from_module(module)
                    if imported_layer and not self._is_allowed_dependency(current_layer, imported_layer):
                        rel_path = file_path.relative_to(Path.cwd())
                        self.violations.append(
                            f"{rel_path}:{node.lineno}: "
                            f"{current_layer}層 が {imported_layer}層 に依存 "
                            f"(from {module} import ...)"
                        )

    def _get_layer_from_module(self, module: str) -> str | None:
        """モジュール名からレイヤーを判定."""
        module_parts = module.split(".")
        if len(module_parts) >= 2 and module_parts[0] == "agentflow":
            sub_module = module_parts[1]
            return self.PATH_TO_LAYER.get(sub_module)
        return None

    def _is_allowed_dependency(self, from_layer: str, to_layer: str) -> bool:
        """依存が許可されているかチェック."""
        allowed = self.LAYER_HIERARCHY.get(from_layer, set())
        return to_layer in allowed

    def validate_all_files(self):
        """全ファイルを検証."""
        for py_file in Path("agentflow").rglob("*.py"):
            self.validate_file(py_file)

    def report(self) -> bool:
        """結果をレポート."""
        if self.violations:
            print("❌ レイヤー依存違反:")
            for violation in self.violations:
                print(f"  - {violation}")
            return False
        else:
            print("✅ レイヤー依存検証通過")
            return True

def main():
    validator = LayerDependencyValidator()
    validator.validate_all_files()
    success = validator.report()
    sys.exit(0 if success else 1)

if __name__ == "__main__":
    main()
```

---

## 📋 AgentFlow Python 固有ルール チートシート

| ルール | 必須 | 例 | 理由 |
|--------|------|-----|------|
| **7コア層遵守** | ✅ | 上位→下位のみ | アーキテクチャ分離 |
| **統一Provider** | ✅ | `get_llm()` | 松耦合設計 |
| **@agent優先** | ✅ | `@agent class MyAgent` | シンプルさ |
| **非同期強制** | ✅ | `async def run()` | スケーラビリティ |
| **100%型アノテーション** | ✅ | `def func(arg: Type) -> Return` | 型安全 |
| **Skills統合** | ✅ | `SkillEngine(auto_learn=True)` | 自動進化 |
| **Protocol活用** | ✅ | `class Tool(Protocol)` | 動的構造 |

*最終更新: 2026-01-19 | BizCore 7コア層 + Apps外層 & 4プロトコル統合*
