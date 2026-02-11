# Auto-Agent と Skills 統合 実装計画

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Auto-Agent アーキテクチャと Skills システムを完全統合し、@agent デコレータと Engine で自動ツールバインディングを実現する

**Architecture:**
- ToolDiscoveryService が Skills を自動発見し ToolRegistry に登録
- @agent デコレータが AgentRegistry に Agent を自動登録（AgentCapabilitySpec 付き）
- SimpleEngine が ToolBinder を使用してツールを自動バインド
- ルートレベルの `skills/` ディレクトリで Skills を一元管理

**Tech Stack:** Python 3.13+, Pydantic v2, pytest, asyncio

---

## Task 1: ToolDiscoveryService に Skills 発見機能を追加

**Files:**
- Modify: `agentflow/core/tool_discovery.py:128-150`
- Modify: `agentflow/core/tool_definition.py:80-120`
- Test: `tests/unit/core/test_tool_discovery.py`

**Step 1: Write the failing test**

```python
# tests/unit/core/test_tool_discovery.py に追加

@pytest.mark.asyncio
async def test_discover_skills_from_skill_engine():
    """SkillEngine からスキルを発見してツールとして登録."""
    from agentflow.core.tool_registry import ToolRegistry
    from agentflow.core.tool_discovery import ToolDiscoveryService
    from agentflow.core.tool_definition import ToolSource

    registry = ToolRegistry()
    service = ToolDiscoveryService(registry)

    # SkillEngine から発見
    count = await service.discover_skills_from_engine()

    # ビルトインスキルが登録されていることを確認
    assert count > 0

    # スキルがツールとして登録されていることを確認
    skill_tools = registry.filter_by_source(ToolSource.SKILL)
    assert len(skill_tools) > 0

    # RAG スキルが存在することを確認
    rag_tool = registry.get("tool://skill/rag")
    assert rag_tool is not None
    assert "検索" in rag_tool.description or "retrieval" in rag_tool.description.lower()
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/unit/core/test_tool_discovery.py::test_discover_skills_from_skill_engine -v`
Expected: FAIL with "AttributeError: 'ToolDiscoveryService' object has no attribute 'discover_skills_from_engine'"

**Step 3: Update ToolDefinition.from_skill to handle Skill objects**

```python
# agentflow/core/tool_definition.py の from_skill メソッドを更新

@classmethod
def from_skill(cls, skill_data: dict[str, Any] | Any) -> "ToolDefinition":
    """Skill からツール定義を作成.

    Args:
        skill_data: スキル定義（dict または Skill オブジェクト）

    Returns:
        ToolDefinition インスタンス
    """
    # Skill オブジェクトの場合は dict に変換
    if hasattr(skill_data, "metadata"):
        # agentflow.skills.base.Skill オブジェクト
        metadata = skill_data.metadata
        name = metadata.name
        description = metadata.description

        # input_schema があれば使用、なければデフォルト
        input_schema = getattr(metadata, "input_schema", None) or {
            "type": "object",
            "properties": {
                "query": {"type": "string", "description": "スキルへの入力"}
            },
        }

        # メタデータを収集
        extra_metadata = {
            "version": metadata.version,
            "author": metadata.author,
            "triggers": metadata.triggers,
            "tags": metadata.tags,
            "requirements": metadata.requirements,
        }
    else:
        # dict の場合
        name = skill_data.get("name", "unknown")
        description = skill_data.get("description", "")
        input_schema = skill_data.get("input_schema") or skill_data.get("parameters", {})
        extra_metadata = {
            k: v for k, v in skill_data.items()
            if k not in ("name", "description", "input_schema", "parameters")
        }

    return cls(
        uri=f"tool://skill/{name}",
        name=name,
        description=description,
        source=ToolSource.SKILL,
        input_schema=input_schema if input_schema else {},
        metadata=extra_metadata,
    )
```

**Step 4: Add discover_skills_from_engine method to ToolDiscoveryService**

```python
# agentflow/core/tool_discovery.py に追加

async def discover_skills_from_engine(self) -> int:
    """SkillEngine からスキルを発見してツールとして登録.

    ビルトインスキルおよびユーザー学習スキルを自動発見。

    Returns:
        登録されたスキル数
    """
    try:
        from agentflow.skills.loader import SkillLoader
        from agentflow.skills.base import Skill
        from pathlib import Path

        count = 0
        loader = SkillLoader()

        # ビルトインスキルディレクトリ
        builtin_dir = Path(__file__).parent.parent / "skills" / "builtin"
        if builtin_dir.exists():
            skills = loader.load_directory(builtin_dir, recursive=True)
            for skill in skills:
                try:
                    tool_def = ToolDefinition.from_skill(skill)
                    self._registry.register(tool_def)
                    count += 1
                except Exception as e:
                    self._logger.warning(f"スキル登録エラー {skill.name}: {e}")

        # ユーザー学習スキルディレクトリ
        user_skills_dir = Path.home() / ".agentflow" / "skills"
        if user_skills_dir.exists():
            skills = loader.load_directory(user_skills_dir, recursive=True)
            for skill in skills:
                try:
                    tool_def = ToolDefinition.from_skill(skill)
                    self._registry.register(tool_def)
                    count += 1
                except Exception as e:
                    self._logger.warning(f"ユーザースキル登録エラー {skill.name}: {e}")

        # ルートスキルディレクトリ（新規）
        root_skills_dir = Path(__file__).parent.parent.parent / "skills"
        if root_skills_dir.exists():
            skills = loader.load_directory(root_skills_dir, recursive=True)
            for skill in skills:
                try:
                    tool_def = ToolDefinition.from_skill(skill)
                    self._registry.register(tool_def)
                    count += 1
                except Exception as e:
                    self._logger.warning(f"ルートスキル登録エラー {skill.name}: {e}")

        self._logger.debug(f"SkillEngine からスキル発見: {count}")
        return count

    except ImportError:
        self._logger.debug("SkillLoader が利用不可")
        return 0
    except Exception as e:
        self._logger.warning(f"スキル発見エラー: {e}")
        return 0
```

**Step 5: Run test to verify it passes**

Run: `pytest tests/unit/core/test_tool_discovery.py::test_discover_skills_from_skill_engine -v`
Expected: PASS

**Step 6: Commit**

```bash
git add agentflow/core/tool_discovery.py agentflow/core/tool_definition.py tests/unit/core/test_tool_discovery.py
git commit -m "$(cat <<'EOF'
feat: Add Skills discovery to ToolDiscoveryService

- Add discover_skills_from_engine() method
- Update ToolDefinition.from_skill() to handle Skill objects
- Auto-discover builtin, user-learned, and root skills

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
```

---

## Task 2: Skills ディレクトリをルートレベルに再構成

**Files:**
- Create: `skills/README.md`
- Create: `skills/builtin/` (symlink or move)
- Modify: `agentflow/skills/loader.py:98-122`
- Test: `tests/unit/skills/test_skill_loader.py`

**Step 1: Create root skills directory structure**

```bash
mkdir -p skills/builtin skills/user skills/apps
```

**Step 2: Create skills/README.md**

```markdown
# Skills ディレクトリ

AgentFlow Skills の一元管理ディレクトリ。Claude Code Skills 形式に完全互換。

## ディレクトリ構成

```
skills/
├── builtin/          # フレームワーク提供のビルトインスキル
│   ├── rag/SKILL.md
│   ├── chatbot/SKILL.md
│   └── ...
├── user/             # ユーザー定義スキル
│   └── my-skill/SKILL.md
└── apps/             # アプリケーション固有スキル
    └── decision_governance_engine/
        ├── dao/SKILL.md
        └── ...
```

## SKILL.md 形式

```markdown
---
name: skill-name
description: スキルの説明
version: 1.0.0
triggers:
  - キーワード1
  - キーワード2
requirements:
  - package>=1.0.0
tags:
  - category
---

# Instructions

スキルの詳細な指示...
```

## 使用方法

```python
from agentflow import agent

@agent(skills=["rag", "chatbot"])
class MyAgent:
    pass
```

詳細は [Skills ガイド](../guide-skills.md) を参照。
```

**Step 3: Write test for root skills directory loading**

```python
# tests/unit/skills/test_skill_loader.py に追加

def test_load_from_root_skills_directory():
    """ルートスキルディレクトリからの読み込みテスト."""
    from agentflow.skills.loader import SkillLoader
    from pathlib import Path

    loader = SkillLoader()

    # ルートスキルディレクトリ
    root_dir = Path(__file__).parent.parent.parent.parent / "skills"

    if root_dir.exists():
        skills = loader.load_directory(root_dir, recursive=True)
        # 読み込み成功を確認
        assert isinstance(skills, list)
```

**Step 4: Run test**

Run: `pytest tests/unit/skills/test_skill_loader.py::test_load_from_root_skills_directory -v`
Expected: PASS

**Step 5: Move builtin skills to root (create symlink for backward compatibility)**

```bash
# ビルトインスキルをルートにコピー
cp -r agentflow/skills/builtin/* skills/builtin/

# 後方互換性のためシンボリックリンクを作成（オプション）
# ln -s ../../../skills/builtin agentflow/skills/builtin_link
```

**Step 6: Commit**

```bash
git add skills/ tests/unit/skills/test_skill_loader.py
git commit -m "$(cat <<'EOF'
feat: Reorganize Skills directory to root level

- Create skills/ directory at project root
- Add builtin/, user/, apps/ subdirectories
- Add README.md with usage documentation
- Maintain backward compatibility with agentflow/skills/builtin/

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
```

---

## Task 3: @agent デコレータと AgentRegistry の統合

**Files:**
- Modify: `agentflow/agent_decorator.py:353-364`
- Modify: `agentflow/core/agent_registry.py`
- Test: `tests/unit/test_agent_decorator_registry.py`

**Step 1: Write the failing test**

```python
# tests/unit/test_agent_decorator_registry.py（新規作成）

import pytest
from agentflow.core.agent_registry import get_global_agent_registry, reset_global_agent_registry


@pytest.fixture(autouse=True)
def reset_registry():
    """テスト前後にレジストリをリセット."""
    reset_global_agent_registry()
    yield
    reset_global_agent_registry()


def test_agent_decorator_registers_in_agent_registry():
    """@agent デコレータが AgentRegistry に登録することを確認."""
    from agentflow import agent
    from agentflow.core.agent_registry import get_global_agent_registry

    @agent
    class TestAgent:
        """テスト用Agent."""
        system_prompt = "テストAgent"

    registry = get_global_agent_registry()

    # AgentRegistry に登録されていることを確認
    assert "TestAgent" in [entry.agent_id for entry in registry.list_all()]

    # ファクトリが取得できることを確認
    factory = registry.get_factory("TestAgent")
    assert factory is not None


def test_agent_decorator_creates_capability_spec():
    """@agent デコレータが AgentCapabilitySpec を作成することを確認."""
    from agentflow import agent
    from agentflow.core.agent_registry import get_global_agent_registry

    @agent(skills=["rag", "chatbot"])
    class SkillfulAgent:
        """スキル付きAgent."""
        system_prompt = "スキル付きAgent"

    registry = get_global_agent_registry()
    capability = registry.get_capability("SkillfulAgent")

    assert capability is not None
    assert "rag" in capability.tags or "rag" in capability.required_tools
    assert capability.description == "スキル付きAgent"


def test_agent_registry_find_matching():
    """AgentRegistry でタスク要件マッチングができることを確認."""
    from agentflow import agent
    from agentflow.core.agent_registry import get_global_agent_registry
    from agentflow.core.capability_spec import CapabilityRequirement

    @agent(skills=["rag"])
    class RAGAgent:
        """RAG Agent."""
        system_prompt = "RAG検索Agent"

    @agent(skills=["chatbot"])
    class ChatAgent:
        """Chat Agent."""
        system_prompt = "チャットAgent"

    registry = get_global_agent_registry()

    # RAG タスク要件でマッチング
    requirement = CapabilityRequirement(
        description="ドキュメントを検索",
        required_tags=["rag"],
    )

    matches = registry.find_matching(requirement)
    assert len(matches) > 0
    assert matches[0][0] == "RAGAgent"
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/unit/test_agent_decorator_registry.py -v`
Expected: FAIL with assertion errors (not registered in AgentRegistry)

**Step 3: Update agent_decorator.py to integrate with AgentRegistry**

```python
# agentflow/agent_decorator.py の該当部分を更新

# インポートを追加（ファイル先頭付近）
from agentflow.core.agent_registry import get_global_agent_registry
from agentflow.core.capability_spec import AgentCapabilitySpec

# agent デコレータ関数内（Lines 353-364 付近）を更新
def agent(...):
    def decorator(cls: type) -> type:
        # ... 既存のコード ...

        # RegisteredAgent を作成（既存）
        registered = RegisteredAgent(
            cls=cls,
            name=agent_name,
            llm=llm,
            temperature=temperature,
            max_tokens=max_tokens,
            system_prompt=system_prompt,
            tools=tools,
            skills=agent_skills,
        )
        _agent_registry[agent_name] = registered

        # NEW: AgentRegistry にも登録
        try:
            global_registry = get_global_agent_registry()

            # AgentCapabilitySpec を作成
            capability = AgentCapabilitySpec(
                id=f"{agent_name}_capability",
                name=agent_name,
                description=system_prompt or cls.__doc__ or f"{agent_name} Agent",
                tags=list(agent_skills) if agent_skills else [],
                required_tools=[f"tool://skill/{s}" for s in (agent_skills or [])],
            )

            # AgentRegistry に登録
            global_registry.register(
                agent_id=agent_name,
                capability=capability,
                factory=lambda r=registered: r.get_instance(),
            )

            _logger.debug(f"Agent '{agent_name}' を AgentRegistry に登録")
        except Exception as e:
            _logger.warning(f"AgentRegistry 登録エラー: {e}")

        return cls

    return decorator
```

**Step 4: Run test to verify it passes**

Run: `pytest tests/unit/test_agent_decorator_registry.py -v`
Expected: PASS

**Step 5: Commit**

```bash
git add agentflow/agent_decorator.py tests/unit/test_agent_decorator_registry.py
git commit -m "$(cat <<'EOF'
feat: Integrate @agent decorator with AgentRegistry

- Auto-register agents in global AgentRegistry
- Create AgentCapabilitySpec from agent metadata
- Enable capability-based agent matching

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
```

---

## Task 4: SimpleEngine と ToolBinder の統合

**Files:**
- Modify: `agentflow/engines/simple_engine.py:47-112`
- Test: `tests/unit/engines/test_simple_engine_binding.py`

**Step 1: Write the failing test**

```python
# tests/unit/engines/test_simple_engine_binding.py（新規作成）

import pytest
from agentflow.core.tool_registry import reset_global_tool_registry


@pytest.fixture(autouse=True)
def reset_registries():
    """テスト前後にレジストリをリセット."""
    reset_global_tool_registry()
    yield
    reset_global_tool_registry()


@pytest.mark.asyncio
async def test_simple_engine_auto_binds_tools():
    """SimpleEngine がツールを自動バインドすることを確認."""
    from agentflow.engines import SimpleEngine
    from agentflow.core.tool_registry import get_global_tool_registry
    from agentflow.core.tool_discovery import ToolDiscoveryService

    # ツールを登録
    registry = get_global_tool_registry()
    service = ToolDiscoveryService(registry)
    service.register_builtin(
        name="test_tool",
        description="テストツール",
        input_schema={"type": "object", "properties": {"input": {"type": "string"}}},
    )

    # モックAgent
    class MockAgent:
        _tools = None
        _tool_executor = None

        async def run(self, inputs):
            return {"result": "ok", "has_tools": self._tools is not None}

    # SimpleEngine で実行
    engine = SimpleEngine(
        agent=MockAgent,
        tools=["tool://builtin/test_tool"],
    )

    result = await engine.run({"input": "test"})

    # ツールがバインドされていることを確認
    assert result.get("has_tools") is True


@pytest.mark.asyncio
async def test_simple_engine_binds_skills_as_tools():
    """SimpleEngine がスキルをツールとしてバインドすることを確認."""
    from agentflow.engines import SimpleEngine
    from agentflow.core.tool_registry import get_global_tool_registry
    from agentflow.core.tool_discovery import ToolDiscoveryService

    # スキルを発見
    registry = get_global_tool_registry()
    service = ToolDiscoveryService(registry)
    await service.discover_skills_from_engine()

    class MockAgent:
        _tools = None

        async def run(self, inputs):
            return {"tools_count": len(self._tools) if self._tools else 0}

    # スキル指定で実行
    engine = SimpleEngine(
        agent=MockAgent,
        skills=["rag"],
    )

    result = await engine.run({"query": "test"})

    # スキルがツールとしてバインドされていることを確認
    assert result.get("tools_count", 0) > 0
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/unit/engines/test_simple_engine_binding.py -v`
Expected: FAIL with assertion errors

**Step 3: Update SimpleEngine to use ToolBinder**

```python
# agentflow/engines/simple_engine.py を更新

from agentflow.core.tool_registry import get_global_tool_registry
from agentflow.core.tool_binding import ToolBinder
from agentflow.core.capability_spec import AgentCapabilitySpec

class SimpleEngine(BaseEngine):
    """単一Agent実行エンジン（ToolBinder統合版）."""

    def __init__(
        self,
        agent: type | Any,
        *,
        skills: list[str] | None = None,
        tools: list[str] | None = None,
        config: EngineConfig | None = None,
    ) -> None:
        super().__init__(config)
        self._agent_class = agent
        self._agent_instance: Any = None
        self._skills = skills or []
        self._tools = tools or []
        self._binder: ToolBinder | None = None

    async def _initialize_agent(self) -> None:
        """Agentインスタンスを初期化し、ツールをバインド."""
        # Agentインスタンスを作成
        if isinstance(self._agent_class, type):
            self._agent_instance = self._agent_class()
        else:
            self._agent_instance = self._agent_class

        # ToolBinder を初期化
        tool_registry = get_global_tool_registry()
        self._binder = ToolBinder(tool_registry)

        # ツールURIを収集
        tool_uris = list(self._tools)

        # スキルをツールURIに変換
        for skill_name in self._skills:
            tool_uris.append(f"tool://skill/{skill_name}")

        # ツールをバインド
        if tool_uris:
            try:
                # AgentCapabilitySpec を作成
                capability = AgentCapabilitySpec(
                    id=f"{self._agent_class.__name__}_runtime",
                    name=getattr(self._agent_class, "__name__", "Agent"),
                    description="Runtime agent",
                    required_tools=tool_uris,
                )

                await self._binder.bind_for_capability(
                    self._agent_instance,
                    capability
                )
                self._logger.debug(f"ツールバインド完了: {len(tool_uris)} ツール")
            except Exception as e:
                self._logger.warning(f"ツールバインドエラー: {e}")

    async def _execute(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """Agent を実行."""
        # 初期化がまだなら実行
        if self._agent_instance is None:
            await self._initialize_agent()

        # Agentを呼び出し
        if hasattr(self._agent_instance, "run"):
            result = await self._agent_instance.run(inputs)
        elif hasattr(self._agent_instance, "invoke"):
            result = await self._agent_instance.invoke(inputs)
        elif hasattr(self._agent_instance, "process"):
            result = await self._agent_instance.process(inputs)
        else:
            raise ValueError(f"Agent has no run/invoke/process method")

        return result if isinstance(result, dict) else {"result": result}
```

**Step 4: Run test to verify it passes**

Run: `pytest tests/unit/engines/test_simple_engine_binding.py -v`
Expected: PASS

**Step 5: Commit**

```bash
git add agentflow/engines/simple_engine.py tests/unit/engines/test_simple_engine_binding.py
git commit -m "$(cat <<'EOF'
feat: Integrate SimpleEngine with ToolBinder

- Auto-bind tools and skills at runtime
- Convert skill names to tool URIs
- Initialize ToolBinder with global registry

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
```

---

## Task 5: Apps ディレクトリの Skills を統合

**Files:**
- Modify: `apps/decision_governance_engine/skills/` → `skills/apps/decision_governance_engine/`
- Modify: `apps/code_migration_assistant/skills/` → `skills/apps/code_migration_assistant/`
- Modify: App 内のスキル参照パス

**Step 1: Move app skills to root skills directory**

```bash
# Decision Governance Engine のスキルを移動
mkdir -p skills/apps/decision_governance_engine
cp -r apps/decision_governance_engine/skills/* skills/apps/decision_governance_engine/

# Code Migration Assistant のスキルを移動
mkdir -p skills/apps/code_migration_assistant
cp -r apps/code_migration_assistant/skills/* skills/apps/code_migration_assistant/
```

**Step 2: Update skill loader paths in apps**

```python
# apps/decision_governance_engine/agents.py の修正が必要な場合
# スキルパスの参照を更新

# 修正前
SKILLS_DIR = Path(__file__).parent / "skills"

# 修正後
SKILLS_DIR = Path(__file__).parent.parent.parent / "skills" / "apps" / "decision_governance_engine"
```

**Step 3: Test app still works**

Run: `pytest apps/decision_governance_engine/tests/ -v --tb=short`
Expected: PASS

**Step 4: Commit**

```bash
git add skills/apps/ apps/
git commit -m "$(cat <<'EOF'
refactor: Move app skills to root skills directory

- Move decision_governance_engine skills to skills/apps/
- Move code_migration_assistant skills to skills/apps/
- Update skill loader paths in apps

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
```

---

## Task 6: 統合テストと ドキュメント更新

**Files:**
- Create: `tests/integration/test_auto_agent_skills_integration.py`
- Modify: `docs/auto-agent-architecture.md`
- Modify: `docs/guide-skills.md`
- Modify: `README.md`

**Step 1: Write integration test**

```python
# tests/integration/test_auto_agent_skills_integration.py

import pytest
from agentflow.core.tool_registry import reset_global_tool_registry
from agentflow.core.agent_registry import reset_global_agent_registry


@pytest.fixture(autouse=True)
def reset_registries():
    """テスト前後にレジストリをリセット."""
    reset_global_tool_registry()
    reset_global_agent_registry()
    yield
    reset_global_tool_registry()
    reset_global_agent_registry()


@pytest.mark.integration
@pytest.mark.asyncio
async def test_full_auto_agent_skills_workflow():
    """Auto-Agent + Skills の完全統合ワークフロー."""
    from agentflow import agent
    from agentflow.engines import SimpleEngine
    from agentflow.core.tool_registry import get_global_tool_registry
    from agentflow.core.agent_registry import get_global_agent_registry
    from agentflow.core.tool_discovery import ToolDiscoveryService
    from agentflow.core.capability_spec import CapabilityRequirement

    # Step 1: Skills を発見
    tool_registry = get_global_tool_registry()
    service = ToolDiscoveryService(tool_registry)
    skill_count = await service.discover_skills_from_engine()
    assert skill_count > 0

    # Step 2: @agent で Agent を定義
    @agent(skills=["rag"])
    class RAGTestAgent:
        """RAG テストAgent."""
        system_prompt = "RAG検索を行うAgent"

        async def run(self, inputs):
            return {
                "query": inputs.get("query"),
                "has_tools": self._tools is not None,
            }

    # Step 3: AgentRegistry でマッチング
    agent_registry = get_global_agent_registry()
    requirement = CapabilityRequirement(
        description="ドキュメント検索",
        required_tags=["rag"],
    )
    matches = agent_registry.find_matching(requirement)
    assert len(matches) > 0
    assert matches[0][0] == "RAGTestAgent"

    # Step 4: SimpleEngine で実行（自動ツールバインド）
    engine = SimpleEngine(
        agent=RAGTestAgent,
        skills=["rag"],
    )
    result = await engine.run({"query": "テストクエリ"})

    assert result["query"] == "テストクエリ"
    # ツールがバインドされていることを確認
    assert result.get("has_tools") is True
```

**Step 2: Run integration test**

Run: `pytest tests/integration/test_auto_agent_skills_integration.py -v`
Expected: PASS

**Step 3: Update documentation**

`docs/auto-agent-architecture.md` に以下を追加:

```markdown
## 🔗 Skills 統合

### Skills をツールとして発見

```python
from agentflow.core.tool_discovery import ToolDiscoveryService
from agentflow.core.tool_registry import get_global_tool_registry

registry = get_global_tool_registry()
service = ToolDiscoveryService(registry)

# Skills を自動発見してツールとして登録
count = await service.discover_skills_from_engine()
print(f"発見されたスキル: {count}")

# スキルをツールとして取得
rag_tool = registry.get("tool://skill/rag")
```

### @agent と AgentRegistry

```python
from agentflow import agent
from agentflow.core.agent_registry import get_global_agent_registry

@agent(skills=["rag", "chatbot"])
class MyAgent:
    system_prompt = "My Agent"

# AgentRegistry に自動登録
registry = get_global_agent_registry()
matches = registry.find_matching(requirement)
```
```

**Step 4: Commit**

```bash
git add tests/integration/test_auto_agent_skills_integration.py docs/
git commit -m "$(cat <<'EOF'
docs: Update Auto-Agent architecture with Skills integration

- Add integration test for full workflow
- Document Skills discovery as tools
- Document @agent + AgentRegistry integration

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
```

---

## 実行順序まとめ

| Task | 内容 | 依存 |
|------|------|------|
| 1 | ToolDiscoveryService に Skills 発見機能追加 | なし |
| 2 | Skills ディレクトリ再構成 | なし |
| 3 | @agent と AgentRegistry 統合 | Task 1 |
| 4 | SimpleEngine と ToolBinder 統合 | Task 1 |
| 5 | Apps の Skills 移動 | Task 2 |
| 6 | 統合テストとドキュメント | Task 1-5 |

---

## 検証コマンド

```bash
# 全テスト実行
pytest tests/unit/core/test_tool_discovery.py tests/unit/test_agent_decorator_registry.py tests/unit/engines/test_simple_engine_binding.py tests/integration/test_auto_agent_skills_integration.py -v

# カバレッジ確認
pytest --cov=agentflow --cov-report=term-missing
```
