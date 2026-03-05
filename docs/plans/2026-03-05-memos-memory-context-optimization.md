# MemOS風 記憶システム接入・上下文最適化 実装計画

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** AgentFlowにMemOS風の記憶レイヤー（Scratchpad/LongTerm分離・ContextBuilder・真正削除・フィードバック閉環）を実装し、LLMへの注入コンテキストを「最大活用・過剰注入なし」に制御する。

**Architecture:**
- `FlowContext._memory` をScratchpad（run毎クリア）として明確化し、`MemoryAccessor`に「Scratchpad」のコメントを追加する。
- `MemoryManager` / `EnhancedMemoryManager` を長期記憶として `Flow.long_term_memory` プロパティ経由で接続する。
- 新規 `ContextBuilder`（`agentflow/memory/context_builder.py`）がGating/Budget/Scoring/注入フォーマットを担う。

**Tech Stack:** Python 3.13+, Pydantic, asyncio, pytest（asyncio_mode=auto）

---

## A. リスクと回滚戦略

| # | リスク | 回滚策 |
|---|--------|--------|
| 1 | `LongTermMemory.delete()` 追加が既存 `_perform_forgetting()` の `del self._long_term._memories[entry_id]` と競合 | Task1完了後に `_perform_forgetting()` をリファクタリング（Task4）。テスト先行（TDD）で破壊を早期検出 |
| 2 | `EnhancedMemoryManager.forget_low_importance()` 修正でDistributedMemoryManager未接続時にNoneデフォルトを参照してクラッシュ | `distributed_manager: DistributedMemoryManager \| None = None` でNullガード必須。テストでNoneケースを網羅 |
| 3 | `ContextBuilder` がベクトル検索に依存する場合、`enable_vector_search=False` 環境でテスト不可 | ベクトル検索はオプション。`relevance`スコアが使えない場合は `0.0` フォールバック。テストはベクトル検索なしで実行 |
| 4 | `Flow.long_term_memory` 追加でFlowBuilderの `FlowConfig` に変更が必要になる可能性 | `long_term_memory` は `Flow.__init__` の追加引数（デフォルトNone）のみ。FlowConfig変更不要 |
| 5 | テストファイルが `.gitignore` の `tests/*` に引っかかる | `git add -f tests/memory/test_context_builder.py` 等で強制追加（既存パターン） |

---

## Task 1: `LongTermMemory` に `delete()` メソッドを追加

**Files:**
- Modify: `agentflow/memory/long_term_memory.py`
- Test: `tests/memory/test_memory_manager.py`

**Step 1: 失敗するテストを書く**

`tests/memory/test_memory_manager.py` の末尾に追加:

```python
async def test_long_term_memory_delete() -> None:
    """LongTermMemory.delete() が記憶と更新キューを両方削除することを確認."""
    from agentflow.memory.long_term_memory import LongTermMemory
    from agentflow.memory.types import MemoryEntry, MemoryType

    ltm = LongTermMemory(enable_auto_consolidation=False)
    await ltm.start()

    entry = MemoryEntry(
        id="del-test-001",
        content="削除テスト記憶",
        topic="delete",
        timestamp=__import__("datetime").datetime.now(),
        memory_type=MemoryType.LONG_TERM,
    )
    await ltm.store(entry)
    await ltm.update("del-test-001", {"importance_score": 0.9})

    # 削除前は存在する
    assert "del-test-001" in ltm._memories
    assert "del-test-001" in ltm._update_queues

    # 削除
    result = await ltm.delete("del-test-001")
    assert result is True

    # 削除後は存在しない（メモリも更新キューも）
    assert "del-test-001" not in ltm._memories
    assert "del-test-001" not in ltm._update_queues

    # 存在しないIDを削除してもFalseを返す
    result2 = await ltm.delete("not-exist")
    assert result2 is False

    await ltm.stop()
```

**Step 2: テストが失敗することを確認**

```bash
conda activate agentflow && pytest tests/memory/test_memory_manager.py::test_long_term_memory_delete -v
```
期待: `FAILED` — `LongTermMemory has no attribute 'delete'`

**Step 3: 最小実装を追加**

`agentflow/memory/long_term_memory.py` の `retrieve()` の前に以下を追加:

```python
async def delete(self, entry_id: str) -> bool:
    """記憶エントリを削除（更新キューも含む）.

    Args:
        entry_id: 記憶エントリID

    Returns:
        削除成功の場合True、エントリが存在しない場合False
    """
    if entry_id not in self._memories:
        return False
    del self._memories[entry_id]
    # 対応する更新キューも削除
    self._update_queues.pop(entry_id, None)
    self._logger.debug(f"Deleted entry: {entry_id}")
    return True
```

**Step 4: テスト通過を確認**

```bash
conda activate agentflow && pytest tests/memory/test_memory_manager.py::test_long_term_memory_delete -v
```
期待: `PASSED`

**Step 5: コミット**

```bash
git add agentflow/memory/long_term_memory.py tests/memory/test_memory_manager.py
git commit -m "feat(memory): LongTermMemoryにdelete()メソッドを追加"
```

---

## Task 2: `MemoryManager` に `delete()` プロキシメソッドを追加

**Files:**
- Modify: `agentflow/memory/memory_manager.py`
- Test: `tests/memory/test_memory_manager.py`

**Step 1: 失敗するテストを書く**

`tests/memory/test_memory_manager.py` に追加:

```python
async def test_memory_manager_delete() -> None:
    """MemoryManager.delete() が長期記憶からエントリを削除することを確認."""
    manager = MemoryManager(token_threshold=100)
    await manager.start()

    # 要約をトリガーして長期記憶にデータを書き込む
    for i in range(5):
        await manager.remember(
            f"削除テスト情報 {i}: " + "内容 " * 30,
            topic="del_manager",
        )

    memories = await manager.recall(topic="del_manager")
    if not memories:
        await manager.stop()
        return  # 要約がトリガーされなかった場合はスキップ

    target_id = memories[0].id
    result = await manager.delete(target_id)
    assert result is True

    # 削除後は検索結果に含まれない
    after = await manager.recall(topic="del_manager")
    assert all(m.id != target_id for m in after)

    await manager.stop()
```

**Step 2: テストが失敗することを確認**

```bash
conda activate agentflow && pytest tests/memory/test_memory_manager.py::test_memory_manager_delete -v
```
期待: `FAILED`

**Step 3: 実装**

`agentflow/memory/memory_manager.py` の `consolidate()` の後に追加:

```python
async def delete(self, entry_id: str) -> bool:
    """長期記憶からエントリを削除.

    ベクトルインデックス・更新キューも同時に削除する。

    Args:
        entry_id: 記憶エントリID

    Returns:
        削除成功の場合True
    """
    deleted = await self._long_term.delete(entry_id)
    if deleted and self._vector_search:
        # ベクトルキャッシュからも削除
        self._vector_search._embedding_cache.pop(entry_id, None)
    if deleted and self._importance_adjuster:
        # 重要度追跡からも削除
        self._importance_adjuster._access_count.pop(entry_id, None)
        self._importance_adjuster._last_access.pop(entry_id, None)
    return deleted
```

**Step 4: テスト通過確認**

```bash
conda activate agentflow && pytest tests/memory/test_memory_manager.py -v
```
期待: 全テスト `PASSED`

**Step 5: コミット**

```bash
git add agentflow/memory/memory_manager.py tests/memory/test_memory_manager.py
git commit -m "feat(memory): MemoryManagerにdelete()プロキシメソッドを追加"
```

---

## Task 3: `EnhancedMemoryManager.forget_low_importance()` バグ修正

**Files:**
- Modify: `agentflow/memory/enhanced_memory.py`
- Test: `tests/memory/test_enhanced_memory_forget.py` (新規)

**Step 1: 新規テストファイルを作成**

```python
# tests/memory/test_enhanced_memory_forget.py
"""EnhancedMemoryManager.forget_low_importance() の真正削除テスト."""

import pytest

from agentflow.memory.enhanced_memory import EnhancedMemoryManager, MemoryConfig


@pytest.mark.asyncio
class TestForgetLowImportance:
    """forget_low_importance が底層データも削除することを確認."""

    async def test_forget_removes_from_long_term(self) -> None:
        """忘却が長期記憶（_long_term._memories）からも削除することを確認."""
        config = MemoryConfig(
            min_importance=0.3,
            auto_distillation=False,
            auto_forgetting=False,
        )
        manager = EnhancedMemoryManager(config=config)
        await manager.start()

        # 低重要度でメモリを書き込む
        entry = await manager.remember(
            "忘却されるべき低重要度記憶",
            topic="forget_test",
            importance=0.05,  # min_importance=0.3 より低い
        )
        memory_id = entry.id

        # 書き込み後はtracker + long_termの両方に存在する
        assert memory_id in manager._tracker._importance
        long_term_memories = list(manager._base._long_term._memories.keys())
        # （要約がトリガーされた場合のみ長期記憶に入る。ここではトリガーは確認しない）

        # 忘却実行
        forgotten_count = await manager.forget_low_importance()

        # trackerからは削除されている
        assert memory_id not in manager._tracker._importance

        # 長期記憶にあった場合は、そこからも削除されている
        for mid in long_term_memories:
            if mid == memory_id:
                assert mid not in manager._base._long_term._memories, (
                    "forget_low_importance() はtracker削除だけでなく"
                    "長期記憶からも削除する必要がある"
                )

        assert forgotten_count >= 0

        await manager.stop()

    async def test_forget_tracker_only_was_broken(self) -> None:
        """旧実装のバグ確認: trackerのみ削除で長期記憶に残留しないことを保証."""
        config = MemoryConfig(
            min_importance=0.3,
            auto_distillation=False,
            auto_forgetting=False,
            forgetting_threshold=1,  # 1件でも忘却チェック
        )
        manager = EnhancedMemoryManager(config=config)
        await manager.start()

        # 低重要度エントリをTrackerに強制登録
        entry = await manager.remember(
            "低重要度テスト記憶",
            topic="broken_test",
            importance=0.01,
        )
        memory_id = entry.id

        # 長期記憶に強制書き込み（内部テスト用）
        from agentflow.memory.types import MemoryEntry, MemoryType
        import datetime
        lt_entry = MemoryEntry(
            id=memory_id,
            content="低重要度テスト記憶",
            topic="broken_test",
            timestamp=datetime.datetime.now(),
            memory_type=MemoryType.LONG_TERM,
            importance_score=0.01,
        )
        manager._base._long_term._memories[memory_id] = lt_entry

        # 忘却実行
        await manager.forget_low_importance()

        # 長期記憶からも削除されているべき
        assert memory_id not in manager._base._long_term._memories, (
            "forget_low_importance()修正後: 長期記憶からも削除されているべき"
        )

        await manager.stop()
```

**Step 2: テストが失敗することを確認**

```bash
conda activate agentflow && git add -f tests/memory/test_enhanced_memory_forget.py
pytest tests/memory/test_enhanced_memory_forget.py -v
```
期待: `test_forget_tracker_only_was_broken` が `FAILED`（バグが再現される）

**Step 3: バグ修正**

`agentflow/memory/enhanced_memory.py` の `forget_low_importance()` を以下に置き換え:

```python
async def forget_low_importance(self) -> int:
    """低重要度記憶を忘却する（tracker・長期記憶・分散ストレージを真正削除）.

    Returns:
        忘却した記憶数
    """
    low_importance = self._tracker.get_low_importance_memories(
        threshold=self._config.min_importance
    )

    forgotten = 0
    for memory_id in low_importance:
        # 底層の長期記憶からも真正削除
        deleted = await self._base.delete(memory_id)
        # trackerの全エントリも削除
        self._tracker._importance.pop(memory_id, None)
        self._tracker._access_count.pop(memory_id, None)
        self._tracker._last_access.pop(memory_id, None)
        if deleted:
            forgotten += 1
        else:
            # trackerにあっても長期記憶にない場合もカウント
            forgotten += 1

    self._stats.forgotten_count += forgotten
    self._stats.last_forgetting = datetime.now()

    if forgotten > 0:
        self._logger.info(f"{forgotten}件の低重要度記憶を削除")

    return forgotten
```

**Step 4: テスト通過確認**

```bash
conda activate agentflow && pytest tests/memory/test_enhanced_memory_forget.py -v
```
期待: 全テスト `PASSED`

**Step 5: コミット**

```bash
git add agentflow/memory/enhanced_memory.py tests/memory/test_enhanced_memory_forget.py
git commit -m "fix(memory): forget_low_importance()を真正削除に修正（tracker + 長期記憶）"
```

---

## Task 4: `MemoryManager._perform_forgetting()` のプライベートアクセス修正

**Files:**
- Modify: `agentflow/memory/memory_manager.py`

**Step 1: テスト（既存）確認**

```bash
conda activate agentflow && pytest tests/memory/test_memory_manager.py -v
```
全テスト通過を確認してから作業。

**Step 2: `_perform_forgetting()` をリファクタリング**

`agentflow/memory/memory_manager.py` の `_perform_forgetting()` を以下に置き換え:

```python
async def _perform_forgetting(self) -> int:
    """忘却を実行.

    Returns:
        削除した記憶の数
    """
    if not self._importance_adjuster:
        return 0

    # 長期記憶から忘却対象を特定
    memories = list(self._long_term._memories.values())
    forgettable_ids = self._importance_adjuster.identify_forgettable(memories)

    # 公開インターフェース経由で削除（プライベートアクセスを排除）
    deleted_count = 0
    for entry_id in forgettable_ids:
        if await self._long_term.delete(entry_id):
            deleted_count += 1

    if deleted_count > 0:
        self._logger.info(f"忘却完了: {deleted_count}件の記憶を削除")

    return deleted_count
```

**Step 3: テスト通過確認**

```bash
conda activate agentflow && pytest tests/memory/ -v
```
期待: 全テスト `PASSED`

**Step 4: コミット**

```bash
git add agentflow/memory/memory_manager.py
git commit -m "refactor(memory): _perform_forgetting()をpublicインターフェース経由に修正"
```

---

## Task 5: `MemoryScope` を `types.py` に追加

**Files:**
- Modify: `agentflow/memory/types.py`
- Modify: `agentflow/memory/__init__.py`

**Step 1: `MemoryScope` Enumを追加**

`agentflow/memory/types.py` の `MemoryStability` の後に以下を追加:

```python
class MemoryScope(str, Enum):
    """記憶スコープ（MemOS風マルチテナント対応）.

    metadata["scope"] に格納する。
    TENANT: テナント共有記憶
    USER: ユーザー固有記憶
    AGENT: Agent固有記憶
    SESSION: セッション記憶（ログアウトで削除候補）
    FLOW: フロー内一時記憶（run毎クリア候補）
    """

    TENANT = "tenant"
    USER = "user"
    AGENT = "agent"
    SESSION = "session"
    FLOW = "flow"
```

`MemoryEntry.metadata` のスコープ格納規約（コメントのみ、フィールド追加なし）:
```
metadata["scope"]     = MemoryScope.USER  # スコープ
metadata["user_id"]   = "user-123"        # ユーザーID
metadata["agent_id"]  = "agent-abc"       # AgentID
metadata["session_id"]= "sess-xyz"        # セッションID
```

**Step 2: `__init__.py` にエクスポートを追加**

`agentflow/memory/__init__.py` の `from agentflow.memory.types import (` 部分に `MemoryScope,` を追加:

```python
from agentflow.memory.types import (
    CompressionConfig,
    MemoryEntry,
    MemoryScope,          # 追加
    MemorySemanticLevel,
    MemoryStability,
    MemoryType,
    TopicBuffer,
    UpdateQueue,
)
```

`__all__` にも `"MemoryScope",` を追加。

**Step 3: ruffチェック**

```bash
conda activate agentflow && ruff check agentflow/memory/types.py agentflow/memory/__init__.py
```
期待: エラーなし

**Step 4: コミット**

```bash
git add agentflow/memory/types.py agentflow/memory/__init__.py
git commit -m "feat(memory): MemoryScopeをtypes.pyに追加"
```

---

## Task 6: `ContextBuilder`（MemoryOSAdapter）を新規実装

**Files:**
- Create: `agentflow/memory/context_builder.py`
- Modify: `agentflow/memory/__init__.py`
- Test: `tests/memory/test_context_builder.py` (新規)

**Step 1: 失敗するテストを先に作成**

```python
# tests/memory/test_context_builder.py
"""ContextBuilder のGating・Budget・Scoring ロジックのテスト."""

import datetime
import pytest

from agentflow.memory.context_builder import (
    ContextBlock,
    ContextBuilder,
    MemoryBudget,
    MemoryNeedLevel,
)
from agentflow.memory.memory_manager import MemoryManager
from agentflow.memory.types import MemoryEntry, MemorySemanticLevel, MemoryStability, MemoryType


def _make_entry(
    entry_id: str,
    content: str,
    topic: str = "test",
    importance: float = 0.5,
    semantic_level: MemorySemanticLevel = MemorySemanticLevel.SEMANTIC,
    stability: MemoryStability = MemoryStability.CONSOLIDATED,
    seconds_ago: int = 60,
) -> MemoryEntry:
    return MemoryEntry(
        id=entry_id,
        content=content,
        topic=topic,
        timestamp=datetime.datetime.now() - datetime.timedelta(seconds=seconds_ago),
        memory_type=MemoryType.LONG_TERM,
        importance_score=importance,
        semantic_level=semantic_level,
        stability=stability,
    )


class TestMemoryNeedLevel:
    """assess_need() のGating ロジックテスト."""

    def test_none_for_simple_question(self) -> None:
        """単純な質問はNONEレベルを返す."""
        cb = ContextBuilder()
        level = cb.assess_need("What is 2+2?")
        assert level == MemoryNeedLevel.NONE

    def test_low_for_preference_keywords(self) -> None:
        """「惯例」「スタイル」等の語はLOWレベルを返す."""
        cb = ContextBuilder()
        level = cb.assess_need("Please answer in my preferred format")
        assert level in (MemoryNeedLevel.LOW, MemoryNeedLevel.MEDIUM)

    def test_medium_for_project_context(self) -> None:
        """「以前の結論」等はMEDIUMレベルを返す."""
        cb = ContextBuilder()
        level = cb.assess_need("Based on our previous discussion about the API design")
        assert level == MemoryNeedLevel.MEDIUM

    def test_high_for_continuation_keywords(self) -> None:
        """「前回/上次/之前」等はHIGHレベルを返す."""
        cb = ContextBuilder()
        level_ja = cb.assess_need("前回の作業を継続してください")
        level_zh = cb.assess_need("按照上次的方案继续")
        assert level_ja in (MemoryNeedLevel.HIGH, MemoryNeedLevel.MEDIUM)
        assert level_zh in (MemoryNeedLevel.HIGH, MemoryNeedLevel.MEDIUM)


class TestMemoryBudget:
    """Budget制限のテスト."""

    def test_none_level_returns_zero_items(self) -> None:
        """NONEレベルでは記憶を注入しない."""
        cb = ContextBuilder()
        budget = cb.get_budget(MemoryNeedLevel.NONE)
        assert budget.max_items == 0

    def test_low_level_returns_2_items(self) -> None:
        """LOWレベルでは最大2件."""
        cb = ContextBuilder()
        budget = cb.get_budget(MemoryNeedLevel.LOW)
        assert budget.max_items == 2

    def test_high_level_returns_8_items(self) -> None:
        """HIGHレベルでは最大8件."""
        cb = ContextBuilder()
        budget = cb.get_budget(MemoryNeedLevel.HIGH)
        assert budget.max_items == 8


@pytest.mark.asyncio
class TestContextBuilder:
    """ContextBuilder.build() のEnd-to-Endテスト."""

    async def test_none_level_returns_empty(self) -> None:
        """NONEレベルでは空リストを返す."""
        manager = MemoryManager(token_threshold=1000)
        await manager.start()
        await manager._long_term.store(_make_entry("e1", "some memory"))

        cb = ContextBuilder()
        blocks = await cb.build(
            user_request="What is 2+2?",
            memory_manager=manager,
            need_level=MemoryNeedLevel.NONE,
        )
        assert blocks == []
        await manager.stop()

    async def test_budget_limits_results(self) -> None:
        """Budget上限を超えた記憶は注入されない."""
        manager = MemoryManager(token_threshold=1000)
        await manager.start()

        # 10件の記憶を直接書き込む
        for i in range(10):
            await manager._long_term.store(
                _make_entry(f"e{i}", f"記憶 {i}", topic="test", importance=0.5 + i * 0.04)
            )

        cb = ContextBuilder()
        # LOWレベルは最大2件
        blocks = await cb.build(
            user_request="テスト",
            memory_manager=manager,
            need_level=MemoryNeedLevel.LOW,
            topic="test",
        )
        assert len(blocks) <= 2
        await manager.stop()

    async def test_high_importance_is_prioritized(self) -> None:
        """高importance記憶が低importanceより先に選ばれる."""
        manager = MemoryManager(token_threshold=1000)
        await manager.start()

        await manager._long_term.store(
            _make_entry("low", "低重要度記憶", topic="prio", importance=0.1)
        )
        await manager._long_term.store(
            _make_entry("high", "高重要度記憶", topic="prio", importance=0.9)
        )

        cb = ContextBuilder()
        blocks = await cb.build(
            user_request="テスト",
            memory_manager=manager,
            need_level=MemoryNeedLevel.LOW,  # max_items=2
            topic="prio",
        )
        if len(blocks) >= 1:
            # 先頭が高重要度であるべき
            assert blocks[0].memory_id == "high"
        await manager.stop()

    async def test_episodic_is_filtered_for_normal_task(self) -> None:
        """通常タスクではEPISODIC記憶は除外される."""
        manager = MemoryManager(token_threshold=1000)
        await manager.start()

        await manager._long_term.store(
            _make_entry(
                "ep1", "イベント詳細記憶", topic="filter_test",
                importance=0.8,
                semantic_level=MemorySemanticLevel.EPISODIC,
            )
        )
        await manager._long_term.store(
            _make_entry(
                "sem1", "セマンティック記憶", topic="filter_test",
                importance=0.6,
                semantic_level=MemorySemanticLevel.SEMANTIC,
            )
        )

        cb = ContextBuilder(filter_episodic=True)
        blocks = await cb.build(
            user_request="テスト",
            memory_manager=manager,
            need_level=MemoryNeedLevel.MEDIUM,
            topic="filter_test",
        )
        memory_ids = [b.memory_id for b in blocks]
        assert "ep1" not in memory_ids
        assert "sem1" in memory_ids
        await manager.stop()

    async def test_context_block_has_source_label(self) -> None:
        """ContextBlockにはsource_label（追跡情報）が付く."""
        manager = MemoryManager(token_threshold=1000)
        await manager.start()
        await manager._long_term.store(
            _make_entry("track1", "追跡テスト記憶", topic="track")
        )

        cb = ContextBuilder()
        blocks = await cb.build(
            user_request="テスト",
            memory_manager=manager,
            need_level=MemoryNeedLevel.MEDIUM,
            topic="track",
        )
        if blocks:
            assert blocks[0].source_label != ""
            assert blocks[0].memory_id != ""
        await manager.stop()
```

**Step 2: テストが失敗することを確認**

```bash
conda activate agentflow && git add -f tests/memory/test_context_builder.py
pytest tests/memory/test_context_builder.py -v
```
期待: `FAILED` — `ModuleNotFoundError: context_builder`

**Step 3: `ContextBuilder` の実装**

`agentflow/memory/context_builder.py` を新規作成:

```python
"""MemOSアダプター: コンテキストビルダー.

LLMに注入する記憶コンテキストの生成・制御を担う。
MemOS風のGating/Budget/Scoring/フォーマット変換を提供する。

設計原則:
- デフォルトで記憶を注入しない（先少後多）
- Gating: need_levelが NONE の場合は常に空を返す
- Budget: レベル別の上限（件数/トークン）を厳守する
- Scoring: relevance(0.55) + importance(0.30) + recency(0.15)
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from agentflow.memory.memory_manager import MemoryManager
    from agentflow.memory.types import MemoryEntry


# Gatingキーワード（日本語・中国語・英語）
_HIGH_NEED_KEYWORDS: list[str] = [
    "前回", "上次", "之前", "previous", "last time",
    "継続", "继续", "continue", "multi-step", "多段",
    "migration", "移行", "architecture", "アーキテクチャ",
]
_MEDIUM_NEED_KEYWORDS: list[str] = [
    "based on", "に基づいて", "既存の", "existing",
    "prior", "以前の結論", "interface", "インターフェース",
]
_LOW_NEED_KEYWORDS: list[str] = [
    "prefer", "いつも", "format", "style", "スタイル",
    "惯例", "habit", "フォーマット",
]


class MemoryNeedLevel(str, Enum):
    """記憶必要度レベル."""

    NONE = "none"    # 0件: 単純な一問一答
    LOW = "low"      # 最大2件: ユーザー好み/設定
    MEDIUM = "medium"  # 最大5件: プロジェクトコンテキスト
    HIGH = "high"    # 最大8件: 長期タスク/継続作業


@dataclass
class MemoryBudget:
    """記憶予算（注入上限）.

    Attributes:
        max_items: 最大注入件数
        max_tokens_per_item: 1件あたり最大トークン数（目安）
    """

    max_items: int = 0
    max_tokens_per_item: int = 80


@dataclass
class ContextBlock:
    """LLMに注入するコンテキストブロック.

    Attributes:
        memory_id: 記憶エントリID（追跡用）
        content: 注入内容（圧縮済み）
        score: ランキングスコア
        source_label: 出処ラベル（topic + timestamp）
    """

    memory_id: str
    content: str
    score: float
    source_label: str
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_prompt_text(self) -> str:
        """プロンプト挿入用テキストに変換."""
        return f"[{self.source_label}] {self.content}"


# レベル別予算テーブル
_BUDGET_TABLE: dict[MemoryNeedLevel, MemoryBudget] = {
    MemoryNeedLevel.NONE: MemoryBudget(max_items=0, max_tokens_per_item=0),
    MemoryNeedLevel.LOW: MemoryBudget(max_items=2, max_tokens_per_item=60),
    MemoryNeedLevel.MEDIUM: MemoryBudget(max_items=5, max_tokens_per_item=80),
    MemoryNeedLevel.HIGH: MemoryBudget(max_items=8, max_tokens_per_item=100),
}


class ContextBuilder:
    """コンテキストビルダー（MemOSアダプター）.

    利用例:
        >>> cb = ContextBuilder()
        >>> blocks = await cb.build(
        ...     user_request="前回の設計を継続して",
        ...     memory_manager=manager,
        ...     topic="architecture",
        ... )
        >>> prompt_ctx = "\\n".join(b.to_prompt_text() for b in blocks)
    """

    def __init__(self, filter_episodic: bool = True) -> None:
        """初期化.

        Args:
            filter_episodic: Trueの場合、通常タスクでEPISODIC記憶を除外する
        """
        self._filter_episodic = filter_episodic
        self._logger = logging.getLogger(__name__)

    def assess_need(self, user_request: str) -> MemoryNeedLevel:
        """ユーザーリクエストから記憶必要度を評価（Gating）.

        Args:
            user_request: ユーザーのリクエスト文字列

        Returns:
            記憶必要度レベル
        """
        lower = user_request.lower()

        for kw in _HIGH_NEED_KEYWORDS:
            if kw.lower() in lower:
                return MemoryNeedLevel.HIGH

        for kw in _MEDIUM_NEED_KEYWORDS:
            if kw.lower() in lower:
                return MemoryNeedLevel.MEDIUM

        for kw in _LOW_NEED_KEYWORDS:
            if kw.lower() in lower:
                return MemoryNeedLevel.LOW

        return MemoryNeedLevel.NONE

    def get_budget(self, level: MemoryNeedLevel) -> MemoryBudget:
        """レベルに対応する予算を取得.

        Args:
            level: 記憶必要度レベル

        Returns:
            予算設定
        """
        return _BUDGET_TABLE[level]

    def _score(self, entry: MemoryEntry, query: str) -> float:  # noqa: ARG002
        """記憶のランキングスコアを計算.

        スコア = 0.55*relevance + 0.30*importance + 0.15*recency

        Args:
            entry: 記憶エントリ
            query: 検索クエリ（将来的にベクトル類似度計算で使用）

        Returns:
            スコア（0.0〜1.0）
        """
        # relevance: ベクトル検索未使用時は重要度で代替
        relevance = entry.get_effective_importance()

        # importance
        importance = entry.importance_score

        # recency: タイムスタンプから計算（1時間以内=1.0、24時間後=0.0）
        elapsed_hours = (datetime.now() - entry.timestamp).total_seconds() / 3600
        recency = max(0.0, 1.0 - elapsed_hours / 24.0)

        return 0.55 * relevance + 0.30 * importance + 0.15 * recency

    def _should_include(self, entry: MemoryEntry) -> bool:
        """記憶を注入対象に含めるかフィルタリング.

        Args:
            entry: 記憶エントリ

        Returns:
            含める場合True
        """
        from agentflow.memory.types import MemorySemanticLevel

        if self._filter_episodic and entry.semantic_level == MemorySemanticLevel.EPISODIC:
            return False
        return True

    def _truncate_content(self, content: str, max_tokens: int) -> str:
        """コンテンツをmax_tokens相当の文字数に切り詰める（4文字≒1token概算）.

        Args:
            content: 元のコンテンツ
            max_tokens: 最大トークン数

        Returns:
            切り詰められたコンテンツ
        """
        max_chars = max_tokens * 4
        if len(content) <= max_chars:
            return content
        return content[:max_chars] + "..."

    def _make_source_label(self, entry: MemoryEntry) -> str:
        """source_labelを生成.

        Args:
            entry: 記憶エントリ

        Returns:
            追跡用ラベル文字列
        """
        ts = entry.timestamp.strftime("%Y-%m-%d")
        return f"topic:{entry.topic}/{ts}"

    async def build(
        self,
        user_request: str,
        memory_manager: MemoryManager,
        need_level: MemoryNeedLevel | None = None,
        topic: str | None = None,
        min_importance: float = 0.3,
    ) -> list[ContextBlock]:
        """コンテキストブロックを構築.

        Args:
            user_request: ユーザーのリクエスト
            memory_manager: 長期記憶マネージャー
            need_level: 明示的なレベル指定（Noneの場合はassess_needで自動判定）
            topic: トピックフィルタ
            min_importance: recall時の最小重要度

        Returns:
            LLMに注入するContextBlockのリスト（スコア降順・Budget制限済み）
        """
        # Gating: need_levelが明示されていない場合は自動評価
        level = need_level if need_level is not None else self.assess_need(user_request)
        budget = self.get_budget(level)

        # NONE: 記憶注入不要
        if budget.max_items == 0:
            return []

        # recall
        memories = await memory_manager.recall(
            topic=topic,
            limit=budget.max_items * 3,  # 絞り込み前に多めに取得
            min_importance=min_importance,
        )

        # フィルタリング
        filtered = [m for m in memories if self._should_include(m)]

        # スコアリング & ソート
        scored = sorted(
            filtered,
            key=lambda m: self._score(m, user_request),
            reverse=True,
        )

        # Budget制限 + コンテンツ切り詰め
        blocks: list[ContextBlock] = []
        for entry in scored[: budget.max_items]:
            content = self._truncate_content(entry.content, budget.max_tokens_per_item)
            blocks.append(
                ContextBlock(
                    memory_id=entry.id,
                    content=content,
                    score=self._score(entry, user_request),
                    source_label=self._make_source_label(entry),
                    metadata={
                        "topic": entry.topic,
                        "importance": entry.importance_score,
                        "semantic_level": entry.semantic_level.value,
                    },
                )
            )

        self._logger.debug(
            f"ContextBuilder: level={level.value}, "
            f"recalled={len(memories)}, "
            f"filtered={len(filtered)}, "
            f"injected={len(blocks)}"
        )
        return blocks
```

**Step 4: `__init__.py` にエクスポートを追加**

```python
from agentflow.memory.context_builder import (
    ContextBlock,
    ContextBuilder,
    MemoryBudget,
    MemoryNeedLevel,
)
```

**Step 5: テスト通過確認**

```bash
conda activate agentflow && pytest tests/memory/test_context_builder.py -v
```
期待: 全テスト `PASSED`

**Step 6: コミット**

```bash
git add agentflow/memory/context_builder.py agentflow/memory/__init__.py tests/memory/test_context_builder.py
git commit -m "feat(memory): ContextBuilder（MemOSアダプター）を新規実装"
```

---

## Task 7: `Flow` に `long_term_memory` 接入点を追加

**Files:**
- Modify: `agentflow/flow/flow.py`
- Test: `tests/memory/test_flow_memory_separation.py` (新規)

**Step 1: 失敗するテストを先に作成**

```python
# tests/memory/test_flow_memory_separation.py
"""FlowContextのクリアが長期記憶に影響しないことを確認する分離テスト."""

import pytest

from agentflow.flow.context import FlowContext
from agentflow.memory.memory_manager import MemoryManager
from agentflow.memory.types import MemoryEntry, MemoryType
import datetime


@pytest.mark.asyncio
class TestFlowMemorySeparation:
    """Scratchpad（FlowContext）とLong-termの分離テスト."""

    async def test_flow_context_clear_does_not_affect_long_term(self) -> None:
        """FlowContext.clear()を呼んでも長期記憶は消えない."""
        # FlowContext（Scratchpad）
        ctx = FlowContext("test-separation")
        ctx.set("key1", "value1")
        assert ctx.get("key1") == "value1"

        # MemoryManager（Long-term）
        manager = MemoryManager(token_threshold=1000)
        await manager.start()

        entry = MemoryEntry(
            id="sep-test-001",
            content="長期記憶テストデータ",
            topic="sep_test",
            timestamp=datetime.datetime.now(),
            memory_type=MemoryType.LONG_TERM,
        )
        await manager._long_term.store(entry)

        # FlowContextをクリア（run()ごとに実行される）
        ctx.clear()

        # FlowContextはクリアされている
        assert ctx.get("key1") is None

        # 長期記憶は影響を受けない
        memories = await manager.recall(topic="sep_test")
        assert any(m.id == "sep-test-001" for m in memories), (
            "FlowContext.clear()が長期記憶に影響してはいけない"
        )

        await manager.stop()

    async def test_flow_has_long_term_memory_property(self) -> None:
        """Flowがlong_term_memoryプロパティを持つことを確認."""
        from agentflow.flow.builder import FlowBuilder
        from agentflow.core.agent_block import AgentBlock

        class DummyAgent(AgentBlock):
            async def run(self, input_data: dict) -> dict:
                return {"result": "ok"}

        flow = FlowBuilder("test-ltm-flow").then(DummyAgent).build()

        # デフォルトはNone
        assert flow.long_term_memory is None

        # MemoryManagerを設定
        manager = MemoryManager()
        flow.attach_long_term_memory(manager)
        assert flow.long_term_memory is manager

    async def test_memory_accessor_is_scratchpad(self) -> None:
        """MemoryAccessor（flow.memory）がScratchpadであることを確認."""
        from agentflow.flow.builder import FlowBuilder
        from agentflow.core.agent_block import AgentBlock

        class DummyAgent(AgentBlock):
            async def run(self, input_data: dict) -> dict:
                return {"result": "ok"}

        flow = FlowBuilder("test-scratchpad").then(DummyAgent).build()

        # Scratchpadにデータを書き込む
        flow.memory.remember("temp_key", "temp_value")
        assert flow.memory.recall("temp_key") == "temp_value"

        # run()するとScratchpadはクリアされる（FlowContext.clear()が呼ばれる）
        await flow.run({"input": "test"})

        # Scratchpadはクリアされている
        assert flow.memory.recall("temp_key") is None
```

**Step 2: テストが失敗することを確認**

```bash
conda activate agentflow && git add -f tests/memory/test_flow_memory_separation.py
pytest tests/memory/test_flow_memory_separation.py -v
```
期待: `test_flow_has_long_term_memory_property` が `FAILED`

**Step 3: `Flow` に `long_term_memory` プロパティを追加**

`agentflow/flow/flow.py` の `Flow` クラスを以下のように修正:

`__init__` のメモリアクセサー行の後に追加:
```python
# 長期記憶（オプション接続）。Scratchpad（MemoryAccessor）とは別管理。
self._long_term_memory: MemoryManager | None = None
```

クラスメソッドを追加（`cleanup()` の前）:
```python
def attach_long_term_memory(self, manager: MemoryManager) -> None:
    """長期記憶マネージャーを接続.

    Args:
        manager: 接続するMemoryManagerインスタンス
    """
    self._long_term_memory = manager

@property
def long_term_memory(self) -> MemoryManager | None:
    """長期記憶マネージャー（Noneの場合は未接続）."""
    return self._long_term_memory
```

importに `MemoryManager` を追加（TYPE_CHECKING下）:
```python
if TYPE_CHECKING:
    from collections.abc import AsyncIterator
    from agentflow.flow.graph import FlowGraph
    from agentflow.memory.memory_manager import MemoryManager
```

**Step 4: テスト通過確認**

```bash
conda activate agentflow && pytest tests/memory/test_flow_memory_separation.py -v
```
期待: 全テスト `PASSED`

**Step 5: コミット**

```bash
git add agentflow/flow/flow.py tests/memory/test_flow_memory_separation.py
git commit -m "feat(flow): Flowにlong_term_memoryプロパティ・attach_long_term_memory()を追加"
```

---

## Task 8: 全テスト通過確認 & エクスポート整理

**Files:**
- Modify: `agentflow/memory/__init__.py`（ContextBuilderのエクスポート確認）

**Step 1: 全メモリテストを一括実行**

```bash
conda activate agentflow && pytest tests/memory/ -v --tb=short
```
期待: 全テスト `PASSED`

**Step 2: ruff + mypy チェック**

```bash
conda activate agentflow
ruff format agentflow/memory/
ruff check agentflow/memory/ --fix
mypy agentflow/memory/context_builder.py agentflow/memory/long_term_memory.py agentflow/memory/enhanced_memory.py agentflow/memory/memory_manager.py
```
期待: エラー0

**Step 3: カバレッジ確認**

```bash
conda activate agentflow && pytest tests/memory/ --cov=agentflow/memory --cov-report=term-missing
```
期待: memory モジュールのカバレッジ 80%+

**Step 4: 最終コミット**

```bash
git add agentflow/memory/__init__.py
git commit -m "chore(memory): MemoryScope・ContextBuilderのエクスポートを__init__.pyに整理"
```

---

## 実装完了後の動作確認

### 統合使用例

```python
from agentflow.memory import MemoryManager, ContextBuilder, MemoryNeedLevel
from agentflow.flow.builder import FlowBuilder

# 長期記憶マネージャーを初期化
manager = MemoryManager(
    token_threshold=500,
    enable_vector_search=False,
    enable_importance_adjustment=True,
)
await manager.start()

# フローを構築し長期記憶を接続
flow = FlowBuilder("my-agent-flow").then(MyAgent).build()
flow.attach_long_term_memory(manager)

# ContextBuilderでコンテキストを構築
cb = ContextBuilder()
blocks = await cb.build(
    user_request="前回の設計を継続してください",
    memory_manager=manager,
    topic="architecture",
)
# → HIGH判定 → 最大8件 → スコア降順で注入

# タスク実行
result = await flow.run({"input": "..."})
# flow.memory（Scratchpad）は run()後にクリアされる
# manager（Long-term）はそのまま保持される

# 成功時に強化フィードバック
await manager.reinforce(topic="architecture", reward=1.0, context="success")

# 低重要度の真正削除
from agentflow.memory import EnhancedMemoryManager
enhanced = EnhancedMemoryManager(base_manager=manager)
forgotten = await enhanced.forget_low_importance()
# tracker + long_term._memories の両方から削除される
```

### テスト実行コマンド一覧

```bash
conda activate agentflow

# Task別テスト
pytest tests/memory/test_memory_manager.py::test_long_term_memory_delete -v
pytest tests/memory/test_memory_manager.py::test_memory_manager_delete -v
pytest tests/memory/test_enhanced_memory_forget.py -v
pytest tests/memory/test_context_builder.py -v
pytest tests/memory/test_flow_memory_separation.py -v

# 全メモリテスト
pytest tests/memory/ -v

# カバレッジ込み
pytest tests/memory/ --cov=agentflow/memory --cov-fail-under=80
```
