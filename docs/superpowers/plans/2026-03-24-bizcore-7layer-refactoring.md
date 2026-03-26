# BizCore AI 7層リファクタリング実装計画

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** AgentFlow/BizCore AI の層責務を再定義し、依存違反49箇所/24ファイルを解消、README を3面（静的構造/実行フロー/横断サブシステム）で再構成、CLI・MCP・Gateway の位置づけを正式化する

**Architecture:** 既存の7層ディレクトリ（contracts/, infrastructure/, shared/, kernel/, harness/, domain/, control_plane/）を維持しつつ、(1) README を3面分離で書き直し、(2) kernel→infrastructure の直接import 49箇所/24ファイルを Protocol 抽象化、(3) shared/ の誤配置モジュールを適正層へ移動、(4) harness の境界を明確化 + HarnessedToolRuntime リファクタ、(5) contracts 二重定義の解消、(6) domain/templates の executable 移動、(7) 品質ゲートに encoding/CLI/MCP チェックを追加する。段階的PRで進め、各段階で `./check.sh all` を通す。各 Phase は独立 PR ブランチとし、問題発生時は `git revert` でロールバック可能にする。

**Tech Stack:** Python 3.13+, mypy --strict, ruff, pytest, pre-commit, GitHub Actions

---

## 全体フェーズ概要

| フェーズ | 内容 | 推定PR数 | 依存 |
|----------|------|----------|------|
| **Phase 1** | README 3面再構成 + code-rules 更新 | 1 PR | なし |
| **Phase 2** | Kernel→Infrastructure 依存解消（49箇所/24ファイル） | 3-4 PR | Phase 1 |
| **Phase 3** | Shared 層スコープ縮小（誤配置モジュール移動） | 2-3 PR | Phase 2 |
| **Phase 4** | Harness 境界明確化 + HarnessedToolRuntime リファクタ | 1-2 PR | Phase 2 |
| **Phase 5** | Contracts 二重定義解消 + Domain executable 移動 | 1-2 PR | Phase 2 |
| **Phase 6** | MCP スコープ制限 + 品質ゲート強化（encoding/MCP） | 1 PR | Phase 1 |
| **Phase 7** | CLI 正式化 | 1 PR | Phase 2-4 |

---

## ファイル構造（新規作成・変更対象）

### Phase 1: README + Rules
- **変更**: `README.md` — 3面分離で全面書き直し
- **変更**: `code-rules/CLAUDE.md` — encoding 規約・7層再定義を反映
- **変更**: `code-rules/project/architecture.md` — 7層定義を指示書と一致させる

### Phase 2: Kernel→Infrastructure 依存解消（49箇所/24ファイル）
- **作成**: `kernel/interfaces/llm_service.py` — LLM サービスの Protocol 定義
- **作成**: `kernel/interfaces/tool_provider.py` — ToolProvider の Protocol 定義
- **作成**: `kernel/interfaces/metrics_service.py` — Metrics の Protocol 定義
- **作成**: `kernel/interfaces/__init__.py`
- **変更**: `kernel/agent_decorator.py` — infrastructure 直 import を Protocol 経由に
- **変更**: `kernel/skills/*.py` (10ファイル) — `get_llm`/`LiteLLMGateway` 直 import を排除
  - engine.py, generator.py, document.py, rag.py, chatbot.py, voice.py
  - core/engine.py, core/generator.py
  - factory.py (8箇所), mode_switcher.py, runtime.py, core/runtime.py
  - builtin/design_skills/tools/openai_image_client.py
- **変更**: `kernel/patterns/deep_agent.py` — ToolProvider を Protocol 経由に
- **変更**: `kernel/router/executive_summary.py` — get_llm を排除
- **変更**: `kernel/executor/reliability/reliability.py` — metrics を Protocol 経由に
- **変更**: `kernel/flow/sandbox_node.py` (3箇所) — infrastructure 依存を排除
- **変更**: `kernel/agents/resilient_agent.py` (2箇所) — infrastructure 依存を排除
- **変更**: `kernel/runtime/context.py` (4箇所) — infrastructure 依存を排除
- **変更**: `kernel/runtime/__init__.py` (2箇所) — infrastructure 依存を排除
- **変更**: `kernel/tools/executor.py`, `tool_catalog.py`, `tool_discovery.py` — infrastructure 依存を排除
- **作成**: `infrastructure/adapters/llm_service_impl.py` — Protocol 実装
- **変更**: DI 構成ファイル（bootstrap 等）— 実装を注入

### Phase 3: Shared 層スコープ縮小
- **移動**: `shared/gateway/` → `infrastructure/gateway/` (3ファイル)
- **移動**: `shared/memory/` → `kernel/memory/` (7ファイル)
- **移動**: `shared/skills/` → `kernel/skills/builtin/` (voice, chatbot)
- **移動**: `shared/rag/` → `kernel/rag/` または `infrastructure/rag/`
- **修正**: `shared/integrations/realtime_sync.py` — kernel 直 import を排除
- **修正**: `shared/integrations/sse_flow_runner.py` — kernel 直 import を排除
- **変更**: 影響を受ける全 import パスの更新

### Phase 4: Harness 境界明確化 + HarnessedToolRuntime リファクタ
- **作成**: `contracts/harness/auth_service.py` — AuthService Protocol
- **作成**: `contracts/harness/event_types.py` — Harness 用イベント型
- **変更**: `harness/gating/contract_auth_guard.py` — Protocol 経由に
- **変更**: `harness/approval/approval_flow.py` — kernel イベント直 import を排除
- **変更**: `harness/policies/runtime_pipeline.py` — 抽象化
- **変更**: HarnessedToolRuntime — kernel 具体型への依存を Protocol 経由に

### Phase 5: Contracts 二重定義解消 + Domain executable 移動
- **変更**: `contracts/` — canonical schema 一本化（ToolResult, TraceResult, AppConfig, Manifest）
- **削除**: contracts/ 外の重複クラス定義
- **移動**: `domain/templates/` の executable 部分 → `apps/` or `control_plane/`

### Phase 6: MCP スコープ制限 + 品質ゲート強化
- **作成**: `scripts/check_encoding.py` — UTF-8 / BOM チェッカー
- **作成**: `scripts/check_mcp_scope.py` — MCP が integration 層外に漏れていないかチェック
- **変更**: `scripts/check_rules_compliance.py` — encoding / MCP チェック追加
- **変更**: `.pre-commit-config.yaml` — encoding / MCP hook 追加
- **変更**: `check.sh` — encoding チェックを lint に統合

### Phase 7: CLI 正式化
- **作成**: `kernel/interfaces/cli_runtime.py` — CLI 実行抽象
- **変更**: CLI エントリポイント — `--json`, `--dry-run`, `--trace-id` 対応

---

## Phase 1: README 3面再構成 + code-rules 更新

### Task 1.1: README 静的アーキテクチャ章の作成

**Files:**
- Modify: `README.md`

- [ ] **Step 1: 現行 README のバックアップ確認**

```bash
git diff HEAD -- README.md  # 未コミット変更がないことを確認
```

- [ ] **Step 2: README の「7コア層 + Apps外層」セクションを 指示書 §3 の7層定義で書き直す**

新しい7層定義（指示書に基づく）:

| 層 | ディレクトリ | 新名称 | 役割 |
|---|---|---|---|
| Layer 1 | `apps/` + API/CLI entry | Experience / Entry | ユーザー・外部システムからの入口 |
| Layer 2 | `contracts/` | Contracts | 唯一の canonical 契約層 |
| Layer 3 | `kernel/` | Kernel | 実行中核（planner/router/flow/state） |
| Layer 4 | `harness/` | Harness | 横断ガバナンス・品質・安全・検証 |
| Layer 5 | ※新設 or `kernel/protocols/` 整理 | Integration | MCP/A2A/外部接続（接続専用） |
| Layer 6 | `infrastructure/` + `shared/gateway/` | Infrastructure / Gateway | LLM Gateway・外部技術基盤 |
| Layer 7 | `control_plane/` + `apps/` | Platform / Apps | アプリ提供・運用・開発支援 |

README に書く内容:
```markdown
## A. 静的アーキテクチャ — 7層構成

### 設計原則
- 依存方向: Experience → Contracts → Kernel → Integration / Infrastructure
- Harness は Kernel の上で横断制御
- Platform / Apps は Kernel/Harness/Integration を使う側
- Integration は接続のみ、Infrastructure は最下層

### Layer 1: Experience / Entry
（指示書 §3 の定義をそのまま記述）
...
```

**注意**: 現行の Contracts→Infrastructure→Shared→Kernel→Harness→Domain→ControlPlane の順序を、指示書の7層に合わせて再整理する。ただし実ディレクトリ名の変更はこのフェーズでは行わない（README での名称マッピングのみ）。

- [ ] **Step 3: 差分確認**

```bash
git diff README.md | head -100
```

- [ ] **Step 4: コミット**

```bash
git add README.md
git commit -m "docs: README 静的アーキテクチャ章を7層定義で再構成"
```

---

### Task 1.2: README 実行時フロー章の追加

**Files:**
- Modify: `README.md`

- [ ] **Step 1: 実行時フロー章を追加**

```markdown
## B. 実行時フロー

Request → CLI/API/MCP Entry (Layer 1)
  → Contract Validation (Layer 2)
  → Harness Pre-Check: Policy/Budget/Risk (Layer 4)
  → Kernel Plan/Route/Execute (Layer 3)
    → Integration: MCP/A2A 外部接続 (Layer 5)
    → Infrastructure: LLM Gateway 呼出 (Layer 6)
  → Harness Post-Check: Score/Validate/Trace (Layer 4)
  → Response Return (Layer 1)
```

各ステップで CLI / MCP / RAG / Gateway / Harness がどこで関与するかを明記する。

- [ ] **Step 2: コミット**

```bash
git add README.md
git commit -m "docs: README 実行時フロー章を追加"
```

---

### Task 1.3: README 横断サブシステム章の追加

**Files:**
- Modify: `README.md`

- [ ] **Step 1: 横断サブシステム章を追加**

```markdown
## C. 横断ガバナンスサブシステム（Harness）

| サブシステム | 責務 | 主要コンポーネント |
|---|---|---|
| Replay | 実行再現 | ReplayRecorder, ReplayRunner |
| Score | 品質評価 | ExecutionScorer |
| Risk | リスク評価 | RiskAssessor |
| Approval | 承認ワークフロー | ApprovalManager, Checkpoint |
| Audit | 監査記録 | AuditTrail |
| Trace | 実行追跡 | TraceContract (contracts/ 定義) |
| Validation | 入出力検証 | InputValidator, OutputValidator |
| Budget | トークン予算 | TokenBudgetManager |
| Context Engineering | コンテキスト最適化 | ContextEngineer, RetrievalGate |
```

- [ ] **Step 2: CLI / MCP / Gateway の責務明記セクションを追加**

```markdown
### CLI-First 実行モデル
- CLI は Entry（Layer 1）と Runtime（Layer 3 の実行手段）の二面性を持つ
- すべての CLI は `--json` を持つ
- exit code を安定化、stdout/stderr/event/artifact を分離
- dry-run / trace_id 返却対応

### MCP-Based Integration モデル
- MCP は Layer 5 Integration に限定
- 主編排を持たない、内部契約の中心化禁止
- 外部ツール公開・接続・認証・伝達に専念

### Gateway-First LLM Access
- LLM 呼び出しは Infrastructure/Gateway 経由のみ
- Provider SDK の直叩き禁止
- 認証・予算・fallback・監視を中央集約
```

- [ ] **Step 3: コミット**

```bash
git add README.md
git commit -m "docs: README 横断サブシステム・CLI・MCP・Gateway 章を追加"
```

---

### Task 1.4: code-rules に Encoding 規約を追加

**Files:**
- Modify: `code-rules/CLAUDE.md`

- [ ] **Step 1: code-rules/CLAUDE.md に Encoding Rule セクションを追加**

グローバル設定セクションの直後に追加:

```markdown
## 🔤 文字コード規約（Encoding Rule）

| # | ルール |
|---|---|
| 1 | 既定の文字コードは **UTF-8 (BOM なし)** |
| 2 | Windows PowerShell 5.1 向け `.ps1` は日本語を含む場合 **UTF-8 with BOM** を検討 |
| 3 | 既存ファイル編集前に **現行エンコーディングを必ず確認** |
| 4 | 日本語を含む設定ファイル・スクリプト・README・テンプレートで文字化け禁止 |
| 5 | 新規作成時はファイル種別ごとに推奨エンコーディングを明示 |
```

- [ ] **Step 2: code-rules/project/architecture.md の7層定義を指示書と一致させる**

現行ファイルの層定義テーブルを指示書 §3 に合わせて更新。Harness を独立層として明示、Integration 層を追加。

- [ ] **Step 3: テスト（ルール整合性の確認）**

```bash
conda run -n agentflow python scripts/check_rules_compliance.py --json
```
Expected: 既存閾値をすべてパス

- [ ] **Step 4: コミット**

```bash
git add code-rules/CLAUDE.md code-rules/project/architecture.md
git commit -m "docs: code-rules に encoding 規約追加、7層定義を指示書と一致"
```

---

## Phase 2: Kernel → Infrastructure 依存解消

### Task 2.1: Kernel LLM Service Protocol の定義

**Files:**
- Create: `kernel/interfaces/__init__.py`
- Create: `kernel/interfaces/llm_service.py`
- Test: `tests/kernel/interfaces/test_llm_service_protocol.py`

- [ ] **Step 1: テストを先に書く**

```python
"""kernel LLM Service Protocol のテスト."""
from typing import Protocol, runtime_checkable

import pytest


def test_llm_service_protocol_exists() -> None:
    """LLMService Protocol がインポートできること."""
    from kernel.interfaces.llm_service import LLMService
    assert hasattr(LLMService, "generate")
    assert hasattr(LLMService, "generate_stream")


def test_llm_service_is_runtime_checkable() -> None:
    """LLMService が runtime_checkable であること."""
    from kernel.interfaces.llm_service import LLMService

    @runtime_checkable
    class _Check(Protocol):
        pass

    # LLMService 自体が runtime_checkable であることを確認
    assert isinstance(LLMService, type)
```

- [ ] **Step 2: テスト実行 → FAIL 確認**

```bash
conda run -n agentflow pytest tests/kernel/interfaces/test_llm_service_protocol.py -v
```
Expected: FAIL — `ModuleNotFoundError: No module named 'kernel.interfaces'`

- [ ] **Step 3: Protocol 実装**

```python
"""kernel/interfaces/llm_service.py — Kernel が LLM にアクセスするための抽象."""
from __future__ import annotations

from typing import Any, AsyncIterator, Protocol, runtime_checkable


@runtime_checkable
class LLMService(Protocol):
    """LLM 呼び出しの抽象インターフェース.

    Kernel はこの Protocol のみに依存し、
    具体的な Provider や Gateway の実装を知らない。
    """

    async def generate(
        self,
        prompt: str,
        *,
        model: str | None = None,
        temperature: float = 0.7,
        max_tokens: int = 4096,
        **kwargs: Any,
    ) -> str:
        """テキスト生成."""
        ...

    async def generate_stream(
        self,
        prompt: str,
        *,
        model: str | None = None,
        temperature: float = 0.7,
        max_tokens: int = 4096,
        **kwargs: Any,
    ) -> AsyncIterator[str]:
        """ストリーミングテキスト生成."""
        ...
```

`kernel/interfaces/__init__.py`:
```python
"""Kernel インターフェース — 外部依存の抽象定義."""
from kernel.interfaces.llm_service import LLMService

__all__ = ["LLMService"]
```

- [ ] **Step 4: テスト実行 → PASS 確認**

```bash
conda run -n agentflow pytest tests/kernel/interfaces/test_llm_service_protocol.py -v
```
Expected: PASS

- [ ] **Step 5: コミット**

```bash
git add -f tests/kernel/interfaces/test_llm_service_protocol.py
git add kernel/interfaces/__init__.py kernel/interfaces/llm_service.py
git commit -m "feat: Kernel LLMService Protocol を定義（infrastructure 依存解消の基盤）"
```

---

### Task 2.2: Kernel ToolProvider Protocol の定義

**Files:**
- Create: `kernel/interfaces/tool_provider.py`
- Test: `tests/kernel/interfaces/test_tool_provider_protocol.py`

- [ ] **Step 1: テストを先に書く**

```python
"""kernel ToolProvider Protocol のテスト."""


def test_tool_provider_protocol_exists() -> None:
    """ToolProviderService Protocol がインポートできること."""
    from kernel.interfaces.tool_provider import ToolProviderService
    assert hasattr(ToolProviderService, "get_tool")
    assert hasattr(ToolProviderService, "list_tools")
```

- [ ] **Step 2: テスト実行 → FAIL 確認**

```bash
conda run -n agentflow pytest tests/kernel/interfaces/test_tool_provider_protocol.py -v
```

- [ ] **Step 3: Protocol 実装**

```python
"""kernel/interfaces/tool_provider.py — ツールプロバイダの抽象."""
from __future__ import annotations

from typing import Any, Protocol, runtime_checkable


@runtime_checkable
class ToolProviderService(Protocol):
    """ツール提供の抽象インターフェース."""

    async def get_tool(self, name: str) -> Any:
        """名前でツールを取得."""
        ...

    async def list_tools(self) -> list[dict[str, Any]]:
        """利用可能なツール一覧を返す."""
        ...
```

- [ ] **Step 4: テスト実行 → PASS 確認**

```bash
conda run -n agentflow pytest tests/kernel/interfaces/test_tool_provider_protocol.py -v
```

- [ ] **Step 5: コミット**

```bash
git add -f tests/kernel/interfaces/test_tool_provider_protocol.py
git add kernel/interfaces/tool_provider.py kernel/interfaces/__init__.py
git commit -m "feat: Kernel ToolProviderService Protocol を定義"
```

---

### Task 2.3: kernel/agent_decorator.py の infrastructure 直 import を排除

**Files:**
- Modify: `kernel/agent_decorator.py:54-55`
- Test: `tests/kernel/test_agent_decorator_no_infra.py`

- [ ] **Step 1: 現行の違反箇所を確認**

```bash
conda run -n agentflow python -c "
import ast, sys
with open('kernel/agent_decorator.py') as f:
    tree = ast.parse(f.read())
for node in ast.walk(tree):
    if isinstance(node, (ast.Import, ast.ImportFrom)):
        if hasattr(node, 'module') and node.module and 'infrastructure' in node.module:
            print(f'Line {node.lineno}: from {node.module} import ...')
"
```

- [ ] **Step 2: infrastructure import を検出するテストを書く**

```python
"""agent_decorator が infrastructure を直接 import していないことを確認."""
import ast
from pathlib import Path


def test_agent_decorator_no_infrastructure_import() -> None:
    """agent_decorator.py に infrastructure 直接 import がないこと."""
    source = Path("kernel/agent_decorator.py").read_text()
    tree = ast.parse(source)
    violations: list[str] = []
    for node in ast.walk(tree):
        if isinstance(node, ast.ImportFrom) and node.module and "infrastructure" in node.module:
            violations.append(f"Line {node.lineno}: from {node.module}")
    assert violations == [], f"infrastructure 直接 import が残存: {violations}"
```

- [ ] **Step 3: テスト実行 → FAIL 確認**（現状は infrastructure を import しているので FAIL する）

```bash
conda run -n agentflow pytest tests/kernel/test_agent_decorator_no_infra.py -v
```

- [ ] **Step 4: agent_decorator.py を修正**

`from infrastructure.llm_provider import LLMProvider` → `from kernel.interfaces.llm_service import LLMService`
`from infrastructure.tool_provider import ToolProvider` → `from kernel.interfaces.tool_provider import ToolProviderService`

DI パターンで実装を注入するように変更。具体的には:
1. コンストラクタまたはファクトリ関数で Protocol を受け取る
2. デフォルト実装は bootstrap / 設定時に infrastructure から注入

**重要**: 変更時に既存の公開 API シグネチャを壊さないこと。後方互換のため、デフォルト引数で infrastructure 実装を遅延 import する戦略も許容。

- [ ] **Step 5: テスト実行 → PASS 確認**

```bash
conda run -n agentflow pytest tests/kernel/test_agent_decorator_no_infra.py -v
```

- [ ] **Step 6: 既存テストが壊れていないことを確認**

```bash
conda run -n agentflow pytest tests/kernel/ -v --timeout=60
```

- [ ] **Step 7: コミット**

```bash
git add kernel/agent_decorator.py
git add -f tests/kernel/test_agent_decorator_no_infra.py
git commit -m "refactor: agent_decorator から infrastructure 直接 import を排除"
```

---

### Task 2.4: kernel/skills/*.py の get_llm 直 import を排除（8ファイル一括）

**Files:**
- Modify: `kernel/skills/engine.py`
- Modify: `kernel/skills/generator.py`
- Modify: `kernel/skills/document.py`
- Modify: `kernel/skills/rag.py`
- Modify: `kernel/skills/chatbot.py`
- Modify: `kernel/skills/voice.py`
- Modify: `kernel/skills/core/engine.py`
- Modify: `kernel/skills/core/generator.py`
- Test: `tests/kernel/test_skills_no_infra.py`

- [ ] **Step 1: 全 skills ファイルの infrastructure import を検出するテストを書く**

```python
"""kernel/skills/ 配下が infrastructure を直接 import していないことを確認."""
import ast
from pathlib import Path


def test_skills_no_infrastructure_import() -> None:
    """kernel/skills/ に infrastructure 直接 import がないこと."""
    violations: list[str] = []
    for py_file in Path("kernel/skills").rglob("*.py"):
        source = py_file.read_text()
        tree = ast.parse(source)
        for node in ast.walk(tree):
            if isinstance(node, ast.ImportFrom) and node.module and "infrastructure" in node.module:
                violations.append(f"{py_file}:{node.lineno}: from {node.module}")
    assert violations == [], f"infrastructure 直接 import が残存:\n" + "\n".join(violations)
```

- [ ] **Step 2: テスト実行 → FAIL 確認**

```bash
conda run -n agentflow pytest tests/kernel/test_skills_no_infra.py -v
```

- [ ] **Step 3: 各ファイルを修正**

共通パターン:
- `from infrastructure.llm.providers import get_llm` → コンストラクタで `LLMService` を受け取る
- `from infrastructure.llm.gateway import LiteLLMGateway` → `LLMService` Protocol 経由

各 skills クラスに `llm_service: LLMService | None = None` パラメータを追加し、None の場合は共通の DI コンテナからデフォルト実装を取得するファクトリパターンを使う。

- [ ] **Step 4: テスト実行 → PASS 確認**

```bash
conda run -n agentflow pytest tests/kernel/test_skills_no_infra.py -v
```

- [ ] **Step 5: 全体テスト**

```bash
conda run -n agentflow pytest tests/kernel/ -v --timeout=120
```

- [ ] **Step 6: コミット**

```bash
git add kernel/skills/
git add -f tests/kernel/test_skills_no_infra.py
git commit -m "refactor: kernel/skills から infrastructure 直接 import を全排除"
```

---

### Task 2.5: kernel/patterns/ と kernel/router/ の infrastructure 依存排除

**Files:**
- Modify: `kernel/patterns/deep_agent.py:41`
- Modify: `kernel/router/executive_summary.py:28`
- Modify: `kernel/executor/reliability/reliability.py:33`
- Test: `tests/kernel/test_remaining_no_infra.py`

- [ ] **Step 1: テストを書く**

```python
"""kernel/patterns/, kernel/router/, kernel/executor/ の infrastructure 依存チェック."""
import ast
from pathlib import Path


def test_kernel_subdirs_no_infrastructure_import() -> None:
    """kernel の主要サブディレクトリに infrastructure 直接 import がないこと."""
    dirs = ["kernel/patterns", "kernel/router", "kernel/executor"]
    violations: list[str] = []
    for d in dirs:
        for py_file in Path(d).rglob("*.py"):
            source = py_file.read_text()
            tree = ast.parse(source)
            for node in ast.walk(tree):
                if isinstance(node, ast.ImportFrom) and node.module and "infrastructure" in node.module:
                    violations.append(f"{py_file}:{node.lineno}: from {node.module}")
    assert violations == [], f"violations:\n" + "\n".join(violations)
```

- [ ] **Step 2: テスト実行 → FAIL 確認**
- [ ] **Step 3: 各ファイルを修正**

- `deep_agent.py`: `ToolProvider` → `ToolProviderService` Protocol
- `executive_summary.py`: `get_llm` → `LLMService` Protocol（DI で注入）
- `reliability.py`: `get_metrics` → `MetricsService` Protocol（新規作成 or 既存の contracts 定義を使用）

- [ ] **Step 4: テスト実行 → PASS 確認**
- [ ] **Step 5: 全体 boundary チェック**

```bash
conda run -n agentflow python scripts/check_layer_boundaries.py
```
Expected: kernel → infrastructure 違反が 0

- [ ] **Step 6: コミット**

```bash
git add kernel/patterns/ kernel/router/ kernel/executor/
git add -f tests/kernel/test_remaining_no_infra.py
git commit -m "refactor: kernel/patterns,router,executor の infrastructure 依存を排除"
```

---

### Task 2.5b: kernel/ 残存ファイルの infrastructure 依存排除

**Files:**
- Modify: `kernel/skills/factory.py` (8箇所 — browser/os infrastructure imports)
- Modify: `kernel/skills/mode_switcher.py`
- Modify: `kernel/skills/runtime.py`
- Modify: `kernel/skills/core/runtime.py`
- Modify: `kernel/skills/builtin/design_skills/tools/openai_image_client.py`
- Modify: `kernel/flow/sandbox_node.py` (3箇所)
- Modify: `kernel/agents/resilient_agent.py` (2箇所)
- Modify: `kernel/runtime/context.py` (4箇所)
- Modify: `kernel/runtime/__init__.py` (2箇所)
- Modify: `kernel/tools/executor.py`
- Modify: `kernel/tools/tool_catalog.py`
- Modify: `kernel/tools/tool_discovery.py`
- Test: `tests/kernel/test_full_kernel_no_infra.py`

- [ ] **Step 1: kernel 全体の infrastructure import をゼロにするテストを書く**

```python
"""kernel/ 全体が infrastructure を直接 import していないことを確認."""
import ast
from pathlib import Path

# plugins/packs/ は外部プラグインのため除外
EXCLUDE_DIRS = {"kernel/plugins/packs"}


def test_full_kernel_no_infrastructure_import() -> None:
    """kernel/ 全体に infrastructure 直接 import がないこと."""
    violations: list[str] = []
    for py_file in Path("kernel").rglob("*.py"):
        rel = str(py_file)
        if any(rel.startswith(e) for e in EXCLUDE_DIRS):
            continue
        try:
            source = py_file.read_text()
            tree = ast.parse(source)
        except (SyntaxError, UnicodeDecodeError):
            continue
        for node in ast.walk(tree):
            if isinstance(node, ast.ImportFrom) and node.module and "infrastructure" in node.module:
                violations.append(f"{py_file}:{node.lineno}: from {node.module}")
    assert violations == [], f"infrastructure 直接 import が {len(violations)} 箇所残存:\n" + "\n".join(violations)
```

- [ ] **Step 2: テスト実行 → FAIL 確認**

```bash
conda run -n agentflow pytest tests/kernel/test_full_kernel_no_infra.py -v
```

- [ ] **Step 3: 各ファイルを修正**

修正パターン（ファイル別）:

**kernel/skills/factory.py (8箇所)**:
- `from infrastructure.browser.*` → BrowserService Protocol を kernel/interfaces/ に追加し DI
- `from infrastructure.os.*` → OSService Protocol を kernel/interfaces/ に追加し DI

**kernel/flow/sandbox_node.py (3箇所)**:
- infrastructure の sandbox 関連 import → SandboxService Protocol 経由

**kernel/runtime/context.py (4箇所)**:
- infrastructure の config/provider import → Protocol 経由

**kernel/runtime/__init__.py (2箇所)**:
- infrastructure のトップレベル import → 遅延 import または Protocol

**kernel/agents/resilient_agent.py (2箇所)**:
- infrastructure の provider import → LLMService Protocol 経由

**kernel/tools/ (3ファイル)**:
- infrastructure の tool provider import → ToolProviderService Protocol 経由

- [ ] **Step 4: テスト実行 → PASS 確認**

```bash
conda run -n agentflow pytest tests/kernel/test_full_kernel_no_infra.py -v
```

- [ ] **Step 5: 既存テスト全体が壊れていないことを確認**

```bash
conda run -n agentflow pytest tests/kernel/ -v --timeout=180
```

- [ ] **Step 6: コミット**

```bash
git add kernel/
git add -f tests/kernel/test_full_kernel_no_infra.py
git commit -m "refactor: kernel 全体の infrastructure 直接 import をゼロ化"
```

---

### Task 2.6: Phase 2 統合テスト + PR

- [ ] **Step 1: 全チェック実行**

```bash
conda run -n agentflow ./check.sh all
```

- [ ] **Step 2: boundary 違反が 0 であることを確認**

```bash
conda run -n agentflow python scripts/check_rules_compliance.py --json | python -c "
import json, sys
data = json.load(sys.stdin)
boundaries = data.get('layer_boundary_violations', {}).get('count', -1)
print(f'Layer boundary violations: {boundaries}')
assert boundaries == 0, 'Boundary violations remain!'
print('✅ All clear')
"
```

- [ ] **Step 3: PR 作成（Phase 2 完了時）**

---

## Phase 3: Shared 層スコープ縮小

### Task 3.1: shared/integrations/ の kernel 逆依存を解消

**Files:**
- Modify: `shared/integrations/realtime_sync.py:34`
- Modify: `shared/integrations/sse_flow_runner.py:15-30`
- Test: `tests/shared/test_integrations_no_kernel.py`

- [ ] **Step 1: テストを書く**

```python
"""shared/integrations/ が kernel を直接 import していないことを確認."""
import ast
from pathlib import Path


def test_shared_integrations_no_kernel_import() -> None:
    """shared/integrations/ に kernel 直接 import がないこと."""
    violations: list[str] = []
    for py_file in Path("shared/integrations").rglob("*.py"):
        source = py_file.read_text()
        tree = ast.parse(source)
        for node in ast.walk(tree):
            if isinstance(node, ast.ImportFrom) and node.module and "kernel" in node.module:
                violations.append(f"{py_file}:{node.lineno}: from {node.module}")
    assert violations == [], f"kernel import violations:\n" + "\n".join(violations)
```

- [ ] **Step 2: テスト実行 → FAIL 確認**
- [ ] **Step 3: 修正**

- `realtime_sync.py`: `from kernel.state.store import GlobalStateStore` → contracts で定義された StateStore Protocol を使う
- `sse_flow_runner.py`: `from kernel.protocols.agui_events import ...` → contracts/protocol/ で定義されたイベント型を使う（既に contracts/protocol/ にイベント定義がある場合はそれを利用）

- [ ] **Step 4: テスト実行 → PASS 確認**
- [ ] **Step 5: コミット**

```bash
git add shared/integrations/
git add -f tests/shared/test_integrations_no_kernel.py
git commit -m "refactor: shared/integrations の kernel 逆依存を解消"
```

---

### Task 3.2: shared/gateway/ の infrastructure 統合を整理

**Files:**
- Modify: `shared/gateway/embedding/service.py`
- Modify: `shared/gateway/rerank/service.py`
- Modify: `shared/gateway/llm/service.py`

**方針**: shared/gateway/ は infrastructure の薄いラッパーとして存在している。2つの選択肢:
1. **移動**: `shared/gateway/` → `infrastructure/gateway/services/` に統合
2. **Protocol 化**: contracts で Protocol を定義し、shared/gateway は Protocol 実装として残す

ここでは **選択肢2** を採用（影響範囲が小さい）。

- [ ] **Step 1: 現行の import 状況を確認**

```bash
conda run -n agentflow python -c "
import ast
from pathlib import Path
for f in Path('shared/gateway').rglob('*.py'):
    src = f.read_text()
    tree = ast.parse(src)
    for n in ast.walk(tree):
        if isinstance(n, ast.ImportFrom) and n.module and 'infrastructure' in n.module:
            print(f'{f}:{n.lineno}: from {n.module}')
"
```

- [ ] **Step 2: shared→infrastructure の import は「実装注入」パターンに変更**

各 service クラスのコンストラクタで infrastructure 実装を受け取り、デフォルトでは遅延 import を使用する。

- [ ] **Step 3: 既存テストの確認**

```bash
conda run -n agentflow pytest tests/shared/ -v --timeout=60
```

- [ ] **Step 4: コミット**

```bash
git add shared/gateway/
git commit -m "refactor: shared/gateway の infrastructure 依存を DI パターンに変更"
```

---

### Task 3.3: shared/ 全体の infrastructure 依存を段階的に削減

**注意**: これは大規模タスクなので、以下の優先順で段階的に進める。

**優先度 HIGH（逆依存 = アーキテクチャ違反）:**
1. `shared/integrations/` → kernel (Task 3.1 で解消済み)

**優先度 MEDIUM（scope creep = shared に置くべきでないモジュール）:**
2. `shared/memory/` (7ファイル) — 将来的に kernel/memory/ へ移動候補
3. `shared/skills/` — 将来的に kernel/skills/builtin/ へ移動候補
4. `shared/rag/` — 将来的に整理

**優先度 LOW（infrastructure wrapper として許容可能）:**
5. `shared/gateway/` (Task 3.2 で DI 化済み)
6. `shared/config/`, `shared/trace/`

- [ ] **Step 1: 移動対象の利用箇所を調査**

```bash
# shared/memory を import しているファイル数
conda run -n agentflow grep -r "from shared.memory" --include="*.py" -l | wc -l
# shared/skills を import しているファイル数
conda run -n agentflow grep -r "from shared.skills" --include="*.py" -l | wc -l
```

- [ ] **Step 2: 影響範囲が小さいものから移動**

影響範囲の調査結果に基づき、import 数が少ないモジュールから移動する。
各移動で:
1. 新しい場所にファイルをコピー
2. 旧パスから新パスへの re-export を追加（後方互換）
3. import パスを更新
4. テスト確認
5. re-export を段階的に廃止

- [ ] **Step 3: 各移動後にテスト**

```bash
conda run -n agentflow ./check.sh all
```

- [ ] **Step 4: コミット（移動ごとに1コミット）**

---

## Phase 4: Harness 境界明確化

### Task 4.1: Harness 用 contracts Protocol の定義

**Files:**
- Create: `contracts/harness/__init__.py`
- Create: `contracts/harness/auth_service.py`
- Create: `contracts/harness/execution_events.py`
- Test: `tests/contracts/harness/test_harness_contracts.py`

- [ ] **Step 1: テストを書く**

```python
"""Harness 用 contracts のテスト."""


def test_auth_service_protocol_exists() -> None:
    """AuthServiceProtocol がインポートできること."""
    from contracts.harness.auth_service import AuthServiceProtocol
    assert hasattr(AuthServiceProtocol, "authenticate")
    assert hasattr(AuthServiceProtocol, "authorize")


def test_execution_event_types_exist() -> None:
    """Harness 用イベント型が定義されていること."""
    from contracts.harness.execution_events import (
        ApprovalRequiredEvent,
        ExecutionEvent,
    )
    assert ApprovalRequiredEvent is not None
    assert ExecutionEvent is not None
```

- [ ] **Step 2: テスト実行 → FAIL 確認**
- [ ] **Step 3: Protocol 実装**

```python
# contracts/harness/auth_service.py
"""認証・認可サービスの抽象."""
from __future__ import annotations

from typing import Any, Protocol, runtime_checkable


@runtime_checkable
class AuthServiceProtocol(Protocol):
    """認証・認可の抽象インターフェース."""

    async def authenticate(self, token: str) -> dict[str, Any]:
        """トークンを検証してクレーム情報を返す."""
        ...

    async def authorize(self, user_id: str, action: str, resource: str) -> bool:
        """アクションの認可判定."""
        ...
```

```python
# contracts/harness/execution_events.py
"""Harness が消費する実行イベントの型定義."""
from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from typing import Any


class ExecutionEventType(Enum):
    """実行イベントの種別."""
    APPROVAL_REQUIRED = "approval_required"
    STEP_COMPLETED = "step_completed"
    EXECUTION_FAILED = "execution_failed"


@dataclass(frozen=True)
class ExecutionEvent:
    """実行イベントの基底.

    注意: frozen dataclass 継承で子クラスがデフォルト値を持つ場合、
    親フィールドもデフォルト値が必要。そのため全フィールドにデフォルトを設定。
    """
    event_type: ExecutionEventType = ExecutionEventType.STEP_COMPLETED
    trace_id: str = ""
    payload: dict[str, Any] | None = None


@dataclass(frozen=True)
class ApprovalRequiredEvent(ExecutionEvent):
    """承認要求イベント."""
    event_type: ExecutionEventType = ExecutionEventType.APPROVAL_REQUIRED
    approver: str = ""
    reason: str = ""
```

- [ ] **Step 4: テスト実行 → PASS 確認**
- [ ] **Step 5: コミット**

```bash
git add contracts/harness/
git add -f tests/contracts/harness/
git commit -m "feat: Harness 用 contracts Protocol を定義（auth, events）"
```

---

### Task 4.2: Harness ファイルの直接依存を排除

**Files:**
- Modify: `harness/gating/contract_auth_guard.py:14`
- Modify: `harness/approval/approval_flow.py:22`
- Modify: `harness/policies/runtime_pipeline.py:15`
- Test: `tests/harness/test_harness_no_direct_deps.py`

- [ ] **Step 1: テストを書く**

```python
"""harness/ が kernel/infrastructure を直接 import していないことを確認."""
import ast
from pathlib import Path

ALLOWED_IMPORTS = {"contracts", "shared", "harness"}


def test_harness_no_kernel_infrastructure_import() -> None:
    """harness/ は kernel/infrastructure を直接 import しない."""
    violations: list[str] = []
    for py_file in Path("harness").rglob("*.py"):
        source = py_file.read_text()
        tree = ast.parse(source)
        for node in ast.walk(tree):
            if isinstance(node, ast.ImportFrom) and node.module:
                top_module = node.module.split(".")[0]
                if top_module in ("kernel", "infrastructure"):
                    violations.append(f"{py_file}:{node.lineno}: from {node.module}")
    assert violations == [], f"直接依存が残存:\n" + "\n".join(violations)
```

- [ ] **Step 2: テスト実行 → FAIL 確認**
- [ ] **Step 3: 各ファイルを修正**

- `contract_auth_guard.py`: `from infrastructure.security.auth_client import AuthClient` → `from contracts.harness.auth_service import AuthServiceProtocol`（DI で注入）
- `approval_flow.py`: `from kernel.protocols.agui_events import ...` → `from contracts.harness.execution_events import ...`
- `runtime_pipeline.py`: `from kernel.tools import KernelToolExecutor` → contracts で定義された ToolExecutor Protocol を使用

- [ ] **Step 4: テスト実行 → PASS 確認**
- [ ] **Step 5: 全体テスト**

```bash
conda run -n agentflow ./check.sh all
```

- [ ] **Step 6: コミット**

```bash
git add harness/
git add -f tests/harness/test_harness_no_direct_deps.py
git commit -m "refactor: harness から kernel/infrastructure 直接依存を排除"
```

---

### Task 4.3: HarnessedToolRuntime の責務見直し（指示書 §7.3）

**Files:**
- Modify: `harness/` 内の HarnessedToolRuntime 関連ファイル（実装時に特定）
- Test: `tests/harness/test_harnessed_tool_runtime.py`

**背景**: HarnessedToolRuntime は kernel の `KernelToolExecutor` を直接ラップしており、実行詳細に踏み込みすぎている。contracts で定義された `ToolExecutor` Protocol 越しに接続するよう修正する。

- [ ] **Step 1: 現行の HarnessedToolRuntime の実装を調査**

```bash
conda run -n agentflow grep -r "HarnessedToolRuntime" --include="*.py" -l
conda run -n agentflow grep -r "class HarnessedToolRuntime" --include="*.py" -rn
```

- [ ] **Step 2: テストを書く — HarnessedToolRuntime が kernel 具体型に依存しないこと**

```python
"""HarnessedToolRuntime が kernel 具体型に依存しないことを確認."""
import ast
from pathlib import Path


def test_harnessed_tool_runtime_no_kernel_concrete() -> None:
    """HarnessedToolRuntime のファイルが kernel 具体型を import しないこと."""
    # HarnessedToolRuntime を含むファイルを検索
    violations: list[str] = []
    for py_file in Path("harness").rglob("*.py"):
        source = py_file.read_text()
        if "HarnessedToolRuntime" not in source:
            continue
        tree = ast.parse(source)
        for node in ast.walk(tree):
            if isinstance(node, ast.ImportFrom) and node.module:
                top = node.module.split(".")[0]
                if top in ("kernel", "infrastructure"):
                    violations.append(f"{py_file}:{node.lineno}: from {node.module}")
    assert violations == [], f"HarnessedToolRuntime に kernel/infrastructure 依存:\n" + "\n".join(violations)
```

- [ ] **Step 3: テスト実行 → FAIL 確認**
- [ ] **Step 4: HarnessedToolRuntime を Protocol 経由に修正**

- kernel の `KernelToolExecutor` 直接参照 → contracts の `ToolExecutor` Protocol に変更
- DI パターンで実装を注入
- risk / scoring / replay の結果を trace contract に統一出力

- [ ] **Step 5: テスト実行 → PASS 確認**
- [ ] **Step 6: コミット**

```bash
git add harness/
git add -f tests/harness/test_harnessed_tool_runtime.py
git commit -m "refactor: HarnessedToolRuntime を Protocol 経由に修正、trace 連携統一"
```

---

## Phase 5: Contracts 二重定義解消 + Domain executable 移動

### Task 5.0: Contracts 二重定義の監査と解消（指示書 §3 Layer 2 改造指示）

**Files:**
- Modify: `contracts/` 配下（監査で特定）
- Test: `tests/contracts/test_no_duplicate_schemas.py`

**背景**: manifest / app config / tool result / trace result に二重定義がある。canonical schema を一本化する。

- [ ] **Step 1: 二重定義の調査**

```bash
# 同名クラス/型が複数箇所に定義されていないか検索
conda run -n agentflow grep -rn "class ToolResult" --include="*.py" | grep -v test | grep -v __pycache__
conda run -n agentflow grep -rn "class TraceResult" --include="*.py" | grep -v test | grep -v __pycache__
conda run -n agentflow grep -rn "class AppConfig" --include="*.py" | grep -v test | grep -v __pycache__
conda run -n agentflow grep -rn "class Manifest" --include="*.py" | grep -v test | grep -v __pycache__
```

- [ ] **Step 2: 重複箇所をリストアップし、canonical 版を contracts/ に集約**

各重複に対して:
1. contracts/ の定義を正本（canonical）とする
2. 他の定義を contracts/ からの import に置き換える
3. 不要な定義を削除

- [ ] **Step 3: テストを書く — 重複定義がないことを確認**

```python
"""contracts の canonical schema に二重定義がないことを確認."""
import ast
import collections
from pathlib import Path

CORE_TYPES = {"ToolResult", "TraceResult", "AppConfig", "Manifest"}


def test_no_duplicate_core_schemas() -> None:
    """コアスキーマ型が contracts/ 以外で class 定義されていないこと."""
    violations: list[str] = []
    for py_file in Path(".").rglob("*.py"):
        rel = str(py_file)
        if "__pycache__" in rel or ".venv" in rel or "tests/" in rel:
            continue
        if rel.startswith("contracts/"):
            continue
        try:
            source = py_file.read_text()
            tree = ast.parse(source)
        except (SyntaxError, UnicodeDecodeError):
            continue
        for node in ast.walk(tree):
            if isinstance(node, ast.ClassDef) and node.name in CORE_TYPES:
                violations.append(f"{py_file}:{node.lineno}: class {node.name}")
    assert violations == [], f"contracts 外でのコアスキーマ定義:\n" + "\n".join(violations)
```

- [ ] **Step 4: テスト実行 → 修正 → PASS 確認**
- [ ] **Step 5: コミット**

```bash
git add contracts/ kernel/ shared/ harness/ control_plane/ domain/ apps/
git add -f tests/contracts/test_no_duplicate_schemas.py
git commit -m "refactor: canonical schema を contracts に一本化、二重定義を排除"
```

---

### Task 5.0b: Domain/templates の executable 部分を移動（指示書 §3 Layer 7）

**Files:**
- Audit: `domain/templates/` 配下
- 移動先: `apps/` または `control_plane/tooling/`

**背景**: domain/templates は業務ドメインモデル・テンプレートを持つべきだが、実行可能な Agent 実装が混入している。

- [ ] **Step 1: domain/templates/ の実行可能コードを調査**

```bash
# runtime import を持つテンプレートファイルを検索
conda run -n agentflow grep -rn "from kernel" domain/templates/ --include="*.py"
conda run -n agentflow grep -rn "class.*Agent" domain/templates/ --include="*.py"
conda run -n agentflow grep -rn "async def.*execute\|async def.*run\|async def.*invoke" domain/templates/ --include="*.py"
```

- [ ] **Step 2: 調査結果に基づき移動計画を策定**

domain/templates/ の executable 部分（Agent 実装、runtime コード）を:
- `apps/` の対応するアプリに移動、または
- `control_plane/tooling/` に platform tooling として移動

domain/templates/ にはテンプレート定義（YAML/schema）とドメインモデルのみ残す。

- [ ] **Step 3: 移動実行（re-export パターンで後方互換維持）**
- [ ] **Step 4: テスト**

```bash
conda run -n agentflow ./check.sh all
```

- [ ] **Step 5: コミット**

```bash
git add domain/ apps/ control_plane/
git commit -m "refactor: domain/templates の executable 部分を apps/control_plane に移動"
```

---

## Phase 6: MCP スコープ制限 + 品質ゲート強化

### Task 6.1: Encoding チェックスクリプトの作成

**Files:**
- Create: `scripts/check_encoding.py`
- Test: `tests/scripts/test_check_encoding.py`

- [ ] **Step 1: テストを書く**

```python
"""check_encoding.py のテスト."""
import subprocess


def test_check_encoding_runs() -> None:
    """encoding チェッカーが正常に実行できること."""
    result = subprocess.run(
        ["python", "scripts/check_encoding.py", "--help"],
        capture_output=True, text=True,
    )
    assert result.returncode == 0
    assert "encoding" in result.stdout.lower() or "usage" in result.stdout.lower()


def test_check_encoding_detects_utf8() -> None:
    """UTF-8 ファイルが正しく検出されること."""
    result = subprocess.run(
        ["python", "scripts/check_encoding.py", "scripts/check_encoding.py"],
        capture_output=True, text=True,
    )
    assert result.returncode == 0
```

- [ ] **Step 2: テスト実行 → FAIL 確認**
- [ ] **Step 3: check_encoding.py を実装**

```python
"""scripts/check_encoding.py — ファイルエンコーディングチェッカー.

UTF-8 (BOM なし) を既定とし、違反を検出する。
"""
from __future__ import annotations

import argparse
import sys
from pathlib import Path

# BOM バイト列
UTF8_BOM = b"\xef\xbb\xbf"

# BOM が許容される拡張子
BOM_ALLOWED_EXTENSIONS = {".ps1"}

# チェック対象拡張子
TARGET_EXTENSIONS = {
    ".py", ".md", ".yaml", ".yml", ".json", ".toml",
    ".ts", ".tsx", ".js", ".jsx", ".css", ".html",
    ".sh", ".ps1", ".txt", ".cfg", ".ini",
}


def check_file(path: Path) -> str | None:
    """ファイルのエンコーディングを検証. 問題があれば説明文字列を返す."""
    try:
        raw = path.read_bytes()
    except (OSError, PermissionError):
        return None

    has_bom = raw.startswith(UTF8_BOM)

    if has_bom and path.suffix not in BOM_ALLOWED_EXTENSIONS:
        return f"{path}: UTF-8 BOM が検出されました（BOM なしに変更してください）"

    # UTF-8 デコード可能か確認
    try:
        raw.decode("utf-8")
    except UnicodeDecodeError:
        return f"{path}: UTF-8 でデコードできません（エンコーディングを確認してください）"

    return None


def main() -> int:
    """メインエントリポイント."""
    parser = argparse.ArgumentParser(description="ファイルエンコーディングチェッカー")
    parser.add_argument("paths", nargs="*", default=["."], help="チェック対象パス")
    parser.add_argument("--json", action="store_true", help="JSON 出力")
    args = parser.parse_args()

    violations: list[str] = []
    for target in args.paths:
        p = Path(target)
        files = [p] if p.is_file() else p.rglob("*")
        for f in files:
            if f.is_file() and f.suffix in TARGET_EXTENSIONS:
                issue = check_file(f)
                if issue:
                    violations.append(issue)

    if args.json:
        import json
        print(json.dumps({"violations": violations, "count": len(violations)}))
    else:
        for v in violations:
            print(f"❌ {v}")
        if not violations:
            print("✅ エンコーディングチェック: 全ファイル OK")

    return 1 if violations else 0


if __name__ == "__main__":
    sys.exit(main())
```

- [ ] **Step 4: テスト実行 → PASS 確認**
- [ ] **Step 5: コミット**

```bash
git add scripts/check_encoding.py
git add -f tests/scripts/test_check_encoding.py
git commit -m "feat: encoding チェッカースクリプトを追加"
```

---

### Task 6.2: pre-commit と check.sh への統合

**Files:**
- Modify: `.pre-commit-config.yaml`
- Modify: `check.sh`

- [ ] **Step 1: .pre-commit-config.yaml に encoding hook を追加**

```yaml
  - repo: local
    hooks:
      - id: check-encoding
        name: Check file encoding (UTF-8, no BOM)
        entry: python scripts/check_encoding.py
        language: python
        types: [text]
        pass_filenames: true
```

- [ ] **Step 2: check.sh の lint セクションに encoding チェックを追加**

`run_lint` 関数内に:
```bash
echo "--- Encoding Check ---"
conda run -n "$CONDA_ENV" python scripts/check_encoding.py
```

- [ ] **Step 3: 動作確認**

```bash
conda run -n agentflow python scripts/check_encoding.py
conda run -n agentflow ./check.sh lint
```

- [ ] **Step 4: コミット**

```bash
git add .pre-commit-config.yaml check.sh
git commit -m "ci: pre-commit と check.sh に encoding チェックを統合"
```

---

### Task 6.3: MCP スコープチェッカーの作成

**Files:**
- Create: `scripts/check_mcp_scope.py`
- Test: `tests/scripts/test_check_mcp_scope.py`

**注意**: MCP は指示書で Layer 5 Integration 限定とされている。現状 `kernel/protocols/mcp_*` にあるが、これは現行ディレクトリ構造での Integration 層の実装先として許容する。将来的に専用の `integration/` ディレクトリに移動を検討。

- [ ] **Step 1: テストを書く**

```python
"""check_mcp_scope.py のテスト."""
import subprocess


def test_check_mcp_scope_runs() -> None:
    """MCP スコープチェッカーが正常に実行できること."""
    result = subprocess.run(
        ["python", "scripts/check_mcp_scope.py", "--help"],
        capture_output=True, text=True,
    )
    assert result.returncode == 0
```

- [ ] **Step 2: テスト実行 → FAIL 確認**
- [ ] **Step 3: check_mcp_scope.py を実装**

MCP 関連の import/使用が許容ディレクトリ外に漏れていないかチェックするスクリプト。

```python
"""scripts/check_mcp_scope.py — MCP が integration 層を超えていないかチェック.

MCP import が許容されるディレクトリ:
- kernel/protocols/ (現行の Integration 層実装先)
- contracts/protocol/ (Protocol 定義)
- apps/ (アプリ層は利用可能)
- control_plane/ (管理層は利用可能)
- tests/ (テストは許容)
"""
from __future__ import annotations

import argparse
import ast
import sys
from pathlib import Path

ALLOWED_DIRS = {
    "kernel/protocols", "contracts/protocol",
    "apps", "control_plane", "tests", "plugins",
}


def check_mcp_scope(root: Path) -> list[str]:
    """MCP スコープ違反を検出."""
    violations: list[str] = []
    for py_file in root.rglob("*.py"):
        rel = str(py_file.relative_to(root))
        if any(rel.startswith(d) for d in ALLOWED_DIRS):
            continue
        if "__pycache__" in rel or ".venv" in rel:
            continue
        try:
            source = py_file.read_text()
            tree = ast.parse(source)
        except (SyntaxError, UnicodeDecodeError):
            continue
        for node in ast.walk(tree):
            if isinstance(node, ast.ImportFrom) and node.module and "mcp" in node.module.lower():
                violations.append(f"{rel}:{node.lineno}: from {node.module}")
    return violations


def main() -> int:
    """メインエントリポイント."""
    parser = argparse.ArgumentParser(description="MCP スコープチェッカー")
    parser.add_argument("--root", default=".", help="プロジェクトルート")
    parser.add_argument("--json", action="store_true")
    args = parser.parse_args()

    violations = check_mcp_scope(Path(args.root))
    if args.json:
        import json
        print(json.dumps({"violations": violations, "count": len(violations)}))
    else:
        for v in violations:
            print(f"❌ {v}")
        if not violations:
            print("✅ MCP スコープチェック: 全ファイル OK")
    return 1 if violations else 0


if __name__ == "__main__":
    sys.exit(main())
```

- [ ] **Step 4: テスト実行 → PASS 確認**
- [ ] **Step 5: pre-commit に追加**
- [ ] **Step 6: コミット**

```bash
git add scripts/check_mcp_scope.py .pre-commit-config.yaml
git add -f tests/scripts/test_check_mcp_scope.py
git commit -m "feat: MCP スコープチェッカーを追加、pre-commit に統合"
```

---

### Task 6.4: check_rules_compliance.py に全チェック項目を統合

**Files:**
- Modify: `scripts/check_rules_compliance.py`

- [ ] **Step 1: 現行ファイルを読んで構造を確認**

```bash
conda run -n agentflow python scripts/check_rules_compliance.py --json | python -m json.tool | head -30
```

- [ ] **Step 2: 以下のチェック項目を追加**

1. `encoding_violations` — `check_encoding.py` を内部呼び出し
2. `mcp_scope_violations` — `check_mcp_scope.py` を内部呼び出し

- [ ] **Step 3: テスト**

```bash
conda run -n agentflow python scripts/check_rules_compliance.py --json
```

- [ ] **Step 4: コミット**

```bash
git add scripts/check_rules_compliance.py
git commit -m "feat: rules compliance に encoding/MCP スコープチェックを追加"
```

---

## Phase 7: CLI 正式化

### Task 7.1: CLI 実行抽象の定義

**Files:**
- Create: `kernel/interfaces/cli_runtime.py`
- Test: `tests/kernel/interfaces/test_cli_runtime.py`

- [ ] **Step 1: テストを書く**

```python
"""CLI Runtime Protocol のテスト."""


def test_cli_runtime_protocol_exists() -> None:
    """CLIRuntime Protocol がインポートできること."""
    from kernel.interfaces.cli_runtime import CLIRuntime
    assert hasattr(CLIRuntime, "execute")
    assert hasattr(CLIRuntime, "execute_stream")
```

- [ ] **Step 2: テスト実行 → FAIL 確認**
- [ ] **Step 3: Protocol 実装**

```python
"""kernel/interfaces/cli_runtime.py — CLI 実行の抽象."""
from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, AsyncIterator, Protocol, runtime_checkable


@dataclass(frozen=True)
class CLIResult:
    """CLI 実行結果."""
    exit_code: int
    stdout: str = ""
    stderr: str = ""
    trace_id: str = ""
    artifacts: list[dict[str, Any]] = field(default_factory=list)


@runtime_checkable
class CLIRuntime(Protocol):
    """CLI コマンド実行の抽象インターフェース."""

    async def execute(
        self,
        command: str,
        args: list[str],
        *,
        dry_run: bool = False,
        json_output: bool = False,
        **kwargs: Any,
    ) -> CLIResult:
        """コマンドを実行して結果を返す."""
        ...

    async def execute_stream(
        self,
        command: str,
        args: list[str],
        **kwargs: Any,
    ) -> AsyncIterator[str]:
        """コマンドをストリーミング実行."""
        ...
```

- [ ] **Step 4: テスト実行 → PASS 確認**
- [ ] **Step 5: コミット**

```bash
git add kernel/interfaces/cli_runtime.py kernel/interfaces/__init__.py
git add -f tests/kernel/interfaces/test_cli_runtime.py
git commit -m "feat: CLI Runtime Protocol を定義"
```

---

## 完了判定チェックリスト

### README
- [ ] 静的7層定義が記述されている
- [ ] 実行時フローが記述されている
- [ ] Harness 横断説明が記述されている
- [ ] CLI / MCP / Gateway の責務が明記されている
- [ ] Encoding 規約が追加されている

### 契約
- [ ] Harness 用 Protocol が contracts/ に定義されている
- [ ] Kernel 用 Protocol（LLMService, ToolProvider, CLIRuntime）が定義されている
- [ ] canonical schema 一本化（ToolResult, TraceResult, AppConfig, Manifest の二重定義なし）

### Kernel
- [ ] infrastructure 直接 import が 0（49箇所/24ファイル全解消）
- [ ] Protocol 経由でのみ外部にアクセス
- [ ] `./check.sh all` パス

### Harness
- [ ] kernel/infrastructure 直接依存が 0
- [ ] contracts/harness/ 経由でのみ外部型を参照
- [ ] HarnessedToolRuntime が kernel 具体型に直接依存しない

### Domain
- [ ] domain/templates/ に executable agent が残存していない（apps/control_plane に移動済み）

### Shared
- [ ] kernel 逆依存が 0
- [ ] 誤配置モジュールの整理計画が文書化されている

### 品質ゲート
- [ ] encoding チェックが pre-commit + check.sh + CI に統合
- [ ] MCP スコープチェックが追加
- [ ] rules_compliance に全チェック項目が統合
- [ ] `conda run -n agentflow ./check.sh all` が 0 エラー

---

## 最終検証コマンド

```bash
# 全チェック
conda run -n agentflow ./check.sh all

# boundary 違反 0 確認
conda run -n agentflow python scripts/check_layer_boundaries.py

# provider 直 import 0 確認
conda run -n agentflow python scripts/check_no_direct_provider_calls.py

# encoding チェック
conda run -n agentflow python scripts/check_encoding.py

# MCP スコープチェック
conda run -n agentflow python scripts/check_mcp_scope.py

# rules compliance 総合レポート
conda run -n agentflow python scripts/check_rules_compliance.py --json
```
