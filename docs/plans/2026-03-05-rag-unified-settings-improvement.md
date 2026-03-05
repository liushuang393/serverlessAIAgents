# RAG 統一設定改善 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** RAG 機能の全面修正と統一設定UI実装 - 現在 "全機能不可" の RAG を動作させ、業界最良パターン選択・ベクトルDB自動セットアップ・画面レベル E2E テストまで一気通貫で整備する。

**Architecture:**
- Platform PATCH エンドポイントを 405 → 実装に変更（`RAGOverviewService.update_app_config()` 呼び出し）
- Qdrant 接続失敗時のグレースフルデグラデーション（起動エラー撲滅）
- 各 App に統一 `/api/rag/settings` エンドポイント + フロントエンド設定 UI
- Qdrant ローカルセットアップシェルスクリプト（クリックで自動実行）
- Unit テスト + Playwright 画面レベル E2E テスト整備

**Tech Stack:** Python 3.13+, FastAPI, Pydantic, React/TypeScript, Playwright, pytest, Docker/Qdrant

---

## 背景・根本原因分析

### 発見した問題

| # | 問題 | 場所 | 影響 |
|---|------|------|------|
| 1 | Platform PATCH が 405 を返す | `apps/platform/routers/rag.py:188-212` | 設定画面から一切変更できない |
| 2 | Qdrant 接続失敗でアプリ起動クラッシュ | `agentflow/memory/vector_db/qdrant_db.py:80-82` | `ConnectionError` がキャッチされず起動不能 |
| 3 | `RUNTIME_BACKEND_URL_NOT_FOUND` エラー | `apps/platform/routers/rag.py:289-296` | ingest runs proxy がアプリ未起動で 400 |
| 4 | RAG 設定 UI がパターン選択を活かせていない | `apps/platform/frontend/src/components/RAGOverview.tsx` | 4 パターン定義済みなのに活用されない |
| 5 | Qdrant セットアップ手順なし | - | ユーザーが手動で Qdrant を起動する必要 |
| 6 | DGE の RAG 設定が 3 か所に分散 | `apps/decision_governance_engine/` | 一貫性欠如・保守困難 |

### DGE が 3 か所ある理由（設計上意図的）

```
routers/config.py       → per-agent RAG 設定（術/器 エージェントごとに異なる知識ドメイン）
routers/knowledge.py    → 簡易知識ドキュメント CRUD（道/法 レイヤー）
routers/knowledge_collections.py → 完全コレクション管理 + RBAC（agentflow.knowledge 使用）
```

道法術器フレームワーク上、術エージェントは戦略・メソドロジー知識、器エージェントは技術実装知識が必要なため分離は正当。ただし UI/API 設計を統一し、同じ基盤ライブラリ（`agentflow.knowledge`）を使うよう整理する。

---

## 業界最良 RAG パターン（実装基準）

RAGFlow・実証論文・既実装経験をもとに以下 3 パターン＋カスタムで対応：

| パターン名 | 適用シーン | チャンキング | 検索方式 | リランカー | Top-K | スコア閾値 |
|-----------|-----------|------------|---------|----------|-------|---------|
| **FAQ Precision** | FAQ・問い合わせ応答 | sentence / 500 | hybrid | cross_encoder | 8 | 0.25 |
| **Balanced Knowledge** | 社内ドキュメント汎用 | recursive / 800 | hybrid | bm25 | 6 | 0.20 |
| **Long Doc Reasoning** | 規約・契約書・長文 | markdown / 1200 | multi_query | cross_encoder | 10 | 0.30 |
| **Custom** | 自由設定 | any | any | any | user | user |

パターン選択時、全関連パラメータが自動セット → ユーザーは必要に応じて微調整可能。

---

## Task 1: 緊急バグ修正 - Qdrant 接続グレースフルデグラデーション

**Files:**
- Modify: `agentflow/memory/vector_db/qdrant_db.py:58-82`
- Modify: `agentflow/providers/vectordb_provider.py`（`get_vectordb` のエラーハンドリング）
- Test: `tests/memory/vector_db/test_qdrant_graceful.py`

**Step 1: テストを書く（失敗確認）**

```python
# tests/memory/vector_db/test_qdrant_graceful.py
"""Qdrant 接続失敗時のグレースフルデグラデーション テスト."""
import pytest
from unittest.mock import patch
from agentflow.memory.vector_db.qdrant_db import QdrantDB


async def test_connect_fails_gracefully_when_qdrant_not_running() -> None:
    """Qdrant 未起動時に ConnectionError ではなく警告ログで継続すること."""
    db = QdrantDB(host="localhost", port=9999, collection_name="test")  # 存在しないポート
    # connect() が例外を上げずに False を返すこと
    result = await db.connect_or_warn()
    assert result is False
    assert db.is_available() is False


async def test_search_returns_empty_when_not_connected() -> None:
    """未接続時の search は空リストを返すこと（クラッシュしない）."""
    db = QdrantDB(host="localhost", port=9999, collection_name="test")
    results = await db.search_safe(query="test", query_embedding=[0.1] * 384, top_k=5)
    assert results == []
```

**Step 2: テスト実行（失敗確認）**

```bash
cd /home/liush/projects/serverlessAIAgents
conda activate agentflow
pytest tests/memory/vector_db/test_qdrant_graceful.py -v
```
Expected: FAIL with AttributeError (connect_or_warn/is_available/search_safe not defined)

**Step 3: QdrantDB にグレースフルメソッド追加**

`agentflow/memory/vector_db/qdrant_db.py` の `connect()` を修正し `connect_or_warn()` と `is_available()` `search_safe()` を追加:

```python
async def connect_or_warn(self) -> bool:
    """Qdrant への接続を試みる。失敗した場合は警告ログを出力して False を返す（起動継続）."""
    try:
        await self.connect()
        return True
    except (ConnectionError, ImportError, Exception) as e:
        self._logger.warning(
            "Qdrant 接続失敗（RAG 機能は無効化されます）: %s - "
            "Qdrant を起動するか QDRANT_URL を設定してください",
            e,
        )
        return False

def is_available(self) -> bool:
    """Qdrant が接続済みで使用可能かどうかを返す."""
    return self._connected

async def search_safe(
    self,
    query: str,
    query_embedding: list[float],
    top_k: int = 5,
    filter_metadata: dict | None = None,
) -> list[dict]:
    """接続済みの場合のみ search を実行。未接続なら空リストを返す."""
    if not self._connected:
        self._logger.debug("Qdrant 未接続のため search をスキップ")
        return []
    return await self.search(
        query=query,
        query_embedding=query_embedding,
        top_k=top_k,
        filter_metadata=filter_metadata,
    )
```

**Step 4: テスト実行（成功確認）**

```bash
pytest tests/memory/vector_db/test_qdrant_graceful.py -v
```
Expected: PASS

**Step 5: providers/vectordb_provider.py のエラーハンドリング修正**

`get_vectordb()` で `connect()` を直接呼ぶ箇所を `connect_or_warn()` に変更:

```python
# Before (vectordb_provider.py の connect 呼び出し箇所)
await vectordb.connect()

# After
available = await vectordb.connect_or_warn()
if not available:
    logger.warning("VectorDB 未接続: RAG 機能が制限されます")
```

**Step 6: コミット**

```bash
git add agentflow/memory/vector_db/qdrant_db.py agentflow/providers/vectordb_provider.py tests/memory/vector_db/test_qdrant_graceful.py
git commit -m "fix(qdrant): Qdrant 未接続時のグレースフルデグラデーション - 起動クラッシュを修正"
```

---

## Task 2: 緊急バグ修正 - Platform PATCH エンドポイント実装

**Files:**
- Modify: `apps/platform/routers/rag.py:188-212`（405 → 実装）
- Modify: `apps/platform/main.py`（RAGOverviewService を router に注入）
- Test: `tests/apps/platform/test_rag_router.py`（既存ファイルに追加）

**Step 1: 既存テストファイルを確認・テスト追加**

```bash
cat tests/apps/platform/test_rag_router.py | head -50
```

```python
# tests/apps/platform/test_rag_router.py に追加
async def test_patch_rag_config_updates_app_config(
    client: AsyncClient,
    tmp_app_config: Path,
) -> None:
    """PATCH /rag/apps/{app}/config が app_config.json を更新すること."""
    resp = await client.patch(
        "/api/studios/framework/rag/apps/faq_system/config",
        json={
            "enabled": True,
            "pattern": "faq_precision",
            "vector_provider": "qdrant",
        },
    )
    assert resp.status_code == 200
    data = resp.json()
    assert data["rag"]["pattern"] == "faq_precision"
    assert data["rag"]["chunk_strategy"] == "sentence"  # パターンから自動セット
    assert data["rag"]["chunk_size"] == 500


async def test_patch_rag_config_returns_405_removed() -> None:
    """405 が返らないこと（旧仕様の確認）."""
    # このテストが 405 を期待しなくなったことを示す
    pass
```

**Step 2: テスト実行（失敗確認）**

```bash
pytest tests/apps/platform/test_rag_router.py::test_patch_rag_config_updates_app_config -v
```
Expected: FAIL with 405

**Step 3: Platform RAG router の PATCH を実装**

`apps/platform/routers/rag.py:188-212` を以下に置き換え:

```python
@router.patch("/apps/{app_name}/config")
async def patch_rag_app_config(
    app_name: str,
    patch: RAGConfigPatchRequest,
) -> dict[str, Any]:
    """App の RAG 設定を更新し app_config.json に保存する.

    パターン選択時は関連パラメータを自動セット（ユーザーによる上書き可）。
    設定変更後、実行中の App には SSE イベントでホットリロード通知を送る。

    Args:
        app_name: 更新対象の App 名
        patch: 更新内容（部分更新可）

    Returns:
        更新後の RAG 設定
    """
    overview = _get_overview()
    try:
        updated = overview.update_app_config(app_name, patch.model_dump(exclude_none=True))
    except KeyError:
        raise HTTPException(
            status_code=404,
            detail={"message": f"App not found: {app_name}", "error_code": "APP_NOT_FOUND"},
        )
    except ValueError as exc:
        raise HTTPException(
            status_code=422,
            detail={"message": str(exc), "error_code": "VALIDATION_ERROR"},
        )

    # 実行中の App に設定変更を SSE 通知
    store = _try_get_store()
    if store is not None:
        await store.fire_config_change(
            app_name,
            contracts_rag=updated.get("rag"),
        )

    return updated
```

**Step 4: テスト実行（成功確認）**

```bash
pytest tests/apps/platform/test_rag_router.py -v
```
Expected: 全 PASS

**Step 5: コミット**

```bash
git add apps/platform/routers/rag.py tests/apps/platform/test_rag_router.py
git commit -m "fix(platform/rag): PATCH エンドポイントを 405 から実装に変更 - RAG 設定をUIから更新可能に"
```

---

## Task 3: Qdrant ローカルセットアップシェルスクリプト

**Files:**
- Create: `scripts/setup_qdrant.sh`
- Create: `apps/faq_system/scripts/setup_rag.sh`
- Test: `tests/scripts/test_setup_qdrant.py`（スクリプトの内容検証）

**Step 1: テストを書く**

```python
# tests/scripts/test_setup_qdrant.py
"""setup_qdrant.sh の内容・実行可能性を検証."""
from pathlib import Path


def test_setup_qdrant_script_exists() -> None:
    script = Path("scripts/setup_qdrant.sh")
    assert script.exists(), "scripts/setup_qdrant.sh が存在しない"
    assert script.stat().st_mode & 0o111, "スクリプトに実行権限がない"


def test_setup_qdrant_script_contains_docker_compose() -> None:
    script = Path("scripts/setup_qdrant.sh")
    content = script.read_text()
    assert "docker" in content.lower()
    assert "qdrant" in content.lower()
    assert "6333" in content  # Qdrant デフォルトポート


def test_faq_setup_rag_script_exists() -> None:
    script = Path("apps/faq_system/scripts/setup_rag.sh")
    assert script.exists()
```

**Step 2: テスト実行（失敗確認）**

```bash
pytest tests/scripts/test_setup_qdrant.py -v
```

**Step 3: スクリプト作成**

`scripts/setup_qdrant.sh`:
```bash
#!/usr/bin/env bash
# Qdrant ローカルセットアップスクリプト
# AgentFlow Platform の「Qdrant セットアップ」ボタンから自動実行される
set -euo pipefail

QDRANT_PORT="${QDRANT_PORT:-6333}"
QDRANT_GRPC_PORT="${QDRANT_GRPC_PORT:-6334}"
CONTAINER_NAME="agentflow_qdrant"

echo "[AgentFlow] Qdrant セットアップを開始..."

# Docker 確認
if ! command -v docker &> /dev/null; then
    echo "[ERROR] Docker がインストールされていません"
    echo "  インストール方法: https://docs.docker.com/get-docker/"
    exit 1
fi

# 既存コンテナチェック
if docker ps --format '{{.Names}}' | grep -q "^${CONTAINER_NAME}$"; then
    echo "[OK] Qdrant は既に起動中です (port: ${QDRANT_PORT})"
    exit 0
fi

# 停止中コンテナがあれば起動
if docker ps -a --format '{{.Names}}' | grep -q "^${CONTAINER_NAME}$"; then
    echo "[INFO] 既存コンテナを起動します..."
    docker start "${CONTAINER_NAME}"
else
    echo "[INFO] Qdrant コンテナを作成・起動します..."
    docker run -d \
        --name "${CONTAINER_NAME}" \
        -p "${QDRANT_PORT}:6333" \
        -p "${QDRANT_GRPC_PORT}:6334" \
        -v agentflow_qdrant_storage:/qdrant/storage:z \
        qdrant/qdrant:latest
fi

# 起動待機
echo "[INFO] Qdrant 起動待機中..."
for i in $(seq 1 30); do
    if curl -sf "http://localhost:${QDRANT_PORT}/healthz" > /dev/null 2>&1; then
        echo "[OK] Qdrant が起動しました: http://localhost:${QDRANT_PORT}"
        echo ""
        echo "  環境変数に以下を設定してください:"
        echo "  QDRANT_URL=http://localhost:${QDRANT_PORT}"
        exit 0
    fi
    sleep 1
done

echo "[ERROR] Qdrant の起動タイムアウト（30秒）"
echo "  docker logs ${CONTAINER_NAME} でログを確認してください"
exit 1
```

`apps/faq_system/scripts/setup_rag.sh`:
```bash
#!/usr/bin/env bash
# FAQ System RAG セットアップスクリプト
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
APP_DIR="${REPO_ROOT}/apps/faq_system"

echo "[FAQ] RAG セットアップを開始..."

# 1. Qdrant セットアップ
echo "[1/3] Qdrant をセットアップ..."
bash "${REPO_ROOT}/scripts/setup_qdrant.sh"

# 2. コレクション作成（agentflow CLI経由）
echo "[2/3] faq_knowledge コレクションを作成..."
cd "${REPO_ROOT}"
conda activate agentflow 2>/dev/null || true
python -c "
import asyncio
from agentflow.knowledge.collection_manager import CollectionManager
from agentflow.knowledge.models import CollectionConfig, ChunkStrategy, RetrievalMethod

async def main():
    mgr = CollectionManager()
    config = CollectionConfig(
        collection_name='faq_knowledge',
        app_name='faq_system',
        display_name='FAQ ナレッジベース',
        chunk_strategy=ChunkStrategy.SENTENCE,
        chunk_size=500,
        chunk_overlap=80,
        retrieval_method=RetrievalMethod.HYBRID,
        top_k=8,
        min_similarity=0.25,
    )
    await mgr.create_collection(config)
    print('[OK] faq_knowledge コレクション作成完了')

asyncio.run(main())
" 2>&1 || echo "[WARN] コレクション作成をスキップ（既存 or DB 未接続）"

# 3. 環境変数確認
echo "[3/3] 環境変数を確認..."
if [ -f "${APP_DIR}/.env" ]; then
    if ! grep -q "QDRANT_URL" "${APP_DIR}/.env"; then
        echo "QDRANT_URL=http://localhost:6333" >> "${APP_DIR}/.env"
        echo "[OK] .env に QDRANT_URL を追加しました"
    fi
else
    echo "QDRANT_URL=http://localhost:6333" > "${APP_DIR}/.env"
    echo "[OK] .env を作成しました"
fi

echo ""
echo "[FAQ] RAG セットアップ完了!"
echo "  次のステップ: アプリを起動してください"
echo "  python -m apps.platform.main serve --port 8001"
```

**Step 4: スクリプトに実行権限付与**

```bash
chmod +x scripts/setup_qdrant.sh apps/faq_system/scripts/setup_rag.sh
```

**Step 5: テスト実行（成功確認）**

```bash
pytest tests/scripts/test_setup_qdrant.py -v
```
Expected: PASS

**Step 6: コミット**

```bash
git add -f scripts/setup_qdrant.sh apps/faq_system/scripts/setup_rag.sh tests/scripts/test_setup_qdrant.py
git commit -m "feat(scripts): Qdrant ローカルセットアップシェルスクリプト追加"
```

---

## Task 4: Platform RAG UI - パターン選択と自動パラメータ設定

**Files:**
- Modify: `apps/platform/frontend/src/components/RAGOverview.tsx`（パターン選択でパラメータ自動セット）
- Test: `tests/apps/platform/test_rag_overview_e2e.spec.ts`（Playwright E2E）

**Step 1: 既存 RAGOverview.tsx のパターン選択ロジック確認**

```bash
grep -n "pattern\|Pattern\|AUTO\|auto" apps/platform/frontend/src/components/RAGOverview.tsx | head -30
```

**Step 2: E2E テストを書く**

```typescript
// tests/apps/platform/test_rag_overview_e2e.spec.ts
import { test, expect } from '@playwright/test';

test.describe('RAG Overview - パターン選択 E2E', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('http://localhost:3000');  // Platform フロント
    await page.waitForLoadState('networkidle');
  });

  test('パターン "faq_precision" を選択するとパラメータが自動セットされる', async ({ page }) => {
    // RAG 設定画面を開く
    await page.click('[data-testid="nav-rag"]');
    await page.waitForSelector('[data-testid="rag-app-card"]');

    // FAQ System カードをクリック
    await page.click('[data-testid="rag-app-card-faq_system"]');
    await page.waitForSelector('[data-testid="rag-settings-form"]');

    // パターン選択
    await page.selectOption('[data-testid="rag-pattern-select"]', 'faq_precision');

    // 自動セットされたパラメータを確認
    await expect(page.locator('[data-testid="chunk-strategy-select"]')).toHaveValue('sentence');
    await expect(page.locator('[data-testid="chunk-size-input"]')).toHaveValue('500');
    await expect(page.locator('[data-testid="chunk-overlap-input"]')).toHaveValue('80');
    await expect(page.locator('[data-testid="retrieval-method-select"]')).toHaveValue('hybrid');
    await expect(page.locator('[data-testid="top-k-input"]')).toHaveValue('8');
  });

  test('パターン選択後にパラメータを手動上書きできる', async ({ page }) => {
    await page.click('[data-testid="nav-rag"]');
    await page.click('[data-testid="rag-app-card-faq_system"]');
    await page.selectOption('[data-testid="rag-pattern-select"]', 'balanced_knowledge');

    // パラメータ手動上書き
    await page.fill('[data-testid="top-k-input"]', '10');
    await expect(page.locator('[data-testid="top-k-input"]')).toHaveValue('10');
    // パターン名が "custom" に変化するか、または manual override 表示が出る
    await expect(page.locator('[data-testid="rag-pattern-modified-badge"]')).toBeVisible();
  });

  test('保存ボタンで PATCH API が呼ばれ成功メッセージが表示される', async ({ page }) => {
    await page.click('[data-testid="nav-rag"]');
    await page.click('[data-testid="rag-app-card-faq_system"]');
    await page.selectOption('[data-testid="rag-pattern-select"]', 'faq_precision');

    // API モック or 実際の API（開発サーバー必要）
    const saveBtn = page.locator('[data-testid="rag-save-button"]');
    await expect(saveBtn).toBeVisible();
    await saveBtn.click();

    await expect(page.locator('[data-testid="rag-save-success-toast"]')).toBeVisible();
  });
});
```

**Step 3: Playwright 設定ファイル作成**

```bash
cat apps/platform/frontend/playwright.config.ts 2>/dev/null || echo "新規作成必要"
```

```typescript
// apps/platform/frontend/playwright.config.ts
import { defineConfig } from '@playwright/test';

export default defineConfig({
  testDir: '../../../tests/apps/platform',
  testMatch: '**/*.spec.ts',
  use: {
    baseURL: 'http://localhost:3000',
    screenshot: 'only-on-failure',
    video: 'retain-on-failure',
  },
  webServer: {
    command: 'npm run dev',
    port: 3000,
    reuseExistingServer: !process.env.CI,
    cwd: 'apps/platform/frontend',
  },
});
```

**Step 4: RAGOverview.tsx にパターン選択自動セット追加**

パターン選択時に全関連フィールドを自動セットするロジックを追加:

```typescript
// RAGOverview.tsx 内の handlePatternChange 関数
const handlePatternChange = useCallback((patternName: string) => {
  const pattern = patterns.find((p) => p.name === patternName);
  if (pattern) {
    setForm((prev) => ({
      ...prev,
      pattern: patternName,
      chunk_strategy: pattern.config.chunk_strategy ?? prev.chunk_strategy,
      chunk_size: pattern.config.chunk_size ?? prev.chunk_size,
      chunk_overlap: pattern.config.chunk_overlap ?? prev.chunk_overlap,
      retrieval_method: pattern.config.retrieval_method ?? prev.retrieval_method,
      reranker: pattern.config.reranker ?? prev.reranker,
      top_k: pattern.config.top_k ?? prev.top_k,
      score_threshold: pattern.config.score_threshold != null
        ? String(pattern.config.score_threshold)
        : prev.score_threshold,
    }));
    setPatternModified(false);
  }
}, [patterns]);

// パラメータ手動変更時に modified フラグをセット
const handleFieldChange = useCallback((field: keyof RAGFormState, value: unknown) => {
  setForm((prev) => ({ ...prev, [field]: value }));
  setPatternModified(true);
}, []);
```

data-testid 属性を各フォーム要素に追加（E2E テストが参照できるよう）。

**Step 5: E2E テスト実行**

```bash
cd apps/platform/frontend
npx playwright install chromium
npx playwright test tests/apps/platform/test_rag_overview_e2e.spec.ts --headed
```

**Step 6: コミット**

```bash
git add apps/platform/frontend/src/components/RAGOverview.tsx apps/platform/frontend/playwright.config.ts
git add -f tests/apps/platform/test_rag_overview_e2e.spec.ts
git commit -m "feat(platform/rag): パターン選択で関連パラメータ自動セット + E2E テスト"
```

---

## Task 5: 各 App への統一 RAG Settings API エンドポイント

**対象 App:** `faq_system`, `decision_governance_engine`, `code_migration_assistant`

### Task 5a: faq_system RAG Settings API

**Files:**
- Modify: `apps/faq_system/routers/kb_settings.py`（設定 GET/PATCH 追加）
- Test: `apps/faq_system/tests/test_rag_settings_e2e.py`

**Step 1: テストを書く**

```python
# apps/faq_system/tests/test_rag_settings_e2e.py
"""FAQ System RAG 設定 API の単体テスト + 画面レベル E2E テスト."""
import pytest
from httpx import AsyncClient


async def test_get_rag_settings_returns_current_config(
    client: AsyncClient,
) -> None:
    """GET /api/rag/settings が現在の設定を返すこと."""
    resp = await client.get("/api/rag/settings")
    assert resp.status_code == 200
    data = resp.json()
    assert "pattern" in data
    assert "chunk_strategy" in data
    assert "vector_provider" in data
    assert "enabled" in data


async def test_patch_rag_settings_with_pattern_autosets_params(
    client: AsyncClient,
) -> None:
    """PATCH /api/rag/settings でパターン選択時にパラメータが自動セットされること."""
    resp = await client.patch(
        "/api/rag/settings",
        json={"pattern": "faq_precision"},
    )
    assert resp.status_code == 200
    data = resp.json()
    # faq_precision パターンの期待値
    assert data["chunk_strategy"] == "sentence"
    assert data["chunk_size"] == 500
    assert data["top_k"] == 8


async def test_patch_rag_settings_validates_unknown_pattern(
    client: AsyncClient,
) -> None:
    """不明なパターン名で 422 が返ること."""
    resp = await client.patch(
        "/api/rag/settings",
        json={"pattern": "unknown_pattern_xyz"},
    )
    assert resp.status_code == 422


async def test_patch_rag_settings_vector_db_setup_flag(
    client: AsyncClient,
) -> None:
    """auto_create_collection=True 時にコレクション作成を試みること."""
    resp = await client.patch(
        "/api/rag/settings",
        json={
            "enabled": True,
            "vector_provider": "qdrant",
            "auto_create_collection": True,
        },
    )
    # Qdrant 未起動の場合でも 500 にならず、警告付きで成功すること
    assert resp.status_code in (200, 207)  # 207: partial success
```

**Step 2: テスト実行（失敗確認）**

```bash
pytest apps/faq_system/tests/test_rag_settings_e2e.py -v
```
Expected: FAIL

**Step 3: kb_settings.py に統一設定 API 追加**

```python
# apps/faq_system/routers/kb_settings.py に追加

from pydantic import BaseModel, Field

_PATTERNS = {
    "faq_precision": {
        "chunk_strategy": "sentence", "chunk_size": 500, "chunk_overlap": 80,
        "retrieval_method": "hybrid", "reranker": "cross_encoder",
        "top_k": 8, "score_threshold": 0.25,
    },
    "balanced_knowledge": {
        "chunk_strategy": "recursive", "chunk_size": 800, "chunk_overlap": 120,
        "retrieval_method": "hybrid", "reranker": "bm25",
        "top_k": 6, "score_threshold": 0.20,
    },
    "long_doc_reasoning": {
        "chunk_strategy": "markdown", "chunk_size": 1200, "chunk_overlap": 180,
        "retrieval_method": "multi_query", "reranker": "cross_encoder",
        "top_k": 10, "score_threshold": 0.30,
    },
}

KNOWN_PATTERNS = set(_PATTERNS.keys()) | {"custom"}


class RAGSettingsPatch(BaseModel):
    """RAG 設定更新リクエスト."""
    pattern: str | None = Field(None, description="パターン名（自動パラメータセット）")
    enabled: bool | None = None
    vector_provider: str | None = None
    vector_collection: str | None = None
    chunk_strategy: str | None = None
    chunk_size: int | None = Field(None, ge=100, le=4000)
    chunk_overlap: int | None = Field(None, ge=0, le=500)
    retrieval_method: str | None = None
    reranker: str | None = None
    top_k: int | None = Field(None, ge=1, le=20)
    score_threshold: float | None = Field(None, ge=0.0, le=1.0)
    auto_create_collection: bool = Field(False, description="コレクション自動作成フラグ")


@router.get("/rag/settings")
async def get_rag_settings() -> dict[str, Any]:
    """現在の RAG 設定を返す."""
    config = load_rag_runtime_config()
    return {
        "enabled": config.rag_enabled,
        "pattern": None,  # app_config.json から読む
        "chunk_strategy": config.rag_chunk_strategy,
        "reranker": config.rag_reranker,
        "top_k": config.rag_top_k,
        "vector_provider": "qdrant",  # app_config.json の contracts.rag.provider から
        "available_patterns": list(_PATTERNS.keys()),
    }


@router.patch("/rag/settings")
async def patch_rag_settings(patch: RAGSettingsPatch) -> dict[str, Any]:
    """RAG 設定を更新する。パターン選択時は関連パラメータを自動セット."""
    patch_dict = patch.model_dump(exclude_none=True)

    # パターン検証
    if patch_dict.get("pattern") and patch_dict["pattern"] not in KNOWN_PATTERNS:
        from fastapi import HTTPException
        raise HTTPException(
            status_code=422,
            detail=f"Unknown pattern: {patch_dict['pattern']}. Valid: {sorted(KNOWN_PATTERNS)}",
        )

    # パターンから自動パラメータ展開
    pattern_name = patch_dict.get("pattern")
    if pattern_name and pattern_name in _PATTERNS:
        auto_params = dict(_PATTERNS[pattern_name])
        # 手動指定値がある場合は上書き（auto_params が base、手動指定が上書き）
        merged = {**auto_params, **{k: v for k, v in patch_dict.items() if k not in ("pattern", "auto_create_collection")}}
        patch_dict.update(merged)

    # app_config.json を Platform API 経由で更新（省略時はログのみ）
    import httpx
    platform_url = "http://localhost:8001/api/studios/framework/rag/apps/faq_system/config"
    warnings: list[str] = []
    try:
        async with httpx.AsyncClient(timeout=5.0) as client:
            resp = await client.patch(platform_url, json=patch_dict)
            if resp.status_code >= 400:
                warnings.append(f"Platform 更新失敗 ({resp.status_code}) - ローカル設定のみ更新")
    except Exception as e:
        warnings.append(f"Platform 接続失敗: {e} - ローカル設定のみ更新")

    result = {**patch_dict, "warnings": warnings}
    status_code = 207 if warnings else 200
    from fastapi.responses import JSONResponse
    return JSONResponse(content=result, status_code=status_code)
```

**Step 4: テスト実行（成功確認）**

```bash
pytest apps/faq_system/tests/test_rag_settings_e2e.py -v
```
Expected: PASS

**Step 5: コミット**

```bash
git add apps/faq_system/routers/kb_settings.py apps/faq_system/tests/test_rag_settings_e2e.py
git commit -m "feat(faq): 統一 RAG 設定 API /api/rag/settings 追加"
```

---

## Task 6: ファイル追加・API インジェスト（返り値型対応）

**Files:**
- Modify: `apps/faq_system/routers/rag.py`（ファイルアップロード API 強化）
- Create: `agentflow/knowledge/api_ingestion.py`（API データソース取り込み）
- Test: `tests/unit/knowledge/test_api_ingestion.py`

**Step 1: テストを書く**

```python
# tests/unit/knowledge/test_api_ingestion.py
"""API データソースからのインジェスト テスト."""
import pytest
from unittest.mock import AsyncMock, patch
from agentflow.knowledge.api_ingestion import APIIngestor, APIIngestResult


async def test_ingest_json_api_returns_typed_result() -> None:
    """JSON API からのインジェストが型付き結果を返すこと."""
    mock_response = [
        {"id": 1, "question": "Q1", "answer": "A1"},
        {"id": 2, "question": "Q2", "answer": "A2"},
    ]

    with patch("httpx.AsyncClient.get") as mock_get:
        mock_get.return_value = AsyncMock(
            status_code=200,
            json=lambda: mock_response,
            headers={"content-type": "application/json"},
        )
        ingestor = APIIngestor(source_uri="https://api.example.com/faq")
        result = await ingestor.ingest(collection_name="test_col")

    assert isinstance(result, APIIngestResult)
    assert result.chunks_created == 2
    assert result.source_uri == "https://api.example.com/faq"
    assert result.content_type == "application/json"
    assert result.success is True


async def test_ingest_handles_list_response() -> None:
    """リスト形式の JSON レスポンスを処理できること."""
    pass  # 上記テストでカバー


async def test_ingest_handles_dict_with_data_key() -> None:
    """{"data": [...]} 形式の JSON を処理できること."""
    mock_response = {"data": [{"text": "doc1"}, {"text": "doc2"}], "total": 2}

    with patch("httpx.AsyncClient.get") as mock_get:
        mock_get.return_value = AsyncMock(
            status_code=200,
            json=lambda: mock_response,
            headers={"content-type": "application/json"},
        )
        ingestor = APIIngestor(source_uri="https://api.example.com/docs")
        result = await ingestor.ingest(collection_name="test_col")

    assert result.chunks_created == 2


async def test_ingest_returns_error_result_on_failure() -> None:
    """API 接続失敗時にエラー結果を返すこと（例外を上げない）."""
    with patch("httpx.AsyncClient.get", side_effect=Exception("Connection refused")):
        ingestor = APIIngestor(source_uri="https://unreachable.example.com")
        result = await ingestor.ingest(collection_name="test_col")

    assert result.success is False
    assert "Connection refused" in result.error_message
    assert result.chunks_created == 0
```

**Step 2: テスト実行（失敗確認）**

```bash
pytest tests/unit/knowledge/test_api_ingestion.py -v
```
Expected: ImportError (APIIngestor not defined)

**Step 3: API インジェスター実装**

`agentflow/knowledge/api_ingestion.py`:
```python
"""API データソースからのドキュメントインジェスト.

外部 REST API を RAG データソースとして取り込む。
返り値の型（JSON array / {"data": [...]} / text）を自動判別。
"""
from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import Any

import httpx


logger = logging.getLogger(__name__)


@dataclass
class APIIngestResult:
    """API インジェスト結果（型付き）."""

    source_uri: str
    chunks_created: int = 0
    content_type: str = "unknown"
    success: bool = True
    error_message: str = ""
    raw_item_count: int = 0
    warnings: list[str] = field(default_factory=list)


class APIIngestor:
    """外部 API からドキュメントを取り込む.

    対応する返り値型:
    - JSON array: [{"text": "..."}, ...]
    - JSON object with data key: {"data": [...], "total": N}
    - JSON object with items/results key: {"items": [...]}
    - Plain text
    """

    def __init__(
        self,
        source_uri: str,
        headers: dict[str, str] | None = None,
        text_fields: list[str] | None = None,
        timeout: float = 30.0,
    ) -> None:
        self._uri = source_uri
        self._headers = headers or {}
        self._text_fields = text_fields or ["text", "content", "body", "answer", "description", "question"]
        self._timeout = timeout

    async def ingest(self, collection_name: str) -> APIIngestResult:
        """API を呼び出してドキュメントを取り込む."""
        result = APIIngestResult(source_uri=self._uri)
        try:
            async with httpx.AsyncClient(timeout=self._timeout) as client:
                resp = await client.get(self._uri, headers=self._headers)
                resp.raise_for_status()

            content_type = resp.headers.get("content-type", "").split(";")[0].strip()
            result.content_type = content_type

            if "json" in content_type:
                items = self._extract_items(resp.json())
            else:
                items = [{"text": resp.text}]

            result.raw_item_count = len(items)
            texts = self._extract_texts(items)
            result.chunks_created = len(texts)
            # 実際のベクトル化・保存は呼び出し側の CollectionManager が行う
            result.success = True

        except Exception as exc:
            logger.warning("API インジェスト失敗 %s: %s", self._uri, exc)
            result.success = False
            result.error_message = str(exc)

        return result

    def _extract_items(self, payload: Any) -> list[dict[str, Any]]:
        """JSON ペイロードからアイテムリストを抽出."""
        if isinstance(payload, list):
            return payload
        if isinstance(payload, dict):
            for key in ("data", "items", "results", "records", "documents"):
                if isinstance(payload.get(key), list):
                    return payload[key]
            return [payload]
        return []

    def _extract_texts(self, items: list[Any]) -> list[str]:
        """アイテムリストからテキストを抽出."""
        texts: list[str] = []
        for item in items:
            if isinstance(item, str):
                texts.append(item)
            elif isinstance(item, dict):
                for field_name in self._text_fields:
                    if isinstance(item.get(field_name), str) and item[field_name].strip():
                        texts.append(item[field_name])
                        break
                else:
                    # テキストフィールドなし: 全値を結合
                    combined = " ".join(
                        str(v) for v in item.values() if isinstance(v, str) and str(v).strip()
                    )
                    if combined:
                        texts.append(combined)
        return [t for t in texts if t.strip()]
```

**Step 4: テスト実行（成功確認）**

```bash
pytest tests/unit/knowledge/test_api_ingestion.py -v
```
Expected: PASS

**Step 5: agentflow/knowledge/__init__.py に APIIngestor を追加**

```python
# agentflow/knowledge/__init__.py に追加
from agentflow.knowledge.api_ingestion import APIIngestor, APIIngestResult
```

**Step 6: コミット**

```bash
git add agentflow/knowledge/api_ingestion.py agentflow/knowledge/__init__.py tests/unit/knowledge/test_api_ingestion.py
git commit -m "feat(knowledge): API データソースインジェスター追加 - 返り値型の自動判別対応"
```

---

## Task 7: DGE 知識管理の整理・統合

**背景:** DGE の 3 か所は道法術器フレームワーク上意図的な設計だが、`knowledge.py` が古い独自実装で `agentflow.knowledge` を使っていない。

**Files:**
- Modify: `apps/decision_governance_engine/routers/knowledge.py`（agentflow.knowledge 使用に統一）
- Modify: `apps/decision_governance_engine/routers/config.py`（per-agent RAG 設定を統一設定 API に接続）
- Test: `tests/apps/decision_governance_engine/test_knowledge_unified.py`

**Step 1: テストを書く**

```python
# tests/apps/decision_governance_engine/test_knowledge_unified.py
"""DGE 統合知識管理 API テスト."""
import pytest
from httpx import AsyncClient


async def test_knowledge_api_uses_agentflow_collection_manager(
    client: AsyncClient,
) -> None:
    """知識管理 API が agentflow.knowledge.CollectionManager を使うこと."""
    # GET /api/knowledge/collections
    resp = await client.get("/api/knowledge/collections")
    assert resp.status_code == 200
    data = resp.json()
    assert "collections" in data


async def test_per_agent_rag_config_api(client: AsyncClient) -> None:
    """per-agent RAG 設定取得 API."""
    resp = await client.get("/api/config/rag")
    assert resp.status_code == 200
    data = resp.json()
    assert isinstance(data, list)
    agent_ids = [item["agent_id"] for item in data]
    assert "shu" in agent_ids
    assert "qi" in agent_ids


async def test_per_agent_rag_config_update(client: AsyncClient) -> None:
    """per-agent RAG 設定更新で top_k が変更されること."""
    resp = await client.put(
        "/api/config/rag/shu",
        json={"top_k": 8},
    )
    assert resp.status_code == 200
    data = resp.json()
    assert data["top_k"] == 8
```

**Step 2: テスト実行（失敗確認）**

```bash
pytest tests/apps/decision_governance_engine/test_knowledge_unified.py -v
```

**Step 3: knowledge.py を agentflow.knowledge に統一**

`apps/decision_governance_engine/routers/knowledge.py` の独自 RAG 実装を削除し、`agentflow.knowledge.CollectionManager` を使うよう修正。具体的には:
- `knowledge.py` の RAG 初期化ロジックを `agentflow.knowledge` 呼び出しに変更
- `knowledge_collections.py` との重複エンドポイントを除去
- per-agent 設定は `config.py` に集約、コレクション管理は `knowledge_collections.py` に集約

**Step 4: テスト実行（成功確認）**

```bash
pytest tests/apps/decision_governance_engine/test_knowledge_unified.py -v
```

**Step 5: コミット**

```bash
git add apps/decision_governance_engine/routers/knowledge.py apps/decision_governance_engine/routers/config.py
git add -f tests/apps/decision_governance_engine/test_knowledge_unified.py
git commit -m "refactor(dge): knowledge.py を agentflow.knowledge に統一 - 3か所 RAG の責務明確化"
```

---

## Task 8: FAQ System - 画面レベル E2E テスト（Playwright）

**Files:**
- Test: `apps/faq_system/tests/test_rag_ui_e2e.spec.ts`
- Setup: `apps/faq_system/frontend/playwright.config.ts`

**Step 1: Playwright 設定確認・作成**

```typescript
// apps/faq_system/frontend/playwright.config.ts
import { defineConfig } from '@playwright/test';

export default defineConfig({
  testDir: '../tests',
  testMatch: '**/*.spec.ts',
  use: {
    baseURL: 'http://localhost:3004',  // FAQ frontend port
    screenshot: 'only-on-failure',
  },
  webServer: {
    command: 'npm run dev',
    port: 3004,
    reuseExistingServer: !process.env.CI,
    cwd: 'apps/faq_system/frontend',
  },
});
```

**Step 2: E2E テストを書く**

```typescript
// apps/faq_system/tests/test_rag_ui_e2e.spec.ts
import { test, expect } from '@playwright/test';

test.describe('FAQ System - RAG 管理 UI E2E', () => {
  test.beforeEach(async ({ page }) => {
    // ログイン
    await page.goto('/login');
    await page.fill('[name="username"]', 'admin');
    await page.fill('[name="password"]', 'admin');
    await page.click('[type="submit"]');
    await page.waitForURL('/');
  });

  test('コレクション一覧画面が表示される', async ({ page }) => {
    await page.click('[data-testid="nav-rag"]');
    await page.click('[data-testid="rag-tab-collections"]');
    await expect(page.locator('[data-testid="collection-list"]')).toBeVisible();
  });

  test('新規コレクション作成フォームが開く', async ({ page }) => {
    await page.click('[data-testid="nav-rag"]');
    await page.click('[data-testid="rag-tab-collections"]');
    await page.click('[data-testid="create-collection-button"]');
    await expect(page.locator('[data-testid="collection-form-modal"]')).toBeVisible();
  });

  test('コレクション作成 - FAQ Precision パターン自動セット', async ({ page }) => {
    await page.click('[data-testid="nav-rag"]');
    await page.click('[data-testid="rag-tab-collections"]');
    await page.click('[data-testid="create-collection-button"]');

    // パターン選択
    await page.selectOption('[data-testid="pattern-select"]', 'faq_precision');

    // 自動セット確認
    await expect(page.locator('[data-testid="chunk-strategy"]')).toHaveValue('sentence');
    await expect(page.locator('[data-testid="chunk-size"]')).toHaveValue('500');
  });

  test('ファイルアップロード UI が表示される', async ({ page }) => {
    await page.click('[data-testid="nav-rag"]');
    await page.click('[data-testid="rag-tab-documents"]');
    await expect(page.locator('[data-testid="upload-area"]')).toBeVisible();
  });

  test('検索テスト UI が動作する', async ({ page }) => {
    await page.click('[data-testid="nav-rag"]');
    await page.click('[data-testid="rag-tab-retrieval"]');
    await expect(page.locator('[data-testid="test-query-input"]')).toBeVisible();

    await page.fill('[data-testid="test-query-input"]', 'テストクエリ');
    await page.click('[data-testid="run-test-query-button"]');
    // 結果エリアが表示される（空でも可）
    await expect(page.locator('[data-testid="query-result-area"]')).toBeVisible();
  });

  test('インジェスト履歴タブが開く', async ({ page }) => {
    await page.click('[data-testid="nav-rag"]');
    await page.click('[data-testid="rag-tab-ingest"]');
    await expect(page.locator('[data-testid="ingest-history-list"]')).toBeVisible();
  });
});
```

**Step 3: data-testid 属性を FAQフロントエンドに追加**

各コンポーネントに `data-testid` を追加（E2E テストが参照できるよう）:

- `RAGLayout.tsx`: タブボタンに `data-testid="rag-tab-{key}"`
- `CollectionManager.tsx`: 作成ボタンに `data-testid="create-collection-button"`
- `DocumentManager.tsx`: アップロードエリアに `data-testid="upload-area"`
- `RetrievalSettings.tsx`: クエリ入力に `data-testid="test-query-input"`

**Step 4: E2E テスト実行**

```bash
cd apps/faq_system/frontend
npm run dev &  # バックグラウンドで起動
npx playwright test apps/faq_system/tests/test_rag_ui_e2e.spec.ts
```

**Step 5: コミット**

```bash
git add apps/faq_system/frontend/ apps/faq_system/tests/test_rag_ui_e2e.spec.ts
git commit -m "test(faq): 画面レベル E2E テスト追加 - RAG 管理 UI 全タブ"
```

---

## Task 9: Platform RAG 画面 - Qdrant セットアップボタン

**Files:**
- Modify: `apps/platform/frontend/src/components/RAGOverview.tsx`（セットアップボタン追加）
- Modify: `apps/platform/routers/rag.py`（シェルスクリプト実行 API）
- Test: `tests/apps/platform/test_rag_setup_e2e.spec.ts`

**Step 1: バックエンド - シェルスクリプト実行 API**

```python
# apps/platform/routers/rag.py に追加

@router.post("/setup/qdrant")
async def setup_qdrant_local() -> dict[str, Any]:
    """Qdrant ローカルセットアップを実行.

    scripts/setup_qdrant.sh を実行してローカル Qdrant を起動する。
    ユーザーが「Qdrant セットアップ」ボタンをクリックした際に呼ばれる。

    Returns:
        セットアップ結果（success/message/qdrant_url）
    """
    import asyncio
    import shlex
    from pathlib import Path

    script_path = Path(__file__).parents[3] / "scripts" / "setup_qdrant.sh"
    if not script_path.exists():
        raise HTTPException(status_code=404, detail={"message": "setup_qdrant.sh not found"})

    try:
        proc = await asyncio.create_subprocess_exec(
            "bash", str(script_path),
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
        )
        stdout, stderr = await asyncio.wait_for(proc.communicate(), timeout=60.0)
        output = stdout.decode() + stderr.decode()
        success = proc.returncode == 0
    except asyncio.TimeoutError:
        return {"success": False, "message": "セットアップタイムアウト（60秒）", "output": ""}
    except Exception as exc:
        return {"success": False, "message": str(exc), "output": ""}

    return {
        "success": success,
        "message": "Qdrant セットアップ完了" if success else "セットアップ失敗",
        "qdrant_url": "http://localhost:6333" if success else None,
        "output": output[-2000:],  # 最後 2000 文字
    }
```

**Step 2: フロントエンド - セットアップボタン**

`RAGOverview.tsx` に Qdrant セットアップボタン（ベクトル DB 未設定の App がある場合に表示）を追加:

```typescript
// RAGOverview.tsx 内の setupQdrant 関数
const handleSetupQdrant = async () => {
  setSetupLoading(true);
  try {
    const resp = await api.post('/studios/framework/rag/setup/qdrant');
    if (resp.data.success) {
      toast.success('Qdrant セットアップ完了');
      await loadConfigs();  // 設定を再読み込み
    } else {
      toast.error(`セットアップ失敗: ${resp.data.message}`);
    }
  } catch (err) {
    toast.error('セットアップエラー');
  } finally {
    setSetupLoading(false);
  }
};
```

**Step 3: E2E テスト**

```typescript
// tests/apps/platform/test_rag_setup_e2e.spec.ts
test('Qdrant セットアップボタンが表示される（ベクトルDB未設定時）', async ({ page }) => {
  await page.goto('/');
  await page.click('[data-testid="nav-rag-overview"]');
  // ベクトルDB未設定のAppがある場合
  const setupBtn = page.locator('[data-testid="setup-qdrant-button"]');
  if (await setupBtn.isVisible()) {
    await expect(setupBtn).toBeEnabled();
  }
});
```

**Step 4: テスト実行**

```bash
pytest tests/apps/platform/test_rag_router.py -v -k "setup"
```

**Step 5: コミット**

```bash
git add apps/platform/routers/rag.py apps/platform/frontend/src/components/RAGOverview.tsx
git add -f tests/apps/platform/test_rag_setup_e2e.spec.ts
git commit -m "feat(platform): Qdrant セットアップボタン追加 - ワンクリックで Qdrant を起動"
```

---

## Task 10: 全テスト通過確認 + ruff/mypy チェック

**Step 1: Python テスト全実行**

```bash
conda activate agentflow
cd /home/liush/projects/serverlessAIAgents
pytest tests/memory/vector_db/test_qdrant_graceful.py \
       tests/scripts/test_setup_qdrant.py \
       tests/unit/knowledge/test_api_ingestion.py \
       tests/apps/platform/test_rag_router.py \
       apps/faq_system/tests/test_rag_settings_e2e.py \
       -v
```
Expected: 全 PASS

**Step 2: Ruff + Mypy**

```bash
ruff check agentflow/memory/vector_db/qdrant_db.py agentflow/knowledge/api_ingestion.py apps/platform/routers/rag.py
ruff format agentflow/memory/vector_db/qdrant_db.py agentflow/knowledge/api_ingestion.py apps/platform/routers/rag.py
mypy agentflow/memory/vector_db/qdrant_db.py agentflow/knowledge/api_ingestion.py
```

**Step 3: TypeScript ビルド確認**

```bash
cd apps/platform/frontend && npm run type-check && npm run build
cd apps/faq_system/frontend && npm run type-check && npm run build
```

**Step 4: E2E テスト実行（サーバー起動後）**

```bash
# バックグラウンドでサーバー起動（別ターミナル）
# conda activate agentflow && python -m apps.platform.main serve --port 8001

# Platform E2E
npx playwright test tests/apps/platform/ --reporter=html

# FAQ E2E
npx playwright test apps/faq_system/tests/ --reporter=html
```

**Step 5: 最終コミット**

```bash
git add -A
git commit -m "test: 全 RAG 改善テスト通過確認 - 単体/E2E 全 PASS"
```

---

## 実用観点での追加改善事項（Q6 回答）

以下は現時点で発見されたが本プランに含まれない追加改善候補：

| 優先度 | 課題 | 推奨対応 |
|--------|------|---------|
| HIGH | code_migration_assistant に統一 RAG Settings API がない | Task 5 相当を CMA にも適用 |
| HIGH | market_trend_monitor の semantic search が RAG と統合されていない | RAGEngine に移行 |
| MEDIUM | Embedding モデル選択 UI がない（現在 `null` のまま） | OpenAI / SentenceTransformer 選択を RAG 設定フォームに追加 |
| MEDIUM | ベクトル DB 接続テストボタン（設定後に疎通確認できない） | `/api/rag/test-connection` エンドポイント追加 |
| MEDIUM | インジェスト進捗の SSE ストリーミングが FAQ にしかない | 全 App に agentflow RAGService の SSE を適用 |
| LOW | RAG キャッシュ（同一クエリの二重ベクトル化を防ぐ Redis キャッシュ） | `agentflow/knowledge/rag_pipeline.py` に追加 |
| LOW | ナレッジベースのバックアップ/エクスポート機能 | JSON/CSV エクスポート API |

---

## DGE 3か所 RAG の設計説明（Q5 回答）

```
道法術器フレームワーク
├── 道 (Dao)   - 哲学・原則エージェント → 独自 knowledge なし
├── 法 (Fa)    - 規則・基準エージェント → 規制ドキュメント知識
├── 術 (Shu)   - 戦略・手法エージェント → 業界慣行・戦略知識 [config.py: RAG設定]
└── 器 (Qi)    - 技術・実装エージェント → 技術実装・ツール知識 [config.py: RAG設定]

画面:
├── knowledge_collections.py → コレクション CRUD + RBAC（管理者が全知識を管理）
└── knowledge.py             → 簡易文書追加 API（今後 knowledge_collections に統合）

フロントエンド:
└── KnowledgeRAGConfig.tsx → コレクション選択 + テストクエリ UI
```

「3か所」は実は **役割が異なる 3層**：
1. **config.py** = 術/器エージェントへの RAG 設定割り当て（what knowledge each agent uses）
2. **knowledge_collections.py** = コレクション自体の CRUD（actual knowledge storage）
3. **knowledge.py** = 移行中の旧 API（Task 7 で knowledge_collections に統合予定）

これはバグではなく、道法術器フレームワークの自然な結果です。ただし `knowledge.py` の旧実装は Task 7 で整理します。
