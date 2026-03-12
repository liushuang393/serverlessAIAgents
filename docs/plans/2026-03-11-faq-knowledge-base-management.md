# FAQ ナレッジベース管理の完善 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** FAQ システムのナレッジベース管理を「設定歯車アイコン」から開く右サイドパネルに移設し、RAG パイプライン（ファイルアップロード→チャンキング→ベクトルDB格納→検索）を確実に動作させ、チャンク設定・TopK・検索方式・権限管理をユーザーフレンドリーに設定可能にする。

**Architecture:**
- 現行の `/rag` 独立ページを廃止し、右サイドパネル（歯車アイコンで開閉）に統合
- RAGFlow/Dify 風の「ドキュメント→チャンク→インデックス→検索テスト」一気通貫ワークフロー
- コレクション単位で chunk_size / chunk_overlap / retrieval_method / top_k / reranker を設定
- agentflow/knowledge/ のフレームワーク層を活用し、アプリ層は薄く保つ
- 権限管理は既存 RBAC (admin/manager/employee/guest) を活かし UI で可視化

**Tech Stack:** React + TypeScript + Zustand + Tailwind CSS, FastAPI, agentflow/knowledge/*, Qdrant, pytest

---

## 現状分析

### 動作するもの ✅
- コレクション CRUD（CollectionManager.tsx ↔ collections.py）
- ドキュメントアップロード（DocumentManager.tsx ↔ collections.py）
- チャンクプレビュー（preview-chunks エンドポイント）
- 検索テスト（RetrievalSettings.tsx ↔ test-query エンドポイント）
- 6種チャンキング戦略（agentflow/knowledge/chunking.py）
- 5種リランカー（agentflow/knowledge/reranker.py）
- RAG API クライアント（frontend/src/api/rag.ts）
- Zustand ストア（ragStore.ts）

### 改善が必要なもの ⚠️
1. **レイアウト/UX**: ナレッジ管理が独立ページ (`/rag`) にあり、チャット中にアクセスしにくい → 右サイドパネル（歯車）に移設
2. **RAG 精度設定**: コレクション作成時にパターンプリセットはあるが、既存コレクションの設定変更が分かりにくい
3. **ドキュメント→ベクトルDB フロー**: アップロード後の手動インデックスが分かりにくい、自動インデックスオプション欲しい
4. **権限管理 UI**: AccessControlPage が read-only 情報表示のみ → 管理者向け操作を追加
5. **設定の粒度**: chunk_size / top_k / retrieval_method の変更後、即座に効果を確認できるテスト機能が弱い

---

## Task 1: 右サイドパネルコンポーネント作成

**目的:** 歯車アイコンで開閉する右サイドパネルのシェル（骨格）を作成する。

**Files:**
- Create: `apps/faq_system/frontend/src/components/settings/KnowledgePanel.tsx`
- Modify: `apps/faq_system/frontend/src/components/chat/ChatWindow.tsx:117-124`

**Step 1: テストファイル作成**

KnowledgePanel のスナップショット的なテストを書く。

Create: `apps/faq_system/frontend/src/__tests__/KnowledgePanel.test.tsx`

```tsx
import { describe, it, expect, vi } from 'vitest';
import { render, screen, fireEvent } from '@testing-library/react';
import { KnowledgePanel } from '../components/settings/KnowledgePanel';

// ragStore をモック
vi.mock('../stores/ragStore', () => ({
  useRagStore: () => ({
    collections: [],
    fetchCollections: vi.fn(),
    activeTab: 'collections',
    setActiveTab: vi.fn(),
    error: null,
    clearError: vi.fn(),
  }),
}));

describe('KnowledgePanel', () => {
  it('パネルが閉じている時は表示されない', () => {
    const { container } = render(
      <KnowledgePanel isOpen={false} onClose={vi.fn()} />
    );
    // パネル要素が非表示
    const panel = container.querySelector('[data-testid="knowledge-panel"]');
    expect(panel?.classList.contains('translate-x-full')).toBe(true);
  });

  it('パネルが開いている時にタブが表示される', () => {
    render(<KnowledgePanel isOpen={true} onClose={vi.fn()} />);
    expect(screen.getByText('コレクション')).toBeTruthy();
    expect(screen.getByText('ドキュメント')).toBeTruthy();
    expect(screen.getByText('検索設定')).toBeTruthy();
  });

  it('閉じるボタンで onClose が呼ばれる', () => {
    const onClose = vi.fn();
    render(<KnowledgePanel isOpen={true} onClose={onClose} />);
    fireEvent.click(screen.getByTitle('閉じる'));
    expect(onClose).toHaveBeenCalled();
  });
});
```

**Step 2: テスト実行で失敗を確認**

Run: `cd apps/faq_system/frontend && npx vitest run src/__tests__/KnowledgePanel.test.tsx`
Expected: FAIL（KnowledgePanel が存在しない）

**Step 3: KnowledgePanel コンポーネント作成**

Create: `apps/faq_system/frontend/src/components/settings/KnowledgePanel.tsx`

```tsx
import { useState } from 'react';
import { X, Database, FileText, Search, Shield, Settings2 } from 'lucide-react';
import { useRagStore } from '../../stores/ragStore';

interface KnowledgePanelProps {
  /** パネルが開いているか */
  isOpen: boolean;
  /** パネルを閉じるコールバック */
  onClose: () => void;
}

type PanelTab = 'collections' | 'documents' | 'retrieval' | 'access';

const TAB_CONFIG: { key: PanelTab; label: string; icon: typeof Database }[] = [
  { key: 'collections', label: 'コレクション', icon: Database },
  { key: 'documents', label: 'ドキュメント', icon: FileText },
  { key: 'retrieval', label: '検索設定', icon: Search },
  { key: 'access', label: 'アクセス', icon: Shield },
];

export const KnowledgePanel = ({ isOpen, onClose }: KnowledgePanelProps) => {
  const [activeTab, setActiveTab] = useState<PanelTab>('collections');

  return (
    <div
      data-testid="knowledge-panel"
      className={`fixed top-0 right-0 h-full w-[480px] max-w-[90vw] glass border-l border-white/10 z-30 flex flex-col transition-transform duration-300 ease-in-out ${
        isOpen ? 'translate-x-0' : 'translate-x-full'
      }`}
    >
      {/* ヘッダー */}
      <div className="flex items-center justify-between p-4 border-b border-white/5">
        <div className="flex items-center gap-2">
          <Settings2 size={18} className="text-[var(--primary)]" />
          <h2 className="text-sm font-bold text-white">ナレッジベース管理</h2>
        </div>
        <button
          onClick={onClose}
          title="閉じる"
          className="p-1.5 rounded-lg hover:bg-white/10 text-[var(--text-muted)] hover:text-white transition-all"
        >
          <X size={16} />
        </button>
      </div>

      {/* タブバー */}
      <div className="flex border-b border-white/5 px-2">
        {TAB_CONFIG.map(({ key, label, icon: Icon }) => (
          <button
            key={key}
            onClick={() => setActiveTab(key)}
            className={`flex items-center gap-1.5 px-3 py-2.5 text-xs font-medium transition-all border-b-2 ${
              activeTab === key
                ? 'border-[var(--primary)] text-white'
                : 'border-transparent text-[var(--text-muted)] hover:text-white'
            }`}
          >
            <Icon size={13} />
            {label}
          </button>
        ))}
      </div>

      {/* タブコンテンツ */}
      <div className="flex-1 overflow-y-auto p-4 custom-scrollbar">
        {activeTab === 'collections' && <PanelCollections />}
        {activeTab === 'documents' && <PanelDocuments />}
        {activeTab === 'retrieval' && <PanelRetrieval />}
        {activeTab === 'access' && <PanelAccess />}
      </div>
    </div>
  );
};

/** コレクション一覧（Task 3 で詳細実装） */
const PanelCollections = () => {
  const { collections, fetchCollections } = useRagStore();
  return (
    <div className="text-sm text-[var(--text-muted)]">
      コレクション管理 — Task 3 で実装
    </div>
  );
};

/** ドキュメント管理（Task 4 で詳細実装） */
const PanelDocuments = () => (
  <div className="text-sm text-[var(--text-muted)]">
    ドキュメント管理 — Task 4 で実装
  </div>
);

/** 検索設定（Task 5 で詳細実装） */
const PanelRetrieval = () => (
  <div className="text-sm text-[var(--text-muted)]">
    検索設定 — Task 5 で実装
  </div>
);

/** アクセス制御（Task 7 で詳細実装） */
const PanelAccess = () => (
  <div className="text-sm text-[var(--text-muted)]">
    アクセス制御 — Task 7 で実装
  </div>
);
```

**Step 4: テスト実行で成功を確認**

Run: `cd apps/faq_system/frontend && npx vitest run src/__tests__/KnowledgePanel.test.tsx`
Expected: PASS

**Step 5: コミット**

```bash
git add apps/faq_system/frontend/src/components/settings/KnowledgePanel.tsx apps/faq_system/frontend/src/__tests__/KnowledgePanel.test.tsx
git commit -m "feat(faq): ナレッジベース管理用右サイドパネルの骨格を作成"
```

---

## Task 2: ChatWindow に歯車アイコン→パネル開閉を統合

**目的:** 現行の設定歯車アイコンをクリックした時に、SettingsModal と KnowledgePanel の両方にアクセスできるようにする。

**Files:**
- Modify: `apps/faq_system/frontend/src/components/chat/ChatWindow.tsx`
- Modify: `apps/faq_system/frontend/src/App.tsx`

**Step 1: ChatWindow にパネルトグルを追加**

Modify: `apps/faq_system/frontend/src/components/chat/ChatWindow.tsx`

現行の設定ボタン（line 117-124）の隣に、ナレッジベース管理ボタンを追加する。

```tsx
// import 追加（既存 import に追加）
import { KnowledgePanel } from '../settings/KnowledgePanel';
// Database は既存 import にあるのでスキップ

// state 追加（isSettingsOpen の後に追加）
const [isKnowledgePanelOpen, setIsKnowledgePanelOpen] = useState(false);

// ヘッダー右側のボタンエリアを変更（line 117-124 を置換）
{/* 設定ボタンエリア（右上） */}
<div className="flex items-center gap-1">
    <button
        onClick={() => setIsKnowledgePanelOpen(prev => !prev)}
        className={`p-2 rounded-xl hover:bg-white/5 transition-all border border-transparent hover:border-white/10 ${
            isKnowledgePanelOpen
                ? 'text-[var(--primary)] bg-white/5 border-white/10'
                : 'text-[var(--text-muted)] hover:text-white'
        }`}
        title="ナレッジベース管理"
    >
        <Database size={16} />
    </button>
    <button
        onClick={() => setIsSettingsOpen(true)}
        className="p-2 rounded-xl hover:bg-white/5 text-[var(--text-muted)] hover:text-white transition-all border border-transparent hover:border-white/10"
        title={t('sidebar.settings')}
    >
        <Settings size={16} />
    </button>
</div>

// SettingsModal の直後に KnowledgePanel を配置
<KnowledgePanel
    isOpen={isKnowledgePanelOpen}
    onClose={() => setIsKnowledgePanelOpen(false)}
/>
```

**Step 2: Sidebar のナビゲーションから `/rag` リンクを残すが非推奨化**

Modify: `apps/faq_system/frontend/src/components/layout/Sidebar.tsx:124-137`

`/rag` ボタンのラベルを `ナレッジベース (詳細)` に変更し、メインアクセスは ChatWindow ヘッダーのアイコンからにする。

> **注意:** `/rag` ルートはまだ残す。フル画面の詳細管理が必要な場合（ダッシュボード、インジェスト履歴）に使うため。

**Step 3: テスト実行**

Run: `cd apps/faq_system/frontend && npx vitest run`
Expected: PASS（既存テストが壊れないこと）

**Step 4: コミット**

```bash
git add apps/faq_system/frontend/src/components/chat/ChatWindow.tsx apps/faq_system/frontend/src/components/layout/Sidebar.tsx
git commit -m "feat(faq): ChatWindow ヘッダーにナレッジベース管理パネルのトグルを追加"
```

---

## Task 3: パネル内コレクション管理の実装

**目的:** サイドパネル内でコレクションの一覧表示・作成・設定変更を可能にする。FlowRAG/Dify 風のカード UI で、パターンプリセット選択→カスタマイズの2段階操作。

**Files:**
- Modify: `apps/faq_system/frontend/src/components/settings/KnowledgePanel.tsx`（PanelCollections 部分）

**Step 1: テスト作成**

Create/Modify: `apps/faq_system/frontend/src/__tests__/PanelCollections.test.tsx`

```tsx
import { describe, it, expect, vi } from 'vitest';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';

// ragStore の全メソッドをモック
vi.mock('../../stores/ragStore', () => ({
  useRagStore: () => ({
    collections: [
      {
        collection_name: 'faq_knowledge',
        display_name: 'FAQ ナレッジ',
        chunk_strategy: 'sentence',
        chunk_size: 500,
        chunk_overlap: 80,
        retrieval_method: 'hybrid',
        reranker: 'cohere',
        top_k: 8,
        min_similarity: 0.25,
        document_count: 12,
      },
    ],
    selectedCollection: null,
    fetchCollections: vi.fn(),
    createCollection: vi.fn(),
    updateCollection: vi.fn(),
    selectCollection: vi.fn(),
    error: null,
  }),
}));

describe('PanelCollections', () => {
  it('コレクション一覧にカードが表示される', async () => {
    // PanelCollections を直接レンダリング
    // 実装後に具体的なテストを追記
  });

  it('プリセット選択で設定値が自動入力される', async () => {
    // faq_precision を選択 → chunk_size=500, top_k=8 が入る
  });

  it('カスタム設定でチャンクサイズを変更できる', async () => {
    // chunk_size のスライダーを動かす → 値が反映
  });
});
```

**Step 2: PanelCollections を実装**

KnowledgePanel.tsx 内の `PanelCollections` コンポーネントを実装する。

主要 UI 要素:
- コレクション一覧カード（名前、ドキュメント数、チャンク戦略、検索方式をバッジ表示）
- 「＋ 新規コレクション」ボタン
- コレクション選択 → 設定フォーム展開
  - **パターンプリセット選択**（3つのプリセット + カスタム）
    - `faq_precision`: FAQ 高精度（sentence/500/hybrid/cohere/top_k=8）
    - `balanced_knowledge`: バランス型（recursive/800/hybrid/bm25/top_k=6）
    - `long_doc_reasoning`: 長文推論（markdown/1200/multi_query/cross_encoder/top_k=10）
    - `custom`: カスタム
  - **チャンキング設定**
    - 戦略ドロップダウン（recursive/sentence/semantic/token/markdown）
    - チャンクサイズスライダー（100〜4000、ステップ50）
    - オーバーラップスライダー（0〜500、ステップ10）
  - **検索設定**
    - 検索方式（semantic/hybrid/keyword）
    - リランカー（none/bm25/cohere/cross_encoder）
    - Top-K スライダー（1〜50）
    - 類似度閾値スライダー（0.0〜1.0、ステップ0.05）
  - 「保存」ボタン

**Step 3: テスト実行**

Run: `cd apps/faq_system/frontend && npx vitest run`
Expected: PASS

**Step 4: コミット**

```bash
git add apps/faq_system/frontend/src/components/settings/KnowledgePanel.tsx apps/faq_system/frontend/src/__tests__/PanelCollections.test.tsx
git commit -m "feat(faq): パネル内コレクション管理 - プリセット＋カスタム設定UI"
```

---

## Task 4: パネル内ドキュメント管理の実装

**目的:** ドキュメントのアップロード→チャンクプレビュー→インデックス→再インデックスのフローをサイドパネル内で完結させる。アップロード後の自動インデックスオプションも追加。

**Files:**
- Modify: `apps/faq_system/frontend/src/components/settings/KnowledgePanel.tsx`（PanelDocuments 部分）
- Modify: `apps/faq_system/routers/collections.py`（自動インデックスオプション追加）

**Step 1: バックエンドに auto_index パラメータ追加**

Modify: `apps/faq_system/routers/collections.py` の `upload_document` エンドポイント

```python
@router.post("/{collection_name}/documents")
@require_auth
async def upload_document(
    collection_name: str,
    file: UploadFile,
    auto_index: bool = Form(False),  # 追加
    request: Request = ...,
) -> dict[str, Any]:
    """ドキュメントアップロード。auto_index=True ならアップロード直後にインデックス実行。"""
    # 既存のアップロード処理
    result = await doc_manager.upload_document(...)

    # 自動インデックス
    if auto_index and result:
        doc_id = result.get("id") or result.get("document_id")
        if doc_id:
            await doc_manager.index_document(doc_id)
            # 最新のドキュメント情報を取得して返す
            updated = await doc_manager.get_document(doc_id)
            if updated:
                return {"document": updated.to_dict()}

    return {"document": result.to_dict() if hasattr(result, 'to_dict') else result}
```

**Step 2: テスト作成**

Create: `apps/faq_system/tests/test_auto_index.py`

```python
"""auto_index パラメータのテスト。"""
import pytest
from unittest.mock import AsyncMock, MagicMock, patch


class TestAutoIndex:
    """アップロード時の自動インデックスオプションのテスト。"""

    @pytest.mark.asyncio
    async def test_upload_without_auto_index(self) -> None:
        """auto_index=False の場合、インデックスは実行されない。"""
        # DocumentManager.index_document が呼ばれないことを検証
        pass  # 実装時にモックで具体化

    @pytest.mark.asyncio
    async def test_upload_with_auto_index(self) -> None:
        """auto_index=True の場合、アップロード後にインデックスが実行される。"""
        # DocumentManager.index_document が呼ばれることを検証
        pass  # 実装時にモックで具体化
```

**Step 3: PanelDocuments を実装**

KnowledgePanel.tsx 内の `PanelDocuments` コンポーネント:

- **コレクション選択ドロップダウン**（対象コレクション選択）
- **ドラッグ＆ドロップアップロードエリア**
  - 対応形式: PDF, DOCX, CSV, TXT, MD, JSON
  - 「アップロード後に自動インデックス」チェックボックス
- **ドキュメント一覧**
  - ステータスバッジ（uploaded=黄, indexed=緑, error=赤）
  - アクション: プレビュー / インデックス / 再インデックス / 削除
- **チャンクプレビューモーダル**
  - チャンク数表示
  - 各チャンクの先頭 200 文字プレビュー
  - メタデータ表示

**Step 4: テスト実行**

Run: `cd apps/faq_system/frontend && npx vitest run`
Expected: PASS

**Step 5: コミット**

```bash
git add apps/faq_system/frontend/src/components/settings/KnowledgePanel.tsx apps/faq_system/routers/collections.py apps/faq_system/tests/test_auto_index.py
git commit -m "feat(faq): パネル内ドキュメント管理 - アップロード・自動インデックス・プレビュー"
```

---

## Task 5: パネル内検索設定＆テストクエリの実装

**目的:** コレクションの RAG 設定（チャンクサイズ、Top-K、検索方式、リランカー等）をリアルタイムに変更し、テストクエリで即座に精度を確認できる UI を実装する。FlowRAG のように「設定変更→テスト→確認→保存」のイテレーションを高速に回せるようにする。

**Files:**
- Modify: `apps/faq_system/frontend/src/components/settings/KnowledgePanel.tsx`（PanelRetrieval 部分）

**Step 1: PanelRetrieval を実装**

主要 UI 要素:

- **コレクション選択**
- **パターン選択ラジオ**（faq_precision / balanced_knowledge / long_doc_reasoning / custom）
  - パターン選択時に全設定が自動適用されることを視覚的にハイライト
- **詳細設定パネル**（アコーディオン展開）
  - チャンキング:
    - 戦略: ドロップダウン（recursive / sentence / semantic / token / markdown）
    - サイズ: 数値入力 + スライダー（100〜4000）
    - オーバーラップ: 数値入力 + スライダー（0〜500）
  - 検索:
    - 検索方式: ドロップダウン（semantic / hybrid / keyword）
    - リランカー: ドロップダウン（none / bm25 / cohere / cross_encoder / llm_listwise）
    - Top-K: 数値入力 + スライダー（1〜50）
    - 類似度閾値: 数値入力 + スライダー（0.0〜1.0）
  - 各設定値の横に説明ツールチップ
- **テストクエリセクション**
  - クエリ入力テキストエリア
  - 「テスト実行」ボタン
  - 結果表示:
    - ヒット数、平均スコア
    - 各結果のスコア、チャンク内容（折りたたみ）、メタデータ
    - ソースファイル名表示
- **「設定を保存」ボタン**（PATCH /api/collections/{name}）

**Step 2: テスト実行**

Run: `cd apps/faq_system/frontend && npx vitest run`
Expected: PASS

**Step 3: コミット**

```bash
git add apps/faq_system/frontend/src/components/settings/KnowledgePanel.tsx
git commit -m "feat(faq): パネル内検索設定 - チャンク/TopK/検索方式のリアルタイム設定＆テストクエリ"
```

---

## Task 6: バックエンド - 設定変更→RAGService 反映の確実な連携

**目的:** コレクション設定を PATCH した後、RAGService のキャッシュが即座に無効化され、次回クエリから新設定が適用されることを保証する。

**Files:**
- Modify: `apps/faq_system/routers/collections.py`
- Modify: `apps/faq_system/routers/dependencies.py`（`invalidate_service_cache` 確認）
- Create: `apps/faq_system/tests/test_rag_config_reload.py`

**Step 1: テスト作成**

```python
"""RAG 設定変更後のキャッシュ無効化テスト。"""
import pytest
from unittest.mock import AsyncMock, patch


class TestRAGConfigReload:
    """コレクション設定変更時に RAGService が再構築されることを検証。"""

    @pytest.mark.asyncio
    async def test_update_collection_invalidates_cache(self) -> None:
        """PATCH /api/collections/{name} 後に service cache が無効化される。"""
        # invalidate_service_cache が呼ばれることを検証
        pass

    @pytest.mark.asyncio
    async def test_query_uses_updated_config(self) -> None:
        """設定変更後のクエリが新しい top_k 値を使用する。"""
        pass
```

**Step 2: collections.py の update_collection に cache 無効化を追加**

```python
# apps/faq_system/routers/collections.py の update_collection 内
# 既存の CollectionManager.update_collection() 呼び出しの後に追加:
from .dependencies import invalidate_service_cache
invalidate_service_cache()
```

**Step 3: テスト実行**

Run: `cd apps/faq_system && python -m pytest tests/test_rag_config_reload.py -v`
Expected: PASS

**Step 4: コミット**

```bash
git add apps/faq_system/routers/collections.py apps/faq_system/tests/test_rag_config_reload.py
git commit -m "fix(faq): コレクション設定変更時の RAGService キャッシュ無効化を保証"
```

---

## Task 7: パネル内アクセス制御の実装

**目的:** 管理者がコレクションごとの権限マッピングを確認・変更できる UI を実装する。

**Files:**
- Modify: `apps/faq_system/frontend/src/components/settings/KnowledgePanel.tsx`（PanelAccess 部分）
- Create: `apps/faq_system/routers/access_control.py`（権限設定 API）
- Modify: `apps/faq_system/backend/security/permission_config.py`

**Step 1: テスト作成**

Create: `apps/faq_system/tests/test_access_control_api.py`

```python
"""アクセス制御 API のテスト。"""
import pytest


class TestAccessControlAPI:
    """コレクション別アクセス制御のテスト。"""

    @pytest.mark.asyncio
    async def test_get_access_matrix(self) -> None:
        """GET /api/access/matrix がロール×コレクションのマトリクスを返す。"""
        pass

    @pytest.mark.asyncio
    async def test_update_collection_access_requires_admin(self) -> None:
        """権限変更は admin ロールのみ可能。"""
        pass

    @pytest.mark.asyncio
    async def test_guest_cannot_access_internal(self) -> None:
        """guest ロールが internal コレクションにアクセスできないことを検証。"""
        pass
```

**Step 2: アクセス制御 API エンドポイント作成**

Create: `apps/faq_system/routers/access_control.py`

```python
"""コレクション別アクセス制御 API。"""
from __future__ import annotations

from typing import Any

from fastapi import APIRouter, Request

from ..backend.auth.dependencies import require_auth, require_role
from ..backend.security.permission_config import RAGAccessControl

router = APIRouter(prefix="/api/access", tags=["access-control"])


@router.get("/matrix")
@require_auth
async def get_access_matrix(request: Request) -> dict[str, Any]:
    """ロール×KB タイプのアクセスマトリクスを返す。"""
    matrix = RAGAccessControl.get_full_matrix()
    return {"matrix": matrix}


@router.patch("/collections/{collection_name}/roles")
@require_role("admin")
async def update_collection_roles(
    collection_name: str,
    request: Request,
) -> dict[str, Any]:
    """コレクションのアクセス可能ロールを更新する。"""
    body = await request.json()
    allowed_roles: list[str] = body.get("allowed_roles", [])
    RAGAccessControl.update_collection_roles(collection_name, allowed_roles)
    return {"status": "updated", "collection": collection_name, "roles": allowed_roles}
```

**Step 3: PanelAccess UI を実装**

- **ロール×コレクション マトリクス表示**（表形式）
  - 行: ロール（admin, manager, employee, guest）
  - 列: コレクション（internal, external, confidential + カスタム）
  - セル: チェックマーク（アクセス可能）/ ×マーク（不可）
- **管理者向け編集モード**
  - admin ロールの場合のみ「編集」ボタン表示
  - チェックボックスで権限を切り替え→「保存」

**Step 4: テスト実行**

Run: `cd apps/faq_system && python -m pytest tests/test_access_control_api.py -v`
Expected: PASS

**Step 5: コミット**

```bash
git add apps/faq_system/routers/access_control.py apps/faq_system/tests/test_access_control_api.py apps/faq_system/frontend/src/components/settings/KnowledgePanel.tsx apps/faq_system/backend/security/permission_config.py
git commit -m "feat(faq): アクセス制御 API + パネル内権限管理マトリクス UI"
```

---

## Task 8: main.py にルーター登録＆統合テスト

**目的:** 新しい access_control ルーターを main.py に登録し、全体の統合テストを行う。

**Files:**
- Modify: `apps/faq_system/main.py`（access_control ルーター登録）
- Create: `apps/faq_system/tests/test_knowledge_panel_integration.py`

**Step 1: main.py にルーター追加**

```python
# apps/faq_system/main.py の既存ルーター登録箇所に追加
from .routers.access_control import router as access_control_router
app.include_router(access_control_router)
```

**Step 2: 統合テスト作成**

```python
"""ナレッジベース管理の統合テスト。"""
import pytest


class TestKnowledgeManagementIntegration:
    """ナレッジベース管理のE2Eフローテスト。"""

    @pytest.mark.asyncio
    async def test_collection_create_upload_index_query_flow(self) -> None:
        """コレクション作成→ドキュメントアップロード→インデックス→検索のフルフロー。"""
        # 1. コレクション作成（faq_precision パターン）
        # 2. ドキュメントアップロード（auto_index=True）
        # 3. テストクエリ実行
        # 4. 結果にアップロードしたドキュメントの内容が含まれることを検証
        pass

    @pytest.mark.asyncio
    async def test_settings_change_affects_query(self) -> None:
        """設定変更（top_k）が次回クエリに反映されることを検証。"""
        pass
```

**Step 3: テスト実行**

Run: `cd apps/faq_system && python -m pytest tests/ -v`
Expected: PASS

**Step 4: コミット**

```bash
git add apps/faq_system/main.py apps/faq_system/tests/test_knowledge_panel_integration.py
git commit -m "feat(faq): access_control ルーター登録 + ナレッジ管理統合テスト"
```

---

## Task 9: i18n 対応（日本語・中国語・英語）

**目的:** KnowledgePanel で使用する全テキストを i18n キーに置換する。

**Files:**
- Modify: `apps/faq_system/frontend/src/i18n/` 内の各ロケールファイル
- Modify: `apps/faq_system/frontend/src/components/settings/KnowledgePanel.tsx`

**Step 1: i18n キーを追加**

```json
{
  "knowledge_panel": {
    "title": "ナレッジベース管理",
    "tab_collections": "コレクション",
    "tab_documents": "ドキュメント",
    "tab_retrieval": "検索設定",
    "tab_access": "アクセス",
    "new_collection": "新規コレクション",
    "pattern_faq_precision": "FAQ 高精度",
    "pattern_balanced": "バランス型",
    "pattern_long_doc": "長文推論",
    "pattern_custom": "カスタム",
    "chunk_strategy": "チャンク戦略",
    "chunk_size": "チャンクサイズ",
    "chunk_overlap": "オーバーラップ",
    "retrieval_method": "検索方式",
    "reranker": "リランカー",
    "top_k": "Top-K",
    "min_similarity": "類似度閾値",
    "test_query": "テストクエリ",
    "run_test": "テスト実行",
    "save_settings": "設定を保存",
    "auto_index": "アップロード後に自動インデックス",
    "upload_area": "ファイルをドラッグ＆ドロップ",
    "supported_formats": "対応形式: PDF, DOCX, CSV, TXT, MD, JSON",
    "access_matrix": "アクセスマトリクス",
    "role": "ロール",
    "allowed": "許可",
    "denied": "拒否"
  }
}
```

**Step 2: KnowledgePanel 内のハードコード文字列を t() に置換**

**Step 3: テスト実行**

Run: `cd apps/faq_system/frontend && npx vitest run`
Expected: PASS

**Step 4: コミット**

```bash
git add apps/faq_system/frontend/src/i18n/ apps/faq_system/frontend/src/components/settings/KnowledgePanel.tsx
git commit -m "feat(faq): ナレッジベース管理パネルの i18n 対応"
```

---

## Task 10: 品質チェック＆最終確認

**目的:** 全体の lint / type-check / テスト / ビルドを通す。

**Step 1: Python チェック**

Run: `ruff format apps/faq_system/ && ruff check apps/faq_system/ --fix`
Run: `cd apps/faq_system && python -m pytest tests/ -v --tb=short`

**Step 2: フロントエンドチェック**

Run: `cd apps/faq_system/frontend && npx tsc --noEmit && npm run lint && npm run build`
Run: `cd apps/faq_system/frontend && npx vitest run`

**Step 3: 動作確認**

1. `python -m apps.faq_system.main serve --port 8002` でバックエンド起動
2. `cd apps/faq_system/frontend && npm run dev` でフロントエンド起動
3. ブラウザで以下を確認:
   - チャット画面右上に Database アイコンが表示される
   - クリックで右パネルがスライドインする
   - コレクション一覧が表示される
   - 新規コレクション作成→パターンプリセット選択→設定が自動入力
   - ドキュメントアップロード→自動インデックス→ステータスが indexed に変わる
   - テストクエリで検索結果が返る
   - アクセスマトリクスが表示される

**Step 4: 最終コミット**

```bash
git add -A
git commit -m "chore(faq): ナレッジベース管理パネル - lint/format/build 修正"
```

---

## アーキテクチャ概要図

```
┌───────────────────────────────────────────────────┐
│  ChatWindow                                        │
│  ┌─────────┐  ┌──────────────┐  ┌──────────────┐ │
│  │ Header  │  │ Chat Area    │  │ Knowledge    │ │
│  │ 📊 ⚙   │  │              │  │ Panel (480px)│ │
│  └─────────┘  │              │  │ ┌──────────┐ │ │
│               │              │  │ │Collection│ │ │
│               │              │  │ │Documents │ │ │
│               │              │  │ │Retrieval │ │ │
│               │              │  │ │Access    │ │ │
│               │              │  │ └──────────┘ │ │
│               │              │  └──────────────┘ │
│  ┌─────────────────────────┐                      │
│  │ Input Area              │                      │
│  └─────────────────────────┘                      │
└───────────────────────────────────────────────────┘

データフロー:
ファイル → アップロード → チャンキング → ベクトルDB → 検索 → 回答
         ↕               ↕                ↕           ↕
      auto_index    chunk_size/strategy  Qdrant    top_k/reranker
                                         ↕
                                    権限チェック (RBAC)
```
