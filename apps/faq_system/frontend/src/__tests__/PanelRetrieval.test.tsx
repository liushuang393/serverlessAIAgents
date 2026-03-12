/**
 * PanelRetrieval コンポーネントのテスト.
 *
 * コレクション選択での設定読み込み、パターンプリセット適用、
 * テストクエリ実行を検証する。
 */
import { describe, it, expect, vi, beforeEach } from 'vitest';
import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { PanelRetrieval } from '../components/settings/PanelRetrieval';
import type { CollectionInfo } from '../api/rag';

// ---------------------------------------------------------------------------
// ragStore モック
// ---------------------------------------------------------------------------

const mockFetchCollections = vi.fn().mockResolvedValue(undefined);
const mockUpdateCollection = vi.fn().mockResolvedValue({} as CollectionInfo);

let mockStoreState: Record<string, unknown> = {};

vi.mock('../stores/ragStore', () => ({
  useRAGStore: () => mockStoreState,
}));

// ---------------------------------------------------------------------------
// ragApi モック
// ---------------------------------------------------------------------------

const mockTestQuery = vi.fn();

vi.mock('../api/rag', async () => {
  const actual = await vi.importActual<typeof import('../api/rag')>('../api/rag');
  return {
    ...actual,
    ragApi: {
      ...actual.ragApi,
      testQuery: (...args: unknown[]) => mockTestQuery(...args),
    },
  };
});

// ---------------------------------------------------------------------------
// ヘルパー
// ---------------------------------------------------------------------------

const MOCK_COLLECTIONS: CollectionInfo[] = [
  {
    id: 1,
    collection_name: 'faq_main',
    app_name: 'faq_system',
    tenant_id: null,
    display_name: 'FAQ メイン',
    description: 'メインFAQコレクション',
    chunk_strategy: 'recursive',
    chunk_size: 800,
    chunk_overlap: 120,
    embedding_model: null,
    retrieval_method: 'hybrid',
    reranker: 'bm25',
    top_k: 6,
    min_similarity: 0.2,
    vector_db_type: null,
    vector_db_url: null,
    document_count: 42,
    last_indexed_at: null,
    created_at: null,
    updated_at: null,
  },
  {
    id: 2,
    collection_name: 'docs_sub',
    app_name: 'faq_system',
    tenant_id: null,
    display_name: 'ドキュメントサブ',
    description: 'サブコレクション',
    chunk_strategy: 'sentence',
    chunk_size: 500,
    chunk_overlap: 80,
    embedding_model: null,
    retrieval_method: 'semantic',
    reranker: null,
    top_k: 5,
    min_similarity: 0.15,
    vector_db_type: null,
    vector_db_url: null,
    document_count: 10,
    last_indexed_at: null,
    created_at: null,
    updated_at: null,
  },
];

function setupStore(overrides: Partial<typeof mockStoreState> = {}) {
  mockStoreState = {
    collections: MOCK_COLLECTIONS,
    collectionsLoading: false,
    fetchCollections: mockFetchCollections,
    updateCollection: mockUpdateCollection,
    selectedCollection: null,
    ...overrides,
  };
}

// ---------------------------------------------------------------------------
// テスト
// ---------------------------------------------------------------------------

describe('PanelRetrieval', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    setupStore();
  });

  // 1. コレクション選択で設定値が読み込まれる
  it('コレクション選択で設定値が読み込まれる', async () => {
    const user = userEvent.setup();
    render(<PanelRetrieval />);

    // コレクションを選択
    const select = screen.getByTestId('select-collection');
    await user.selectOptions(select, 'faq_main');

    // faq_main の設定値が表示される
    expect(screen.getByTestId('chunk-size-value')).toHaveTextContent('800');
    expect(screen.getByTestId('chunk-overlap-value')).toHaveTextContent('120');
    expect(screen.getByTestId('top-k-value')).toHaveTextContent('6');
    expect(screen.getByTestId('min-similarity-value')).toHaveTextContent('0.20');
    expect(screen.getByTestId('select-chunk-strategy')).toHaveValue('recursive');
    expect(screen.getByTestId('select-retrieval-method')).toHaveValue('hybrid');
    expect(screen.getByTestId('select-reranker')).toHaveValue('bm25');
  });

  // 2. パターン選択で設定値が自動適用される
  it('パターン選択で設定値が自動適用される', async () => {
    const user = userEvent.setup();
    render(<PanelRetrieval />);

    // まずコレクションを選択（プリセットは選択後に表示される）
    await user.selectOptions(screen.getByTestId('select-collection'), 'faq_main');

    // FAQ 高精度プリセットを選択
    await user.click(screen.getByTestId('preset-faq_precision'));

    // プリセット値が反映される
    expect(screen.getByTestId('chunk-size-value')).toHaveTextContent('500');
    expect(screen.getByTestId('chunk-overlap-value')).toHaveTextContent('80');
    expect(screen.getByTestId('top-k-value')).toHaveTextContent('8');
    expect(screen.getByTestId('min-similarity-value')).toHaveTextContent('0.25');
    expect(screen.getByTestId('select-chunk-strategy')).toHaveValue('sentence');
    expect(screen.getByTestId('select-retrieval-method')).toHaveValue('hybrid');
    expect(screen.getByTestId('select-reranker')).toHaveValue('cohere');
  });

  // 3. テスト実行ボタンクリックで API が呼ばれる
  it('テスト実行ボタンクリックで API が呼ばれる', async () => {
    mockTestQuery.mockResolvedValue({
      results: [
        { score: 0.85, content: 'テスト結果のコンテンツ', source: 'doc1.pdf' },
      ],
      total: 1,
    });

    const user = userEvent.setup();
    render(<PanelRetrieval />);

    // コレクションを選択
    await user.selectOptions(screen.getByTestId('select-collection'), 'faq_main');

    // クエリを入力
    const queryInput = screen.getByTestId('test-query-input');
    await user.type(queryInput, 'テストクエリ');

    // テスト実行ボタンをクリック
    await user.click(screen.getByTestId('btn-test-query'));

    // API が正しい引数で呼ばれる
    await waitFor(() => {
      expect(mockTestQuery).toHaveBeenCalledWith('faq_main', 'テストクエリ', 6);
    });

    // 結果が表示される
    await waitFor(() => {
      expect(screen.getByTestId('test-results')).toBeInTheDocument();
      expect(screen.getByTestId('hit-count')).toHaveTextContent('1');
    });
  });

  // 4. 設定保存ボタンで updateCollection が呼ばれる
  it('設定保存ボタンで updateCollection が呼ばれる', async () => {
    const user = userEvent.setup();
    render(<PanelRetrieval />);

    // コレクションを選択
    await user.selectOptions(screen.getByTestId('select-collection'), 'faq_main');

    // 保存ボタンをクリック
    await user.click(screen.getByTestId('btn-save-settings'));

    await waitFor(() => {
      expect(mockUpdateCollection).toHaveBeenCalledWith('faq_main', {
        chunk_strategy: 'recursive',
        chunk_size: 800,
        chunk_overlap: 120,
        retrieval_method: 'hybrid',
        reranker: 'bm25',
        top_k: 6,
        min_similarity: 0.2,
      });
    });
  });
});
