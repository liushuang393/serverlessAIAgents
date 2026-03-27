/**
 * PanelCollections コンポーネントのテスト.
 *
 * コレクション一覧表示、カード操作、フォーム表示、プリセット適用、
 * 保存操作、バリデーションを検証する。
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { PanelCollections } from "../components/settings/PanelCollections";
import type { CollectionInfo } from "../api/rag";

// ---------------------------------------------------------------------------
// ragStore モック
// ---------------------------------------------------------------------------

const mockFetchCollections = vi.fn().mockResolvedValue(undefined);
const mockCreateCollection = vi.fn().mockResolvedValue({} as CollectionInfo);
const mockUpdateCollection = vi.fn().mockResolvedValue({} as CollectionInfo);
const mockSelectCollection = vi.fn();

let mockStoreState: Record<string, unknown> = {};

vi.mock("../stores/ragStore", () => ({
  useRAGStore: () => mockStoreState,
}));

// ---------------------------------------------------------------------------
// ヘルパー
// ---------------------------------------------------------------------------

const MOCK_COLLECTIONS: CollectionInfo[] = [
  {
    id: 1,
    collection_name: "faq_main",
    app_name: "faq_system",
    tenant_id: null,
    display_name: "FAQ メイン",
    description: "メインFAQコレクション",
    chunk_strategy: "recursive",
    chunk_size: 800,
    chunk_overlap: 120,
    embedding_model: null,
    retrieval_method: "hybrid",
    reranker: "bm25",
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
    collection_name: "docs_sub",
    app_name: "faq_system",
    tenant_id: null,
    display_name: "ドキュメントサブ",
    description: "サブコレクション",
    chunk_strategy: "sentence",
    chunk_size: 500,
    chunk_overlap: 80,
    embedding_model: null,
    retrieval_method: "semantic",
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
    createCollection: mockCreateCollection,
    updateCollection: mockUpdateCollection,
    selectCollection: mockSelectCollection,
    selectedCollection: null,
    ...overrides,
  };
}

// ---------------------------------------------------------------------------
// テスト
// ---------------------------------------------------------------------------

describe("PanelCollections", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    setupStore();
  });

  // 1. モックデータからコレクションカードが表示される
  it("モックデータからコレクションカードが表示される", () => {
    render(<PanelCollections />);

    const cardList = screen.getByTestId("collection-card-list");
    expect(
      within(cardList).getByTestId("collection-card-faq_main"),
    ).toBeInTheDocument();
    expect(
      within(cardList).getByTestId("collection-card-docs_sub"),
    ).toBeInTheDocument();

    // display_name が表示されている
    expect(screen.getByText("FAQ メイン")).toBeInTheDocument();
    expect(screen.getByText("ドキュメントサブ")).toBeInTheDocument();

    // メタデータバッジ
    expect(screen.getByText("42 docs")).toBeInTheDocument();
    expect(screen.getByText("recursive")).toBeInTheDocument();
    expect(screen.getByText("hybrid")).toBeInTheDocument();
  });

  // 2. カードクリックで編集フォームが展開
  it("カードクリックで編集フォームが展開される", async () => {
    // selectCollection が呼ばれた後、selectedCollection が更新される動作をシミュレート
    mockSelectCollection.mockImplementation((name: string | null) => {
      mockStoreState.selectedCollection = name;
    });

    const user = userEvent.setup();
    const { rerender } = render(<PanelCollections />);

    // フォームはまだ表示されていない
    expect(
      screen.queryByTestId("panel-collection-form"),
    ).not.toBeInTheDocument();

    // カードをクリック
    await user.click(screen.getByTestId("collection-card-faq_main"));

    // selectCollection が呼ばれた
    expect(mockSelectCollection).toHaveBeenCalledWith("faq_main");

    // store の selectedCollection が更新されたので再レンダリング
    setupStore({ selectedCollection: "faq_main" });
    rerender(<PanelCollections />);

    // 編集フォームが展開されている
    expect(screen.getByTestId("panel-collection-form")).toBeInTheDocument();

    // 編集モードでは collection_name 入力フィールドは非表示
    expect(
      screen.queryByTestId("input-collection-name"),
    ).not.toBeInTheDocument();
  });

  // 3. 「新規コレクション」ボタンで作成フォーム表示（name フィールドあり）
  it("「新規コレクション」ボタンで作成フォーム表示（name フィールドあり）", async () => {
    const user = userEvent.setup();
    render(<PanelCollections />);

    await user.click(screen.getByTestId("btn-new-collection"));

    // フォームが表示
    expect(screen.getByTestId("panel-collection-form")).toBeInTheDocument();

    // 作成モードでは collection_name 入力フィールドがある
    expect(screen.getByTestId("input-collection-name")).toBeInTheDocument();
    expect(screen.getByTestId("input-display-name")).toBeInTheDocument();
    expect(screen.getByTestId("input-description")).toBeInTheDocument();
  });

  // 4. プリセット選択で正しい値がフォームに適用される
  it("プリセット選択で正しい値がフォームに適用される", async () => {
    const user = userEvent.setup();
    render(<PanelCollections />);

    // 作成フォームを開く
    await user.click(screen.getByTestId("btn-new-collection"));

    // 「FAQ 高精度」プリセットを選択
    await user.click(screen.getByTestId("preset-faq_precision"));

    // プリセット値が反映されている
    expect(screen.getByTestId("chunk-size-value")).toHaveTextContent("500");
    expect(screen.getByTestId("chunk-overlap-value")).toHaveTextContent("80");
    expect(screen.getByTestId("top-k-value")).toHaveTextContent("8");
    expect(screen.getByTestId("min-similarity-value")).toHaveTextContent(
      "0.25",
    );

    // select 要素の値
    expect(screen.getByTestId("select-chunk-strategy")).toHaveValue("sentence");
    expect(screen.getByTestId("select-retrieval-method")).toHaveValue("hybrid");
    expect(screen.getByTestId("select-reranker")).toHaveValue("cohere");
  });

  // 5. 保存ボタンが create モードで createCollection を呼ぶ
  it("保存ボタンが create モードで createCollection を呼ぶ", async () => {
    const user = userEvent.setup({ delay: null });
    render(<PanelCollections />);

    // 作成フォームを開く
    await user.click(screen.getByTestId("btn-new-collection"));

    // collection_name を入力
    await user.type(
      screen.getByTestId("input-collection-name"),
      "new_collection",
    );
    await user.type(screen.getByTestId("input-display-name"), "New Collection");

    // 保存ボタンをクリック
    await user.click(screen.getByTestId("btn-save-collection"));

    expect(mockCreateCollection).toHaveBeenCalledTimes(1);
    const callArg = mockCreateCollection.mock.calls[0][0];
    expect(callArg.collection_name).toBe("new_collection");
    expect(callArg.display_name).toBe("New Collection");
  });

  // 6. collection_name が空の時に保存ボタンが disabled
  it("collection_name が空の時に保存ボタンが disabled", async () => {
    const user = userEvent.setup();
    render(<PanelCollections />);

    // 作成フォームを開く
    await user.click(screen.getByTestId("btn-new-collection"));

    // collection_name は空のまま
    const saveBtn = screen.getByTestId("btn-save-collection");
    expect(saveBtn).toBeDisabled();

    // 入力すると enabled になる
    await user.type(screen.getByTestId("input-collection-name"), "test");
    expect(saveBtn).not.toBeDisabled();
  });
});
