/**
 * PanelDocuments コンポーネントのテスト.
 *
 * コレクション選択、ステータスバッジ表示、削除操作を検証する。
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { PanelDocuments } from "../components/settings/PanelDocuments";
import type { CollectionInfo, DocumentInfo, ChunkPreview } from "../api/rag";

// ---------------------------------------------------------------------------
// ragStore モック
// ---------------------------------------------------------------------------

const mockFetchCollections = vi.fn().mockResolvedValue(undefined);
const mockFetchDocuments = vi.fn().mockResolvedValue(undefined);
const mockUploadDocument = vi.fn().mockResolvedValue({} as DocumentInfo);
const mockDeleteDocument = vi.fn().mockResolvedValue(undefined);
const mockIndexDocument = vi.fn().mockResolvedValue(undefined);
const mockReindexDocument = vi.fn().mockResolvedValue(undefined);
const mockPreviewChunks = vi.fn().mockResolvedValue(undefined);

let mockStoreState: Record<string, unknown> = {};

vi.mock("../stores/ragStore", () => ({
  useRAGStore: () => mockStoreState,
}));

// ---------------------------------------------------------------------------
// テストデータ
// ---------------------------------------------------------------------------

const MOCK_COLLECTIONS: CollectionInfo[] = [
  {
    id: 1,
    collection_name: "faq_main",
    app_name: "faq_system",
    tenant_id: null,
    display_name: "FAQ メイン",
    description: "",
    chunk_strategy: "recursive",
    chunk_size: 800,
    chunk_overlap: 120,
    embedding_model: null,
    retrieval_method: "hybrid",
    reranker: null,
    top_k: 5,
    min_similarity: 0.3,
    vector_db_type: null,
    vector_db_url: null,
    document_count: 3,
    last_indexed_at: null,
    created_at: null,
    updated_at: null,
  },
];

const MOCK_DOCUMENTS: DocumentInfo[] = [
  {
    id: 1,
    document_id: "doc-001",
    collection_name: "faq_main",
    filename: "guide.pdf",
    file_type: "pdf",
    file_size: 51200,
    status: "uploaded",
    chunk_count: 0,
    content_hash: "abc123",
    document_group_id: null,
    tags: [],
    uploaded_by: null,
    uploaded_at: null,
    indexed_at: null,
    error_message: null,
  },
  {
    id: 2,
    document_id: "doc-002",
    collection_name: "faq_main",
    filename: "faq.md",
    file_type: "md",
    file_size: 2048,
    status: "indexed",
    chunk_count: 12,
    content_hash: "def456",
    document_group_id: "group-001",
    tags: ["faq", "markdown"],
    uploaded_by: null,
    uploaded_at: null,
    indexed_at: "2026-01-01",
    error_message: null,
  },
  {
    id: 3,
    document_id: "doc-003",
    collection_name: "faq_main",
    filename: "broken.csv",
    file_type: "csv",
    file_size: 1024,
    status: "error",
    chunk_count: 0,
    content_hash: "ghi789",
    document_group_id: null,
    tags: ["csv"],
    uploaded_by: null,
    uploaded_at: null,
    indexed_at: null,
    error_message: "Parse error",
  },
];

// ---------------------------------------------------------------------------
// ヘルパー
// ---------------------------------------------------------------------------

function setupStore(overrides: Partial<typeof mockStoreState> = {}) {
  mockStoreState = {
    collections: MOCK_COLLECTIONS,
    fetchCollections: mockFetchCollections,
    documents: [],
    documentsLoading: false,
    fetchDocuments: mockFetchDocuments,
    uploadDocument: mockUploadDocument,
    deleteDocument: mockDeleteDocument,
    indexDocument: mockIndexDocument,
    reindexDocument: mockReindexDocument,
    chunkPreviews: [] as ChunkPreview[],
    chunksLoading: false,
    previewChunks: mockPreviewChunks,
    ...overrides,
  };
}

// ---------------------------------------------------------------------------
// テスト
// ---------------------------------------------------------------------------

describe("PanelDocuments", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.stubGlobal("crypto", {
      randomUUID: vi.fn().mockReturnValue("group-hr-travel-001"),
    });
    setupStore();
  });

  it("コレクション選択で fetchDocuments が呼ばれる", async () => {
    const user = userEvent.setup();
    setupStore({ documents: MOCK_DOCUMENTS });
    const { rerender } = render(<PanelDocuments />);

    // コレクションドロップダウンを選択
    const select = screen.getByTestId("collection-select");
    await user.selectOptions(select, "faq_main");

    // store 更新をシミュレートして再レンダリング
    setupStore({ documents: MOCK_DOCUMENTS });
    rerender(<PanelDocuments />);

    expect(mockFetchDocuments).toHaveBeenCalledWith("faq_main");
  });

  it("ドキュメント一覧にステータスバッジが表示される", async () => {
    const user = userEvent.setup();
    setupStore({ documents: MOCK_DOCUMENTS });
    const { rerender } = render(<PanelDocuments />);

    // コレクション選択
    await user.selectOptions(
      screen.getByTestId("collection-select"),
      "faq_main",
    );
    setupStore({ documents: MOCK_DOCUMENTS });
    rerender(<PanelDocuments />);

    // ドキュメントが表示されている
    const list = screen.getByTestId("document-list");
    expect(within(list).getByText("guide.pdf")).toBeInTheDocument();
    expect(within(list).getByText("faq.md")).toBeInTheDocument();
    expect(within(list).getByText("broken.csv")).toBeInTheDocument();

    // ステータスバッジが正しい
    const uploadedBadge = screen.getByTestId("status-badge-doc-001");
    expect(uploadedBadge).toHaveTextContent("uploaded");
    expect(uploadedBadge.className).toContain("bg-amber-500/20");

    const indexedBadge = screen.getByTestId("status-badge-doc-002");
    expect(indexedBadge).toHaveTextContent("indexed");
    expect(indexedBadge.className).toContain("bg-emerald-500/20");

    const errorBadge = screen.getByTestId("status-badge-doc-003");
    expect(errorBadge).toHaveTextContent("error");
    expect(errorBadge.className).toContain("bg-rose-500/20");
  });

  it("削除ボタンで確認後 deleteDocument が呼ばれる", async () => {
    const user = userEvent.setup();
    setupStore({ documents: MOCK_DOCUMENTS });
    const { rerender } = render(<PanelDocuments />);

    // コレクション選択
    await user.selectOptions(
      screen.getByTestId("collection-select"),
      "faq_main",
    );
    setupStore({ documents: MOCK_DOCUMENTS });
    rerender(<PanelDocuments />);

    // window.confirm をモック
    vi.spyOn(window, "confirm").mockReturnValue(true);

    // 削除ボタンをクリック
    const deleteBtn = screen.getByTestId("btn-delete-doc-001");
    await user.click(deleteBtn);

    expect(window.confirm).toHaveBeenCalled();
    expect(mockDeleteDocument).toHaveBeenCalledWith("faq_main", "doc-001");
  });

  it("複数ファイルのアップロード時に同じ group と metadata を渡す", async () => {
    const user = userEvent.setup();
    render(<PanelDocuments />);

    await user.selectOptions(
      screen.getByTestId("collection-select"),
      "faq_main",
    );

    await user.type(screen.getByTestId("scenario-id-input"), "hr-travel");
    await user.type(screen.getByTestId("tag-input"), "policy, travel");

    const input = screen.getByTestId("file-input");
    const first = new File(["policy"], "policy.pdf", { type: "application/pdf" });
    const second = new File(["faq"], "faq.docx", {
      type: "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    });
    await user.upload(input, [first, second]);

    expect(mockUploadDocument).toHaveBeenNthCalledWith(
      1,
      "faq_main",
      first,
      false,
      {
        document_group_id: "group-hr-travel-001",
        scenario_id: "hr-travel",
        tags: ["policy", "travel"],
      },
    );
    expect(mockUploadDocument).toHaveBeenNthCalledWith(
      2,
      "faq_main",
      second,
      false,
      {
        document_group_id: "group-hr-travel-001",
        scenario_id: "hr-travel",
        tags: ["policy", "travel"],
      },
    );
  });
});
