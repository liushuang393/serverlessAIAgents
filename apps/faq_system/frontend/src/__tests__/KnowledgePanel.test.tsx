/**
 * KnowledgePanel コンポーネントのテスト.
 *
 * パネルの開閉状態、タブ表示、閉じるボタンの動作を検証する。
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { KnowledgePanel } from "../components/settings/KnowledgePanel";

// ---------------------------------------------------------------------------
// ragStore モック: KnowledgePanel が useRAGStore を利用するため
// ---------------------------------------------------------------------------
const mockSetActiveTab = vi.fn();

vi.mock("../stores/ragStore", () => ({
  useRAGStore: () => ({
    activeTab: "dashboard",
    setActiveTab: mockSetActiveTab,
    collections: [],
    collectionsLoading: false,
    fetchCollections: vi.fn().mockResolvedValue(undefined),
    ingestRuns: [],
    fetchIngestRuns: vi.fn().mockResolvedValue(undefined),
  }),
}));

describe("KnowledgePanel", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });
  /** パネルが閉じている時に translate-x-full クラスがある */
  it("閉じている時に translate-x-full クラスを持つ", () => {
    render(<KnowledgePanel isOpen={false} onClose={vi.fn()} />);
    const panel = screen.getByTestId("knowledge-panel");
    expect(panel.className).toContain("translate-x-full");
  });

  /** パネルが開いている時にタブ名が表示される */
  it("開いている時にタブ名が表示される", () => {
    render(<KnowledgePanel isOpen={true} onClose={vi.fn()} />);
    expect(
      screen.getByText("knowledge_panel.tab_dashboard"),
    ).toBeInTheDocument();
    expect(
      screen.getByText("knowledge_panel.tab_collections"),
    ).toBeInTheDocument();
    expect(
      screen.getByText("knowledge_panel.tab_documents"),
    ).toBeInTheDocument();
    expect(screen.getByText("knowledge_panel.tab_ingest")).toBeInTheDocument();
    expect(
      screen.getByText("knowledge_panel.tab_retrieval"),
    ).toBeInTheDocument();
  });

  /** 閉じるボタンクリックで onClose が呼ばれる */
  it("閉じるボタンクリックで onClose が呼ばれる", async () => {
    const onClose = vi.fn();
    render(<KnowledgePanel isOpen={true} onClose={onClose} />);
    const closeButton = screen.getByRole("button", {
      name: "knowledge_panel.close",
    });
    await userEvent.click(closeButton);
    expect(onClose).toHaveBeenCalledTimes(1);
  });
});
