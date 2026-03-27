/**
 * PanelAccess コンポーネントのテスト.
 *
 * アクセスマトリクスの表示、ロール情報の表示、
 * admin 用編集ボタンの表示を検証する。
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen, waitFor } from "@testing-library/react";
import { PanelAccess } from "../components/settings/PanelAccess";

// ---------------------------------------------------------------------------
// ragApi モック
// ---------------------------------------------------------------------------

const mockGetAccessMatrix = vi.fn();

vi.mock("../api/rag", async () => {
  const actual =
    await vi.importActual<typeof import("../api/rag")>("../api/rag");
  return {
    ...actual,
    ragApi: {
      ...actual.ragApi,
      getAccessMatrix: (...args: unknown[]) => mockGetAccessMatrix(...args),
    },
  };
});

// ---------------------------------------------------------------------------
// localStorage モック
// ---------------------------------------------------------------------------

const localStorageMock = (() => {
  let store: Record<string, string> = {};
  return {
    getItem: vi.fn((key: string) => store[key] ?? null),
    setItem: vi.fn((key: string, value: string) => {
      store[key] = value;
    }),
    removeItem: vi.fn((key: string) => {
      delete store[key];
    }),
    clear: vi.fn(() => {
      store = {};
    }),
    get length() {
      return Object.keys(store).length;
    },
    key: vi.fn((_index: number) => null),
  };
})();

Object.defineProperty(window, "localStorage", { value: localStorageMock });

// ---------------------------------------------------------------------------
// テストデータ
// ---------------------------------------------------------------------------

const MOCK_MATRIX = {
  admin: { internal: true, external: true, confidential: true },
  manager: { internal: true, external: true, confidential: true },
  employee: { internal: true, external: true, confidential: false },
  guest: { internal: false, external: true, confidential: false },
};

// ---------------------------------------------------------------------------
// テスト
// ---------------------------------------------------------------------------

describe("PanelAccess", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    localStorageMock.clear();
    mockGetAccessMatrix.mockResolvedValue({ matrix: MOCK_MATRIX });
  });

  it("マトリクスが表形式で表示される", async () => {
    localStorageMock.setItem("user_info", JSON.stringify({ role: "employee" }));

    render(<PanelAccess />);

    await waitFor(() => {
      expect(screen.getByTestId("access-matrix-table")).toBeInTheDocument();
    });

    // ロール名がテーブル内に表示される（current-role 表示と重複しうるので getAllByText を使用）
    expect(screen.getAllByText("admin").length).toBeGreaterThanOrEqual(1);
    expect(screen.getAllByText("manager").length).toBeGreaterThanOrEqual(1);
    expect(screen.getAllByText("employee").length).toBeGreaterThanOrEqual(1);
    expect(screen.getAllByText("guest").length).toBeGreaterThanOrEqual(1);

    // KB タイプヘッダーが表示される
    expect(screen.getByText("internal")).toBeInTheDocument();
    expect(screen.getByText("external")).toBeInTheDocument();
    expect(screen.getByText("confidential")).toBeInTheDocument();
  });

  it("現在のユーザーロールが表示される", async () => {
    localStorageMock.setItem("user_info", JSON.stringify({ role: "manager" }));

    render(<PanelAccess />);

    await waitFor(() => {
      expect(screen.getByTestId("current-role")).toHaveTextContent("manager");
    });
  });

  it("admin ロールの場合のみ編集ボタンが表示される", async () => {
    localStorageMock.setItem("user_info", JSON.stringify({ role: "admin" }));

    render(<PanelAccess />);

    await waitFor(() => {
      expect(screen.getByTestId("btn-edit-access")).toBeInTheDocument();
    });
  });

  it("admin 以外のロールでは編集ボタンが非表示", async () => {
    localStorageMock.setItem("user_info", JSON.stringify({ role: "employee" }));

    render(<PanelAccess />);

    await waitFor(() => {
      expect(screen.getByTestId("access-matrix-table")).toBeInTheDocument();
    });

    expect(screen.queryByTestId("btn-edit-access")).not.toBeInTheDocument();
  });

  it("API エラー時にエラーメッセージを表示する", async () => {
    mockGetAccessMatrix.mockRejectedValue(new Error("Network error"));

    render(<PanelAccess />);

    await waitFor(() => {
      expect(screen.getByTestId("panel-access-error")).toBeInTheDocument();
    });
  });
});
