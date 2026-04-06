/**
 * FAQ System - 多文書アップロード / 検索検証 E2E.
 *
 * Playwright 設定で frontend(3004) / backend(8005) を起動する前提。
 */
import { expect, test } from "@playwright/test";

const BUNDLE_DIR = "../tests/fixtures/knowledge_samples/hr_travel_bundle";
const BUNDLE_FILES = [
  "travel_policy_official_2025.pdf",
  "travel_faq_exceptions.docx",
  "travel_allowance_matrix.xlsx",
  "travel_policy_update_notice_2026.txt",
].map((filename) => `${BUNDLE_DIR}/${filename}`);

async function login(page): Promise<void> {
  await page.goto("/login");
  await page.locator("#login-username").fill("admin");
  await page.locator("#login-password").fill("admin123");
  await page.getByRole("button", { name: /sign in|ログイン/i }).click();
  await page.waitForURL("/");
}

async function createCollection(page, collectionName: string): Promise<void> {
  await page.evaluate(async ({ collectionName }) => {
    const token = window.localStorage.getItem("access_token");
    if (!token) {
      throw new Error("access_token is missing");
    }

    const response = await window.fetch("/api/collections", {
      method: "POST",
      headers: {
        Authorization: `Bearer ${token}`,
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        collection_name: collectionName,
        display_name: "HR Travel Sample",
        description: "Multi-document HR travel reimbursement sample",
        chunk_strategy: "sentence",
        chunk_size: 500,
        chunk_overlap: 80,
        retrieval_method: "hybrid",
        reranker: "bm25",
        top_k: 8,
        min_similarity: 0.15,
      }),
    });

    if (!response.ok && response.status !== 409) {
      throw new Error(await response.text());
    }
  }, { collectionName });
}

async function deleteCollection(page, collectionName: string): Promise<void> {
  await page.evaluate(async ({ collectionName }) => {
    const token = window.localStorage.getItem("access_token");
    if (!token) {
      return;
    }

    await window.fetch(`/api/collections/${collectionName}`, {
      method: "DELETE",
      headers: {
        Authorization: `Bearer ${token}`,
      },
    });
  }, { collectionName });
}

async function openKnowledgePanel(page): Promise<void> {
  await page.getByTestId("btn-open-knowledge-panel").click();
  await expect(page.getByTestId("knowledge-panel")).toBeVisible();
}

test.describe("FAQ System - RAG 管理 UI E2E", () => {
  let collectionName = "";

  test.beforeEach(async ({ page }) => {
    collectionName = `e2e_hr_travel_${Date.now()}`;
    await login(page);
    await createCollection(page, collectionName);
  });

  test.afterEach(async ({ page }) => {
    await deleteCollection(page, collectionName);
  });

  test("ログイン後に sample bundle をアップロードし、関連文書をまたぐ検索結果を確認できる", async ({
    page,
  }) => {
    test.slow();

    await openKnowledgePanel(page);

    await page.getByTestId("knowledge-tab-documents").click();
    await expect(page.getByTestId("panel-documents")).toBeVisible();

    await page.getByTestId("collection-select").selectOption(collectionName);
    await page.getByTestId("scenario-id-input").fill("hr_travel_policy_2026");
    await page.getByTestId("tag-input").fill("travel, policy, reimbursement");
    await page.getByTestId("auto-index-checkbox").check();
    await page.getByTestId("file-input").setInputFiles(BUNDLE_FILES);

    await expect(page.getByTestId("batch-group-notice")).toBeVisible();
    await expect.poll(async () => {
      return page.locator('[data-testid^="doc-item-"]').count();
    }, { timeout: 180000 }).toBe(4);

    await expect.poll(async () => {
      const texts = await page
        .locator('[data-testid^="status-badge-"]')
        .allTextContents();
      return texts.length === 4 && texts.every((value) => value.trim() === "indexed");
    }, { timeout: 120000 }).toBeTruthy();

    await expect(page.getByTestId("document-list")).toContainText(
      "travel_policy_update_notice_2026.txt",
    );
    await expect(page.getByTestId("document-list")).toContainText("travel");

    await page.getByTestId("knowledge-tab-retrieval").click();
    await expect(page.getByTestId("panel-retrieval")).toBeVisible();

    await page.getByTestId("select-collection").selectOption(collectionName);
    await page
      .getByTestId("test-query-input")
      .fill(
        "What changed between the baseline 2025 matrix and the 2026 notice for a manager staying overnight in Tokyo?",
      );
    await page.getByTestId("btn-test-query").click();

    await expect(page.getByTestId("test-results")).toBeVisible({ timeout: 120000 });
    await expect(page.getByTestId("hit-count")).not.toHaveText("0");
    await expect(page.getByTestId("test-results")).toContainText(
      "travel_allowance_matrix.xlsx",
    );
    await expect(page.getByTestId("test-results")).toContainText(
      "travel_policy_update_notice_2026.txt",
    );
  });
});
