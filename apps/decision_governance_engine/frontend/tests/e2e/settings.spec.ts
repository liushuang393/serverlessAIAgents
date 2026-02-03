import { expect, test } from "@playwright/test";

import { installErrorMonitor } from "./helpers/assertions";
import { setupAuthMocks, setupConfigMocks } from "./helpers/mocks";

test.describe("設定モーダル", () => {
  let errorMonitor: ReturnType<typeof installErrorMonitor> | null = null;

  test.beforeEach(async ({ page }) => {
    errorMonitor = installErrorMonitor(page);
    await setupAuthMocks(page, { initialState: "unauthenticated" });
    await setupConfigMocks(page);
  });

  test.afterEach(async () => {
    if (errorMonitor) {
      await errorMonitor.assertNoErrors();
    }
  });

  test("RAG設定の切替ができる", async ({ page }) => {
    await page.goto("/");

    await expect(page.getByRole("heading", { name: "Decision Agent" })).toBeVisible();

    await page.getByPlaceholder("ユーザー名を入力").fill("admin");
    await page.getByPlaceholder("パスワードを入力").fill("admin123");
    await page.getByRole("button", { name: /ログイン/ }).click();

    await expect(page.getByText("解決したい問題・意思決定事項")).toBeVisible();

    await page.getByTitle("設定").click();
    await expect(page.getByRole("heading", { name: "⚙️ RAG設定" })).toBeVisible();

    await page.getByRole("button", { name: "器" }).click();

    const ragToggle = page.getByText("RAGを使用").locator("..").locator("..").getByRole("button");
    await ragToggle.click();

    await expect(page.getByText("検索件数 (Top K)")).toBeVisible();

    await page.getByRole("button", { name: "閉じる" }).click();
    await expect(page.getByRole("heading", { name: "⚙️ RAG設定" })).not.toBeVisible();
  });
});
