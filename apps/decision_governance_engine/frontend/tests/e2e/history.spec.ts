import { expect, test } from "@playwright/test";

import { installErrorMonitor } from "./helpers/assertions";
import { setupAuthMocks, setupHistoryMocks } from "./helpers/mocks";

test.describe("履歴画面", () => {
  let errorMonitor: ReturnType<typeof installErrorMonitor> | null = null;

  test.beforeEach(async ({ page }) => {
    errorMonitor = installErrorMonitor(page);
    await setupAuthMocks(page, { initialState: "unauthenticated" });
    await setupHistoryMocks(page);
  });

  test.afterEach(async () => {
    if (errorMonitor) {
      await errorMonitor.assertNoErrors();
    }
  });

  test("履歴一覧と詳細モーダルが表示できる", async ({ page }) => {
    await page.goto("/");

    await expect(page.getByRole("heading", { name: "Decision Agent" })).toBeVisible();

    await page.getByPlaceholder("ユーザー名を入力").fill("admin");
    await page.getByPlaceholder("パスワードを入力").fill("admin123");
    await page.getByRole("button", { name: /ログイン/ }).click();

    await expect(page.getByText("解決したい問題・意思決定事項")).toBeVisible();

    await page.getByRole("button", { name: "履歴" }).click();

    await expect(page.getByRole("heading", { name: "決策履歴" })).toBeVisible();

    await page.getByRole("button", { name: "詳細" }).first().click();

    await expect(page.getByRole("heading", { name: "決策詳細" })).toBeVisible();

    await page.getByRole("button", { name: "✕" }).click();
    await expect(page.getByRole("heading", { name: "決策詳細" })).not.toBeVisible();
  });
});
