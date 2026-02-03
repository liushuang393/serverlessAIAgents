import { expect, test } from "@playwright/test";

import { installErrorMonitor } from "./helpers/assertions";
import { setupAuthMocks } from "./helpers/mocks";

test.describe("ログインスモーク", () => {
  let errorMonitor: ReturnType<typeof installErrorMonitor> | null = null;

  test.beforeEach(async ({ page }) => {
    errorMonitor = installErrorMonitor(page);
    await setupAuthMocks(page, { initialState: "unauthenticated" });
  });

  test.afterEach(async () => {
    if (errorMonitor) {
      await errorMonitor.assertNoErrors();
    }
  });

  test("ログイン後に入力画面へ遷移できる", async ({ page }) => {
    await page.goto("/");

    await expect(page.getByRole("heading", { name: "Decision Agent" })).toBeVisible();

    const loginButton = page.getByRole("button", { name: /ログイン/ });
    await expect(loginButton).toBeDisabled();

    await page.getByPlaceholder("ユーザー名を入力").fill("admin");
    await page.getByPlaceholder("パスワードを入力").fill("admin123");

    await expect(loginButton).toBeEnabled();
    await loginButton.click();

    await expect(page.getByText("解決したい問題・意思決定事項")).toBeVisible();
    await expect(
      page.getByRole("button", { name: /決策分析を開始する/ })
    ).toBeVisible();
  });
});
