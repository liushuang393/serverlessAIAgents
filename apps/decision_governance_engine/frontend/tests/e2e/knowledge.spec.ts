import { expect, test } from "@playwright/test";

import { installErrorMonitor } from "./helpers/assertions";
import { setupAuthMocks, setupKnowledgeMocks } from "./helpers/mocks";

test.describe("知識ベース管理", () => {
  let errorMonitor: ReturnType<typeof installErrorMonitor> | null = null;

  test.beforeEach(async ({ page }) => {
    errorMonitor = installErrorMonitor(page);
    await setupAuthMocks(page, { initialState: "unauthenticated" });
    await setupKnowledgeMocks(page);
  });

  test.afterEach(async () => {
    if (errorMonitor) {
      await errorMonitor.assertNoErrors();
    }
  });

  test("術/器の知識追加と削除ができる", async ({ page }) => {
    await page.goto("/");

    await expect(
      page.getByRole("heading", { name: "Decision Agent" }),
    ).toBeVisible();

    await page.getByPlaceholder("ユーザー名を入力").fill("admin");
    await page.getByPlaceholder("パスワードを入力").fill("admin123");
    await page.getByRole("button", { name: /ログイン/ }).click();

    await expect(page.getByText("解決したい問題・意思決定事項")).toBeVisible();

    await page.getByRole("button", { name: "📚 知識追加" }).first().click();

    await expect(
      page.getByRole("heading", { name: "術・知識ベース設定" }),
    ).toBeVisible();

    const addButton = page.getByRole("button", { name: "＋ 知識を追加" });
    await expect(addButton).toBeDisabled();

    const newKnowledge = "術の知識テスト用コンテンツ";
    await page
      .getByPlaceholder(
        "例: アジャイル開発では2週間のスプリントが推奨される...",
      )
      .fill(newKnowledge);
    await expect(addButton).toBeEnabled();
    await addButton.click();

    await expect(page.getByText(newKnowledge)).toBeVisible();

    page.on("dialog", async (dialog) => {
      await dialog.accept();
    });

    const deleteButton = page.getByRole("button", { name: "🗑️" }).first();
    await deleteButton.click();
    await expect(page.getByText(newKnowledge)).not.toBeVisible();

    await page.getByRole("button", { name: "← 戻る" }).click();
    await expect(page.getByText("解決したい問題・意思決定事項")).toBeVisible();

    await page.getByRole("button", { name: "📚 知識追加" }).nth(1).click();
    await expect(
      page.getByRole("heading", { name: "器・知識ベース設定" }),
    ).toBeVisible();
  });
});
