/**
 * RAG Overview - パターン選択 E2E テスト
 *
 * Platform フロントエンドが起動していること (npm run dev, port 3000) が前提。
 * CI では webServer 設定により自動起動される。
 */
import { test, expect } from '@playwright/test';

test.describe('RAG Overview - パターン選択 E2E', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('http://localhost:3000');
    await page.waitForLoadState('networkidle');
  });

  test('パターン "faq_precision" を選択するとパラメータが自動セットされる', async ({ page }) => {
    // RAG 設定画面を開く
    await page.click('[data-testid="nav-rag"]');
    await page.waitForSelector('[data-testid^="rag-app-card-"]');

    // FAQ System カードをクリック
    await page.click('[data-testid="rag-app-card-faq_system"]');
    await page.waitForSelector('[data-testid="rag-settings-form"]');

    // パターン選択
    await page.selectOption('[data-testid="rag-pattern-select"]', 'faq_precision');

    // 自動セットされたパラメータを確認
    await expect(page.locator('[data-testid="chunk-strategy-select"]')).toHaveValue('sentence');
    await expect(page.locator('[data-testid="chunk-size-input"]')).toHaveValue('500');
    await expect(page.locator('[data-testid="chunk-overlap-input"]')).toHaveValue('80');
    await expect(page.locator('[data-testid="retrieval-method-select"]')).toHaveValue('hybrid');
    await expect(page.locator('[data-testid="top-k-input"]')).toHaveValue('8');

    // modified バッジが表示されないこと（パターン直後はクリーン状態）
    await expect(page.locator('[data-testid="rag-pattern-modified-badge"]')).not.toBeVisible();
  });

  test('パターン選択後にパラメータを手動上書きすると modified バッジが表示される', async ({ page }) => {
    await page.click('[data-testid="nav-rag"]');
    await page.waitForSelector('[data-testid^="rag-app-card-"]');
    await page.click('[data-testid="rag-app-card-faq_system"]');
    await page.waitForSelector('[data-testid="rag-settings-form"]');

    await page.selectOption('[data-testid="rag-pattern-select"]', 'faq_precision');

    // パラメータ手動上書き
    await page.fill('[data-testid="top-k-input"]', '10');
    await expect(page.locator('[data-testid="top-k-input"]')).toHaveValue('10');

    // modified バッジが表示されること
    await expect(page.locator('[data-testid="rag-pattern-modified-badge"]')).toBeVisible();
  });

  test('保存ボタンが表示される', async ({ page }) => {
    await page.click('[data-testid="nav-rag"]');
    await page.waitForSelector('[data-testid^="rag-app-card-"]');
    await page.click('[data-testid="rag-app-card-faq_system"]');
    await page.waitForSelector('[data-testid="rag-settings-form"]');

    const saveBtn = page.locator('[data-testid="rag-save-button"]');
    await expect(saveBtn).toBeVisible();
    await expect(saveBtn).toBeEnabled();
  });
});
