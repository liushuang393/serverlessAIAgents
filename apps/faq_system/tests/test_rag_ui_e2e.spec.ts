/**
 * FAQ System - RAG 管理 UI E2E テスト
 *
 * FAQ System フロントエンドが起動していること (npm run dev, port 3004) が前提。
 * バックエンド認証は dev モードで動作が必要。
 */
import { test, expect } from '@playwright/test';

test.describe('FAQ System - RAG 管理 UI E2E', () => {
  test.beforeEach(async ({ page }) => {
    // ログイン
    await page.goto('/login');
    await page.fill('[name="username"]', 'admin');
    await page.fill('[name="password"]', 'admin');
    await page.click('[type="submit"]');
    await page.waitForURL('/');
  });

  test('コレクション一覧画面が表示される', async ({ page }) => {
    await page.click('[data-testid="nav-rag"]');
    await page.click('[data-testid="rag-tab-collections"]');
    await expect(page.locator('[data-testid="collection-list"]')).toBeVisible();
  });

  test('新規コレクション作成フォームが開く', async ({ page }) => {
    await page.click('[data-testid="nav-rag"]');
    await page.click('[data-testid="rag-tab-collections"]');
    await page.click('[data-testid="create-collection-button"]');
    await expect(page.locator('[data-testid="collection-form-modal"]')).toBeVisible();
  });

  test('コレクション作成 - FAQ Precision パターン自動セット', async ({ page }) => {
    await page.click('[data-testid="nav-rag"]');
    await page.click('[data-testid="rag-tab-collections"]');
    await page.click('[data-testid="create-collection-button"]');

    // パターン選択
    await page.selectOption('[data-testid="pattern-select"]', 'faq_precision');

    // 自動セット確認
    await expect(page.locator('[data-testid="chunk-strategy"]')).toHaveValue('sentence');
    await expect(page.locator('[data-testid="chunk-size"]')).toHaveValue('500');
  });

  test('ファイルアップロード UI が表示される', async ({ page }) => {
    await page.click('[data-testid="nav-rag"]');
    await page.click('[data-testid="rag-tab-documents"]');
    await expect(page.locator('[data-testid="upload-area"]')).toBeVisible();
  });

  test('検索テスト UI が動作する', async ({ page }) => {
    await page.click('[data-testid="nav-rag"]');
    await page.click('[data-testid="rag-tab-retrieval"]');
    await expect(page.locator('[data-testid="test-query-input"]')).toBeVisible();

    await page.fill('[data-testid="test-query-input"]', 'テストクエリ');
    await page.click('[data-testid="run-test-query-button"]');
    // 結果エリアが表示される（空でも可）
    await expect(page.locator('[data-testid="query-result-area"]')).toBeVisible();
  });

  test('インジェスト履歴タブが開く', async ({ page }) => {
    await page.click('[data-testid="nav-rag"]');
    await page.click('[data-testid="rag-tab-ingest"]');
    await expect(page.locator('[data-testid="ingest-history-list"]')).toBeVisible();
  });
});
