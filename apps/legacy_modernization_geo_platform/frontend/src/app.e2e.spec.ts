import { expect, test } from '@playwright/test';

async function startCampaign(
  page: import('@playwright/test').Page,
  campaignName: string,
  industry: string,
  stacks: string,
): Promise<void> {
  await page.goto('/');
  await page.getByTestId('campaign-name-input').fill(campaignName);
  await page.getByTestId('industries-input').fill(industry);
  await page.getByTestId('legacy-stacks-input').fill(stacks);
  await page.getByTestId('start-campaign-button').click();
}

test('covers operator analysis, rewrite, approval, publish, and public CTA flow', async ({ page }) => {
  await startCampaign(
    page,
    'construction-cobol-demand-japan',
    'construction',
    'COBOL, RPG',
  );

  await expect(page.getByTestId('task-status')).toContainText('waiting_approval', { timeout: 15000 });

  await page.getByTestId('tab-workspace').click();
  await expect(page.getByTestId('account-workspace')).toBeVisible();
  await expect(page.getByTestId('fit-score')).not.toHaveText('--');
  await expect(page.getByTestId('question-map-card')).toContainText('cio');
  await expect(page.getByTestId('evidence-summary-card').locator('li').first()).toBeVisible();

  await page.getByTestId('tab-content').click();
  await expect(page.getByTestId('content-studio')).toBeVisible();
  await expect(page.getByTestId('draft-preview')).toContainText('モダナイゼーションガイド');
  await expect(page.getByTestId('qa-risk-level')).toContainText('MEDIUM');
  await page.getByTestId('rewrite-note').fill('请补充阶段性迁移边界、适用前提、联络动作');
  await page.getByTestId('rewrite-button').click();
  await expect(page.getByTestId('draft-preview')).toContainText('レビュー反映メモ', { timeout: 15000 });

  await page.getByTestId('tab-approval').click();
  await expect(page.getByTestId('pending-approval-card')).toBeVisible({ timeout: 15000 });
  await page.getByRole('button', { name: 'Approve' }).click();

  await page.getByTestId('tab-report').click();
  await expect(page.getByTestId('report-center')).toBeVisible();
  await expect(page.getByTestId('published-page-link')).toBeVisible({ timeout: 10000 });
  await expect(page.getByTestId('report-markdown')).toContainText('Legacy Modernization GEO Platform Report');

  const href = await page.getByTestId('published-page-link').getAttribute('href');
  expect(href).toBeTruthy();

  if (href) {
    const publishedUrl = new URL(href);
    await page.goto(`${publishedUrl.pathname}${publishedUrl.search}`);
    await expect(page.getByTestId('public-title')).toBeVisible();
    await expect(page.getByTestId('public-hero-cta')).toBeVisible();
    await expect(page.getByTestId('public-contact-cta')).toHaveAttribute(
      'href',
      /mailto:modernization@example\.com/,
    );
    await expect(page.getByTestId('public-faq')).toContainText('よくある質問');
    const jsonLd = await page.locator('script[type="application/ld+json"]').textContent();
    expect(jsonLd).toContain('FAQPage');
  }

  await page.goto('/geo/sitemap.xml');
  await expect(page.locator('body')).toContainText('modernization-guide');
});

test('covers operator rejection flow before publishing', async ({ page }) => {
  await startCampaign(
    page,
    'manufacturing-cobol-risk-review',
    'manufacturing',
    'COBOL',
  );

  await page.getByTestId('tab-approval').click();
  await expect(page.getByTestId('pending-approval-card')).toBeVisible({ timeout: 15000 });
  await page.getByRole('button', { name: 'Reject' }).click();

  await expect(page.getByTestId('task-status')).toContainText('failed', { timeout: 15000 });
  await page.getByTestId('tab-report').click();
  await expect(page.getByTestId('report-markdown')).toContainText('等待报告生成');
  await expect(page.getByTestId('report-center')).toContainText('等待发布完成');
});
