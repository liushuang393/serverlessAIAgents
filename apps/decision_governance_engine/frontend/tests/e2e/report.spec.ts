import { expect, test } from "@playwright/test";

import { installErrorMonitor } from "./helpers/assertions";
import {
  installMockEventSource,
  setupAuthMocks,
  setupReportMocks,
} from "./helpers/mocks";

test.describe("レポート画面", () => {
  let errorMonitor: ReturnType<typeof installErrorMonitor> | null = null;
  let eventSource: Awaited<ReturnType<typeof installMockEventSource>> | null = null;

  test.beforeEach(async ({ page }) => {
    errorMonitor = installErrorMonitor(page);
    await setupAuthMocks(page, { initialState: "unauthenticated" });
    await setupReportMocks(page);
    eventSource = await installMockEventSource(page);
  });

  test.afterEach(async () => {
    if (errorMonitor) {
      await errorMonitor.assertNoErrors();
    }
  });

  test("PDF出力と電子署名ができる", async ({ page }) => {
    if (!eventSource) {
      throw new Error("EventSourceモックが初期化されていません");
    }

    await page.goto("/");

    await expect(page.getByRole("heading", { name: "Decision Agent" })).toBeVisible();

    await page.getByPlaceholder("ユーザー名を入力").fill("admin");
    await page.getByPlaceholder("パスワードを入力").fill("admin123");
    await page.getByRole("button", { name: /ログイン/ }).click();

    await expect(page.getByText("解決したい問題・意思決定事項")).toBeVisible();

    await page
      .getByText("解決したい問題・意思決定事項")
      .locator("..")
      .locator("textarea")
      .fill("レポート出力と署名を確認するための質問です。");

    await page.getByRole("button", { name: "決策分析を開始する" }).click();

    await expect
      .poll(async () =>
        (await eventSource.getUrls()).some((url) => url.includes("/api/decision/stream"))
      )
      .toBeTruthy();

    const timestamp = Date.now();

    await eventSource.emitMessage({
      event_type: "flow.start",
      timestamp,
      flow_id: "flow-002",
    });

    const reportPayload = {
      report_id: "report-002",
      created_at: "2026-02-03T00:00:00Z",
      version: "v3.1",
      dao: {
        problem_type: "意思決定",
        problem_nature: null,
        essence: "意思決定の妥当性",
        essence_derivation: null,
        existing_alternatives: [],
        immutable_constraints: [],
        hidden_assumptions: [],
        causal_gears: [],
        bottleneck_gear: null,
        death_traps: [],
      },
      fa: {
        recommended_paths: [],
        rejected_paths: [],
        decision_criteria: [],
        path_comparison: null,
        strategic_prohibitions: [],
        differentiation_axis: null,
        why_existing_fails: "",
      },
      shu: {
        phases: [],
        first_action: "関係者との合意形成",
        dependencies: [],
        rhythm_control: null,
        cut_list: [],
        context_specific_actions: [],
        single_validation_point: null,
        exit_criteria: null,
      },
      qi: {
        implementations: [],
        tool_recommendations: [],
        integration_points: [],
        technical_debt_warnings: [],
        domain_technologies: [],
        regulatory_considerations: [],
        geographic_considerations: [],
      },
      review: {
        overall_verdict: "FEASIBLE",
        confidence_score: 0.82,
        findings: [],
        final_warnings: [],
        success_factors: [],
        supplementary_questions: [],
      },
      executive_summary: {
        one_line_decision: "実行可能",
        recommended_action: "施策を進める",
        key_risks: [],
        first_step: "詳細設計を開始",
        estimated_impact: "高",
      },
    } as const;

    await eventSource.emitMessage({
      event_type: "flow.complete",
      timestamp: timestamp + 1,
      flow_id: "flow-002",
      result: reportPayload as unknown as Record<string, unknown>,
    });

    const viewReportButton = page.getByRole("button", { name: "📄 決策レポートを表示" });
    await expect(viewReportButton).toBeVisible();
    await viewReportButton.click();

    await expect(page.getByRole("heading", { name: "提案書", exact: true })).toBeVisible();

    const downloadPromise = page.waitForEvent("download");
    await page.getByRole("button", { name: "📄 PDF出力" }).click();
    await downloadPromise;

    page.once("dialog", async (dialog) => {
      await dialog.accept();
    });
    await page.getByRole("button", { name: "電子署名" }).click();

    await expect(page.getByText("署名")).toBeVisible();
  });
});
