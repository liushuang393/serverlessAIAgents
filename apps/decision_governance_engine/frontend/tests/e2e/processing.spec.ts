import { expect, test } from "@playwright/test";

import { installErrorMonitor } from "./helpers/assertions";
import { installMockEventSource, setupAuthMocks } from "./helpers/mocks";

test.describe("進捗画面SSE", () => {
  let errorMonitor: ReturnType<typeof installErrorMonitor> | null = null;
  let eventSource: Awaited<ReturnType<typeof installMockEventSource>> | null = null;

  test.beforeEach(async ({ page }) => {
    errorMonitor = installErrorMonitor(page);
    await setupAuthMocks(page, { initialState: "unauthenticated" });
    eventSource = await installMockEventSource(page);
  });

  test.afterEach(async () => {
    if (errorMonitor) {
      await errorMonitor.assertNoErrors();
    }
  });

  test("SSE進捗からレポート表示まで遷移できる", async ({ page }) => {
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
      .fill("新規事業AとBのどちらに投資すべきかを判断したいです。");

    await page.getByRole("button", { name: "決策分析を開始する" }).click();

    await expect
      .poll(async () =>
        (await eventSource?.getUrls() ?? []).some((url) => url.includes("/api/decision/stream"))
      )
      .toBeTruthy();

    const timestamp = Date.now();

    await eventSource.emitMessage({
      event_type: "connection.established",
      timestamp,
      flow_id: "flow-001",
    });

    await expect(page.getByText("ストリーム接続中")).toBeVisible();

    await eventSource.emitMessage({
      event_type: "flow.start",
      timestamp: timestamp + 1,
      flow_id: "flow-001",
    });

    await eventSource.emitMessage({
      event_type: "progress",
      timestamp: timestamp + 2,
      flow_id: "flow-001",
      node_id: "cognitive_gate",
      percentage: 45,
      message: "進捗更新",
    });

    await expect(page.getByText("進捗更新")).toBeVisible();

    await eventSource.emitMessage({
      event_type: "node.complete",
      timestamp: timestamp + 3,
      flow_id: "flow-001",
      node_id: "cognitive_gate",
      data: {
        evaluation_object: "新規事業の投資判断",
      },
    });

    const reportPayload = {
      report_id: "report-001",
      created_at: "2026-02-03T00:00:00Z",
      version: "v3.1",
      dao: {
        problem_type: "投資判断",
        problem_nature: null,
        essence: "収益性と成長性の見極め",
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
        first_action: "市場検証を開始",
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
      executive_summary: {
        one_line_decision: "事業Aを優先すべき",
        recommended_action: "市場検証を加速",
        key_risks: [],
        first_step: "パイロット顧客の選定",
        estimated_impact: "中",
      },
    } as const;

    await eventSource.emitMessage({
      event_type: "flow.complete",
      timestamp: timestamp + 4,
      flow_id: "flow-001",
      result: reportPayload as unknown as Record<string, unknown>,
    });

    const viewReportButton = page.getByRole("button", { name: "📄 決策レポートを表示" });
    await expect(viewReportButton).toBeVisible();
    await viewReportButton.click();

    await expect(page.getByRole("heading", { name: "提案書", exact: true })).toBeVisible();
  });
});
