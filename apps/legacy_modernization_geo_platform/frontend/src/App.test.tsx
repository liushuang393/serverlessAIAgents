import { render } from "@testing-library/react";
import { fireEvent, screen, waitFor } from "@testing-library/dom";
import { afterEach, describe, expect, it, vi } from "vitest";
import App from "./App";
import { I18nProvider } from "./i18n";
import * as api from "./lib/api";

vi.mock("./lib/api", () => ({
  buildStreamUrl: vi.fn((path: string) => path),
  startExecution: vi
    .fn()
    .mockResolvedValue({ task_id: "geo-test", ws_url: "/api/ws/geo-test" }),
  fetchState: vi.fn().mockResolvedValue({
    task_id: "geo-test",
    status: "waiting_approval",
    current_stage: "geo_qa",
    campaign_name: "demo",
    package: "assessment",
    request: {
      campaign_name: "demo",
      package: "assessment",
      targets: {
        industries: ["manufacturing"],
        legacy_stacks: ["COBOL"],
        regions: ["Japan"],
      },
    },
    events: [
      {
        event_type: "approval_required",
        timestamp: new Date().toISOString(),
        task_id: "geo-test",
        stage: "publish_review",
        agent: "GeoQA",
        message: "Human approval required",
        payload: {},
      },
    ],
    artifacts: [],
    approvals: [
      {
        request_id: "apr-1",
        task_id: "geo-test",
        stage: "publish_review",
        object_id: "geo-test",
        risk_level: "MEDIUM",
        reason: "review",
        status: "pending",
        actions: ["approve", "reject", "rewrite"],
        created_at: new Date().toISOString(),
        updated_at: new Date().toISOString(),
      },
    ],
    report: {
      task_id: "geo-test",
      markdown: "# report",
      summary: {},
      created_at: new Date().toISOString(),
      updated_at: new Date().toISOString(),
    },
    published_pages: [],
    error_message: null,
  }),
  fetchArtifacts: vi.fn().mockResolvedValue({
    signal: {
      company: "demo",
      signals: [],
      urgency_hypothesis: "高い",
      modernization_fit_score: 87,
    },
    draft: {
      pages: [
        {
          slug: "demo-page",
          title: "Draft Title",
          summary: "Draft Summary",
          body_markdown: "# Draft",
          cta: "CTA",
          faq_entries: [],
        },
      ],
      target_language: "ja",
      unknowns: [],
    },
    qa: {
      pass_or_fail: "REVIEW",
      issues: ["Need approval"],
      fix_instructions: [],
      publish_ready: true,
      risk_level: "MEDIUM",
      metrics: {},
    },
    questionGraph: {
      personas: [
        {
          role: "cio",
          questions: [],
          high_intent_questions: ["Can we migrate?"],
        },
      ],
    },
    evidenceMatrix: { entries: [] },
  }),
  submitApproval: vi.fn().mockResolvedValue({}),
  postCommand: vi.fn().mockResolvedValue({}),
}));

function renderApp(): void {
  render(
    <I18nProvider>
      <App />
    </I18nProvider>,
  );
}

afterEach(() => {
  vi.clearAllMocks();
});

describe("App", () => {
  it("renders japanese labels by default", async () => {
    renderApp();
    expect(await screen.findByText("キャンペーン開始")).toBeInTheDocument();
    expect(screen.getByTestId("start-campaign-button")).toBeInTheDocument();
  });

  it("switches UI labels between en and zh", async () => {
    renderApp();
    const localeSwitcher = await screen.findByTestId("locale-switcher");

    fireEvent.change(localeSwitcher, { target: { value: "en" } });
    expect(await screen.findByText("Start Campaign")).toBeInTheDocument();

    fireEvent.change(localeSwitcher, { target: { value: "zh" } });
    expect(await screen.findByText("启动活动")).toBeInTheDocument();
  });

  it("sends selected locale as content language when starting execution", async () => {
    renderApp();
    const localeSwitcher = await screen.findByTestId("locale-switcher");
    fireEvent.change(localeSwitcher, { target: { value: "en" } });
    await screen.findByText("Start Campaign");

    fireEvent.click(screen.getByTestId("start-campaign-button"));

    await waitFor(() => {
      expect(api.startExecution).toHaveBeenCalledTimes(1);
    });

    const payload = vi.mocked(api.startExecution).mock.calls[0][0];
    expect(payload.inputs?.content_languages).toEqual(["en"]);
  });
});
