import { expect, test, type Page } from "@playwright/test";

const overviewPayload = {
  gateway: {
    default_role: "reasoning",
    request_timeout_seconds: 120,
    max_retries: 2,
  },
  providers: [
    {
      name: "openai",
      api_base: "https://api.openai.com/v1",
      api_key_env: "OPENAI_API_KEY",
      models: [],
      enabled: true,
    },
  ],
  providers_runtime: [],
  inference_engines: [],
  models: [],
  registry: { reasoning: "reasoning_openai_gpt_4o_mini" },
  routing_policy: {
    priority: "latency",
    fallback_chain: {},
    load_balance_strategy: "round_robin",
    cost_budget: null,
  },
  cost_summary: {
    total_cost_usd: 0,
    details: [],
    cost_budget: null,
    budget_exceeded: false,
  },
  config_version: "version-a",
};

const catalogPayload = {
  providers: [
    {
      name: "openai",
      canonical_name: "openai",
      aliases: [],
      requires_api_key: true,
      default_api_key_env: "OPENAI_API_KEY",
      default_api_base: "https://api.openai.com/v1",
      recommended_models: ["gpt-4o-mini"],
      install_recipes: [],
    },
  ],
  backends: [],
  models: [
    {
      alias: "reasoning_openai_gpt_4o_mini",
      provider: "openai",
      model: "gpt-4o-mini",
      capabilities: ["reasoning"],
      context_window: 128000,
      recommended_for: ["reasoning"],
    },
  ],
  generated_at: "2026-03-05T00:00:00",
};

async function mockLLMSuccessRoutes(page: Page): Promise<void> {
  await page.route("**/openapi.json", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({
        paths: { "/api/studios/framework/llm/overview": {} },
      }),
    });
  });
  await page.route("**/api/studios/framework/llm/**", async (route) => {
    const url = route.request().url();
    if (url.endsWith("/overview")) {
      await route.fulfill({
        status: 200,
        contentType: "application/json",
        body: JSON.stringify(overviewPayload),
      });
      return;
    }
    if (url.endsWith("/catalog")) {
      await route.fulfill({
        status: 200,
        contentType: "application/json",
        body: JSON.stringify(catalogPayload),
      });
      return;
    }
    if (url.endsWith("/engines/status")) {
      await route.fulfill({
        status: 200,
        contentType: "application/json",
        body: JSON.stringify({ engine_status: [] }),
      });
      return;
    }
    if (url.endsWith("/reload")) {
      await route.fulfill({
        status: 200,
        contentType: "application/json",
        body: JSON.stringify({
          reloaded: true,
          gateway: overviewPayload.gateway,
        }),
      });
      return;
    }
    if (url.endsWith("/setup-and-switch")) {
      await route.fulfill({
        status: 200,
        contentType: "application/json",
        body: JSON.stringify({
          preflight: {
            status: "success",
            started_at: "2026-03-05T00:00:00",
            completed_at: "2026-03-05T00:00:01",
            request: {
              providers: ["openai"],
              backends: [],
              auto_install: true,
              auto_start: true,
              health_check: true,
              dry_run: false,
            },
            steps: [],
            summary: "status=success",
          },
          switch: {
            success: true,
            rolled_back: false,
            config_version: "version-b",
            applied_alias: "reasoning_openai_gpt_4o_mini",
            registry: { reasoning: "reasoning_openai_gpt_4o_mini" },
            diffs: [],
            runtime_check: {
              provider_status: "available",
              backend_status: null,
              errors: [],
            },
            message: "switch applied successfully",
          },
          success: true,
          message: "setup and switch completed",
        }),
      });
      return;
    }
    if (url.endsWith("/diagnostics")) {
      await route.fulfill({
        status: 200,
        contentType: "application/json",
        body: JSON.stringify({
          has_llm_routes: true,
          route_count: 12,
          config_path: "/tmp/.bizcore/llm_gateway.yaml",
          config_exists: true,
          config_version: "version-a",
          last_preflight: null,
          hints: ["healthy"],
          server_time: "2026-03-05T00:00:00",
        }),
      });
      return;
    }
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: "{}",
    });
  });
}

async function mockLLMMissingRoute(page: Page): Promise<void> {
  await page.route("**/api/studios/framework/llm/overview", async (route) => {
    await route.fulfill({
      status: 404,
      contentType: "application/json",
      body: JSON.stringify({ detail: "Not Found" }),
    });
  });
  await page.route("**/api/studios/framework/llm/catalog", async (route) => {
    await route.fulfill({
      status: 404,
      contentType: "application/json",
      body: JSON.stringify({ detail: "Not Found" }),
    });
  });
  await page.route(
    "**/api/studios/framework/llm/diagnostics",
    async (route) => {
      await route.fulfill({
        status: 200,
        contentType: "application/json",
        body: JSON.stringify({
          has_llm_routes: false,
          route_count: 0,
          config_path: "/tmp/.bizcore/llm_gateway.yaml",
          config_exists: true,
          config_version: "version-a",
          last_preflight: null,
          hints: [
            "LLM routes are missing. Restart platform backend with latest code.",
          ],
          server_time: "2026-03-05T00:00:00",
        }),
      });
    },
  );
  await page.route("**/openapi.json", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({ paths: { "/health": {} } }),
    });
  });
}

test.describe("LLM Management E2E", () => {
  test("setup-and-switch success flow (mock backend)", async ({ page }) => {
    await mockLLMSuccessRoutes(page);
    await page.goto("http://localhost:3000/llm-management");
    await expect(
      page.locator('[data-testid="llm-switch-panel"]'),
    ).toBeVisible();

    await page.fill('[data-testid="llm-switch-model"]', "gpt-4o-mini");
    await page.click('[data-testid="llm-setup-switch-button"]');

    await expect(
      page.locator('[data-testid="llm-setup-switch-success"]'),
    ).toBeVisible();
    await expect(
      page.locator('[data-testid="llm-setup-switch-success"]'),
    ).toContainText("Switch completed successfully");
  });

  test("shows route missing diagnostics when overview returns 404", async ({
    page,
  }) => {
    await mockLLMMissingRoute(page);
    await page.goto("http://localhost:3000/llm-management");

    const diagnostic = page.locator(
      '[data-testid="llm-route-missing-diagnostic"]',
    );
    await expect(diagnostic).toBeVisible();
    await expect(diagnostic).toContainText("OpenAPI");
  });
});
