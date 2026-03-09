import { expect, test } from "playwright/test";

interface McpServer {
  name: string;
  command: string;
  args: string[];
  env: Record<string, string>;
  enabled: boolean;
  description: string;
  install_method: "config" | "dynamic";
}

interface LazyLoadingConfig {
  enabled: boolean;
  threshold: number;
  auto_load_on_call: boolean;
  cache_session: boolean;
  default_load_count: number;
}

interface ToolIndexEntry {
  name: string;
  description: string;
  source: string;
  server: string;
  loaded: boolean;
}

interface McpInstallRequestPayload {
  name: string;
  command: string;
  args: string[];
  env: Record<string, string>;
  description: string;
  install_method: "config" | "dynamic";
}

interface FileOrganizerRequestPayload {
  dry_run: boolean;
}

/**
 * 主要業務画面の E2E 検証。
 *
 * 承認・タイムライン・Skills・MCP・File Organizer の主要導線について、
 * ネットワークモックと UI 操作の両面から設計どおりに動作することを確認する。
 */
test("approvals, timeline, skills, MCP and file organizer routes are usable", async ({
  page,
}) => {
  await page.addInitScript(() => {
    window.localStorage.setItem("MESSAGING_HUB_API_KEY", "e2e-key");
  });

  let approveCalled = false;
  let organizeCallCount = 0;
  let toolResetCalled = false;
  let installedMcpPayload: McpInstallRequestPayload | null = null;
  const mcpToggleEvents: string[] = [];
  const deletedMcpServers: string[] = [];
  const toolSearchQueries: string[] = [];
  const lazyPatchPayloads: LazyLoadingConfig[] = [];

  const defaultLoadedTools = new Set<string>([
    "read_file",
    "filesystem.read_file",
  ]);
  const mcpServers: McpServer[] = [
    {
      name: "filesystem",
      command: "npx",
      args: ["-y", "@modelcontextprotocol/server-filesystem", "/workspace"],
      env: {},
      enabled: true,
      description: "ローカルファイル参照用の MCP サーバー",
      install_method: "dynamic",
    },
  ];
  const lazyLoading: LazyLoadingConfig = {
    enabled: true,
    threshold: 2,
    auto_load_on_call: true,
    cache_session: true,
    default_load_count: 2,
  };
  const toolIndex: ToolIndexEntry[] = [
    {
      name: "read_file",
      description: "ローカルファイルを読む標準スキル",
      source: "skill",
      server: "",
      loaded: true,
    },
    {
      name: "filesystem.read_file",
      description: "filesystem MCP 経由でファイルを読む",
      source: "mcp",
      server: "filesystem",
      loaded: true,
    },
    {
      name: "browser.open_tab",
      description: "ブラウザを起動する標準スキル",
      source: "skill",
      server: "",
      loaded: false,
    },
  ];

  /**
   * 現在のツール状態から統計情報レスポンスを生成する。
   *
   * UI 表示と検索・リセット後の状態確認を一貫させるため、毎回最新値を返す。
   */
  const buildToolStats = () => ({
    total_indexed: toolIndex.length,
    loaded_count: toolIndex.filter((tool) => tool.loaded).length,
    unloaded_count: toolIndex.filter((tool) => !tool.loaded).length,
    search_count: toolSearchQueries.length,
    default_skills: Array.from(defaultLoadedTools),
    loaded_tools: toolIndex
      .filter((tool) => tool.loaded)
      .map((tool) => tool.name),
  });

  /**
   * 新規 MCP サーバーに紐づくテスト用ツールエントリを生成する。
   *
   * 追加直後は未ロードとし、検索時に lazy loading が働く状態を再現する。
   */
  const buildMcpToolEntry = (
    serverName: string,
    description: string,
  ): ToolIndexEntry => ({
    name: `${serverName}.tool`,
    description: description || `${serverName} 連携用 MCP ツール`,
    source: "mcp",
    server: serverName,
    loaded: false,
  });

  await page.route("**/api/health", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({
        status: "healthy",
        auth_required: false,
        auth_key_configured: true,
      }),
    });
  });

  await page.route("**/api/approvals/pending", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({
        requests: [
          {
            id: "req_1",
            skill_name: "file_organizer.organize",
            risk_level: "high",
            params: { path: "/tmp/demo" },
            user_id: "u1",
            status: "pending",
            created_at: "2026-03-06T00:00:00Z",
            expires_at: "2026-03-06T01:00:00Z",
            decided_at: null,
            decided_by: null,
            rejection_reason: null,
          },
        ],
        total: 1,
      }),
    });
  });

  await page.route("**/api/approvals/history**", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({ requests: [], total: 0 }),
    });
  });

  await page.route("**/api/approvals/stats", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({
        pending: 1,
        approved: 0,
        rejected: 0,
        expired: 0,
        auto_approved: 0,
        total_processed: 0,
      }),
    });
  });

  await page.route("**/api/approvals/*/approve", async (route) => {
    approveCalled = true;
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({ ok: true }),
    });
  });

  await page.route("**/api/executions?*", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({ events: [], total: 0 }),
    });
  });

  await page.route("**/api/executions/stats", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({
        total_executions: 0,
        success_count: 0,
        failed_count: 0,
        success_rate: 0,
        avg_duration_ms: 0,
        by_skill: {},
        by_hour: {},
      }),
    });
  });

  await page.route("**/api/skills", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({
        skills: [
          {
            name: "read_file",
            description: "read file",
            category: "os_read",
            risk_level: "low",
            requires_confirmation: false,
            enabled: true,
          },
        ],
        total: 1,
      }),
    });
  });

  await page.route("**/api/workflows", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({ workflows: [], total: 0 }),
    });
  });

  await page.route("**/api/mcp/servers", async (route) => {
    if (route.request().method() === "GET") {
      await route.fulfill({
        status: 200,
        contentType: "application/json",
        body: JSON.stringify({ servers: mcpServers, total: mcpServers.length }),
      });
      return;
    }

    const payload = route.request().postDataJSON() as McpInstallRequestPayload;
    installedMcpPayload = payload;
    const nextServer: McpServer = {
      ...payload,
      enabled: true,
    };
    mcpServers.push(nextServer);
    toolIndex.push(buildMcpToolEntry(payload.name, payload.description));

    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({ server: nextServer, ok: true }),
    });
  });

  await page.route("**/api/mcp/servers/*", async (route) => {
    const pathname = new URL(route.request().url()).pathname;
    const segments = pathname.split("/").filter(Boolean);
    const method = route.request().method();

    if (method === "DELETE") {
      const serverName = decodeURIComponent(
        segments[segments.length - 1] ?? "",
      );
      const serverIndex = mcpServers.findIndex(
        (server) => server.name === serverName,
      );
      if (serverIndex >= 0) {
        mcpServers.splice(serverIndex, 1);
      }
      for (let index = toolIndex.length - 1; index >= 0; index -= 1) {
        if (toolIndex[index]?.server === serverName) {
          toolIndex.splice(index, 1);
        }
      }
      deletedMcpServers.push(serverName);

      await route.fulfill({
        status: 200,
        contentType: "application/json",
        body: JSON.stringify({ ok: true }),
      });
      return;
    }

    const action = segments[segments.length - 1] ?? "";
    const serverName = decodeURIComponent(segments[segments.length - 2] ?? "");
    const targetServer = mcpServers.find(
      (server) => server.name === serverName,
    );
    if (targetServer) {
      targetServer.enabled = action === "enable";
      mcpToggleEvents.push(`${serverName}:${action}`);
    }

    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({ ok: true }),
    });
  });

  await page.route("**/api/mcp/lazy-loading", async (route) => {
    if (route.request().method() === "GET") {
      await route.fulfill({
        status: 200,
        contentType: "application/json",
        body: JSON.stringify({ lazy_loading: lazyLoading }),
      });
      return;
    }

    const payload = route
      .request()
      .postDataJSON() as Partial<LazyLoadingConfig>;
    Object.assign(lazyLoading, payload);
    lazyPatchPayloads.push({ ...lazyLoading });
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({ lazy_loading: lazyLoading }),
    });
  });

  await page.route("**/api/tools/index", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({ tools: toolIndex, total: toolIndex.length }),
    });
  });

  await page.route("**/api/tools/stats", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify(buildToolStats()),
    });
  });

  await page.route("**/api/tools/search", async (route) => {
    const payload = route.request().postDataJSON() as { query: string };
    const normalizedQuery = payload.query.trim().toLowerCase();
    toolSearchQueries.push(payload.query.trim());

    let loadedCount = 0;
    const matches = toolIndex
      .filter((tool) =>
        [tool.name, tool.description, tool.source, tool.server]
          .join(" ")
          .toLowerCase()
          .includes(normalizedQuery),
      )
      .map((tool) => {
        if (!tool.loaded) {
          tool.loaded = true;
          loadedCount += 1;
        }
        return { ...tool };
      });

    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({
        query: payload.query.trim(),
        matches,
        loaded_count: loadedCount,
      }),
    });
  });

  await page.route("**/api/tools/reset", async (route) => {
    toolResetCalled = true;
    toolSearchQueries.length = 0;
    for (const tool of toolIndex) {
      tool.loaded = defaultLoadedTools.has(tool.name);
    }

    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({ ok: true }),
    });
  });

  await page.route("**/api/file-organizer/analyze", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({
        ok: true,
        total_files: 3,
        total_dirs: 1,
        total_size_mb: 2.5,
        by_category: { documents: { count: 1, size: 1024 } },
        by_extension: { ".txt": 1 },
        old_files_count: 0,
        large_files_count: 0,
        recommendations: [],
      }),
    });
  });

  await page.route("**/api/file-organizer/duplicates", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({ ok: true, duplicates: [] }),
    });
  });

  await page.route("**/api/file-organizer/organize", async (route) => {
    organizeCallCount += 1;
    const payload = route
      .request()
      .postDataJSON() as FileOrganizerRequestPayload;
    if (payload.dry_run) {
      await route.fulfill({
        status: 200,
        contentType: "application/json",
        body: JSON.stringify({
          ok: true,
          files_moved: 2,
          files_renamed: 0,
          dirs_created: 2,
          total_actions: 4,
          dry_run: true,
          errors: [],
        }),
      });
      return;
    }

    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({
        ok: false,
        requires_approval: true,
        request_id: "req_organize_1",
        status: "pending",
      }),
    });
  });

  await page.goto("/approvals", { waitUntil: "domcontentloaded" });
  await expect(page.getByRole("heading", { name: "承認管理" })).toBeVisible();
  await page.getByRole("button", { name: "承認" }).click();
  expect(approveCalled).toBeTruthy();

  await page.goto("/timeline", { waitUntil: "domcontentloaded" });
  await expect(
    page.getByRole("heading", { name: "実行タイムライン" }),
  ).toBeVisible();

  await page.goto("/skills", { waitUntil: "domcontentloaded" });
  await expect(page.getByRole("heading", { name: "スキル管理" })).toBeVisible();
  await page.getByRole("button", { name: /MCP 管理/ }).click();
  await expect(
    page.getByRole("heading", { name: "インストール済み MCP" }),
  ).toBeVisible();
  await expect(page.getByText("filesystem")).toBeVisible();
  await expect(page.getByText("MCP を新規追加")).toBeVisible();
  await expect(page.getByText("構成管理型")).toBeVisible();
  await expect(page.getByText("動的取得型")).toBeVisible();

  await page.getByLabel("サーバー名").fill("slack");
  await page.getByLabel("説明").fill("Slack 連携用の外部 MCP サーバー");
  await page
    .getByLabel("引数（半角スペース区切り）")
    .fill("-y @modelcontextprotocol/server-slack");
  await page
    .getByLabel("環境変数（1 行 1 件、KEY=VALUE 形式）")
    .fill("SLACK_BOT_TOKEN=xoxb-test");
  await page.getByRole("button", { name: "MCP を追加" }).click();
  await expect(
    page.getByText("MCP サーバー「slack」を追加しました。"),
  ).toBeVisible();
  expect(installedMcpPayload?.install_method).toBe("dynamic");
  expect(installedMcpPayload?.env.SLACK_BOT_TOKEN).toBe("xoxb-test");

  const slackServerCard = page
    .locator("div.rounded-lg.border.border-gray-200.p-4")
    .filter({ hasText: "slack" })
    .first();
  await expect(slackServerCard).toBeVisible();
  await slackServerCard.getByRole("button", { name: "無効化" }).click();
  await expect(slackServerCard.getByText("無効")).toBeVisible();
  await slackServerCard.getByRole("button", { name: "有効化" }).click();
  await expect(slackServerCard.getByText("有効")).toBeVisible();
  expect(mcpToggleEvents).toEqual(["slack:disable", "slack:enable"]);

  await page.getByLabel("検索しきい値").fill("3");
  await page.getByLabel("初期ロード件数").fill("1");
  await page.getByLabel("セッションキャッシュを維持").uncheck();
  await page.getByRole("button", { name: "懒加载設定を保存" }).click();
  await expect(page.getByText("懒加载設定を更新しました。")).toBeVisible();
  expect(lazyPatchPayloads.at(-1)).toMatchObject({
    threshold: 3,
    default_load_count: 1,
    cache_session: false,
  });

  await page.getByPlaceholder("例: slack / browser / read").fill("slack");
  await page.getByRole("button", { name: "検索してロード" }).click();
  await expect(page.getByText("検索語「slack」")).toBeVisible();
  expect(toolSearchQueries).toContain("slack");

  await page.getByRole("button", { name: "セッションをリセット" }).click();
  await expect(
    page.getByText("ツールセッションをリセットしました。"),
  ).toBeVisible();
  expect(toolResetCalled).toBeTruthy();

  await slackServerCard.getByRole("button", { name: "削除" }).click();
  await expect(
    page.getByText("MCP サーバー「slack」を削除しました。"),
  ).toBeVisible();
  expect(deletedMcpServers).toContain("slack");
  await expect(
    page
      .locator("div.rounded-lg.border.border-gray-200.p-4")
      .filter({ hasText: "slack" }),
  ).toHaveCount(0);

  await page.goto("/file-organizer", { waitUntil: "domcontentloaded" });
  await expect(
    page.getByRole("heading", { name: "File Organizer" }),
  ).toBeVisible();
  await page.getByRole("button", { name: /^organize$/i }).click();
  await page.getByRole("button", { name: "Dry Run (Preview)" }).click();
  await expect(page.getByText("Preview Result")).toBeVisible();
  await page.getByRole("button", { name: "Execute" }).click();
  await expect(page.getByText("承認待ちリクエスト")).toBeVisible();
  expect(organizeCallCount).toBeGreaterThanOrEqual(2);
});
