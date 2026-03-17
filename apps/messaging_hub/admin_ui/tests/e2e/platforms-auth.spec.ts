import { expect, test } from "playwright/test";

type PlatformEndpoint = "health" | "platforms" | "connect" | "credentials";

interface EndpointMetrics {
  seen: number;
  ok: number;
  nonOk: number;
}

function createEndpointMetrics(): Record<PlatformEndpoint, EndpointMetrics> {
  return {
    health: { seen: 0, ok: 0, nonOk: 0 },
    platforms: { seen: 0, ok: 0, nonOk: 0 },
    connect: { seen: 0, ok: 0, nonOk: 0 },
    credentials: { seen: 0, ok: 0, nonOk: 0 },
  };
}

function markEndpoint(
  metrics: Record<PlatformEndpoint, EndpointMetrics>,
  endpoint: PlatformEndpoint,
  status: number,
): void {
  metrics[endpoint].seen += 1;
  if (status >= 200 && status < 300) {
    metrics[endpoint].ok += 1;
    return;
  }
  metrics[endpoint].nonOk += 1;
}

test("platforms page covers all business network events without API errors and matches expected UI flows", async ({
  page,
}) => {
  const endpointMetrics = createEndpointMetrics();
  const failedRequests: string[] = [];
  const nonOkApiResponses: Array<{ status: number; url: string }> = [];

  let platformsFetchCount = 0;
  let connectCalled = false;
  let slackConnectCalled = false;
  let saveCalled = false;
  let savedBotToken = "";

  await page.addInitScript(() => {
    window.localStorage.setItem("MESSAGING_HUB_API_KEY", "e2e-key");
  });

  page.on("requestfailed", (request) => {
    const requestUrl = new URL(request.url());
    if (!requestUrl.pathname.startsWith("/api")) {
      return;
    }
    failedRequests.push(
      `${request.method()} ${requestUrl.pathname} (${request.failure()?.errorText ?? "unknown"})`,
    );
  });

  page.on("response", (response) => {
    const responseUrl = new URL(response.url());
    if (!responseUrl.pathname.startsWith("/api")) {
      return;
    }
    if (response.status() >= 400) {
      nonOkApiResponses.push({
        status: response.status(),
        url: responseUrl.pathname,
      });
    }
  });

  await page.route("**/api/health", async (route) => {
    const status = 200;
    markEndpoint(endpointMetrics, "health", status);
    await route.fulfill({
      status,
      contentType: "application/json",
      body: JSON.stringify({
        status: "healthy",
        auth_required: false,
        auth_key_configured: true,
      }),
    });
  });

  await page.route("**/api/platforms", async (route) => {
    const status = 200;
    platformsFetchCount += 1;
    markEndpoint(endpointMetrics, "platforms", status);
    await route.fulfill({
      status,
      contentType: "application/json",
      body: JSON.stringify([
        {
          name: "bizcore_nexus",
          displayName: "BizCore Sovereign Nexus",
          icon: "🛡️",
          description: "Managed internal platform",
          connected: true,
          managed: true,
          authUrl: null,
          docsUrl: null,
          authMode: "managed",
          lastActivity: "2026-03-06T01:00:00Z",
          messageCount: 12,
          credentialFields: [],
        },
        {
          name: "slack",
          displayName: "Slack Enterprise Bridge",
          icon: "💼",
          description:
            "Slack App を作成して Bot Token と Signing Secret を設定します。",
          connected: false,
          managed: false,
          authUrl: "https://api.slack.com/apps",
          docsUrl: "https://api.slack.com/authentication",
          authMode: "oauth_and_api_key",
          lastActivity: null,
          messageCount: 0,
          credentialFields: [
            {
              key: "bot_token",
              label: "Bot Token",
              required: true,
              configured: false,
              maskedValue: null,
              placeholder: "xoxb-...",
            },
            {
              key: "signing_secret",
              label: "Signing Secret",
              required: false,
              configured: false,
              maskedValue: null,
              placeholder: "Slack Signing Secret",
            },
          ],
        },
        {
          name: "telegram",
          displayName: "Telegram Command Cloud",
          icon: "📱",
          description: "Telegram integration",
          connected: false,
          managed: false,
          authUrl: "https://t.me/BotFather",
          docsUrl: "https://core.telegram.org/bots",
          authMode: "oauth_and_api_key",
          lastActivity: null,
          messageCount: 0,
          credentialFields: [
            {
              key: "bot_token",
              label: "Bot Token",
              required: true,
              configured: true,
              maskedValue: "********1234",
              placeholder: "123456:AA...",
            },
          ],
        },
      ]),
    });
  });

  await page.route("**/api/platforms/slack/connect", async (route) => {
    slackConnectCalled = true;
    await route.fulfill({
      status: 400,
      contentType: "application/json",
      body: JSON.stringify({
        ok: false,
        error: "missing_required_credentials",
        message: "missing required credentials: Bot Token",
      }),
    });
  });

  await page.route("**/api/platforms/telegram/connect", async (route) => {
    connectCalled = true;
    const status = 200;
    markEndpoint(endpointMetrics, "connect", status);
    await route.fulfill({
      status,
      contentType: "application/json",
      body: JSON.stringify({
        ok: true,
        platform: "telegram",
        connected: true,
        message: "connected",
      }),
    });
  });

  await page.route("**/api/platforms/telegram/credentials", async (route) => {
    saveCalled = true;
    const status = 200;
    markEndpoint(endpointMetrics, "credentials", status);
    const payload = route.request().postDataJSON() as {
      values?: { bot_token?: string };
    };
    savedBotToken = payload.values?.bot_token ?? "";
    await route.fulfill({
      status,
      contentType: "application/json",
      body: JSON.stringify({
        ok: true,
        platform: "telegram",
        connected: true,
        message: "credentials_saved",
      }),
    });
  });

  await page.goto("/platforms", { waitUntil: "domcontentloaded" });

  await expect(
    page.getByRole("heading", { name: "プラットフォーム" }),
  ).toBeVisible();
  await expect(page.getByText("AgentFlow Sovereign Nexus")).toBeVisible();
  await expect(
    page.getByText(
      "このチャネルは内蔵プラットフォームです。追加認証は不要です。",
    ),
  ).toBeVisible();

  const cards = page.locator("div.glass-panel.p-6.space-y-4");
  await expect(cards).toHaveCount(3);
  await expect(
    cards.nth(0).getByRole("heading", { name: "AgentFlow Sovereign Nexus" }),
  ).toBeVisible();

  const slackCard = cards
    .filter({ hasText: "Slack Enterprise Bridge" })
    .first();
  await slackCard.getByRole("button", { name: "接続テスト" }).click();
  await expect(
    slackCard.getByText("接続前に必須認証情報を設定してください: Bot Token"),
  ).toBeVisible();
  await expect(slackCard.getByPlaceholder("xoxb-...")).toBeVisible();
  expect(slackConnectCalled).toBeFalsy();

  const telegramCard = cards
    .filter({ hasText: "Telegram Command Cloud" })
    .first();
  await expect(telegramCard.getByText("未接続")).toBeVisible();
  await expect(
    telegramCard.getByRole("link", { name: "認証ページ" }),
  ).toHaveAttribute("href", "https://t.me/BotFather");
  await expect(
    telegramCard.getByRole("link", { name: "設定ガイド" }),
  ).toHaveAttribute("href", "https://core.telegram.org/bots");

  await telegramCard.getByRole("button", { name: "接続テスト" }).click();
  await expect(
    telegramCard.getByText("接続試行に成功しました。"),
  ).toBeVisible();

  await telegramCard.getByRole("button", { name: "API Key入力" }).click();
  const tokenInput = telegramCard.getByPlaceholder("123456:AA...");
  await tokenInput.fill("   ");
  await telegramCard
    .getByRole("button", { name: "安全に保存して接続" })
    .click();
  await expect(
    telegramCard.getByText("保存する API Key / Secret を入力してください。"),
  ).toBeVisible();

  expect(saveCalled).toBeFalsy();

  await tokenInput.fill("123456789:ABCDTOKEN1234");

  await telegramCard.getByTitle("表示切替").click();
  await expect(tokenInput).toHaveAttribute("type", "text");
  await telegramCard.getByTitle("表示切替").click();
  await expect(tokenInput).toHaveAttribute("type", "password");

  await telegramCard
    .getByRole("button", { name: "安全に保存して接続" })
    .click();
  await expect(
    telegramCard.getByText("認証情報を保存し、接続を更新しました。"),
  ).toBeVisible();

  await page.getByRole("button", { name: "更新" }).click();

  await expect
    .poll(() => endpointMetrics.platforms.seen)
    .toBeGreaterThanOrEqual(4);
  await expect(tokenInput).toHaveValue("");

  expect(connectCalled).toBeTruthy();
  expect(slackConnectCalled).toBeFalsy();
  expect(saveCalled).toBeTruthy();
  expect(savedBotToken).toBe("123456789:ABCDTOKEN1234");
  expect(platformsFetchCount).toBeGreaterThanOrEqual(4);

  const endpointKeys: PlatformEndpoint[] = [
    "health",
    "platforms",
    "connect",
    "credentials",
  ];
  for (const endpoint of endpointKeys) {
    const metric = endpointMetrics[endpoint];
    expect(
      metric.seen,
      `${endpoint} should be called at least once`,
    ).toBeGreaterThan(0);
    expect(metric.nonOk, `${endpoint} should not return non-2xx status`).toBe(
      0,
    );
    expect(metric.ok, `${endpoint} should always return 2xx status`).toBe(
      metric.seen,
    );
  }

  expect(failedRequests).toEqual([]);
  expect(nonOkApiResponses).toEqual([]);
  await expect(page.getByText("API エラー", { exact: false })).toHaveCount(0);
});
