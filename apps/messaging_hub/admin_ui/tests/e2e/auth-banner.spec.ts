import { expect, test } from "playwright/test";

test("shows auth guidance and uses stored API key", async ({ page }) => {
  let keyAttached = false;

  await page.route("**/api/health", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({
        status: "healthy",
        auth_required: true,
        auth_key_configured: true,
        auth_env_var: "MESSAGING_HUB_API_KEY",
      }),
    });
  });

  await page.route("**/api/admin-key", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({
        api_key: null,
        auth_required: true,
        configured: true,
      }),
    });
  });

  await page.route("**/api/sr_chat/auth.test", async (route) => {
    const apiKey = route.request().headers()["x-api-key"];
    const ok = apiKey === "e2e-key";
    await route.fulfill({
      status: ok ? 200 : 401,
      contentType: "application/json",
      body: JSON.stringify(ok ? { ok: true } : { detail: "Invalid API key" }),
    });
  });

  await page.route("**/api/sr_chat/conversations.list", async (route) => {
    const apiKey = route.request().headers()["x-api-key"];
    keyAttached = keyAttached || apiKey === "e2e-key";
    const ok = apiKey === "e2e-key";
    await route.fulfill({
      status: ok ? 200 : 401,
      contentType: "application/json",
      body: JSON.stringify(
        ok
          ? {
              ok: true,
              conversations: [
                {
                  conversation_id: "chat:e2e",
                  message_count: 2,
                  last_message_at: "2026-03-06T00:00:00Z",
                },
              ],
              total: 1,
            }
          : { detail: "Invalid API key" },
      ),
    });
  });

  await page.route("**/api/sr_chat/conversations.history**", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({
        ok: true,
        conversation_id: "chat:e2e",
        messages: [],
        total: 0,
      }),
    });
  });

  await page.route("**/api/sr_chat/events.subscribe", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({
        ok: true,
        subscription_id: "sub-e2e",
        events: [],
        ws_url: "",
      }),
    });
  });

  await page.goto("/conversations", { waitUntil: "domcontentloaded" });

  await expect(page.getByText("API 認証が必要です")).toBeVisible();

  await page.getByPlaceholder("MESSAGING_HUB_API_KEY を入力").fill("e2e-key");
  await page.getByRole("button", { name: "保存" }).click();

  await page.getByRole("button", { name: "更新" }).first().click();
  await expect(page.getByRole("button", { name: /chat:e2e/ })).toBeVisible();
  expect(keyAttached).toBeTruthy();
});
