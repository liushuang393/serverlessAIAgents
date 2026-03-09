import { expect, test } from "playwright/test";

test("conversations page supports post/update/upload/export flows", async ({
  page,
}) => {
  await page.addInitScript(() => {
    window.localStorage.setItem("MESSAGING_HUB_API_KEY", "e2e-key");
  });

  const conversationId = "chat:e2e";
  const messages = [
    {
      message_id: "msg_1",
      conversation_id: conversationId,
      role: "user",
      content: "こんにちは",
      created_at: "2026-03-06T00:00:00Z",
      updated_at: "2026-03-06T00:00:00Z",
      metadata: {},
    },
    {
      message_id: "msg_2",
      conversation_id: conversationId,
      role: "assistant",
      content: "初期応答",
      created_at: "2026-03-06T00:00:01Z",
      updated_at: "2026-03-06T00:00:01Z",
      metadata: {},
    },
  ];
  let uploadCalled = false;
  let updateCalled = false;

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

  await page.route("**/api/sr_chat/auth.test", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({ ok: true }),
    });
  });

  await page.route("**/api/sr_chat/conversations.list", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({
        ok: true,
        conversations: [
          {
            conversation_id: conversationId,
            message_count: messages.length,
            last_message_at: messages[messages.length - 1].updated_at,
          },
        ],
        total: 1,
      }),
    });
  });

  await page.route("**/api/sr_chat/conversations.history**", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({
        ok: true,
        conversation_id: conversationId,
        messages: [...messages].reverse(),
        total: messages.length,
      }),
    });
  });

  await page.route("**/api/sr_chat/events.subscribe", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({
        ok: true,
        subscription_id: "sub-test",
        events: [],
        ws_url: "",
      }),
    });
  });

  await page.route("**/api/sr_chat/chat.postMessage", async (route) => {
    const payload = route.request().postDataJSON() as {
      text: string;
      user_id: string;
      conversation_id: string;
    };
    messages.push(
      {
        message_id: `msg_u_${messages.length}`,
        conversation_id: payload.conversation_id,
        role: "user",
        content: payload.text,
        created_at: "2026-03-06T00:01:00Z",
        updated_at: "2026-03-06T00:01:00Z",
        metadata: { user_id: payload.user_id },
      },
      {
        message_id: `msg_a_${messages.length}`,
        conversation_id: payload.conversation_id,
        role: "assistant",
        content: `echo:${payload.text}`,
        created_at: "2026-03-06T00:01:01Z",
        updated_at: "2026-03-06T00:01:01Z",
        metadata: {},
      },
    );
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({
        ok: true,
        conversation_id: payload.conversation_id,
        run_id: "run-e2e",
        message: {
          id: "msg_new",
          role: "assistant",
          text: `echo:${payload.text}`,
        },
      }),
    });
  });

  await page.route("**/api/sr_chat/chat.update", async (route) => {
    updateCalled = true;
    const payload = route.request().postDataJSON() as {
      message_id: string;
      text: string;
    };
    const target = messages.find(
      (item) => item.message_id === payload.message_id,
    );
    if (target) {
      target.content = payload.text;
      target.updated_at = "2026-03-06T00:02:00Z";
    }
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({ ok: true, message_id: payload.message_id }),
    });
  });

  await page.route("**/api/sr_chat/files.upload", async (route) => {
    uploadCalled = true;
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({
        ok: true,
        run_id: "file-run",
        file_name: "note.txt",
      }),
    });
  });

  await page.route("**/api/sr_chat/export**", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({
        ok: true,
        format: "json",
        filename: "sr_chat_chat_e2e.json",
        data: '{"items":[]}',
      }),
    });
  });

  await page.goto("/conversations", { waitUntil: "domcontentloaded" });

  await expect(page.getByText("初期応答")).toBeVisible();

  await page.getByPlaceholder("メッセージを入力...").fill("テスト送信");
  await page.getByRole("button", { name: "送信" }).click();
  await expect(page.getByText("echo:テスト送信")).toBeVisible();

  await page.getByTitle("メッセージを編集").first().click();
  await page.locator("textarea").nth(1).fill("更新済み応答");
  await page.getByRole("button", { name: "更新" }).nth(1).click();
  await expect(page.getByText("更新済み応答")).toBeVisible();
  expect(updateCalled).toBeTruthy();

  await page.locator('input[type="file"]').setInputFiles({
    name: "note.txt",
    mimeType: "text/plain",
    buffer: Buffer.from("hello"),
  });
  await expect.poll(() => uploadCalled).toBeTruthy();

  const downloadPromise = page.waitForEvent("download");
  await page.getByRole("button", { name: "エクスポート", exact: true }).click();
  const download = await downloadPromise;
  expect(download.suggestedFilename()).toBe("sr_chat_chat_e2e.json");
});
