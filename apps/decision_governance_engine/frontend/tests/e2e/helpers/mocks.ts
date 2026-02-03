import type { Page } from "@playwright/test";

export type AuthState = "authenticated" | "unauthenticated";

export interface EventSourceMockController {
  getUrls: () => Promise<string[]>;
  emitMessage: (data: unknown, target?: string | number) => Promise<void>;
  emitEvent: (name: string, data: unknown, target?: string | number) => Promise<void>;
  emitError: (target?: string | number) => Promise<void>;
  close: (target?: string | number) => Promise<void>;
}

export interface RAGSourceConfig {
  name: string;
  enabled: boolean;
  directory?: string;
  url?: string;
}

export interface AgentRAGConfig {
  agent_id: string;
  agent_name: string;
  use_rag: boolean;
  rag_sources: RAGSourceConfig[];
  top_k: number;
  min_similarity: number;
}

export interface ConfigMockOptions {
  configs?: AgentRAGConfig[];
}

export interface KnowledgeDoc {
  id: string;
  content: string;
  topic: string;
  metadata?: Record<string, unknown>;
}

export interface KnowledgeMockOptions {
  shu?: KnowledgeDoc[];
  qi?: KnowledgeDoc[];
}

export interface ServerHistoryItem {
  id: string;
  request_id: string;
  question: string;
  decision_role: "GO" | "NO_GO" | "DELAY" | "PILOT" | "REJECTED";
  confidence: number | null;
  mode: "FAST" | "STANDARD" | "AUDIT";
  created_at: string;
}

export interface HistoryDetailData {
  id: string;
  request_id: string;
  question: string;
  decision_role: string;
  confidence: number | null;
  mode: string;
  dao_result: Record<string, unknown> | null;
  fa_result: Record<string, unknown> | null;
  shu_result: Record<string, unknown> | null;
  qi_result: Record<string, unknown> | null;
  review_result: Record<string, unknown> | null;
  summary_bullets: string[] | null;
  warnings: string[] | null;
  processing_time_ms: number | null;
  created_at: string;
}

export interface HistoryMockOptions {
  items?: ServerHistoryItem[];
  details?: Record<string, HistoryDetailData>;
}

export interface ReportMockOptions {
  signature?: {
    signed?: boolean;
    forceUnauthorized?: boolean;
  };
}

declare global {
  interface Window {
    __mockEventSource?: {
      getUrls: () => string[];
      emitMessage: (data: unknown, target?: string | number) => void;
      emitEvent: (name: string, data: unknown, target?: string | number) => void;
      emitError: (target?: string | number) => void;
      close: (target?: string | number) => void;
    };
  }
}

export interface AuthUser {
  user_id: string;
  username: string;
  display_name: string;
  department: string;
  position: string;
  created_at: string;
}

export interface AuthMockOptions {
  initialState?: AuthState;
  user?: Partial<AuthUser>;
}

export interface AuthMockController {
  setAuthenticated: (state: AuthState) => void;
  setUser: (user: Partial<AuthUser>) => void;
  getUser: () => AuthUser;
}

const defaultUser: AuthUser = {
  user_id: "user-admin-001",
  username: "admin",
  display_name: "管理者 太郎",
  department: "経営企画部",
  position: "管理者",
  created_at: "2026-02-03T00:00:00Z",
};

const buildUser = (override?: Partial<AuthUser>): AuthUser => ({
  ...defaultUser,
  ...override,
});

const defaultConfigs: AgentRAGConfig[] = [
  {
    agent_id: "shu",
    agent_name: "術",
    use_rag: true,
    rag_sources: [
      { name: "社内計画", enabled: true, directory: "/knowledge/shu" },
      { name: "外部市場", enabled: false, url: "https://example.com" },
    ],
    top_k: 5,
    min_similarity: 0.7,
  },
  {
    agent_id: "qi",
    agent_name: "器",
    use_rag: false,
    rag_sources: [],
    top_k: 3,
    min_similarity: 0.6,
  },
];

const buildConfigs = (override?: AgentRAGConfig[]): AgentRAGConfig[] =>
  override && override.length > 0
    ? override.map((config) => ({ ...config }))
    : defaultConfigs.map((config) => ({ ...config }));

const defaultKnowledgeDocs: Record<"shu" | "qi", KnowledgeDoc[]> = {
  shu: [
    {
      id: "shu-001",
      topic: "industry_practices",
      content: "標準的な実行計画のテンプレートを参照する。",
    },
  ],
  qi: [
    {
      id: "qi-001",
      topic: "technical_docs",
      content: "技術スタックの互換性を確認する。",
    },
  ],
};

const buildKnowledgeDocs = (
  override: KnowledgeMockOptions = {}
): Record<"shu" | "qi", KnowledgeDoc[]> => ({
  shu: override.shu ? override.shu.map((doc) => ({ ...doc })) : [...defaultKnowledgeDocs.shu],
  qi: override.qi ? override.qi.map((doc) => ({ ...doc })) : [...defaultKnowledgeDocs.qi],
});

const defaultHistoryItems: ServerHistoryItem[] = [
  {
    id: "history-001",
    request_id: "request-001",
    question: "新規事業AとBのどちらに投資すべきか",
    decision_role: "GO",
    confidence: 0.72,
    mode: "STANDARD",
    created_at: "2026-02-03T00:00:00Z",
  },
  {
    id: "history-002",
    request_id: "request-002",
    question: "海外市場への進出タイミングを判断したい",
    decision_role: "DELAY",
    confidence: 0.58,
    mode: "FAST",
    created_at: "2026-02-02T00:00:00Z",
  },
];

const buildHistoryItems = (override?: ServerHistoryItem[]): ServerHistoryItem[] =>
  override && override.length > 0
    ? override.map((item) => ({ ...item }))
    : defaultHistoryItems.map((item) => ({ ...item }));

const buildHistoryDetails = (
  items: ServerHistoryItem[],
  override: Record<string, HistoryDetailData> = {}
): Record<string, HistoryDetailData> => {
  const baseDetails = items.reduce<Record<string, HistoryDetailData>>((acc, item) => {
    acc[item.request_id] = {
      id: item.id,
      request_id: item.request_id,
      question: item.question,
      decision_role: item.decision_role,
      confidence: item.confidence,
      mode: item.mode,
      dao_result: null,
      fa_result: null,
      shu_result: null,
      qi_result: null,
      review_result: null,
      summary_bullets: null,
      warnings: null,
      processing_time_ms: null,
      created_at: item.created_at,
    };
    return acc;
  }, {});

  return { ...baseDetails, ...override };
};

export const setupAuthMocks = async (
  page: Page,
  options: AuthMockOptions = {}
): Promise<AuthMockController> => {
  let isAuthenticated = options.initialState === "authenticated";
  let currentUser = buildUser(options.user);

  await page.addInitScript(() => {
    localStorage.clear();
  });

  await page.route("**/api/auth/me", async (route) => {
    const response = isAuthenticated
      ? { success: true, message: "認証済み", user: currentUser }
      : { success: false, message: "未認証", user: null };

    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify(response),
    });
  });

  await page.route("**/api/auth/login", async (route) => {
    isAuthenticated = true;
    const response = { success: true, message: "ログイン成功", user: currentUser };

    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify(response),
    });
  });

  return {
    setAuthenticated: (state) => {
      isAuthenticated = state === "authenticated";
    },
    setUser: (user) => {
      currentUser = { ...currentUser, ...user };
    },
    getUser: () => currentUser,
  };
};

export const setupConfigMocks = async (
  page: Page,
  options: ConfigMockOptions = {}
): Promise<void> => {
  let configs = buildConfigs(options.configs);

  await page.route("**/api/config/rag", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify(configs),
    });
  });

  await page.route("**/api/config/rag/*", async (route) => {
    const request = route.request();
    const agentId = request.url().split("/api/config/rag/")[1] || "";
    const payload = request.postDataJSON() as Partial<AgentRAGConfig> | null;

    configs = configs.map((config) =>
      config.agent_id === agentId
        ? {
            ...config,
            ...(payload ?? {}),
          }
        : config
    );

    const updated = configs.find((config) => config.agent_id === agentId);

    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({ success: true, config: updated ?? null }),
    });
  });
};

export const setupKnowledgeMocks = async (
  page: Page,
  options: KnowledgeMockOptions = {}
): Promise<void> => {
  const docsByAgent = buildKnowledgeDocs(options);

  await page.route("**/api/knowledge/**", async (route) => {
    const request = route.request();
    const url = request.url();
    const method = request.method();
    const agentType = url.includes("/api/knowledge/qi") ? "qi" : "shu";

    if (method === "GET") {
      await route.fulfill({
        status: 200,
        contentType: "application/json",
        body: JSON.stringify({ documents: docsByAgent[agentType] }),
      });
      return;
    }

    if (method === "POST") {
      const payload = request.postDataJSON() as { content?: string; topic?: string } | null;
      const newDoc: KnowledgeDoc = {
        id: `${agentType}-${Date.now()}`,
        content: payload?.content ?? "",
        topic: payload?.topic ?? "default",
      };
      docsByAgent[agentType] = [...docsByAgent[agentType], newDoc];

      await route.fulfill({
        status: 200,
        contentType: "application/json",
        body: JSON.stringify({ success: true, document: newDoc }),
      });
      return;
    }

    if (method === "DELETE") {
      const docId = url.split(`/api/knowledge/${agentType}/`)[1] || "";
      docsByAgent[agentType] = docsByAgent[agentType].filter((doc) => doc.id !== docId);

      await route.fulfill({
        status: 200,
        contentType: "application/json",
        body: JSON.stringify({ success: true }),
      });
      return;
    }

    await route.fulfill({
      status: 405,
      contentType: "application/json",
      body: JSON.stringify({ success: false, message: "Method Not Allowed" }),
    });
  });
};

export const setupHistoryMocks = async (
  page: Page,
  options: HistoryMockOptions = {}
): Promise<void> => {
  let items = buildHistoryItems(options.items);
  let details = buildHistoryDetails(items, options.details);

  await page.route("**/api/decision/history**", async (route) => {
    const request = route.request();
    const url = new URL(request.url());
    const method = request.method();
    const basePath = "/api/decision/history";
    const pathname = url.pathname;
    const isListRequest = pathname === basePath || pathname === `${basePath}/`;

    if (method === "GET" && isListRequest) {
      await route.fulfill({
        status: 200,
        contentType: "application/json",
        body: JSON.stringify({ status: "success", items, total: items.length }),
      });
      return;
    }

    if (method === "GET" && pathname.startsWith(`${basePath}/`)) {
      const requestId = pathname.slice(`${basePath}/`.length);
      const detail = details[requestId];
      await route.fulfill({
        status: 200,
        contentType: "application/json",
        body: JSON.stringify({ status: "success", data: detail ?? null }),
      });
      return;
    }

    if (method === "DELETE" && pathname.startsWith(`${basePath}/`)) {
      const requestId = pathname.slice(`${basePath}/`.length);
      items = items.filter((item) => item.request_id !== requestId);
      details = buildHistoryDetails(items, options.details);

      await route.fulfill({
        status: 200,
        contentType: "application/json",
        body: JSON.stringify({ status: "success", message: "deleted" }),
      });
      return;
    }

    await route.fulfill({
      status: 405,
      contentType: "application/json",
      body: JSON.stringify({ status: "error", message: "Method Not Allowed" }),
    });
  });
};

export const setupReportMocks = async (
  page: Page,
  options: ReportMockOptions = {}
): Promise<void> => {
  const signed = options.signature?.signed ?? false;
  const forceUnauthorized = options.signature?.forceUnauthorized ?? false;

  const signaturePayload = {
    report_id: "report-001",
    signed_by: "管理者 太郎",
    signer_id: "user-admin-001",
    department: "経営企画部",
    position: "管理者",
    signed_at: "2026-02-03T00:00:00Z",
    signed_at_display: "2026/02/03 09:00",
  };

  const pdfBytes = new Uint8Array([
    0x25, 0x50, 0x44, 0x46, 0x2d, 0x31, 0x2e, 0x33, 0x0a,
    0x25, 0xe2, 0xe3, 0xcf, 0xd3, 0x0a,
    0x31, 0x20, 0x30, 0x20, 0x6f, 0x62, 0x6a, 0x0a,
    0x3c, 0x3c, 0x3e, 0x3e, 0x0a,
    0x65, 0x6e, 0x64, 0x6f, 0x62, 0x6a, 0x0a,
    0x74, 0x72, 0x61, 0x69, 0x6c, 0x65, 0x72, 0x0a,
    0x3c, 0x3c, 0x3e, 0x3e, 0x0a,
    0x25, 0x25, 0x45, 0x4f, 0x46,
  ]);

  await page.route("**/api/report/*/pdf", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/pdf",
      body: Buffer.from(pdfBytes),
    });
  });

  await page.route("**/api/report/*/sign", async (route) => {
    if (forceUnauthorized) {
      await route.fulfill({
        status: 401,
        contentType: "application/json",
        body: JSON.stringify({ success: false, message: "unauthorized" }),
      });
      return;
    }

    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({
        success: true,
        message: "署名しました",
        signature: signaturePayload,
      }),
    });
  });

  await page.route("**/api/report/*/signature", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({
        signed,
        ...(signed ? { signature: signaturePayload } : {}),
      }),
    });
  });
};

export const installMockEventSource = async (
  page: Page
): Promise<EventSourceMockController> => {
  await page.addInitScript(() => {
    if (window.__mockEventSource) {
      return;
    }

    const controller = {
      _instances: [] as Array<{
        url: string;
        readyState: number;
        close: () => void;
        _emitMessage: (type: string, data: string) => void;
        _emitError: () => void;
      }>,
      _urls: [] as string[],
      getUrls: () => [...controller._urls],
      _resolveTargets: (target?: string | number) => {
        if (typeof target === "number") {
          return controller._instances[target] ? [controller._instances[target]] : [];
        }
        if (typeof target === "string") {
          return controller._instances.filter((instance) => instance.url === target);
        }
        return controller._instances;
      },
      emitMessage: (data: unknown, target?: string | number) => {
        const payload = typeof data === "string" ? data : JSON.stringify(data);
        controller
          ._resolveTargets(target)
          .forEach((instance) => instance._emitMessage("message", payload));
      },
      emitEvent: (name: string, data: unknown, target?: string | number) => {
        const payload = typeof data === "string" ? data : JSON.stringify(data);
        controller
          ._resolveTargets(target)
          .forEach((instance) => instance._emitMessage(name, payload));
      },
      emitError: (target?: string | number) => {
        controller._resolveTargets(target).forEach((instance) => instance._emitError());
      },
      close: (target?: string | number) => {
        controller._resolveTargets(target).forEach((instance) => instance.close());
      },
    };

    class MockEventSource {
      static CONNECTING = 0;
      static OPEN = 1;
      static CLOSED = 2;

      readyState = MockEventSource.CONNECTING;
      onopen: ((event: Event) => void) | null = null;
      onmessage: ((event: MessageEvent) => void) | null = null;
      onerror: ((event: Event) => void) | null = null;
      private listeners: Record<string, Set<(event: Event | MessageEvent) => void>> = {};

      constructor(public url: string) {
        controller._instances.push(this);
        controller._urls.push(url);

        setTimeout(() => {
          if (this.readyState === MockEventSource.CLOSED) {
            return;
          }
          this.readyState = MockEventSource.OPEN;
          const openEvent = new Event("open");
          this.onopen?.(openEvent);
          this.emitToListeners("open", openEvent);
        }, 0);
      }

      addEventListener(type: string, handler: (event: Event | MessageEvent) => void): void {
        if (!this.listeners[type]) {
          this.listeners[type] = new Set();
        }
        this.listeners[type].add(handler);
      }

      removeEventListener(type: string, handler: (event: Event | MessageEvent) => void): void {
        this.listeners[type]?.delete(handler);
      }

      close(): void {
        this.readyState = MockEventSource.CLOSED;
      }

      emitToListeners(type: string, event: Event | MessageEvent): void {
        this.listeners[type]?.forEach((handler) => handler(event));
      }

      _emitMessage(type: string, data: string): void {
        const messageEvent = new MessageEvent(type, { data });
        if (type === "message") {
          this.onmessage?.(messageEvent);
        }
        this.emitToListeners(type, messageEvent);
      }

      _emitError(): void {
        const errorEvent = new Event("error");
        this.onerror?.(errorEvent);
        this.emitToListeners("error", errorEvent);
      }
    }

    window.__mockEventSource = {
      getUrls: controller.getUrls,
      emitMessage: controller.emitMessage,
      emitEvent: controller.emitEvent,
      emitError: controller.emitError,
      close: controller.close,
    };

    window.EventSource = MockEventSource as typeof EventSource;
  });

  return {
    getUrls: () => page.evaluate(() => window.__mockEventSource?.getUrls() ?? []),
    emitMessage: (data, target) =>
      page.evaluate(({ data, target }) => {
        window.__mockEventSource?.emitMessage(data, target);
      }, { data, target }),
    emitEvent: (name, data, target) =>
      page.evaluate(({ name, data, target }) => {
        window.__mockEventSource?.emitEvent(name, data, target);
      }, { name, data, target }),
    emitError: (target) =>
      page.evaluate((targetArg) => {
        window.__mockEventSource?.emitError(targetArg);
      }, target),
    close: (target) =>
      page.evaluate((targetArg) => {
        window.__mockEventSource?.close(targetArg);
      }, target),
  };
};
