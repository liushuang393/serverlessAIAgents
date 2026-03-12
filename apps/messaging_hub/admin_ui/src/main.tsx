import React from "react";
import ReactDOM from "react-dom/client";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { BrowserRouter } from "react-router-dom";
import App from "./App";
import {
  API_ERROR_EVENT,
  getMessagingHubApiKey,
  type ApiErrorEventDetail,
} from "./shared/auth";
import "./index.css";

function isApiRequest(input: RequestInfo | URL): boolean {
  const rawUrl =
    typeof input === "string"
      ? input
      : input instanceof URL
        ? input.toString()
        : input.url;
  const resolved = new URL(rawUrl, window.location.href);
  return resolved.pathname.startsWith("/api");
}

function dispatchApiError(detail: ApiErrorEventDetail): void {
  window.dispatchEvent(
    new CustomEvent<ApiErrorEventDetail>(API_ERROR_EVENT, { detail }),
  );
}

async function readErrorMessage(response: Response): Promise<string> {
  const body = await response.clone().text();
  if (!body) {
    return `${response.status} ${response.statusText}`;
  }
  try {
    const parsed = JSON.parse(body) as { detail?: string; error?: string };
    if (typeof parsed.detail === "string" && parsed.detail.trim()) {
      return parsed.detail;
    }
    if (typeof parsed.error === "string" && parsed.error.trim()) {
      return parsed.error;
    }
  } catch {
    // no-op
  }
  return body.slice(0, 280);
}

const nativeFetch = window.fetch.bind(window);
window.fetch = async (
  input: RequestInfo | URL,
  init?: RequestInit,
): Promise<Response> => {
  const apiKey = getMessagingHubApiKey();
  const headers = new Headers(init?.headers);
  if (apiKey && !headers.has("x-api-key")) {
    headers.set("x-api-key", apiKey);
  }

  try {
    const response = await nativeFetch(input, { ...init, headers });
    if (
      isApiRequest(input) &&
      (response.status === 401 || response.status === 503)
    ) {
      dispatchApiError({
        status: response.status,
        url: response.url,
        message: await readErrorMessage(response),
      });
    }
    return response;
  } catch (error) {
    if (isApiRequest(input)) {
      dispatchApiError({
        status: 0,
        url:
          typeof input === "string"
            ? input
            : input instanceof URL
              ? input.toString()
              : input.url,
        message: error instanceof Error ? error.message : "Network error",
      });
    }
    throw error;
  }
};

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      staleTime: 5000,
      refetchOnWindowFocus: false,
    },
  },
});

const SW_CACHE_PREFIX = "messaging-hub-shell-";

async function unregisterServiceWorkersForDev(): Promise<void> {
  const registrations = await navigator.serviceWorker.getRegistrations();
  await Promise.all(registrations.map((registration) => registration.unregister()));
  if (!("caches" in window)) {
    return;
  }
  const cacheKeys = await window.caches.keys();
  await Promise.all(
    cacheKeys
      .filter((key) => key.startsWith(SW_CACHE_PREFIX))
      .map((key) => window.caches.delete(key)),
  );
}

function setupServiceWorker(): void {
  if (!("serviceWorker" in navigator)) {
    return;
  }
  const bootstrapModules = Array.from(
    document.querySelectorAll<HTMLScriptElement>('script[type="module"][src]'),
  );
  const moduleScriptSources = bootstrapModules.map(
    (script) => script.getAttribute("src") ?? "",
  );
  const isDevServerRuntime =
    moduleScriptSources.some(
      (src) => src.startsWith("/@vite/client") || src.startsWith("/src/"),
    );

  window.addEventListener("load", () => {
    if (!isDevServerRuntime) {
      void navigator.serviceWorker.register("/sw.js", { scope: "/" });
      return;
    }
    void unregisterServiceWorkersForDev();
  });
}

setupServiceWorker();

ReactDOM.createRoot(document.getElementById("root")!).render(
  <React.StrictMode>
    <QueryClientProvider client={queryClient}>
      <BrowserRouter
        future={{
          v7_startTransition: true,
          v7_relativeSplatPath: true,
        }}
      >
        <App />
      </BrowserRouter>
    </QueryClientProvider>
  </React.StrictMode>,
);
