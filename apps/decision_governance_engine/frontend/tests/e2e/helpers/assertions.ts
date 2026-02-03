import type { ConsoleMessage, Page } from "@playwright/test";
import { expect } from "@playwright/test";

export interface ErrorMonitor {
  assertNoErrors: () => Promise<void>;
}

const formatConsoleError = (message: ConsoleMessage): string => {
  const location = message.location();
  const locationText = location.url
    ? `${location.url}:${location.lineNumber}:${location.columnNumber}`
    : "unknown";
  return `${locationText} ${message.text()}`.trim();
};

export const installErrorMonitor = (page: Page): ErrorMonitor => {
  const consoleErrors: string[] = [];
  const pageErrors: string[] = [];

  page.on("console", (message) => {
    if (message.type() !== "error") {
      return;
    }
    consoleErrors.push(formatConsoleError(message));
  });

  page.on("pageerror", (error) => {
    pageErrors.push(error.message);
  });

  return {
    assertNoErrors: async () => {
      expect(consoleErrors, "コンソールエラーが検出されました").toEqual([]);
      expect(pageErrors, "ページエラーが検出されました").toEqual([]);
    },
  };
};
