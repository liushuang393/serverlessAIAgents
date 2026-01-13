/**
 * Vitest テストセットアップ.
 *
 * 目的: テスト環境の初期化とグローバルモックの設定
 */

import { vi } from 'vitest';

// ========================================
// グローバルモック
// ========================================

// fetch モック
globalThis.fetch = vi.fn();

// EventSource モック
class MockEventSource {
  static CONNECTING = 0;
  static OPEN = 1;
  static CLOSED = 2;

  readyState = MockEventSource.CONNECTING;
  onopen: ((event: Event) => void) | null = null;
  onmessage: ((event: MessageEvent) => void) | null = null;
  onerror: ((event: Event) => void) | null = null;

  constructor(public url: string) {
    // 接続をシミュレート
    setTimeout(() => {
      this.readyState = MockEventSource.OPEN;
      this.onopen?.(new Event('open'));
    }, 0);
  }

  close(): void {
    this.readyState = MockEventSource.CLOSED;
  }
}

globalThis.EventSource = MockEventSource as unknown as typeof EventSource;

// localStorage モック
const localStorageMock = {
  getItem: vi.fn(),
  setItem: vi.fn(),
  removeItem: vi.fn(),
  clear: vi.fn(),
  length: 0,
  key: vi.fn(),
};
globalThis.localStorage = localStorageMock as unknown as Storage;

// ========================================
// テスト後のクリーンアップ
// ========================================

afterEach(() => {
  vi.clearAllMocks();
});

