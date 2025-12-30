/**
 * テストセットアップファイル.
 *
 * 目的: Vitest のグローバル設定
 */

import { expect, afterEach } from 'vitest';
import { cleanup } from '@testing-library/react';
import * as matchers from '@testing-library/jest-dom/matchers';

// jest-dom マッチャーを追加
expect.extend(matchers);

// 各テスト後にクリーンアップ
afterEach(() => {
  cleanup();
});

// ResizeObserver のモック (Recharts 用)
global.ResizeObserver = class ResizeObserver {
  observe() {}
  unobserve() {}
  disconnect() {}
};

