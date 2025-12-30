/**
 * Vitest 型定義拡張.
 * 
 * 目的: jest-dom マッチャーの型定義を追加
 */

import 'vitest';
import type { TestingLibraryMatchers } from '@testing-library/jest-dom/matchers';

declare module 'vitest' {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  interface Assertion<T = any> extends jest.Matchers<void, T>, TestingLibraryMatchers<T, void> {}
}

