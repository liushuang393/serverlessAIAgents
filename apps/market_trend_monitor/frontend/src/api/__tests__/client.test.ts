/**
 * API クライアントのユニットテスト.
 * 
 * 目的: APIクライアントの各メソッドが正しく動作することを検証
 */

import { describe, it, expect } from 'vitest';
import { ApiError } from '../client';

describe('ApiClient', () => {
  describe('ApiError', () => {
    it('エラー情報を正しく保持する', () => {
      const error = new ApiError('Test error', 404, { detail: 'Not found' });

      expect(error.message).toBe('Test error');
      expect(error.statusCode).toBe(404);
      expect(error.details).toEqual({ detail: 'Not found' });
      expect(error.name).toBe('ApiError');
    });
  });
});

