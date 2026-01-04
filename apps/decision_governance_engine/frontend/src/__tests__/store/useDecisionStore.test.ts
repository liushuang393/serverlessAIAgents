/**
 * useDecisionStore テスト.
 * 
 * 目的: 状態管理ロジックの検証
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { useDecisionStore } from '../../store/useDecisionStore';
import { act } from 'react';

describe('useDecisionStore', () => {
  beforeEach(() => {
    // ストアをリセット
    act(() => {
      useDecisionStore.getState().reset();
    });
  });

  describe('初期状態', () => {
    it('デフォルト値が正しく設定されている', () => {
      const state = useDecisionStore.getState();
      
      expect(state.currentPage).toBe('input');
      expect(state.question).toBe('');
      expect(state.constraints.budget).toBe('');
      expect(state.constraints.timeline).toBe('');
      expect(state.constraints.team).toBe('');
      expect(state.constraints.technical).toEqual([]);
      expect(state.constraints.regulatory).toEqual([]);
      expect(state.reportId).toBeNull();
      expect(state.report).toBeNull();
      expect(state.error).toBeNull();
    });
  });

  describe('setQuestion', () => {
    it('質問を設定できる', () => {
      act(() => {
        useDecisionStore.getState().setQuestion('新規事業への投資判断');
      });
      
      expect(useDecisionStore.getState().question).toBe('新規事業への投資判断');
    });
  });

  describe('setConstraints', () => {
    it('制約条件を部分的に更新できる', () => {
      act(() => {
        useDecisionStore.getState().setConstraints({ budget: '500' });
      });
      
      const state = useDecisionStore.getState();
      expect(state.constraints.budget).toBe('500');
      expect(state.constraints.timeline).toBe(''); // 他は変更なし
    });

    it('技術制約を追加できる', () => {
      act(() => {
        useDecisionStore.getState().setConstraints({ technical: ['AWS', 'Python'] });
      });
      
      expect(useDecisionStore.getState().constraints.technical).toEqual(['AWS', 'Python']);
    });
  });

  describe('setPage', () => {
    it('ページを切り替えできる', () => {
      act(() => {
        useDecisionStore.getState().setPage('processing');
      });
      
      expect(useDecisionStore.getState().currentPage).toBe('processing');
    });
  });

  describe('buildRequest', () => {
    it('リクエストオブジェクトを正しく構築する', () => {
      act(() => {
        const store = useDecisionStore.getState();
        store.setQuestion('テスト質問です。これは十分な長さです。');
        store.setConstraints({
          budget: '500',
          timeline: '6',
          technical: ['AWS'],
          regulatory: ['GDPR'],
        });
      });
      
      const request = useDecisionStore.getState().buildRequest();
      
      expect(request.question).toBe('テスト質問です。これは十分な長さです。');
      expect(request.budget).toBe(500);
      expect(request.timeline_months).toBe(6);
      expect(request.technical_constraints).toEqual(['AWS']);
      expect(request.regulatory_constraints).toEqual(['GDPR']);
    });

    it('空の制約は変換しない', () => {
      act(() => {
        useDecisionStore.getState().setQuestion('テスト質問です。');
      });
      
      const request = useDecisionStore.getState().buildRequest();
      
      expect(request.budget).toBeUndefined();
      expect(request.timeline_months).toBeUndefined();
    });
  });

  describe('reset', () => {
    it('全ての状態をリセットする', () => {
      // 状態を変更
      act(() => {
        const store = useDecisionStore.getState();
        store.setQuestion('テスト');
        store.setConstraints({ budget: '100' });
        store.setPage('report');
        store.setReportId('test-id');
      });
      
      // リセット
      act(() => {
        useDecisionStore.getState().reset();
      });
      
      const state = useDecisionStore.getState();
      expect(state.currentPage).toBe('input');
      expect(state.question).toBe('');
      expect(state.constraints.budget).toBe('');
      expect(state.reportId).toBeNull();
    });
  });
});

