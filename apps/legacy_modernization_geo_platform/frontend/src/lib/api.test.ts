// @vitest-environment jsdom

import { afterEach, describe, expect, it } from 'vitest';

import { buildStreamUrl, buildWebSocketUrl } from './api';

describe('buildWebSocketUrl', () => {
  afterEach(() => {
    window.localStorage.clear();
  });

  it('converts http origins to websocket urls', () => {
    const url = buildWebSocketUrl('/api/ws/task-1');

    expect(url.startsWith('ws://')).toBe(true);
    expect(url.endsWith('/api/ws/task-1')).toBe(true);
  });

  it('passes the stored access token via query string', () => {
    window.localStorage.setItem('GEO_PLATFORM_ACCESS_TOKEN', 'secret-token');

    const url = new URL(buildWebSocketUrl('/api/ws/task-1?mode=live'));

    expect(url.protocol).toBe('ws:');
    expect(url.searchParams.get('mode')).toBe('live');
    expect(url.searchParams.get('access_token')).toBe('secret-token');
  });

  it('builds authenticated stream urls for eventsource', () => {
    window.localStorage.setItem('GEO_PLATFORM_ACCESS_TOKEN', 'stream-token');

    const url = new URL(buildStreamUrl('/api/geo/task-1/stream'));

    expect(url.pathname.endsWith('/api/geo/task-1/stream')).toBe(true);
    expect(url.searchParams.get('access_token')).toBe('stream-token');
  });
});
