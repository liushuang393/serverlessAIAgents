import { describe, expect, it } from 'vitest';

import {
  getEventNumber,
  getEventRecord,
  getEventResult,
  getEventString,
  resolveAgentTarget,
  type AGUIEvent,
} from './events';

describe('event helpers', () => {
  it('reads nested event data as a fallback', () => {
    const event: AGUIEvent = {
      event_type: 'progress',
      timestamp: Date.now(),
      flow_id: 'flow-1',
      data: {
        message: 'working',
        percentage: 42,
      },
    };

    expect(getEventString(event, 'message')).toBe('working');
    expect(getEventNumber(event, 'percentage')).toBe(42);
  });

  it('prefers top-level fields when available', () => {
    const event: AGUIEvent = {
      event_type: 'node.start',
      timestamp: Date.now(),
      flow_id: 'flow-1',
      node_id: 'agent-1',
      data: {
        node_id: 'ignored-agent',
      },
    };

    expect(resolveAgentTarget(event)).toBe('agent-1');
  });

  it('extracts result from top-level result first', () => {
    const event: AGUIEvent = {
      event_type: 'flow.complete',
      timestamp: Date.now(),
      flow_id: 'flow-1',
      result: {
        answer: 'ok',
      },
      data: {
        result: {
          answer: 'nested',
        },
      },
    };

    expect(getEventResult<{ answer: string }>(event)).toEqual({ answer: 'ok' });
  });

  it('falls back to the data payload when explicit result is absent', () => {
    const event: AGUIEvent = {
      event_type: 'flow.complete',
      timestamp: Date.now(),
      flow_id: 'flow-1',
      data: {
        report_id: 'r-1',
      },
    };

    expect(getEventResult<{ report_id: string }>(event)).toEqual({ report_id: 'r-1' });
  });

  it('resolves agent target from alternate identifiers', () => {
    const event: AGUIEvent = {
      event_type: 'progress',
      timestamp: Date.now(),
      flow_id: 'flow-1',
      data: {
        agent: 'GeoQA',
      },
    };

    expect(resolveAgentTarget(event)).toBe('GeoQA');
  });

  it('extracts structured payloads from top-level or nested records', () => {
    const event: AGUIEvent = {
      event_type: 'a2ui.component',
      timestamp: Date.now(),
      flow_id: 'flow-1',
      data: {
        component: {
          type: 'card',
        },
      },
    };

    expect(getEventRecord(event, 'component')).toEqual({ type: 'card' });
  });
});
