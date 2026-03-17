import { fireEvent, render, screen } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import App from './App';

vi.mock('./lib/api', () => ({
  startExecution: vi.fn().mockResolvedValue({ task_id: 'geo-test', ws_url: '/api/ws/geo-test' }),
  fetchState: vi.fn().mockResolvedValue({
    task_id: 'geo-test',
    status: 'waiting_approval',
    current_stage: 'geo_qa',
    campaign_name: 'demo',
    package: 'assessment',
    request: {
      campaign_name: 'demo',
      package: 'assessment',
      targets: { industries: ['manufacturing'], legacy_stacks: ['COBOL'], regions: ['Japan'] },
    },
    events: [
      {
        event_type: 'approval_required',
        timestamp: new Date().toISOString(),
        task_id: 'geo-test',
        stage: 'publish_review',
        agent: 'GeoQA',
        message: 'Human approval required',
        payload: {},
      },
    ],
    artifacts: [],
    approvals: [
      {
        request_id: 'apr-1',
        task_id: 'geo-test',
        stage: 'publish_review',
        object_id: 'geo-test',
        risk_level: 'MEDIUM',
        reason: 'review',
        status: 'pending',
        actions: ['approve', 'reject', 'rewrite'],
        created_at: new Date().toISOString(),
        updated_at: new Date().toISOString(),
      },
    ],
    report: {
      task_id: 'geo-test',
      markdown: '# report',
      summary: {},
      created_at: new Date().toISOString(),
      updated_at: new Date().toISOString(),
    },
    published_pages: [],
    error_message: null,
  }),
  fetchArtifacts: vi.fn().mockResolvedValue({
    signal: {
      company: 'demo',
      signals: [],
      urgency_hypothesis: '高い',
      modernization_fit_score: 87,
    },
    draft: {
      pages: [
        {
          slug: 'demo-page',
          title: 'Draft Title',
          summary: 'Draft Summary',
          body_markdown: '# Draft',
          cta: 'CTA',
          faq_entries: [],
        },
      ],
      target_language: 'ja',
      unknowns: [],
    },
    qa: {
      pass_or_fail: 'REVIEW',
      issues: ['Need approval'],
      fix_instructions: [],
      publish_ready: true,
      risk_level: 'MEDIUM',
      metrics: {},
    },
    questionGraph: {
      personas: [{ role: 'cio', questions: [], high_intent_questions: ['Can we migrate?'] }],
    },
    evidenceMatrix: { entries: [] },
  }),
  submitApproval: vi.fn().mockResolvedValue({}),
  postCommand: vi.fn().mockResolvedValue({}),
}));

afterEach(() => {
  vi.clearAllMocks();
});

describe('App', () => {
  it('renders the shell and start action', () => {
    render(<App />);
    expect(screen.getByText('GEO Platform')).toBeInTheDocument();
    expect(screen.getByTestId('start-campaign-button')).toBeInTheDocument();
  });

  it('switches tabs and renders content studio', () => {
    render(<App />);
    fireEvent.click(screen.getByTestId('tab-content'));
    expect(screen.getByTestId('content-studio')).toBeInTheDocument();
  });
});
