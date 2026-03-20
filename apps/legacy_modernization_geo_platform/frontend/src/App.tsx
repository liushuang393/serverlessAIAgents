import { type ReactNode, useEffect, useMemo, useRef, useState } from 'react';
import {
  buildStreamUrl,
  fetchArtifacts,
  fetchState,
  postCommand,
  startExecution,
  submitApproval,
} from './lib/api';
import { LocaleSwitcher, useI18n } from './i18n';
import type {
  ApprovalRecord,
  ContentDraftArtifact,
  EvidenceMatrixArtifact,
  GeoQAReport,
  QuestionGraphArtifact,
  TaskEvent,
  TaskStateResponse,
} from './types';

type TabKey = 'console' | 'workspace' | 'content' | 'approval' | 'report';
type SurfaceKey = 'workspace' | 'content' | 'approval' | 'report';

interface StreamEvent {
  event_type: string;
  timestamp: number;
  flow_id: string;
  data?: Record<string, unknown>;
  [key: string]: unknown;
}

interface A2UIComponentNode {
  type: string;
  id?: string;
  props?: Record<string, unknown>;
  children?: A2UIComponentNode[];
  style?: Record<string, unknown>;
}

const DEFAULT_FORM = {
  campaign_name: 'legacy-modernization-japan-b2b',
  package: 'assessment',
  targets: {
    industries: ['manufacturing'],
    legacy_stacks: ['COBOL', 'Struts'],
    regions: ['Japan'],
  },
};

function createEmptySurfaces(): Record<SurfaceKey, A2UIComponentNode[]> {
  return {
    workspace: [],
    content: [],
    approval: [],
    report: [],
  };
}

function readEventField(event: StreamEvent, key: string): unknown {
  if (event[key] !== undefined) {
    return event[key];
  }
  return event.data?.[key];
}

function readEventString(event: StreamEvent, ...keys: string[]): string | undefined {
  for (const key of keys) {
    const value = readEventField(event, key);
    if (typeof value === 'string' && value.trim()) {
      return value;
    }
  }
  return undefined;
}

function readEventRecord(event: StreamEvent, key: string): Record<string, unknown> | null {
  const value = readEventField(event, key);
  if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
    return value as Record<string, unknown>;
  }
  return null;
}

function toTimelineEvent(event: StreamEvent, taskId: string): TaskEvent {
  const message =
    readEventString(event, 'message', 'reason', 'error_message') || event.event_type;

  return {
    event_type: event.event_type,
    timestamp: new Date(event.timestamp * 1000).toISOString(),
    task_id: taskId,
    stage: readEventString(event, 'stage') ?? null,
    agent: readEventString(event, 'agent', 'node_name') ?? null,
    message,
    payload: event.data ?? {},
  };
}

function replaceSurfaceComponent(
  components: A2UIComponentNode[],
  nextComponent: A2UIComponentNode,
): A2UIComponentNode[] {
  const nextId = nextComponent.id;
  if (!nextId) {
    return [...components, nextComponent];
  }
  const index = components.findIndex((component) => component.id === nextId);
  if (index === -1) {
    return [...components, nextComponent];
  }
  return components.map((component, componentIndex) =>
    componentIndex === index ? nextComponent : component,
  );
}

function renderA2UIComponent(
  component: A2UIComponentNode,
  keyPrefix: string,
  t: (key: string, params?: Record<string, string>) => string,
): ReactNode {
  const componentKey = component.id || keyPrefix;
  const props = component.props || {};
  const children = component.children || [];

  if (component.type === 'text') {
    const content =
      typeof props.content === 'string' ? props.content : typeof props.text === 'string' ? props.text : '';
    return (
      <pre key={componentKey} className="a2ui-text">
        {content}
      </pre>
    );
  }

  if (component.type === 'card') {
    const title = typeof props.title === 'string' ? props.title : t('common.surfaceCard');
    return (
      <article key={componentKey} className="card a2ui-card">
        <h4>{title}</h4>
        {children.map((child, index) => renderA2UIComponent(child, `${componentKey}-${index}`, t))}
      </article>
    );
  }

  if (component.type === 'list') {
    return (
      <ul key={componentKey} className="a2ui-list">
        {children.map((child, index) => (
          <li key={`${componentKey}-${index}`}>{renderA2UIComponent(child, `${componentKey}-${index}`, t)}</li>
        ))}
      </ul>
    );
  }

  return (
    <pre key={componentKey} className="a2ui-text">
      {JSON.stringify(component, null, 2)}
    </pre>
  );
}

export default function App() {
  const { locale, t } = useI18n();
  const tabs: { key: TabKey; label: string }[] = useMemo(
    () => [
      { key: 'console', label: t('tabs.console') },
      { key: 'workspace', label: t('tabs.workspace') },
      { key: 'content', label: t('tabs.content') },
      { key: 'approval', label: t('tabs.approval') },
      { key: 'report', label: t('tabs.report') },
    ],
    [t],
  );

  const defaultRewriteNote = t('content.defaultRewriteNote');
  const defaultRewriteNoteRef = useRef(defaultRewriteNote);

  const [activeTab, setActiveTab] = useState<TabKey>('console');
  const [formState, setFormState] = useState(DEFAULT_FORM);
  const [taskId, setTaskId] = useState<string | null>(null);
  const [streamPath, setStreamPath] = useState<string | null>(null);
  const [taskState, setTaskState] = useState<TaskStateResponse | null>(null);
  const [eventLog, setEventLog] = useState<TaskEvent[]>([]);
  const [artifacts, setArtifacts] = useState<{
    draft?: ContentDraftArtifact;
    qa?: GeoQAReport;
    questionGraph?: QuestionGraphArtifact;
    evidenceMatrix?: EvidenceMatrixArtifact;
    signal?: {
      company: string;
      signals: { type: string; description: string; source: string; confidence: number }[];
      urgency_hypothesis: string;
      modernization_fit_score: number;
    };
  }>({});
  const [surfaces, setSurfaces] = useState<Record<SurfaceKey, A2UIComponentNode[]>>(createEmptySurfaces());
  const [rewriteNote, setRewriteNote] = useState(defaultRewriteNote);
  const [busy, setBusy] = useState(false);
  const eventSourceRef = useRef<EventSource | null>(null);

  useEffect(() => {
    setRewriteNote((current) =>
      current === defaultRewriteNoteRef.current ? defaultRewriteNote : current,
    );
    defaultRewriteNoteRef.current = defaultRewriteNote;
  }, [defaultRewriteNote]);

  async function refreshTaskState(currentTaskId: string): Promise<void> {
    const [nextState, nextArtifacts] = await Promise.all([
      fetchState(currentTaskId),
      fetchArtifacts(currentTaskId),
    ]);
    setTaskState(nextState);
    setEventLog(nextState.events);
    setArtifacts(nextArtifacts);
  }

  useEffect(() => {
    if (!taskId) {
      return;
    }
    void refreshTaskState(taskId);
  }, [taskId]);

  useEffect(() => {
    if (!taskId || !streamPath) {
      return undefined;
    }

    eventSourceRef.current?.close();
    const source = new EventSource(buildStreamUrl(streamPath));
    eventSourceRef.current = source;

    source.onmessage = (message) => {
      const parsed = JSON.parse(message.data) as StreamEvent;
      const timelineEvent = toTimelineEvent(parsed, taskId);

      setEventLog((current) => {
        const alreadyExists = current.some(
          (entry) =>
            entry.timestamp === timelineEvent.timestamp &&
            entry.event_type === timelineEvent.event_type &&
            entry.message === timelineEvent.message,
        );
        return alreadyExists ? current : [...current, timelineEvent];
      });

      setTaskState((current) => {
        if (!current) {
          return current;
        }
        const nextStatus =
          parsed.event_type === 'flow.complete'
            ? 'completed'
            : parsed.event_type === 'flow.error'
              ? 'failed'
              : parsed.event_type === 'approval_required'
                ? 'waiting_approval'
                : parsed.event_type === 'node.start' || parsed.event_type === 'node.complete'
                  ? 'running'
                  : current.status;

        return {
          ...current,
          status: nextStatus,
          current_stage: readEventString(parsed, 'stage') || current.current_stage,
        };
      });

      const surfaceId = readEventString(parsed, 'surface_id');
      if (
        surfaceId &&
        (surfaceId === 'workspace' ||
          surfaceId === 'content' ||
          surfaceId === 'approval' ||
          surfaceId === 'report')
      ) {
        setSurfaces((current) => {
          if (parsed.event_type === 'a2ui.clear') {
            return { ...current, [surfaceId]: [] };
          }

          if (parsed.event_type === 'a2ui.component') {
            const component = readEventRecord(parsed, 'component') as A2UIComponentNode | null;
            if (!component) {
              return current;
            }
            return {
              ...current,
              [surfaceId]: replaceSurfaceComponent(current[surfaceId], component),
            };
          }

          if (parsed.event_type === 'a2ui.update') {
            const updates = readEventRecord(parsed, 'updates');
            const replacement = updates?.component;
            if (typeof replacement !== 'object' || replacement === null || Array.isArray(replacement)) {
              return current;
            }
            return {
              ...current,
              [surfaceId]: replaceSurfaceComponent(
                current[surfaceId],
                replacement as A2UIComponentNode,
              ),
            };
          }

          return current;
        });
      }

      if (
        parsed.event_type === 'approval_required' ||
        parsed.event_type === 'approval_submitted' ||
        parsed.event_type === 'node.complete' ||
        parsed.event_type === 'flow.complete' ||
        parsed.event_type === 'flow.error'
      ) {
        void refreshTaskState(taskId);
      }
    };

    source.onerror = () => {
      source.close();
    };

    return () => {
      source.close();
    };
  }, [streamPath, taskId]);

  const pendingApprovals = useMemo(
    () => (taskState?.approvals || []).filter((item) => item.status === 'pending'),
    [taskState],
  );

  async function handleStart() {
    setBusy(true);
    try {
      setTaskState(null);
      setArtifacts({});
      setEventLog([]);
      setSurfaces(createEmptySurfaces());
      const response = await startExecution({
        ...formState,
        inputs: {
          content_languages: [locale],
        },
      });
      setTaskId(response.task_id);
      setStreamPath(response.stream_url || `/api/geo/${response.task_id}/stream`);
      setActiveTab('console');
    } finally {
      setBusy(false);
    }
  }

  async function handleApproval(approved: boolean, action?: string) {
    if (!taskId) {
      return;
    }
    setBusy(true);
    try {
      const comment =
        action === 'rewrite' ? t('content.rewriteApprovalComment') : t('approval.uiApprovalComment');
      await submitApproval(taskId, approved, action, comment);
      await refreshTaskState(taskId);
    } finally {
      setBusy(false);
    }
  }

  async function handleRewrite() {
    if (!taskId) {
      return;
    }
    setBusy(true);
    try {
      await postCommand(taskId, 'content.rewrite', rewriteNote);
      await refreshTaskState(taskId);
    } finally {
      setBusy(false);
    }
  }

  const latestApproval = pendingApprovals[0];
  const publishedPage = taskState?.published_pages[0];

  return (
    <div className="shell">
      <aside className="rail">
        <div>
          <p className="eyebrow">{t('sidebar.eyebrow')}</p>
          <h1>{t('sidebar.title')}</h1>
          <p className="muted">{t('sidebar.description')}</p>
        </div>
        <div className="locale-row">
          <span className="locale-label">{t('locale.label')}</span>
          <LocaleSwitcher
            className="locale-switcher"
            ariaLabel={t('locale.selectAria')}
            testId="locale-switcher"
          />
        </div>
        <nav className="tabs">
          {tabs.map((tab) => (
            <button
              key={tab.key}
              type="button"
              className={activeTab === tab.key ? 'tab active' : 'tab'}
              data-testid={`tab-${tab.key}`}
              onClick={() => setActiveTab(tab.key)}
            >
              {tab.label}
            </button>
          ))}
        </nav>
      </aside>
      <main className="content">
        <section className="hero-panel">
          <div>
            <p className="eyebrow">{t('hero.eyebrow')}</p>
            <h2>{t('hero.title')}</h2>
            <p className="muted">{t('hero.description')}</p>
          </div>
          <div className="status-badges">
            <span className="badge" data-testid="task-status">
              {taskState?.status || t('status.idle')}
            </span>
            <span className="badge" data-testid="current-stage">
              {taskState?.current_stage || t('status.notStarted')}
            </span>
          </div>
        </section>

        {activeTab === 'console' && (
          <section className="panel" data-testid="campaign-console">
            <div className="panel-header">
              <div>
                <p className="eyebrow">{t('console.eyebrow')}</p>
                <h3>{t('console.title')}</h3>
              </div>
              <button
                type="button"
                data-testid="start-campaign-button"
                onClick={() => void handleStart()}
                disabled={busy}
                className="primary-button"
              >
                {busy ? t('console.processing') : t('console.start')}
              </button>
            </div>
            <div className="grid-two">
              <label>
                {t('console.fields.campaignName')}
                <input
                  data-testid="campaign-name-input"
                  value={formState.campaign_name}
                  onChange={(event) =>
                    setFormState((current) => ({ ...current, campaign_name: event.target.value }))
                  }
                />
              </label>
              <label>
                {t('console.fields.industry')}
                <input
                  data-testid="industries-input"
                  value={formState.targets.industries.join(', ')}
                  onChange={(event) =>
                    setFormState((current) => ({
                      ...current,
                      targets: {
                        ...current.targets,
                        industries: event.target.value.split(',').map((item) => item.trim()).filter(Boolean),
                      },
                    }))
                  }
                />
              </label>
              <label>
                {t('console.fields.legacyStacks')}
                <input
                  data-testid="legacy-stacks-input"
                  value={formState.targets.legacy_stacks.join(', ')}
                  onChange={(event) =>
                    setFormState((current) => ({
                      ...current,
                      targets: {
                        ...current.targets,
                        legacy_stacks: event.target.value.split(',').map((item) => item.trim()).filter(Boolean),
                      },
                    }))
                  }
                />
              </label>
              <label>
                {t('console.fields.regions')}
                <input
                  value={formState.targets.regions.join(', ')}
                  onChange={(event) =>
                    setFormState((current) => ({
                      ...current,
                      targets: {
                        ...current.targets,
                        regions: event.target.value.split(',').map((item) => item.trim()).filter(Boolean),
                      },
                    }))
                  }
                />
              </label>
            </div>
            <div className="timeline" data-testid="event-log">
              {eventLog.length === 0 && <p className="muted">{t('console.emptyTimeline')}</p>}
              {eventLog.map((event) => (
                <article key={`${event.timestamp}-${event.event_type}`} className="timeline-item">
                  <strong>{event.event_type}</strong>
                  <span>{event.stage || t('common.pipeline')}</span>
                  <p>{event.message || t('common.noMessage')}</p>
                </article>
              ))}
            </div>
          </section>
        )}

        {activeTab === 'workspace' && (
          <section className="panel" data-testid="account-workspace">
            <div className="panel-header">
              <div>
                <p className="eyebrow">{t('workspace.eyebrow')}</p>
                <h3>{t('workspace.title')}</h3>
              </div>
            </div>
            {surfaces.workspace.length > 0 ? (
              <div className="grid-two">
                {surfaces.workspace.map((component, index) =>
                  renderA2UIComponent(component, `workspace-${index}`, t),
                )}
              </div>
            ) : (
              <div className="grid-two">
                <div className="card" data-testid="demand-signals-card">
                  <h4>{t('workspace.demandSignals')}</h4>
                  <p className="score" data-testid="fit-score">
                    {artifacts.signal?.modernization_fit_score ?? '--'}
                  </p>
                  <p>{artifacts.signal?.urgency_hypothesis || t('workspace.pendingResult')}</p>
                  <ul>
                    {(artifacts.signal?.signals || []).map((signal) => (
                      <li key={`${signal.type}-${signal.source}`}>{signal.description}</li>
                    ))}
                  </ul>
                </div>
                <div className="card" data-testid="question-map-card">
                  <h4>{t('workspace.questionMap')}</h4>
                  <ul>
                    {(artifacts.questionGraph?.personas || []).map((persona) => (
                      <li key={persona.role}>
                        <strong>{persona.role}</strong>: {persona.high_intent_questions[0] || persona.questions[0]}
                      </li>
                    ))}
                  </ul>
                </div>
                <div className="card" data-testid="evidence-summary-card">
                  <h4>{t('workspace.evidenceSummary')}</h4>
                  <ul>
                    {(artifacts.evidenceMatrix?.entries || []).slice(0, 4).map((entry) => (
                      <li key={entry.source_url}>{entry.title}</li>
                    ))}
                  </ul>
                </div>
              </div>
            )}
          </section>
        )}

        {activeTab === 'content' && (
          <section className="panel" data-testid="content-studio">
            <div className="panel-header">
              <div>
                <p className="eyebrow">{t('content.eyebrow')}</p>
                <h3>{t('content.title')}</h3>
              </div>
              <div className="action-row">
                <button
                  type="button"
                  className="ghost-button"
                  data-testid="rewrite-button"
                  onClick={() => void handleRewrite()}
                  disabled={!taskId || busy}
                >
                  {t('content.rewrite')}
                </button>
                <button
                  type="button"
                  className="primary-button"
                  data-testid="publish-button"
                  onClick={() => void handleApproval(true, 'approved')}
                  disabled={!latestApproval || busy}
                >
                  {t('content.publish')}
                </button>
              </div>
            </div>
            <label className="stacked">
              {t('content.rewriteNote')}
              <textarea
                data-testid="rewrite-note"
                value={rewriteNote}
                onChange={(event) => setRewriteNote(event.target.value)}
              />
            </label>
            {surfaces.content.length > 0 ? (
              <div className="grid-two">
                {surfaces.content.map((component, index) =>
                  renderA2UIComponent(component, `content-${index}`, t),
                )}
              </div>
            ) : (
              <div className="grid-two">
                <div className="card">
                  <h4>{t('content.draft')}</h4>
                  <article data-testid="draft-preview">
                    <h5>{artifacts.draft?.pages[0]?.title || t('content.pendingDraft')}</h5>
                    <p>{artifacts.draft?.pages[0]?.summary}</p>
                    <pre>{artifacts.draft?.pages[0]?.body_markdown}</pre>
                  </article>
                </div>
                <div className="card">
                  <h4>{t('content.qa')}</h4>
                  <p data-testid="qa-risk-level">{artifacts.qa?.risk_level || '--'}</p>
                  <ul>
                    {(artifacts.qa?.issues || []).map((issue) => (
                      <li key={issue}>{issue}</li>
                    ))}
                  </ul>
                </div>
              </div>
            )}
          </section>
        )}

        {activeTab === 'approval' && (
          <section className="panel" data-testid="approval-center">
            <div className="panel-header">
              <div>
                <p className="eyebrow">{t('approval.eyebrow')}</p>
                <h3>{t('approval.title')}</h3>
              </div>
            </div>
            {surfaces.approval.length > 0 && (
              <div className="grid-two">
                {surfaces.approval.map((component, index) =>
                  renderA2UIComponent(component, `approval-surface-${index}`, t),
                )}
              </div>
            )}
            {pendingApprovals.length === 0 ? (
              <p className="muted">{t('approval.none')}</p>
            ) : (
              pendingApprovals.map((approval: ApprovalRecord) => (
                <article key={approval.request_id} className="approval-card" data-testid="pending-approval-card">
                  <h4>{approval.risk_level}</h4>
                  <p>{approval.reason}</p>
                  <div className="action-row">
                    <button type="button" onClick={() => void handleApproval(true, 'approved')}>
                      {t('approval.approve')}
                    </button>
                    <button type="button" onClick={() => void handleApproval(false, 'rejected')}>
                      {t('approval.reject')}
                    </button>
                    <button type="button" onClick={() => void handleApproval(true, 'rewrite')}>
                      {t('approval.rewrite')}
                    </button>
                  </div>
                </article>
              ))
            )}
          </section>
        )}

        {activeTab === 'report' && (
          <section className="panel" data-testid="report-center">
            <div className="panel-header">
              <div>
                <p className="eyebrow">{t('report.eyebrow')}</p>
                <h3>{t('report.title')}</h3>
              </div>
            </div>
            {surfaces.report.length > 0 ? (
              <div className="grid-two">
                {surfaces.report.map((component, index) =>
                  renderA2UIComponent(component, `report-${index}`, t),
                )}
              </div>
            ) : (
              <div className="grid-two">
                <div className="card">
                  <h4>{t('report.campaignReport')}</h4>
                  <pre data-testid="report-markdown">{taskState?.report?.markdown || t('report.pendingReport')}</pre>
                </div>
                <div className="card">
                  <h4>{t('report.publishedAssets')}</h4>
                  {publishedPage ? (
                    <a
                      data-testid="published-page-link"
                      href={publishedPage.page_url}
                      target="_blank"
                      rel="noreferrer"
                    >
                      {publishedPage.title}
                    </a>
                  ) : (
                    <p className="muted">{t('report.pendingPublish')}</p>
                  )}
                </div>
              </div>
            )}
          </section>
        )}
      </main>
    </div>
  );
}
