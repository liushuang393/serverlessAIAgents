import { useEffect, useMemo, useRef, useState } from 'react';
import {
  fetchArtifacts,
  fetchState,
  postCommand,
  startExecution,
  submitApproval,
} from './lib/api';
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

const TABS: { key: TabKey; label: string }[] = [
  { key: 'console', label: 'Campaign Console' },
  { key: 'workspace', label: 'Account Workspace' },
  { key: 'content', label: 'Content Studio' },
  { key: 'approval', label: 'Approval Center' },
  { key: 'report', label: 'Report Center' },
];

const DEFAULT_FORM = {
  campaign_name: 'legacy-modernization-japan-b2b',
  package: 'assessment',
  targets: {
    industries: ['manufacturing'],
    legacy_stacks: ['COBOL', 'Struts'],
    regions: ['Japan'],
  },
};

export default function App() {
  const [activeTab, setActiveTab] = useState<TabKey>('console');
  const [formState, setFormState] = useState(DEFAULT_FORM);
  const [taskId, setTaskId] = useState<string | null>(null);
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
  const [rewriteNote, setRewriteNote] = useState('请补充更明确的阶段迁移边界');
  const [busy, setBusy] = useState(false);
  const wsRef = useRef<WebSocket | null>(null);

  useEffect(() => {
    if (!taskId) {
      return undefined;
    }
    let cancelled = false;
    const load = async () => {
      try {
        const nextState = await fetchState(taskId);
        if (!cancelled) {
          setTaskState(nextState);
        }
      } catch (error) {
        console.error(error);
      }
    };
    void load();
    const timer = window.setInterval(() => {
      void load();
    }, 1500);
    return () => {
      cancelled = true;
      window.clearInterval(timer);
    };
  }, [taskId]);

  useEffect(() => {
    if (!taskState) {
      return;
    }
    setEventLog(taskState.events);
    void fetchArtifacts(taskState.task_id).then(setArtifacts);
  }, [taskState]);

  useEffect(() => {
    if (!taskId) {
      return undefined;
    }
    if (wsRef.current) {
      wsRef.current.close();
    }
    const protocol = window.location.protocol === 'https:' ? 'wss' : 'ws';
    const socket = new WebSocket(`${protocol}://${window.location.host}/api/ws/${taskId}`);
    socket.onmessage = (message) => {
      const event = JSON.parse(message.data) as TaskEvent;
      setEventLog((current) => [...current, event]);
    };
    wsRef.current = socket;
    return () => {
      socket.close();
    };
  }, [taskId]);

  const pendingApprovals = useMemo(
    () => (taskState?.approvals || []).filter((item) => item.status === 'pending'),
    [taskState],
  );

  async function handleStart() {
    setBusy(true);
    try {
      const response = await startExecution(formState);
      setTaskId(response.task_id);
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
      await submitApproval(taskId, approved, action);
      const nextState = await fetchState(taskId);
      setTaskState(nextState);
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
      const nextState = await fetchState(taskId);
      setTaskState(nextState);
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
          <p className="eyebrow">Legacy Modernization</p>
          <h1>GEO Platform</h1>
          <p className="muted">
            需求诊断、内容构建、审批发布、报告闭环。
          </p>
        </div>
        <nav className="tabs">
          {TABS.map((tab) => (
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
            <p className="eyebrow">Operator Surface</p>
            <h2>旧系统刷新 GEO 的最小闭环</h2>
            <p className="muted">
              操作台负责 orchestration 与人工介入，对外页面由发布阶段生成静态 HTML。
            </p>
          </div>
          <div className="status-badges">
            <span className="badge" data-testid="task-status">
              {taskState?.status || 'idle'}
            </span>
            <span className="badge" data-testid="current-stage">
              {taskState?.current_stage || 'not-started'}
            </span>
          </div>
        </section>

        {activeTab === 'console' && (
          <section className="panel" data-testid="campaign-console">
            <div className="panel-header">
              <div>
                <p className="eyebrow">Campaign Console</p>
                <h3>启动与进度流</h3>
              </div>
              <button
                type="button"
                data-testid="start-campaign-button"
                onClick={() => void handleStart()}
                disabled={busy}
                className="primary-button"
              >
                {busy ? '处理中...' : 'Start Campaign'}
              </button>
            </div>
            <div className="grid-two">
              <label>
                Campaign Name
                <input
                  data-testid="campaign-name-input"
                  value={formState.campaign_name}
                  onChange={(event) =>
                    setFormState((current) => ({ ...current, campaign_name: event.target.value }))
                  }
                />
              </label>
              <label>
                Industry
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
                Legacy Stacks
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
                Regions
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
              {eventLog.length === 0 && <p className="muted">暂无事件，启动后会显示 pipeline 进度。</p>}
              {eventLog.map((event) => (
                <article key={`${event.timestamp}-${event.event_type}`} className="timeline-item">
                  <strong>{event.event_type}</strong>
                  <span>{event.stage || 'pipeline'}</span>
                  <p>{event.message || 'No message'}</p>
                </article>
              ))}
            </div>
          </section>
        )}

        {activeTab === 'workspace' && (
          <section className="panel" data-testid="account-workspace">
            <div className="panel-header">
              <div>
                <p className="eyebrow">Account Workspace</p>
                <h3>需求信号、问题图谱与证据摘要</h3>
              </div>
            </div>
            <div className="grid-two">
              <div className="card" data-testid="demand-signals-card">
                <h4>Demand Signals</h4>
                <p className="score" data-testid="fit-score">
                  {artifacts.signal?.modernization_fit_score ?? '--'}
                </p>
                <p>{artifacts.signal?.urgency_hypothesis || '等待执行结果'}</p>
                <ul>
                  {(artifacts.signal?.signals || []).map((signal) => (
                    <li key={`${signal.type}-${signal.source}`}>{signal.description}</li>
                  ))}
                </ul>
              </div>
              <div className="card" data-testid="question-map-card">
                <h4>Question Map</h4>
                <ul>
                  {(artifacts.questionGraph?.personas || []).map((persona) => (
                    <li key={persona.role}>
                      <strong>{persona.role}</strong>: {persona.high_intent_questions[0] || persona.questions[0]}
                    </li>
                  ))}
                </ul>
              </div>
              <div className="card" data-testid="evidence-summary-card">
                <h4>Evidence Summary</h4>
                <ul>
                  {(artifacts.evidenceMatrix?.entries || []).slice(0, 4).map((entry) => (
                    <li key={entry.source_url}>{entry.title}</li>
                  ))}
                </ul>
              </div>
            </div>
          </section>
        )}

        {activeTab === 'content' && (
          <section className="panel" data-testid="content-studio">
            <div className="panel-header">
              <div>
                <p className="eyebrow">Content Studio</p>
                <h3>草稿预览、QA 与人工改写</h3>
              </div>
              <div className="action-row">
                <button
                  type="button"
                  className="ghost-button"
                  data-testid="rewrite-button"
                  onClick={() => void handleRewrite()}
                  disabled={!taskId || busy}
                >
                  Rewrite
                </button>
                <button
                  type="button"
                  className="primary-button"
                  data-testid="publish-button"
                  onClick={() => void handleApproval(true, 'approved')}
                  disabled={!latestApproval || busy}
                >
                  Publish
                </button>
              </div>
            </div>
            <label className="stacked">
              Rewrite Note
              <textarea
                data-testid="rewrite-note"
                value={rewriteNote}
                onChange={(event) => setRewriteNote(event.target.value)}
              />
            </label>
            <div className="grid-two">
              <div className="card">
                <h4>Draft</h4>
                <article data-testid="draft-preview">
                  <h5>{artifacts.draft?.pages[0]?.title || '等待生成内容'}</h5>
                  <p>{artifacts.draft?.pages[0]?.summary}</p>
                  <pre>{artifacts.draft?.pages[0]?.body_markdown}</pre>
                </article>
              </div>
              <div className="card">
                <h4>QA</h4>
                <p data-testid="qa-risk-level">{artifacts.qa?.risk_level || '--'}</p>
                <ul>
                  {(artifacts.qa?.issues || []).map((issue) => (
                    <li key={issue}>{issue}</li>
                  ))}
                </ul>
              </div>
            </div>
          </section>
        )}

        {activeTab === 'approval' && (
          <section className="panel" data-testid="approval-center">
            <div className="panel-header">
              <div>
                <p className="eyebrow">Approval Center</p>
                <h3>人工决策门禁</h3>
              </div>
            </div>
            {pendingApprovals.length === 0 ? (
              <p className="muted">当前没有待审批项。</p>
            ) : (
              pendingApprovals.map((approval: ApprovalRecord) => (
                <article key={approval.request_id} className="approval-card" data-testid="pending-approval-card">
                  <h4>{approval.risk_level}</h4>
                  <p>{approval.reason}</p>
                  <div className="action-row">
                    <button type="button" onClick={() => void handleApproval(true, 'approved')}>
                      Approve
                    </button>
                    <button type="button" onClick={() => void handleApproval(false, 'rejected')}>
                      Reject
                    </button>
                    <button type="button" onClick={() => void handleApproval(true, 'rewrite')}>
                      Rewrite
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
                <p className="eyebrow">Report Center</p>
                <h3>执行摘要与发布结果</h3>
              </div>
            </div>
            <div className="grid-two">
              <div className="card">
                <h4>Campaign Report</h4>
                <pre data-testid="report-markdown">{taskState?.report?.markdown || '等待报告生成'}</pre>
              </div>
              <div className="card">
                <h4>Published Assets</h4>
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
                  <p className="muted">等待发布完成</p>
                )}
              </div>
            </div>
          </section>
        )}
      </main>
    </div>
  );
}
