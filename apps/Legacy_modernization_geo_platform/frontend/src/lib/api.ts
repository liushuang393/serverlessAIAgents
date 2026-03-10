import type {
  AccountSignalArtifact,
  ContentDraftArtifact,
  EvidenceMatrixArtifact,
  GeoExecuteRequest,
  GeoQAReport,
  QuestionGraphArtifact,
  TaskStateResponse,
} from '../types';

const API_BASE = import.meta.env.VITE_API_BASE_URL || window.location.origin;

function buildHeaders(): HeadersInit {
  const apiKey = window.localStorage.getItem('GEO_PLATFORM_API_KEY');
  if (!apiKey) {
    return {};
  }
  return { 'x-api-key': apiKey };
}

async function readJson<T>(response: Response): Promise<T> {
  if (!response.ok) {
    const detail = await response.text();
    throw new Error(detail || response.statusText);
  }
  return (await response.json()) as T;
}

export async function startExecution(payload: GeoExecuteRequest): Promise<{ task_id: string; ws_url: string }> {
  const response = await fetch(`${API_BASE}/api/geo/execute`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      ...buildHeaders(),
    },
    body: JSON.stringify(payload),
  });
  return readJson(response);
}

export async function fetchState(taskId: string): Promise<TaskStateResponse> {
  const response = await fetch(`${API_BASE}/api/geo/${taskId}/state`, {
    headers: buildHeaders(),
  });
  return readJson(response);
}

export async function submitApproval(taskId: string, approved: boolean, action?: string): Promise<unknown> {
  const response = await fetch(`${API_BASE}/api/geo/${taskId}/approval`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      ...buildHeaders(),
    },
    body: JSON.stringify({
      approved,
      reviewer_name: 'operator-ui',
      action,
      comment: action === 'rewrite' ? '请调整比较性表述' : 'UI approval',
    }),
  });
  return readJson(response);
}

export async function postCommand(taskId: string, command: string, comment?: string): Promise<unknown> {
  const response = await fetch(`${API_BASE}/api/geo/${taskId}/commands`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      ...buildHeaders(),
    },
    body: JSON.stringify({
      command,
      actor: 'operator-ui',
      comment,
      payload: {},
    }),
  });
  return readJson(response);
}

export async function fetchArtifact<T>(
  taskId: string,
  artifactName: string,
): Promise<T> {
  const response = await fetch(`${API_BASE}/api/geo/${taskId}/artifacts/${artifactName}`, {
    headers: buildHeaders(),
  });
  return readJson(response);
}

export async function fetchArtifacts(taskId: string): Promise<{
  signal?: AccountSignalArtifact;
  draft?: ContentDraftArtifact;
  qa?: GeoQAReport;
  questionGraph?: QuestionGraphArtifact;
  evidenceMatrix?: EvidenceMatrixArtifact;
}> {
  const [signal, draft, qa, questionGraph, evidenceMatrix] = await Promise.all([
    fetchArtifact<AccountSignalArtifact>(taskId, 'account_signal_artifact').catch(() => undefined),
    fetchArtifact<ContentDraftArtifact>(taskId, 'content_draft_artifact').catch(() => undefined),
    fetchArtifact<GeoQAReport>(taskId, 'geo_qa_report').catch(() => undefined),
    fetchArtifact<QuestionGraphArtifact>(taskId, 'question_graph_artifact').catch(() => undefined),
    fetchArtifact<EvidenceMatrixArtifact>(taskId, 'evidence_matrix').catch(() => undefined),
  ]);
  return { signal, draft, qa, questionGraph, evidenceMatrix };
}

