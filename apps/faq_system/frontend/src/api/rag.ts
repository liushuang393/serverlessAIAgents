/**
 * FAQ System RAG 管理 API アダプター.
 *
 * コレクション CRUD・ドキュメント管理・テストクエリなどの
 * バックエンド API を呼び出す。
 */

const BASE_URL = import.meta.env.DEV ? '/api' : '';

function getToken(): string | null {
  return localStorage.getItem('access_token');
}

function authHeaders(): Record<string, string> {
  const token = getToken();
  return token ? { Authorization: `Bearer ${token}` } : {};
}

async function handleResponse<T>(response: Response): Promise<T> {
  if (response.status === 401) {
    localStorage.removeItem('access_token');
    localStorage.removeItem('user_info');
    if (!window.location.pathname.startsWith('/login')) {
      window.location.href = '/login';
    }
    throw new Error('Unauthorized');
  }
  if (!response.ok) {
    const err = await response.json().catch(() => ({ detail: response.statusText }));
    throw new Error(typeof err.detail === 'string' ? err.detail : JSON.stringify(err.detail));
  }
  return response.json();
}

async function get<T>(endpoint: string): Promise<T> {
  const resp = await fetch(`${BASE_URL}${endpoint}`, {
    method: 'GET',
    headers: { ...authHeaders() },
  });
  return handleResponse<T>(resp);
}

async function post<T>(endpoint: string, body?: unknown): Promise<T> {
  const resp = await fetch(`${BASE_URL}${endpoint}`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json', ...authHeaders() },
    body: body != null ? JSON.stringify(body) : undefined,
  });
  return handleResponse<T>(resp);
}

async function patch<T>(endpoint: string, body: unknown): Promise<T> {
  const resp = await fetch(`${BASE_URL}${endpoint}`, {
    method: 'PATCH',
    headers: { 'Content-Type': 'application/json', ...authHeaders() },
    body: JSON.stringify(body),
  });
  return handleResponse<T>(resp);
}

async function del<T>(endpoint: string): Promise<T> {
  const resp = await fetch(`${BASE_URL}${endpoint}`, {
    method: 'DELETE',
    headers: { ...authHeaders() },
  });
  return handleResponse<T>(resp);
}

async function upload<T>(endpoint: string, file: File, extra?: Record<string, string>): Promise<T> {
  const form = new FormData();
  form.append('file', file);
  if (extra) {
    for (const [key, value] of Object.entries(extra)) {
      form.append(key, value);
    }
  }
  const resp = await fetch(`${BASE_URL}${endpoint}`, {
    method: 'POST',
    headers: { ...authHeaders() },
    body: form,
  });
  return handleResponse<T>(resp);
}

// ---------------------------------------------------------------------------
// 型定義
// ---------------------------------------------------------------------------

export interface CollectionInfo {
  id: number;
  collection_name: string;
  app_name: string;
  tenant_id: string | null;
  display_name: string;
  description: string;
  chunk_strategy: string;
  chunk_size: number;
  chunk_overlap: number;
  embedding_model: string | null;
  retrieval_method: string;
  reranker: string | null;
  top_k: number;
  min_similarity: number;
  vector_db_type: string | null;
  vector_db_url: string | null;
  document_count: number;
  last_indexed_at: string | null;
  created_at: string | null;
  updated_at: string | null;
}

export interface DocumentInfo {
  id: number;
  document_id: string;
  collection_name: string;
  filename: string;
  file_type: string;
  file_size: number;
  status: string;
  chunk_count: number;
  content_hash: string;
  uploaded_by: string | null;
  uploaded_at: string | null;
  indexed_at: string | null;
  error_message: string | null;
}

export interface ChunkPreview {
  index: number;
  content: string;
  length: number;
  strategy: string;
}

export interface IngestRunSummary {
  id: string;
  status: string;
  trigger_mode: string;
  started_at: string;
  finished_at: string | null;
  summary_json: Record<string, unknown>;
}

// ---------------------------------------------------------------------------
// API 関数
// ---------------------------------------------------------------------------

export const ragApi = {
  // コレクション
  listCollections: () =>
    get<{ total: number; collections: CollectionInfo[] }>('/collections'),

  getCollection: (name: string) =>
    get<{ collection: CollectionInfo }>(`/collections/${name}`),

  createCollection: (data: Partial<CollectionInfo>) =>
    post<{ collection: CollectionInfo }>('/collections', data),

  updateCollection: (name: string, data: Partial<CollectionInfo>) =>
    patch<{ collection: CollectionInfo }>(`/collections/${name}`, data),

  deleteCollection: (name: string) =>
    del<{ deleted: boolean }>(`/collections/${name}`),

  getCollectionStats: (name: string) =>
    get<Record<string, unknown>>(`/collections/${name}/stats`),

  testQuery: (name: string, query: string, topK = 5) =>
    post<Record<string, unknown>>(`/collections/${name}/test-query`, { query, top_k: topK }),

  // ドキュメント
  listDocuments: (collection: string, status?: string, limit = 100, offset = 0) => {
    const params = new URLSearchParams({ limit: String(limit), offset: String(offset) });
    if (status) params.set('status', status);
    return get<{ total: number; documents: DocumentInfo[] }>(
      `/collections/${collection}/documents?${params}`,
    );
  },

  getDocument: (collection: string, docId: string) =>
    get<{ document: DocumentInfo }>(`/collections/${collection}/documents/${docId}`),

  uploadDocument: (collection: string, file: File, autoIndex = false) =>
    upload<{ document: DocumentInfo }>(
      `/collections/${collection}/documents`,
      file,
      autoIndex ? { auto_index: 'true' } : undefined,
    ),

  deleteDocument: (collection: string, docId: string) =>
    del<{ deleted: boolean }>(`/collections/${collection}/documents/${docId}`),

  previewChunks: (collection: string, docId: string, options?: { chunk_strategy?: string; chunk_size?: number; chunk_overlap?: number }) =>
    post<{ total: number; chunks: ChunkPreview[] }>(
      `/collections/${collection}/documents/${docId}/preview-chunks`,
      options ?? {},
    ),

  indexDocument: (collection: string, docId: string) =>
    post<{ document: DocumentInfo }>(`/collections/${collection}/documents/${docId}/index`),

  reindexDocument: (collection: string, docId: string) =>
    post<{ document: DocumentInfo }>(`/collections/${collection}/documents/${docId}/reindex`),

  reindexCollection: (collection: string) =>
    post<{ total: number; reindexed: number; errors: number }>(`/collections/${collection}/reindex`),

  // インジェスト
  listIngestRuns: (limit = 20) =>
    get<{ total: number; runs: IngestRunSummary[] }>(`/rag/ingest/runs?limit=${limit}`),

  triggerIngest: (sourceIds?: string[], dryRun = false, asyncMode = false) =>
    post<Record<string, unknown>>('/rag/ingest', { source_ids: sourceIds ?? [], dry_run: dryRun, async_mode: asyncMode }),

  // アクセス制御
  getAccessMatrix: () =>
    get<{ matrix: Record<string, Record<string, boolean>> }>('/access/matrix'),
};
