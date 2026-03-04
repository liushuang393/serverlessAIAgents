/**
 * 知識ベースコレクション管理 API.
 *
 * 新 /api/knowledge/collections エンドポイントのクライアント。
 */

const BASE = '';

/** コレクション情報 */
export interface CollectionInfo {
  collection_name: string;
  app_name: string;
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
  document_count: number;
  last_indexed_at: string | null;
  created_at: string;
  updated_at: string;
}

/** ドキュメント情報 */
export interface DocumentInfo {
  document_id: string;
  collection_name: string;
  filename: string;
  file_type: string;
  file_size: number;
  status: 'uploaded' | 'chunked' | 'indexed' | 'error';
  chunk_count: number;
  uploaded_by: string | null;
  uploaded_at: string;
  indexed_at: string | null;
  error_message: string | null;
}

/** チャンクプレビュー */
export interface ChunkPreview {
  index: number;
  text: string;
  metadata?: Record<string, unknown>;
}

// ---------------------------------------------------------------------------
// HTTP ヘルパー
// ---------------------------------------------------------------------------

async function get<T>(path: string): Promise<T> {
  const res = await fetch(`${BASE}${path}`, { credentials: 'include' });
  if (!res.ok) throw new Error(`GET ${path} failed: ${res.status}`);
  return res.json();
}

async function post<T>(path: string, body?: unknown): Promise<T> {
  const res = await fetch(`${BASE}${path}`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    credentials: 'include',
    body: body ? JSON.stringify(body) : undefined,
  });
  if (!res.ok) throw new Error(`POST ${path} failed: ${res.status}`);
  return res.json();
}

async function patch<T>(path: string, body: unknown): Promise<T> {
  const res = await fetch(`${BASE}${path}`, {
    method: 'PATCH',
    headers: { 'Content-Type': 'application/json' },
    credentials: 'include',
    body: JSON.stringify(body),
  });
  if (!res.ok) throw new Error(`PATCH ${path} failed: ${res.status}`);
  return res.json();
}

async function del<T>(path: string): Promise<T> {
  const res = await fetch(`${BASE}${path}`, {
    method: 'DELETE',
    credentials: 'include',
  });
  if (!res.ok) throw new Error(`DELETE ${path} failed: ${res.status}`);
  return res.json();
}

async function upload<T>(path: string, file: File): Promise<T> {
  const form = new FormData();
  form.append('file', file);
  const res = await fetch(`${BASE}${path}`, {
    method: 'POST',
    credentials: 'include',
    body: form,
  });
  if (!res.ok) throw new Error(`UPLOAD ${path} failed: ${res.status}`);
  return res.json();
}

// ---------------------------------------------------------------------------
// API クライアント
// ---------------------------------------------------------------------------

const P = '/api/knowledge/collections';

export const knowledgeApi = {
  // コレクション
  listCollections: () =>
    get<{ total: number; collections: CollectionInfo[] }>(P),

  createCollection: (data: Record<string, unknown>) =>
    post<{ collection: CollectionInfo }>(P, data),

  getCollection: (name: string) =>
    get<{ collection: CollectionInfo }>(`${P}/${name}`),

  updateCollection: (name: string, data: Record<string, unknown>) =>
    patch<{ collection: CollectionInfo }>(`${P}/${name}`, data),

  deleteCollection: (name: string) =>
    del<{ deleted: boolean }>(`${P}/${name}`),

  getCollectionStats: (name: string) =>
    get<Record<string, unknown>>(`${P}/${name}/stats`),

  testQuery: (name: string, query: string, topK = 5) =>
    post<Record<string, unknown>>(`${P}/${name}/test-query`, { query, top_k: topK }),

  // ドキュメント
  listDocuments: (name: string) =>
    get<{ total: number; documents: DocumentInfo[] }>(`${P}/${name}/documents`),

  uploadDocument: (name: string, file: File) =>
    upload<{ document: DocumentInfo }>(`${P}/${name}/documents`, file),

  getDocument: (name: string, docId: string) =>
    get<{ document: DocumentInfo }>(`${P}/${name}/documents/${docId}`),

  deleteDocument: (name: string, docId: string) =>
    del<{ deleted: boolean }>(`${P}/${name}/documents/${docId}`),

  previewChunks: (name: string, docId: string, opts?: Record<string, unknown>) =>
    post<{ total: number; chunks: ChunkPreview[] }>(
      `${P}/${name}/documents/${docId}/preview-chunks`,
      opts,
    ),

  indexDocument: (name: string, docId: string) =>
    post<{ document: DocumentInfo }>(`${P}/${name}/documents/${docId}/index`),

  reindexDocument: (name: string, docId: string) =>
    post<{ document: DocumentInfo }>(`${P}/${name}/documents/${docId}/reindex`),

  reindexCollection: (name: string) =>
    post<Record<string, unknown>>(`${P}/${name}/reindex`),
};
