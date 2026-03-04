/**
 * RAG 管理共有型定義.
 *
 * CollectionInfo / DocumentInfo / RAGConfigValues 等、
 * 全アプリで共用する型をここに集約する。
 */

// ---------------------------------------------------------------------------
// コレクション
// ---------------------------------------------------------------------------

/** コレクション情報 */
export interface CollectionInfo {
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

/** コレクション作成リクエスト */
export interface CreateCollectionRequest {
  collection_name: string;
  app_name: string;
  tenant_id?: string | null;
  display_name?: string;
  description?: string;
  chunk_strategy?: string;
  chunk_size?: number;
  chunk_overlap?: number;
  embedding_model?: string | null;
  retrieval_method?: string;
  reranker?: string | null;
  top_k?: number;
  min_similarity?: number;
  vector_db_type?: string | null;
  vector_db_url?: string | null;
}

// ---------------------------------------------------------------------------
// ドキュメント
// ---------------------------------------------------------------------------

/** ドキュメントステータス */
export type DocumentStatus = 'uploaded' | 'chunked' | 'indexed' | 'error';

/** ドキュメント情報 */
export interface DocumentInfo {
  document_id: string;
  collection_name: string;
  filename: string;
  file_type: string;
  file_size: number;
  status: DocumentStatus;
  chunk_count: number;
  content_hash: string;
  uploaded_by: string | null;
  uploaded_at: string | null;
  indexed_at: string | null;
  error_message: string | null;
}

/** ページネーション付きドキュメントリスト */
export interface PaginatedDocs {
  documents: DocumentInfo[];
  total: number;
  limit: number;
  offset: number;
}

/** ドキュメント一覧取得オプション */
export interface ListDocumentsOptions {
  status?: DocumentStatus;
  limit?: number;
  offset?: number;
}

// ---------------------------------------------------------------------------
// チャンク
// ---------------------------------------------------------------------------

/** チャンクプレビュー */
export interface ChunkPreview {
  index: number;
  content: string;
  length: number;
  strategy: string;
}

/** チャンキングオプション */
export interface ChunkOptions {
  chunk_strategy?: string;
  chunk_size?: number;
  chunk_overlap?: number;
}

// ---------------------------------------------------------------------------
// RAG 設定
// ---------------------------------------------------------------------------

/** RAG 設定値（コレクション単位で変更可能） */
export interface RAGConfigValues {
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
}

// ---------------------------------------------------------------------------
// インジェスト
// ---------------------------------------------------------------------------

/** インジェスト実行概要 */
export interface IngestRunSummary {
  run_id: string;
  status: 'queued' | 'running' | 'completed' | 'failed';
  trigger_mode?: string;
  dry_run: boolean;
  started_at: string | null;
  finished_at: string | null;
  duration_ms: number;
  summary?: Record<string, unknown>;
}

/** インジェスト実行オプション */
export interface IngestOptions {
  source_ids?: string[];
  dry_run?: boolean;
  async_mode?: boolean;
}

// ---------------------------------------------------------------------------
// テストクエリ
// ---------------------------------------------------------------------------

/** テストクエリオプション */
export interface TestQueryOptions {
  collection?: string;
  top_k?: number;
  min_similarity?: number;
}

/** テストクエリ結果 */
export interface TestQueryResult {
  query: string;
  results: TestQueryHit[];
  total: number;
  took_ms: number;
}

/** テストクエリヒット */
export interface TestQueryHit {
  content: string;
  score: number;
  metadata: Record<string, unknown>;
  source: string;
}

// ---------------------------------------------------------------------------
// アクセス制御
// ---------------------------------------------------------------------------

/** KB 権限定義 */
export interface KBPermission {
  role: string;
  collections: string[];
  permission_level: 'read' | 'write' | 'admin';
}

// ---------------------------------------------------------------------------
// API アダプターインターフェース
// ---------------------------------------------------------------------------

/**
 * RAG API アダプター.
 *
 * 各アプリはこのインターフェースを実装して、
 * 共有 UI コンポーネントに接続する。
 */
export interface RAGApiAdapter {
  /** コレクション一覧 */
  listCollections(): Promise<CollectionInfo[]>;
  /** コレクション作成 */
  createCollection(config: CreateCollectionRequest): Promise<CollectionInfo>;
  /** コレクション更新 */
  updateCollection(
    name: string,
    config: Partial<RAGConfigValues>,
  ): Promise<CollectionInfo>;
  /** コレクション削除 */
  deleteCollection(name: string): Promise<void>;

  /** ドキュメント一覧 */
  listDocuments(
    collection: string,
    options?: ListDocumentsOptions,
  ): Promise<PaginatedDocs>;
  /** ドキュメントアップロード */
  uploadDocument(
    collection: string,
    file: File,
    metadata?: Record<string, unknown>,
  ): Promise<DocumentInfo>;
  /** ドキュメント削除 */
  deleteDocument(collection: string, docId: string): Promise<void>;

  /** チャンクプレビュー */
  previewChunks(
    collection: string,
    docId: string,
    options?: ChunkOptions,
  ): Promise<ChunkPreview[]>;
  /** ドキュメントインデックス */
  indexDocument(collection: string, docId: string): Promise<DocumentInfo>;
  /** ドキュメント再インデックス */
  reindexDocument(collection: string, docId: string): Promise<void>;

  /** テストクエリ */
  testQuery(query: string, options: TestQueryOptions): Promise<TestQueryResult>;
  /** インジェスト実行 */
  triggerIngest(options?: IngestOptions): Promise<IngestRunSummary>;
  /** インジェスト履歴 */
  listIngestRuns(limit?: number): Promise<IngestRunSummary[]>;
}

// ---------------------------------------------------------------------------
// テーマ
// ---------------------------------------------------------------------------

/** RAG UI テーマ（アプリ毎にカスタマイズ可） */
export interface RAGTheme {
  primaryColor: string;
  accentColor: string;
  cardBg: string;
  textPrimary: string;
  textSecondary: string;
}

/** デフォルトテーマ（ダークモード） */
export const DEFAULT_RAG_THEME: RAGTheme = {
  primaryColor: '#6366f1',
  accentColor: '#22d3ee',
  cardBg: '#1e1e2e',
  textPrimary: '#e2e8f0',
  textSecondary: '#94a3b8',
};
