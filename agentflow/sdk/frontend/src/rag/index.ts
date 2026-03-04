/**
 * @agentflow/ui RAG 管理モジュール.
 *
 * 共有コンポーネント・フック・型・ストアを集約エクスポート。
 */

// --- 型定義 ---
export type {
  ChunkOptions,
  ChunkPreview,
  CollectionInfo,
  CreateCollectionRequest,
  DocumentInfo,
  DocumentStatus,
  IngestOptions,
  IngestRunSummary,
  KBPermission,
  ListDocumentsOptions,
  PaginatedDocs,
  RAGApiAdapter,
  RAGConfigValues,
  RAGTheme,
  TestQueryHit,
  TestQueryOptions,
  TestQueryResult,
} from './types/rag';

export { DEFAULT_RAG_THEME } from './types/rag';

// --- コンポーネント ---
export { RAGStatCard, type RAGStatCardProps } from './components/RAGStatCard';
export { RAGStatusBadge, type RAGHealthStatus, type RAGStatusBadgeProps } from './components/RAGStatusBadge';
export { RAGCollectionTable, type RAGCollectionTableProps } from './components/RAGCollectionTable';
export { RAGDocumentList, type RAGDocumentListProps } from './components/RAGDocumentList';
export { RAGDocumentUploader, type RAGDocumentUploaderProps } from './components/RAGDocumentUploader';
export { RAGChunkPreview, type RAGChunkPreviewProps } from './components/RAGChunkPreview';
export { RAGConfigForm, type RAGConfigFormProps } from './components/RAGConfigForm';
export { RAGRetrievalTestPanel, type RAGRetrievalTestPanelProps } from './components/RAGRetrievalTestPanel';
export { RAGIngestHistory, type RAGIngestHistoryProps } from './components/RAGIngestHistory';
export { RAGAccessControlTable, type RAGAccessControlTableProps } from './components/RAGAccessControlTable';

// --- フック ---
export { useRAGCollections, type UseRAGCollectionsReturn } from './hooks/useRAGCollections';
export { useRAGDocuments, type UseRAGDocumentsReturn } from './hooks/useRAGDocuments';
export { useFileUpload, type UseFileUploadReturn, type UploadState } from './hooks/useFileUpload';

// --- ストア ---
export { createRAGStore, type RAGStore, type RAGStoreState, type RAGStoreActions } from './store/createRAGStore';
