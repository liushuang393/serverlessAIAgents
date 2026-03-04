/**
 * RAG 管理レイアウト.
 *
 * サブナビゲーションタブ付きのラッパーコンポーネント。
 * 各 RAG 管理ページを切り替える。
 */
import { Database, FileText, Search, Clock, Shield, LayoutDashboard } from 'lucide-react';
import { useI18n } from '../../i18n';
import { useRAGStore, type RAGTab } from '../../stores/ragStore';
import { KBDashboard } from './KBDashboard';
import { CollectionManager } from './CollectionManager';
import { DocumentManager } from './DocumentManager';
import { RetrievalSettings } from './RetrievalSettings';
import { IngestHistoryPage } from './IngestHistoryPage';
import { AccessControlPage } from './AccessControlPage';

const tabs: { key: RAGTab; icon: typeof Database; labelKey: string }[] = [
  { key: 'dashboard', icon: LayoutDashboard, labelKey: 'rag.tab_dashboard' },
  { key: 'collections', icon: Database, labelKey: 'rag.tab_collections' },
  { key: 'documents', icon: FileText, labelKey: 'rag.tab_documents' },
  { key: 'retrieval', icon: Search, labelKey: 'rag.tab_retrieval' },
  { key: 'ingest', icon: Clock, labelKey: 'rag.tab_ingest' },
  { key: 'access', icon: Shield, labelKey: 'rag.tab_access' },
];

function TabContent({ tab }: { tab: RAGTab }) {
  switch (tab) {
    case 'dashboard':
      return <KBDashboard />;
    case 'collections':
      return <CollectionManager />;
    case 'documents':
      return <DocumentManager />;
    case 'retrieval':
      return <RetrievalSettings />;
    case 'ingest':
      return <IngestHistoryPage />;
    case 'access':
      return <AccessControlPage />;
  }
}

export function RAGLayout() {
  const { t } = useI18n();
  const { activeTab, setActiveTab, error, clearError } = useRAGStore();

  return (
    <div className="flex flex-col h-full bg-[var(--bg-main)]">
      {/* ヘッダー + タブ */}
      <div className="border-b border-white/5 bg-[var(--bg-main)]">
        <div className="max-w-6xl mx-auto px-6 pt-5 pb-0">
          <h1 className="text-xl font-bold text-white mb-1">
            {t('rag.title')}
          </h1>
          <p className="text-xs text-[var(--text-muted)] mb-4">
            {t('rag.subtitle')}
          </p>

          {/* タブナビゲーション */}
          <div className="flex gap-1 -mb-px">
            {tabs.map(({ key, icon: Icon, labelKey }) => (
              <button
                key={key}
                onClick={() => setActiveTab(key)}
                className={`flex items-center gap-1.5 px-3.5 py-2 text-xs font-medium rounded-t-lg border-b-2 transition-all ${
                  activeTab === key
                    ? 'border-[var(--primary)] text-[var(--primary)] bg-white/5'
                    : 'border-transparent text-[var(--text-muted)] hover:text-white hover:bg-white/[0.03]'
                }`}
              >
                <Icon size={14} />
                {t(labelKey)}
              </button>
            ))}
          </div>
        </div>
      </div>

      {/* エラーバー */}
      {error && (
        <div className="mx-6 mt-4 bg-red-500/10 border border-red-500/20 rounded-lg p-3 flex items-center justify-between">
          <span className="text-red-400 text-xs">{error}</span>
          <button onClick={clearError} className="text-red-400 hover:text-red-300 text-xs ml-4">
            &times;
          </button>
        </div>
      )}

      {/* タブコンテンツ */}
      <div className="flex-1 overflow-y-auto">
        <div className="max-w-6xl mx-auto px-6 py-6">
          <TabContent tab={activeTab} />
        </div>
      </div>
    </div>
  );
}
