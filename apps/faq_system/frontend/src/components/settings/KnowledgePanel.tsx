/**
 * ナレッジベース管理サイドパネル.
 *
 * チャット画面右側からスライドインするパネル。
 * コレクション、ドキュメント、検索設定、アクセスの4タブを持つ。
 * 各タブの詳細実装は後続タスクで行う。
 */

import type { JSX } from "react";
import {
  Settings2,
  X,
  Database,
  FileText,
  Search,
  Shield,
  BarChart3,
  Clock,
} from "lucide-react";
import { useI18n } from "../../i18n";
import { useRAGStore, type RAGTab } from "../../stores/ragStore";
import { PanelAccess } from "./PanelAccess";
import { PanelCollections } from "./PanelCollections";
import { PanelDocuments } from "./PanelDocuments";
import { PanelRetrieval } from "./PanelRetrieval";
import { KBDashboard } from "../rag/KBDashboard";
import { IngestHistoryPage } from "../rag/IngestHistoryPage";

/** タブ定義の型 */
interface TabDef {
  /** タブ識別キー */
  key: string;
  /** i18n キー */
  i18nKey: string;
  /** タブアイコンコンポーネント */
  icon: typeof Database;
}

/** パネルのタブ一覧 */
const tabs: TabDef[] = [
  {
    key: "dashboard",
    i18nKey: "knowledge_panel.tab_dashboard",
    icon: BarChart3,
  },
  {
    key: "collections",
    i18nKey: "knowledge_panel.tab_collections",
    icon: Database,
  },
  {
    key: "documents",
    i18nKey: "knowledge_panel.tab_documents",
    icon: FileText,
  },
  { key: "ingest", i18nKey: "knowledge_panel.tab_ingest", icon: Clock },
  { key: "retrieval", i18nKey: "knowledge_panel.tab_retrieval", icon: Search },
  { key: "access", i18nKey: "knowledge_panel.tab_access", icon: Shield },
];

/** KnowledgePanel の props 型定義 */
interface KnowledgePanelProps {
  /** パネルが開いているか */
  isOpen: boolean;
  /** パネルを閉じるコールバック */
  onClose: () => void;
}

/** ナレッジベース管理サイドパネル */
export const KnowledgePanel = ({
  isOpen,
  onClose,
}: KnowledgePanelProps): JSX.Element => {
  const { t } = useI18n();
  /** RAG ストアのタブ状態を共有し、KBDashboard 等の内部リンクと同期 */
  const { activeTab, setActiveTab } = useRAGStore();

  return (
    <div
      data-testid="knowledge-panel"
      className={`fixed top-0 right-0 h-full w-[480px] max-w-[90vw] z-50 glass border-l border-white/5 shadow-2xl transition-transform duration-300 ease-in-out ${isOpen ? "translate-x-0" : "translate-x-full"
        }`}
    >
      {/* ヘッダー */}
      <div className="flex items-center justify-between p-4 border-b border-white/5">
        <div className="flex items-center gap-2">
          <Settings2 size={18} className="text-[var(--primary)]" />
          <h2 className="text-base font-bold text-white">
            {t("knowledge_panel.title")}
          </h2>
        </div>
        <button
          onClick={onClose}
          className="p-2 rounded-xl hover:bg-white/5 text-[var(--text-muted)] hover:text-white transition-all"
          aria-label={t("knowledge_panel.close")}
        >
          <X size={20} />
        </button>
      </div>

      {/* タブナビゲーション */}
      <div className="flex gap-1 px-4 pt-3 border-b border-white/5">
        {tabs.map(({ key, i18nKey, icon: Icon }) => (
          <button
            key={key}
            onClick={() => setActiveTab(key as RAGTab)}
            className={`flex items-center gap-1.5 px-3 py-2 text-xs font-medium rounded-t-lg border-b-2 transition-all ${activeTab === key
              ? "border-[var(--primary)] text-[var(--primary)] bg-white/5"
              : "border-transparent text-[var(--text-muted)] hover:text-white hover:bg-white/[0.03]"
              }`}
          >
            <Icon size={14} />
            {t(i18nKey)}
          </button>
        ))}
      </div>

      {/* タブコンテンツ */}
      <div className="flex-1 overflow-y-auto p-4">
        {activeTab === "dashboard" && <KBDashboard />}
        {activeTab === "collections" && <PanelCollections />}
        {activeTab === "documents" && <PanelDocuments />}
        {activeTab === "ingest" && <IngestHistoryPage />}
        {activeTab === "retrieval" && <PanelRetrieval />}
        {activeTab === "access" && <PanelAccess />}
      </div>
    </div>
  );
};
