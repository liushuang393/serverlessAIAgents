import { useCallback, useEffect, type JSX } from "react";
import {
  X,
  Database,
  FileText,
  Search,
  Shield,
  BarChart3,
  Clock,
  UserCog,
} from "lucide-react";
import { useI18n } from "../../i18n";
import { useRAGStore, type RAGTab } from "../../stores/ragStore";
import { PanelAccess } from "./PanelAccess";
import { PanelCollections } from "./PanelCollections";
import { PanelDocuments } from "./PanelDocuments";
import { PanelRetrieval } from "./PanelRetrieval";
import { RoleManager } from "./RoleManager";
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
  /** admin のみ表示（省略時は全ユーザー表示） */
  adminOnly?: boolean;
}

/** パネルのタブ一覧 */
const allTabs: TabDef[] = [
  { key: "dashboard", i18nKey: "knowledge_panel.tab_dashboard", icon: BarChart3 },
  { key: "collections", i18nKey: "knowledge_panel.tab_collections", icon: Database },
  { key: "documents", i18nKey: "knowledge_panel.tab_documents", icon: FileText },
  { key: "ingest", i18nKey: "knowledge_panel.tab_ingest", icon: Clock },
  { key: "retrieval", i18nKey: "knowledge_panel.tab_retrieval", icon: Search },
  { key: "access", i18nKey: "knowledge_panel.tab_access", icon: Shield },
  { key: "role_management", i18nKey: "knowledge_panel.tab_role_management", icon: UserCog, adminOnly: true },
];

/** ユーザー情報型（localStorage から取得） */
interface StoredUser {
  role?: string;
}

function getCurrentUserRole(): string {
  try {
    const raw = localStorage.getItem("user_info");
    if (raw) {
      const parsed: StoredUser = JSON.parse(raw);
      return parsed.role ?? "guest";
    }
  } catch {
    // パース失敗時はデフォルト
  }
  return "guest";
}

/** KnowledgePanel の props 型定義 */
interface KnowledgePanelProps {
  /** パネルが開いているか */
  isOpen: boolean;
  /** パネルを閉じるコールバック */
  onClose: () => void;
}

/** ナレッジベース管理モーダル */
export const KnowledgePanel = ({
  isOpen,
  onClose,
}: KnowledgePanelProps): JSX.Element => {
  const { t } = useI18n();
  /** RAG ストアのタブ状態を共有し、KBDashboard 等の内部リンクと同期 */
  const { activeTab, setActiveTab } = useRAGStore();
  const currentRole = getCurrentUserRole();
  const isAdmin = currentRole === "admin";

  /** admin のみのタブをフィルタ */
  const tabs = allTabs.filter((tab) => !tab.adminOnly || isAdmin);

  /** Escape キーでモーダルを閉じる */
  const handleKeyDown = useCallback(
    (e: KeyboardEvent) => {
      if (e.key === "Escape") onClose();
    },
    [onClose],
  );

  useEffect(() => {
    if (isOpen) {
      document.addEventListener("keydown", handleKeyDown);
      return () => document.removeEventListener("keydown", handleKeyDown);
    }
  }, [isOpen, handleKeyDown]);

  return (
    <div
      data-testid="knowledge-panel"
      className={`fixed inset-0 z-50 flex items-center justify-center p-4 sm:p-6 transition-all duration-300 ${isOpen ? "opacity-100 visible" : "opacity-0 invisible pointer-events-none"
        }`}
    >
      {/* 背景（クリックで閉じる） */}
      <div
        className={`absolute inset-0 bg-black/60 backdrop-blur-md transition-opacity duration-300 ${isOpen ? "opacity-100" : "opacity-0"
          }`}
        onClick={onClose}
      />

      {/* モーダル本体 */}
      <div
        className={`relative w-full max-w-5xl h-[85vh] flex flex-col bg-[var(--bg-main)] border border-white/10 rounded-2xl shadow-[0_32px_64px_rgba(0,0,0,0.6)] overflow-hidden transition-all duration-300 transform ${isOpen ? "scale-100 translate-y-0" : "scale-95 translate-y-8"
          }`}
      >
        {/* ヘッダー */}
        <div className="flex items-center justify-between p-6 px-8 border-b border-white/10 bg-gradient-to-r from-white/[0.03] to-transparent">
          <div className="flex items-center gap-4">
            <div className="w-10 h-10 rounded-2xl bg-gradient-to-br from-[var(--primary)]/20 to-[var(--primary)]/5 flex items-center justify-center border border-[var(--primary)]/20 shadow-[0_0_20px_rgba(94,234,212,0.1)]">
              <Database size={20} className="text-[var(--primary)]" />
            </div>
            <div>
              <h2 className="text-xl font-bold text-white tracking-tight">
                {t("knowledge_panel.title")}
              </h2>
              <p className="text-[10px] text-[var(--text-muted)] uppercase tracking-[0.2em] font-bold opacity-70">
                Knowledge Base Management
              </p>
            </div>
          </div>
          <button
            onClick={onClose}
            className="p-2.5 rounded-xl hover:bg-white/5 text-[var(--text-muted)] hover:text-white transition-all border border-transparent hover:border-white/10 group"
            aria-label={t("knowledge_panel.close")}
          >
            <X size={22} className="group-hover:rotate-90 transition-transform duration-300" />
          </button>
        </div>

        {/* タブナビゲーション */}
        <div className="flex gap-1 px-8 pt-[23px] border-b border-white/5 bg-white/[0.01]">
          {tabs.map(({ key, i18nKey, icon: Icon }) => {
            const isActive = activeTab === key;
            return (
              <button
                key={key}
                onClick={() => setActiveTab(key as RAGTab)}
                className={`flex items-center gap-2 px-5 py-3.5 text-xs font-bold rounded-t-xl transition-all relative group ${isActive
                  ? "text-[var(--primary)] bg-[var(--primary)]/5"
                  : "text-[var(--text-muted)] hover:text-white hover:bg-white/[0.03]"
                  }`}
              >
                <Icon size={16} className={isActive ? "animate-pulse" : "group-hover:scale-110 transition-transform"} />
                {t(i18nKey)}
                {isActive && (
                  <div className="absolute bottom-0 left-0 right-0 h-0.5 bg-[var(--primary)] shadow-[0_0_10px_rgba(94,234,212,0.5)]" />
                )}
              </button>
            );
          })}
        </div>

        {/* タブコンテンツ */}
        <div className="flex-1 overflow-y-auto p-6 px-8 custom-scrollbar bg-black/10">
          <div className="max-w-5xl mx-auto h-full">
            {activeTab === "dashboard" && <KBDashboard />}
            {activeTab === "collections" && <PanelCollections />}
            {activeTab === "documents" && <PanelDocuments />}
            {activeTab === "ingest" && <IngestHistoryPage />}
            {activeTab === "retrieval" && <PanelRetrieval />}
            {activeTab === "access" && <PanelAccess />}
            {activeTab === "role_management" && isAdmin && <RoleManager />}
          </div>
        </div>
      </div>
    </div>
  );
};
