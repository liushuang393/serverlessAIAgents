/**
 * ナレッジベース管理サイドパネル.
 *
 * チャット画面右側からスライドインするパネル。
 * コレクション、ドキュメント、検索設定、アクセスの4タブを持つ。
 * 各タブの詳細実装は後続タスクで行う。
 */

import { useState } from 'react';
import { Settings2, X, Database, FileText, Search, Shield } from 'lucide-react';
import { PanelAccess } from './PanelAccess';
import { PanelCollections } from './PanelCollections';
import { PanelDocuments } from './PanelDocuments';
import { PanelRetrieval } from './PanelRetrieval';

/** タブ定義の型 */
interface TabDef {
  /** タブ識別キー */
  key: string;
  /** タブ表示ラベル */
  label: string;
  /** タブアイコンコンポーネント */
  icon: typeof Database;
}

/** パネルのタブ一覧 */
const tabs: TabDef[] = [
  { key: 'collections', label: 'コレクション', icon: Database },
  { key: 'documents', label: 'ドキュメント', icon: FileText },
  { key: 'retrieval', label: '検索設定', icon: Search },
  { key: 'access', label: 'アクセス', icon: Shield },
];

/** KnowledgePanel の props 型定義 */
interface KnowledgePanelProps {
  /** パネルが開いているか */
  isOpen: boolean;
  /** パネルを閉じるコールバック */
  onClose: () => void;
}

/** ナレッジベース管理サイドパネル */
export const KnowledgePanel = ({ isOpen, onClose }: KnowledgePanelProps): JSX.Element => {
  /** 現在選択中のタブキー */
  const [activeTab, setActiveTab] = useState<string>('collections');

  return (
    <div
      data-testid="knowledge-panel"
      className={`fixed top-0 right-0 h-full w-[480px] max-w-[90vw] z-50 glass border-l border-white/5 shadow-2xl transition-transform duration-300 ease-in-out ${
        isOpen ? 'translate-x-0' : 'translate-x-full'
      }`}
    >
      {/* ヘッダー */}
      <div className="flex items-center justify-between p-4 border-b border-white/5">
        <div className="flex items-center gap-2">
          <Settings2 size={18} className="text-[var(--primary)]" />
          <h2 className="text-base font-bold text-white">ナレッジベース管理</h2>
        </div>
        <button
          onClick={onClose}
          className="p-2 rounded-xl hover:bg-white/5 text-[var(--text-muted)] hover:text-white transition-all"
          aria-label="閉じる"
        >
          <X size={20} />
        </button>
      </div>

      {/* タブナビゲーション */}
      <div className="flex gap-1 px-4 pt-3 border-b border-white/5">
        {tabs.map(({ key, label, icon: Icon }) => (
          <button
            key={key}
            onClick={() => setActiveTab(key)}
            className={`flex items-center gap-1.5 px-3 py-2 text-xs font-medium rounded-t-lg border-b-2 transition-all ${
              activeTab === key
                ? 'border-[var(--primary)] text-[var(--primary)] bg-white/5'
                : 'border-transparent text-[var(--text-muted)] hover:text-white hover:bg-white/[0.03]'
            }`}
          >
            <Icon size={14} />
            {label}
          </button>
        ))}
      </div>

      {/* タブコンテンツ */}
      <div className="flex-1 overflow-y-auto p-4">
        {activeTab === 'collections' && <PanelCollections />}
        {activeTab === 'documents' && <PanelDocuments />}
        {activeTab === 'retrieval' && <PanelRetrieval />}
        {activeTab === 'access' && <PanelAccess />}
      </div>
    </div>
  );
};
