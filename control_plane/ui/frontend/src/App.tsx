import { useState } from 'react';
import { ReactFlowProvider } from 'reactflow';
import 'reactflow/dist/style.css';
import Canvas from './components/Canvas';
import Sidebar from './components/Sidebar';
import PropertiesPanel from './components/PropertiesPanel';
import PreviewPanel from './components/PreviewPanel';
import PublishDialog from './components/PublishDialog';
import { useWorkflowStore } from './stores/workflowStore';

/**
 * AgentFlow Studio メインアプリケーション
 *
 * ビジュアルワークフローエディタのメインコンポーネント。
 * React Flow を使用してドラッグ&ドロップ可能なキャンバスを提供。
 *
 * v0.3.0 新機能:
 * - Preview パネル: ワークフローの実行とデバッグ
 * - Publish ダイアログ: コード生成とデプロイ
 */
function App() {
  const { workflow, saveWorkflow } = useWorkflowStore();
  const [showPreview, setShowPreview] = useState(true);
  const [showPublish, setShowPublish] = useState(false);
  const [saving, setSaving] = useState(false);

  const handleSave = async () => {
    setSaving(true);
    try {
      await saveWorkflow();
    } catch (error) {
      console.error('Save failed:', error);
    } finally {
      setSaving(false);
    }
  };

  return (
    <ReactFlowProvider>
      <div className="flex flex-col h-screen w-screen overflow-hidden bg-background">
        {/* ツールバー */}
        <div className="h-12 border-b bg-background flex items-center justify-between px-4">
          {/* 左側: ロゴ & ワークフロー名 */}
          <div className="flex items-center gap-4">
            <span className="font-bold text-primary">🤖 AgentFlow Studio</span>
            <span className="text-sm text-muted-foreground">
              {workflow.name || '新しいワークフロー'}
            </span>
          </div>

          {/* 右側: アクションボタン */}
          <div className="flex items-center gap-2">
            <button
              onClick={handleSave}
              disabled={saving}
              className="px-3 py-1.5 text-sm border rounded-md hover:bg-muted disabled:opacity-50 flex items-center gap-1"
            >
              {saving ? '💾 保存中...' : '💾 保存'}
            </button>
            <button
              onClick={() => setShowPreview(!showPreview)}
              className={`px-3 py-1.5 text-sm border rounded-md hover:bg-muted flex items-center gap-1 ${
                showPreview ? 'bg-primary/10 border-primary' : ''
              }`}
            >
              ▶ Preview
            </button>
            <button
              onClick={() => setShowPublish(true)}
              className="px-3 py-1.5 text-sm bg-primary text-primary-foreground rounded-md hover:bg-primary/90 flex items-center gap-1"
            >
              🚀 発行
            </button>
          </div>
        </div>

        {/* メインコンテンツ */}
        <div className="flex flex-1 overflow-hidden">
          {/* サイドバー: エージェント一覧 */}
          <Sidebar />

          {/* メインキャンバス: ワークフローエディタ */}
          <div className="flex-1 relative">
            <Canvas />
          </div>

          {/* プロパティパネル: 選択ノードの設定 */}
          <PropertiesPanel />

          {/* プレビューパネル: 実行とデバッグ */}
          {showPreview && <PreviewPanel />}
        </div>
      </div>

      {/* 発行ダイアログ */}
      <PublishDialog open={showPublish} onClose={() => setShowPublish(false)} />
    </ReactFlowProvider>
  );
}

export default App;
