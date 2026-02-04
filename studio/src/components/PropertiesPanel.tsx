import { X } from 'lucide-react';
import { useWorkflowStore } from '../stores/workflowStore';

/**
 * プロパティパネルコンポーネント
 *
 * 選択されたノードの設定を表示・編集。
 * ノードが選択されていない場合は非表示。
 */

export default function PropertiesPanel() {
  const { selectedNode, workflow, setSelectedNode, updateNodeData } =
    useWorkflowStore();

  // 選択されたノードを取得
  const node = workflow.nodes.find((n) => n.id === selectedNode);

  if (!node) {
    return null;
  }

  /**
   * パネルを閉じる
   */
  const handleClose = () => {
    setSelectedNode(null);
  };

  /**
   * ノードデータを更新
   */
  const handleConfigChange = (key: string, value: unknown) => {
    updateNodeData(node.id, {
      ...node.data,
      config: {
        ...node.data.config,
        [key]: value,
      },
    });
  };

  return (
    <div className="w-80 border-l border-border bg-card flex flex-col">
      {/* ヘッダー */}
      <div className="p-4 border-b border-border flex items-center justify-between">
        <h2 className="text-lg font-semibold text-foreground">プロパティ</h2>
        <button
          onClick={handleClose}
          className="p-1 rounded hover:bg-accent transition-colors"
          title="閉じる"
        >
          <X className="w-4 h-4 text-muted-foreground" />
        </button>
      </div>

      {/* ノード情報 */}
      <div className="flex-1 overflow-y-auto p-4 space-y-4">
        {/* ノード名 */}
        <div>
          <label className="block text-sm font-medium text-foreground mb-2">
            ノード名
          </label>
          <input
            type="text"
            value={node.data.label}
            onChange={(e) =>
              updateNodeData(node.id, {
                ...node.data,
                label: e.target.value,
              })
            }
            className="w-full px-3 py-2 text-sm rounded-md border border-input bg-background text-foreground focus:outline-none focus:ring-2 focus:ring-ring"
          />
        </div>

        {/* エージェント ID */}
        <div>
          <label className="block text-sm font-medium text-foreground mb-2">
            エージェント ID
          </label>
          <div className="px-3 py-2 text-sm rounded-md border border-input bg-muted text-muted-foreground">
            {node.data.agentId}
          </div>
        </div>

        {/* 設定 */}
        <div>
          <label className="block text-sm font-medium text-foreground mb-2">
            設定
          </label>
          <div className="space-y-3">
            {/* 設定項目の例 */}
            <div>
              <label className="block text-xs text-muted-foreground mb-1">
                入力データ
              </label>
              <textarea
                value={node.data.config.input || ''}
                onChange={(e) => handleConfigChange('input', e.target.value)}
                placeholder='{"key": "value"}'
                className="w-full px-3 py-2 text-sm rounded-md border border-input bg-background text-foreground font-mono focus:outline-none focus:ring-2 focus:ring-ring"
                rows={4}
              />
            </div>

            <div>
              <label className="block text-xs text-muted-foreground mb-1">
                タイムアウト (秒)
              </label>
              <input
                type="number"
                value={node.data.config.timeout || 30}
                onChange={(e) =>
                  handleConfigChange('timeout', parseInt(e.target.value))
                }
                className="w-full px-3 py-2 text-sm rounded-md border border-input bg-background text-foreground focus:outline-none focus:ring-2 focus:ring-ring"
              />
            </div>

            <div>
              <label className="flex items-center gap-2 text-sm text-foreground cursor-pointer">
                <input
                  type="checkbox"
                  checked={node.data.config.enabled !== false}
                  onChange={(e) =>
                    handleConfigChange('enabled', e.target.checked)
                  }
                  className="w-4 h-4 rounded border-input text-primary focus:ring-2 focus:ring-ring"
                />
                有効化
              </label>
            </div>
          </div>
        </div>

        {/* ノード位置 */}
        <div>
          <label className="block text-sm font-medium text-foreground mb-2">
            位置
          </label>
          <div className="grid grid-cols-2 gap-2">
            <div>
              <label className="block text-xs text-muted-foreground mb-1">
                X
              </label>
              <div className="px-3 py-2 text-sm rounded-md border border-input bg-muted text-muted-foreground">
                {Math.round(node.position.x)}
              </div>
            </div>
            <div>
              <label className="block text-xs text-muted-foreground mb-1">
                Y
              </label>
              <div className="px-3 py-2 text-sm rounded-md border border-input bg-muted text-muted-foreground">
                {Math.round(node.position.y)}
              </div>
            </div>
          </div>
        </div>
      </div>

      {/* フッター */}
      <div className="p-4 border-t border-border">
        <button
          onClick={handleClose}
          className="w-full px-4 py-2 text-sm font-medium rounded-md bg-primary text-primary-foreground hover:bg-primary/90 transition-colors"
        >
          完了
        </button>
      </div>
    </div>
  );
}
