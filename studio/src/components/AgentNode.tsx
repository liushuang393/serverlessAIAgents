import { memo } from "react";
import { Handle, Position, type NodeProps } from "reactflow";
import { Settings } from "lucide-react";
import { useWorkflowStore } from "../stores/workflowStore";

/**
 * エージェントノードコンポーネント
 *
 * ワークフローキャンバス上のエージェントを表すカスタムノード。
 * 入力/出力ハンドル、設定ボタンを含む。
 */

interface AgentNodeData {
  label: string;
  agentId: string;
  config: Record<string, unknown>;
}

function AgentNode({ id, data, selected }: NodeProps<AgentNodeData>) {
  const { setSelectedNode } = useWorkflowStore();

  /**
   * 設定ボタンクリック時のハンドラー
   *
   * プロパティパネルを開き、ノードの設定を表示。
   */
  const handleSettingsClick = () => {
    setSelectedNode(id);
  };

  return (
    <div
      className={`
        px-4 py-3 rounded-lg border-2 bg-card shadow-lg
        min-w-[200px] transition-all
        ${selected ? "border-primary ring-2 ring-primary/20" : "border-border"}
      `}
    >
      {/* 入力ハンドル */}
      <Handle type="target" position={Position.Top} className="w-3 h-3 !bg-primary" />

      {/* ノードヘッダー */}
      <div className="flex items-center justify-between gap-2 mb-2">
        <div className="flex-1">
          <div className="text-sm font-semibold text-foreground">{data.label}</div>
          <div className="text-xs text-muted-foreground">{data.agentId}</div>
        </div>

        {/* 設定ボタン */}
        <button
          onClick={handleSettingsClick}
          className="p-1 rounded hover:bg-accent transition-colors"
          title="設定を開く"
        >
          <Settings className="w-4 h-4 text-muted-foreground" />
        </button>
      </div>

      {/* ノード情報 */}
      {Object.keys(data.config).length > 0 && (
        <div className="text-xs text-muted-foreground border-t border-border pt-2 mt-2">
          {Object.keys(data.config).length} 個の設定
        </div>
      )}

      {/* 出力ハンドル */}
      <Handle type="source" position={Position.Bottom} className="w-3 h-3 !bg-primary" />
    </div>
  );
}

export default memo(AgentNode);
