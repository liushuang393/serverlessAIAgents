import { useCallback } from 'react'
import ReactFlow, {
  Background,
  Controls,
  MiniMap,
  addEdge,
  useNodesState,
  useEdgesState,
  type Connection,
  type Edge,
  type Node,
} from 'reactflow'
import AgentNode from './AgentNode'
import { useWorkflowStore } from '../stores/workflowStore'

/**
 * ワークフローキャンバスコンポーネント
 * 
 * React Flow を使用したビジュアルワークフローエディタ。
 * エージェントノードのドラッグ&ドロップ、接続、編集機能を提供。
 */

// カスタムノードタイプの定義
const nodeTypes = {
  agent: AgentNode,
}

export default function Canvas() {
  const { workflow, updateWorkflow } = useWorkflowStore()
  
  const [nodes, setNodes, onNodesChange] = useNodesState(workflow.nodes)
  const [edges, setEdges, onEdgesChange] = useEdgesState(workflow.edges)

  /**
   * ノード接続時のハンドラー
   * 
   * 2つのノードを接続する際に呼び出される。
   * エッジを追加し、ワークフローストアを更新。
   */
  const onConnect = useCallback(
    (connection: Connection) => {
      const newEdge = addEdge(connection, edges)
      setEdges(newEdge)
      updateWorkflow({ edges: newEdge })
    },
    [edges, setEdges, updateWorkflow]
  )

  /**
   * ドラッグオーバー時のハンドラー
   * 
   * サイドバーからエージェントをドラッグしている際に呼び出される。
   * ドロップを許可するために preventDefault を呼び出す。
   */
  const onDragOver = useCallback((event: React.DragEvent) => {
    event.preventDefault()
    event.dataTransfer.dropEffect = 'move'
  }, [])

  /**
   * ドロップ時のハンドラー
   * 
   * サイドバーからエージェントをキャンバスにドロップした際に呼び出される。
   * 新しいエージェントノードを作成し、キャンバスに追加。
   */
  const onDrop = useCallback(
    (event: React.DragEvent) => {
      event.preventDefault()

      const agentData = event.dataTransfer.getData('application/reactflow')
      if (!agentData) return

      const agent = JSON.parse(agentData)
      const position = {
        x: event.clientX - 250, // サイドバー幅を考慮
        y: event.clientY - 40,  // ヘッダー高さを考慮
      }

      const newNode: Node = {
        id: `${agent.id}-${Date.now()}`,
        type: 'agent',
        position,
        data: {
          label: agent.name,
          agentId: agent.id,
          config: {},
        },
      }

      const newNodes = [...nodes, newNode]
      setNodes(newNodes)
      updateWorkflow({ nodes: newNodes })
    },
    [nodes, setNodes, updateWorkflow]
  )

  /**
   * ノード変更時のハンドラー
   * 
   * ノードの位置変更、削除などの際に呼び出される。
   * ワークフローストアを更新。
   */
  const handleNodesChange = useCallback(
    (changes: any) => {
      onNodesChange(changes)
      // ノード変更後にストアを更新
      setTimeout(() => {
        updateWorkflow({ nodes })
      }, 0)
    },
    [nodes, onNodesChange, updateWorkflow]
  )

  /**
   * エッジ変更時のハンドラー
   * 
   * エッジの削除などの際に呼び出される。
   * ワークフローストアを更新。
   */
  const handleEdgesChange = useCallback(
    (changes: any) => {
      onEdgesChange(changes)
      // エッジ変更後にストアを更新
      setTimeout(() => {
        updateWorkflow({ edges })
      }, 0)
    },
    [edges, onEdgesChange, updateWorkflow]
  )

  return (
    <div className="w-full h-full">
      <ReactFlow
        nodes={nodes}
        edges={edges}
        onNodesChange={handleNodesChange}
        onEdgesChange={handleEdgesChange}
        onConnect={onConnect}
        onDragOver={onDragOver}
        onDrop={onDrop}
        nodeTypes={nodeTypes}
        fitView
        className="bg-background"
      >
        {/* グリッド背景 */}
        <Background />
        
        {/* ズーム・パンコントロール */}
        <Controls />
        
        {/* ミニマップ */}
        <MiniMap
          nodeColor={(node) => {
            switch (node.type) {
              case 'agent':
                return '#3b82f6'
              default:
                return '#6b7280'
            }
          }}
          className="bg-card border border-border"
        />
      </ReactFlow>
    </div>
  )
}

