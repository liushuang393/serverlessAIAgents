import { create } from 'zustand'
import type { Node, Edge } from 'reactflow'

/**
 * ワークフローストア
 * 
 * Zustand を使用したグローバル状態管理。
 * ワークフローのノード、エッジ、選択状態を管理。
 */

interface Workflow {
  id: string
  name: string
  description: string
  nodes: Node[]
  edges: Edge[]
}

interface WorkflowStore {
  workflow: Workflow
  selectedNode: string | null
  history: Workflow[]
  historyIndex: number
  
  // アクション
  updateWorkflow: (updates: Partial<Workflow>) => void
  setSelectedNode: (nodeId: string | null) => void
  updateNodeData: (nodeId: string, data: any) => void
  saveWorkflow: () => Promise<void>
  loadWorkflow: (workflowId: string) => Promise<void>
  undo: () => void
  redo: () => void
  canUndo: () => boolean
  canRedo: () => boolean
}

export const useWorkflowStore = create<WorkflowStore>((set, get) => ({
  workflow: {
    id: 'new-workflow',
    name: '新しいワークフロー',
    description: '',
    nodes: [],
    edges: [],
  },
  selectedNode: null,
  history: [],
  historyIndex: -1,

  /**
   * ワークフローを更新
   * 
   * 履歴に追加し、変更を保存。
   */
  updateWorkflow: (updates) => {
    set((state) => {
      const newWorkflow = { ...state.workflow, ...updates }
      const newHistory = state.history.slice(0, state.historyIndex + 1)
      newHistory.push(newWorkflow)
      
      return {
        workflow: newWorkflow,
        history: newHistory,
        historyIndex: newHistory.length - 1,
      }
    })
  },

  /**
   * 選択ノードを設定
   */
  setSelectedNode: (nodeId) => {
    set({ selectedNode: nodeId })
  },

  /**
   * ノードデータを更新
   */
  updateNodeData: (nodeId, data) => {
    set((state) => {
      const newNodes = state.workflow.nodes.map((node) =>
        node.id === nodeId ? { ...node, data } : node
      )
      return {
        workflow: { ...state.workflow, nodes: newNodes },
      }
    })
  },

  /**
   * ワークフローを保存
   */
  saveWorkflow: async () => {
    const { workflow } = get()
    
    try {
      const response = await fetch(`/api/workflows/${workflow.id}`, {
        method: 'PUT',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          name: workflow.name,
          description: workflow.description,
          nodes: workflow.nodes,
          edges: workflow.edges,
        }),
      })

      if (!response.ok) {
        throw new Error('Failed to save workflow')
      }

      console.log('Workflow saved successfully')
    } catch (error) {
      console.error('Failed to save workflow:', error)
      throw error
    }
  },

  /**
   * ワークフローを読み込み
   */
  loadWorkflow: async (workflowId) => {
    try {
      const response = await fetch(`/api/workflows/${workflowId}`)
      
      if (!response.ok) {
        throw new Error('Failed to load workflow')
      }

      const data = await response.json()
      
      set({
        workflow: {
          id: data.id,
          name: data.name,
          description: data.description,
          nodes: data.nodes || [],
          edges: data.edges || [],
        },
        history: [],
        historyIndex: -1,
      })

      console.log('Workflow loaded successfully')
    } catch (error) {
      console.error('Failed to load workflow:', error)
      throw error
    }
  },

  /**
   * 元に戻す
   */
  undo: () => {
    set((state) => {
      if (state.historyIndex > 0) {
        const newIndex = state.historyIndex - 1
        return {
          workflow: state.history[newIndex]!,
          historyIndex: newIndex,
        }
      }
      return state
    })
  },

  /**
   * やり直す
   */
  redo: () => {
    set((state) => {
      if (state.historyIndex < state.history.length - 1) {
        const newIndex = state.historyIndex + 1
        return {
          workflow: state.history[newIndex]!,
          historyIndex: newIndex,
        }
      }
      return state
    })
  },

  /**
   * 元に戻せるかチェック
   */
  canUndo: () => {
    const { historyIndex } = get()
    return historyIndex > 0
  },

  /**
   * やり直せるかチェック
   */
  canRedo: () => {
    const { historyIndex, history } = get()
    return historyIndex < history.length - 1
  },
}))

