import { ReactFlowProvider } from 'reactflow'
import 'reactflow/dist/style.css'
import Canvas from './components/Canvas'
import Sidebar from './components/Sidebar'
import PropertiesPanel from './components/PropertiesPanel'

/**
 * AgentFlow Studio メインアプリケーション
 * 
 * ビジュアルワークフローエディタのメインコンポーネント。
 * React Flow を使用してドラッグ&ドロップ可能なキャンバスを提供。
 */
function App() {
  return (
    <ReactFlowProvider>
      <div className="flex h-screen w-screen overflow-hidden bg-background">
        {/* サイドバー: エージェント一覧 */}
        <Sidebar />
        
        {/* メインキャンバス: ワークフローエディタ */}
        <div className="flex-1 relative">
          <Canvas />
        </div>
        
        {/* プロパティパネル: 選択ノードの設定 */}
        <PropertiesPanel />
      </div>
    </ReactFlowProvider>
  )
}

export default App

