import React, { useEffect, useState } from 'react';
import { Search, Package } from 'lucide-react';

/**
 * サイドバーコンポーネント
 *
 * インストール済みエージェントの一覧を表示。
 * エージェントをキャンバスにドラッグ&ドロップ可能。
 */

interface Agent {
  id: string;
  name: string;
  version: string;
  description: string;
  category: string;
}

export default function Sidebar() {
  const [agents, setAgents] = useState<Agent[]>([]);
  const [searchQuery, setSearchQuery] = useState('');
  const [loading, setLoading] = useState(true);

  /**
   * エージェント一覧を取得
   */
  useEffect(() => {
    fetchAgents();
  }, []);

  const fetchAgents = async () => {
    try {
      const response = await fetch('/api/agents');
      const data = await response.json();
      setAgents(data);
    } catch (error) {
      console.error('Failed to fetch agents:', error);
    } finally {
      setLoading(false);
    }
  };

  /**
   * ドラッグ開始時のハンドラー
   *
   * エージェント情報を dataTransfer に設定。
   */
  const onDragStart = (event: React.DragEvent, agent: Agent) => {
    event.dataTransfer.setData('application/reactflow', JSON.stringify(agent));
    event.dataTransfer.effectAllowed = 'move';
  };

  /**
   * 検索フィルター
   */
  const filteredAgents = agents.filter(
    (agent) =>
      agent.name.toLowerCase().includes(searchQuery.toLowerCase()) ||
      agent.description.toLowerCase().includes(searchQuery.toLowerCase()),
  );

  return (
    <div className="w-64 border-r border-border bg-card flex flex-col">
      {/* ヘッダー */}
      <div className="p-4 border-b border-border">
        <h2 className="text-lg font-semibold text-foreground mb-3">
          エージェント
        </h2>

        {/* 検索ボックス */}
        <div className="relative">
          <Search className="absolute left-3 top-1/2 -translate-y-1/2 w-4 h-4 text-muted-foreground" />
          <input
            type="text"
            placeholder="検索..."
            value={searchQuery}
            onChange={(e) => setSearchQuery(e.target.value)}
            className="w-full pl-9 pr-3 py-2 text-sm rounded-md border border-input bg-background text-foreground placeholder:text-muted-foreground focus:outline-none focus:ring-2 focus:ring-ring"
          />
        </div>
      </div>

      {/* エージェント一覧 */}
      <div className="flex-1 overflow-y-auto p-2">
        {loading ? (
          <div className="flex items-center justify-center h-32 text-muted-foreground">
            読み込み中...
          </div>
        ) : filteredAgents.length === 0 ? (
          <div className="flex flex-col items-center justify-center h-32 text-muted-foreground text-sm">
            <Package className="w-8 h-8 mb-2" />
            <p>エージェントが見つかりません</p>
          </div>
        ) : (
          <div className="space-y-2">
            {filteredAgents.map((agent) => (
              <div
                key={agent.id}
                draggable
                onDragStart={(e) => onDragStart(e, agent)}
                className="p-3 rounded-lg border border-border bg-background hover:bg-accent cursor-move transition-colors"
              >
                <div className="flex items-start justify-between gap-2 mb-1">
                  <div className="font-medium text-sm text-foreground">
                    {agent.name}
                  </div>
                  <div className="text-xs text-muted-foreground">
                    v{agent.version}
                  </div>
                </div>
                <div className="text-xs text-muted-foreground line-clamp-2">
                  {agent.description}
                </div>
                <div className="mt-2">
                  <span className="inline-block px-2 py-0.5 text-xs rounded-full bg-primary/10 text-primary">
                    {agent.category}
                  </span>
                </div>
              </div>
            ))}
          </div>
        )}
      </div>

      {/* フッター */}
      <div className="p-4 border-t border-border text-xs text-muted-foreground">
        {filteredAgents.length} 個のエージェント
      </div>
    </div>
  );
}
