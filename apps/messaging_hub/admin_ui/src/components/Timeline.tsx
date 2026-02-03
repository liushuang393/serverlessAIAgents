import { useEffect, useState } from 'react';
import {
  Clock,
  CheckCircle,
  XCircle,
  AlertCircle,
  Loader2,
  Filter,
  Calendar,
  RefreshCw,
} from 'lucide-react';
import clsx from 'clsx';

interface ExecutionEvent {
  id: string;
  skill_name: string;
  params: Record<string, unknown>;
  status: 'pending' | 'running' | 'success' | 'failed' | 'cancelled' | 'timeout';
  started_at: string;
  completed_at: string | null;
  result: unknown;
  error: string | null;
  duration_ms: number;
  user_id: string;
}

interface ExecutionStats {
  total_executions: number;
  success_count: number;
  failed_count: number;
  success_rate: number;
  avg_duration_ms: number;
  by_skill: Record<string, number>;
  by_hour: Record<number, number>;
}

const statusConfig = {
  pending: { icon: Clock, color: 'text-gray-500', bg: 'bg-gray-100' },
  running: { icon: Loader2, color: 'text-blue-500', bg: 'bg-blue-100' },
  success: { icon: CheckCircle, color: 'text-green-500', bg: 'bg-green-100' },
  failed: { icon: XCircle, color: 'text-red-500', bg: 'bg-red-100' },
  cancelled: { icon: AlertCircle, color: 'text-yellow-500', bg: 'bg-yellow-100' },
  timeout: { icon: Clock, color: 'text-orange-500', bg: 'bg-orange-100' },
};

/**
 * タイムラインページ
 *
 * スキル実行履歴を時系列で表示
 */
export default function Timeline() {
  const [events, setEvents] = useState<ExecutionEvent[]>([]);
  const [stats, setStats] = useState<ExecutionStats | null>(null);
  const [loading, setLoading] = useState(true);
  const [filter, setFilter] = useState({
    status: '',
    skill: '',
    date: '',
  });

  useEffect(() => {
    fetchTimeline();
    fetchStats();
    const interval = setInterval(fetchTimeline, 5000);
    return () => clearInterval(interval);
  }, [filter]);

  const fetchTimeline = async () => {
    try {
      const params = new URLSearchParams();
      if (filter.status) params.append('status', filter.status);
      if (filter.skill) params.append('skill', filter.skill);
      if (filter.date) params.append('date', filter.date);

      const response = await fetch(`/api/executions?${params}`);
      if (response.ok) {
        const data = await response.json();
        setEvents(data.events || []);
      }
    } catch (error) {
      console.error('Timeline fetch error:', error);
    } finally {
      setLoading(false);
    }
  };

  const fetchStats = async () => {
    try {
      const response = await fetch('/api/executions/stats');
      if (response.ok) {
        const data = await response.json();
        setStats(data);
      }
    } catch (error) {
      console.error('Stats fetch error:', error);
    }
  };

  const formatDuration = (ms: number) => {
    if (ms < 1000) return `${Math.round(ms)}ms`;
    if (ms < 60000) return `${(ms / 1000).toFixed(1)}s`;
    return `${(ms / 60000).toFixed(1)}m`;
  };

  const formatTime = (isoString: string) => {
    const date = new Date(isoString);
    return date.toLocaleTimeString('ja-JP', { hour: '2-digit', minute: '2-digit' });
  };

  const formatDate = (isoString: string) => {
    const date = new Date(isoString);
    return date.toLocaleDateString('ja-JP', { month: 'short', day: 'numeric' });
  };

  // イベントを日付でグループ化
  const groupedEvents = events.reduce((acc, event) => {
    const date = formatDate(event.started_at);
    if (!acc[date]) acc[date] = [];
    acc[date].push(event);
    return acc;
  }, {} as Record<string, ExecutionEvent[]>);

  return (
    <div className="space-y-6">
      {/* ヘッダー */}
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-2xl font-bold text-gray-900">実行タイムライン</h1>
          <p className="text-gray-600 mt-1">スキル実行履歴を時系列で表示</p>
        </div>
        <button
          onClick={() => { fetchTimeline(); fetchStats(); }}
          className="flex items-center gap-2 px-4 py-2 bg-white border rounded-lg hover:bg-gray-50"
        >
          <RefreshCw size={16} />
          更新
        </button>
      </div>

      {/* 統計カード */}
      {stats && (
        <div className="grid grid-cols-4 gap-4">
          <div className="bg-white rounded-lg shadow p-4">
            <p className="text-sm text-gray-500">総実行数</p>
            <p className="text-2xl font-bold">{stats.total_executions}</p>
          </div>
          <div className="bg-white rounded-lg shadow p-4">
            <p className="text-sm text-gray-500">成功率</p>
            <p className="text-2xl font-bold text-green-600">{stats.success_rate}%</p>
          </div>
          <div className="bg-white rounded-lg shadow p-4">
            <p className="text-sm text-gray-500">平均実行時間</p>
            <p className="text-2xl font-bold">{formatDuration(stats.avg_duration_ms)}</p>
          </div>
          <div className="bg-white rounded-lg shadow p-4">
            <p className="text-sm text-gray-500">失敗数</p>
            <p className="text-2xl font-bold text-red-600">{stats.failed_count}</p>
          </div>
        </div>
      )}

      {/* フィルター */}
      <div className="bg-white rounded-lg shadow p-4 flex items-center gap-4">
        <Filter size={20} className="text-gray-400" />
        <select
          value={filter.status}
          onChange={(e) => setFilter({ ...filter, status: e.target.value })}
          className="border rounded-lg px-3 py-2"
        >
          <option value="">すべてのステータス</option>
          <option value="success">成功</option>
          <option value="failed">失敗</option>
          <option value="running">実行中</option>
        </select>
        <input
          type="text"
          placeholder="スキル名で検索"
          value={filter.skill}
          onChange={(e) => setFilter({ ...filter, skill: e.target.value })}
          className="border rounded-lg px-3 py-2 flex-1"
        />
        <input
          type="date"
          value={filter.date}
          onChange={(e) => setFilter({ ...filter, date: e.target.value })}
          className="border rounded-lg px-3 py-2"
        />
      </div>

      {/* タイムライン */}
      <div className="bg-white rounded-lg shadow">
        {loading ? (
          <div className="flex items-center justify-center py-12">
            <Loader2 className="animate-spin text-primary-500" size={32} />
          </div>
        ) : events.length === 0 ? (
          <div className="text-center py-12 text-gray-500">
            <Clock size={48} className="mx-auto mb-4 text-gray-300" />
            <p>実行履歴がありません</p>
          </div>
        ) : (
          <div className="divide-y">
            {Object.entries(groupedEvents).map(([date, dateEvents]) => (
              <div key={date}>
                {/* 日付ヘッダー */}
                <div className="px-6 py-3 bg-gray-50 flex items-center gap-2">
                  <Calendar size={16} className="text-gray-400" />
                  <span className="text-sm font-medium text-gray-600">{date}</span>
                </div>

                {/* イベント */}
                <div className="relative pl-8">
                  {/* タイムライン線 */}
                  <div className="absolute left-6 top-0 bottom-0 w-0.5 bg-gray-200" />

                  {dateEvents.map((event) => {
                    const config = statusConfig[event.status];
                    const Icon = config.icon;

                    return (
                      <div key={event.id} className="relative py-4 px-6">
                        {/* ドット */}
                        <div
                          className={clsx(
                            'absolute left-4 w-4 h-4 rounded-full flex items-center justify-center',
                            config.bg
                          )}
                          style={{ transform: 'translateX(-50%)' }}
                        >
                          <Icon
                            size={10}
                            className={clsx(
                              config.color,
                              event.status === 'running' && 'animate-spin'
                            )}
                          />
                        </div>

                        {/* コンテンツ */}
                        <div className="ml-6">
                          <div className="flex items-center justify-between">
                            <div className="flex items-center gap-3">
                              <span className="font-medium">{event.skill_name}</span>
                              <span
                                className={clsx(
                                  'text-xs px-2 py-0.5 rounded-full',
                                  config.bg,
                                  config.color
                                )}
                              >
                                {event.status}
                              </span>
                            </div>
                            <span className="text-sm text-gray-500">
                              {formatTime(event.started_at)}
                            </span>
                          </div>

                          <div className="mt-1 text-sm text-gray-600">
                            {event.duration_ms > 0 && (
                              <span className="mr-4">
                                ⏱️ {formatDuration(event.duration_ms)}
                              </span>
                            )}
                            <span>👤 {event.user_id}</span>
                          </div>

                          {event.error && (
                            <div className="mt-2 p-2 bg-red-50 rounded text-sm text-red-600">
                              {event.error}
                            </div>
                          )}

                          {event.params && Object.keys(event.params).length > 0 && (
                            <details className="mt-2">
                              <summary className="text-sm text-gray-500 cursor-pointer hover:text-gray-700">
                                パラメータを表示
                              </summary>
                              <pre className="mt-1 p-2 bg-gray-50 rounded text-xs overflow-auto">
                                {JSON.stringify(event.params, null, 2)}
                              </pre>
                            </details>
                          )}
                        </div>
                      </div>
                    );
                  })}
                </div>
              </div>
            ))}
          </div>
        )}
      </div>
    </div>
  );
}
