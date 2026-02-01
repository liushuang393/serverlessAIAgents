import { useQuery } from '@tanstack/react-query';
import {
  LineChart,
  Line,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  ResponsiveContainer,
} from 'recharts';
import { MessageSquare, Users, Radio, Activity } from 'lucide-react';
import { getStatistics, getHealth } from '../api/client';

/**
 * 統計カード
 */
function StatCard({
  icon: Icon,
  title,
  value,
  change,
}: {
  icon: React.ElementType;
  title: string;
  value: number | string;
  change?: string;
}) {
  return (
    <div className="bg-white rounded-lg shadow p-6">
      <div className="flex items-center justify-between">
        <div>
          <p className="text-sm text-gray-500">{title}</p>
          <p className="text-2xl font-bold mt-1">{value}</p>
          {change && (
            <p className="text-sm text-green-600 mt-1">{change}</p>
          )}
        </div>
        <div className="p-3 bg-primary-100 rounded-full">
          <Icon className="text-primary-600" size={24} />
        </div>
      </div>
    </div>
  );
}

/**
 * ダッシュボードコンポーネント
 *
 * 全体統計、リアルタイムチャート、システム状態を表示
 */
export default function Dashboard() {
  const { data: stats } = useQuery({
    queryKey: ['statistics'],
    queryFn: getStatistics,
    refetchInterval: 5000,
  });

  const { data: health } = useQuery({
    queryKey: ['health'],
    queryFn: getHealth,
    refetchInterval: 10000,
  });

  // モックチャートデータ
  const chartData = [
    { time: '00:00', messages: 12 },
    { time: '04:00', messages: 5 },
    { time: '08:00', messages: 45 },
    { time: '12:00', messages: 78 },
    { time: '16:00', messages: 89 },
    { time: '20:00', messages: 56 },
    { time: '24:00', messages: 34 },
  ];

  return (
    <div>
      <h2 className="text-2xl font-bold mb-6">ダッシュボード</h2>

      {/* 統計カード */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
        <StatCard
          icon={MessageSquare}
          title="総メッセージ数"
          value={stats?.totalMessages ?? 0}
        />
        <StatCard
          icon={Users}
          title="アクティブセッション"
          value={stats?.activeSessions ?? 0}
        />
        <StatCard
          icon={Radio}
          title="接続プラットフォーム"
          value={Object.keys(stats?.platformStats ?? {}).length}
        />
        <StatCard
          icon={Activity}
          title="システム状態"
          value={health?.status ?? 'unknown'}
        />
      </div>

      {/* メッセージ推移チャート */}
      <div className="bg-white rounded-lg shadow p-6 mb-8">
        <h3 className="text-lg font-semibold mb-4">メッセージ推移</h3>
        <ResponsiveContainer width="100%" height={300}>
          <LineChart data={chartData}>
            <CartesianGrid strokeDasharray="3 3" />
            <XAxis dataKey="time" />
            <YAxis />
            <Tooltip />
            <Line
              type="monotone"
              dataKey="messages"
              stroke="#0ea5e9"
              strokeWidth={2}
            />
          </LineChart>
        </ResponsiveContainer>
      </div>

      {/* プラットフォーム別統計 */}
      <div className="bg-white rounded-lg shadow p-6">
        <h3 className="text-lg font-semibold mb-4">プラットフォーム別</h3>
        <div className="space-y-3">
          {Object.entries(stats?.platformStats ?? {}).map(([platform, count]) => (
            <div key={platform} className="flex items-center justify-between">
              <span className="capitalize">{platform}</span>
              <span className="font-semibold">{count} メッセージ</span>
            </div>
          ))}
        </div>
      </div>
    </div>
  );
}

