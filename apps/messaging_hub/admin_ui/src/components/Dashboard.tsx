import { lazy, Suspense, useMemo } from "react";
import { useQuery } from "@tanstack/react-query";
import { MessageSquare, Users, Radio, Activity } from "lucide-react";
import { getStatistics, getHealth } from "../api/client";
import { usePageVisibility } from "../hooks/usePageVisibility";

const PlatformMessageChart = lazy(
  () => import("./dashboard/PlatformMessageChart"),
);

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
    <div className="glass-panel elevated p-6">
      <div className="flex items-center justify-between">
        <div>
          <p className="text-sm text-muted">{title}</p>
          <p className="text-2xl font-bold mt-1">{value}</p>
          {change && <p className="text-sm text-green-600 mt-1">{change}</p>}
        </div>
        <div className="p-3 bg-white/90 rounded-full shadow">
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
  const isVisible = usePageVisibility();
  const { data: stats } = useQuery({
    queryKey: ["statistics"],
    queryFn: getStatistics,
    refetchInterval: isVisible ? 5000 : false,
    refetchIntervalInBackground: false,
  });

  const { data: health } = useQuery({
    queryKey: ["health"],
    queryFn: getHealth,
    refetchInterval: isVisible ? 10000 : false,
    refetchIntervalInBackground: false,
  });

  const chartData = useMemo(
    () =>
      Object.entries(stats?.platformStats ?? {}).map(
        ([platform, messages]) => ({ platform, messages }),
      ),
    [stats?.platformStats],
  );

  return (
    <div>
      <h2 className="text-2xl font-bold mb-6 text-slate-900">ダッシュボード</h2>

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
          value={health?.status ?? "unknown"}
        />
      </div>

      {/* プラットフォーム別メッセージ分布 */}
      <div className="glass-panel p-6 mb-8">
        <h3 className="text-lg font-semibold mb-4">
          プラットフォーム別メッセージ分布
        </h3>
        <Suspense
          fallback={
            <div className="h-[300px] flex items-center justify-center text-sm text-muted">
              チャートを読み込み中...
            </div>
          }
        >
          <PlatformMessageChart data={chartData} />
        </Suspense>
      </div>

      {/* プラットフォーム別統計 */}
      <div className="glass-panel p-6">
        <h3 className="text-lg font-semibold mb-4">プラットフォーム別</h3>
        <div className="space-y-3">
          {Object.entries(stats?.platformStats ?? {}).map(
            ([platform, count]) => (
              <div key={platform} className="flex items-center justify-between">
                <span className="capitalize">{platform}</span>
                <span className="font-semibold">{count} メッセージ</span>
              </div>
            ),
          )}
        </div>
      </div>
    </div>
  );
}
