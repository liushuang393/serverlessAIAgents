import { useQuery } from '@tanstack/react-query';
import { CheckCircle, XCircle, RefreshCw } from 'lucide-react';
import { getPlatforms } from '../api/client';
import { formatDistanceToNow } from 'date-fns';
import { ja } from 'date-fns/locale';

/**
 * プラットフォーム一覧
 *
 * 各メッセージングプラットフォームの接続状態を表示
 */
export default function Platforms() {
  const { data: platforms, isLoading, refetch } = useQuery({
    queryKey: ['platforms'],
    queryFn: getPlatforms,
    refetchInterval: 10000,
  });

  // プラットフォームアイコン
  const platformIcons: Record<string, string> = {
    telegram: '📱',
    slack: '💼',
    discord: '🎮',
    teams: '🏢',
    whatsapp: '💬',
    signal: '🔒',
  };

  return (
    <div>
      <div className="flex items-center justify-between mb-6">
        <h2 className="text-2xl font-bold">プラットフォーム</h2>
        <button
          onClick={() => refetch()}
          className="flex items-center gap-2 px-4 py-2 bg-primary-500 text-white rounded-lg hover:bg-primary-600 transition-colors"
        >
          <RefreshCw size={16} />
          更新
        </button>
      </div>

      {isLoading ? (
        <div className="text-center py-12">読み込み中...</div>
      ) : (
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
          {platforms?.map((platform) => (
            <div
              key={platform.name}
              className="bg-white rounded-lg shadow p-6"
            >
              <div className="flex items-center justify-between mb-4">
                <div className="flex items-center gap-3">
                  <span className="text-3xl">
                    {platformIcons[platform.name] || '📡'}
                  </span>
                  <h3 className="text-lg font-semibold capitalize">
                    {platform.name}
                  </h3>
                </div>
                {platform.connected ? (
                  <CheckCircle className="text-green-500" size={24} />
                ) : (
                  <XCircle className="text-red-500" size={24} />
                )}
              </div>

              <div className="space-y-2 text-sm">
                <div className="flex justify-between">
                  <span className="text-gray-500">状態</span>
                  <span
                    className={
                      platform.connected
                        ? 'text-green-600'
                        : 'text-red-600'
                    }
                  >
                    {platform.connected ? '接続中' : '未接続'}
                  </span>
                </div>
                <div className="flex justify-between">
                  <span className="text-gray-500">メッセージ数</span>
                  <span>{platform.messageCount.toLocaleString()}</span>
                </div>
                {platform.lastActivity && (
                  <div className="flex justify-between">
                    <span className="text-gray-500">最終アクティビティ</span>
                    <span>
                      {formatDistanceToNow(new Date(platform.lastActivity), {
                        addSuffix: true,
                        locale: ja,
                      })}
                    </span>
                  </div>
                )}
              </div>
            </div>
          ))}
        </div>
      )}
    </div>
  );
}

