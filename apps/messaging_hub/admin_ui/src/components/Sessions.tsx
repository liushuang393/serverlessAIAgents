import { useQuery } from '@tanstack/react-query';
import { getSessions } from '../api/client';
import { formatDistanceToNow } from 'date-fns';
import { ja } from 'date-fns/locale';

/**
 * セッション一覧
 *
 * アクティブな会話セッションを表示
 */
export default function Sessions() {
  const { data: sessions, isLoading } = useQuery({
    queryKey: ['sessions'],
    queryFn: getSessions,
    refetchInterval: 5000,
  });

  return (
    <div>
      <h2 className="text-2xl font-bold mb-6">セッション</h2>

      {isLoading ? (
        <div className="text-center py-12">読み込み中...</div>
      ) : sessions?.length === 0 ? (
        <div className="text-center py-12 text-gray-500">
          アクティブなセッションはありません
        </div>
      ) : (
        <div className="bg-white rounded-lg shadow overflow-hidden">
          <table className="w-full">
            <thead className="bg-gray-50">
              <tr>
                <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                  ユーザー
                </th>
                <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                  プラットフォーム
                </th>
                <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                  メッセージ数
                </th>
                <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                  開始時刻
                </th>
                <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                  最終メッセージ
                </th>
              </tr>
            </thead>
            <tbody className="divide-y divide-gray-200">
              {sessions?.map((session) => (
                <tr key={session.id} className="hover:bg-gray-50">
                  <td className="px-6 py-4 whitespace-nowrap">
                    <div>
                      <div className="font-medium">{session.userName}</div>
                      <div className="text-sm text-gray-500">
                        {session.userId}
                      </div>
                    </div>
                  </td>
                  <td className="px-6 py-4 whitespace-nowrap">
                    <span className="px-2 py-1 text-xs font-medium bg-primary-100 text-primary-700 rounded capitalize">
                      {session.platform}
                    </span>
                  </td>
                  <td className="px-6 py-4 whitespace-nowrap">
                    {session.messageCount}
                  </td>
                  <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                    {formatDistanceToNow(new Date(session.startedAt), {
                      addSuffix: true,
                      locale: ja,
                    })}
                  </td>
                  <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                    {formatDistanceToNow(new Date(session.lastMessageAt), {
                      addSuffix: true,
                      locale: ja,
                    })}
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}
    </div>
  );
}

