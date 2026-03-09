import { useQuery } from "@tanstack/react-query";
import { RefreshCw } from "lucide-react";
import { getSessions } from "../api/client";
import { formatDistanceToNow } from "date-fns";
import { ja } from "date-fns/locale";
import { usePageVisibility } from "../hooks/usePageVisibility";

/**
 * セッション一覧
 *
 * アクティブな会話セッションを表示
 */
export default function Sessions() {
  const isVisible = usePageVisibility();
  const {
    data: sessions,
    isLoading,
    isError,
    error,
    isFetching,
    refetch,
  } = useQuery({
    queryKey: ["sessions"],
    queryFn: getSessions,
    refetchInterval: isVisible ? 10000 : false,
    refetchIntervalInBackground: false,
    retry: 1,
  });

  return (
    <div>
      <div className="flex items-center justify-between mb-6">
        <h2 className="text-2xl font-bold">セッション</h2>
        <button
          onClick={() => refetch()}
          className="flex items-center gap-2 px-4 py-2 bg-primary-500 text-white rounded-lg hover:bg-primary-600 transition-colors"
        >
          <RefreshCw size={16} className={isFetching ? "animate-spin" : ""} />
          更新
        </button>
      </div>

      {isLoading ? (
        <div className="text-center py-12">読み込み中...</div>
      ) : isError ? (
        <div className="glass-panel p-6 text-center">
          <p className="text-sm text-rose-700">
            セッション情報の取得に失敗しました。
          </p>
          <p className="text-xs text-rose-600 mt-1">
            {error instanceof Error ? error.message : "不明なエラー"}
          </p>
        </div>
      ) : sessions?.length === 0 ? (
        <div className="text-center py-12 text-gray-500">
          アクティブなセッションはありません
        </div>
      ) : (
        <div className="glass-panel overflow-hidden">
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
