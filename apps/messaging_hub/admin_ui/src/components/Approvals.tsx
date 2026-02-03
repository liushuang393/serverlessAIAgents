import { useEffect, useState } from 'react';
import {
  ShieldCheck,
  ShieldAlert,
  Clock,
  CheckCircle,
  XCircle,
  AlertTriangle,
  Loader2,
  RefreshCw,
} from 'lucide-react';
import clsx from 'clsx';

interface ApprovalRequest {
  id: string;
  skill_name: string;
  risk_level: 'low' | 'medium' | 'high' | 'critical';
  params: Record<string, unknown>;
  user_id: string;
  status: 'pending' | 'approved' | 'rejected' | 'expired' | 'auto_approved';
  created_at: string;
  expires_at: string | null;
  decided_at: string | null;
  decided_by: string | null;
  rejection_reason: string | null;
}

interface ApprovalStats {
  pending: number;
  approved: number;
  rejected: number;
  expired: number;
  auto_approved: number;
  total_processed: number;
}

const riskConfig = {
  low: { color: 'text-green-600', bg: 'bg-green-100', label: '低' },
  medium: { color: 'text-yellow-600', bg: 'bg-yellow-100', label: '中' },
  high: { color: 'text-orange-600', bg: 'bg-orange-100', label: '高' },
  critical: { color: 'text-red-600', bg: 'bg-red-100', label: '危険' },
};

const statusConfig = {
  pending: { icon: Clock, color: 'text-blue-500', bg: 'bg-blue-100', label: '保留中' },
  approved: { icon: CheckCircle, color: 'text-green-500', bg: 'bg-green-100', label: '承認済み' },
  rejected: { icon: XCircle, color: 'text-red-500', bg: 'bg-red-100', label: '拒否' },
  expired: { icon: Clock, color: 'text-gray-500', bg: 'bg-gray-100', label: '期限切れ' },
  auto_approved: { icon: ShieldCheck, color: 'text-green-500', bg: 'bg-green-100', label: '自動承認' },
};

/**
 * 承認管理ページ
 *
 * 高リスク操作の承認/拒否を管理
 */
export default function Approvals() {
  const [pendingRequests, setPendingRequests] = useState<ApprovalRequest[]>([]);
  const [history, setHistory] = useState<ApprovalRequest[]>([]);
  const [stats, setStats] = useState<ApprovalStats | null>(null);
  const [loading, setLoading] = useState(true);
  const [activeTab, setActiveTab] = useState<'pending' | 'history'>('pending');
  const [rejectReason, setRejectReason] = useState('');
  const [selectedRequest, setSelectedRequest] = useState<string | null>(null);

  useEffect(() => {
    fetchData();
    const interval = setInterval(fetchData, 5000);
    return () => clearInterval(interval);
  }, []);

  const fetchData = async () => {
    try {
      const [pendingRes, historyRes, statsRes] = await Promise.all([
        fetch('/api/approvals/pending'),
        fetch('/api/approvals/history?limit=50'),
        fetch('/api/approvals/stats'),
      ]);

      if (pendingRes.ok) {
        const data = await pendingRes.json();
        setPendingRequests(data.requests || []);
      }
      if (historyRes.ok) {
        const data = await historyRes.json();
        setHistory(data.requests || []);
      }
      if (statsRes.ok) {
        const data = await statsRes.json();
        setStats(data);
      }
    } catch (error) {
      console.error('Approvals fetch error:', error);
    } finally {
      setLoading(false);
    }
  };

  const handleApprove = async (id: string) => {
    try {
      const response = await fetch(`/api/approvals/${id}/approve`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ approver_id: 'admin' }),
      });
      if (response.ok) {
        fetchData();
      }
    } catch (error) {
      console.error('Approve error:', error);
    }
  };

  const handleReject = async (id: string) => {
    try {
      const response = await fetch(`/api/approvals/${id}/reject`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          rejecter_id: 'admin',
          reason: rejectReason || '管理者により拒否',
        }),
      });
      if (response.ok) {
        setSelectedRequest(null);
        setRejectReason('');
        fetchData();
      }
    } catch (error) {
      console.error('Reject error:', error);
    }
  };

  const formatTime = (isoString: string) => {
    const date = new Date(isoString);
    return date.toLocaleString('ja-JP', {
      month: 'numeric',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
    });
  };

  const getTimeRemaining = (expiresAt: string | null) => {
    if (!expiresAt) return null;
    const remaining = new Date(expiresAt).getTime() - Date.now();
    if (remaining <= 0) return '期限切れ';
    const minutes = Math.floor(remaining / 60000);
    if (minutes < 60) return `${minutes}分`;
    return `${Math.floor(minutes / 60)}時間${minutes % 60}分`;
  };

  const renderRequestCard = (request: ApprovalRequest, isPending: boolean) => {
    const risk = riskConfig[request.risk_level];
    const status = statusConfig[request.status];
    const StatusIcon = status.icon;

    return (
      <div
        key={request.id}
        className={clsx(
          'bg-white rounded-lg shadow p-4 border-l-4',
          isPending ? 'border-blue-500' : 'border-gray-300'
        )}
      >
        <div className="flex items-start justify-between">
          <div className="flex-1">
            <div className="flex items-center gap-3">
              <span className="font-medium text-lg">{request.skill_name}</span>
              <span className={clsx('text-xs px-2 py-0.5 rounded-full', risk.bg, risk.color)}>
                リスク: {risk.label}
              </span>
              {!isPending && (
                <span className={clsx('text-xs px-2 py-0.5 rounded-full flex items-center gap-1', status.bg, status.color)}>
                  <StatusIcon size={12} />
                  {status.label}
                </span>
              )}
            </div>

            <p className="text-sm text-gray-500 mt-1">
              要求者: {request.user_id} | {formatTime(request.created_at)}
            </p>

            {isPending && request.expires_at && (
              <p className="text-sm text-orange-600 mt-1 flex items-center gap-1">
                <AlertTriangle size={14} />
                有効期限: {getTimeRemaining(request.expires_at)}
              </p>
            )}

            {request.decided_at && (
              <p className="text-sm text-gray-500 mt-1">
                決定: {formatTime(request.decided_at)} by {request.decided_by}
              </p>
            )}

            {request.rejection_reason && (
              <p className="text-sm text-red-600 mt-1">
                理由: {request.rejection_reason}
              </p>
            )}

            <details className="mt-2">
              <summary className="text-sm text-gray-500 cursor-pointer hover:text-gray-700">
                パラメータを表示
              </summary>
              <pre className="mt-1 p-2 bg-gray-50 rounded text-xs overflow-auto max-h-32">
                {JSON.stringify(request.params, null, 2)}
              </pre>
            </details>
          </div>

          {isPending && (
            <div className="flex flex-col gap-2 ml-4">
              <button
                onClick={() => handleApprove(request.id)}
                className="flex items-center gap-1 px-4 py-2 bg-green-500 text-white rounded-lg hover:bg-green-600"
              >
                <CheckCircle size={16} />
                承認
              </button>
              <button
                onClick={() => setSelectedRequest(request.id)}
                className="flex items-center gap-1 px-4 py-2 bg-red-500 text-white rounded-lg hover:bg-red-600"
              >
                <XCircle size={16} />
                拒否
              </button>
            </div>
          )}
        </div>

        {/* 拒否理由入力モーダル */}
        {selectedRequest === request.id && (
          <div className="mt-4 p-4 bg-gray-50 rounded-lg">
            <label className="block text-sm font-medium text-gray-700 mb-2">
              拒否理由（任意）
            </label>
            <textarea
              value={rejectReason}
              onChange={(e) => setRejectReason(e.target.value)}
              className="w-full border rounded-lg px-3 py-2 text-sm"
              rows={2}
              placeholder="拒否の理由を入力..."
            />
            <div className="flex justify-end gap-2 mt-2">
              <button
                onClick={() => { setSelectedRequest(null); setRejectReason(''); }}
                className="px-3 py-1 text-gray-600 hover:bg-gray-200 rounded"
              >
                キャンセル
              </button>
              <button
                onClick={() => handleReject(request.id)}
                className="px-3 py-1 bg-red-500 text-white rounded hover:bg-red-600"
              >
                拒否を確定
              </button>
            </div>
          </div>
        )}
      </div>
    );
  };

  return (
    <div className="space-y-6">
      {/* ヘッダー */}
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-2xl font-bold text-gray-900">承認管理</h1>
          <p className="text-gray-600 mt-1">高リスク操作の承認・拒否を管理</p>
        </div>
        <button
          onClick={fetchData}
          className="flex items-center gap-2 px-4 py-2 bg-white border rounded-lg hover:bg-gray-50"
        >
          <RefreshCw size={16} />
          更新
        </button>
      </div>

      {/* 統計カード */}
      {stats && (
        <div className="grid grid-cols-5 gap-4">
          <div className="bg-blue-50 rounded-lg p-4 border border-blue-200">
            <p className="text-sm text-blue-600">保留中</p>
            <p className="text-2xl font-bold text-blue-700">{stats.pending}</p>
          </div>
          <div className="bg-green-50 rounded-lg p-4 border border-green-200">
            <p className="text-sm text-green-600">承認済み</p>
            <p className="text-2xl font-bold text-green-700">{stats.approved}</p>
          </div>
          <div className="bg-red-50 rounded-lg p-4 border border-red-200">
            <p className="text-sm text-red-600">拒否</p>
            <p className="text-2xl font-bold text-red-700">{stats.rejected}</p>
          </div>
          <div className="bg-gray-50 rounded-lg p-4 border border-gray-200">
            <p className="text-sm text-gray-600">期限切れ</p>
            <p className="text-2xl font-bold text-gray-700">{stats.expired}</p>
          </div>
          <div className="bg-purple-50 rounded-lg p-4 border border-purple-200">
            <p className="text-sm text-purple-600">自動承認</p>
            <p className="text-2xl font-bold text-purple-700">{stats.auto_approved}</p>
          </div>
        </div>
      )}

      {/* タブ */}
      <div className="border-b border-gray-200">
        <div className="flex gap-4">
          <button
            onClick={() => setActiveTab('pending')}
            className={clsx(
              'px-4 py-2 font-medium border-b-2 transition-colors',
              activeTab === 'pending'
                ? 'border-primary-500 text-primary-600'
                : 'border-transparent text-gray-500 hover:text-gray-700'
            )}
          >
            <div className="flex items-center gap-2">
              <ShieldAlert size={18} />
              保留中
              {pendingRequests.length > 0 && (
                <span className="bg-red-500 text-white text-xs rounded-full px-2">
                  {pendingRequests.length}
                </span>
              )}
            </div>
          </button>
          <button
            onClick={() => setActiveTab('history')}
            className={clsx(
              'px-4 py-2 font-medium border-b-2 transition-colors',
              activeTab === 'history'
                ? 'border-primary-500 text-primary-600'
                : 'border-transparent text-gray-500 hover:text-gray-700'
            )}
          >
            <div className="flex items-center gap-2">
              <Clock size={18} />
              履歴
            </div>
          </button>
        </div>
      </div>

      {/* コンテンツ */}
      {loading ? (
        <div className="flex items-center justify-center py-12">
          <Loader2 className="animate-spin text-primary-500" size={32} />
        </div>
      ) : activeTab === 'pending' ? (
        <div className="space-y-4">
          {pendingRequests.length === 0 ? (
            <div className="text-center py-12 text-gray-500 bg-white rounded-lg shadow">
              <ShieldCheck size={48} className="mx-auto mb-4 text-green-300" />
              <p>保留中の承認リクエストはありません</p>
            </div>
          ) : (
            pendingRequests.map((request) => renderRequestCard(request, true))
          )}
        </div>
      ) : (
        <div className="space-y-4">
          {history.length === 0 ? (
            <div className="text-center py-12 text-gray-500 bg-white rounded-lg shadow">
              <Clock size={48} className="mx-auto mb-4 text-gray-300" />
              <p>履歴がありません</p>
            </div>
          ) : (
            history.map((request) => renderRequestCard(request, false))
          )}
        </div>
      )}
    </div>
  );
}
