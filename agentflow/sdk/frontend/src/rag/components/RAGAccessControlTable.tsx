/**
 * RAG アクセス制御テーブル.
 *
 * ロール別の KB 権限設定を表示・編集。
 */
import React from 'react';
import type { KBPermission } from '../types/rag';

export interface RAGAccessControlTableProps {
  permissions: KBPermission[];
  roles?: string[];
  onUpdate?: (permissions: KBPermission[]) => void;
  readOnly?: boolean;
}

const PERMISSION_STYLES: Record<string, string> = {
  read: 'text-blue-400 bg-blue-500/10',
  write: 'text-amber-400 bg-amber-500/10',
  admin: 'text-emerald-400 bg-emerald-500/10',
};

/** アクセス制御テーブル */
export const RAGAccessControlTable: React.FC<RAGAccessControlTableProps> = ({
  permissions,
  readOnly = false,
}) => {
  if (permissions.length === 0) {
    return (
      <div className="text-center py-8 text-slate-500">
        権限設定がありません
      </div>
    );
  }

  return (
    <div className="overflow-x-auto">
      <table className="w-full text-sm text-left">
        <thead className="text-xs text-slate-400 uppercase border-b border-slate-700">
          <tr>
            <th className="px-4 py-3">ロール</th>
            <th className="px-4 py-3">アクセス可能コレクション</th>
            <th className="px-4 py-3">権限レベル</th>
          </tr>
        </thead>
        <tbody>
          {permissions.map((perm) => (
            <tr
              key={perm.role}
              className="border-b border-slate-700/50"
            >
              <td className="px-4 py-3 font-medium text-slate-200">
                {perm.role}
              </td>
              <td className="px-4 py-3">
                <div className="flex flex-wrap gap-1">
                  {perm.collections.map((col) => (
                    <span
                      key={col}
                      className="text-xs px-2 py-0.5 rounded-full bg-slate-700/50 text-slate-300"
                    >
                      {col}
                    </span>
                  ))}
                </div>
              </td>
              <td className="px-4 py-3">
                {readOnly ? (
                  <span
                    className={`text-xs px-2 py-0.5 rounded-full ${
                      PERMISSION_STYLES[perm.permission_level] ?? ''
                    }`}
                  >
                    {perm.permission_level}
                  </span>
                ) : (
                  <select
                    value={perm.permission_level}
                    disabled={readOnly}
                    className="px-2 py-1 bg-slate-800 border border-slate-700 rounded text-xs text-slate-200"
                  >
                    <option value="read">read</option>
                    <option value="write">write</option>
                    <option value="admin">admin</option>
                  </select>
                )}
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};
