/**
 * DataTableRenderer コンポーネント
 *
 * @description テーブルデータをレンダリング（ソート・ページネーション付き）
 */

import React, { useState, useMemo } from 'react';
import type { RichComponent } from '../types';

interface DataTableRendererProps {
  /** RichComponent */
  component: RichComponent;
}

/**
 * DataTableRenderer
 *
 * @param props - DataTableRendererProps
 * @returns JSX.Element
 */
/** テーブル列定義 */
interface TableColumn {
  key: string;
  header: string;
  sortable?: boolean;
  width?: number;
}

export function DataTableRenderer({ component }: DataTableRendererProps): React.JSX.Element {
  const rawColumns = component.props.columns;
  const rawData = component.props.data;
  const columns: TableColumn[] = Array.isArray(rawColumns) ? rawColumns : [];
  const data: Record<string, unknown>[] = Array.isArray(rawData) ? rawData : [];
  const pageSize = typeof component.props.pageSize === 'number' ? component.props.pageSize : 10;

  const [sortKey, setSortKey] = useState<string | null>(null);
  const [sortDir, setSortDir] = useState<'asc' | 'desc'>('asc');
  const [page, setPage] = useState(0);

  // ソート処理
  const sortedData = useMemo(() => {
    if (!sortKey) return data;
    return [...data].sort((a, b) => {
      const aVal = a[sortKey];
      const bVal = b[sortKey];
      if (aVal === bVal) return 0;
      // 型安全な比較
      const aStr = String(aVal ?? '');
      const bStr = String(bVal ?? '');
      const result = aStr < bStr ? -1 : 1;
      return sortDir === 'asc' ? result : -result;
    });
  }, [data, sortKey, sortDir]);

  // ページネーション
  const pagedData = useMemo(() => {
    const start = page * pageSize;
    return sortedData.slice(start, start + pageSize);
  }, [sortedData, page, pageSize]);

  const totalPages = Math.ceil(data.length / pageSize);

  const handleSort = (key: string) => {
    if (sortKey === key) {
      setSortDir((d) => (d === 'asc' ? 'desc' : 'asc'));
    } else {
      setSortKey(key);
      setSortDir('asc');
    }
  };

  return (
    <div className="overflow-x-auto rounded-lg border border-gray-200 dark:border-gray-700">
      <table className="min-w-full divide-y divide-gray-200 dark:divide-gray-700">
        <thead className="bg-gray-50 dark:bg-gray-800">
          <tr>
            {columns.map((col) => (
              <th
                key={col.key}
                className="px-4 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider"
                style={col.width ? { width: col.width } : undefined}
              >
                {col.sortable !== false ? (
                  <button
                    onClick={() => handleSort(col.key)}
                    className="flex items-center gap-1 hover:text-gray-700 dark:hover:text-gray-200"
                  >
                    {col.header}
                    {sortKey === col.key && (
                      <span>{sortDir === 'asc' ? '↑' : '↓'}</span>
                    )}
                  </button>
                ) : (
                  col.header
                )}
              </th>
            ))}
          </tr>
        </thead>
        <tbody className="bg-white dark:bg-gray-900 divide-y divide-gray-200 dark:divide-gray-700">
          {pagedData.map((row, rowIdx) => (
            <tr key={rowIdx} className="hover:bg-gray-50 dark:hover:bg-gray-800">
              {columns.map((col) => (
                <td key={col.key} className="px-4 py-3 text-sm text-gray-900 dark:text-gray-100">
                  {String(row[col.key] ?? '')}
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>

      {/* ページネーション */}
      {totalPages > 1 && (
        <div className="flex items-center justify-between px-4 py-3 bg-gray-50 dark:bg-gray-800 border-t border-gray-200 dark:border-gray-700">
          <span className="text-sm text-gray-500 dark:text-gray-400">
            {page * pageSize + 1} - {Math.min((page + 1) * pageSize, data.length)} / {data.length} 件
          </span>
          <div className="flex gap-2">
            <button
              onClick={() => setPage((p) => Math.max(0, p - 1))}
              disabled={page === 0}
              className="px-3 py-1 text-sm rounded border border-gray-300 dark:border-gray-600 disabled:opacity-50"
            >
              前へ
            </button>
            <button
              onClick={() => setPage((p) => Math.min(totalPages - 1, p + 1))}
              disabled={page >= totalPages - 1}
              className="px-3 py-1 text-sm rounded border border-gray-300 dark:border-gray-600 disabled:opacity-50"
            >
              次へ
            </button>
          </div>
        </div>
      )}
    </div>
  );
}

export default DataTableRenderer;

