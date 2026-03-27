import React from "react";

interface DataTableProps {
  /** データ行（各行は {columnName: value} 形式） */
  data: Array<Record<string, unknown>>;
  /** カラム名リスト（省略時はデータから自動検出） */
  columns?: string[];
  /** テーブルタイトル */
  title?: string;
}

/** 値をフォーマットして表示用文字列に変換 */
function formatCellValue(value: unknown): string {
  if (value === null || value === undefined) return "—";
  if (typeof value === "number") {
    // 整数か小数かで分岐
    return Number.isInteger(value)
      ? value.toLocaleString()
      : value.toLocaleString(undefined, { maximumFractionDigits: 2 });
  }
  return String(value);
}

/**
 * データテーブルコンポーネント
 *
 * SQL 結果やテーブルデータをレンダリングする。
 */
export const DataTable: React.FC<DataTableProps> = ({
  data,
  columns,
  title,
}) => {
  if (!data || data.length === 0) return null;

  // カラム名の解決：引数 > データ最初の行のキー
  const resolvedColumns =
    columns && columns.length > 0 ? columns : Object.keys(data[0]);

  return (
    <div className="my-4 overflow-hidden rounded-lg border border-white/10">
      {title && (
        <div className="px-4 py-2 bg-white/5 text-xs font-semibold text-[var(--text-muted)] uppercase tracking-wider border-b border-white/10">
          {title}
        </div>
      )}
      <div className="overflow-x-auto">
        <table className="w-full text-sm">
          <thead>
            <tr className="bg-white/5">
              {resolvedColumns.map((col) => (
                <th
                  key={col}
                  className="px-4 py-2.5 text-left text-xs font-semibold text-[var(--text-muted)] whitespace-nowrap"
                >
                  {col}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {data.map((row, rowIdx) => (
              <tr
                key={rowIdx}
                className="border-t border-white/5 hover:bg-white/5 transition-colors"
              >
                {resolvedColumns.map((col) => (
                  <td
                    key={col}
                    className="px-4 py-2 text-[var(--text-main)] whitespace-nowrap"
                  >
                    {formatCellValue(row[col])}
                  </td>
                ))}
              </tr>
            ))}
          </tbody>
        </table>
      </div>
      <div className="px-4 py-1.5 bg-white/5 text-[10px] text-[var(--text-muted)] border-t border-white/10">
        {data.length} 件
      </div>
    </div>
  );
};
