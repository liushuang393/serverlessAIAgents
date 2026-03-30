/**
 * アクセス制御パネル（読み取り専用）.
 *
 * ロール × KB タイプのアクセスマトリクスを表形式で表示する。
 * 全ユーザー閲覧のみ。権限の変更はロール管理画面（admin 専用）で行う。
 */

import { useEffect, useState } from "react";
import type { JSX } from "react";
import { Shield } from "lucide-react";
import { useI18n } from "../../i18n";
import { ragApi } from "../../api/rag";

/** KB タイプの表示順序 */
const KB_TYPES = ["internal", "external", "confidential"] as const;

/** マトリクスデータ型 */
type AccessMatrix = Record<string, Record<string, boolean>>;

/** ユーザー情報型（localStorage から取得） */
interface StoredUser {
  role?: string;
}

function getCurrentUserRole(): string {
  try {
    const raw = localStorage.getItem("user_info");
    if (raw) {
      const parsed: StoredUser = JSON.parse(raw);
      return parsed.role ?? "guest";
    }
  } catch {
    // パース失敗時はデフォルト
  }
  return "guest";
}

export function PanelAccess(): JSX.Element {
  const { t } = useI18n();
  const [matrix, setMatrix] = useState<AccessMatrix | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const currentRole = getCurrentUserRole();

  useEffect(() => {
    let cancelled = false;
    setLoading(true);
    setError(null);
    ragApi
      .getAccessMatrix()
      .then((resp) => {
        if (!cancelled) {
          setMatrix(resp.matrix);
          setLoading(false);
        }
      })
      .catch((e: Error) => {
        if (!cancelled) {
          setError(e.message);
          setLoading(false);
        }
      });
    return () => {
      cancelled = true;
    };
  }, []);

  if (loading) {
    return (
      <div
        data-testid="panel-access-loading"
        className="text-sm text-[var(--text-muted)] p-4"
      >
        {t("knowledge_panel.loading")}
      </div>
    );
  }

  if (error && !matrix) {
    return (
      <div
        data-testid="panel-access-error"
        className="text-sm text-rose-400 p-4"
      >
        {t("knowledge_panel.error_prefix")}: {error}
      </div>
    );
  }

  if (!matrix) {
    return (
      <div className="text-sm text-[var(--text-muted)] p-4">
        {t("knowledge_panel.no_data")}
      </div>
    );
  }

  const roles = Object.keys(matrix);

  return (
    <div data-testid="panel-access" className="space-y-4">
      {/* 情報テキスト */}
      <div className="flex items-start gap-3 p-3 rounded-xl bg-indigo-500/5 border border-indigo-500/10">
        <Shield size={16} className="text-indigo-400 flex-shrink-0 mt-0.5" />
        <div>
          <p className="text-xs text-white">
            {t("knowledge_panel.current_role")}:{" "}
            <span
              data-testid="current-role"
              className="font-medium text-[var(--primary)]"
            >
              {currentRole}
            </span>
          </p>
          <p className="text-xs text-[var(--text-muted)] mt-1">
            {t("knowledge_panel.access_description")}
          </p>
        </div>
      </div>

      {/* マトリクス表（読み取り専用） */}
      <div className="rounded-xl glass border border-white/5 overflow-hidden">
        <table data-testid="access-matrix-table" className="w-full text-sm">
          <thead>
            <tr className="border-b border-white/5">
              <th className="text-left px-4 py-3 text-xs text-[var(--text-muted)] uppercase font-medium">
                {t("knowledge_panel.role")}
              </th>
              {KB_TYPES.map((kb) => (
                <th
                  key={kb}
                  className="text-center px-4 py-3 text-xs text-[var(--text-muted)] uppercase font-medium"
                >
                  {kb}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {roles.map((role) => (
              <tr
                key={role}
                className="border-b border-white/5 last:border-b-0"
              >
                <td className="px-4 py-3 font-medium text-white">{role}</td>
                {KB_TYPES.map((kb) => {
                  const allowed = matrix[role]?.[kb] ?? false;
                  return (
                    <td key={kb} className="text-center px-4 py-3">
                      {allowed ? (
                        <span
                          className="text-emerald-400"
                          aria-label={`${role} ${kb} allowed`}
                        >
                          &#10003;
                        </span>
                      ) : (
                        <span
                          className="text-[var(--text-muted)]"
                          aria-label={`${role} ${kb} denied`}
                        >
                          &#10005;
                        </span>
                      )}
                    </td>
                  );
                })}
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
}
