/**
 * アクセス制御パネル.
 *
 * ロール x KB タイプのアクセスマトリクスを表形式で表示する。
 * admin ロールの場合のみ「編集」ボタンを表示（将来拡張用）。
 */

import { useCallback, useEffect, useState } from 'react';
import type { JSX } from 'react';
import { Shield, Pencil, Save, X } from 'lucide-react';
import { useI18n } from '../../i18n';
import { ragApi } from '../../api/rag';

/** KB タイプの表示順序 */
const KB_TYPES = ['internal', 'external', 'confidential'] as const;

/** マトリクスデータ型 */
type AccessMatrix = Record<string, Record<string, boolean>>;

/** ユーザー情報型（localStorage から取得） */
interface StoredUser {
  role?: string;
}

function getCurrentUserRole(): string {
  try {
    const raw = localStorage.getItem('user_info');
    if (raw) {
      const parsed: StoredUser = JSON.parse(raw);
      return parsed.role ?? 'guest';
    }
  } catch {
    // パース失敗時はデフォルト
  }
  return 'guest';
}

/** マトリクスのディープコピーを生成 */
function cloneMatrix(m: AccessMatrix): AccessMatrix {
  const copy: AccessMatrix = {};
  for (const [role, perms] of Object.entries(m)) {
    copy[role] = { ...perms };
  }
  return copy;
}

export function PanelAccess(): JSX.Element {
  const { t } = useI18n();
  const [matrix, setMatrix] = useState<AccessMatrix | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [editing, setEditing] = useState(false);
  const [draft, setDraft] = useState<AccessMatrix | null>(null);
  const [saving, setSaving] = useState(false);
  const currentRole = getCurrentUserRole();
  const isAdmin = currentRole === 'admin';

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

  /** 編集モード開始 */
  const startEditing = useCallback(() => {
    if (matrix) {
      setDraft(cloneMatrix(matrix));
      setEditing(true);
    }
  }, [matrix]);

  /** 編集モードキャンセル */
  const cancelEditing = useCallback(() => {
    setDraft(null);
    setEditing(false);
  }, []);

  /** チェックボックス切替 */
  const togglePermission = useCallback((role: string, kb: string) => {
    setDraft((prev) => {
      if (!prev) return prev;
      const next = cloneMatrix(prev);
      next[role] = next[role] ?? {};
      next[role][kb] = !next[role][kb];
      return next;
    });
  }, []);

  /** 保存: KB タイプごとに許可ロール一覧を送信 */
  const handleSave = useCallback(async () => {
    if (!draft) return;
    setSaving(true);
    setError(null);
    try {
      let latestMatrix: AccessMatrix | null = null;
      for (const kb of KB_TYPES) {
        const allowedRoles = Object.entries(draft)
          .filter(([, perms]) => perms[kb])
          .map(([role]) => role);
        const resp = await ragApi.updateAccessRoles(kb, allowedRoles);
        latestMatrix = resp.matrix;
      }
      if (latestMatrix) {
        setMatrix(latestMatrix);
      }
      setEditing(false);
      setDraft(null);
    } catch (e: unknown) {
      const message = e instanceof Error ? e.message : String(e);
      setError(message);
    } finally {
      setSaving(false);
    }
  }, [draft]);

  if (loading) {
    return (
      <div data-testid="panel-access-loading" className="text-sm text-[var(--text-muted)] p-4">
        {t('knowledge_panel.loading')}
      </div>
    );
  }

  if (error && !matrix) {
    return (
      <div data-testid="panel-access-error" className="text-sm text-rose-400 p-4">
        {t('knowledge_panel.error_prefix')}: {error}
      </div>
    );
  }

  if (!matrix) {
    return (
      <div className="text-sm text-[var(--text-muted)] p-4">{t('knowledge_panel.no_data')}</div>
    );
  }

  const displayMatrix = editing && draft ? draft : matrix;
  const roles = Object.keys(displayMatrix);

  return (
    <div data-testid="panel-access" className="space-y-4">
      {/* 情報テキスト */}
      <div className="flex items-start gap-3 p-3 rounded-xl bg-indigo-500/5 border border-indigo-500/10">
        <Shield size={16} className="text-indigo-400 flex-shrink-0 mt-0.5" />
        <div>
          <p className="text-xs text-white">
            {t('knowledge_panel.current_role')}: <span data-testid="current-role" className="font-medium text-[var(--primary)]">{currentRole}</span>
          </p>
          <p className="text-xs text-[var(--text-muted)] mt-1">
            {t('knowledge_panel.access_description')}
          </p>
        </div>
      </div>

      {/* 保存エラー */}
      {error && (
        <div className="p-3 rounded-xl bg-rose-500/10 border border-rose-500/20 text-xs text-rose-400">
          {t('knowledge_panel.error_prefix')}: {error}
        </div>
      )}

      {/* マトリクス表 */}
      <div className="rounded-xl glass border border-white/5 overflow-hidden">
        <table data-testid="access-matrix-table" className="w-full text-sm">
          <thead>
            <tr className="border-b border-white/5">
              <th className="text-left px-4 py-3 text-xs text-[var(--text-muted)] uppercase font-medium">
                {t('knowledge_panel.role')}
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
              <tr key={role} className="border-b border-white/5 last:border-b-0">
                <td className="px-4 py-3 font-medium text-white">{role}</td>
                {KB_TYPES.map((kb) => {
                  const allowed = displayMatrix[role]?.[kb] ?? false;
                  return (
                    <td key={kb} className="text-center px-4 py-3">
                      {editing ? (
                        <input
                          type="checkbox"
                          checked={allowed}
                          onChange={() => togglePermission(role, kb)}
                          className="rounded border-white/20 cursor-pointer"
                          aria-label={`${role} ${kb} ${allowed ? 'allowed' : 'denied'}`}
                        />
                      ) : allowed ? (
                        <span className="text-emerald-400" aria-label={`${role} ${kb} allowed`}>
                          &#10003;
                        </span>
                      ) : (
                        <span className="text-[var(--text-muted)]" aria-label={`${role} ${kb} denied`}>
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

      {/* admin 用アクションボタン */}
      {isAdmin && (
        <div className="flex justify-end gap-2">
          {editing ? (
            <>
              <button
                data-testid="btn-cancel-access"
                className="flex items-center gap-1.5 px-3 py-1.5 rounded-lg bg-white/5 border border-white/10 text-xs text-[var(--text-muted)] hover:text-white hover:bg-white/10 transition-all"
                onClick={cancelEditing}
                disabled={saving}
              >
                <X size={12} />
                {t('knowledge_panel.cancel')}
              </button>
              <button
                data-testid="btn-save-access"
                className="flex items-center gap-1.5 px-3 py-1.5 rounded-lg bg-[var(--primary)]/10 text-[var(--primary)] text-xs font-medium border border-[var(--primary)]/20 hover:bg-[var(--primary)]/20 transition-all disabled:opacity-50"
                onClick={() => void handleSave()}
                disabled={saving}
              >
                <Save size={12} />
                {saving ? t('knowledge_panel.saving') : t('knowledge_panel.save')}
              </button>
            </>
          ) : (
            <button
              data-testid="btn-edit-access"
              className="flex items-center gap-1.5 px-3 py-1.5 rounded-lg bg-white/5 border border-white/10 text-xs text-[var(--text-muted)] hover:text-white hover:bg-white/10 transition-all"
              onClick={startEditing}
            >
              <Pencil size={12} />
              {t('knowledge_panel.edit')}
            </button>
          )}
        </div>
      )}
    </div>
  );
}
