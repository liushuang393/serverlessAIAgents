/**
 * KB 権限管理コンポーネント（admin 専用・auth_service 連携）.
 *
 * ロール定義は auth_service 側で管理される。
 * このコンポーネントでは auth_service のロールに対して
 * KB（internal / external / confidential）の read / write 権限を設定する。
 */

import { useCallback, useEffect, useState } from "react";
import type { JSX } from "react";
import { Shield, Save, X, UserCog, ExternalLink, Info } from "lucide-react";
import { useI18n } from "../../i18n";
import { ragApi, type RoleInfo } from "../../api/rag";

/** KB タイプ × アクション定義 */
const KB_TYPES = ["internal", "external", "confidential"] as const;
const ACTIONS = ["read", "write"] as const;

export function RoleManager(): JSX.Element {
  const { t } = useI18n();
  const [roles, setRoles] = useState<RoleInfo[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [saving, setSaving] = useState(false);
  const [editingRole, setEditingRole] = useState<string | null>(null);
  const [editPerms, setEditPerms] = useState<Record<string, boolean>>({});

  /** ロール一覧を取得 */
  const fetchRoles = useCallback(async () => {
    setLoading(true);
    setError(null);
    try {
      const resp = await ragApi.listRoles();
      setRoles(resp.roles);
    } catch (e: unknown) {
      setError(e instanceof Error ? e.message : String(e));
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    void fetchRoles();
  }, [fetchRoles]);

  /** 権限キーを生成 */
  const permKey = (kb: string, action: string): string => `${kb}:${action}`;

  /** 編集開始 */
  const startEdit = useCallback((role: RoleInfo) => {
    const perms: Record<string, boolean> = {};
    for (const kb of KB_TYPES) {
      for (const action of ACTIONS) {
        const key = permKey(kb, action);
        perms[key] = role.kb_permissions.some(
          (p) => p.kb_type === kb && p.action === action,
        );
      }
    }
    setEditPerms(perms);
    setEditingRole(role.role_name);
  }, []);

  /** 編集保存 */
  const handleSavePerms = useCallback(async () => {
    if (!editingRole) return;
    setSaving(true);
    setError(null);
    try {
      const kbPermissions: Array<{ kb_type: string; action: string }> = [];
      for (const [key, enabled] of Object.entries(editPerms)) {
        if (enabled) {
          const [kb_type, action] = key.split(":");
          kbPermissions.push({ kb_type, action });
        }
      }
      await ragApi.updateKBPermissions(editingRole, { kb_permissions: kbPermissions });
      setEditingRole(null);
      await fetchRoles();
    } catch (e: unknown) {
      setError(e instanceof Error ? e.message : String(e));
    } finally {
      setSaving(false);
    }
  }, [editingRole, editPerms, fetchRoles]);

  /** 権限トグル */
  const togglePerm = useCallback((key: string) => {
    setEditPerms((prev) => ({ ...prev, [key]: !prev[key] }));
  }, []);

  if (loading) {
    return (
      <div className="text-sm text-[var(--text-muted)] p-4">
        {t("common.loading")}
      </div>
    );
  }

  return (
    <div data-testid="role-manager" className="space-y-4">
      {/* ヘッダー */}
      <div className="flex items-center gap-2">
        <UserCog size={16} className="text-[var(--primary)]" />
        <h3 className="text-sm font-bold text-white">
          {t("role_manager.title")}
        </h3>
      </div>

      {/* auth_service 案内 */}
      <div className="p-3 rounded-xl bg-blue-500/10 border border-blue-500/20 flex items-start gap-2">
        <Info size={14} className="text-blue-400 mt-0.5 shrink-0" />
        <div className="text-xs text-blue-300">
          <p>{t("role_manager.auth_service_note")}</p>
          <a
            href="/auth/admin"
            target="_blank"
            rel="noopener noreferrer"
            className="inline-flex items-center gap-1 mt-1 text-blue-400 hover:text-blue-300 underline"
          >
            {t("role_manager.go_to_auth_service")}
            <ExternalLink size={10} />
          </a>
        </div>
      </div>

      {/* エラー表示 */}
      {error && (
        <div className="p-3 rounded-xl bg-rose-500/10 border border-rose-500/20 text-xs text-rose-400">
          {error}
        </div>
      )}

      {/* ロール一覧（KB 権限のみ編集可能） */}
      <div className="space-y-2">
        {roles.map((role) => {
          const isEditing = editingRole === role.role_name;
          return (
            <div key={role.role_name} className="rounded-xl glass border border-white/5 overflow-hidden">
              {/* ロールヘッダー行 */}
              <div className="flex items-center justify-between p-3">
                <div className="flex items-center gap-2">
                  <Shield size={14} className={role.is_system ? "text-amber-400" : "text-indigo-400"} />
                  <span className="text-sm font-medium text-white">{role.role_name}</span>
                  {role.display_name && role.display_name !== role.role_name && (
                    <span className="text-[10px] text-[var(--text-muted)]">({role.display_name})</span>
                  )}
                  {role.is_system && (
                    <span className="text-[10px] bg-amber-500/10 text-amber-400 px-1.5 py-0.5 rounded border border-amber-500/20">
                      {t("role_manager.system_role")}
                    </span>
                  )}
                </div>
                <div className="flex items-center gap-1">
                  {isEditing ? (
                    <>
                      <button
                        onClick={() => setEditingRole(null)}
                        className="p-1.5 rounded-lg hover:bg-white/5 text-[var(--text-muted)] hover:text-white"
                        disabled={saving}
                      >
                        <X size={14} />
                      </button>
                      <button
                        onClick={() => void handleSavePerms()}
                        className="p-1.5 rounded-lg hover:bg-[var(--primary)]/10 text-[var(--primary)]"
                        disabled={saving}
                      >
                        <Save size={14} />
                      </button>
                    </>
                  ) : (
                    <button
                      onClick={() => startEdit(role)}
                      className="px-2 py-1 rounded-lg text-[10px] bg-white/5 text-[var(--text-muted)] hover:text-white hover:bg-white/10 border border-white/10"
                    >
                      {t("role_manager.edit_perms")}
                    </button>
                  )}
                </div>
              </div>

              {/* 権限編集マトリクス（編集中のみ表示） */}
              {isEditing && (
                <div className="border-t border-white/5 p-3">
                  <table className="w-full text-xs">
                    <thead>
                      <tr>
                        <th className="text-left py-1 text-[var(--text-muted)] uppercase">KB</th>
                        {ACTIONS.map((action) => (
                          <th key={action} className="text-center py-1 text-[var(--text-muted)] uppercase">{action}</th>
                        ))}
                      </tr>
                    </thead>
                    <tbody>
                      {KB_TYPES.map((kb) => (
                        <tr key={kb} className="border-t border-white/5">
                          <td className="py-2 text-white font-medium">{kb}</td>
                          {ACTIONS.map((action) => {
                            const key = permKey(kb, action);
                            return (
                              <td key={action} className="text-center py-2">
                                <input
                                  type="checkbox"
                                  checked={editPerms[key] ?? false}
                                  onChange={() => togglePerm(key)}
                                  className="rounded border-white/20 cursor-pointer"
                                />
                              </td>
                            );
                          })}
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>
              )}

              {/* 権限サマリ（非編集時） */}
              {!isEditing && role.kb_permissions.length > 0 && (
                <div className="border-t border-white/5 px-3 py-2 flex flex-wrap gap-1">
                  {role.kb_permissions.map((p) => (
                    <span
                      key={`${p.kb_type}:${p.action}`}
                      className="text-[10px] bg-white/5 px-2 py-0.5 rounded text-[var(--text-muted)]"
                    >
                      {p.kb_type}:{p.action}
                    </span>
                  ))}
                </div>
              )}
            </div>
          );
        })}
      </div>
    </div>
  );
}