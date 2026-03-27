/**
 * ロール管理ページ
 *
 * ロールの一覧表示、作成、編集、削除と
 * パーミッションの割り当て・解除を行う。
 */

import { useState, useEffect, useCallback } from "react";
import {
  fetchRoles,
  createRole,
  updateRole,
  deleteRole,
  fetchPermissions,
  assignPermissionToRole,
  removePermissionFromRole,
  type RoleInfo,
  type PermissionInfo,
  ApiError,
} from "../api/client";

// --------------------------------------------------------------------------
// スタイル
// --------------------------------------------------------------------------

const heading: React.CSSProperties = {
  fontSize: "1.5rem",
  fontWeight: 700,
  marginBottom: "1.2rem",
};

const topBar: React.CSSProperties = {
  display: "flex",
  justifyContent: "space-between",
  alignItems: "center",
  marginBottom: "1rem",
};

const table: React.CSSProperties = {
  width: "100%",
  borderCollapse: "collapse" as const,
  background: "#fff",
  borderRadius: "6px",
  overflow: "hidden",
  boxShadow: "0 1px 4px rgba(0,0,0,0.06)",
};

const th: React.CSSProperties = {
  padding: "0.7rem 0.8rem",
  textAlign: "left" as const,
  background: "#f7f8fa",
  fontWeight: 600,
  fontSize: "0.85rem",
  color: "#555",
  borderBottom: "1px solid #e8e8e8",
};

const td: React.CSSProperties = {
  padding: "0.6rem 0.8rem",
  borderBottom: "1px solid #f0f0f0",
  fontSize: "0.9rem",
};

const btnSmall: React.CSSProperties = {
  padding: "0.3rem 0.7rem",
  fontSize: "0.8rem",
  border: "1px solid #d0d0d0",
  borderRadius: "4px",
  background: "#fff",
  cursor: "pointer",
  marginRight: "0.3rem",
};

const btnDanger: React.CSSProperties = {
  ...btnSmall,
  borderColor: "#e74c3c",
  color: "#e74c3c",
};

const btnPrimary: React.CSSProperties = {
  ...btnSmall,
  background: "#1a1a2e",
  color: "#fff",
  borderColor: "#1a1a2e",
};

const systemBadge: React.CSSProperties = {
  display: "inline-block",
  padding: "0.15rem 0.5rem",
  borderRadius: "10px",
  fontSize: "0.75rem",
  fontWeight: 600,
  background: "#eef2ff",
  color: "#4a9eff",
};

const modalOverlay: React.CSSProperties = {
  position: "fixed",
  top: 0,
  left: 0,
  right: 0,
  bottom: 0,
  background: "rgba(0,0,0,0.4)",
  display: "flex",
  alignItems: "center",
  justifyContent: "center",
  zIndex: 1000,
};

const modalCard: React.CSSProperties = {
  background: "#fff",
  borderRadius: "8px",
  padding: "1.5rem",
  width: "100%",
  maxWidth: "520px",
  maxHeight: "80vh",
  overflow: "auto",
  boxShadow: "0 4px 24px rgba(0,0,0,0.15)",
};

const modalTitle: React.CSSProperties = {
  fontSize: "1.2rem",
  fontWeight: 700,
  marginBottom: "1rem",
};

const formLabel: React.CSSProperties = {
  display: "block",
  fontSize: "0.85rem",
  fontWeight: 600,
  color: "#333",
  marginBottom: "0.2rem",
  marginTop: "0.8rem",
};

const formInput: React.CSSProperties = {
  width: "100%",
  padding: "0.5rem 0.7rem",
  border: "1px solid #d0d0d0",
  borderRadius: "4px",
  fontSize: "0.9rem",
};

const formActions: React.CSSProperties = {
  display: "flex",
  justifyContent: "flex-end",
  gap: "0.5rem",
  marginTop: "1.2rem",
};

const alertBox = (type: "success" | "error"): React.CSSProperties => ({
  padding: "0.6rem 0.8rem",
  borderRadius: "4px",
  fontSize: "0.85rem",
  marginBottom: "1rem",
  background: type === "success" ? "#e6f9ee" : "#ffeaea",
  color: type === "success" ? "#27ae60" : "#c0392b",
});

const permTag: React.CSSProperties = {
  display: "inline-flex",
  alignItems: "center",
  gap: "0.3rem",
  padding: "0.2rem 0.5rem",
  background: "#f0f2f5",
  borderRadius: "4px",
  fontSize: "0.8rem",
  marginRight: "0.3rem",
  marginBottom: "0.2rem",
};

const permRemoveBtn: React.CSSProperties = {
  background: "none",
  border: "none",
  color: "#999",
  cursor: "pointer",
  fontSize: "0.9rem",
  padding: "0 0.1rem",
  lineHeight: 1,
};

// --------------------------------------------------------------------------
// コンポーネント
// --------------------------------------------------------------------------

export function RolesPage() {
  const [roles, setRoles] = useState<RoleInfo[]>([]);
  const [allPerms, setAllPerms] = useState<PermissionInfo[]>([]);
  const [loading, setLoading] = useState(true);
  const [alert, setAlert] = useState<{
    type: "success" | "error";
    msg: string;
  } | null>(null);

  // 作成・編集モーダル
  const [modalMode, setModalMode] = useState<"create" | "edit" | null>(null);
  const [editTarget, setEditTarget] = useState<RoleInfo | null>(null);
  const [form, setForm] = useState({
    name: "",
    display_name: "",
    description: "",
    priority: 0,
  });

  // パーミッション管理モーダル
  const [permTarget, setPermTarget] = useState<RoleInfo | null>(null);
  const [selectedPerm, setSelectedPerm] = useState("");

  const loadRoles = useCallback(async () => {
    setLoading(true);
    try {
      const [rolesData, permsData] = await Promise.all([
        fetchRoles(),
        fetchPermissions(),
      ]);
      setRoles(rolesData);
      setAllPerms(permsData);
    } catch (err) {
      const msg =
        err instanceof ApiError
          ? err.message
          : "ロール情報の取得に失敗しました";
      setAlert({ type: "error", msg });
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    void loadRoles();
  }, [loadRoles]);

  useEffect(() => {
    if (!alert) return;
    const t = setTimeout(() => setAlert(null), 4000);
    return () => clearTimeout(t);
  }, [alert]);

  // --- 作成 ---

  function openCreate() {
    setForm({ name: "", display_name: "", description: "", priority: 0 });
    setModalMode("create");
    setEditTarget(null);
  }

  // --- 編集 ---

  function openEdit(r: RoleInfo) {
    setForm({
      name: r.name,
      display_name: r.display_name,
      description: r.description,
      priority: r.priority,
    });
    setEditTarget(r);
    setModalMode("edit");
  }

  async function handleSave() {
    try {
      if (modalMode === "create") {
        await createRole({
          name: form.name,
          display_name: form.display_name,
          description: form.description,
          priority: form.priority,
        });
        setAlert({ type: "success", msg: "ロールを作成しました" });
      } else if (modalMode === "edit" && editTarget) {
        await updateRole(editTarget.name, {
          display_name: form.display_name,
          description: form.description,
          priority: form.priority,
        });
        setAlert({ type: "success", msg: "ロールを更新しました" });
      }
      setModalMode(null);
      void loadRoles();
    } catch (err) {
      const msg = err instanceof ApiError ? err.message : "保存に失敗しました";
      setAlert({ type: "error", msg });
    }
  }

  // --- 削除 ---

  async function handleDelete(r: RoleInfo) {
    if (!window.confirm(`ロール「${r.display_name}」を削除しますか？`)) return;
    try {
      await deleteRole(r.name);
      setAlert({ type: "success", msg: "ロールを削除しました" });
      void loadRoles();
    } catch (err) {
      const msg = err instanceof ApiError ? err.message : "削除に失敗しました";
      setAlert({ type: "error", msg });
    }
  }

  // --- パーミッション管理 ---

  function openPermModal(r: RoleInfo) {
    setPermTarget(r);
    setSelectedPerm("");
  }

  async function handleAssignPerm() {
    if (!permTarget || !selectedPerm) return;
    try {
      await assignPermissionToRole(permTarget.name, selectedPerm);
      // ロール情報を再取得
      const updated = await fetchRoles();
      setRoles(updated);
      const refreshed = updated.find((r) => r.name === permTarget.name);
      if (refreshed) setPermTarget(refreshed);
      setSelectedPerm("");
      setAlert({ type: "success", msg: "パーミッションを割り当てました" });
    } catch (err) {
      const msg =
        err instanceof ApiError
          ? err.message
          : "パーミッション割り当てに失敗しました";
      setAlert({ type: "error", msg });
    }
  }

  async function handleRemovePerm(permName: string) {
    if (!permTarget) return;
    try {
      await removePermissionFromRole(permTarget.name, permName);
      const updated = await fetchRoles();
      setRoles(updated);
      const refreshed = updated.find((r) => r.name === permTarget.name);
      if (refreshed) setPermTarget(refreshed);
      setAlert({ type: "success", msg: "パーミッションを解除しました" });
    } catch (err) {
      const msg =
        err instanceof ApiError
          ? err.message
          : "パーミッション解除に失敗しました";
      setAlert({ type: "error", msg });
    }
  }

  return (
    <div>
      <div style={topBar}>
        <h1 style={{ ...heading, marginBottom: 0 }}>ロール管理</h1>
        <button type="button" style={btnPrimary} onClick={openCreate}>
          新規ロール作成
        </button>
      </div>

      {alert && <div style={alertBox(alert.type)}>{alert.msg}</div>}

      {loading ? (
        <div>読み込み中...</div>
      ) : (
        <table style={table}>
          <thead>
            <tr>
              <th style={th}>ロール名</th>
              <th style={th}>表示名</th>
              <th style={th}>説明</th>
              <th style={th}>優先度</th>
              <th style={th}>種別</th>
              <th style={th}>パーミッション数</th>
              <th style={th}>操作</th>
            </tr>
          </thead>
          <tbody>
            {roles.map((r) => (
              <tr key={r.name}>
                <td style={td}>{r.name}</td>
                <td style={td}>{r.display_name}</td>
                <td style={td}>{r.description || "-"}</td>
                <td style={td}>{r.priority}</td>
                <td style={td}>
                  {r.is_system && <span style={systemBadge}>システム</span>}
                </td>
                <td style={td}>{r.permissions.length}</td>
                <td style={td}>
                  <button
                    type="button"
                    style={btnSmall}
                    onClick={() => openEdit(r)}
                  >
                    編集
                  </button>
                  <button
                    type="button"
                    style={btnSmall}
                    onClick={() => openPermModal(r)}
                  >
                    権限
                  </button>
                  {!r.is_system && (
                    <button
                      type="button"
                      style={btnDanger}
                      onClick={() => void handleDelete(r)}
                    >
                      削除
                    </button>
                  )}
                </td>
              </tr>
            ))}
            {roles.length === 0 && (
              <tr>
                <td style={{ ...td, textAlign: "center" }} colSpan={7}>
                  ロールが見つかりません
                </td>
              </tr>
            )}
          </tbody>
        </table>
      )}

      {/* 作成・編集モーダル */}
      {modalMode && (
        <div style={modalOverlay} onClick={() => setModalMode(null)}>
          <div style={modalCard} onClick={(e) => e.stopPropagation()}>
            <div style={modalTitle}>
              {modalMode === "create" ? "新規ロール作成" : "ロール編集"}
            </div>

            {modalMode === "create" && (
              <>
                <label style={formLabel} htmlFor="role-name">
                  ロール名（英数字）
                </label>
                <input
                  id="role-name"
                  style={formInput}
                  value={form.name}
                  onChange={(e) =>
                    setForm((f) => ({ ...f, name: e.target.value }))
                  }
                />
              </>
            )}

            <label style={formLabel} htmlFor="role-display-name">
              表示名
            </label>
            <input
              id="role-display-name"
              style={formInput}
              value={form.display_name}
              onChange={(e) =>
                setForm((f) => ({ ...f, display_name: e.target.value }))
              }
            />

            <label style={formLabel} htmlFor="role-description">
              説明
            </label>
            <input
              id="role-description"
              style={formInput}
              value={form.description}
              onChange={(e) =>
                setForm((f) => ({ ...f, description: e.target.value }))
              }
            />

            <label style={formLabel} htmlFor="role-priority">
              優先度
            </label>
            <input
              id="role-priority"
              style={formInput}
              type="number"
              min={0}
              max={999}
              value={form.priority}
              onChange={(e) =>
                setForm((f) => ({
                  ...f,
                  priority: parseInt(e.target.value, 10) || 0,
                }))
              }
            />

            <div style={formActions}>
              <button
                type="button"
                style={btnSmall}
                onClick={() => setModalMode(null)}
              >
                キャンセル
              </button>
              <button
                type="button"
                style={btnPrimary}
                onClick={() => void handleSave()}
              >
                保存
              </button>
            </div>
          </div>
        </div>
      )}

      {/* パーミッション管理モーダル */}
      {permTarget && (
        <div style={modalOverlay} onClick={() => setPermTarget(null)}>
          <div style={modalCard} onClick={(e) => e.stopPropagation()}>
            <div style={modalTitle}>
              パーミッション管理: {permTarget.display_name}
            </div>

            <div style={{ marginBottom: "1rem" }}>
              <div
                style={{
                  fontWeight: 600,
                  fontSize: "0.85rem",
                  marginBottom: "0.4rem",
                }}
              >
                現在のパーミッション
              </div>
              {permTarget.permissions.length === 0 ? (
                <span style={{ color: "#999", fontSize: "0.85rem" }}>
                  パーミッション未割り当て
                </span>
              ) : (
                <div style={{ display: "flex", flexWrap: "wrap" }}>
                  {permTarget.permissions.map((p) => (
                    <span key={p} style={permTag}>
                      {p}
                      <button
                        type="button"
                        style={permRemoveBtn}
                        title="解除"
                        onClick={() => void handleRemovePerm(p)}
                      >
                        x
                      </button>
                    </span>
                  ))}
                </div>
              )}
            </div>

            <div
              style={{
                display: "flex",
                gap: "0.5rem",
                alignItems: "flex-end",
              }}
            >
              <div style={{ flex: 1 }}>
                <label style={formLabel} htmlFor="assign-perm">
                  パーミッションを追加
                </label>
                <select
                  id="assign-perm"
                  style={{ ...formInput, cursor: "pointer" }}
                  value={selectedPerm}
                  onChange={(e) => setSelectedPerm(e.target.value)}
                >
                  <option value="">-- 選択 --</option>
                  {allPerms
                    .filter((p) => !permTarget.permissions.includes(p.name))
                    .map((p) => (
                      <option key={p.name} value={p.name}>
                        {p.display_name} ({p.name})
                      </option>
                    ))}
                </select>
              </div>
              <button
                type="button"
                style={btnPrimary}
                disabled={!selectedPerm}
                onClick={() => void handleAssignPerm()}
              >
                追加
              </button>
            </div>

            <div style={formActions}>
              <button
                type="button"
                style={btnSmall}
                onClick={() => setPermTarget(null)}
              >
                閉じる
              </button>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}
