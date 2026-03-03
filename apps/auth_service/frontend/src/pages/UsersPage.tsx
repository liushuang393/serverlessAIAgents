/**
 * ユーザー一覧・管理ページ
 *
 * ページネーション付きユーザー一覧の表示、
 * ユーザー詳細の編集、無効化、パスワードリセット、
 * ロール割り当て機能を提供する。
 */

import { useState, useEffect, useCallback } from "react";
import {
  fetchUsers,
  updateUser,
  deactivateUser,
  resetUserPassword,
  fetchUserRoles,
  assignRoleToUser,
  removeRoleFromUser,
  fetchRoles,
  type UserAdminInfo,
  type RoleInfo,
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

const badge = (active: boolean): React.CSSProperties => ({
  display: "inline-block",
  padding: "0.15rem 0.5rem",
  borderRadius: "10px",
  fontSize: "0.75rem",
  fontWeight: 600,
  background: active ? "#e6f9ee" : "#fce8e8",
  color: active ? "#27ae60" : "#c0392b",
});

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

const paginationBar: React.CSSProperties = {
  display: "flex",
  alignItems: "center",
  justifyContent: "center",
  gap: "1rem",
  marginTop: "1rem",
  fontSize: "0.9rem",
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
  maxWidth: "500px",
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

const roleTag: React.CSSProperties = {
  display: "inline-flex",
  alignItems: "center",
  gap: "0.3rem",
  padding: "0.2rem 0.5rem",
  background: "#eef2ff",
  borderRadius: "4px",
  fontSize: "0.8rem",
  marginRight: "0.3rem",
  marginBottom: "0.2rem",
};

const roleRemoveBtn: React.CSSProperties = {
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

export function UsersPage() {
  const [users, setUsers] = useState<UserAdminInfo[]>([]);
  const [total, setTotal] = useState(0);
  const [page, setPage] = useState(1);
  const [pageSize] = useState(20);
  const [loading, setLoading] = useState(true);
  const [alert, setAlert] = useState<{
    type: "success" | "error";
    msg: string;
  } | null>(null);

  // 編集モーダル
  const [editing, setEditing] = useState<UserAdminInfo | null>(null);
  const [editForm, setEditForm] = useState({
    display_name: "",
    department: "",
    position: "",
  });

  // ロール管理モーダル
  const [roleTarget, setRoleTarget] = useState<UserAdminInfo | null>(null);
  const [userRoles, setUserRoles] = useState<string[]>([]);
  const [allRoles, setAllRoles] = useState<RoleInfo[]>([]);
  const [selectedRole, setSelectedRole] = useState("");

  const loadUsers = useCallback(async () => {
    setLoading(true);
    try {
      const data = await fetchUsers(page, pageSize);
      setUsers(data.users);
      setTotal(data.total);
    } catch (err) {
      const msg =
        err instanceof ApiError ? err.message : "ユーザー一覧の取得に失敗しました";
      setAlert({ type: "error", msg });
    } finally {
      setLoading(false);
    }
  }, [page, pageSize]);

  useEffect(() => {
    void loadUsers();
  }, [loadUsers]);

  // 自動クリア
  useEffect(() => {
    if (!alert) return;
    const t = setTimeout(() => setAlert(null), 4000);
    return () => clearTimeout(t);
  }, [alert]);

  const totalPages = Math.max(1, Math.ceil(total / pageSize));

  // --- 編集 ---

  function openEdit(u: UserAdminInfo) {
    setEditForm({
      display_name: u.display_name,
      department: u.department,
      position: u.position,
    });
    setEditing(u);
  }

  async function handleSaveEdit() {
    if (!editing) return;
    try {
      await updateUser(editing.user_id, editForm);
      setEditing(null);
      setAlert({ type: "success", msg: "ユーザー情報を更新しました" });
      void loadUsers();
    } catch (err) {
      const msg =
        err instanceof ApiError ? err.message : "更新に失敗しました";
      setAlert({ type: "error", msg });
    }
  }

  // --- 無効化 ---

  async function handleDeactivate(u: UserAdminInfo) {
    if (!window.confirm(`${u.display_name} を無効化しますか？`)) return;
    try {
      await deactivateUser(u.user_id);
      setAlert({ type: "success", msg: "ユーザーを無効化しました" });
      void loadUsers();
    } catch (err) {
      const msg =
        err instanceof ApiError ? err.message : "無効化に失敗しました";
      setAlert({ type: "error", msg });
    }
  }

  // --- パスワードリセット ---

  async function handleResetPassword(u: UserAdminInfo) {
    if (!window.confirm(`${u.display_name} のパスワードをリセットしますか？`))
      return;
    try {
      const res = await resetUserPassword(u.user_id);
      setAlert({
        type: "success",
        msg: `新しいパスワード: ${res.new_password}`,
      });
    } catch (err) {
      const msg =
        err instanceof ApiError
          ? err.message
          : "パスワードリセットに失敗しました";
      setAlert({ type: "error", msg });
    }
  }

  // --- ロール管理 ---

  async function openRoleModal(u: UserAdminInfo) {
    setRoleTarget(u);
    try {
      const [rolesRes, allRes] = await Promise.all([
        fetchUserRoles(u.user_id),
        fetchRoles(),
      ]);
      setUserRoles(rolesRes.roles);
      setAllRoles(allRes);
      setSelectedRole("");
    } catch (err) {
      const msg =
        err instanceof ApiError ? err.message : "ロール情報の取得に失敗しました";
      setAlert({ type: "error", msg });
      setRoleTarget(null);
    }
  }

  async function handleAssignRole() {
    if (!roleTarget || !selectedRole) return;
    try {
      await assignRoleToUser(roleTarget.user_id, selectedRole);
      setUserRoles((prev) => [...prev, selectedRole]);
      setSelectedRole("");
      setAlert({ type: "success", msg: "ロールを割り当てました" });
    } catch (err) {
      const msg =
        err instanceof ApiError ? err.message : "ロール割り当てに失敗しました";
      setAlert({ type: "error", msg });
    }
  }

  async function handleRemoveRole(roleName: string) {
    if (!roleTarget) return;
    try {
      await removeRoleFromUser(roleTarget.user_id, roleName);
      setUserRoles((prev) => prev.filter((r) => r !== roleName));
      setAlert({ type: "success", msg: "ロールを解除しました" });
    } catch (err) {
      const msg =
        err instanceof ApiError ? err.message : "ロール解除に失敗しました";
      setAlert({ type: "error", msg });
    }
  }

  // --- 表示 ---

  return (
    <div>
      <h1 style={heading}>ユーザー管理</h1>

      {alert && <div style={alertBox(alert.type)}>{alert.msg}</div>}

      {loading ? (
        <div>読み込み中...</div>
      ) : (
        <>
          <table style={table}>
            <thead>
              <tr>
                <th style={th}>ユーザー名</th>
                <th style={th}>表示名</th>
                <th style={th}>部署</th>
                <th style={th}>ロール</th>
                <th style={th}>状態</th>
                <th style={th}>操作</th>
              </tr>
            </thead>
            <tbody>
              {users.map((u) => (
                <tr key={u.user_id}>
                  <td style={td}>{u.username}</td>
                  <td style={td}>{u.display_name}</td>
                  <td style={td}>{u.department || "-"}</td>
                  <td style={td}>
                    {u.roles.length > 0 ? u.roles.join(", ") : u.role}
                  </td>
                  <td style={td}>
                    <span style={badge(u.is_active)}>
                      {u.is_active ? "有効" : "無効"}
                    </span>
                  </td>
                  <td style={td}>
                    <button
                      type="button"
                      style={btnSmall}
                      onClick={() => openEdit(u)}
                    >
                      編集
                    </button>
                    <button
                      type="button"
                      style={btnSmall}
                      onClick={() => void openRoleModal(u)}
                    >
                      ロール
                    </button>
                    <button
                      type="button"
                      style={btnSmall}
                      onClick={() => void handleResetPassword(u)}
                    >
                      PW リセット
                    </button>
                    {u.is_active && (
                      <button
                        type="button"
                        style={btnDanger}
                        onClick={() => void handleDeactivate(u)}
                      >
                        無効化
                      </button>
                    )}
                  </td>
                </tr>
              ))}
              {users.length === 0 && (
                <tr>
                  <td style={{ ...td, textAlign: "center" }} colSpan={6}>
                    ユーザーが見つかりません
                  </td>
                </tr>
              )}
            </tbody>
          </table>

          <div style={paginationBar}>
            <button
              type="button"
              style={btnSmall}
              disabled={page <= 1}
              onClick={() => setPage((p) => p - 1)}
            >
              前へ
            </button>
            <span>
              {page} / {totalPages}（全 {total} 件）
            </span>
            <button
              type="button"
              style={btnSmall}
              disabled={page >= totalPages}
              onClick={() => setPage((p) => p + 1)}
            >
              次へ
            </button>
          </div>
        </>
      )}

      {/* 編集モーダル */}
      {editing && (
        <div style={modalOverlay} onClick={() => setEditing(null)}>
          <div style={modalCard} onClick={(e) => e.stopPropagation()}>
            <div style={modalTitle}>ユーザー編集: {editing.username}</div>

            <label style={formLabel} htmlFor="edit-display-name">
              表示名
            </label>
            <input
              id="edit-display-name"
              style={formInput}
              value={editForm.display_name}
              onChange={(e) =>
                setEditForm((f) => ({ ...f, display_name: e.target.value }))
              }
            />

            <label style={formLabel} htmlFor="edit-department">
              部署
            </label>
            <input
              id="edit-department"
              style={formInput}
              value={editForm.department}
              onChange={(e) =>
                setEditForm((f) => ({ ...f, department: e.target.value }))
              }
            />

            <label style={formLabel} htmlFor="edit-position">
              役職
            </label>
            <input
              id="edit-position"
              style={formInput}
              value={editForm.position}
              onChange={(e) =>
                setEditForm((f) => ({ ...f, position: e.target.value }))
              }
            />

            <div style={formActions}>
              <button
                type="button"
                style={btnSmall}
                onClick={() => setEditing(null)}
              >
                キャンセル
              </button>
              <button
                type="button"
                style={btnPrimary}
                onClick={() => void handleSaveEdit()}
              >
                保存
              </button>
            </div>
          </div>
        </div>
      )}

      {/* ロール管理モーダル */}
      {roleTarget && (
        <div style={modalOverlay} onClick={() => setRoleTarget(null)}>
          <div style={modalCard} onClick={(e) => e.stopPropagation()}>
            <div style={modalTitle}>
              ロール管理: {roleTarget.display_name}
            </div>

            <div style={{ marginBottom: "1rem" }}>
              <div
                style={{ fontWeight: 600, fontSize: "0.85rem", marginBottom: "0.4rem" }}
              >
                現在のロール
              </div>
              {userRoles.length === 0 ? (
                <span style={{ color: "#999", fontSize: "0.85rem" }}>
                  ロール未割り当て
                </span>
              ) : (
                <div style={{ display: "flex", flexWrap: "wrap" }}>
                  {userRoles.map((r) => (
                    <span key={r} style={roleTag}>
                      {r}
                      <button
                        type="button"
                        style={roleRemoveBtn}
                        title="解除"
                        onClick={() => void handleRemoveRole(r)}
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
                <label style={formLabel} htmlFor="assign-role">
                  ロールを追加
                </label>
                <select
                  id="assign-role"
                  style={{ ...formInput, cursor: "pointer" }}
                  value={selectedRole}
                  onChange={(e) => setSelectedRole(e.target.value)}
                >
                  <option value="">-- 選択 --</option>
                  {allRoles
                    .filter((r) => !userRoles.includes(r.name))
                    .map((r) => (
                      <option key={r.name} value={r.name}>
                        {r.display_name} ({r.name})
                      </option>
                    ))}
                </select>
              </div>
              <button
                type="button"
                style={btnPrimary}
                disabled={!selectedRole}
                onClick={() => void handleAssignRole()}
              >
                追加
              </button>
            </div>

            <div style={formActions}>
              <button
                type="button"
                style={btnSmall}
                onClick={() => {
                  setRoleTarget(null);
                  void loadUsers();
                }}
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
