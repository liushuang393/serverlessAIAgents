/**
 * リソースパーミッション設定ページ
 *
 * リソース単位のアクセス権限（role x resource_type x resource_id）の
 * 一覧表示、作成、削除を行う。
 */

import { useState, useEffect, useCallback } from "react";
import {
  fetchResourcePermissions,
  createResourcePermission,
  deleteResourcePermission,
  fetchRoles,
  type ResourcePermission,
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

const levelBadge = (level: string): React.CSSProperties => {
  const colors: Record<string, { bg: string; fg: string }> = {
    admin: { bg: "#fce8e8", fg: "#c0392b" },
    write: { bg: "#fff3e0", fg: "#e67e22" },
    read: { bg: "#e6f9ee", fg: "#27ae60" },
    none: { bg: "#f0f0f0", fg: "#999" },
  };
  const c = colors[level] ?? { bg: "#eef2ff", fg: "#4a9eff" };
  return {
    display: "inline-block",
    padding: "0.15rem 0.5rem",
    borderRadius: "10px",
    fontSize: "0.75rem",
    fontWeight: 600,
    background: c.bg,
    color: c.fg,
  };
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
  maxWidth: "480px",
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

// --------------------------------------------------------------------------
// 定数
// --------------------------------------------------------------------------

const PERMISSION_LEVELS = ["none", "read", "write", "admin"] as const;

// --------------------------------------------------------------------------
// コンポーネント
// --------------------------------------------------------------------------

export function ResourcesPage() {
  const [items, setItems] = useState<ResourcePermission[]>([]);
  const [roles, setRoles] = useState<RoleInfo[]>([]);
  const [loading, setLoading] = useState(true);
  const [alert, setAlert] = useState<{
    type: "success" | "error";
    msg: string;
  } | null>(null);

  // 作成モーダル
  const [showCreate, setShowCreate] = useState(false);
  const [form, setForm] = useState({
    role_name: "",
    resource_type: "",
    resource_id: "",
    permission_level: "read" as string,
  });

  const loadData = useCallback(async () => {
    setLoading(true);
    try {
      const [rpData, rolesData] = await Promise.all([
        fetchResourcePermissions(),
        fetchRoles(),
      ]);
      setItems(rpData);
      setRoles(rolesData);
    } catch (err) {
      const msg =
        err instanceof ApiError
          ? err.message
          : "リソースパーミッションの取得に失敗しました";
      setAlert({ type: "error", msg });
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    void loadData();
  }, [loadData]);

  useEffect(() => {
    if (!alert) return;
    const t = setTimeout(() => setAlert(null), 4000);
    return () => clearTimeout(t);
  }, [alert]);

  // --- 作成 ---

  function openCreate() {
    setForm({
      role_name: roles.length > 0 ? roles[0].name : "",
      resource_type: "",
      resource_id: "",
      permission_level: "read",
    });
    setShowCreate(true);
  }

  async function handleCreate() {
    if (!form.role_name || !form.resource_type || !form.resource_id) {
      setAlert({ type: "error", msg: "全項目を入力してください" });
      return;
    }
    try {
      await createResourcePermission({
        role_name: form.role_name,
        resource_type: form.resource_type,
        resource_id: form.resource_id,
        permission_level: form.permission_level,
      });
      setShowCreate(false);
      setAlert({
        type: "success",
        msg: "リソースパーミッションを作成しました",
      });
      void loadData();
    } catch (err) {
      const msg = err instanceof ApiError ? err.message : "作成に失敗しました";
      setAlert({ type: "error", msg });
    }
  }

  // --- 削除 ---

  async function handleDelete(item: ResourcePermission) {
    const desc = `${item.role_name} / ${item.resource_type}:${item.resource_id}`;
    if (!window.confirm(`リソースパーミッション「${desc}」を削除しますか？`))
      return;
    try {
      await deleteResourcePermission(item.id);
      setAlert({
        type: "success",
        msg: "リソースパーミッションを削除しました",
      });
      void loadData();
    } catch (err) {
      const msg = err instanceof ApiError ? err.message : "削除に失敗しました";
      setAlert({ type: "error", msg });
    }
  }

  return (
    <div>
      <div style={topBar}>
        <h1 style={{ ...heading, marginBottom: 0 }}>リソースパーミッション</h1>
        <button type="button" style={btnPrimary} onClick={openCreate}>
          新規作成
        </button>
      </div>

      {alert && <div style={alertBox(alert.type)}>{alert.msg}</div>}

      {loading ? (
        <div>読み込み中...</div>
      ) : (
        <table style={table}>
          <thead>
            <tr>
              <th style={th}>ロール</th>
              <th style={th}>リソース種別</th>
              <th style={th}>リソースID</th>
              <th style={th}>レベル</th>
              <th style={th}>条件</th>
              <th style={th}>操作</th>
            </tr>
          </thead>
          <tbody>
            {items.map((item) => (
              <tr key={item.id}>
                <td style={td}>{item.role_name}</td>
                <td style={td}>{item.resource_type}</td>
                <td style={td}>{item.resource_id}</td>
                <td style={td}>
                  <span style={levelBadge(item.permission_level)}>
                    {item.permission_level}
                  </span>
                </td>
                <td style={td}>{item.conditions || "-"}</td>
                <td style={td}>
                  <button
                    type="button"
                    style={btnDanger}
                    onClick={() => void handleDelete(item)}
                  >
                    削除
                  </button>
                </td>
              </tr>
            ))}
            {items.length === 0 && (
              <tr>
                <td style={{ ...td, textAlign: "center" }} colSpan={6}>
                  リソースパーミッションが見つかりません
                </td>
              </tr>
            )}
          </tbody>
        </table>
      )}

      {/* 作成モーダル */}
      {showCreate && (
        <div style={modalOverlay} onClick={() => setShowCreate(false)}>
          <div style={modalCard} onClick={(e) => e.stopPropagation()}>
            <div style={modalTitle}>リソースパーミッション作成</div>

            <label style={formLabel} htmlFor="rp-role">
              ロール
            </label>
            <select
              id="rp-role"
              style={{ ...formInput, cursor: "pointer" }}
              value={form.role_name}
              onChange={(e) =>
                setForm((f) => ({ ...f, role_name: e.target.value }))
              }
            >
              {roles.map((r) => (
                <option key={r.name} value={r.name}>
                  {r.display_name} ({r.name})
                </option>
              ))}
            </select>

            <label style={formLabel} htmlFor="rp-resource-type">
              リソース種別
            </label>
            <input
              id="rp-resource-type"
              style={formInput}
              placeholder="例: faq_collection"
              value={form.resource_type}
              onChange={(e) =>
                setForm((f) => ({ ...f, resource_type: e.target.value }))
              }
            />

            <label style={formLabel} htmlFor="rp-resource-id">
              リソースID
            </label>
            <input
              id="rp-resource-id"
              style={formInput}
              placeholder="例: collection-001"
              value={form.resource_id}
              onChange={(e) =>
                setForm((f) => ({ ...f, resource_id: e.target.value }))
              }
            />

            <label style={formLabel} htmlFor="rp-level">
              パーミッションレベル
            </label>
            <select
              id="rp-level"
              style={{ ...formInput, cursor: "pointer" }}
              value={form.permission_level}
              onChange={(e) =>
                setForm((f) => ({ ...f, permission_level: e.target.value }))
              }
            >
              {PERMISSION_LEVELS.map((lv) => (
                <option key={lv} value={lv}>
                  {lv}
                </option>
              ))}
            </select>

            <div style={formActions}>
              <button
                type="button"
                style={btnSmall}
                onClick={() => setShowCreate(false)}
              >
                キャンセル
              </button>
              <button
                type="button"
                style={btnPrimary}
                onClick={() => void handleCreate()}
              >
                作成
              </button>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}
