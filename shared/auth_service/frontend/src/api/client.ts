/**
 * auth_service API クライアント
 *
 * fetch ベースの HTTP クライアント。
 * JWT トークンを localStorage で管理し、リクエストヘッダに自動付与する。
 */

const TOKEN_KEY = "auth_access_token";
const REFRESH_KEY = "auth_refresh_token";

// --------------------------------------------------------------------------
// トークン管理
// --------------------------------------------------------------------------

export function getAccessToken(): string | null {
  return localStorage.getItem(TOKEN_KEY);
}

export function getRefreshToken(): string | null {
  return localStorage.getItem(REFRESH_KEY);
}

export function setTokens(access: string, refresh: string): void {
  localStorage.setItem(TOKEN_KEY, access);
  localStorage.setItem(REFRESH_KEY, refresh);
}

export function clearTokens(): void {
  localStorage.removeItem(TOKEN_KEY);
  localStorage.removeItem(REFRESH_KEY);
}

// --------------------------------------------------------------------------
// 型定義
// --------------------------------------------------------------------------

export interface UserInfo {
  user_id: string;
  username: string;
  display_name: string;
  department: string;
  position: string;
  role: string;
  roles: string[];
  tenant_id: string | null;
  scopes: string[];
  azp: string | null;
  email: string | null;
  mfa_enabled: boolean;
  permissions: string[];
}

export interface AuthResponse {
  success: boolean;
  message: string;
  user: UserInfo | null;
  access_token: string | null;
  refresh_token: string | null;
  token_type: string;
  expires_in: number | null;
}

export interface RoleInfo {
  name: string;
  display_name: string;
  description: string;
  is_system: boolean;
  priority: number;
  permissions: string[];
}

export interface PermissionInfo {
  name: string;
  display_name: string;
  description: string;
  resource_type: string;
  action: string;
  is_system: boolean;
}

export interface UserAdminInfo {
  user_id: string;
  username: string;
  display_name: string;
  department: string;
  position: string;
  role: string;
  roles: string[];
  email: string | null;
  is_active: boolean;
  auth_source: string;
  mfa_enabled: boolean;
  permissions: string[];
  created_at: string | null;
  last_login_at: string | null;
}

export interface PaginatedUsers {
  users: UserAdminInfo[];
  total: number;
  page: number;
  page_size: number;
}

export interface ResourcePermission {
  id: string;
  role_name: string;
  resource_type: string;
  resource_id: string;
  permission_level: string;
  conditions: string | null;
}

export interface UserPermissions {
  user_id: string;
  roles: string[];
  permissions: string[];
}

// --------------------------------------------------------------------------
// API エラー
// --------------------------------------------------------------------------

export class ApiError extends Error {
  constructor(
    public status: number,
    message: string,
  ) {
    super(message);
    this.name = "ApiError";
  }
}

// --------------------------------------------------------------------------
// 共通 fetch ラッパー
// --------------------------------------------------------------------------

async function request<T>(
  path: string,
  options: RequestInit = {},
): Promise<T> {
  const token = getAccessToken();
  const headers: Record<string, string> = {
    "Content-Type": "application/json",
    ...(options.headers as Record<string, string> | undefined),
  };
  if (token) {
    headers["Authorization"] = `Bearer ${token}`;
  }

  const res = await fetch(path, {
    ...options,
    headers,
  });

  if (res.status === 401) {
    clearTokens();
    window.location.href = "/login";
    throw new ApiError(401, "認証が無効です");
  }

  if (!res.ok) {
    let msg = `HTTP ${res.status}`;
    try {
      const body = await res.json();
      if (body.detail) {
        msg = typeof body.detail === "string" ? body.detail : JSON.stringify(body.detail);
      } else if (body.message) {
        msg = body.message;
      }
    } catch {
      // JSON パース失敗時はデフォルトメッセージを使用
    }
    throw new ApiError(res.status, msg);
  }

  return res.json() as Promise<T>;
}

// --------------------------------------------------------------------------
// 認証 API
// --------------------------------------------------------------------------

export async function login(
  username: string,
  password: string,
): Promise<AuthResponse> {
  const data = await request<AuthResponse>("/auth/login", {
    method: "POST",
    body: JSON.stringify({ username, password }),
  });
  if (data.success && data.access_token && data.refresh_token) {
    setTokens(data.access_token, data.refresh_token);
  }
  return data;
}

export async function fetchMe(): Promise<{
  success: boolean;
  message: string;
  user: UserInfo;
}> {
  return request("/auth/me");
}

export async function logout(): Promise<void> {
  try {
    await request<{ success: boolean }>("/auth/logout", { method: "POST" });
  } finally {
    clearTokens();
  }
}

// --------------------------------------------------------------------------
// ロール API
// --------------------------------------------------------------------------

export async function fetchRoles(): Promise<RoleInfo[]> {
  return request("/auth/authorization/roles");
}

export async function fetchRole(name: string): Promise<RoleInfo> {
  return request(`/auth/authorization/roles/${encodeURIComponent(name)}`);
}

export async function createRole(body: {
  name: string;
  display_name: string;
  description?: string;
  priority?: number;
}): Promise<RoleInfo> {
  return request("/auth/authorization/roles", {
    method: "POST",
    body: JSON.stringify(body),
  });
}

export async function updateRole(
  name: string,
  body: {
    display_name?: string;
    description?: string;
    priority?: number;
  },
): Promise<RoleInfo> {
  return request(`/auth/authorization/roles/${encodeURIComponent(name)}`, {
    method: "PUT",
    body: JSON.stringify(body),
  });
}

export async function deleteRole(name: string): Promise<void> {
  await request(`/auth/authorization/roles/${encodeURIComponent(name)}`, {
    method: "DELETE",
  });
}

// --------------------------------------------------------------------------
// パーミッション API
// --------------------------------------------------------------------------

export async function fetchPermissions(): Promise<PermissionInfo[]> {
  return request("/auth/authorization/permissions");
}

export async function assignPermissionToRole(
  roleName: string,
  permissionName: string,
): Promise<void> {
  await request(
    `/auth/authorization/roles/${encodeURIComponent(roleName)}/permissions`,
    {
      method: "POST",
      body: JSON.stringify({ permission_name: permissionName }),
    },
  );
}

export async function removePermissionFromRole(
  roleName: string,
  permissionName: string,
): Promise<void> {
  await request(
    `/auth/authorization/roles/${encodeURIComponent(roleName)}/permissions/${encodeURIComponent(permissionName)}`,
    { method: "DELETE" },
  );
}

// --------------------------------------------------------------------------
// ユーザーロール API
// --------------------------------------------------------------------------

export async function fetchUserRoles(
  userId: string,
): Promise<{ roles: string[] }> {
  return request(`/auth/authorization/users/${encodeURIComponent(userId)}/roles`);
}

export async function assignRoleToUser(
  userId: string,
  roleName: string,
): Promise<void> {
  await request(
    `/auth/authorization/users/${encodeURIComponent(userId)}/roles`,
    {
      method: "POST",
      body: JSON.stringify({ role_name: roleName }),
    },
  );
}

export async function removeRoleFromUser(
  userId: string,
  roleName: string,
): Promise<void> {
  await request(
    `/auth/authorization/users/${encodeURIComponent(userId)}/roles/${encodeURIComponent(roleName)}`,
    { method: "DELETE" },
  );
}

export async function fetchUserPermissions(
  userId: string,
): Promise<UserPermissions> {
  return request(
    `/auth/authorization/users/${encodeURIComponent(userId)}/permissions`,
  );
}

// --------------------------------------------------------------------------
// 管理 API
// --------------------------------------------------------------------------

export async function fetchUsers(
  page: number = 1,
  pageSize: number = 20,
): Promise<PaginatedUsers> {
  return request(`/auth/admin/users?page=${page}&page_size=${pageSize}`);
}

export async function fetchUser(userId: string): Promise<UserAdminInfo> {
  return request(`/auth/admin/users/${encodeURIComponent(userId)}`);
}

export async function updateUser(
  userId: string,
  body: {
    display_name?: string;
    department?: string;
    position?: string;
    role?: string;
    is_active?: boolean;
  },
): Promise<UserAdminInfo> {
  return request(`/auth/admin/users/${encodeURIComponent(userId)}`, {
    method: "PUT",
    body: JSON.stringify(body),
  });
}

export async function deactivateUser(
  userId: string,
): Promise<{ success: boolean; message: string }> {
  return request(`/auth/admin/users/${encodeURIComponent(userId)}`, {
    method: "DELETE",
  });
}

export async function resetUserPassword(
  userId: string,
): Promise<{ new_password: string }> {
  return request(
    `/auth/admin/users/${encodeURIComponent(userId)}/reset-password`,
    { method: "POST" },
  );
}

// --------------------------------------------------------------------------
// リソースパーミッション API
// --------------------------------------------------------------------------

export async function fetchResourcePermissions(): Promise<
  ResourcePermission[]
> {
  return request("/auth/authorization/resource-permissions");
}

export async function createResourcePermission(body: {
  role_name: string;
  resource_type: string;
  resource_id: string;
  permission_level: string;
  conditions?: string;
}): Promise<ResourcePermission> {
  return request("/auth/authorization/resource-permissions", {
    method: "POST",
    body: JSON.stringify(body),
  });
}

export async function deleteResourcePermission(id: string): Promise<void> {
  await request(
    `/auth/authorization/resource-permissions/${encodeURIComponent(id)}`,
    { method: "DELETE" },
  );
}
