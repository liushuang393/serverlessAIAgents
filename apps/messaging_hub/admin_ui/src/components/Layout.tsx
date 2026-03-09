import { useEffect, useMemo, useState } from "react";
import { NavLink, Outlet } from "react-router-dom";
import {
  AlertTriangle,
  Clock3,
  FolderOpen,
  LayoutDashboard,
  MessageSquare,
  PanelLeft,
  PanelLeftClose,
  Radio,
  RefreshCw,
  Settings,
  ShieldCheck,
  Users,
  Wrench,
  X,
} from "lucide-react";
import type { LucideIcon } from "lucide-react";
import clsx from "clsx";
import InstallAppButton from "./InstallAppButton";
import {
  API_ERROR_EVENT,
  API_KEY_STORAGE_KEY,
  getMessagingHubApiKey,
  type ApiErrorEventDetail,
} from "../shared/auth";

interface HealthStatus {
  status: string;
  auth_required?: boolean;
  auth_key_configured?: boolean;
  auth_env_var?: string;
}

interface ApiNotice {
  status: number;
  url: string;
  message: string;
  detectedAt: number;
}

type NavItem = {
  to: string;
  icon: LucideIcon;
  label: string;
  preload?: () => Promise<unknown>;
};

/** サイドバーナビグループ定義。グループラベルとアイテム一覧を持つ。 */
type NavGroup = {
  groupLabel: string;
  items: NavItem[];
};

/**
 * ナビグループ定義（使用頻度順）。
 * - チャット：最もよく使う機能
 * - 日常業務：ダッシュボード・履歴・承認・セッション管理
 * - 管理・設定：プラットフォーム・スキル・ファイル・設定
 */
const navGroups: ReadonlyArray<NavGroup> = [
  {
    groupLabel: "チャット",
    items: [
      {
        to: "/conversations",
        icon: MessageSquare,
        label: "チャット",
        preload: () => import("./Conversations"),
      },
    ],
  },
  {
    groupLabel: "日常業務",
    items: [
      {
        to: "/",
        icon: LayoutDashboard,
        label: "ダッシュボード",
        preload: () => import("./Dashboard"),
      },
      {
        to: "/timeline",
        icon: Clock3,
        label: "実行履歴",
        preload: () => import("./Timeline"),
      },
      {
        to: "/approvals",
        icon: ShieldCheck,
        label: "承認管理",
        preload: () => import("./Approvals"),
      },
      {
        to: "/sessions",
        icon: Users,
        label: "セッション",
        preload: () => import("./Sessions"),
      },
    ],
  },
  {
    groupLabel: "管理・設定",
    items: [
      {
        to: "/platforms",
        icon: Radio,
        label: "プラットフォーム",
        preload: () => import("./Platforms"),
      },
      {
        to: "/skills",
        icon: Wrench,
        label: "スキル",
        preload: () => import("./SkillsManager"),
      },
      {
        to: "/file-organizer",
        icon: FolderOpen,
        label: "ファイル整理",
        preload: () => import("./FileOrganizer"),
      },
      {
        to: "/settings",
        icon: Settings,
        label: "設定",
        preload: () => import("./Settings"),
      },
    ],
  },
];

/** 折り畳み時・モバイル用フラットなアイテム一覧 */
const allNavItems: ReadonlyArray<NavItem> = navGroups.flatMap((g) => g.items);

function warmupRoute(item: NavItem) {
  if (!item.preload) {
    return;
  }
  void item.preload();
}

function HealthAndAuthBanner() {
  const [health, setHealth] = useState<HealthStatus | null>(null);
  const [notice, setNotice] = useState<ApiNotice | null>(null);
  const [apiKeyInput, setApiKeyInput] = useState(getMessagingHubApiKey());

  useEffect(() => {
    const fetchHealth = async () => {
      try {
        const response = await fetch("/api/health");
        if (!response.ok) {
          return;
        }
        const payload = (await response.json()) as HealthStatus;
        setHealth(payload);
      } catch {
        // ignore
      }
    };

    void fetchHealth();
    const timer = window.setInterval(() => void fetchHealth(), 15000);
    return () => window.clearInterval(timer);
  }, []);

  // .env にキーが設定されている場合、バックエンドから自動取得して localStorage に保存する
  useEffect(() => {
    if (!health?.auth_required || !health.auth_key_configured) {
      return;
    }
    if (getMessagingHubApiKey()) {
      // 既に localStorage にキーがある場合はスキップ
      return;
    }
    const bootstrap = async () => {
      try {
        const res = await fetch("/api/admin-key");
        if (!res.ok) {
          return;
        }
        const data = (await res.json()) as { api_key: string | null };
        if (data.api_key) {
          window.localStorage.setItem(API_KEY_STORAGE_KEY, data.api_key);
          setApiKeyInput(data.api_key);
          // キーを設定後、ページを再読み込みして全 API 呼び出しにキーを適用する
          window.location.reload();
        }
      } catch {
        // 取得失敗時は手動入力にフォールバック
      }
    };
    void bootstrap();
  }, [health]);

  useEffect(() => {
    const handler = (event: Event) => {
      const customEvent = event as CustomEvent<ApiErrorEventDetail>;
      if (!customEvent.detail) {
        return;
      }
      setNotice({
        ...customEvent.detail,
        detectedAt: Date.now(),
      });
    };
    window.addEventListener(API_ERROR_EVENT, handler);
    return () => window.removeEventListener(API_ERROR_EVENT, handler);
  }, []);

  const showAuthHint = useMemo(() => {
    if (!health?.auth_required) {
      return false;
    }
    if (!health.auth_key_configured) {
      return true;
    }
    return !getMessagingHubApiKey();
  }, [health]);

  const saveApiKey = () => {
    const value = apiKeyInput.trim();
    if (!value) {
      window.localStorage.removeItem(API_KEY_STORAGE_KEY);
      return;
    }
    window.localStorage.setItem(API_KEY_STORAGE_KEY, value);
    setNotice(null);
  };

  const clearApiKey = () => {
    window.localStorage.removeItem(API_KEY_STORAGE_KEY);
    setApiKeyInput("");
  };

  if (!showAuthHint && !notice) {
    return null;
  }

  return (
    <div className="space-y-3 mb-4">
      {showAuthHint && (
        <div className="glass-panel p-4 border border-amber-300/70 bg-amber-50/70">
          <div className="flex items-start justify-between gap-3">
            <div className="flex items-start gap-2">
              <AlertTriangle size={18} className="text-amber-600 mt-0.5" />
              <div>
                <p className="font-semibold text-amber-800">
                  API 認証が必要です
                </p>
                <p className="text-sm text-amber-700 mt-1">
                  {health?.auth_key_configured
                    ? "画面右下の API 呼び出しが失敗しています。API Key を保存して再試行してください。"
                    : `バックエンド側で ${health?.auth_env_var ?? "MESSAGING_HUB_API_KEY"} が未設定です。`}
                </p>
              </div>
            </div>
            <button
              onClick={() => window.location.reload()}
              className="text-amber-700 hover:text-amber-900"
              title="再読み込み"
            >
              <RefreshCw size={16} />
            </button>
          </div>

          <div className="mt-3 flex flex-wrap items-center gap-2">
            <input
              value={apiKeyInput}
              onChange={(event) => setApiKeyInput(event.target.value)}
              placeholder="MESSAGING_HUB_API_KEY を入力"
              className="flex-1 min-w-[280px] border rounded px-3 py-1.5 text-sm bg-white"
            />
            <button
              onClick={saveApiKey}
              className="neo-button px-4 py-1.5 text-sm"
            >
              保存
            </button>
            <button
              onClick={clearApiKey}
              className="px-3 py-1.5 text-sm border rounded bg-white/70"
            >
              クリア
            </button>
          </div>
        </div>
      )}

      {notice && (
        <div className="glass-panel p-4 border border-rose-300/70 bg-rose-50/70">
          <div className="flex items-start justify-between gap-3">
            <div>
              <p className="font-semibold text-rose-800">
                API エラー ({notice.status || "network"})
              </p>
              <p className="text-sm text-rose-700 mt-1">{notice.message}</p>
              <p className="text-xs text-rose-600 mt-1 break-all">
                {notice.url}
              </p>
            </div>
            <button
              onClick={() => setNotice(null)}
              className="text-rose-700 hover:text-rose-900"
              title="閉じる"
            >
              <X size={16} />
            </button>
          </div>
        </div>
      )}
    </div>
  );
}

/**
 * レイアウトコンポーネント
 *
 * サイドバーナビゲーションとメインコンテンツエリア。
 * サイドバーは折り畳み可能（faqapp スタイルの PanelLeftClose/PanelLeft ボタン）。
 */
export default function Layout() {
  const [sidebarOpen, setSidebarOpen] = useState(true);

  return (
    <div className="app-shell">
      <div className="ambient-orb ambient-orb--one" />
      <div className="ambient-orb ambient-orb--two" />
      <div className="relative z-10 flex h-screen">
        {/* ===== デスクトップ サイドバー ===== */}
        <aside
          className={clsx(
            "glass-panel m-3 hidden md:flex flex-col overflow-hidden transition-all duration-300 ease-in-out",
            sidebarOpen ? "w-64" : "w-12",
          )}
        >
          {/* 折り畳み時：アイコンのみ縦並び（グループ区切り線あり） */}
          {!sidebarOpen && (
            <div className="flex flex-col items-center pt-3 gap-1">
              {/* 展開ボタン */}
              <button
                onClick={() => setSidebarOpen(true)}
                className="p-1.5 rounded-lg text-slate-500 hover:text-primary-700 hover:bg-white/60 transition-all"
                title="サイドバーを開く"
              >
                <PanelLeft size={18} />
              </button>
              {/* ナビアイコン（グループ間に区切り線） */}
              {navGroups.map((group, gi) => (
                <div
                  key={group.groupLabel}
                  className={clsx(
                    "flex flex-col items-center gap-1 w-full",
                    gi > 0 && "mt-1 pt-1 border-t border-slate-200/60",
                  )}
                >
                  {group.items.map((item) => {
                    const { to, icon: Icon, label } = item;
                    return (
                      <NavLink
                        key={`collapsed-${to}`}
                        to={to}
                        end={to === "/"}
                        onMouseEnter={() => warmupRoute(item)}
                        title={label}
                        className={({ isActive }) =>
                          clsx(
                            "p-2 rounded-xl transition-all",
                            isActive
                              ? "bg-white text-primary-700 shadow"
                              : "text-slate-500 hover:bg-white/60 hover:text-primary-700",
                          )
                        }
                      >
                        <Icon size={18} />
                      </NavLink>
                    );
                  })}
                </div>
              ))}
            </div>
          )}

          {/* 展開時：グループヘッダー付きフルサイドバー */}
          {sidebarOpen && (
            <>
              <div className="px-5 pt-5 pb-2 flex items-center justify-between">
                <div>
                  <h1 className="text-lg font-bold text-primary-700 sidebar-title leading-tight">
                    📱 Messaging Hub
                  </h1>
                  <p className="text-xs text-muted mt-0.5">Control Deck</p>
                </div>
                {/* 折り畳みボタン */}
                <button
                  onClick={() => setSidebarOpen(false)}
                  className="p-1.5 rounded-lg text-slate-400 hover:text-primary-700 hover:bg-white/60 transition-all"
                  title="サイドバーを閉じる"
                >
                  <PanelLeftClose size={18} />
                </button>
              </div>

              <nav className="mt-1 flex-1 overflow-auto pb-3">
                {navGroups.map((group, gi) => (
                  <div
                    key={group.groupLabel}
                    className={clsx(
                      gi > 0 && "mt-3 pt-2 border-t border-slate-200/60",
                    )}
                  >
                    {/* グループラベル */}
                    <p className="mx-4 mb-1 text-[10px] font-semibold uppercase tracking-widest text-slate-400 select-none">
                      {group.groupLabel}
                    </p>
                    {/* グループ内アイテム */}
                    {group.items.map((item) => {
                      const { to, icon: Icon, label } = item;
                      return (
                        <NavLink
                          key={to}
                          to={to}
                          end={to === "/"}
                          onMouseEnter={() => warmupRoute(item)}
                          onFocus={() => warmupRoute(item)}
                          className={({ isActive }) =>
                            clsx(
                              "mx-3 mb-0.5 flex items-center gap-3 px-3 py-2.5 rounded-xl text-slate-700 transition-all",
                              "hover:bg-white/90 hover:text-primary-700 elevated",
                              isActive && "bg-white text-primary-700 shadow",
                            )
                          }
                        >
                          <Icon size={17} />
                          <span className="font-medium text-sm">{label}</span>
                        </NavLink>
                      );
                    })}
                  </div>
                ))}
              </nav>
            </>
          )}
        </aside>

        <main className="flex-1 overflow-auto p-4 md:p-6">
          <div className="mb-3 flex justify-end">
            <InstallAppButton />
          </div>
          <HealthAndAuthBanner />
          {/* モバイル用ナビ */}
          <div className="md:hidden glass-panel mb-4 p-2">
            <nav className="flex flex-wrap gap-2">
              {allNavItems.map((item) => {
                const { to, icon: Icon, label } = item;
                return (
                  <NavLink
                    key={`mobile-${to}`}
                    to={to}
                    end={to === "/"}
                    onTouchStart={() => warmupRoute(item)}
                    onFocus={() => warmupRoute(item)}
                    className={({ isActive }) =>
                      clsx(
                        "flex items-center gap-2 px-3 py-2 rounded-lg text-sm",
                        isActive
                          ? "bg-white text-primary-700 shadow"
                          : "text-slate-700",
                      )
                    }
                  >
                    <Icon size={15} />
                    <span>{label}</span>
                  </NavLink>
                );
              })}
            </nav>
          </div>
          <Outlet />
        </main>
      </div>
    </div>
  );
}
