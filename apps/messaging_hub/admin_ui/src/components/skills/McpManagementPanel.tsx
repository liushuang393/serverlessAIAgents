import { useEffect, useState } from "react";
import {
  CheckCircle2,
  Loader2,
  Plus,
  RotateCcw,
  Search,
  Server,
  Trash2,
} from "lucide-react";
import clsx from "clsx";

export type InstallMethod = "config" | "dynamic";

export interface MCPServer {
  name: string;
  command: string;
  args: string[];
  env: Record<string, string>;
  enabled: boolean;
  description: string;
  install_method: InstallMethod;
}

export interface LazyLoadingConfig {
  enabled: boolean;
  threshold: number;
  auto_load_on_call: boolean;
  cache_session: boolean;
  default_load_count: number;
}

export interface ToolIndexEntry {
  name: string;
  description: string;
  source: string;
  server: string;
  loaded: boolean;
}

export interface ToolLoaderStats {
  total_indexed: number;
  loaded_count: number;
  unloaded_count: number;
  search_count: number;
  default_skills: string[];
  loaded_tools: string[];
}

export interface ToolSearchResponse {
  query: string;
  matches: ToolIndexEntry[];
  loaded_count: number;
}

export interface MCPInstallRequestPayload {
  name: string;
  command: string;
  args: string[];
  env: Record<string, string>;
  description: string;
  install_method: InstallMethod;
}

interface McpManagementPanelProps {
  servers: MCPServer[];
  lazyLoading: LazyLoadingConfig | null;
  toolIndex: ToolIndexEntry[];
  toolStats: ToolLoaderStats | null;
  searchResult: ToolSearchResponse | null;
  onInstallServer: (payload: MCPInstallRequestPayload) => Promise<boolean>;
  onToggleServer: (serverName: string, enabled: boolean) => Promise<boolean>;
  onDeleteServer: (serverName: string) => Promise<boolean>;
  onUpdateLazyLoading: (payload: LazyLoadingConfig) => Promise<boolean>;
  onSearchTools: (query: string) => Promise<boolean>;
  onResetTools: () => Promise<boolean>;
}

interface InstallFormState {
  name: string;
  description: string;
  command: string;
  argsText: string;
  envText: string;
  installMethod: InstallMethod;
}

const installMethodDescriptions: Record<InstallMethod, string> = {
  config:
    "構成管理型です。自社 MCP や固定接続先を永続登録し、再起動後も同じ設定で運用します。",
  dynamic:
    "動的取得型です。npx / uvx などを利用し、外部パッケージを必要時に取得して起動します。",
};

const initialInstallForm: InstallFormState = {
  name: "",
  description: "",
  command: "npx",
  argsText: "",
  envText: "",
  installMethod: "dynamic",
};

/**
 * MCP 管理パネル。
 *
 * MCP サーバーの追加・有効化・削除、懒加载設定、
 * ツールインデックス検索を 1 画面で操作する。
 */
export function McpManagementPanel({
  servers,
  lazyLoading,
  toolIndex,
  toolStats,
  searchResult,
  onInstallServer,
  onToggleServer,
  onDeleteServer,
  onUpdateLazyLoading,
  onSearchTools,
  onResetTools,
}: McpManagementPanelProps) {
  const [installForm, setInstallForm] =
    useState<InstallFormState>(initialInstallForm);
  const [formErrorMessage, setFormErrorMessage] = useState<string | null>(null);
  const [searchQuery, setSearchQuery] = useState("");
  const [lazyDraft, setLazyDraft] = useState<LazyLoadingConfig | null>(
    lazyLoading,
  );
  const [installing, setInstalling] = useState(false);
  const [savingLazyConfig, setSavingLazyConfig] = useState(false);
  const [searchingTools, setSearchingTools] = useState(false);
  const [resettingTools, setResettingTools] = useState(false);

  useEffect(() => {
    setLazyDraft(lazyLoading);
  }, [lazyLoading]);

  const handleInstallMethodChange = (value: string) => {
    const installMethod: InstallMethod =
      value === "config" ? "config" : "dynamic";
    setInstallForm((current) => ({
      ...current,
      installMethod,
      command: installMethod === "config" ? "python" : "npx",
    }));
  };

  const parseEnvText = (envText: string): Record<string, string> | null => {
    const lines = envText
      .split("\n")
      .map((line) => line.trim())
      .filter((line) => line.length > 0);
    const env: Record<string, string> = {};

    for (const line of lines) {
      const separatorIndex = line.indexOf("=");
      if (separatorIndex <= 0) {
        setFormErrorMessage("環境変数は KEY=VALUE 形式で入力してください。");
        return null;
      }

      const key = line.slice(0, separatorIndex).trim();
      const value = line.slice(separatorIndex + 1).trim();
      if (!key) {
        setFormErrorMessage("環境変数キーは空にできません。");
        return null;
      }
      env[key] = value;
    }

    return env;
  };

  const handleInstall = async (): Promise<void> => {
    if (!installForm.name.trim() || !installForm.command.trim()) {
      setFormErrorMessage("サーバー名と起動コマンドは必須です。");
      return;
    }

    setInstalling(true);
    setFormErrorMessage(null);
    const env = parseEnvText(installForm.envText);
    if (env === null) {
      setInstalling(false);
      return;
    }

    const installed = await onInstallServer({
      name: installForm.name.trim(),
      description: installForm.description.trim(),
      command: installForm.command.trim(),
      args: installForm.argsText
        .split(/\s+/)
        .map((value) => value.trim())
        .filter((value) => value.length > 0),
      env,
      install_method: installForm.installMethod,
    });

    if (installed) {
      setInstallForm(initialInstallForm);
    }
    setInstalling(false);
  };

  const handleLazyConfigSave = async (): Promise<void> => {
    if (lazyDraft === null) {
      return;
    }

    setSavingLazyConfig(true);
    await onUpdateLazyLoading(lazyDraft);
    setSavingLazyConfig(false);
  };

  const handleToolSearch = async (): Promise<void> => {
    if (!searchQuery.trim()) {
      return;
    }

    setSearchingTools(true);
    await onSearchTools(searchQuery.trim());
    setSearchingTools(false);
  };

  const handleToolReset = async (): Promise<void> => {
    setResettingTools(true);
    await onResetTools();
    setResettingTools(false);
  };

  return (
    <div className="grid gap-6 xl:grid-cols-[1.1fr_0.9fr]">
      <div className="space-y-6">
        <section className="bg-white rounded-lg shadow p-5 space-y-4">
          <div className="flex items-center gap-2">
            <Server className="text-primary-500" size={20} />
            <h2 className="text-lg font-semibold text-gray-900">
              インストール済み MCP
            </h2>
          </div>

          {servers.length === 0 ? (
            <div className="rounded-lg border border-dashed border-gray-300 px-4 py-8 text-center text-gray-500">
              MCP サーバーはまだ登録されていません。
            </div>
          ) : (
            <div className="space-y-3">
              {servers.map((server) => (
                <div
                  key={server.name}
                  className="rounded-lg border border-gray-200 p-4"
                >
                  <div className="flex flex-wrap items-start justify-between gap-3">
                    <div className="space-y-2">
                      <div className="flex flex-wrap items-center gap-2">
                        <span className="font-medium text-gray-900">
                          {server.name}
                        </span>
                        <span
                          className={clsx(
                            "text-xs rounded-full px-2 py-0.5",
                            server.enabled
                              ? "bg-emerald-100 text-emerald-700"
                              : "bg-gray-200 text-gray-600",
                          )}
                        >
                          {server.enabled ? "有効" : "無効"}
                        </span>
                        <span className="text-xs rounded-full bg-blue-100 px-2 py-0.5 text-blue-700">
                          {server.install_method === "config"
                            ? "構成管理型"
                            : "動的取得型"}
                        </span>
                      </div>
                      <p className="text-sm text-gray-600">
                        {server.description || "説明なし"}
                      </p>
                      <p className="text-xs rounded bg-gray-50 px-3 py-2 font-mono text-gray-600">
                        {server.command} {server.args.join(" ")}
                      </p>
                    </div>

                    <div className="flex flex-wrap gap-2">
                      <button
                        onClick={() =>
                          void onToggleServer(server.name, !server.enabled)
                        }
                        className="rounded-lg border border-gray-200 px-3 py-2 text-sm font-medium text-gray-700 hover:bg-gray-50"
                      >
                        {server.enabled ? "無効化" : "有効化"}
                      </button>
                      <button
                        onClick={() => void onDeleteServer(server.name)}
                        className="flex items-center gap-1 rounded-lg border border-rose-200 px-3 py-2 text-sm font-medium text-rose-700 hover:bg-rose-50"
                      >
                        <Trash2 size={14} />
                        削除
                      </button>
                    </div>
                  </div>
                </div>
              ))}
            </div>
          )}
        </section>

        <section className="bg-white rounded-lg shadow p-5 space-y-4">
          <div className="flex items-center gap-2">
            <Plus className="text-primary-500" size={20} />
            <h2 className="text-lg font-semibold text-gray-900">
              MCP を新規追加
            </h2>
          </div>

          <div className="grid gap-3 md:grid-cols-2">
            {(["config", "dynamic"] as InstallMethod[]).map((method) => (
              <label key={method} className="cursor-pointer">
                <input
                  type="radio"
                  name="install_method"
                  className="sr-only"
                  checked={installForm.installMethod === method}
                  onChange={(event) =>
                    handleInstallMethodChange(event.target.value)
                  }
                  value={method}
                />
                <div
                  className={clsx(
                    "rounded-lg border p-4 transition-colors",
                    installForm.installMethod === method
                      ? "border-primary-500 bg-primary-50"
                      : "border-gray-200 bg-white",
                  )}
                >
                  <div className="flex items-center gap-2 font-medium text-gray-900">
                    {installForm.installMethod === method && (
                      <CheckCircle2 size={16} className="text-primary-500" />
                    )}
                    {method === "config" ? "構成管理型" : "動的取得型"}
                  </div>
                  <p className="mt-2 text-sm text-gray-600">
                    {installMethodDescriptions[method]}
                  </p>
                </div>
              </label>
            ))}
          </div>

          {formErrorMessage && (
            <div className="rounded-lg border border-rose-200 bg-rose-50 px-4 py-3 text-sm text-rose-700">
              {formErrorMessage}
            </div>
          )}

          <div className="grid gap-4 md:grid-cols-2">
            <label className="space-y-2 text-sm text-gray-700">
              <span className="font-medium">サーバー名</span>
              <input
                value={installForm.name}
                onChange={(event) =>
                  setInstallForm((current) => ({
                    ...current,
                    name: event.target.value,
                  }))
                }
                className="w-full rounded-lg border border-gray-300 px-3 py-2"
                placeholder="slack"
              />
            </label>
            <label className="space-y-2 text-sm text-gray-700">
              <span className="font-medium">起動コマンド</span>
              <input
                value={installForm.command}
                onChange={(event) =>
                  setInstallForm((current) => ({
                    ...current,
                    command: event.target.value,
                  }))
                }
                className="w-full rounded-lg border border-gray-300 px-3 py-2"
                placeholder={
                  installForm.installMethod === "config" ? "python" : "npx"
                }
              />
            </label>
          </div>

          <label className="space-y-2 text-sm text-gray-700">
            <span className="font-medium">説明</span>
            <input
              value={installForm.description}
              onChange={(event) =>
                setInstallForm((current) => ({
                  ...current,
                  description: event.target.value,
                }))
              }
              className="w-full rounded-lg border border-gray-300 px-3 py-2"
              placeholder="Slack 連携用の外部 MCP サーバー"
            />
          </label>

          <label className="space-y-2 text-sm text-gray-700">
            <span className="font-medium">引数（半角スペース区切り）</span>
            <input
              value={installForm.argsText}
              onChange={(event) =>
                setInstallForm((current) => ({
                  ...current,
                  argsText: event.target.value,
                }))
              }
              className="w-full rounded-lg border border-gray-300 px-3 py-2 font-mono text-sm"
              placeholder={
                installForm.installMethod === "config"
                  ? "-m apps.messaging_hub.mcp.slack_server"
                  : "-y @modelcontextprotocol/server-slack"
              }
            />
          </label>

          <label className="space-y-2 text-sm text-gray-700">
            <span className="font-medium">
              環境変数（1 行 1 件、KEY=VALUE 形式）
            </span>
            <textarea
              value={installForm.envText}
              onChange={(event) =>
                setInstallForm((current) => ({
                  ...current,
                  envText: event.target.value,
                }))
              }
              className="min-h-28 w-full rounded-lg border border-gray-300 px-3 py-2 font-mono text-sm"
              placeholder="SLACK_BOT_TOKEN=***"
            />
          </label>

          <button
            onClick={() => void handleInstall()}
            disabled={installing}
            className="flex items-center gap-2 rounded-lg bg-primary-500 px-4 py-2 text-white hover:bg-primary-600 disabled:opacity-50"
          >
            {installing ? (
              <Loader2 className="animate-spin" size={16} />
            ) : (
              <Plus size={16} />
            )}
            MCP を追加
          </button>
        </section>
      </div>

      <div className="space-y-6">
        <section className="bg-white rounded-lg shadow p-5 space-y-4">
          <h2 className="text-lg font-semibold text-gray-900">懒加载設定</h2>

          {lazyDraft ? (
            <>
              <div className="grid gap-4 sm:grid-cols-2">
                <label className="flex items-center gap-3 rounded-lg border border-gray-200 px-3 py-3 text-sm text-gray-700">
                  <input
                    type="checkbox"
                    checked={lazyDraft.enabled}
                    onChange={(event) =>
                      setLazyDraft((current) =>
                        current
                          ? { ...current, enabled: event.target.checked }
                          : current,
                      )
                    }
                  />
                  懒加载を有効化
                </label>
                <label className="flex items-center gap-3 rounded-lg border border-gray-200 px-3 py-3 text-sm text-gray-700">
                  <input
                    type="checkbox"
                    checked={lazyDraft.auto_load_on_call}
                    onChange={(event) =>
                      setLazyDraft((current) =>
                        current
                          ? {
                              ...current,
                              auto_load_on_call: event.target.checked,
                            }
                          : current,
                      )
                    }
                  />
                  呼び出し時に自動ロード
                </label>
                <label className="flex items-center gap-3 rounded-lg border border-gray-200 px-3 py-3 text-sm text-gray-700">
                  <input
                    type="checkbox"
                    checked={lazyDraft.cache_session}
                    onChange={(event) =>
                      setLazyDraft((current) =>
                        current
                          ? { ...current, cache_session: event.target.checked }
                          : current,
                      )
                    }
                  />
                  セッションキャッシュを維持
                </label>
              </div>

              <div className="grid gap-4 sm:grid-cols-2">
                <label className="space-y-2 text-sm text-gray-700">
                  <span className="font-medium">検索しきい値</span>
                  <input
                    type="number"
                    min={1}
                    value={lazyDraft.threshold}
                    onChange={(event) =>
                      setLazyDraft((current) =>
                        current
                          ? {
                              ...current,
                              threshold: Math.max(
                                1,
                                Number(event.target.value) || 1,
                              ),
                            }
                          : current,
                      )
                    }
                    className="w-full rounded-lg border border-gray-300 px-3 py-2"
                  />
                </label>
                <label className="space-y-2 text-sm text-gray-700">
                  <span className="font-medium">初期ロード件数</span>
                  <input
                    type="number"
                    min={1}
                    value={lazyDraft.default_load_count}
                    onChange={(event) =>
                      setLazyDraft((current) =>
                        current
                          ? {
                              ...current,
                              default_load_count: Math.max(
                                1,
                                Number(event.target.value) || 1,
                              ),
                            }
                          : current,
                      )
                    }
                    className="w-full rounded-lg border border-gray-300 px-3 py-2"
                  />
                </label>
              </div>

              <button
                onClick={() => void handleLazyConfigSave()}
                disabled={savingLazyConfig}
                className="flex items-center gap-2 rounded-lg bg-primary-500 px-4 py-2 text-white hover:bg-primary-600 disabled:opacity-50"
              >
                {savingLazyConfig ? (
                  <Loader2 className="animate-spin" size={16} />
                ) : (
                  <CheckCircle2 size={16} />
                )}
                懒加载設定を保存
              </button>
            </>
          ) : (
            <div className="text-sm text-gray-500">
              懒加载設定を読み込み中です。
            </div>
          )}
        </section>

        <section className="bg-white rounded-lg shadow p-5 space-y-4">
          <h2 className="text-lg font-semibold text-gray-900">
            ツールインデックス
          </h2>

          {toolStats && (
            <div className="grid gap-3 sm:grid-cols-2">
              <div className="rounded-lg bg-slate-50 p-3 text-sm text-slate-700">
                総数: {toolStats.total_indexed}
              </div>
              <div className="rounded-lg bg-emerald-50 p-3 text-sm text-emerald-700">
                ロード済み: {toolStats.loaded_count}
              </div>
              <div className="rounded-lg bg-amber-50 p-3 text-sm text-amber-700">
                未ロード: {toolStats.unloaded_count}
              </div>
              <div className="rounded-lg bg-blue-50 p-3 text-sm text-blue-700">
                検索回数: {toolStats.search_count}
              </div>
            </div>
          )}

          {toolStats && (
            <div className="rounded-lg border border-gray-200 bg-gray-50 px-4 py-3 text-sm text-gray-600">
              既定ロード: {toolStats.default_skills.join(", ") || "なし"}
            </div>
          )}

          <div className="flex flex-col gap-3 sm:flex-row">
            <input
              value={searchQuery}
              onChange={(event) => setSearchQuery(event.target.value)}
              className="flex-1 rounded-lg border border-gray-300 px-3 py-2"
              placeholder="例: slack / browser / read"
            />
            <button
              onClick={() => void handleToolSearch()}
              disabled={searchingTools || !searchQuery.trim()}
              className="flex items-center justify-center gap-2 rounded-lg border border-primary-200 px-4 py-2 text-primary-700 hover:bg-primary-50 disabled:opacity-50"
            >
              {searchingTools ? (
                <Loader2 className="animate-spin" size={16} />
              ) : (
                <Search size={16} />
              )}
              検索してロード
            </button>
            <button
              onClick={() => void handleToolReset()}
              disabled={resettingTools}
              className="flex items-center justify-center gap-2 rounded-lg border border-gray-200 px-4 py-2 text-gray-700 hover:bg-gray-50 disabled:opacity-50"
            >
              {resettingTools ? (
                <Loader2 className="animate-spin" size={16} />
              ) : (
                <RotateCcw size={16} />
              )}
              セッションをリセット
            </button>
          </div>

          {searchResult && (
            <div className="rounded-lg border border-blue-200 bg-blue-50 px-4 py-3 text-sm text-blue-800">
              検索語「{searchResult.query}」で {searchResult.matches.length}{" "}
              件一致し、
              {searchResult.loaded_count} 件を新規ロードしました。
            </div>
          )}

          <div className="space-y-2">
            {toolIndex.map((tool) => (
              <div
                key={tool.name}
                className="rounded-lg border border-gray-200 px-4 py-3"
              >
                <div className="flex flex-wrap items-center gap-2">
                  <span className="font-medium text-gray-900">{tool.name}</span>
                  <span className="rounded-full bg-gray-100 px-2 py-0.5 text-xs text-gray-600">
                    {tool.source}
                  </span>
                  {tool.server && (
                    <span className="rounded-full bg-blue-100 px-2 py-0.5 text-xs text-blue-700">
                      {tool.server}
                    </span>
                  )}
                  <span
                    className={clsx(
                      "rounded-full px-2 py-0.5 text-xs",
                      tool.loaded
                        ? "bg-emerald-100 text-emerald-700"
                        : "bg-amber-100 text-amber-700",
                    )}
                  >
                    {tool.loaded ? "ロード済み" : "未ロード"}
                  </span>
                </div>
                <p className="mt-2 text-sm text-gray-600">
                  {tool.description || "説明なし"}
                </p>
              </div>
            ))}
          </div>
        </section>
      </div>
    </div>
  );
}
