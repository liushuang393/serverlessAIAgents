import { useMemo, useState } from "react";
import { useQuery } from "@tanstack/react-query";
import {
  CheckCircle,
  ExternalLink,
  Eye,
  EyeOff,
  KeyRound,
  PlugZap,
  Radio,
  RefreshCw,
  ShieldCheck,
  XCircle,
} from "lucide-react";
import clsx from "clsx";
import {
  connectPlatform,
  getPlatforms,
  savePlatformCredentials,
  type PlatformStatus,
} from "../api/client";
import { formatDistanceToNow } from "date-fns";
import { ja } from "date-fns/locale";
import { usePageVisibility } from "../hooks/usePageVisibility";

const platformIcons: Record<string, string> = {
  telegram: "📱",
  slack: "💼",
  discord: "🎮",
  teams: "🏢",
  whatsapp: "💬",
  signal: "🔒",
  bizcore_nexus: "🛡️",
};

type NoticeKind = "success" | "error";

interface PlatformNotice {
  kind: NoticeKind;
  message: string;
}

function formatPlatformErrorMessage(rawMessage: string): string {
  const normalized = rawMessage.trim();
  if (!normalized) {
    return "接続に失敗しました。";
  }

  const lower = normalized.toLowerCase();
  const missingPrefix = "missing required credentials:";
  if (lower.startsWith(missingPrefix)) {
    const missing = normalized.slice(missingPrefix.length).trim();
    return missing
      ? `接続前に必須認証情報を設定してください: ${missing}`
      : "接続前に必須認証情報を設定してください。";
  }
  if (lower === "missing_required_credentials") {
    return "接続前に必須認証情報を設定してください。";
  }
  return normalized;
}

function requiredCredentialLabels(platform: PlatformStatus): string[] {
  const fields = platform.credentialFields ?? [];
  return fields
    .filter((field) => field.required && !field.configured)
    .map((field) => field.label || field.key);
}

/**
 * プラットフォーム一覧
 *
 * 各メッセージングプラットフォームの接続状態を表示
 */
export default function Platforms() {
  const isVisible = usePageVisibility();
  const {
    data: platforms,
    isLoading,
    isError,
    error,
    isFetching,
    refetch,
  } = useQuery({
    queryKey: ["platforms"],
    queryFn: getPlatforms,
    refetchInterval: isVisible ? 15000 : false,
    refetchIntervalInBackground: false,
    retry: 1,
  });
  const hasPlatforms = Array.isArray(platforms) && platforms.length > 0;
  const [expandedPlatform, setExpandedPlatform] = useState<string | null>(null);
  const [credentialDrafts, setCredentialDrafts] = useState<
    Record<string, Record<string, string>>
  >({});
  const [revealFields, setRevealFields] = useState<
    Record<string, Record<string, boolean>>
  >({});
  const [savingPlatform, setSavingPlatform] = useState<string | null>(null);
  const [connectingPlatform, setConnectingPlatform] = useState<string | null>(
    null,
  );
  const [notices, setNotices] = useState<Record<string, PlatformNotice>>({});

  const orderedPlatforms = useMemo(
    () =>
      [...(platforms ?? [])].sort((left, right) => {
        if (left.connected === right.connected) {
          return left.name.localeCompare(right.name);
        }
        return left.connected ? -1 : 1;
      }),
    [platforms],
  );

  const setNotice = (platform: string, notice: PlatformNotice) => {
    setNotices((current) => ({ ...current, [platform]: notice }));
  };

  const updateCredentialDraft = (
    platform: string,
    key: string,
    value: string,
  ) => {
    setCredentialDrafts((current) => ({
      ...current,
      [platform]: {
        ...(current[platform] ?? {}),
        [key]: value,
      },
    }));
  };

  const toggleReveal = (platform: string, key: string) => {
    setRevealFields((current) => ({
      ...current,
      [platform]: {
        ...(current[platform] ?? {}),
        [key]: !(current[platform]?.[key] ?? false),
      },
    }));
  };

  const handleConnect = async (platform: PlatformStatus) => {
    const platformName = platform.name;
    const missingLabels = requiredCredentialLabels(platform);
    if (missingLabels.length > 0) {
      setExpandedPlatform(platformName);
      setNotice(platformName, {
        kind: "error",
        message: `接続前に必須認証情報を設定してください: ${missingLabels.join(", ")}`,
      });
      return;
    }

    setConnectingPlatform(platformName);
    try {
      const result = await connectPlatform(platformName);
      if (result.ok) {
        setNotice(platformName, {
          kind: "success",
          message: "接続試行に成功しました。",
        });
      } else {
        setNotice(platformName, {
          kind: "error",
          message: formatPlatformErrorMessage(
            result.message ?? "接続に失敗しました。",
          ),
        });
      }
    } catch (actionError) {
      setNotice(platformName, {
        kind: "error",
        message: formatPlatformErrorMessage(
          actionError instanceof Error
            ? actionError.message
            : "接続に失敗しました。",
        ),
      });
    } finally {
      setConnectingPlatform(null);
      await refetch();
    }
  };

  const handleSaveCredentials = async (platform: PlatformStatus) => {
    const fieldSpecs = platform.credentialFields ?? [];
    const draft = credentialDrafts[platform.name] ?? {};
    const values = fieldSpecs.reduce<Record<string, string>>((acc, spec) => {
      const raw = draft[spec.key]?.trim();
      if (raw) {
        acc[spec.key] = raw;
      }
      return acc;
    }, {});

    if (Object.keys(values).length === 0) {
      setNotice(platform.name, {
        kind: "error",
        message: "保存する API Key / Secret を入力してください。",
      });
      return;
    }

    setSavingPlatform(platform.name);
    try {
      const result = await savePlatformCredentials(platform.name, values, true);
      if (result.ok) {
        setNotice(platform.name, {
          kind: "success",
          message: "認証情報を保存し、接続を更新しました。",
        });
        setCredentialDrafts((current) => ({ ...current, [platform.name]: {} }));
      } else {
        setNotice(platform.name, {
          kind: "error",
          message: result.message ?? "認証情報の保存に失敗しました。",
        });
      }
    } catch (actionError) {
      setNotice(platform.name, {
        kind: "error",
        message: formatPlatformErrorMessage(
          actionError instanceof Error
            ? actionError.message
            : "認証情報の保存に失敗しました。",
        ),
      });
    } finally {
      setSavingPlatform(null);
      await refetch();
    }
  };

  return (
    <div>
      <div className="flex items-center justify-between mb-6">
        <h2 className="text-2xl font-bold">プラットフォーム</h2>
        <button
          onClick={() => refetch()}
          className="flex items-center gap-2 px-4 py-2 bg-primary-500 text-white rounded-lg hover:bg-primary-600 transition-colors"
        >
          <RefreshCw size={16} className={isFetching ? "animate-spin" : ""} />
          更新
        </button>
      </div>

      {isLoading ? (
        <div className="text-center py-12">読み込み中...</div>
      ) : isError ? (
        <div className="glass-panel p-6 text-center">
          <p className="text-sm text-rose-700">
            プラットフォーム情報の取得に失敗しました。
          </p>
          <p className="text-xs text-rose-600 mt-1">
            {error instanceof Error ? error.message : "不明なエラー"}
          </p>
          <button
            onClick={() => refetch()}
            className="mt-3 px-3 py-1.5 text-sm border rounded bg-white/80 hover:bg-white"
          >
            再試行
          </button>
        </div>
      ) : !hasPlatforms ? (
        <div className="glass-panel p-10 text-center text-gray-600">
          <Radio size={40} className="mx-auto mb-3 text-gray-400" />
          <p className="font-medium">接続済みプラットフォームがありません</p>
          <p className="text-sm mt-1">
            Bot トークンを設定すると、ここに接続状態が表示されます。
          </p>
        </div>
      ) : (
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
          {orderedPlatforms.map((platform) => {
            const credentials = platform.credentialFields ?? [];
            const isExpanded = expandedPlatform === platform.name;
            const hasCredentialForm =
              !platform.managed && credentials.length > 0;
            const notice = notices[platform.name] ?? null;
            return (
              <div key={platform.name} className="glass-panel p-6 space-y-4">
                <div className="flex items-center justify-between mb-4">
                  <div className="flex items-center gap-3">
                    <span className="text-3xl">
                      {platform.icon || platformIcons[platform.name] || "📡"}
                    </span>
                    <div>
                      <h3 className="text-lg font-semibold">
                        {platform.displayName || platform.name}
                      </h3>
                      <p className="text-xs text-gray-500">{platform.name}</p>
                    </div>
                  </div>
                  {platform.connected ? (
                    <CheckCircle className="text-green-500" size={24} />
                  ) : (
                    <XCircle className="text-red-500" size={24} />
                  )}
                </div>

                <div className="space-y-2 text-sm">
                  <div className="flex justify-between">
                    <span className="text-gray-500">状態</span>
                    <span
                      className={
                        platform.connected ? "text-green-600" : "text-red-600"
                      }
                    >
                      {platform.connected ? "接続中" : "未接続"}
                    </span>
                  </div>
                  {platform.description && (
                    <p className="text-xs text-gray-500">
                      {platform.description}
                    </p>
                  )}
                  <div className="flex justify-between">
                    <span className="text-gray-500">メッセージ数</span>
                    <span>{platform.messageCount.toLocaleString()}</span>
                  </div>
                  {platform.lastActivity && (
                    <div className="flex justify-between">
                      <span className="text-gray-500">最終アクティビティ</span>
                      <span>
                        {formatDistanceToNow(new Date(platform.lastActivity), {
                          addSuffix: true,
                          locale: ja,
                        })}
                      </span>
                    </div>
                  )}
                </div>

                <div className="flex flex-wrap gap-2 pt-2 border-t border-white/60">
                  {platform.authUrl && (
                    <a
                      href={platform.authUrl}
                      target="_blank"
                      rel="noreferrer"
                      className="inline-flex items-center gap-1 px-3 py-1.5 text-xs rounded border bg-white/80 hover:bg-white"
                    >
                      <ExternalLink size={12} />
                      認証ページ
                    </a>
                  )}
                  {platform.docsUrl && (
                    <a
                      href={platform.docsUrl}
                      target="_blank"
                      rel="noreferrer"
                      className="inline-flex items-center gap-1 px-3 py-1.5 text-xs rounded border bg-white/80 hover:bg-white"
                    >
                      <ShieldCheck size={12} />
                      設定ガイド
                    </a>
                  )}
                  {!platform.managed && (
                    <button
                      onClick={() => void handleConnect(platform)}
                      disabled={connectingPlatform === platform.name}
                      className="inline-flex items-center gap-1 px-3 py-1.5 text-xs rounded border bg-primary-500 text-white hover:bg-primary-600 disabled:opacity-60"
                    >
                      <PlugZap size={12} />
                      {connectingPlatform === platform.name
                        ? "接続中..."
                        : "接続テスト"}
                    </button>
                  )}
                  {hasCredentialForm && (
                    <button
                      onClick={() =>
                        setExpandedPlatform((current) =>
                          current === platform.name ? null : platform.name,
                        )
                      }
                      className="inline-flex items-center gap-1 px-3 py-1.5 text-xs rounded border bg-white/80 hover:bg-white"
                    >
                      <KeyRound size={12} />
                      {isExpanded ? "入力を閉じる" : "API Key入力"}
                    </button>
                  )}
                </div>

                {platform.managed && (
                  <div className="text-xs text-emerald-700 bg-emerald-50 border border-emerald-200 rounded px-3 py-2">
                    このチャネルは内蔵プラットフォームです。追加認証は不要です。
                  </div>
                )}

                {hasCredentialForm && isExpanded && (
                  <div className="space-y-3 border border-cyan-200 bg-cyan-50/60 rounded-lg p-3">
                    <p className="text-xs text-cyan-800">
                      入力値はマスク表示され、ブラウザ localStorage
                      には保持しません。
                    </p>
                    {credentials.map((field) => (
                      <div
                        key={`${platform.name}-${field.key}`}
                        className="space-y-1"
                      >
                        <label className="text-xs text-gray-700 flex items-center gap-2">
                          <span>{field.label}</span>
                          {field.required && (
                            <span className="text-rose-600">*</span>
                          )}
                          {field.configured && field.maskedValue && (
                            <span className="text-emerald-700">
                              設定済み: {field.maskedValue}
                            </span>
                          )}
                        </label>
                        <div className="flex items-center gap-2">
                          <input
                            type={
                              revealFields[platform.name]?.[field.key]
                                ? "text"
                                : "password"
                            }
                            autoComplete="new-password"
                            spellCheck={false}
                            maxLength={512}
                            value={
                              credentialDrafts[platform.name]?.[field.key] ?? ""
                            }
                            onChange={(event) =>
                              updateCredentialDraft(
                                platform.name,
                                field.key,
                                event.target.value,
                              )
                            }
                            placeholder={field.placeholder || field.label}
                            className="flex-1 border rounded-lg px-3 py-2 text-sm bg-white"
                          />
                          <button
                            onClick={() =>
                              toggleReveal(platform.name, field.key)
                            }
                            className="px-2 py-2 border rounded bg-white hover:bg-gray-50"
                            title="表示切替"
                          >
                            {revealFields[platform.name]?.[field.key] ? (
                              <EyeOff size={14} />
                            ) : (
                              <Eye size={14} />
                            )}
                          </button>
                        </div>
                      </div>
                    ))}
                    <button
                      onClick={() => void handleSaveCredentials(platform)}
                      disabled={savingPlatform === platform.name}
                      className={clsx(
                        "inline-flex items-center gap-2 px-4 py-2 text-sm rounded text-white",
                        "bg-primary-600 hover:bg-primary-700 disabled:opacity-60",
                      )}
                    >
                      <KeyRound size={14} />
                      {savingPlatform === platform.name
                        ? "保存中..."
                        : "安全に保存して接続"}
                    </button>
                  </div>
                )}

                {notice && (
                  <div
                    className={clsx(
                      "text-xs rounded px-3 py-2 border",
                      notice.kind === "success"
                        ? "bg-emerald-50 text-emerald-700 border-emerald-200"
                        : "bg-rose-50 text-rose-700 border-rose-200",
                    )}
                  >
                    {notice.message}
                  </div>
                )}
              </div>
            );
          })}
        </div>
      )}
    </div>
  );
}
