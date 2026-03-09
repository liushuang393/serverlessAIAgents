import { useCallback, useEffect, useState } from "react";
import {
  Wrench,
  Play,
  Pause,
  Plus,
  Code,
  Workflow,
  Sparkles,
  Loader2,
  RefreshCw,
  ChevronRight,
  AlertCircle,
} from "lucide-react";
import clsx from "clsx";
import {
  McpManagementPanel,
  type LazyLoadingConfig,
  type MCPInstallRequestPayload,
  type MCPServer,
  type ToolIndexEntry,
  type ToolLoaderStats,
  type ToolSearchResponse,
} from "./skills/McpManagementPanel";

interface Skill {
  name: string;
  description: string;
  category: string;
  risk_level: string;
  requires_confirmation: boolean;
  enabled: boolean;
}

interface WorkflowDef {
  id: string;
  name: string;
  description: string;
  status: "draft" | "active" | "paused" | "archived";
  steps: { id: string; skill_name: string }[];
  updated_at: string;
}

interface SkillTestResult extends Record<string, unknown> {
  ok?: boolean;
}

const categoryEmoji: Record<string, string> = {
  os_read: "📂",
  os_write: "✏️",
  os_execute: "⚙️",
  browser: "🌐",
  network: "🔗",
};

const riskColors: Record<string, string> = {
  low: "bg-green-100 text-green-700",
  medium: "bg-yellow-100 text-yellow-700",
  high: "bg-orange-100 text-orange-700",
  critical: "bg-red-100 text-red-700",
};

/**
 * エラーメッセージを安全に文字列化する。
 *
 * 想定外の例外でも UI に意味のある文言を表示する。
 */
function toErrorMessage(error: unknown, fallbackMessage: string): string {
  if (error instanceof Error && error.message.trim()) {
    return error.message;
  }
  return fallbackMessage;
}

/**
 * レスポンス本文からユーザー向けメッセージを抽出する。
 *
 * API がテキスト・JSON どちらを返しても、空文字列なら既定文言へフォールバックする。
 */
async function readResponseMessage(
  response: Response,
  fallbackMessage: string,
): Promise<string> {
  const responseText = (await response.text()).trim();
  return responseText || fallbackMessage;
}

/**
 * JSON 入力をドライラン用パラメータとして正規化する。
 *
 * 配列や null を拒否し、オブジェクト形式のみ受け付ける。
 */
function parseJsonObject(text: string): Record<string, unknown> {
  const parsedValue: unknown = JSON.parse(text);
  if (
    typeof parsedValue !== "object" ||
    parsedValue === null ||
    Array.isArray(parsedValue)
  ) {
    throw new Error(
      "テストパラメータは JSON オブジェクト形式で入力してください。",
    );
  }
  return parsedValue as Record<string, unknown>;
}

/**
 * スキル管理ページ
 *
 * スキル一覧、ワークフロー管理、自然言語スキル生成
 */
export default function SkillsManager() {
  const [skills, setSkills] = useState<Skill[]>([]);
  const [workflows, setWorkflows] = useState<WorkflowDef[]>([]);
  const [mcpServers, setMcpServers] = useState<MCPServer[]>([]);
  const [lazyLoading, setLazyLoading] = useState<LazyLoadingConfig | null>(
    null,
  );
  const [toolIndex, setToolIndex] = useState<ToolIndexEntry[]>([]);
  const [toolStats, setToolStats] = useState<ToolLoaderStats | null>(null);
  const [toolSearchResult, setToolSearchResult] =
    useState<ToolSearchResponse | null>(null);
  const [loading, setLoading] = useState(true);
  const [activeTab, setActiveTab] = useState<
    "skills" | "mcp" | "workflows" | "generate"
  >("skills");
  const [generatePrompt, setGeneratePrompt] = useState("");
  const [generatedSkill, setGeneratedSkill] = useState<Record<
    string,
    unknown
  > | null>(null);
  const [generating, setGenerating] = useState(false);
  const [selectedSkill, setSelectedSkill] = useState<Skill | null>(null);
  const [testParams, setTestParams] = useState("{}");
  const [testResult, setTestResult] = useState<SkillTestResult | null>(null);
  const [testing, setTesting] = useState(false);
  const [errorMessage, setErrorMessage] = useState<string | null>(null);
  const [statusMessage, setStatusMessage] = useState<string | null>(null);

  const fetchData = useCallback(async (): Promise<void> => {
    setLoading(true);
    setErrorMessage(null);
    try {
      const [
        skillsRes,
        workflowsRes,
        mcpServersRes,
        lazyLoadingRes,
        toolIndexRes,
        toolStatsRes,
      ] = await Promise.all([
        fetch("/api/skills"),
        fetch("/api/workflows"),
        fetch("/api/mcp/servers"),
        fetch("/api/mcp/lazy-loading"),
        fetch("/api/tools/index"),
        fetch("/api/tools/stats"),
      ]);

      const nextErrors: string[] = [];

      if (skillsRes.ok) {
        const data = await skillsRes.json();
        setSkills(data.skills || []);
      } else {
        nextErrors.push(
          await readResponseMessage(
            skillsRes,
            "スキル一覧の取得に失敗しました。",
          ),
        );
      }
      if (workflowsRes.ok) {
        const data = await workflowsRes.json();
        setWorkflows(data.workflows || []);
      } else {
        nextErrors.push(
          await readResponseMessage(
            workflowsRes,
            "ワークフロー一覧の取得に失敗しました。",
          ),
        );
      }

      if (mcpServersRes.ok) {
        const data = await mcpServersRes.json();
        setMcpServers(data.servers || []);
      } else {
        nextErrors.push(
          await readResponseMessage(
            mcpServersRes,
            "MCP サーバー一覧の取得に失敗しました。",
          ),
        );
      }

      if (lazyLoadingRes.ok) {
        const data = await lazyLoadingRes.json();
        setLazyLoading(data.lazy_loading || null);
      } else {
        nextErrors.push(
          await readResponseMessage(
            lazyLoadingRes,
            "懒加载設定の取得に失敗しました。",
          ),
        );
      }

      if (toolIndexRes.ok) {
        const data = await toolIndexRes.json();
        setToolIndex(data.tools || []);
      } else {
        nextErrors.push(
          await readResponseMessage(
            toolIndexRes,
            "ツールインデックスの取得に失敗しました。",
          ),
        );
      }

      if (toolStatsRes.ok) {
        const data = await toolStatsRes.json();
        setToolStats(data || null);
      } else {
        nextErrors.push(
          await readResponseMessage(
            toolStatsRes,
            "ツール統計の取得に失敗しました。",
          ),
        );
      }

      if (nextErrors.length > 0) {
        setErrorMessage(nextErrors.join(" / "));
      }
    } catch (error: unknown) {
      setErrorMessage(
        toErrorMessage(error, "管理データの取得に失敗しました。"),
      );
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    void fetchData();
  }, [fetchData]);

  const handleToggleSkill = async (
    skillName: string,
    enabled: boolean,
  ): Promise<void> => {
    try {
      setErrorMessage(null);
      const response = await fetch(
        `/api/skills/${skillName}/${enabled ? "enable" : "disable"}`,
        {
          method: "POST",
        },
      );

      if (!response.ok) {
        setErrorMessage(
          await readResponseMessage(
            response,
            "スキル状態の更新に失敗しました。",
          ),
        );
        return;
      }

      setStatusMessage(
        enabled
          ? `スキル「${skillName}」を有効化しました。`
          : `スキル「${skillName}」を無効化しました。`,
      );
      await fetchData();
    } catch (error: unknown) {
      setErrorMessage(
        toErrorMessage(error, "スキル状態の更新に失敗しました。"),
      );
    }
  };

  const handleGenerate = async (): Promise<void> => {
    if (!generatePrompt.trim()) return;

    setGenerating(true);
    setGeneratedSkill(null);
    setErrorMessage(null);

    try {
      const response = await fetch("/api/skills/generate", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ description: generatePrompt }),
      });

      if (response.ok) {
        const data = await response.json();
        setGeneratedSkill(data);
        setStatusMessage("スキル定義を生成しました。内容を確認してください。");
      } else {
        setErrorMessage(
          await readResponseMessage(response, "スキル生成に失敗しました。"),
        );
      }
    } catch (error: unknown) {
      setErrorMessage(toErrorMessage(error, "スキル生成に失敗しました。"));
    } finally {
      setGenerating(false);
    }
  };

  const handleTestSkill = async (): Promise<void> => {
    if (!selectedSkill) return;

    setTesting(true);
    setTestResult(null);
    setErrorMessage(null);

    try {
      const params = parseJsonObject(testParams);
      const response = await fetch(`/api/skills/${selectedSkill.name}/call`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ params, dry_run: true }),
      });

      if (response.ok) {
        const data = await response.json();
        setTestResult(data);
      } else {
        setTestResult({
          ok: false,
          error: await readResponseMessage(
            response,
            "スキルのドライランに失敗しました。",
          ),
        });
      }
    } catch (error: unknown) {
      setTestResult({
        ok: false,
        error: toErrorMessage(error, "スキルのドライランに失敗しました。"),
      });
    } finally {
      setTesting(false);
    }
  };

  const handleInstallMcpServer = useCallback(
    async (payload: MCPInstallRequestPayload): Promise<boolean> => {
      try {
        setErrorMessage(null);
        const response = await fetch("/api/mcp/servers", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify(payload),
        });

        if (!response.ok) {
          setErrorMessage(
            await readResponseMessage(
              response,
              "MCP サーバーの追加に失敗しました。",
            ),
          );
          return false;
        }

        setStatusMessage(`MCP サーバー「${payload.name}」を追加しました。`);
        await fetchData();
        return true;
      } catch (error: unknown) {
        setErrorMessage(
          toErrorMessage(error, "MCP サーバーの追加に失敗しました。"),
        );
        return false;
      }
    },
    [fetchData],
  );

  const handleToggleMcpServer = useCallback(
    async (serverName: string, enabled: boolean): Promise<boolean> => {
      try {
        setErrorMessage(null);
        const response = await fetch(
          `/api/mcp/servers/${serverName}/${enabled ? "enable" : "disable"}`,
          { method: "POST" },
        );

        if (!response.ok) {
          setErrorMessage(
            await readResponseMessage(
              response,
              "MCP サーバー状態の更新に失敗しました。",
            ),
          );
          return false;
        }

        setStatusMessage(
          enabled
            ? `MCP サーバー「${serverName}」を有効化しました。`
            : `MCP サーバー「${serverName}」を無効化しました。`,
        );
        await fetchData();
        return true;
      } catch (error: unknown) {
        setErrorMessage(
          toErrorMessage(error, "MCP サーバー状態の更新に失敗しました。"),
        );
        return false;
      }
    },
    [fetchData],
  );

  const handleDeleteMcpServer = useCallback(
    async (serverName: string): Promise<boolean> => {
      try {
        setErrorMessage(null);
        const response = await fetch(`/api/mcp/servers/${serverName}`, {
          method: "DELETE",
        });

        if (!response.ok) {
          setErrorMessage(
            await readResponseMessage(
              response,
              "MCP サーバーの削除に失敗しました。",
            ),
          );
          return false;
        }

        setStatusMessage(`MCP サーバー「${serverName}」を削除しました。`);
        await fetchData();
        return true;
      } catch (error: unknown) {
        setErrorMessage(
          toErrorMessage(error, "MCP サーバーの削除に失敗しました。"),
        );
        return false;
      }
    },
    [fetchData],
  );

  const handleUpdateLazyLoading = useCallback(
    async (payload: LazyLoadingConfig): Promise<boolean> => {
      try {
        setErrorMessage(null);
        const response = await fetch("/api/mcp/lazy-loading", {
          method: "PATCH",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify(payload),
        });

        if (!response.ok) {
          setErrorMessage(
            await readResponseMessage(
              response,
              "懒加载設定の更新に失敗しました。",
            ),
          );
          return false;
        }

        const data = await response.json();
        setLazyLoading(data.lazy_loading || payload);
        setStatusMessage("懒加载設定を更新しました。");
        await fetchData();
        return true;
      } catch (error: unknown) {
        setErrorMessage(
          toErrorMessage(error, "懒加载設定の更新に失敗しました。"),
        );
        return false;
      }
    },
    [fetchData],
  );

  const handleSearchTools = useCallback(
    async (query: string): Promise<boolean> => {
      try {
        setErrorMessage(null);
        const response = await fetch("/api/tools/search", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ query }),
        });

        if (!response.ok) {
          setErrorMessage(
            await readResponseMessage(response, "ツール検索に失敗しました。"),
          );
          return false;
        }

        const data = await response.json();
        setToolSearchResult(data);
        setStatusMessage(`ツール検索「${query}」を実行しました。`);
        await fetchData();
        return true;
      } catch (error: unknown) {
        setErrorMessage(toErrorMessage(error, "ツール検索に失敗しました。"));
        return false;
      }
    },
    [fetchData],
  );

  const handleResetTools = useCallback(async (): Promise<boolean> => {
    try {
      setErrorMessage(null);
      const response = await fetch("/api/tools/reset", { method: "POST" });

      if (!response.ok) {
        setErrorMessage(
          await readResponseMessage(
            response,
            "ツールセッションのリセットに失敗しました。",
          ),
        );
        return false;
      }

      setToolSearchResult(null);
      setStatusMessage("ツールセッションをリセットしました。");
      await fetchData();
      return true;
    } catch (error: unknown) {
      setErrorMessage(
        toErrorMessage(error, "ツールセッションのリセットに失敗しました。"),
      );
      return false;
    }
  }, [fetchData]);

  const renderSkillCard = (skill: Skill) => (
    <div
      key={skill.name}
      className={clsx(
        "bg-white rounded-lg shadow p-4 border-l-4 cursor-pointer transition-shadow hover:shadow-md",
        skill.enabled ? "border-green-500" : "border-gray-300",
      )}
      onClick={() => setSelectedSkill(skill)}
    >
      <div className="flex items-start justify-between">
        <div className="flex-1">
          <div className="flex items-center gap-2">
            <span className="text-lg">
              {categoryEmoji[skill.category] || "🔧"}
            </span>
            <span className="font-medium">{skill.name}</span>
            <span
              className={clsx(
                "text-xs px-2 py-0.5 rounded-full",
                riskColors[skill.risk_level],
              )}
            >
              {skill.risk_level}
            </span>
            {skill.requires_confirmation && (
              <span title="承認が必要">
                <AlertCircle size={14} className="text-orange-500" />
              </span>
            )}
          </div>
          <p className="text-sm text-gray-600 mt-1">{skill.description}</p>
        </div>

        <button
          onClick={(e) => {
            e.stopPropagation();
            void handleToggleSkill(skill.name, !skill.enabled);
          }}
          className={clsx(
            "p-2 rounded-lg transition-colors",
            skill.enabled
              ? "bg-green-100 text-green-600"
              : "bg-gray-100 text-gray-400",
          )}
        >
          {skill.enabled ? <Play size={18} /> : <Pause size={18} />}
        </button>
      </div>
    </div>
  );

  const renderWorkflowCard = (workflow: WorkflowDef) => (
    <div key={workflow.id} className="bg-white rounded-lg shadow p-4">
      <div className="flex items-center justify-between">
        <div>
          <div className="flex items-center gap-2">
            <Workflow size={18} className="text-primary-500" />
            <span className="font-medium">{workflow.name}</span>
            <span
              className={clsx(
                "text-xs px-2 py-0.5 rounded-full",
                workflow.status === "active"
                  ? "bg-green-100 text-green-700"
                  : "bg-gray-100 text-gray-600",
              )}
            >
              {workflow.status}
            </span>
          </div>
          <p className="text-sm text-gray-600 mt-1">{workflow.description}</p>
          <p className="text-xs text-gray-400 mt-1">
            {workflow.steps.length} ステップ • 更新:{" "}
            {new Date(workflow.updated_at).toLocaleDateString("ja-JP")}
          </p>
        </div>

        <button className="flex items-center gap-1 px-3 py-1 bg-primary-100 text-primary-600 rounded-lg hover:bg-primary-200">
          編集 <ChevronRight size={16} />
        </button>
      </div>
    </div>
  );

  return (
    <div className="space-y-6">
      {/* ヘッダー */}
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-2xl font-bold text-gray-900">スキル管理</h1>
          <p className="text-gray-600 mt-1">スキルとワークフローを管理</p>
        </div>
        <button
          onClick={() => void fetchData()}
          className="flex items-center gap-2 px-4 py-2 bg-white border rounded-lg hover:bg-gray-50"
        >
          <RefreshCw size={16} />
          更新
        </button>
      </div>

      {/* タブ */}
      <div className="border-b border-gray-200">
        <div className="flex gap-4">
          <button
            onClick={() => setActiveTab("skills")}
            className={clsx(
              "px-4 py-2 font-medium border-b-2 transition-colors",
              activeTab === "skills"
                ? "border-primary-500 text-primary-600"
                : "border-transparent text-gray-500 hover:text-gray-700",
            )}
          >
            <div className="flex items-center gap-2">
              <Wrench size={18} />
              インストール済み
              <span className="text-xs bg-gray-200 px-2 rounded-full">
                {skills.length}
              </span>
            </div>
          </button>
          <button
            onClick={() => setActiveTab("mcp")}
            className={clsx(
              "px-4 py-2 font-medium border-b-2 transition-colors",
              activeTab === "mcp"
                ? "border-primary-500 text-primary-600"
                : "border-transparent text-gray-500 hover:text-gray-700",
            )}
          >
            <div className="flex items-center gap-2">
              <Code size={18} />
              MCP 管理
              <span className="text-xs bg-gray-200 px-2 rounded-full">
                {mcpServers.length}
              </span>
            </div>
          </button>
          <button
            onClick={() => setActiveTab("workflows")}
            className={clsx(
              "px-4 py-2 font-medium border-b-2 transition-colors",
              activeTab === "workflows"
                ? "border-primary-500 text-primary-600"
                : "border-transparent text-gray-500 hover:text-gray-700",
            )}
          >
            <div className="flex items-center gap-2">
              <Workflow size={18} />
              ワークフロー
              <span className="text-xs bg-gray-200 px-2 rounded-full">
                {workflows.length}
              </span>
            </div>
          </button>
          <button
            onClick={() => setActiveTab("generate")}
            className={clsx(
              "px-4 py-2 font-medium border-b-2 transition-colors",
              activeTab === "generate"
                ? "border-primary-500 text-primary-600"
                : "border-transparent text-gray-500 hover:text-gray-700",
            )}
          >
            <div className="flex items-center gap-2">
              <Sparkles size={18} />
              スキル生成
            </div>
          </button>
        </div>
      </div>

      {errorMessage && (
        <div className="rounded-lg border border-rose-200 bg-rose-50 px-4 py-3 text-sm text-rose-700">
          {errorMessage}
        </div>
      )}

      {statusMessage && (
        <div className="rounded-lg border border-emerald-200 bg-emerald-50 px-4 py-3 text-sm text-emerald-700">
          {statusMessage}
        </div>
      )}

      {/* コンテンツ */}
      {loading ? (
        <div className="flex items-center justify-center py-12">
          <Loader2 className="animate-spin text-primary-500" size={32} />
        </div>
      ) : activeTab === "skills" ? (
        <div className="grid grid-cols-2 gap-4">
          {/* スキル一覧 */}
          <div className="space-y-4">
            {skills.length === 0 ? (
              <div className="text-center py-12 text-gray-500 bg-white rounded-lg shadow">
                <Wrench size={48} className="mx-auto mb-4 text-gray-300" />
                <p>スキルがありません</p>
              </div>
            ) : (
              skills.map(renderSkillCard)
            )}
          </div>

          {/* スキル詳細・テスト */}
          <div className="bg-white rounded-lg shadow p-4">
            {selectedSkill ? (
              <div className="space-y-4">
                <div className="flex items-center gap-3">
                  <span className="text-2xl">
                    {categoryEmoji[selectedSkill.category] || "🔧"}
                  </span>
                  <div>
                    <h3 className="text-lg font-bold">{selectedSkill.name}</h3>
                    <p className="text-sm text-gray-500">
                      {selectedSkill.category}
                    </p>
                  </div>
                </div>

                <p className="text-gray-600">{selectedSkill.description}</p>

                <div className="flex gap-2">
                  <span
                    className={clsx(
                      "text-xs px-2 py-1 rounded",
                      riskColors[selectedSkill.risk_level],
                    )}
                  >
                    リスク: {selectedSkill.risk_level}
                  </span>
                  {selectedSkill.requires_confirmation && (
                    <span className="text-xs px-2 py-1 rounded bg-orange-100 text-orange-700">
                      要承認
                    </span>
                  )}
                </div>

                <hr />

                <div>
                  <h4 className="font-medium mb-2">テスト実行（ドライラン）</h4>
                  <textarea
                    value={testParams}
                    onChange={(e) => setTestParams(e.target.value)}
                    className="w-full border rounded-lg px-3 py-2 font-mono text-sm"
                    rows={4}
                    placeholder='{"path": "/tmp"}'
                  />
                  <button
                    onClick={() => void handleTestSkill()}
                    disabled={testing}
                    className="mt-2 flex items-center gap-2 px-4 py-2 bg-primary-500 text-white rounded-lg hover:bg-primary-600 disabled:opacity-50"
                  >
                    {testing ? (
                      <Loader2 className="animate-spin" size={16} />
                    ) : (
                      <Play size={16} />
                    )}
                    テスト
                  </button>
                </div>

                {testResult && (
                  <div
                    className={clsx(
                      "p-3 rounded-lg",
                      testResult.ok ? "bg-green-50" : "bg-red-50",
                    )}
                  >
                    <pre className="text-xs overflow-auto">
                      {JSON.stringify(testResult, null, 2)}
                    </pre>
                  </div>
                )}
              </div>
            ) : (
              <div className="text-center py-12 text-gray-500">
                <Code size={48} className="mx-auto mb-4 text-gray-300" />
                <p>スキルを選択して詳細を表示</p>
              </div>
            )}
          </div>
        </div>
      ) : activeTab === "mcp" ? (
        <McpManagementPanel
          servers={mcpServers}
          lazyLoading={lazyLoading}
          toolIndex={toolIndex}
          toolStats={toolStats}
          searchResult={toolSearchResult}
          onInstallServer={handleInstallMcpServer}
          onToggleServer={handleToggleMcpServer}
          onDeleteServer={handleDeleteMcpServer}
          onUpdateLazyLoading={handleUpdateLazyLoading}
          onSearchTools={handleSearchTools}
          onResetTools={handleResetTools}
        />
      ) : activeTab === "workflows" ? (
        <div className="space-y-4">
          <button className="flex items-center gap-2 px-4 py-2 bg-primary-500 text-white rounded-lg hover:bg-primary-600">
            <Plus size={18} />
            新規ワークフロー
          </button>

          {workflows.length === 0 ? (
            <div className="text-center py-12 text-gray-500 bg-white rounded-lg shadow">
              <Workflow size={48} className="mx-auto mb-4 text-gray-300" />
              <p>ワークフローがありません</p>
            </div>
          ) : (
            workflows.map(renderWorkflowCard)
          )}
        </div>
      ) : (
        <div className="bg-white rounded-lg shadow p-6">
          <div className="max-w-2xl mx-auto space-y-6">
            <div className="text-center">
              <Sparkles size={48} className="mx-auto mb-4 text-primary-500" />
              <h2 className="text-xl font-bold">自然言語でスキルを生成</h2>
              <p className="text-gray-600 mt-2">
                やりたいことを説明すると、AIがスキル定義を生成します
              </p>
            </div>

            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                スキルの説明
              </label>
              <textarea
                value={generatePrompt}
                onChange={(e) => setGeneratePrompt(e.target.value)}
                className="w-full border rounded-lg px-4 py-3"
                rows={4}
                placeholder="例: 指定したフォルダ内の古いファイルを見つけて削除候補をリストアップする"
              />
            </div>

            <button
              onClick={() => void handleGenerate()}
              disabled={generating || !generatePrompt.trim()}
              className="w-full flex items-center justify-center gap-2 px-4 py-3 bg-gradient-to-r from-primary-500 to-primary-600 text-white rounded-lg hover:from-primary-600 hover:to-primary-700 disabled:opacity-50"
            >
              {generating ? (
                <Loader2 className="animate-spin" size={20} />
              ) : (
                <Sparkles size={20} />
              )}
              スキルを生成
            </button>

            {generatedSkill && (
              <div className="bg-gray-50 rounded-lg p-4">
                <h3 className="font-medium mb-2">生成されたスキル定義</h3>
                <pre className="text-sm overflow-auto bg-white p-3 rounded border">
                  {JSON.stringify(generatedSkill, null, 2)}
                </pre>
                <button className="mt-3 px-4 py-2 bg-green-500 text-white rounded-lg hover:bg-green-600">
                  このスキルを保存
                </button>
              </div>
            )}
          </div>
        </div>
      )}
    </div>
  );
}
