import { useEffect, useState } from "react";
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
 * スキル管理ページ
 *
 * スキル一覧、ワークフロー管理、自然言語スキル生成
 */
export default function SkillsManager() {
  const [skills, setSkills] = useState<Skill[]>([]);
  const [workflows, setWorkflows] = useState<WorkflowDef[]>([]);
  const [loading, setLoading] = useState(true);
  const [activeTab, setActiveTab] = useState<
    "skills" | "workflows" | "generate"
  >("skills");
  const [generatePrompt, setGeneratePrompt] = useState("");
  const [generatedSkill, setGeneratedSkill] = useState<Record<
    string,
    unknown
  > | null>(null);
  const [generating, setGenerating] = useState(false);
  const [selectedSkill, setSelectedSkill] = useState<Skill | null>(null);
  const [testParams, setTestParams] = useState("{}");
  const [testResult, setTestResult] = useState<Record<string, unknown> | null>(
    null,
  );
  const [testing, setTesting] = useState(false);

  useEffect(() => {
    fetchData();
  }, []);

  const fetchData = async () => {
    setLoading(true);
    try {
      const [skillsRes, workflowsRes] = await Promise.all([
        fetch("/api/skills"),
        fetch("/api/workflows"),
      ]);

      if (skillsRes.ok) {
        const data = await skillsRes.json();
        setSkills(data.skills || []);
      }
      if (workflowsRes.ok) {
        const data = await workflowsRes.json();
        setWorkflows(data.workflows || []);
      }
    } catch (error) {
      console.error("Fetch error:", error);
    } finally {
      setLoading(false);
    }
  };

  const handleToggleSkill = async (skillName: string, enabled: boolean) => {
    try {
      await fetch(
        `/api/skills/${skillName}/${enabled ? "enable" : "disable"}`,
        {
          method: "POST",
        },
      );
      fetchData();
    } catch (error) {
      console.error("Toggle error:", error);
    }
  };

  const handleGenerate = async () => {
    if (!generatePrompt.trim()) return;

    setGenerating(true);
    setGeneratedSkill(null);

    try {
      const response = await fetch("/api/skills/generate", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ description: generatePrompt }),
      });

      if (response.ok) {
        const data = await response.json();
        setGeneratedSkill(data);
      }
    } catch (error) {
      console.error("Generate error:", error);
    } finally {
      setGenerating(false);
    }
  };

  const handleTestSkill = async () => {
    if (!selectedSkill) return;

    setTesting(true);
    setTestResult(null);

    try {
      const params = JSON.parse(testParams);
      const response = await fetch(`/api/skills/${selectedSkill.name}/call`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ params, dry_run: true }),
      });

      if (response.ok) {
        const data = await response.json();
        setTestResult(data);
      }
    } catch (error) {
      setTestResult({ error: String(error) });
    } finally {
      setTesting(false);
    }
  };

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
            handleToggleSkill(skill.name, !skill.enabled);
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
          onClick={fetchData}
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
                    onClick={handleTestSkill}
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
              onClick={handleGenerate}
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
