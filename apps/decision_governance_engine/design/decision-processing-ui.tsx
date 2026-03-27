import React, { useState, useEffect } from "react";

const ProcessingPage = () => {
  const [agents, setAgents] = useState([
    {
      id: "dao",
      name: "道",
      label: "本質分析",
      status: "running",
      progress: 0,
      message: "問題の本質を抽出中...",
      result: null,
    },
    {
      id: "fa",
      name: "法",
      label: "戦略選定",
      status: "waiting",
      progress: 0,
      message: "",
      result: null,
    },
    {
      id: "shu",
      name: "術",
      label: "実行計画",
      status: "waiting",
      progress: 0,
      message: "",
      result: null,
    },
    {
      id: "qi",
      name: "器",
      label: "技術実装",
      status: "waiting",
      progress: 0,
      message: "",
      result: null,
    },
    {
      id: "review",
      name: "検証",
      label: "最終検証",
      status: "waiting",
      progress: 0,
      message: "",
      result: null,
    },
  ]);

  // シミュレーション用
  useEffect(() => {
    const sequence = [
      {
        delay: 500,
        updates: { dao: { progress: 30, message: "問題タイプを分類中..." } },
      },
      {
        delay: 1500,
        updates: { dao: { progress: 70, message: "不可変制約を特定中..." } },
      },
      {
        delay: 2500,
        updates: {
          dao: {
            progress: 100,
            status: "completed",
            message: "完了",
            result: {
              type: "TRADE_OFF",
              essence: "短期収益と長期成長のバランス判断",
            },
          },
        },
      },
      {
        delay: 3000,
        updates: {
          fa: {
            status: "running",
            progress: 20,
            message: "戦略オプションを評価中...",
          },
        },
      },
      {
        delay: 4000,
        updates: { fa: { progress: 60, message: "推奨パスを選定中..." } },
      },
      {
        delay: 5500,
        updates: {
          fa: {
            progress: 100,
            status: "completed",
            message: "完了",
            result: { paths: 2, recommended: "B案" },
          },
        },
      },
      {
        delay: 6000,
        updates: {
          shu: {
            status: "running",
            progress: 30,
            message: "フェーズ分解中...",
          },
        },
      },
      {
        delay: 7500,
        updates: {
          shu: { progress: 100, status: "completed", message: "完了" },
        },
      },
      {
        delay: 8000,
        updates: {
          qi: {
            status: "running",
            progress: 40,
            message: "実装方針を策定中...",
          },
        },
      },
      {
        delay: 9500,
        updates: {
          qi: { progress: 100, status: "completed", message: "完了" },
        },
      },
      {
        delay: 10000,
        updates: {
          review: {
            status: "running",
            progress: 50,
            message: "全体整合性を検証中...",
          },
        },
      },
      {
        delay: 12000,
        updates: {
          review: { progress: 100, status: "completed", message: "検証完了" },
        },
      },
    ];

    const timers = sequence.map(({ delay, updates }) =>
      setTimeout(() => {
        setAgents((prev) =>
          prev.map((agent) => {
            const update = updates[agent.id];
            return update ? { ...agent, ...update } : agent;
          }),
        );
      }, delay),
    );

    return () => timers.forEach(clearTimeout);
  }, []);

  const getStatusIcon = (status) => {
    switch (status) {
      case "completed":
        return "✓";
      case "running":
        return "⟳";
      case "waiting":
        return "○";
      case "failed":
        return "✕";
      default:
        return "○";
    }
  };

  const getStatusColor = (status) => {
    switch (status) {
      case "completed":
        return "text-emerald-400";
      case "running":
        return "text-indigo-400";
      case "waiting":
        return "text-slate-600";
      case "failed":
        return "text-red-400";
      default:
        return "text-slate-600";
    }
  };

  const allCompleted = agents.every((a) => a.status === "completed");

  return (
    <div className="min-h-screen bg-[#0a0a0f] text-white">
      {/* Header */}
      <header className="border-b border-white/5 px-6 py-4">
        <div className="max-w-6xl mx-auto flex items-center justify-between">
          <div className="flex items-center gap-3">
            <div className="w-10 h-10 rounded-xl bg-gradient-to-br from-indigo-500 to-violet-600 flex items-center justify-center">
              <span className="text-xl">⚡</span>
            </div>
            <div>
              <h1 className="font-semibold text-lg">Decision Agent</h1>
              <p className="text-xs text-slate-500">
                Enterprise Decision Platform
              </p>
            </div>
          </div>
        </div>
      </header>

      {/* Main */}
      <main className="max-w-3xl mx-auto px-6 py-12">
        <div className="text-center mb-10">
          <div
            className={`inline-flex items-center gap-2 px-4 py-2 rounded-full mb-4 ${
              allCompleted
                ? "bg-emerald-500/10 text-emerald-400"
                : "bg-indigo-500/10 text-indigo-400"
            }`}
          >
            {allCompleted ? (
              <>✓ 分析完了</>
            ) : (
              <>
                <div className="w-4 h-4 border-2 border-indigo-400/30 border-t-indigo-400 rounded-full animate-spin" />
                分析進行中
              </>
            )}
          </div>
          <h2 className="text-2xl font-bold mb-2">
            {allCompleted
              ? "決策レポートの準備ができました"
              : "意思決定を構造化しています..."}
          </h2>
          <p className="text-slate-400">
            {allCompleted
              ? "全ての分析が完了しました。結果を確認してください。"
              : "道・法・術・器のフレームワークで多角的に分析中"}
          </p>
        </div>

        {/* Agent Progress Cards */}
        <div className="space-y-4 mb-8">
          {agents.slice(0, 4).map((agent, index) => (
            <div
              key={agent.id}
              className={`bg-[#12121a] rounded-xl border transition-all duration-500 ${
                agent.status === "running"
                  ? "border-indigo-500/30 shadow-lg shadow-indigo-500/10"
                  : agent.status === "completed"
                    ? "border-emerald-500/20"
                    : "border-white/5"
              }`}
            >
              <div className="p-5">
                <div className="flex items-center justify-between mb-3">
                  <div className="flex items-center gap-3">
                    <div
                      className={`w-10 h-10 rounded-lg flex items-center justify-center text-xl ${
                        agent.status === "completed"
                          ? "bg-emerald-500/10"
                          : agent.status === "running"
                            ? "bg-indigo-500/10"
                            : "bg-slate-800"
                      }`}
                    >
                      {agent.status === "completed"
                        ? "✓"
                        : ["🎯", "🛤️", "📋", "🔧"][index]}
                    </div>
                    <div>
                      <div className="font-medium">
                        {agent.name}{" "}
                        <span className="text-slate-500 font-normal">
                          / {agent.label}
                        </span>
                      </div>
                      <div className="text-xs text-slate-500">
                        {agent.message ||
                          (agent.status === "waiting" ? "待機中" : "")}
                      </div>
                    </div>
                  </div>
                  <span className={`text-sm ${getStatusColor(agent.status)}`}>
                    {agent.status === "completed"
                      ? "完了"
                      : agent.status === "running"
                        ? `${agent.progress}%`
                        : ""}
                  </span>
                </div>

                {/* Progress Bar */}
                <div className="h-1.5 bg-slate-800 rounded-full overflow-hidden">
                  <div
                    className={`h-full transition-all duration-500 rounded-full ${
                      agent.status === "completed"
                        ? "bg-emerald-500"
                        : agent.status === "running"
                          ? "bg-indigo-500"
                          : "bg-slate-700"
                    }`}
                    style={{ width: `${agent.progress}%` }}
                  />
                </div>

                {/* Result Preview */}
                {agent.result && (
                  <div className="mt-3 pt-3 border-t border-white/5">
                    {agent.id === "dao" && (
                      <div className="flex items-center gap-4 text-sm">
                        <span className="px-2 py-1 bg-indigo-500/10 text-indigo-400 rounded text-xs">
                          {agent.result.type}
                        </span>
                        <span className="text-slate-400">
                          {agent.result.essence}
                        </span>
                      </div>
                    )}
                    {agent.id === "fa" && (
                      <div className="text-sm text-slate-400">
                        {agent.result.paths}つの戦略を評価 →{" "}
                        <span className="text-emerald-400">
                          {agent.result.recommended}を推奨
                        </span>
                      </div>
                    )}
                  </div>
                )}
              </div>
            </div>
          ))}
        </div>

        {/* Review Agent (Special) */}
        <div
          className={`bg-[#12121a] rounded-xl border-2 border-dashed transition-all ${
            agents[4].status === "completed"
              ? "border-emerald-500/30"
              : agents[4].status === "running"
                ? "border-amber-500/30"
                : "border-white/10"
          }`}
        >
          <div className="p-5">
            <div className="flex items-center justify-between">
              <div className="flex items-center gap-3">
                <div
                  className={`w-10 h-10 rounded-lg flex items-center justify-center text-xl ${
                    agents[4].status === "completed"
                      ? "bg-emerald-500/10"
                      : agents[4].status === "running"
                        ? "bg-amber-500/10"
                        : "bg-slate-800"
                  }`}
                >
                  🔍
                </div>
                <div>
                  <div className="font-medium">
                    検証{" "}
                    <span className="text-slate-500 font-normal">
                      / ReviewAgent
                    </span>
                  </div>
                  <div className="text-xs text-slate-500">
                    {agents[4].message || "全層の整合性・リスクを検証"}
                  </div>
                </div>
              </div>
              <span className={`text-sm ${getStatusColor(agents[4].status)}`}>
                {agents[4].status === "completed"
                  ? "✓ PASS"
                  : agents[4].status === "running"
                    ? `${agents[4].progress}%`
                    : ""}
              </span>
            </div>

            <div className="h-1.5 bg-slate-800 rounded-full overflow-hidden mt-3">
              <div
                className={`h-full transition-all duration-500 rounded-full ${
                  agents[4].status === "completed"
                    ? "bg-emerald-500"
                    : agents[4].status === "running"
                      ? "bg-amber-500"
                      : "bg-slate-700"
                }`}
                style={{ width: `${agents[4].progress}%` }}
              />
            </div>
          </div>
        </div>

        {/* Actions */}
        <div className="flex justify-center gap-4 mt-8">
          {allCompleted ? (
            <button className="px-8 py-3 bg-gradient-to-r from-indigo-600 to-violet-600 hover:from-indigo-500 hover:to-violet-500 rounded-xl font-medium transition-all shadow-lg shadow-indigo-500/25">
              📄 決策レポートを表示
            </button>
          ) : (
            <button className="px-6 py-3 bg-slate-800 hover:bg-slate-700 rounded-xl text-slate-400 transition-all">
              キャンセル
            </button>
          )}
        </div>

        {/* Tip */}
        {!allCompleted && (
          <div className="mt-8 text-center">
            <p className="text-sm text-slate-600">
              💡 各段階で深層分析を行っています。通常2〜3分で完了します。
            </p>
          </div>
        )}
      </main>
    </div>
  );
};

export default ProcessingPage;
