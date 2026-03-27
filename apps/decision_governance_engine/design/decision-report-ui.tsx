import React, { useState } from "react";

const ReportPage = () => {
  const [activeTab, setActiveTab] = useState("summary");

  const report = {
    confidence: 87,
    conclusion:
      "B案（SaaS プロダクト開発）を選択し、6ヶ月以内にMVP検証を実施すべき",
    firstStep: "プロダクトチームとの30分キックオフMTGを明日設定する",
    keyRisks: [
      "既存顧客の離反リスク（対策：段階移行）",
      "開発リソースの競合",
      "市場タイミングの不確実性",
    ],
    dao: {
      problemType: "TRADE_OFF",
      essence:
        "短期の安定収益（受託）と長期の成長ポテンシャル（SaaS）のバランス判断",
      constraints: [
        "現金残高12ヶ月分",
        "開発者5名体制",
        "既存顧客10社のリテンション必須",
      ],
      assumptions: ["市場成長率は維持される", "コア技術の優位性は2年間有効"],
    },
    fa: {
      recommended: {
        name: "B案: SaaS MVP優先",
        description: "6ヶ月でMVPを開発し、既存顧客3社でパイロット検証",
        pros: [
          "長期的な収益の安定化",
          "スケーラビリティの確保",
          "企業価値向上",
        ],
        cons: ["短期キャッシュフロー圧迫", "既存業務との並行負荷"],
        probability: 0.72,
      },
      rejected: {
        name: "A案: 受託強化",
        reason: "短期安定するが、3年後の競争力低下リスクが高い",
      },
    },
    shu: {
      phases: [
        {
          num: 1,
          name: "準備",
          duration: "2週間",
          actions: ["チーム編成", "要件定義", "技術選定"],
        },
        {
          num: 2,
          name: "MVP開発",
          duration: "3ヶ月",
          actions: ["コア機能実装", "インフラ構築", "テスト"],
        },
        {
          num: 3,
          name: "パイロット",
          duration: "2ヶ月",
          actions: ["既存顧客3社導入", "フィードバック収集", "改善"],
        },
        {
          num: 4,
          name: "判断",
          duration: "2週間",
          actions: ["Go/No-Go判定", "スケール計画策定"],
        },
      ],
    },
    review: {
      verdict: "PASS",
      findings: [
        {
          severity: "WARNING",
          desc: "フェーズ2の期間が楽観的な可能性。バッファ2週間推奨。",
        },
        {
          severity: "INFO",
          desc: "撤退条件の明確化を推奨（パイロットNPS 30未満で中止）",
        },
      ],
    },
  };

  const tabs = [
    { id: "summary", name: "サマリー", icon: "📊" },
    { id: "dao", name: "道", icon: "🎯" },
    { id: "fa", name: "法", icon: "🛤️" },
    { id: "shu", name: "術", icon: "📋" },
    { id: "review", name: "検証", icon: "🔍" },
  ];

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
                Decision Report #2024-001
              </p>
            </div>
          </div>
          <div className="flex items-center gap-3">
            <button className="px-4 py-2 bg-slate-800 hover:bg-slate-700 rounded-lg text-sm flex items-center gap-2 transition-all">
              📄 PDF出力
            </button>
            <button className="px-4 py-2 bg-slate-800 hover:bg-slate-700 rounded-lg text-sm flex items-center gap-2 transition-all">
              🔄 再分析
            </button>
          </div>
        </div>
      </header>

      <main className="max-w-5xl mx-auto px-6 py-8">
        {/* Executive Summary Card */}
        <div className="bg-gradient-to-br from-[#12121a] to-[#1a1a24] rounded-2xl border border-white/5 p-8 mb-8 relative overflow-hidden">
          <div className="absolute top-0 right-0 w-64 h-64 bg-indigo-500/5 rounded-full blur-3xl -translate-y-1/2 translate-x-1/2" />

          <div className="relative">
            <div className="flex items-start justify-between mb-6">
              <div>
                <h2 className="text-xs text-slate-500 uppercase tracking-wider mb-1">
                  EXECUTIVE SUMMARY
                </h2>
                <div className="text-2xl font-bold">意思決定レポート</div>
              </div>
              <div className="text-right">
                <div className="text-xs text-slate-500 mb-1">信頼度スコア</div>
                <div className="text-3xl font-bold text-emerald-400">
                  {report.confidence}%
                </div>
              </div>
            </div>

            {/* Main Conclusion */}
            <div className="bg-[#0a0a0f] rounded-xl p-6 mb-6 border border-indigo-500/20">
              <div className="flex items-center gap-2 text-indigo-400 text-sm mb-2">
                <span>💡</span> 結論
              </div>
              <p className="text-lg font-medium">{report.conclusion}</p>
            </div>

            {/* First Step */}
            <div className="bg-emerald-500/5 rounded-xl p-5 mb-6 border border-emerald-500/20">
              <div className="flex items-center gap-2 text-emerald-400 text-sm mb-2">
                <span>🎯</span> 最初の一歩（明日実行可能）
              </div>
              <p className="font-medium">{report.firstStep}</p>
            </div>

            {/* Key Risks */}
            <div>
              <div className="flex items-center gap-2 text-amber-400 text-sm mb-3">
                <span>⚠️</span> 主要リスク
              </div>
              <div className="grid grid-cols-1 gap-2">
                {report.keyRisks.map((risk, i) => (
                  <div
                    key={i}
                    className="flex items-center gap-3 text-sm text-slate-400"
                  >
                    <span className="w-1.5 h-1.5 rounded-full bg-amber-500" />
                    {risk}
                  </div>
                ))}
              </div>
            </div>
          </div>
        </div>

        {/* Tab Navigation */}
        <div className="flex gap-2 mb-6 border-b border-white/5 pb-4">
          {tabs.map((tab) => (
            <button
              key={tab.id}
              onClick={() => setActiveTab(tab.id)}
              className={`px-4 py-2 rounded-lg text-sm font-medium transition-all flex items-center gap-2 ${
                activeTab === tab.id
                  ? "bg-indigo-500/10 text-indigo-400 border border-indigo-500/30"
                  : "text-slate-400 hover:text-white hover:bg-slate-800"
              }`}
            >
              <span>{tab.icon}</span>
              {tab.name}
            </button>
          ))}
        </div>

        {/* Tab Content */}
        <div className="bg-[#12121a] rounded-xl border border-white/5 p-6">
          {activeTab === "summary" && (
            <div className="text-center py-8 text-slate-500">
              上記のエグゼクティブサマリーが全体概要です。
              <br />
              各タブで詳細を確認できます。
            </div>
          )}

          {activeTab === "dao" && (
            <div className="space-y-6">
              <div>
                <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
                  <span className="w-8 h-8 rounded-lg bg-indigo-500/10 flex items-center justify-center">
                    🎯
                  </span>
                  道 / 本質分析
                </h3>
              </div>

              <div className="grid grid-cols-2 gap-4">
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-1">問題タイプ</div>
                  <div className="px-3 py-1 bg-indigo-500/10 text-indigo-400 rounded inline-block text-sm">
                    {report.dao.problemType}
                  </div>
                </div>
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-1">
                    本質（一文）
                  </div>
                  <div className="text-sm">{report.dao.essence}</div>
                </div>
              </div>

              <div className="bg-[#0a0a0f] rounded-lg p-4">
                <div className="text-xs text-slate-500 mb-3">不可変制約</div>
                <div className="space-y-2">
                  {report.dao.constraints.map((c, i) => (
                    <div key={i} className="flex items-center gap-2 text-sm">
                      <span className="text-red-400">🔒</span> {c}
                    </div>
                  ))}
                </div>
              </div>

              <div className="bg-[#0a0a0f] rounded-lg p-4">
                <div className="text-xs text-slate-500 mb-3">隠れた前提</div>
                <div className="space-y-2">
                  {report.dao.assumptions.map((a, i) => (
                    <div
                      key={i}
                      className="flex items-center gap-2 text-sm text-slate-400"
                    >
                      <span>💭</span> {a}
                    </div>
                  ))}
                </div>
              </div>
            </div>
          )}

          {activeTab === "fa" && (
            <div className="space-y-6">
              <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
                <span className="w-8 h-8 rounded-lg bg-violet-500/10 flex items-center justify-center">
                  🛤️
                </span>
                法 / 戦略選定
              </h3>

              {/* Recommended */}
              <div className="bg-emerald-500/5 rounded-xl p-5 border border-emerald-500/20">
                <div className="flex items-center justify-between mb-3">
                  <div className="flex items-center gap-2">
                    <span className="text-emerald-400">✓</span>
                    <span className="font-semibold">
                      {report.fa.recommended.name}
                    </span>
                  </div>
                  <span className="text-emerald-400 text-sm">
                    成功確率{" "}
                    {Math.round(report.fa.recommended.probability * 100)}%
                  </span>
                </div>
                <p className="text-sm text-slate-400 mb-4">
                  {report.fa.recommended.description}
                </p>

                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <div className="text-xs text-emerald-400 mb-2">
                      メリット
                    </div>
                    {report.fa.recommended.pros.map((p, i) => (
                      <div
                        key={i}
                        className="text-sm text-slate-400 flex items-center gap-2 mb-1"
                      >
                        <span className="text-emerald-400">+</span> {p}
                      </div>
                    ))}
                  </div>
                  <div>
                    <div className="text-xs text-amber-400 mb-2">
                      デメリット
                    </div>
                    {report.fa.recommended.cons.map((c, i) => (
                      <div
                        key={i}
                        className="text-sm text-slate-400 flex items-center gap-2 mb-1"
                      >
                        <span className="text-amber-400">-</span> {c}
                      </div>
                    ))}
                  </div>
                </div>
              </div>

              {/* Rejected */}
              <div className="bg-[#0a0a0f] rounded-xl p-5 border border-red-500/10 opacity-60">
                <div className="flex items-center gap-2 mb-2">
                  <span className="text-red-400">✕</span>
                  <span className="font-semibold text-slate-500">
                    {report.fa.rejected.name}
                  </span>
                  <span className="text-xs text-red-400 px-2 py-0.5 bg-red-500/10 rounded">
                    不推奨
                  </span>
                </div>
                <p className="text-sm text-slate-600">
                  {report.fa.rejected.reason}
                </p>
              </div>
            </div>
          )}

          {activeTab === "shu" && (
            <div className="space-y-6">
              <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
                <span className="w-8 h-8 rounded-lg bg-blue-500/10 flex items-center justify-center">
                  📋
                </span>
                術 / 実行計画
              </h3>

              <div className="relative">
                {report.shu.phases.map((phase, i) => (
                  <div key={i} className="flex gap-4 mb-6 last:mb-0">
                    <div className="flex flex-col items-center">
                      <div className="w-10 h-10 rounded-full bg-blue-500/10 border-2 border-blue-500/30 flex items-center justify-center text-blue-400 font-semibold">
                        {phase.num}
                      </div>
                      {i < report.shu.phases.length - 1 && (
                        <div className="w-0.5 h-full bg-blue-500/20 mt-2" />
                      )}
                    </div>
                    <div className="flex-1 bg-[#0a0a0f] rounded-lg p-4">
                      <div className="flex items-center justify-between mb-2">
                        <span className="font-medium">{phase.name}</span>
                        <span className="text-xs text-slate-500 px-2 py-1 bg-slate-800 rounded">
                          {phase.duration}
                        </span>
                      </div>
                      <div className="flex flex-wrap gap-2">
                        {phase.actions.map((action, j) => (
                          <span
                            key={j}
                            className="text-xs text-slate-400 px-2 py-1 bg-slate-800/50 rounded"
                          >
                            {action}
                          </span>
                        ))}
                      </div>
                    </div>
                  </div>
                ))}
              </div>
            </div>
          )}

          {activeTab === "review" && (
            <div className="space-y-6">
              <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
                <span className="w-8 h-8 rounded-lg bg-amber-500/10 flex items-center justify-center">
                  🔍
                </span>
                検証 / ReviewAgent
              </h3>

              <div className="flex items-center gap-3 mb-6">
                <span className="text-sm text-slate-400">判定結果:</span>
                <span
                  className={`px-3 py-1 rounded-lg text-sm font-medium ${
                    report.review.verdict === "PASS"
                      ? "bg-emerald-500/10 text-emerald-400"
                      : "bg-red-500/10 text-red-400"
                  }`}
                >
                  {report.review.verdict}
                </span>
              </div>

              <div className="space-y-3">
                {report.review.findings.map((finding, i) => (
                  <div
                    key={i}
                    className={`rounded-lg p-4 border ${
                      finding.severity === "WARNING"
                        ? "bg-amber-500/5 border-amber-500/20"
                        : "bg-blue-500/5 border-blue-500/20"
                    }`}
                  >
                    <div className="flex items-center gap-2 mb-2">
                      <span
                        className={`text-xs px-2 py-0.5 rounded ${
                          finding.severity === "WARNING"
                            ? "bg-amber-500/10 text-amber-400"
                            : "bg-blue-500/10 text-blue-400"
                        }`}
                      >
                        {finding.severity}
                      </span>
                    </div>
                    <p className="text-sm text-slate-400">{finding.desc}</p>
                  </div>
                ))}
              </div>
            </div>
          )}
        </div>

        {/* Signature Section */}
        <div className="mt-8 bg-[#12121a] rounded-xl border border-white/5 p-6">
          <div className="flex items-center justify-between">
            <div>
              <div className="text-sm text-slate-500 mb-1">
                このレポートに基づいて意思決定を行う場合
              </div>
              <div className="text-lg font-medium">署名して確定</div>
            </div>
            <button className="px-6 py-3 bg-gradient-to-r from-indigo-600 to-violet-600 hover:from-indigo-500 hover:to-violet-500 rounded-xl font-medium transition-all shadow-lg shadow-indigo-500/25 flex items-center gap-2">
              ✍️ 電子署名
            </button>
          </div>
        </div>
      </main>
    </div>
  );
};

export default ReportPage;
