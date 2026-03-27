/**
 * 入力画面コンポーネント.
 *
 * 目的: 意思決定の質問と制約条件を入力
 * API対接: POST /api/decision (REST)
 * 設計参考: design/decision-input-ui.tsx
 */

import React, { useState, useCallback } from "react";
import { useDecisionStore } from "../store/useDecisionStore";
import { useAuthStore } from "../store/useAuthStore";
import { SettingsModal } from "./SettingsModal";
import { useI18n } from "../i18n";

const MIN_QUESTION_LENGTH = 15;

export const DecisionInputPage: React.FC = () => {
  const { t } = useI18n();
  const {
    question,
    constraints,
    stakeholders,
    setQuestion,
    setConstraints,
    setStakeholders,
    setPage,
    reset,
  } = useDecisionStore();
  const { user, performLogout } = useAuthStore();

  const [isSubmitting, setIsSubmitting] = useState(false);
  const [rejection, setRejection] = useState<{
    category: string;
    message: string;
  } | null>(null);
  const [apiError, setApiError] = useState<string | null>(null);
  const [techInput, setTechInput] = useState("");
  const [regInput, setRegInput] = useState("");
  const [isSettingsOpen, setIsSettingsOpen] = useState(false);

  /** 即時拒否パターン（ローカライズ対応） */
  const rejectPatterns = [
    {
      regex: /(天気|気温|weather|何時)/i,
      message: t("input.reject_weather"),
      category: t("input.reject_cat_fact"),
    },
    {
      regex: /(このシステム|このAI|どうやって作|仕組み)/i,
      message: t("input.reject_system"),
      category: t("input.reject_cat_system"),
    },
    {
      regex: /(.+)(とは何|って何|とは？)/i,
      message: t("input.reject_definition"),
      category: t("input.reject_cat_definition"),
    },
    {
      regex: /^(こんにちは|hello|hi|ありがとう)/i,
      message: t("input.reject_chat"),
      category: t("input.reject_cat_chat"),
    },
    {
      regex: /(コード.*書いて|プログラム.*作)/i,
      message: t("input.reject_code"),
      category: t("input.reject_cat_code"),
    },
  ];

  /** ログアウト */
  const handleLogout = useCallback(async () => {
    await performLogout();
    reset();
  }, [performLogout, reset]);

  /** タグ追加 */
  const addTag = useCallback(
    (type: "technical" | "regulatory", value: string) => {
      if (value.trim()) {
        setConstraints({
          [type]: [...constraints[type], value.trim()],
        });
        if (type === "technical") setTechInput("");
        if (type === "regulatory") setRegInput("");
      }
    },
    [constraints, setConstraints],
  );

  /** タグ削除 */
  const removeTag = useCallback(
    (type: "technical" | "regulatory", index: number) => {
      setConstraints({
        [type]: constraints[type].filter((_, i) => i !== index),
      });
    },
    [constraints, setConstraints],
  );

  /** SSE モードで送信 */
  const handleSubmitWithStream = () => {
    if (question.length < MIN_QUESTION_LENGTH || isSubmitting) {
      return;
    }

    // 補足提案を表示（送信は継続）
    const rejectResult = (() => {
      for (const pattern of rejectPatterns) {
        if (pattern.regex.test(question)) {
          return { category: pattern.category, message: pattern.message };
        }
      }
      return null;
    })();
    setRejection(rejectResult);
    setApiError(null);
    setIsSubmitting(true);

    // 進捗画面へ遷移（SSE接続は進捗画面で開始）
    setPage("processing");
  };

  const isValid = question.length >= MIN_QUESTION_LENGTH;

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
              <h1 className="font-semibold text-lg">{t("input.title")}</h1>
              <p className="text-xs text-slate-500">{t("input.subtitle")}</p>
            </div>
          </div>

          {/* ユーザーメニュー */}
          {user && (
            <div className="flex items-center gap-3">
              {/* 知識管理ボタン */}
              <button
                onClick={() => setPage("knowledge")}
                className="px-3 py-2 bg-slate-800 hover:bg-slate-700 rounded-lg text-sm flex items-center gap-2 transition-all text-slate-300 hover:text-white"
                title={t("nav.knowledge_mgmt")}
              >
                <span aria-hidden="true">&#128218;</span>
                {t("nav.knowledge_mgmt")}
              </button>
              {/* 履歴ボタン */}
              <button
                onClick={() => setPage("history")}
                className="px-3 py-2 bg-slate-800 hover:bg-slate-700 rounded-lg text-sm flex items-center gap-2 transition-all text-slate-300 hover:text-white"
                title={t("input.history_tooltip")}
              >
                <span aria-hidden="true">&#128196;</span>
                {t("input.history")}
              </button>
              {/* 設定ボタン */}
              <button
                onClick={() => setIsSettingsOpen(true)}
                className="p-2 hover:bg-slate-800 rounded-lg transition-colors text-slate-400 hover:text-white"
                title={t("input.settings")}
              >
                ⚙️
              </button>
              <div className="text-right">
                <div className="text-sm font-medium text-white">
                  {user.display_name}
                </div>
                <div className="text-xs text-slate-500">{user.department}</div>
              </div>
              <button
                onClick={handleLogout}
                className="p-2 hover:bg-slate-800 rounded-lg transition-colors text-slate-400 hover:text-white"
                title={t("common.logout")}
              >
                🚪
              </button>
            </div>
          )}
        </div>
      </header>

      {/* Main */}
      <main className="max-w-3xl mx-auto px-6 py-12">
        <div className="text-center mb-12">
          <h2 className="text-3xl font-bold mb-3 bg-gradient-to-r from-white to-slate-400 bg-clip-text text-transparent">
            <span aria-hidden="true">⚖️</span>
            {t("input.hero_title")}
          </h2>
          <p className="text-slate-400">{t("input.hero_desc")}</p>
        </div>

        {/* Input Card - 続きは str-replace-editor で追加 */}
        <div className="bg-[#12121a] rounded-2xl border border-white/5 p-8">
          {/* エラー表示 */}
          {apiError && (
            <div className="mb-6 bg-orange-500/5 border border-orange-500/20 rounded-xl p-4">
              <span className="text-orange-400">
                <span aria-hidden="true">🔌</span> {t("input.api_error")}{" "}
                {apiError}
              </span>
            </div>
          )}

          {rejection && (
            <div className="mb-6 bg-amber-500/5 border border-amber-500/20 rounded-xl p-5">
              <div className="flex items-start gap-3">
                <div className="w-10 h-10 rounded-lg bg-amber-500/10 flex items-center justify-center flex-shrink-0">
                  <span className="text-amber-400">💡</span>
                </div>
                <div className="flex-1">
                  <div className="flex items-center gap-2 mb-1">
                    <span className="text-amber-300 font-medium">
                      {t("input.reject_title")}
                    </span>
                    <span className="text-xs px-2 py-0.5 bg-amber-500/10 text-amber-300 rounded">
                      {rejection.category}
                    </span>
                  </div>
                  <p className="text-sm text-slate-400 mb-3">
                    {rejection.message}
                  </p>
                  <div className="bg-[#0a0a0f] rounded-lg p-3">
                    <div className="text-xs text-slate-500 mb-2">
                      <span aria-hidden="true">✅</span>{" "}
                      {t("input.accept_examples_title")}
                    </div>
                    <ul className="text-xs text-slate-400 space-y-1">
                      <li>• {t("input.accept_example_1")}</li>
                      <li>• {t("input.accept_example_2")}</li>
                      <li>• {t("input.accept_example_3")}</li>
                    </ul>
                  </div>
                </div>
                <button
                  onClick={() => setRejection(null)}
                  className="text-slate-500 hover:text-white transition-colors"
                  aria-label={t("common.close")}
                >
                  ✕
                </button>
              </div>
            </div>
          )}

          {/* 質問入力 */}
          <div className="mb-8">
            <label className="block text-sm font-medium text-slate-300 mb-3">
              {t("input.question_label")}
            </label>
            <textarea
              value={question}
              onChange={(e) => setQuestion(e.target.value)}
              placeholder={t("input.question_placeholder")}
              className="w-full h-40 bg-[#0a0a0f] border border-white/10 rounded-xl px-4 py-3 text-white placeholder-slate-600 resize-none focus:outline-none focus:border-indigo-500/50 focus:ring-2 focus:ring-indigo-500/20 transition-all"
            />
            <div className="flex justify-between mt-2">
              <span className="text-xs text-slate-600">
                {question.length < MIN_QUESTION_LENGTH
                  ? t("input.min_chars")
                  : t("input.input_ok")}
              </span>
              <span className="text-xs text-slate-600">
                {t("input.chars_count").replaceAll(
                  "{count}",
                  String(question.length),
                )}
              </span>
            </div>
          </div>

          {/* 制約条件セクション */}
          <div className="mb-8">
            <h3 className="text-sm font-medium text-slate-300 mb-4 flex items-center gap-2">
              <span
                aria-hidden="true"
                className="w-5 h-5 rounded bg-slate-800 flex items-center justify-center text-xs"
              >
                ⚙️
              </span>
              {t("input.constraints_title")}
            </h3>

            {/* 主要制約（3カラム） */}
            <div className="grid grid-cols-3 gap-4 mb-6">
              <div className="bg-[#0a0a0f] rounded-xl p-4 border border-white/5">
                <div className="flex items-center gap-2 mb-2">
                  <span aria-hidden="true">💰</span>
                  <span className="text-xs text-slate-400">
                    {t("input.budget")}
                  </span>
                </div>
                <div className="flex items-center">
                  <span className="text-slate-600 mr-1">
                    {t("input.currency_prefix")}
                  </span>
                  <input
                    type="text"
                    value={constraints.budget}
                    onChange={(e) => setConstraints({ budget: e.target.value })}
                    placeholder="500"
                    className="w-full bg-transparent text-white text-lg font-medium focus:outline-none"
                  />
                  <span className="text-slate-600 ml-1">
                    {t("input.currency_suffix")}
                  </span>
                </div>
              </div>

              <div className="bg-[#0a0a0f] rounded-xl p-4 border border-white/5">
                <div className="flex items-center gap-2 mb-2">
                  <span aria-hidden="true">⏱️</span>
                  <span className="text-xs text-slate-400">
                    {t("input.deadline")}
                  </span>
                </div>
                <div className="flex items-center">
                  <input
                    type="text"
                    value={constraints.timeline}
                    onChange={(e) =>
                      setConstraints({ timeline: e.target.value })
                    }
                    placeholder="6"
                    className="w-full bg-transparent text-white text-lg font-medium focus:outline-none"
                  />
                  <span className="text-slate-600 ml-1">
                    {t("input.months_suffix")}
                  </span>
                </div>
              </div>

              <div className="bg-[#0a0a0f] rounded-xl p-4 border border-white/5">
                <div className="flex items-center gap-2 mb-2">
                  <span aria-hidden="true">👥</span>
                  <span className="text-xs text-slate-400">
                    {t("input.team")}
                  </span>
                </div>
                <div className="flex items-center">
                  <input
                    type="text"
                    value={constraints.team}
                    onChange={(e) => setConstraints({ team: e.target.value })}
                    placeholder="5"
                    className="w-full bg-transparent text-white text-lg font-medium focus:outline-none"
                  />
                  <span className="text-slate-600 ml-1">
                    {t("input.people_suffix")}
                  </span>
                </div>
              </div>
            </div>

            {/* タグ入力（2カラム） */}
            <div className="grid grid-cols-2 gap-4">
              <div className="bg-[#0a0a0f] rounded-xl p-4 border border-white/5">
                <div className="flex items-center gap-2 mb-3">
                  <span aria-hidden="true">🔧</span>
                  <span className="text-xs text-slate-400">
                    {t("input.technical_constraints")}
                  </span>
                </div>
                <div className="flex flex-wrap gap-2 mb-2">
                  {constraints.technical.map((tag, i) => (
                    <span
                      key={i}
                      className="px-2 py-1 bg-indigo-500/20 text-indigo-300 rounded-lg text-xs flex items-center gap-1"
                    >
                      {tag}
                      <button
                        onClick={() => removeTag("technical", i)}
                        className="hover:text-white"
                      >
                        ×
                      </button>
                    </span>
                  ))}
                </div>
                <input
                  type="text"
                  value={techInput}
                  onChange={(e) => setTechInput(e.target.value)}
                  onKeyDown={(e) => {
                    if (e.key === "Enter") {
                      e.preventDefault();
                      addTag("technical", techInput);
                    }
                  }}
                  placeholder={t("input.technical_placeholder")}
                  className="w-full bg-transparent text-sm text-white focus:outline-none placeholder-slate-600"
                />
              </div>

              <div className="bg-[#0a0a0f] rounded-xl p-4 border border-white/5">
                <div className="flex items-center gap-2 mb-3">
                  <span aria-hidden="true">📋</span>
                  <span className="text-xs text-slate-400">
                    {t("input.regulatory_constraints")}
                  </span>
                </div>
                <div className="flex flex-wrap gap-2 mb-2">
                  {constraints.regulatory.map((tag, i) => (
                    <span
                      key={i}
                      className="px-2 py-1 bg-amber-500/20 text-amber-300 rounded-lg text-xs flex items-center gap-1"
                    >
                      {tag}
                      <button
                        onClick={() => removeTag("regulatory", i)}
                        className="hover:text-white"
                      >
                        ×
                      </button>
                    </span>
                  ))}
                </div>
                <input
                  type="text"
                  value={regInput}
                  onChange={(e) => setRegInput(e.target.value)}
                  onKeyDown={(e) => {
                    if (e.key === "Enter") {
                      e.preventDefault();
                      addTag("regulatory", regInput);
                    }
                  }}
                  placeholder={t("input.regulatory_placeholder")}
                  className="w-full bg-transparent text-sm text-white focus:outline-none placeholder-slate-600"
                />
              </div>
            </div>
          </div>

          {/* ステークホルダー（責任者）情報 - オプション */}
          <div className="bg-[#12121a] rounded-2xl p-6 border border-white/5">
            <div className="flex items-center gap-2 mb-4">
              <span>👥</span>
              <h2 className="text-sm font-medium text-slate-300">
                {t("input.stakeholders_title")}
              </h2>
              <span className="text-xs text-slate-600">
                — {t("input.stakeholders_desc")}
              </span>
            </div>
            <div className="grid grid-cols-2 gap-4">
              <div className="bg-[#0a0a0f] rounded-xl p-4 border border-white/5">
                <label className="text-xs text-slate-500 mb-2 block">
                  {t("input.product_owner")}
                </label>
                <input
                  type="text"
                  value={stakeholders.product_owner}
                  onChange={(e) =>
                    setStakeholders({ product_owner: e.target.value })
                  }
                  placeholder={t("input.product_owner_hint")}
                  maxLength={100}
                  className="w-full bg-transparent text-sm text-white focus:outline-none placeholder-slate-600"
                />
              </div>
              <div className="bg-[#0a0a0f] rounded-xl p-4 border border-white/5">
                <label className="text-xs text-slate-500 mb-2 block">
                  {t("input.tech_lead")}
                </label>
                <input
                  type="text"
                  value={stakeholders.tech_lead}
                  onChange={(e) =>
                    setStakeholders({ tech_lead: e.target.value })
                  }
                  placeholder={t("input.tech_lead_hint")}
                  maxLength={100}
                  className="w-full bg-transparent text-sm text-white focus:outline-none placeholder-slate-600"
                />
              </div>
              <div className="bg-[#0a0a0f] rounded-xl p-4 border border-white/5">
                <label className="text-xs text-slate-500 mb-2 block">
                  {t("input.business_owner")}
                </label>
                <input
                  type="text"
                  value={stakeholders.business_owner}
                  onChange={(e) =>
                    setStakeholders({ business_owner: e.target.value })
                  }
                  placeholder={t("input.business_owner_hint")}
                  maxLength={100}
                  className="w-full bg-transparent text-sm text-white focus:outline-none placeholder-slate-600"
                />
              </div>
              <div className="bg-[#0a0a0f] rounded-xl p-4 border border-white/5">
                <label className="text-xs text-slate-500 mb-2 block">
                  {t("input.legal_reviewer")}
                </label>
                <input
                  type="text"
                  value={stakeholders.legal_reviewer}
                  onChange={(e) =>
                    setStakeholders({ legal_reviewer: e.target.value })
                  }
                  placeholder={t("input.legal_reviewer_hint")}
                  maxLength={100}
                  className="w-full bg-transparent text-sm text-white focus:outline-none placeholder-slate-600"
                />
              </div>
            </div>
          </div>

          {/* 送信ボタン */}
          <button
            onClick={handleSubmitWithStream}
            disabled={!isValid || isSubmitting}
            className={`w-full py-4 rounded-xl font-medium text-lg transition-all flex items-center justify-center gap-3 ${
              isValid && !isSubmitting
                ? "bg-gradient-to-r from-indigo-600 to-violet-600 hover:from-indigo-500 hover:to-violet-500 text-white shadow-lg shadow-indigo-500/25"
                : "bg-slate-800 text-slate-500 cursor-not-allowed"
            }`}
          >
            {isSubmitting ? (
              <>
                <div className="w-5 h-5 border-2 border-white/30 border-t-white rounded-full animate-spin" />
                {t("input.submitting")}
              </>
            ) : (
              <>
                <span aria-hidden="true">▶</span>
                {t("input.submit")}
              </>
            )}
          </button>

          {/* 処理時間の目安 */}
          <p className="text-center text-xs text-slate-600 mt-4">
            {t("input.processing_note")}
          </p>
        </div>

        {/* 機能説明カード + 知識ベース設定 */}
        <div className="grid grid-cols-4 gap-4 mt-8">
          {/* 道（知識設定なし） */}
          <div className="bg-[#12121a]/50 rounded-xl p-4 text-center border border-white/5">
            <span aria-hidden="true" className="text-2xl">
              🎯
            </span>
            <div className="text-sm font-medium mt-2">{t("input.fw_dao")}</div>
            <div className="text-xs text-slate-500">
              {t("input.fw_dao_desc")}
            </div>
          </div>

          {/* 法（知識設定なし） */}
          <div className="bg-[#12121a]/50 rounded-xl p-4 text-center border border-white/5">
            <span aria-hidden="true" className="text-2xl">
              🛤️
            </span>
            <div className="text-sm font-medium mt-2">{t("input.fw_fa")}</div>
            <div className="text-xs text-slate-500">
              {t("input.fw_fa_desc")}
            </div>
          </div>

          {/* 術（知識設定あり） */}
          <div className="bg-[#12121a]/50 rounded-xl p-4 text-center border border-white/5 group relative">
            <span aria-hidden="true" className="text-2xl">
              📋
            </span>
            <div className="text-sm font-medium mt-2">{t("input.fw_shu")}</div>
            <div className="text-xs text-slate-500">
              {t("input.fw_shu_desc")}
            </div>
            <button
              onClick={() => setPage("knowledge-shu")}
              className="mt-2 px-3 py-1 bg-indigo-600/20 hover:bg-indigo-600/40 text-indigo-300 text-xs rounded-lg transition-all flex items-center gap-1 mx-auto"
            >
              <span aria-hidden="true">📚</span> {t("input.add_knowledge")}
            </button>
          </div>

          {/* 器（知識設定あり） */}
          <div className="bg-[#12121a]/50 rounded-xl p-4 text-center border border-white/5 group relative">
            <span aria-hidden="true" className="text-2xl">
              🔧
            </span>
            <div className="text-sm font-medium mt-2">{t("input.fw_qi")}</div>
            <div className="text-xs text-slate-500">
              {t("input.fw_qi_desc")}
            </div>
            <button
              onClick={() => setPage("knowledge-qi")}
              className="mt-2 px-3 py-1 bg-violet-600/20 hover:bg-violet-600/40 text-violet-300 text-xs rounded-lg transition-all flex items-center gap-1 mx-auto"
            >
              <span aria-hidden="true">📚</span> {t("input.add_knowledge")}
            </button>
          </div>
        </div>
      </main>

      {/* 設定モーダル */}
      <SettingsModal
        isOpen={isSettingsOpen}
        onClose={() => setIsSettingsOpen(false)}
      />
    </div>
  );
};
