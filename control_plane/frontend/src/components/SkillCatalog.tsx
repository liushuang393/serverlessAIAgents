/**
 * SkillCatalog - カテゴリ別ビルトイン Skill 一覧.
 *
 * 利用シナリオ別にグループ化されたスキルカタログ表示。
 */

import { useEffect, useMemo, useState } from "react";
import { useAppStore } from "@/store/useAppStore";
import type { SkillCategoryId, SkillInfo } from "@/types";
import { useI18n } from "../i18n";

/** カテゴリ ID → i18n キーのマッピング */
const CATEGORY_I18N_KEY: Record<SkillCategoryId, string> = {
  common: "skill.cat_common",
  code_development: "skill.cat_code_development",
  web_search: "skill.cat_web_search",
  enterprise_office: "skill.cat_enterprise_office",
  ad_marketing: "skill.cat_ad_marketing",
  enterprise_workflow: "skill.cat_enterprise_workflow",
  ai_assistant: "skill.cat_ai_assistant",
  media_creative: "skill.cat_media_creative",
};

/** スキルカード Props */
interface SkillCardProps {
  readonly skill: SkillInfo;
  readonly expanded: boolean;
  readonly onToggle: () => void;
  readonly t: (key: string) => string;
}

/** スキルカード（単一スキル表示） */
function SkillCard({ skill, expanded, onToggle, t }: SkillCardProps) {
  return (
    <button
      type="button"
      className="bg-slate-900/50 border border-slate-800 rounded-xl p-5 hover:border-emerald-500/30 transition-all cursor-pointer text-left w-full"
      onClick={onToggle}
    >
      <div className="flex items-start justify-between mb-2">
        <div>
          <h3 className="text-base font-semibold text-slate-200">
            {skill.label}
          </h3>
          <p className="text-xs text-slate-500">
            {skill.name} v{skill.version}
          </p>
        </div>
        <span className="text-xs text-slate-600">{skill.author}</span>
      </div>
      <p className="text-sm text-slate-400 mb-3">{skill.description}</p>
      {/* タグ */}
      <div className="flex flex-wrap gap-1.5 mb-2">
        {skill.tags.map((tag) => (
          <span
            key={tag}
            className="px-2 py-0.5 bg-emerald-500/10 text-emerald-400 text-[10px] rounded-full"
          >
            {tag}
          </span>
        ))}
      </div>
      {/* トリガー（展開時） */}
      {expanded && skill.triggers.length > 0 && (
        <div className="mt-3 pt-3 border-t border-slate-800">
          <p className="text-[10px] text-slate-500 uppercase tracking-wider mb-2">
            {t("skill.trigger_words")}
          </p>
          <div className="flex flex-wrap gap-1.5">
            {skill.triggers.map((trigger) => (
              <span
                key={trigger}
                className="px-2 py-0.5 bg-amber-500/10 text-amber-400 text-[10px] rounded-full"
              >
                {trigger}
              </span>
            ))}
          </div>
        </div>
      )}
    </button>
  );
}

export function SkillCatalog() {
  const { t } = useI18n();
  const { skillGroups, loading, error, loadSkillsGrouped, clearError } =
    useAppStore();

  const [expanded, setExpanded] = useState<string | null>(null);
  const [collapsedCategories, setCollapsedCategories] = useState<Set<string>>(
    new Set(),
  );

  useEffect(() => {
    loadSkillsGrouped();
  }, [loadSkillsGrouped]);

  /** 全スキル数を計算 */
  const totalSkills = useMemo(
    () => skillGroups.reduce((sum, g) => sum + g.skills.length, 0),
    [skillGroups],
  );

  /** カテゴリの折りたたみ切り替え */
  const toggleCategory = (catId: string) => {
    setCollapsedCategories((prev) => {
      const next = new Set(prev);
      if (next.has(catId)) {
        next.delete(catId);
      } else {
        next.add(catId);
      }
      return next;
    });
  };

  return (
    <div className="p-6 max-w-6xl mx-auto space-y-6">
      {/* ヘッダー */}
      <div>
        <h1 className="text-2xl font-bold text-slate-100">
          {t("skill.title")}
        </h1>
        <p className="text-sm text-slate-500 mt-1">
          {t("skill.subtitle")}（{totalSkills} skills / {skillGroups.length}{" "}
          categories）
        </p>
      </div>

      {/* エラー */}
      {error && (
        <div className="bg-red-500/10 border border-red-500/20 rounded-lg p-4 flex items-center justify-between">
          <span className="text-red-400 text-sm">{error}</span>
          <button
            onClick={clearError}
            className="text-red-400 hover:text-red-300 text-xs"
          >
            ✕
          </button>
        </div>
      )}

      {/* ローディング */}
      {loading && (
        <div className="flex justify-center py-12">
          <div className="w-10 h-10 border-4 border-emerald-500/30 border-t-emerald-500 rounded-full animate-spin" />
        </div>
      )}

      {/* カテゴリ別セクション */}
      {!loading && skillGroups.length > 0 && (
        <div className="space-y-6 max-h-[75vh] overflow-y-auto pr-1">
          {skillGroups.map((group) => {
            const catId = group.id;
            const isCollapsed = collapsedCategories.has(catId);
            const label = t(CATEGORY_I18N_KEY[catId] ?? `skill.cat_${catId}`);

            return (
              <section key={catId}>
                {/* カテゴリヘッダー */}
                <button
                  onClick={() => toggleCategory(catId)}
                  className="flex items-center gap-2 mb-3 w-full text-left group"
                >
                  <span className="text-lg">{group.icon}</span>
                  <h2 className="text-lg font-semibold text-slate-200 group-hover:text-emerald-400 transition-colors">
                    {label}
                  </h2>
                  <span className="text-xs text-slate-500 ml-1">
                    ({group.skills.length})
                  </span>
                  <span
                    className={`text-slate-500 text-xs ml-auto transition-transform ${isCollapsed ? "" : "rotate-90"}`}
                  >
                    ▶
                  </span>
                </button>

                {/* スキルカードグリッド */}
                {!isCollapsed && (
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
                    {group.skills.map((skill) => (
                      <SkillCard
                        key={skill.name}
                        skill={skill}
                        expanded={expanded === skill.name}
                        onToggle={() =>
                          setExpanded(
                            expanded === skill.name ? null : skill.name,
                          )
                        }
                        t={t}
                      />
                    ))}
                  </div>
                )}
              </section>
            );
          })}
        </div>
      )}

      {/* 空状態 */}
      {!loading && skillGroups.length === 0 && (
        <div className="text-center py-16">
          <p className="text-4xl mb-4">🧩</p>
          <p className="text-slate-400">{t("skill.no_skills")}</p>
        </div>
      )}
    </div>
  );
}
