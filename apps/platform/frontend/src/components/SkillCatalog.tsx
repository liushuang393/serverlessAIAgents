/**
 * SkillCatalog - ビルトイン Skill 一覧.
 *
 * タグフィルタ + トリガーワード表示。
 */

import { useEffect, useState } from 'react';
import { useAppStore } from '@/store/useAppStore';

export function SkillCatalog() {
  const {
    skills,
    skillTags,
    loading,
    error,
    loadSkills,
    searchSkillsByTag,
    clearError,
  } = useAppStore();

  const [selectedTag, setSelectedTag] = useState<string>('');
  const [expanded, setExpanded] = useState<string | null>(null);

  useEffect(() => {
    loadSkills();
  }, [loadSkills]);

  /** タグフィルタ適用 */
  const handleFilter = (tag: string) => {
    setSelectedTag(tag);
    if (tag === '') {
      loadSkills();
    } else {
      searchSkillsByTag(tag);
    }
  };

  return (
    <div className="p-6 max-w-6xl mx-auto space-y-6">
      {/* ヘッダー */}
      <div>
        <h1 className="text-2xl font-bold text-slate-100">Skill Catalog</h1>
        <p className="text-sm text-slate-500 mt-1">
          ビルトインスキル一覧（{skills.length} skills）
        </p>
      </div>

      {/* エラー */}
      {error && (
        <div className="bg-red-500/10 border border-red-500/20 rounded-lg p-4 flex items-center justify-between">
          <span className="text-red-400 text-sm">{error}</span>
          <button onClick={clearError} className="text-red-400 hover:text-red-300 text-xs">✕</button>
        </div>
      )}

      {/* タグフィルタ */}
      {skillTags.length > 0 && (
        <div className="bg-slate-900/50 border border-slate-800 rounded-xl p-4">
          <p className="text-xs text-slate-500 uppercase tracking-wider mb-3">Filter by Tag</p>
          <div className="flex flex-wrap gap-2">
            <button
              onClick={() => handleFilter('')}
              className={`px-3 py-1.5 text-xs rounded-full transition-colors ${
                selectedTag === '' ? 'bg-emerald-600 text-white' : 'bg-slate-800 text-slate-400 hover:bg-slate-700'
              }`}
            >
              All
            </button>
            {skillTags.map((t) => (
              <button
                key={t.tag}
                onClick={() => handleFilter(t.tag)}
                className={`px-3 py-1.5 text-xs rounded-full transition-colors ${
                  selectedTag === t.tag ? 'bg-emerald-600 text-white' : 'bg-slate-800 text-slate-400 hover:bg-slate-700'
                }`}
              >
                {t.tag} <span className="ml-1 opacity-60">({t.count})</span>
              </button>
            ))}
          </div>
        </div>
      )}

      {/* ローディング */}
      {loading && (
        <div className="flex justify-center py-12">
          <div className="w-10 h-10 border-4 border-emerald-500/30 border-t-emerald-500 rounded-full animate-spin" />
        </div>
      )}

      {/* Skill カードグリッド */}
      {!loading && skills.length > 0 && (
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          {skills.map((skill) => (
            <div
              key={skill.name}
              className="bg-slate-900/50 border border-slate-800 rounded-xl p-5 hover:border-emerald-500/30 transition-all cursor-pointer"
              onClick={() => setExpanded(expanded === skill.name ? null : skill.name)}
            >
              <div className="flex items-start justify-between mb-2">
                <div>
                  <h3 className="text-base font-semibold text-slate-200">{skill.label}</h3>
                  <p className="text-xs text-slate-500">{skill.name} v{skill.version}</p>
                </div>
                <span className="text-xs text-slate-600">{skill.author}</span>
              </div>
              <p className="text-sm text-slate-400 mb-3">{skill.description}</p>
              {/* タグ */}
              <div className="flex flex-wrap gap-1.5 mb-2">
                {skill.tags.map((tag) => (
                  <span key={tag} className="px-2 py-0.5 bg-emerald-500/10 text-emerald-400 text-[10px] rounded-full">
                    {tag}
                  </span>
                ))}
              </div>
              {/* トリガー（展開時） */}
              {expanded === skill.name && skill.triggers.length > 0 && (
                <div className="mt-3 pt-3 border-t border-slate-800">
                  <p className="text-[10px] text-slate-500 uppercase tracking-wider mb-2">Trigger Words</p>
                  <div className="flex flex-wrap gap-1.5">
                    {skill.triggers.map((trigger) => (
                      <span key={trigger} className="px-2 py-0.5 bg-amber-500/10 text-amber-400 text-[10px] rounded-full">
                        {trigger}
                      </span>
                    ))}
                  </div>
                </div>
              )}
            </div>
          ))}
        </div>
      )}

      {/* 空状態 */}
      {!loading && skills.length === 0 && (
        <div className="text-center py-16">
          <p className="text-4xl mb-4">🧩</p>
          <p className="text-slate-400">No skills found</p>
        </div>
      )}
    </div>
  );
}

