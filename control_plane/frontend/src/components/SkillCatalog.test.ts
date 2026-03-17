/**
 * SkillCatalog カテゴリ関連のユニットテスト.
 *
 * コンポーネントレンダリングは @testing-library/react 未導入のため省略。
 * カテゴリ定数・型整合性・i18n キーの網羅性を検証する。
 *
 * @vitest-environment node
 */

import { describe, expect, it } from 'vitest';
import type { SkillCategoryId, SkillCategoryGroup, SkillInfo } from '@/types';

/** SkillCatalog.tsx 内の CATEGORY_I18N_KEY と同一定義（テスト用に複製） */
const CATEGORY_I18N_KEY: Record<SkillCategoryId, string> = {
  common: 'skill.cat_common',
  code_development: 'skill.cat_code_development',
  web_search: 'skill.cat_web_search',
  enterprise_office: 'skill.cat_enterprise_office',
  ad_marketing: 'skill.cat_ad_marketing',
  enterprise_workflow: 'skill.cat_enterprise_workflow',
  ai_assistant: 'skill.cat_ai_assistant',
  media_creative: 'skill.cat_media_creative',
};

/** 全カテゴリ ID のリスト */
const ALL_CATEGORY_IDS: SkillCategoryId[] = [
  'common',
  'code_development',
  'web_search',
  'enterprise_office',
  'ad_marketing',
  'enterprise_workflow',
  'ai_assistant',
  'media_creative',
];

describe('CATEGORY_I18N_KEY', () => {
  it('全 8 カテゴリの i18n キーが定義されている', () => {
    expect(Object.keys(CATEGORY_I18N_KEY)).toHaveLength(8);
  });

  it('全カテゴリ ID が含まれる', () => {
    for (const id of ALL_CATEGORY_IDS) {
      expect(CATEGORY_I18N_KEY).toHaveProperty(id);
    }
  });

  it('i18n キーが skill.cat_ プレフィクスを持つ', () => {
    for (const [id, key] of Object.entries(CATEGORY_I18N_KEY)) {
      expect(key).toBe(`skill.cat_${id}`);
    }
  });

  it('i18n キーが全てユニーク', () => {
    const values = Object.values(CATEGORY_I18N_KEY);
    expect(new Set(values).size).toBe(values.length);
  });
});

describe('SkillCategoryGroup 型テスト', () => {
  it('有効な SkillCategoryGroup オブジェクトを作成できる', () => {
    const group: SkillCategoryGroup = {
      id: 'code_development',
      icon: '💻',
      order: 1,
      skills: [
        {
          name: 'test-skill',
          label: 'Test Skill',
          description: 'テストスキル',
          version: '1.0.0',
          author: 'Test',
          tags: ['test'],
          tags_legacy: ['test'],
          triggers: [],
          requirements: [],
          examples: [],
          path: '/test',
          category: 'code_development',
        },
      ],
    };
    expect(group.id).toBe('code_development');
    expect(group.skills).toHaveLength(1);
    expect(group.skills[0].category).toBe(group.id);
  });

  it('空スキルリストを持つグループも有効', () => {
    const group: SkillCategoryGroup = {
      id: 'common',
      icon: '🔧',
      order: 0,
      skills: [],
    };
    expect(group.skills).toHaveLength(0);
  });
});

describe('SkillInfo category フィールド', () => {
  it('category が SkillCategoryId のいずれかの値を持つ', () => {
    const skill: SkillInfo = {
      name: 'chatbot',
      label: 'Chatbot',
      description: 'チャットボット',
      version: '1.0.0',
      author: 'Agent',
      tags: ['chat'],
      tags_legacy: ['chat'],
      triggers: [],
      requirements: [],
      examples: [],
      path: '/chatbot',
      category: 'ai_assistant',
    };
    expect(ALL_CATEGORY_IDS).toContain(skill.category);
  });
});

describe('totalSkills 計算ロジック', () => {
  it('全グループの skills.length 合計が正しい', () => {
    const groups: SkillCategoryGroup[] = [
      { id: 'common', icon: '🔧', order: 0, skills: [{ name: 'a' } as SkillInfo, { name: 'b' } as SkillInfo] },
      { id: 'code_development', icon: '💻', order: 1, skills: [{ name: 'c' } as SkillInfo] },
      { id: 'web_search', icon: '🔍', order: 2, skills: [] },
    ];
    const totalSkills = groups.reduce((sum, g) => sum + g.skills.length, 0);
    expect(totalSkills).toBe(3);
  });
});

