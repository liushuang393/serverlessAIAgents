# Decision Governance Engine v2.0 変更履歴

## 概要

v2.0 では「認知アーキテクト（Cognitive Architect）」モデルに基づいて、以下の改善を実施しました：

1. **ClarificationAgent（問題診断）の新設**
2. **DaoAgent の強化（因果齿轮・死穴分析）**
3. **ワークフローの更新（7 Agent 構成）**

---

## 🆕 新機能

### 1. ClarificationAgent（問題診断/澄清）

**目的**: ユーザーの質問に即座に答えるのではなく、まず深く診断する

**出力内容**:
| フィールド | 説明 |
|-----------|------|
| `restated_question` | 質問の復唱（100字以内） |
| `ambiguities` | 曖昧な点（最大3つ） |
| `hidden_assumptions` | 暗黙の仮定（最大3つ） |
| `cognitive_biases` | 認知バイアス（最大2つ） |
| `refined_question` | 精緻化された質問 |
| `diagnosis_confidence` | 診断確信度（0.0-1.0） |

**認知バイアス検出パターン**:
- サンクコスト（埋没費用）
- 確証バイアス
- アンカリング
- 可用性ヒューリスティック
- 楽観バイアス
- 現状維持バイアス
- 二分法的思考

**ファイル**:
- `skills/clarification/SKILL.md` - プロンプト定義
- `agents/clarification_agent.py` - 実装

---

### 2. DaoAgent v2.0（因果齿轮・死穴分析）

**新機能1: 因果齿轮（Causal Gears）**

問題を3-5個の「齿轮」（構造モジュール）に分解し、因果関係を明らかにする。

```
[1] 外部環境
      │
      ▼
[2] 内部リソース ──▶ [3] 戦略的方向性
      │                     │
      ▼                     ▼
[4] 実行計画 ◀──────▶ [5] 組織/チーム
```

**CausalGear フィールド**:
| フィールド | 説明 |
|-----------|------|
| `gear_id` | 齿轮ID（1-5） |
| `name` | 齿轮名（20字以内） |
| `description` | 説明（100字以内） |
| `drives` | 駆動する齿轮ID（因→果） |
| `driven_by` | 駆動される齿轮ID（果←因） |
| `leverage` | レバレッジ効果（HIGH/MEDIUM/LOW） |

**新機能2: 死穴分析（Death Traps）**

現段階で絶対にやってはいけないことを明確にする。

**DeathTrap フィールド**:
| フィールド | 説明 |
|-----------|------|
| `action` | 禁止行動 |
| `reason` | なぜ致命的か |
| `severity` | 深刻度（FATAL/SEVERE/MODERATE） |

**深刻度の定義**:
- `FATAL`: これをやったら取り返しがつかない
- `SEVERE`: 大きなダメージを受ける
- `MODERATE`: 痛いが回復可能

---

## 🔄 ワークフローの変更

### Before (v1.0)
```
Gatekeeper → Dao → Fa → Shu → Qi → Review
   (6 Agents)
```

### After (v2.0)
```
Gatekeeper → Clarification → Dao → Fa → Shu → Qi → Review
                  ↑新設
   (7 Agents)
```

---

## 📁 変更ファイル一覧

### 新規作成
| ファイル | 説明 |
|---------|------|
| `skills/clarification/SKILL.md` | ClarificationAgent プロンプト定義 |
| `agents/clarification_agent.py` | ClarificationAgent 実装 |

### 更新
| ファイル | 変更内容 |
|---------|----------|
| `schemas/agent_schemas.py` | ClarificationInput/Output, CausalGear, DeathTrap, RhythmControl, FocusArea 追加 |
| `skills/dao/SKILL.md` | 因果齿轮・死穴分析のプロンプト追加 |
| `agents/dao_agent.py` | 因果齿轮・死穴ロジック実装 |
| `skills/fa/SKILL.md` | 稳健 vs 激进 比較マトリックスのプロンプト追加 |
| `agents/fa_agent.py` | 戦略タイプ・比較マトリックスロジック実装 |
| `skills/shu/SKILL.md` | 30天行動節奏控制のプロンプト追加 |
| `agents/shu_agent.py` | rhythm_control ロジック実装 |
| `agents/__init__.py` | ClarificationAgent エクスポート追加 |
| `workflow.py` | ClarificationAgent 統合、v2.0 対応 |
| `schemas/output_schemas.py` | DecisionReport に clarification 追加 |
| `agent.yaml` | v2.0 構成に更新 |

---

## 🎯 認知アーキテクト対応表

| 認知アーキテクトモデル | v2.0 対応 |
|----------------------|----------|
| Step 1: 問題澄清（論理漏洞・隠含仮定・思維誤区） | ✅ ClarificationAgent |
| Step 2: 構造拆解（3-5個の因果齿轮） | ✅ DaoAgent.causal_gears |
| Step 3: 核心判断（死穴・絶対禁忌） | ✅ DaoAgent.death_traps |
| Step 4: 路径規画（稳健 vs 激进） | ✅ FaAgent v2.0 |
| Step 5: 行動節奏（30天行動） | ✅ ShuAgent v2.0 |

---

## 🆕 FaAgent v2.0 新機能（稳健型 vs 激进型）

### 戦略タイプ分類
| タイプ | 説明 | 特徴 |
|-------|------|------|
| CONSERVATIVE | 稳健型 | 低リスク、慢回報、可控性高 |
| AGGRESSIVE | 激进型 | 高リスク、快回報、不確実性大 |
| BALANCED | バランス型 | 中間的アプローチ |

### PathOption 拡張フィールド
| フィールド | 説明 |
|-----------|------|
| `strategy_type` | 戦略タイプ（CONSERVATIVE/AGGRESSIVE/BALANCED） |
| `suitable_conditions` | 適用条件（このパスが有効な条件） |
| `risks` | 主要リスク |
| `costs` | コスト（金銭・時間・機会） |
| `time_to_value` | 価値実現までの時間 |
| `reversibility` | 可逆性（HIGH/MEDIUM/LOW） |

### 比較マトリックス（path_comparison）
```json
{
  "dimensions": ["ROI", "リスク", "時間", "可逆性", "リソース効率"],
  "scores": {
    "A": [3, 5, 2, 5, 4],  // 稳健型
    "B": [5, 2, 5, 2, 3]   // 激进型
  },
  "recommendation_summary": "状況に応じた推奨サマリー"
}
```

---

## 🆕 ShuAgent v2.0 新機能（30天行動節奏控制）

### 核心原則
**「接下来30天，只做这一件事」**

人は複数のことを同時に進めようとすると、どれも中途半端になる。最初の30日間は「一点突破」に集中し、成果を出すことで勢いをつける。

### rhythm_control フィールド
| フィールド | 説明 |
|-----------|------|
| `period` | 節奏周期（WEEK_1/WEEK_2/MONTH_1/MONTH_3） |
| `focus` | 聚焦領域（唯一の目標） |
| `checkpoint_date` | 検査点日期 |
| `checkpoint_criteria` | 検査点評価基準 |
| `next_decision_point` | 次の決策ポイント |

### FocusArea フィールド
| フィールド | 制約 | 説明 |
|-----------|------|------|
| `name` | 20字以内 | 聚焦名称 |
| `description` | 100字以内 | 具体的に何をするか |
| `success_metric` | 必須 | 数値で測定可能な指標 |
| `avoid_list` | max 3 | この期間中に「やらないこと」 |

### 節奏周期の選択基準
| 周期 | 適用シーン |
|------|-----------|
| WEEK_1 | 緊急対応、危機管理 |
| WEEK_2 | スプリント、MVP検証 |
| **MONTH_1** | **標準（推奨）** |
| MONTH_3 | 大規模変革、組織改革 |

### 出力例
```json
{
  "rhythm_control": {
    "period": "MONTH_1",
    "focus": {
      "name": "MVP完成と初期検証",
      "description": "コア機能3つのみを実装し、10名のベータユーザーから直接フィードバックを取得する",
      "success_metric": "ベータユーザー10名獲得、NPS 40以上",
      "avoid_list": [
        "追加機能の開発要望への対応",
        "大規模マーケティング施策",
        "完璧主義による過度な磨き込み"
      ]
    },
    "checkpoint_date": "30天後",
    "checkpoint_criteria": ["ユーザー目標達成度", "コア機能完成度", "フィードバック質"],
    "next_decision_point": "30日後に継続/ピボット/撤退を経営判断"
  }
}
```

---

## 📋 次期改善項目（v2.1）

1. **UI プロンプト分離**: 画面レベルのプロンプト管理
2. **ReviewAgent**: 死穴回避の検証項目追加
3. **リアルタイムフィードバック**: checkpoint_date に基づく自動リマインダー

---

## 使用例

```python
from apps.decision_governance_engine.workflow import DecisionEngine

engine = DecisionEngine()
result = await engine.process("新規事業への投資と既存事業強化、どちらを優先すべきか")

# v2.0 出力
print(result.clarification)  # 問題診断結果
print(result.dao.causal_gears)  # 因果齿轮
print(result.dao.death_traps)  # 死穴（禁忌）
```

