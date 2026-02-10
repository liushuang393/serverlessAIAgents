# Decision Governance Engine - スコアリングシステム

## 概要

8次元評価による多次元スコアリングシステム。重み付け平均、inverse 次元の正規化、閾値判定（GO/PILOT/DELAY/NO_GO）、Hard veto 機能を提供。

## アーキテクチャ

```
┌─────────────────────────────────────────────────────────┐
│  DecisionWeightsConfig (YAML設定)                        │
│  - 8次元定義（user_impact, cost, risk, etc.）           │
│  - プリセット（growth_first/risk_first/efficiency_first）│
│  - 閾値設定（GO/PILOT/DELAY）                            │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│  ScoringEngine                                           │
│  1. 次元別スコア正規化（inverse 対応）                   │
│  2. 重み付け平均計算                                     │
│  3. Hard veto チェック                                   │
│  4. 閾値判定                                             │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│  ScoringResult                                           │
│  - weighted_score: 重み付け平均（1.0-5.0）              │
│  - verdict: GO/PILOT/DELAY/NO_GO                         │
│  - dimension_scores: 次元別詳細                          │
│  - hard_veto_triggered: Hard veto 発動フラグ            │
└─────────────────────────────────────────────────────────┘
```

## 評価次元（8次元）

| 次元ID | 名称 | inverse | 説明 |
|--------|------|---------|------|
| user_impact | ユーザー価値 | ❌ | ユーザーが得る実際の価値 |
| growth_potential | 成長ポテンシャル | ❌ | 市場規模・収益成長の可能性 |
| timing_window | タイミング | ❌ | 市場機会・競争窓口 |
| channel_reach | チャネル到達性 | ❌ | ユーザー獲得の容易さ |
| cost | コスト | ✅ | 開発+運用コスト（低い方が良い） |
| risk | リスク | ✅ | 市場/実行/法規制リスク（低い方が良い） |
| reversibility | 可逆性 | ❌ | 失敗時の回復容易さ |
| dependencies | 依存複雑度 | ✅ | 外部依存・技術負債（低い方が良い） |

## inverse 次元の正規化

inverse=true の次元（cost, risk, dependencies）は、スコアが低いほど良いため、正規化時に反転します。

```python
# 例: cost=5（高コスト）の場合
normalized = max_score - raw_score + min_score
normalized = 5 - 5 + 1 = 1  # 悪いスコア

# 例: cost=1（低コスト）の場合
normalized = 5 - 1 + 1 = 5  # 良いスコア
```

## 重み付け平均の計算

```python
weighted_score = Σ(normalized_score_i × weight_i) / Σ(weight_i)
```

### プリセット別の重み

#### growth_first（成長優先）
```yaml
user_impact: 18%
growth_potential: 24%  # 最重視
timing_window: 14%
channel_reach: 14%
cost: 8%
risk: 10%
reversibility: 7%
dependencies: 5%
```

#### risk_first（リスク優先）
```yaml
user_impact: 12%
growth_potential: 14%
timing_window: 8%
channel_reach: 8%
cost: 12%
risk: 22%  # 最重視
reversibility: 14%
dependencies: 10%
```

#### efficiency_first（効率優先）
```yaml
user_impact: 14%
growth_potential: 16%
timing_window: 10%
channel_reach: 10%
cost: 20%  # 最重視
risk: 12%
reversibility: 8%
dependencies: 10%
```

## 閾値判定

### GO（実行推奨）
- weighted_score >= 3.8
- confidence >= 0.70
- evidence_coverage >= 0.6
- Hard veto なし

### PILOT（試験実施）
- weighted_score >= 3.0
- confidence >= 0.45
- evidence_coverage >= 0.4

### DELAY（延期検討）
- weighted_score >= 2.5
- confidence >= 0.30
- evidence_coverage >= 0.2

### NO_GO（実行非推奨）
- 上記いずれにも該当しない
- または Hard veto 発動

## Hard Veto（拒否権）

特定の次元が閾値を超えた場合、無条件で NO_GO 判定。

```yaml
# growth_first の例
hard_veto_dimensions: ["risk"]
hard_veto_score: 4

# risk >= 4 の場合、他のスコアに関わらず NO_GO
```

## 使用例

### 基本的な使用

```python
from apps.decision_governance_engine.config import get_config
from apps.decision_governance_engine.services.scoring_engine import ScoringEngine

# 設定読み込み
config = get_config()

# エンジン初期化
engine = ScoringEngine(config)

# スコア計算
dimension_scores = {
    "user_impact": 4.5,
    "growth_potential": 4.0,
    "timing_window": 3.5,
    "channel_reach": 3.0,
    "cost": 2.0,  # 低コスト = 良い
    "risk": 2.5,  # 低リスク = 良い
    "reversibility": 4.0,
    "dependencies": 2.0,  # 低依存 = 良い
}

result = engine.calculate_score(
    dimension_scores=dimension_scores,
    confidence=0.8,
    evidence_coverage=0.7,
)

print(f"重み付けスコア: {result.weighted_score}")
print(f"判定: {result.verdict}")
print(f"Hard veto: {result.hard_veto_triggered}")

# 次元別詳細
for dim in result.dimension_scores:
    print(f"{dim.dimension_id}: raw={dim.raw_score}, "
          f"normalized={dim.normalized_score}, "
          f"weight={dim.weight}%, "
          f"contribution={dim.weighted_contribution:.2f}")
```

### プリセット切り替え

```python
from apps.decision_governance_engine.config import reload_config

# decision_weights.yaml の active_preset を変更
# active_preset: "risk_first"

# 設定リロード
config = reload_config()
engine = ScoringEngine(config)

# 同じスコアでも判定が変わる可能性
result = engine.calculate_score(dimension_scores, 0.8, 0.7)
```

## 信頼度スコアリング（Intelligence Service）

証拠の信頼度を数値化し、鮮度減衰を適用。

### ドメインスコア

```yaml
domain_scores:
  gov: 0.95      # .gov ドメイン
  edu: 0.90      # .edu ドメイン
  org: 0.75      # .org ドメイン
  com: 0.60      # .com ドメイン
  default: 0.50
```

### 鮮度減衰

```yaml
freshness_decay:
  days_30: 1.0    # 30日以内: 減衰なし
  days_90: 0.9    # 90日以内: 10% 減衰
  days_180: 0.7   # 180日以内: 30% 減衰
  days_365: 0.5   # 365日以内: 50% 減衰
  older: 0.3      # それ以上: 70% 減衰
```

### 最終スコア計算

```python
final_score = domain_score × freshness_factor

# 3段階に変換
if final_score >= 0.75:
    reliability = HIGH
elif final_score >= 0.50:
    reliability = MEDIUM
else:
    reliability = LOW
```

## 人間確認フロー

### API エンドポイント

#### 承認/却下
```bash
POST /api/human-review/approve
{
  "report_id": "PROP-202601-ABC123",
  "approved": true,
  "reviewer_name": "山田太郎",
  "reviewer_email": "yamada@example.com",
  "review_notes": "リスク評価を再確認しました"
}
```

#### 確認状態取得
```bash
GET /api/human-review/status/{report_id}
```

#### 未確認リスト
```bash
GET /api/human-review/pending
```

### DecisionReport への統合

```python
from apps.decision_governance_engine.schemas.output_schemas import (
    DecisionReport,
    HumanReview,
)

report = DecisionReport(
    report_id="PROP-202601-ABC123",
    # ... 他のフィールド
    human_review=HumanReview(
        requires_review=True,
        approved=None,  # 未確認
    ),
)

# 承認後
report.human_review.approved = True
report.human_review.reviewer_name = "山田太郎"
report.human_review.reviewed_at = "2026-02-10T10:30:00Z"
```

## テスト

```bash
# スコアリングエンジンのテスト
pytest --no-cov apps/decision_governance_engine/tests/unit/test_scoring_engine.py -v

# 特定のテスト
pytest --no-cov apps/decision_governance_engine/tests/unit/test_scoring_engine.py::test_inverse_dimension_normalization -v
```

## 設定ファイル

`apps/decision_governance_engine/config/decision_weights.yaml`

```yaml
version: "1.0.0"
active_preset: "growth_first"

dimensions:
  user_impact:
    id: "user_impact"
    name_ja: "ユーザー価値"
    min_score: 1
    max_score: 5
    inverse: false
  # ... 他の次元

presets:
  growth_first:
    weights:
      user_impact: 18
      growth_potential: 24
      # ...
    thresholds:
      go:
        min_score: 3.8
        min_confidence: 0.70
        # ...
```

## 今後の拡張

### 統計的根拠の追加

```yaml
thresholds:
  go:
    min_score: 3.8
    rationale: "過去100件の成功案件の平均スコア 3.92 の -1σ"
    historical_success_rate: 0.85
```

### A/B テスト機能

```python
# 複数の閾値設定を試験
engine.run_ab_test(
    dimension_scores=scores,
    threshold_variants=["conservative", "aggressive"],
)
```

### 機械学習による重み最適化

```python
# 過去の成功/失敗データから重みを学習
optimizer = WeightOptimizer()
optimal_weights = optimizer.fit(historical_decisions)
```

## 参考資料

- [config/__init__.py](../config/__init__.py) - 設定モデル定義
- [services/scoring_engine.py](../services/scoring_engine.py) - スコアリングエンジン実装
- [services/intelligence_service.py](../services/intelligence_service.py) - 信頼度スコアリング
- [routers/human_review.py](../routers/human_review.py) - 人間確認 API
- [schemas/output_schemas.py](../schemas/output_schemas.py) - HumanReview スキーマ
