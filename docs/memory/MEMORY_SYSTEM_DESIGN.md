# AgentFlow Memory System - 設計ドキュメント

## 概要

AgentFlowの記憶システムは、LightMem論文の思想に基づいた3段階記憶アーキテクチャを実装しています。

さらに、HOPE/Evo-Memory思想に基づく自動最適化機能を統合:
- **記憶蒸留**: 類似記憶を抽象知識に自動変換
- **主動忘却**: 低価値記憶を自動削除
- **強化学習**: タスク結果に基づく記憶価値調整

### 参考論文

- **LightMem**: 効率的なLLM記憶システム（3段階アーキテクチャ）
- **HOPE**: 分層記憶アーキテクチャ（エピソード/セマンティック/手続き）
- **Evo-Memory**: 記憶蒸留と自動最適化

## アーキテクチャ

### 3段階記憶システム

```
入力テキスト
    ↓
┌─────────────────────────────────────┐
│ Light1: 感覚記憶 (Sensory Memory)   │
│ - 予圧縮 + トピック分割              │
│ - 冗長Token削減: 20-80%             │
└─────────────────────────────────────┘
    ↓
┌─────────────────────────────────────┐
│ Light2: 短期記憶 (Short-Term Memory)│
│ - トピックバッファ管理               │
│ - 閾値到達時に要約生成               │
│ - API呼び出し削減: 17-177倍         │
└─────────────────────────────────────┘
    ↓
┌─────────────────────────────────────┐
│ Light3: 長期記憶 (Long-Term Memory) │
│ - オンライン追加（ゼロ遅延）         │
│ - オフライン並行統合                 │
│ - 実行時間削減: 1.7-12倍            │
└─────────────────────────────────────┘
```

## コンポーネント

### 1. Light1: 感覚記憶 (SensoryMemory)

**職責:**
- 入力テキストの予圧縮
- トピック自動検出
- 重要度スコアリング
- 動的閾値による冗長Token削減

**実装:**
```python
from agentflow.memory import SensoryMemory, CompressionConfig

config = CompressionConfig(
    compression_ratio=0.6,  # 40%削減
    min_importance_threshold=0.3,
)
sensory = SensoryMemory(config)
entry = await sensory.process("長いテキスト...", topic="AI")
```

**効果:**
- Token数を20-80%削減
- 重要な情報を保持
- トピック別に整理

### 2. Light2: 短期記憶 (ShortTermMemory)

**職責:**
- トピック別バッファ管理
- Token閾値監視
- 閾値到達時の一括要約生成
- 主題純度の維持

**実装:**
```python
from agentflow.memory import ShortTermMemory

stm = ShortTermMemory(token_threshold=1000)
await stm.add_entry(entry)

if stm.should_summarize("AI"):
    summary = await stm.summarize_topic("AI")
```

**効果:**
- API呼び出しを17-177倍削減
- トピックの一貫性を維持
- 要約品質の向上

### 3. Light3: 長期記憶 (LongTermMemory)

**職責:**
- オンライン追加書き込み（ゼロ遅延）
- オフライン並行マージ
- 更新キュー管理
- タイムスタンプベースの統合

**実装:**
```python
from agentflow.memory import LongTermMemory

ltm = LongTermMemory(consolidation_interval=300)
await ltm.start()

# オンライン追加（即座に返る）
await ltm.store(entry)

# オフライン統合（バックグラウンド）
await ltm.consolidate()
```

**効果:**
- 実行時間を1.7-12倍削減
- ゼロ遅延の書き込み
- 並行処理による高速統合

## 統合マネージャー

### MemoryManager

3つのメモリレイヤーを統合管理するマネージャー。

**使用例:**
```python
from agentflow.memory import MemoryManager

manager = MemoryManager()
await manager.start()

# 情報を記憶（自動的に3段階フロー）
await manager.remember("重要な情報", topic="AI")

# 記憶を検索
memories = await manager.recall(topic="AI", limit=10)

await manager.stop()
```

## SharedContextとの統合

SharedContextに記憶システムを統合することで、Multi-Agentパターンで記憶を共有できます。

**使用例:**
```python
from agentflow.patterns import SharedContext

# 記憶システムを有効化
context = SharedContext(enable_memory=True)
await context.start()

# Agent間で記憶を共有
await context.remember("Agent Aの結果", topic="task1")
memories = await context.recall(topic="task1")

await context.stop()
```

## パフォーマンス

### 削減効果

| 指標 | 削減率 |
|------|--------|
| Token数 | 20-80% |
| API呼び出し | 17-177倍 |
| 実行時間 | 1.7-12倍 |

### 適用シーン

- **長時間対話**: 超長多轮对话での記憶管理
- **Multi-Agent**: 複数エージェント間の記憶共有
- **知識蓄積**: 継続的な学習と知識の蓄積

## 設計原則

1. **効率性**: Token削減とAPI呼び出し削減
2. **品質**: 重要な情報を保持
3. **スケーラビリティ**: 並行処理とオフライン統合
4. **柔軟性**: オプション機能として統合
5. **互換性**: 既存のAgentFlowと完全互換
6. **透過性**: 自動最適化はユーザーから透過的

---

## 自動最適化機能（v0.3.0+）

### 記憶の意味レベル分類

HOPE思想に基づき、記憶を3つの意味レベルに分類:

```python
class MemorySemanticLevel(str, Enum):
    EPISODIC = "episodic"      # エピソード記憶（具体的なイベント）
    SEMANTIC = "semantic"       # セマンティック記憶（抽象知識）
    PROCEDURAL = "procedural"   # 手続き記憶（スキル、パターン）
```

### 記憶の安定性レベル

記憶の固定度合いを示す安定性レベル:

```python
class MemoryStability(str, Enum):
    VOLATILE = "volatile"           # 揮発性（新情報で上書き可能）
    CONSOLIDATED = "consolidated"   # 固定済み（明示的な更新が必要）
    CRYSTALLIZED = "crystallized"   # 結晶化（コア知識、極めて安定）
```

### 記憶蒸留（MemoryDistiller）

Evo-Memory思想に基づき、類似記憶を抽象知識に自動変換:

**動作原理:**
1. 同一トピック内の記憶をセマンティック類似度でクラスタリング
2. 3件以上の類似記憶があれば蒸留対象
3. LLMを使用して抽象知識を生成
4. 蒸留された知識は`SEMANTIC`レベルに昇格

**効果:**
- 記憶数を削減しつつ情報量を維持
- パターンと規則性を自動抽出
- 検索効率の向上

```
┌─────────────────────────────────────────┐
│ 蒸留前（エピソード記憶 x 5）              │
│ - "AIは画像認識に強い"                   │
│ - "AIは自然言語処理が得意"               │
│ - "AIは音声認識で高精度"                 │
│ - "AIはパターン認識が優秀"               │
│ - "AIは機械学習で成果"                   │
└─────────────────────────────────────────┘
                    ↓ 蒸留
┌─────────────────────────────────────────┐
│ 蒸留後（セマンティック記憶 x 1）          │
│ - "AIはパターン認識タスク全般に強み"      │
│   (stability: CONSOLIDATED)              │
└─────────────────────────────────────────┘
```

### 主動忘却ポリシー

低価値記憶を自動削除し、記憶システムの効率を維持:

**忘却条件（全て満たす場合）:**
- 重要度スコア < 0.1
- 最終アクセスから60日以上経過
- 強化スコア < -0.3（タスクに悪影響）
- 結晶化されていない

**安全機構:**
- `CRYSTALLIZED`記憶は忘却対象外
- `CONSOLIDATED`記憶は厳しい条件で判定

### 強化学習フィードバック

タスクの成功/失敗に基づいて記憶の価値を調整:

**使用例:**
```python
from agentflow.memory import MemoryManager

manager = MemoryManager()
await manager.start()

# 記憶を使用してタスク実行
memories = await manager.recall(topic="AI")
result = await execute_task(memories)

# タスク結果をフィードバック
if result.success:
    await manager.reinforce(topic="AI", reward=1.0, context="タスク成功")
else:
    await manager.reinforce(topic="AI", reward=-0.5, context="タスク失敗")

await manager.stop()
```

**効果:**
- 有用な記憶の重要度が上昇
- 有害な記憶は忘却対象になりやすい
- 継続的な正のフィードバックで安定性が昇格

### 自動最適化タイミング

| 機能 | 間隔 | 設定パラメータ |
|------|------|---------------|
| 記憶蒸留 | 1時間 | `distill_interval` |
| 主動忘却 | 1日 | `forget_interval` |
| 重要度再計算 | 1時間 | `recalculation_interval` |

### MemoryManager拡張パラメータ

```python
manager = MemoryManager(
    # 既存パラメータ
    compression_config=None,
    token_threshold=1000,
    consolidation_interval=300,
    llm_client=None,
    enable_vector_search=False,
    enable_importance_adjustment=False,

    # 自動最適化パラメータ（デフォルト有効）
    enable_auto_distill=True,      # 自動蒸留
    enable_auto_forget=True,       # 自動忘却
    distill_interval=3600,         # 蒸留間隔（秒）
    forget_interval=86400,         # 忘却間隔（秒）
)
```

---

## 今後の拡張

- [x] ~~記憶の重要度自動調整~~（実装済み）
- [x] ~~記憶蒸留~~（実装済み）
- [x] ~~主動忘却~~（実装済み）
- [ ] LLMLingua-2風の高度な圧縮モデル
- [ ] ベクトル検索による意味的蒸留クラスタリング
- [ ] 分散記憶システムとの連携
- [ ] 記憶のバージョン管理

