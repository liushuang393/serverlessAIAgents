# AgentFlow Memory System - 設計ドキュメント

## 概要

AgentFlowの記憶システムは、LightMem論文の思想に基づいた3段階記憶アーキテクチャを実装しています。

### 参考論文: LightMem

LightMemは、LLMの記憶システムを効率化するために、人間の記憶モデル（Atkinson-Shiffrin）を応用した3段階アーキテクチャを提案しています。

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

## 今後の拡張

- LLMLingua-2風の高度な圧縮モデル
- ベクトル検索による意味的検索
- 記憶の重要度自動調整
- 分散記憶システム

