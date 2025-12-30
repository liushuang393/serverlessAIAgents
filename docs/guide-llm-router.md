# LLM モデルルーター ガイド

> **バージョン**: 1.0.0
> **更新日**: 2025-01-20

---

## 📋 概要

AgentFlow の **ModelRouter** は、複数の LLM モデルを統一インターフェースで管理し、自動切替・コスト最適化・負荷分散を実現します。

### 主な特徴

| 特徴 | 説明 |
|------|------|
| 🔄 **自動切替** | 障害時に自動でフォールバック |
| 💰 **コスト最適化** | 予算内で最適なモデルを選択 |
| ⚡ **低レイテンシ** | 応答時間ベースのルーティング |
| 📊 **統計追跡** | 使用量・コスト・エラー率を監視 |
| 🎯 **能力マッチング** | タスクに最適なモデルを自動選択 |

---

## 🚀 クイックスタート

### 基本的な使用

```python
from agentflow.llm import (
    ModelRouter,
    LLMConfig,
    LLMMessage,
    RoutingStrategy,
    RoutingConfig,
)

# モデル設定
models = {
    "primary": LLMConfig(
        provider="anthropic",
        model="claude-3-5-sonnet-20241022",
        api_key="sk-ant-...",
    ),
    "fallback": LLMConfig(
        provider="openai",
        model="gpt-4o",
        api_key="sk-...",
    ),
    "economy": LLMConfig(
        provider="openai",
        model="gpt-4o-mini",
        api_key="sk-...",
    ),
}

# ルーター作成
router = ModelRouter(
    models=models,
    routing_config=RoutingConfig(
        strategy=RoutingStrategy.BALANCED,
        fallback_models=["fallback", "economy"],
        max_retries=3,
    ),
)

# リクエスト送信（自動でベストモデル選択）
messages = [LLMMessage(role="user", content="Hello!")]
response = await router.chat(messages)
print(response.content)
```

### 環境変数から自動設定

```python
from agentflow.llm import create_router_from_env

# OPENAI_API_KEY, ANTHROPIC_API_KEY を自動検出
router = create_router_from_env()
response = await router.chat(messages)
```

---

## 📋 対応モデル

### OpenAI

| モデル | ティア | 特徴 | 入力コスト |
|--------|--------|------|-----------|
| gpt-4o | Premium | マルチモーダル、最新 | $5/M tokens |
| gpt-4o-mini | Economy | 高コスパ | $0.15/M tokens |
| gpt-4-turbo | Premium | 長コンテキスト | $10/M tokens |

### Anthropic

| モデル | ティア | 特徴 | 入力コスト |
|--------|--------|------|-----------|
| claude-3-5-sonnet | Premium | コード最強 | $3/M tokens |
| claude-3-5-haiku | Economy | 高速 | $1/M tokens |
| claude-3-opus | Premium | 推論最強 | $15/M tokens |

### Google

| モデル | ティア | 特徴 | 入力コスト |
|--------|--------|------|-----------|
| gemini-1.5-pro | Premium | 100万トークンコンテキスト | $1.25/M tokens |
| gemini-1.5-flash | Economy | 超高速 | $0.075/M tokens |

### DeepSeek

| モデル | ティア | 特徴 | 入力コスト |
|--------|--------|------|-----------|
| deepseek-chat | Economy | コスパ最強 | $0.14/M tokens |
| deepseek-reasoner | Standard | 推理特化 | $0.55/M tokens |

---

## 🎯 ルーティング戦略

### 戦略一覧

| 戦略 | 説明 | ユースケース |
|------|------|------------|
| `COST_OPTIMIZED` | 最安モデルを選択 | 予算重視 |
| `QUALITY_OPTIMIZED` | 最高品質モデルを選択 | 品質重視 |
| `BALANCED` | コスト・品質・レイテンシのバランス | 一般用途 |
| `LATENCY_OPTIMIZED` | 最速モデルを選択 | リアルタイム処理 |
| `ROUND_ROBIN` | 順番に使用 | 負荷分散 |
| `CAPABILITY_MATCH` | 必要な能力に基づいて選択 | 特殊タスク |

### 戦略設定

```python
from agentflow.llm import RoutingStrategy, RoutingConfig

# コスト最適化
config = RoutingConfig(
    strategy=RoutingStrategy.COST_OPTIMIZED,
    cost_limit_per_request=0.01,  # $0.01/リクエスト上限
)

# 品質最適化
config = RoutingConfig(
    strategy=RoutingStrategy.QUALITY_OPTIMIZED,
    preferred_providers=["anthropic"],  # Anthropic を優先
)

# バランス（デフォルト）
config = RoutingConfig(
    strategy=RoutingStrategy.BALANCED,
    fallback_models=["fallback", "economy"],
    max_retries=3,
)
```

---

## 🎯 能力ベースの選択

### モデル能力

```python
from agentflow.llm import ModelCapability

# 能力タイプ
capabilities = [
    ModelCapability.CHAT,           # 対話
    ModelCapability.CODE,           # コード生成
    ModelCapability.REASONING,      # 推論
    ModelCapability.VISION,         # 画像理解
    ModelCapability.FUNCTION_CALLING,  # 関数呼び出し
]
```

### 能力に基づく選択

```python
# コードと推論が必要なタスク
response = await router.chat_with_capability(
    messages,
    required_capabilities=[
        ModelCapability.CODE,
        ModelCapability.REASONING,
    ],
)
```

---

## 💰 コスト管理

### コスト制限

```python
# リクエストあたりのコスト上限
response = await router.chat_cost_limited(
    messages,
    max_cost_per_request=0.01,  # $0.01
)
```

### コスト追跡

```python
# 総コスト取得
total_cost = router.get_total_cost()
print(f"総コスト: ${total_cost:.4f}")

# モデル別コスト
breakdown = router.get_cost_breakdown()
for model, cost in breakdown.items():
    print(f"{model}: ${cost:.4f}")
```

---

## 📊 統計と監視

### 統計取得

```python
# 全モデルの統計
stats = router.get_stats()

for model, s in stats.items():
    print(f"=== {model} ===")
    print(f"リクエスト数: {s.total_requests}")
    print(f"成功率: {(1 - s.error_rate) * 100:.1f}%")
    print(f"平均レイテンシ: {s.avg_latency_ms:.0f}ms")
    print(f"コスト: ${s.total_cost:.4f}")
```

### 統計リセット

```python
# 全統計リセット
router.reset_stats()

# 特定モデルのみ
router.reset_stats("economy")
```

---

## 🔄 フォールバックとリトライ

### 自動フォールバック

```python
config = RoutingConfig(
    strategy=RoutingStrategy.QUALITY_OPTIMIZED,
    fallback_models=["fallback", "economy"],  # フォールバック順序
    max_retries=3,
    retry_delay=1.0,  # リトライ間隔（秒）
)

# primary が失敗 → fallback → economy と試行
router = ModelRouter(models=models, routing_config=config)
```

### エラーハンドリング

```python
try:
    response = await router.chat(messages)
except Exception as e:
    # 全モデルが失敗した場合
    logger.error(f"全モデル失敗: {e}")
    # フォールバック処理
```

---

## 🔧 動的モデル管理

### モデルの追加・削除

```python
# モデル追加
router.add_model(
    "new_model",
    LLMConfig(provider="openai", model="gpt-4", api_key="..."),
)

# モデル削除
router.remove_model("old_model")

# モデル一覧
models = router.list_models()
```

### モデル情報取得

```python
from agentflow.llm import MODELS

# 登録済みモデルの情報
info = MODELS.get("claude-3-5-sonnet-20241022")
if info:
    print(f"プロバイダー: {info.provider}")
    print(f"ティア: {info.tier}")
    print(f"コンテキスト: {info.context_window}")
    print(f"コスト: ${info.input_cost_per_1k}/1K tokens")
```

---

## 🏗️ Agent 統合

### SkillEngine との統合

```python
from agentflow.skills import SkillEngine
from agentflow.llm import ModelRouter, create_router_from_env

router = create_router_from_env()
engine = SkillEngine()

@engine.tool("smart_chat")
async def smart_chat(message: str, task_type: str = "general") -> str:
    """タスクタイプに応じてモデルを選択"""
    
    messages = [LLMMessage(role="user", content=message)]
    
    if task_type == "code":
        # コードタスクは高品質モデル
        response = await router.chat_with_capability(
            messages,
            required_capabilities=[ModelCapability.CODE],
        )
    elif task_type == "simple":
        # 簡単なタスクはコスト重視
        response = await router.chat_cost_limited(messages, max_cost=0.001)
    else:
        # 一般タスクはバランス
        response = await router.chat(messages)
    
    return response.content
```

---

## 📚 関連ドキュメント

- [内蔵 Skills ガイド](guide-builtin-skills.md) - DB/決済/認証
- [Skills ガイド](guide-skills.md) - 自動進化システム
- [API リファレンス](api.md) - 詳細 API

