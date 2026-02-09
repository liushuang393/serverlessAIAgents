"""LLM モデル定義.

モデル階層、能力、事前定義モデルライブラリを提供。
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum


class ModelTier(Enum):
    """モデル階層."""

    ECONOMY = "economy"  # エコノミー型（高速、低コスト）
    STANDARD = "standard"  # スタンダード型（バランス）
    PREMIUM = "premium"  # プレミアム型（最強、最高価格）


class ModelCapability(Enum):
    """モデル能力."""

    CHAT = "chat"  # 対話
    COMPLETION = "completion"  # 補完
    CODE = "code"  # コード生成
    REASONING = "reasoning"  # 推論
    VISION = "vision"  # 画像理解
    EMBEDDING = "embedding"  # ベクトル埋め込み
    FUNCTION_CALLING = "function_calling"  # 関数呼び出し
    AUDIO = "audio"  # 音声理解/生成
    REALTIME = "realtime"  # リアルタイム音声対話


@dataclass
class ModelInfo:
    """モデル情報."""

    name: str
    provider: str
    tier: ModelTier
    capabilities: list[ModelCapability]
    context_window: int
    input_cost_per_1k: float  # 1000トークンあたりの入力コスト（USD）
    output_cost_per_1k: float  # 1000トークンあたりの出力コスト（USD）
    max_output_tokens: int = 4096
    supports_streaming: bool = True
    supports_json_mode: bool = False
    description: str = ""


# 事前定義モデルライブラリ（2025年12月30日更新）
MODELS: dict[str, ModelInfo] = {
    # ========================================
    # OpenAI（テキスト/マルチモーダル）
    # ========================================
    "gpt-4o": ModelInfo(
        name="gpt-4o",
        provider="openai",
        tier=ModelTier.PREMIUM,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
            ModelCapability.VISION,
            ModelCapability.FUNCTION_CALLING,
            ModelCapability.AUDIO,
        ],
        context_window=128000,
        input_cost_per_1k=0.0025,
        output_cost_per_1k=0.01,
        max_output_tokens=16384,
        supports_json_mode=True,
        description="GPT-4o フラグシップマルチモーダルモデル（テキスト/画像/音声）",
    ),
    "gpt-4o-mini": ModelInfo(
        name="gpt-4o-mini",
        provider="openai",
        tier=ModelTier.ECONOMY,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.FUNCTION_CALLING,
            ModelCapability.VISION,
        ],
        context_window=128000,
        input_cost_per_1k=0.00015,
        output_cost_per_1k=0.0006,
        max_output_tokens=16384,
        supports_json_mode=True,
        description="GPT-4o-mini 高コストパフォーマンスモデル",
    ),
    "o1": ModelInfo(
        name="o1",
        provider="openai",
        tier=ModelTier.PREMIUM,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
            ModelCapability.VISION,
            ModelCapability.FUNCTION_CALLING,
        ],
        context_window=200000,
        input_cost_per_1k=0.015,
        output_cost_per_1k=0.06,
        max_output_tokens=100000,
        supports_json_mode=True,
        description="o1 深度推論モデル、複雑なタスクに適している",
    ),
    "o1-mini": ModelInfo(
        name="o1-mini",
        provider="openai",
        tier=ModelTier.STANDARD,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
        ],
        context_window=128000,
        input_cost_per_1k=0.003,
        output_cost_per_1k=0.012,
        max_output_tokens=65536,
        supports_json_mode=True,
        description="o1-mini 軽量推論モデル",
    ),
    "o3-mini": ModelInfo(
        name="o3-mini",
        provider="openai",
        tier=ModelTier.STANDARD,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
            ModelCapability.FUNCTION_CALLING,
        ],
        context_window=200000,
        input_cost_per_1k=0.0011,
        output_cost_per_1k=0.0044,
        max_output_tokens=100000,
        supports_json_mode=True,
        description="o3-mini 最新推論モデル（2025年1月リリース）",
    ),
    # OpenAI音声モデル
    "gpt-4o-realtime": ModelInfo(
        name="gpt-4o-realtime-preview",
        provider="openai",
        tier=ModelTier.PREMIUM,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.AUDIO,
            ModelCapability.REALTIME,
        ],
        context_window=128000,
        input_cost_per_1k=0.005,
        output_cost_per_1k=0.02,
        max_output_tokens=4096,
        description="GPT-4o Realtime リアルタイム音声対話",
    ),
    "gpt-4o-audio": ModelInfo(
        name="gpt-4o-audio-preview",
        provider="openai",
        tier=ModelTier.PREMIUM,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.AUDIO,
        ],
        context_window=128000,
        input_cost_per_1k=0.0025,
        output_cost_per_1k=0.01,
        max_output_tokens=16384,
        description="GPT-4o Audio 音声理解と生成",
    ),
    # ========================================
    # Anthropic Claude（テキスト/マルチモーダル）
    # ========================================
    "claude-sonnet-4-20250514": ModelInfo(
        name="claude-sonnet-4-20250514",
        provider="anthropic",
        tier=ModelTier.PREMIUM,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
            ModelCapability.VISION,
            ModelCapability.FUNCTION_CALLING,
        ],
        context_window=200000,
        input_cost_per_1k=0.003,
        output_cost_per_1k=0.015,
        max_output_tokens=64000,
        description="Claude Sonnet 4 最新フラグシップ（2025年リリース予定）",
    ),
    "claude-3-5-sonnet-20241022": ModelInfo(
        name="claude-3-5-sonnet-20241022",
        provider="anthropic",
        tier=ModelTier.PREMIUM,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
            ModelCapability.VISION,
            ModelCapability.FUNCTION_CALLING,
        ],
        context_window=200000,
        input_cost_per_1k=0.003,
        output_cost_per_1k=0.015,
        max_output_tokens=8192,
        description="Claude 3.5 Sonnet コード能力が最強",
    ),
    "claude-3-5-haiku-20241022": ModelInfo(
        name="claude-3-5-haiku-20241022",
        provider="anthropic",
        tier=ModelTier.ECONOMY,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.FUNCTION_CALLING,
        ],
        context_window=200000,
        input_cost_per_1k=0.0008,
        output_cost_per_1k=0.004,
        max_output_tokens=8192,
        description="Claude 3.5 Haiku 高速エコノミー",
    ),
    "claude-3-opus-20240229": ModelInfo(
        name="claude-3-opus-20240229",
        provider="anthropic",
        tier=ModelTier.PREMIUM,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
            ModelCapability.VISION,
        ],
        context_window=200000,
        input_cost_per_1k=0.015,
        output_cost_per_1k=0.075,
        max_output_tokens=4096,
        description="Claude 3 Opus 最強推論能力",
    ),
    # ========================================
    # Google Gemini（テキスト/マルチモーダル）
    # ========================================
    "gemini-2.0-flash": ModelInfo(
        name="gemini-2.0-flash-exp",
        provider="google",
        tier=ModelTier.STANDARD,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
            ModelCapability.VISION,
            ModelCapability.AUDIO,
            ModelCapability.FUNCTION_CALLING,
        ],
        context_window=1000000,
        input_cost_per_1k=0.0,
        output_cost_per_1k=0.0,
        max_output_tokens=8192,
        description="Gemini 2.0 Flash 最新マルチモーダル（無料プレビュー）",
    ),
    "gemini-2.0-flash-thinking": ModelInfo(
        name="gemini-2.0-flash-thinking-exp",
        provider="google",
        tier=ModelTier.STANDARD,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
        ],
        context_window=1000000,
        input_cost_per_1k=0.0,
        output_cost_per_1k=0.0,
        max_output_tokens=8192,
        description="Gemini 2.0 Flash Thinking 推論強化",
    ),
    "gemini-1.5-pro": ModelInfo(
        name="gemini-1.5-pro",
        provider="google",
        tier=ModelTier.PREMIUM,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
            ModelCapability.VISION,
            ModelCapability.AUDIO,
            ModelCapability.FUNCTION_CALLING,
        ],
        context_window=2000000,
        input_cost_per_1k=0.00125,
        output_cost_per_1k=0.005,
        max_output_tokens=8192,
        description="Gemini 1.5 Pro 200万コンテキスト",
    ),
    "gemini-1.5-flash": ModelInfo(
        name="gemini-1.5-flash",
        provider="google",
        tier=ModelTier.ECONOMY,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.VISION,
            ModelCapability.AUDIO,
        ],
        context_window=1000000,
        input_cost_per_1k=0.000075,
        output_cost_per_1k=0.0003,
        max_output_tokens=8192,
        description="Gemini 1.5 Flash 超高速レスポンス",
    ),
    # ========================================
    # DeepSeek（テキスト/コード）
    # ========================================
    "deepseek-chat": ModelInfo(
        name="deepseek-chat",
        provider="deepseek",
        tier=ModelTier.ECONOMY,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
            ModelCapability.FUNCTION_CALLING,
        ],
        context_window=64000,
        input_cost_per_1k=0.00014,
        output_cost_per_1k=0.00028,
        max_output_tokens=8192,
        description="DeepSeek V3 コストパフォーマンスの王者",
    ),
    "deepseek-reasoner": ModelInfo(
        name="deepseek-reasoner",
        provider="deepseek",
        tier=ModelTier.STANDARD,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
        ],
        context_window=64000,
        input_cost_per_1k=0.00055,
        output_cost_per_1k=0.00219,
        max_output_tokens=8192,
        description="DeepSeek R1 推論専用",
    ),
    # ========================================
    # ローカルモデル（Ollama/LocalAI）
    # ========================================
    "llama3.3-70b": ModelInfo(
        name="llama3.3:70b",
        provider="ollama",
        tier=ModelTier.PREMIUM,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
        ],
        context_window=128000,
        input_cost_per_1k=0.0,
        output_cost_per_1k=0.0,
        max_output_tokens=4096,
        description="Llama 3.3 70B ローカル実行",
    ),
    "qwen2.5-72b": ModelInfo(
        name="qwen2.5:72b",
        provider="ollama",
        tier=ModelTier.PREMIUM,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
        ],
        context_window=131072,
        input_cost_per_1k=0.0,
        output_cost_per_1k=0.0,
        max_output_tokens=8192,
        description="Qwen 2.5 72B 中国語最強ローカルモデル",
    ),
    "qwen2.5-coder-32b": ModelInfo(
        name="qwen2.5-coder:32b",
        provider="ollama",
        tier=ModelTier.STANDARD,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
        ],
        context_window=131072,
        input_cost_per_1k=0.0,
        output_cost_per_1k=0.0,
        max_output_tokens=8192,
        description="Qwen 2.5 Coder 32B コード専用",
    ),
    "mistral-large": ModelInfo(
        name="mistral-large:latest",
        provider="ollama",
        tier=ModelTier.PREMIUM,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
            ModelCapability.FUNCTION_CALLING,
        ],
        context_window=128000,
        input_cost_per_1k=0.0,
        output_cost_per_1k=0.0,
        max_output_tokens=4096,
        description="Mistral Large 123B ローカル実行",
    ),
    "phi-4": ModelInfo(
        name="phi4:latest",
        provider="ollama",
        tier=ModelTier.ECONOMY,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
        ],
        context_window=16384,
        input_cost_per_1k=0.0,
        output_cost_per_1k=0.0,
        max_output_tokens=4096,
        description="Microsoft Phi-4 14B 軽量高効率",
    ),
}

