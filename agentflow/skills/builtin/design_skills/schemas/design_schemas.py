"""Design Skills Engine - Pydanticスキーマ定義.

デザインパイプラインの全データ契約を定義:
- DesignBriefInput: ユーザーの自然言語デザインブリーフ
- IntentAnalysis: 解析されたデザイン意図(IntentAnalyzerAgent出力)
- GlobalStyle: 全画像共通のスタイルパラメータ
- ImageSpec: 個別画像の生成仕様
- PromptPlanOutput: 完全なプロンプト計画(PromptPlannerAgent出力)
- GeneratedImage: 生成された個別画像の結果
- WorkflowResult: ワークフロー実行結果全体
"""

from enum import Enum

from pydantic import BaseModel, Field


# =============================================================================
# 列挙型
# =============================================================================


class ImageRole(str, Enum):
    """デザインセット内の画像役割."""

    HERO = "HERO"  # メインビジュアル / キービジュアル
    FEATURE = "FEATURE"  # 特徴ハイライト
    DETAIL = "DETAIL"  # クローズアップ / ディテール
    LIFESTYLE = "LIFESTYLE"  # 使用シーン / ライフスタイル
    COMPARISON = "COMPARISON"  # 比較 / サイズ比較
    INFOGRAPHIC = "INFOGRAPHIC"  # テキストオーバーレイ / スペックシート


class DesignCategory(str, Enum):
    """デザイン意図カテゴリ."""

    PRODUCT_PHOTOGRAPHY = "PRODUCT_PHOTOGRAPHY"  # 商品撮影
    BRAND_IDENTITY = "BRAND_IDENTITY"  # ブランドアイデンティティ
    SOCIAL_MEDIA = "SOCIAL_MEDIA"  # SNS投稿
    ADVERTISING = "ADVERTISING"  # 広告
    PACKAGING = "PACKAGING"  # パッケージ
    UI_MOCKUP = "UI_MOCKUP"  # UIモックアップ


# =============================================================================
# 入力スキーマ
# =============================================================================


class DesignBriefInput(BaseModel):
    """ユーザーのデザインブリーフ - 自然言語入力."""

    brief: str = Field(..., description="デザイン内容の自然言語記述")
    style_preferences: list[str] = Field(
        default_factory=list,
        max_length=10,
        description="スタイルキーワード(例: 'ミニマル', 'テクノ', '暖色系')",
    )
    target_platform: str = Field(
        default="",
        description="ターゲットプラットフォーム(例: 'amazon', 'instagram', 'website')",
    )
    num_images: int = Field(
        default=8,
        ge=1,
        le=20,
        description="生成画像数(1-20枚)",
    )
    brand_colors: list[str] = Field(
        default_factory=list,
        max_length=5,
        description="ブランドカラー(16進数コード)",
    )
    aspect_ratio: str = Field(
        default="1:1",
        description="アスペクト比(例: '1:1', '16:9', '4:3')",
    )
    reference_image_paths: list[str] = Field(
        default_factory=list,
        max_length=5,
        description="スタイル一貫性のための参照画像パス",
    )


# =============================================================================
# IntentAnalyzerAgent 出力
# =============================================================================


class IntentAnalysis(BaseModel):
    """解析されたデザイン意図 - IntentAnalyzerAgentの出力."""

    category: DesignCategory = Field(..., description="デザインカテゴリ")
    subject: str = Field(
        ...,
        max_length=100,
        description="メインの被写体(例: 'Bluetoothスピーカー')",
    )
    key_features: list[str] = Field(
        ...,
        max_length=5,
        description="強調すべき主要特徴(最大5つ)",
    )
    target_audience: str = Field(
        default="",
        max_length=100,
        description="ターゲットオーディエンス",
    )
    style_direction: str = Field(
        ...,
        max_length=200,
        description="解釈されたスタイル方向性",
    )
    image_roles: list[ImageRole] = Field(
        ...,
        description="セット内の画像役割配分",
    )
    platform_constraints: dict[str, str] = Field(
        default_factory=dict,
        description="プラットフォーム固有の制約(例: 解像度、セーフゾーン)",
    )


# =============================================================================
# PromptPlannerAgent 入出力
# =============================================================================


class GlobalStyle(BaseModel):
    """グローバルスタイル定義 - 全画像で共有."""

    color_palette: list[str] = Field(
        ...,
        max_length=5,
        description="カラーパレットキーワード",
    )
    lighting: str = Field(..., description="ライティング記述")
    camera_angle: str = Field(..., description="デフォルトカメラアングル")
    mood: str = Field(..., description="全体のムード・雰囲気")
    negative_prompt: str = Field(
        ...,
        description="グローバルネガティブプロンプト(全画像に適用)",
    )
    lora_models: list[str] = Field(
        default_factory=list,
        max_length=3,
        description="スタイル一貫性のためのLoRAモデル名",
    )
    base_model: str = Field(
        default="sd_xl_base_1.0.safetensors",
        description="ベースモデルチェックポイント名",
    )


class ImageSpec(BaseModel):
    """個別画像の生成仕様."""

    image_id: str = Field(..., description="画像の一意識別子")
    role: ImageRole = Field(..., description="セット内での画像役割")
    prompt: str = Field(..., description="画像固有のポジティブプロンプト")
    seed: int = Field(default=-1, description="シード値(-1でランダム)")
    weight: float = Field(
        default=1.0,
        ge=0.1,
        le=2.0,
        description="プロンプトウェイト",
    )
    extra_negative: str = Field(
        default="",
        description="この画像専用の追加ネガティブプロンプト",
    )
    controlnet_image_path: str = Field(
        default="",
        description="ControlNet参照画像パス",
    )
    ip_adapter_image_path: str = Field(
        default="",
        description="IP-Adapter参照画像パス",
    )
    width: int = Field(default=1024, ge=512, le=2048, description="画像幅")
    height: int = Field(default=1024, ge=512, le=2048, description="画像高さ")
    cfg_scale: float = Field(
        default=7.0,
        ge=1.0,
        le=20.0,
        description="CFGスケール",
    )
    steps: int = Field(default=30, ge=10, le=100, description="サンプリングステップ数")
    sampler: str = Field(
        default="euler_ancestral",
        description="サンプラー名",
    )


class PromptPlanInput(BaseModel):
    """PromptPlannerAgentへの入力."""

    intent: IntentAnalysis = Field(..., description="解析済みデザイン意図")
    brand_colors: list[str] = Field(
        default_factory=list,
        description="ブランドカラー(16進数コード)",
    )
    aspect_ratio: str = Field(default="1:1", description="ターゲットアスペクト比")
    reference_image_paths: list[str] = Field(
        default_factory=list,
        description="スタイル一致のための参照画像",
    )


class PromptPlanOutput(BaseModel):
    """完全なプロンプト計画 - PromptPlannerAgentの出力."""

    design_concept: str = Field(
        ...,
        max_length=200,
        description="一文でのデザインコンセプト",
    )
    global_style: GlobalStyle = Field(..., description="グローバルスタイル定義")
    images: list[ImageSpec] = Field(
        ...,
        min_length=1,
        max_length=20,
        description="個別画像仕様",
    )
    consistency_seed: int = Field(
        default=-1,
        description="一貫性のためのグローバルシード(-1でランダム)",
    )


# =============================================================================
# WorkflowExecutorAgent 入出力
# =============================================================================


class WorkflowExecutorInput(BaseModel):
    """WorkflowExecutorAgentへの入力."""

    prompt_plan: PromptPlanOutput = Field(..., description="完全なプロンプト計画")
    output_directory: str = Field(
        default="/tmp/design_output",
        description="画像出力ディレクトリ",
    )
    save_locally: bool = Field(
        default=True,
        description="ローカルディスクに画像を保存するか",
    )


class GeneratedImage(BaseModel):
    """生成された個別画像の結果."""

    image_id: str = Field(..., description="ImageSpec.image_idと対応")
    role: ImageRole = Field(..., description="画像役割")
    file_path: str = Field(..., description="出力ファイルパス")
    prompt_used: str = Field(..., description="実際に使用されたプロンプト")
    seed_used: int = Field(..., description="実際に使用されたシード")
    generation_time_seconds: float = Field(
        default=0.0,
        description="生成時間(秒)",
    )


class WorkflowResult(BaseModel):
    """ワークフロー実行結果全体."""

    images: list[GeneratedImage] = Field(..., description="生成画像リスト")
    output_directory: str = Field(..., description="出力ディレクトリ")
    total_generation_time_seconds: float = Field(
        ...,
        description="総生成時間(秒)",
    )
    errors: list[str] = Field(
        default_factory=list,
        description="発生した非致命的エラー",
    )
