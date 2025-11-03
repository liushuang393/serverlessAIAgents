
# backend/app/core/config.py
# アプリケーション設定 - 環境変数と設定管理
import os
from typing import List, Optional
from pydantic import BaseSettings, validator
from dotenv import load_dotenv

# 環境変数を読み込み
load_dotenv()

class Settings(BaseSettings):
    """
    アプリケーション設定クラス
    環境変数から設定を読み込み
    """
    
    # プロジェクト基本情報
    PROJECT_NAME: str = "AI学習プラットフォーム"
    PROJECT_VERSION: str = "1.0.0"
    PROJECT_DESCRIPTION: str = "個人化されたAI・機械学習学習プラットフォーム"
    
    # API設定
    API_V1_STR: str = "/api/v1"
    
    # セキュリティ設定
    SECRET_KEY: str = os.getenv("SECRET_KEY")

    @validator("SECRET_KEY")
    def validate_secret_key(cls, v):
        if not v or v == "your-secret-key-change-in-production":
            raise ValueError("本番環境用のSECRET_KEYを設定してください")
        return v

    ACCESS_TOKEN_EXPIRE_MINUTES: int = 30
    
    # データベース設定
    DATABASE_URL: str = os.getenv(
        "DATABASE_URL",
        "mysql+pymysql://root:1234@localhost:3306/ai_learning_platform"
    )
    
    # OpenAI API設定
    OPENAI_API_KEY: str = os.getenv("OPENAI_API_KEY", "")
    
    @validator("OPENAI_API_KEY")
    def validate_openai_key(cls, v):
        if not v:
            raise ValueError("OPENAI_API_KEY環境変数が設定されていません")
        return v
    
    # CORS設定
    BACKEND_CORS_ORIGINS: List[str] = [
        "http://localhost:3000",  # React開発サーバー
        "http://localhost:8000",  # FastAPI開発サーバー
        "http://127.0.0.1:3000",
        "http://127.0.0.1:8000",
    ]
    
    @validator("BACKEND_CORS_ORIGINS", pre=True)
    def assemble_cors_origins(cls, v):
        if isinstance(v, str) and not v.startswith("["):
            return [i.strip() for i in v.split(",")]
        elif isinstance(v, (list, str)):
            return v
        raise ValueError(v)
    
    # ログ設定
    LOG_LEVEL: str = os.getenv("LOG_LEVEL", "INFO")
    
    # ファイルアップロード設定
    MAX_UPLOAD_SIZE: int = 10 * 1024 * 1024  # 10MB
    UPLOAD_DIR: str = "uploads"
    
    # 学習コンテンツ設定
    CONTENT_DIR: str = "learning_contents"
    
    # Redis設定（キャッシュ用、オプション）
    REDIS_URL: Optional[str] = os.getenv("REDIS_URL")
    
    # メール設定（通知用、オプション）
    SMTP_TLS: bool = True
    SMTP_PORT: Optional[int] = None
    SMTP_HOST: Optional[str] = None
    SMTP_USER: Optional[str] = None
    SMTP_PASSWORD: Optional[str] = None
    EMAILS_FROM_EMAIL: Optional[str] = None
    EMAILS_FROM_NAME: Optional[str] = None
    
    # 開発/本番環境フラグ
    DEBUG: bool = os.getenv("DEBUG", "False").lower() == "true"
    TESTING: bool = os.getenv("TESTING", "False").lower() == "true"
    
    class Config:
        case_sensitive = True
        env_file = ".env"

# 設定インスタンスを作成
settings = Settings()
