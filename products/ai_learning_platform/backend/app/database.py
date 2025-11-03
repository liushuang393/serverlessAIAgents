# backend/app/database.py
# データベース設定 - SQLAlchemy設定とセッション管理
import os
from sqlalchemy import create_engine
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker
from dotenv import load_dotenv

# 環境変数を読み込み
load_dotenv()

# データベースURL
DATABASE_URL = os.getenv(
    "DATABASE_URL",
    "mysql+pymysql://root:1234@localhost:3306/ai_learning_platform"
)

# SQLAlchemyエンジンを作成
engine = create_engine(
    DATABASE_URL,
    echo=True,  # 開発時はSQL文をログ出力
    pool_pre_ping=True,  # 接続プールの健全性チェック
    pool_recycle=3600,  # 1時間で接続をリサイクル
)

# セッションメーカーを作成
SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)

# ベースクラスを作成
Base = declarative_base()

def get_db():
    """
    データベースセッションを取得
    FastAPIの依存性注入で使用

    Yields:
        データベースセッション
    """
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()

def create_tables():
    """
    全テーブルを作成
    アプリケーション起動時に実行
    """
    Base.metadata.create_all(bind=engine)
