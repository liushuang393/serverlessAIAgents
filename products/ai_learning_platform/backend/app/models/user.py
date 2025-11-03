# backend/app/models/user.py
# ユーザーモデル - データベーステーブル定義
from sqlalchemy import Column, Integer, String, Boolean, DateTime, Text
from sqlalchemy.sql import func
from app.database import Base

class User(Base):
    """
    ユーザーテーブル
    学習プラットフォームのユーザー情報を管理
    """
    __tablename__ = "users"

    id = Column(Integer, primary_key=True, index=True)
    username = Column(String(50), unique=True, index=True, nullable=False)
    email = Column(String(100), unique=True, index=True, nullable=False)
    hashed_password = Column(String(255), nullable=False)
    is_active = Column(Boolean, default=True)
    is_admin = Column(Boolean, default=False)  # 管理者フラグ
    
    # プロフィール情報
    full_name = Column(String(100))
    bio = Column(Text)  # 自己紹介
    
    # タイムスタンプ
    created_at = Column(DateTime(timezone=True), server_default=func.now())
    updated_at = Column(DateTime(timezone=True), server_default=func.now(), onupdate=func.now())
    last_login = Column(DateTime(timezone=True))

    def __repr__(self):
        return f"<User(id={self.id}, username='{self.username}', email='{self.email}')>"
