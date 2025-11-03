# backend/app/models/chapter.py
# チャプターモデル - 学習章の管理
from sqlalchemy import Column, Integer, String, Text, DateTime
from sqlalchemy.sql import func
from sqlalchemy.orm import relationship
from app.database import Base

class Chapter(Base):
    """
    チャプターテーブル
    学習コンテンツの章を管理
    """
    __tablename__ = "chapters"

    id = Column(Integer, primary_key=True, index=True)
    title = Column(String(255), nullable=False)
    description = Column(Text)
    order_index = Column(Integer, unique=True, nullable=False)  # チャプター順序
    
    # タイムスタンプ
    created_at = Column(DateTime(timezone=True), server_default=func.now())
    
    # リレーション
    contents = relationship("LearningContent", back_populates="chapter", cascade="all, delete-orphan")

    def __repr__(self):
        return f"<Chapter(id={self.id}, title='{self.title}', order={self.order_index})>"
