# backend/app/models/learning_content.py
# 学習コンテンツモデル - 各種学習素材の管理
from sqlalchemy import Column, Integer, String, Text, DateTime, ForeignKey, Enum
from sqlalchemy.sql import func
from sqlalchemy.orm import relationship
import enum
from app.database import Base

class ContentType(enum.Enum):
    """コンテンツタイプ列挙"""
    MARKDOWN = "markdown"
    VIDEO = "video"
    QUIZ = "quiz"
    SIMULATION = "simulation"

class LearningContent(Base):
    """
    学習コンテンツテーブル
    チャプター内の個別学習素材を管理
    """
    __tablename__ = "learning_contents"

    id = Column(Integer, primary_key=True, index=True)
    chapter_id = Column(Integer, ForeignKey("chapters.id"), nullable=False)
    title = Column(String(255), nullable=False)
    content_type = Column(Enum(ContentType), nullable=False)
    content_path = Column(String(500))  # ファイルパスまたはURL
    order_index = Column(Integer)  # チャプター内のコンテンツ順序
    
    # タイムスタンプ
    created_at = Column(DateTime(timezone=True), server_default=func.now())
    
    # リレーション
    chapter = relationship("Chapter", back_populates="contents")
    user_progress = relationship("UserProgress", back_populates="content", cascade="all, delete-orphan")

    def __repr__(self):
        return f"<LearningContent(id={self.id}, title='{self.title}', type='{self.content_type.value}')>"
