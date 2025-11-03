# backend/app/models/user_progress.py
# ユーザー進捗モデル - 学習進捗の追跡
from sqlalchemy import Column, Integer, DateTime, ForeignKey, Enum, UniqueConstraint
from sqlalchemy.sql import func
from sqlalchemy.orm import relationship
import enum
from app.database import Base

class ProgressStatus(enum.Enum):
    """進捗ステータス列挙"""
    NOT_STARTED = "not_started"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"

class UserProgress(Base):
    """
    ユーザー学習進捗テーブル
    各ユーザーの学習コンテンツ進捗を追跡
    """
    __tablename__ = "user_learning_progress"

    id = Column(Integer, primary_key=True, index=True)
    user_id = Column(Integer, ForeignKey("users.id"), nullable=False)
    content_id = Column(Integer, ForeignKey("learning_contents.id"), nullable=False)
    status = Column(Enum(ProgressStatus), default=ProgressStatus.NOT_STARTED)
    
    # タイムスタンプ
    started_at = Column(DateTime(timezone=True))
    completed_at = Column(DateTime(timezone=True))
    last_accessed = Column(DateTime(timezone=True), server_default=func.now(), onupdate=func.now())
    
    # ユニーク制約
    __table_args__ = (UniqueConstraint('user_id', 'content_id', name='unique_user_content'),)
    
    # リレーション
    user = relationship("User")
    content = relationship("LearningContent", back_populates="user_progress")

    def __repr__(self):
        return f"<UserProgress(user_id={self.user_id}, content_id={self.content_id}, status='{self.status.value}')>"
