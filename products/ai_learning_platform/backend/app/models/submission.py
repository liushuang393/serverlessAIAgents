# backend/app/models/submission.py
# 提出物モデル - ユーザーの学習成果物管理
from sqlalchemy import Column, Integer, String, Text, DateTime, ForeignKey, Enum
from sqlalchemy.sql import func
from sqlalchemy.orm import relationship
import enum
from app.database import Base

class SubmissionType(enum.Enum):
    """提出タイプ列挙"""
    EMAIL = "email"
    REPORT = "report"
    PRESENTATION = "presentation"

class Submission(Base):
    """
    ユーザー提出物テーブル
    学習者が提出した成果物を管理
    """
    __tablename__ = "user_submissions"

    id = Column(Integer, primary_key=True, index=True)
    user_id = Column(Integer, ForeignKey("users.id"), nullable=False)
    content_type = Column(Enum(SubmissionType), nullable=False)
    title = Column(String(255))
    content = Column(Text, nullable=False)  # ユーザーが提出した原文内容
    
    # タイムスタンプ
    submitted_at = Column(DateTime(timezone=True), server_default=func.now())
    
    # リレーション
    user = relationship("User")
    feedbacks = relationship("Feedback", back_populates="submission", cascade="all, delete-orphan")

    def __repr__(self):
        return f"<Submission(id={self.id}, user_id={self.user_id}, type='{self.content_type.value}')>"
