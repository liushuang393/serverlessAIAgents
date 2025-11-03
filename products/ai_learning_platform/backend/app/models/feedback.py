# backend/app/models/feedback.py
# フィードバックモデル - AI生成フィードバック管理
from sqlalchemy import Column, Integer, Text, DateTime, ForeignKey
from sqlalchemy.sql import func
from sqlalchemy.orm import relationship
from app.database import Base

class Feedback(Base):
    """
    AIフィードバックテーブル
    提出物に対するAI生成フィードバックを管理
    """
    __tablename__ = "ai_feedbacks"

    id = Column(Integer, primary_key=True, index=True)
    submission_id = Column(Integer, ForeignKey("user_submissions.id"), nullable=False)
    feedback_text = Column(Text, nullable=False)  # AI生成のフィードバック内容
    
    # タイムスタンプ
    generated_at = Column(DateTime(timezone=True), server_default=func.now())
    
    # リレーション
    submission = relationship("Submission", back_populates="feedbacks")

    def __repr__(self):
        return f"<Feedback(id={self.id}, submission_id={self.submission_id})>"
