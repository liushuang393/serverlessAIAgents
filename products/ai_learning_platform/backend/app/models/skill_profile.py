# backend/app/models/skill_profile.py
# 技能プロフィールモデル - ユーザーの技能評価結果管理
from sqlalchemy import Column, Integer, DECIMAL, JSON, DateTime, ForeignKey
from sqlalchemy.sql import func
from sqlalchemy.orm import relationship
from app.database import Base

class UserSkillProfile(Base):
    """
    ユーザー技能プロフィールテーブル
    技能評価の結果と推奨学習内容を管理
    """
    __tablename__ = "user_skill_profiles"

    id = Column(Integer, primary_key=True, index=True)
    user_id = Column(Integer, ForeignKey("users.id"), nullable=False, unique=True)
    overall_score = Column(DECIMAL(5, 2))  # 総合スコア (0-100)
    strengths = Column(JSON)  # 強み一覧、例：["Prompt Engineering", "Text Classification"]
    weaknesses = Column(JSON)  # 改善項目一覧
    recommendations = Column(JSON)  # 学習提案、例：[{"chapter_id": 3, "reason": "強化が必要..."}]
    
    # タイムスタンプ
    generated_at = Column(DateTime(timezone=True), server_default=func.now())
    updated_at = Column(DateTime(timezone=True), server_default=func.now(), onupdate=func.now())
    
    # リレーション
    user = relationship("User")

    def __repr__(self):
        return f"<UserSkillProfile(user_id={self.user_id}, score={self.overall_score})>"
