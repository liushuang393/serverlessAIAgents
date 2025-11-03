# backend/app/schemas/skill_assessment.py
# 技能評価関連のPydanticスキーマ
from pydantic import BaseModel
from typing import List, Dict, Any, Optional
from datetime import datetime

class SkillQuestionBase(BaseModel):
    """技能質問ベーススキーマ"""
    category: str
    question: str
    options: Dict[str, str]  # {"A": "選択肢1", "B": "選択肢2", ...}
    correct_answer: str
    explanation: Optional[str] = None

class SkillQuestionCreate(SkillQuestionBase):
    """技能質問作成スキーマ"""
    pass

class SkillQuestion(SkillQuestionBase):
    """技能質問レスポンススキーマ"""
    id: int
    
    class Config:
        from_attributes = True

class SkillQuestionPublic(BaseModel):
    """公開用技能質問スキーマ（正解を含まない）"""
    id: int
    category: str
    question: str
    options: Dict[str, str]
    
    class Config:
        from_attributes = True

class UserAnswerCreate(BaseModel):
    """ユーザー回答作成スキーマ"""
    question_id: int
    selected_option: str

class UserAnswerBatch(BaseModel):
    """ユーザー回答一括送信スキーマ"""
    answers: List[UserAnswerCreate]

class SkillAssessmentResult(BaseModel):
    """技能評価結果スキーマ"""
    overall_score: float
    strengths: List[str]
    weaknesses: List[str]
    recommendations: List[Dict[str, Any]]
    detailed_analysis: str
    
class UserSkillProfile(BaseModel):
    """ユーザー技能プロフィールスキーマ"""
    id: int
    user_id: int
    overall_score: float
    strengths: List[str]
    weaknesses: List[str] 
    recommendations: List[Dict[str, Any]]
    generated_at: datetime
    
    class Config:
        from_attributes = True
