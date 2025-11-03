# backend/app/schemas/skill_question.py
from pydantic import BaseModel
from typing import Optional, List

class QuestionOption(BaseModel):
    key: str
    text: str

class SkillQuestionBase(BaseModel):
    category: str
    question: str
    options: List[QuestionOption]
    correct_answer: str
    explanation: Optional[str] = None

class SkillQuestionCreate(SkillQuestionBase):
    pass

class SkillQuestion(SkillQuestionBase):
    id: int

    class Config:
        from_attributes = True