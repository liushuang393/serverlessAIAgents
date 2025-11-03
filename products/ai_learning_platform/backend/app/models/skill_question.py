# backend/app/models/skill_question.py
from sqlalchemy import Column, Integer, String, Text
from app.database import Base

class SkillQuestion(Base):
    __tablename__ = "skill_questions"

    id = Column(Integer, primary_key=True, index=True)
    category = Column(String(100), index=True)  # e.g., "Prompt Engineering"
    question = Column(Text, nullable=False)
    options = Column(Text)  # JSON string of options
    correct_answer = Column(String(10), nullable=False)
    explanation = Column(Text)