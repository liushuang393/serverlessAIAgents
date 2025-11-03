# backend/app/schemas/__init__.py
# Pydanticスキーマ - APIリクエスト/レスポンスモデル

from .user import (
    UserBase,
    UserCreate,
    UserUpdate,
    UserInDB,
    User,
    Token,
    TokenData,
    UserLogin
)

from .skill_assessment import (
    SkillQuestionBase,
    SkillQuestion,
    SkillQuestionCreate,
    UserAnswerCreate,
    SkillAssessmentResult
)

from .learning import (
    ChapterBase,
    Chapter,
    LearningContentBase,
    LearningContent,
    UserProgressBase,
    UserProgress,
    SubmissionCreate,
    FeedbackResponse
)

__all__ = [
    # User schemas
    "UserBase",
    "UserCreate", 
    "UserUpdate",
    "UserInDB",
    "User",
    "Token",
    "TokenData",
    "UserLogin",
    
    # Skill assessment schemas
    "SkillQuestionBase",
    "SkillQuestion",
    "SkillQuestionCreate", 
    "UserAnswerCreate",
    "SkillAssessmentResult",
    
    # Learning schemas
    "ChapterBase",
    "Chapter",
    "LearningContentBase",
    "LearningContent",
    "UserProgressBase", 
    "UserProgress",
    "SubmissionCreate",
    "FeedbackResponse"
]
