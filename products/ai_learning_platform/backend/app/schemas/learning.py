# backend/app/schemas/learning.py
# 学習関連のPydanticスキーマ
from pydantic import BaseModel
from typing import List, Optional
from datetime import datetime
from enum import Enum

class ContentType(str, Enum):
    """コンテンツタイプ列挙"""
    MARKDOWN = "markdown"
    VIDEO = "video"
    QUIZ = "quiz"
    SIMULATION = "simulation"

class ProgressStatus(str, Enum):
    """進捗ステータス列挙"""
    NOT_STARTED = "not_started"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"

class SubmissionType(str, Enum):
    """提出タイプ列挙"""
    EMAIL = "email"
    REPORT = "report"
    PRESENTATION = "presentation"

class ChapterBase(BaseModel):
    """チャプターベーススキーマ"""
    title: str
    description: Optional[str] = None
    order_index: int

class Chapter(ChapterBase):
    """チャプターレスポンススキーマ"""
    id: int
    created_at: datetime
    
    class Config:
        from_attributes = True

class LearningContentBase(BaseModel):
    """学習コンテンツベーススキーマ"""
    chapter_id: int
    title: str
    content_type: ContentType
    content_path: Optional[str] = None
    order_index: Optional[int] = None

class LearningContentCreate(LearningContentBase):
    """学習コンテンツ作成スキーマ"""
    pass

class LearningContent(LearningContentBase):
    """学習コンテンツレスポンススキーマ"""
    id: int
    created_at: datetime
    
    class Config:
        from_attributes = True

class UserProgressBase(BaseModel):
    """ユーザー進捗ベーススキーマ"""
    user_id: int
    content_id: int
    status: ProgressStatus = ProgressStatus.NOT_STARTED

class UserProgressUpdate(BaseModel):
    """ユーザー進捗更新スキーマ"""
    status: ProgressStatus

class UserProgress(UserProgressBase):
    """ユーザー進捗レスポンススキーマ"""
    id: int
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    last_accessed: datetime
    
    class Config:
        from_attributes = True

class SubmissionCreate(BaseModel):
    """提出物作成スキーマ"""
    content_type: SubmissionType
    title: Optional[str] = None
    content: str

class Submission(BaseModel):
    """提出物レスポンススキーマ"""
    id: int
    user_id: int
    content_type: SubmissionType
    title: Optional[str] = None
    content: str
    submitted_at: datetime
    
    class Config:
        from_attributes = True

class FeedbackResponse(BaseModel):
    """フィードバックレスポンススキーマ"""
    id: int
    submission_id: int
    feedback_text: str
    generated_at: datetime
    
    class Config:
        from_attributes = True

class ChapterWithContent(Chapter):
    """コンテンツ付きチャプタースキーマ"""
    contents: List[LearningContent] = []

class LearningPathRecommendation(BaseModel):
    """学習パス推奨スキーマ"""
    recommended_chapters: List[Chapter]
    personalized_message: str
    estimated_duration: str  # "約4週間"など
