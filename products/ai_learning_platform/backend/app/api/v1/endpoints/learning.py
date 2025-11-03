# backend/app/api/v1/endpoints/learning.py
# 学習コンテンツエンドポイント - チャプター、コンテンツ、進捗管理
from fastapi import APIRouter, Depends, HTTPException, status
from sqlalchemy.orm import Session
from typing import List, Optional
import json

from app.database import get_db
from app.models.user import User
from app.schemas.learning import (
    Chapter,
    ChapterWithContent,
    LearningContent,
    LearningContentCreate,
    UserProgress,
    UserProgressUpdate,
    SubmissionCreate,
    Submission,
    FeedbackResponse,
    LearningPathRecommendation
)
from app.core.auth import get_current_active_user
from app.core.openai_client import generate_feedback, generate_learning_content

router = APIRouter(prefix="/learning", tags=["学習"])

@router.get("/chapters", response_model=List[Chapter])
def get_all_chapters(db: Session = Depends(get_db)):
    """
    全チャプター一覧を取得
    
    Returns:
        チャプターのリスト（順序付き）
    """
    from app.models.chapter import Chapter as ChapterModel
    
    chapters = db.query(ChapterModel).order_by(ChapterModel.order_index).all()
    return chapters

@router.get("/chapters/{chapter_id}", response_model=ChapterWithContent)
def get_chapter_with_content(
    chapter_id: int,
    db: Session = Depends(get_db),
    current_user: User = Depends(get_current_active_user)
):
    """
    指定されたチャプターとその学習コンテンツを取得
    
    Args:
        chapter_id: チャプターID
        db: データベースセッション
        current_user: 認証済みユーザー
        
    Returns:
        チャプター情報と学習コンテンツ
    """
    from app.models.chapter import Chapter as ChapterModel
    from app.models.learning_content import LearningContent as LearningContentModel
    
    chapter = db.query(ChapterModel).filter(ChapterModel.id == chapter_id).first()
    if not chapter:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="指定されたチャプターが見つかりません"
        )
    
    contents = db.query(LearningContentModel).filter(
        LearningContentModel.chapter_id == chapter_id
    ).order_by(LearningContentModel.order_index).all()
    
    return ChapterWithContent(
        id=chapter.id,
        title=chapter.title,
        description=chapter.description,
        order_index=chapter.order_index,
        created_at=chapter.created_at,
        contents=contents
    )

@router.get("/content/{content_id}", response_model=LearningContent)
def get_learning_content(
    content_id: int,
    db: Session = Depends(get_db),
    current_user: User = Depends(get_current_active_user)
):
    """
    指定された学習コンテンツを取得
    
    Args:
        content_id: コンテンツID
        db: データベースセッション
        current_user: 認証済みユーザー
        
    Returns:
        学習コンテンツ
    """
    from app.models.learning_content import LearningContent as LearningContentModel
    
    content = db.query(LearningContentModel).filter(
        LearningContentModel.id == content_id
    ).first()
    
    if not content:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="指定された学習コンテンツが見つかりません"
        )
    
    return content

@router.get("/progress", response_model=List[UserProgress])
def get_user_progress(
    current_user: User = Depends(get_current_active_user),
    db: Session = Depends(get_db)
):
    """
    現在のユーザーの学習進捗を取得
    
    Args:
        current_user: 認証済みユーザー
        db: データベースセッション
        
    Returns:
        ユーザーの学習進捗リスト
    """
    from app.models.user_progress import UserProgress as UserProgressModel
    
    progress = db.query(UserProgressModel).filter(
        UserProgressModel.user_id == current_user.id
    ).all()
    
    return progress

@router.put("/progress/{content_id}", response_model=UserProgress)
def update_user_progress(
    content_id: int,
    progress_update: UserProgressUpdate,
    current_user: User = Depends(get_current_active_user),
    db: Session = Depends(get_db)
):
    """
    指定されたコンテンツの学習進捗を更新
    
    Args:
        content_id: コンテンツID
        progress_update: 進捗更新データ
        current_user: 認証済みユーザー
        db: データベースセッション
        
    Returns:
        更新された進捗情報
    """
    from app.models.user_progress import UserProgress as UserProgressModel
    from datetime import datetime
    
    # 既存の進捗を検索
    progress = db.query(UserProgressModel).filter(
        UserProgressModel.user_id == current_user.id,
        UserProgressModel.content_id == content_id
    ).first()
    
    if not progress:
        # 新しい進捗レコードを作成
        progress = UserProgressModel(
            user_id=current_user.id,
            content_id=content_id,
            status=progress_update.status
        )
        db.add(progress)
    else:
        # 既存の進捗を更新
        progress.status = progress_update.status
        progress.last_accessed = datetime.utcnow()
        
        # ステータスに応じてタイムスタンプを更新
        if progress_update.status == "in_progress" and not progress.started_at:
            progress.started_at = datetime.utcnow()
        elif progress_update.status == "completed":
            progress.completed_at = datetime.utcnow()
    
    db.commit()
    db.refresh(progress)
    return progress

@router.post("/submissions", response_model=Submission)
def create_submission(
    submission_data: SubmissionCreate,
    current_user: User = Depends(get_current_active_user),
    db: Session = Depends(get_db)
):
    """
    学習成果物を提出
    
    Args:
        submission_data: 提出データ
        current_user: 認証済みユーザー
        db: データベースセッション
        
    Returns:
        作成された提出物情報
    """
    from app.models.submission import Submission as SubmissionModel
    
    submission = SubmissionModel(
        user_id=current_user.id,
        content_type=submission_data.content_type,
        title=submission_data.title,
        content=submission_data.content
    )
    
    db.add(submission)
    db.commit()
    db.refresh(submission)
    
    return submission

@router.get("/submissions/{submission_id}/feedback", response_model=FeedbackResponse)
def get_ai_feedback(
    submission_id: int,
    current_user: User = Depends(get_current_active_user),
    db: Session = Depends(get_db)
):
    """
    提出物に対するAIフィードバックを取得
    
    Args:
        submission_id: 提出物ID
        current_user: 認証済みユーザー
        db: データベースセッション
        
    Returns:
        AIが生成したフィードバック
    """
    from app.models.submission import Submission as SubmissionModel
    from app.models.feedback import Feedback as FeedbackModel
    
    # 提出物を確認
    submission = db.query(SubmissionModel).filter(
        SubmissionModel.id == submission_id,
        SubmissionModel.user_id == current_user.id
    ).first()
    
    if not submission:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="指定された提出物が見つかりません"
        )
    
    # 既存のフィードバックを確認
    existing_feedback = db.query(FeedbackModel).filter(
        FeedbackModel.submission_id == submission_id
    ).first()
    
    if existing_feedback:
        return existing_feedback
    
    # 新しいフィードバックを生成
    feedback_text = generate_feedback(
        submission.content,
        submission.content_type
    )
    
    feedback = FeedbackModel(
        submission_id=submission_id,
        feedback_text=feedback_text
    )
    
    db.add(feedback)
    db.commit()
    db.refresh(feedback)
    
    return feedback

@router.get("/recommendations", response_model=LearningPathRecommendation)
def get_learning_recommendations(
    current_user: User = Depends(get_current_active_user),
    db: Session = Depends(get_db)
):
    """
    ユーザーの技能プロフィールに基づく学習推奨を取得
    
    Args:
        current_user: 認証済みユーザー
        db: データベースセッション
        
    Returns:
        個人化された学習推奨
    """
    # TODO: ユーザーの技能プロフィールを基に推奨を生成
    # 現在は基本的な推奨を返す
    from app.models.chapter import Chapter as ChapterModel
    
    chapters = db.query(ChapterModel).order_by(ChapterModel.order_index).limit(3).all()
    
    return LearningPathRecommendation(
        recommended_chapters=chapters,
        personalized_message="AI・機械学習の基礎から始めることをお勧めします。",
        estimated_duration="約6週間"
    )
