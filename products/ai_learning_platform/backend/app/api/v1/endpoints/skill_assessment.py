# backend/app/api/v1/endpoints/skill_assessment.py
# 技能評価エンドポイント - 診断テスト、結果分析、学習推奨
from fastapi import APIRouter, Depends, HTTPException, status
from sqlalchemy.orm import Session
from typing import List, Dict, Any
import json
from datetime import datetime

from app.database import get_db
from app.models.skill_question import SkillQuestion
from app.models.user import User
from app.schemas.skill_assessment import (
    SkillQuestionPublic,
    UserAnswerBatch,
    SkillAssessmentResult,
    UserSkillProfile
)
from app.core.auth import get_current_active_user
from app.core.openai_client import analyze_user_answers

router = APIRouter(prefix="/skill-assessment", tags=["技能評価"])

@router.get("/categories", response_model=List[str])
def get_skill_categories(db: Session = Depends(get_db)):
    """
    利用可能な技能カテゴリ一覧を取得

    Returns:
        技能カテゴリのリスト
    """
    categories = db.query(SkillQuestion.category).distinct().all()
    return [category[0] for category in categories]

@router.get("/questions/{category}", response_model=List[SkillQuestionPublic])
def get_questions_by_category(
    category: str,
    db: Session = Depends(get_db),
    current_user: User = Depends(get_current_active_user)
):
    """
    指定されたカテゴリの技能評価問題を取得

    Args:
        category: 技能カテゴリ
        db: データベースセッション
        current_user: 認証済みユーザー

    Returns:
        技能評価問題のリスト（正解は含まない）
    """
    questions = db.query(SkillQuestion).filter(
        SkillQuestion.category == category
    ).all()

    if not questions:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"カテゴリ '{category}' の問題が見つかりません"
        )

    # 正解を除外して返す
    return [
        SkillQuestionPublic(
            id=q.id,
            category=q.category,
            question=q.question,
            options=json.loads(q.options) if isinstance(q.options, str) else q.options
        )
        for q in questions
    ]

@router.get("/questions", response_model=List[SkillQuestionPublic])
def get_all_questions(
    limit: int = 20,
    db: Session = Depends(get_db),
    current_user: User = Depends(get_current_active_user)
):
    """
    全カテゴリの技能評価問題を取得（制限付き）

    Args:
        limit: 取得する問題数の上限
        db: データベースセッション
        current_user: 認証済みユーザー

    Returns:
        技能評価問題のリスト
    """
    questions = db.query(SkillQuestion).limit(limit).all()

    return [
        SkillQuestionPublic(
            id=q.id,
            category=q.category,
            question=q.question,
            options=json.loads(q.options) if isinstance(q.options, str) else q.options
        )
        for q in questions
    ]

@router.post("/submit", response_model=SkillAssessmentResult)
def submit_assessment_answers(
    answers: UserAnswerBatch,
    db: Session = Depends(get_db),
    current_user: User = Depends(get_current_active_user)
):
    """
    技能評価の回答を提出して分析結果を取得

    Args:
        answers: ユーザーの回答データ
        db: データベースセッション
        current_user: 認証済みユーザー

    Returns:
        AI分析による技能評価結果
    """
    try:
        # 回答データを整理
        answer_data = {}
        correct_count = 0
        total_count = len(answers.answers)

        for answer in answers.answers:
            # 問題情報を取得
            question = db.query(SkillQuestion).filter(
                SkillQuestion.id == answer.question_id
            ).first()

            if not question:
                continue

            is_correct = answer.selected_option == question.correct_answer
            if is_correct:
                correct_count += 1

            answer_data[str(answer.question_id)] = {
                "category": question.category,
                "question": question.question,
                "selected_option": answer.selected_option,
                "correct_answer": question.correct_answer,
                "is_correct": is_correct
            }

        # 基本スコアを計算
        basic_score = (correct_count / total_count * 100) if total_count > 0 else 0

        # OpenAI APIで詳細分析
        analysis_result = analyze_user_answers({
            "user_id": current_user.id,
            "username": current_user.username,
            "basic_score": basic_score,
            "correct_count": correct_count,
            "total_count": total_count,
            "answers": answer_data
        })

        # 結果をデータベースに保存（user_skill_profilesテーブル）
        # TODO: 実装する場合はここでデータベース保存処理を追加

        return SkillAssessmentResult(**analysis_result)

    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"技能評価の処理中にエラーが発生しました: {str(e)}"
        )