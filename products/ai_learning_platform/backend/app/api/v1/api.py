# backend/app/api/v1/api.py
# APIルーター - 全エンドポイントの統合
from fastapi import APIRouter

from app.api.v1.endpoints import auth, skill_assessment, learning

api_router = APIRouter()

# 各エンドポイントを統合
api_router.include_router(auth.router)
api_router.include_router(skill_assessment.router)
api_router.include_router(learning.router)
