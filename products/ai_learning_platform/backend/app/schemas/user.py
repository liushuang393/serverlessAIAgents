# backend/app/schemas/user.py
# ユーザー関連のPydanticスキーマ
from pydantic import BaseModel, EmailStr
from typing import Optional
from datetime import datetime

class UserBase(BaseModel):
    """ユーザーベーススキーマ"""
    username: str
    email: EmailStr
    full_name: Optional[str] = None
    bio: Optional[str] = None
    is_active: bool = True

class UserCreate(BaseModel):
    """ユーザー作成スキーマ"""
    username: str
    email: EmailStr
    password: str
    full_name: Optional[str] = None

class UserUpdate(BaseModel):
    """ユーザー更新スキーマ"""
    username: Optional[str] = None
    email: Optional[EmailStr] = None
    full_name: Optional[str] = None
    bio: Optional[str] = None
    is_active: Optional[bool] = None

class UserInDB(UserBase):
    """データベース内のユーザー（パスワードハッシュ含む）"""
    id: int
    hashed_password: str
    is_admin: bool = False
    created_at: datetime
    updated_at: datetime
    last_login: Optional[datetime] = None

    class Config:
        from_attributes = True

class User(UserBase):
    """APIレスポンス用ユーザースキーマ"""
    id: int
    is_admin: bool = False
    created_at: datetime
    updated_at: datetime
    last_login: Optional[datetime] = None

    class Config:
        from_attributes = True

class UserLogin(BaseModel):
    """ログインリクエストスキーマ"""
    username: str
    password: str

class Token(BaseModel):
    """JWTトークンレスポンススキーマ"""
    access_token: str
    token_type: str = "bearer"
    expires_in: int
    user: User

class TokenData(BaseModel):
    """トークンデータスキーマ"""
    username: Optional[str] = None
