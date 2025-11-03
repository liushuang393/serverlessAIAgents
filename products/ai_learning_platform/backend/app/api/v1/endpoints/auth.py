# backend/app/api/v1/endpoints/auth.py
# 認証エンドポイント - ユーザー登録、ログイン、トークン管理
from datetime import timedelta
from fastapi import APIRouter, Depends, HTTPException, status
from fastapi.security import OAuth2PasswordRequestForm
from sqlalchemy.orm import Session
from sqlalchemy.exc import IntegrityError

from app.database import get_db
from app.models.user import User
from app.schemas.user import UserCreate, User as UserSchema, Token, UserLogin
from app.core.auth import (
    authenticate_user,
    create_access_token,
    get_password_hash,
    get_current_active_user,
    ACCESS_TOKEN_EXPIRE_MINUTES
)

router = APIRouter(prefix="/auth", tags=["認証"])

@router.post("/register", response_model=UserSchema, status_code=status.HTTP_201_CREATED)
def register_user(user_data: UserCreate, db: Session = Depends(get_db)):
    """
    新規ユーザー登録
    
    Args:
        user_data: ユーザー登録データ
        db: データベースセッション
        
    Returns:
        作成されたユーザー情報
        
    Raises:
        HTTPException: ユーザー名またはメールアドレスが既に存在する場合
    """
    try:
        # パスワードをハッシュ化
        hashed_password = get_password_hash(user_data.password)
        
        # 新しいユーザーを作成
        db_user = User(
            username=user_data.username,
            email=user_data.email,
            hashed_password=hashed_password,
            full_name=user_data.full_name,
            is_active=True
        )
        
        db.add(db_user)
        db.commit()
        db.refresh(db_user)
        
        return db_user
        
    except IntegrityError:
        db.rollback()
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="ユーザー名またはメールアドレスが既に使用されています"
        )

@router.post("/login", response_model=Token)
def login_user(user_credentials: UserLogin, db: Session = Depends(get_db)):
    """
    ユーザーログイン
    
    Args:
        user_credentials: ログイン認証情報
        db: データベースセッション
        
    Returns:
        JWTアクセストークンとユーザー情報
        
    Raises:
        HTTPException: 認証失敗時
    """
    user = authenticate_user(db, user_credentials.username, user_credentials.password)
    if not user:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="ユーザー名またはパスワードが正しくありません",
            headers={"WWW-Authenticate": "Bearer"},
        )
    
    # アクセストークンを作成
    access_token_expires = timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)
    access_token = create_access_token(
        data={"sub": user.username}, expires_delta=access_token_expires
    )
    
    # ログイン時刻を更新
    from datetime import datetime
    user.last_login = datetime.utcnow()
    db.commit()
    
    return {
        "access_token": access_token,
        "token_type": "bearer",
        "expires_in": ACCESS_TOKEN_EXPIRE_MINUTES * 60,  # 秒単位
        "user": user
    }

@router.post("/token", response_model=Token)
def login_for_access_token(
    form_data: OAuth2PasswordRequestForm = Depends(),
    db: Session = Depends(get_db)
):
    """
    OAuth2互換のトークン取得エンドポイント
    
    Args:
        form_data: OAuth2フォームデータ
        db: データベースセッション
        
    Returns:
        JWTアクセストークンとユーザー情報
    """
    user = authenticate_user(db, form_data.username, form_data.password)
    if not user:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="ユーザー名またはパスワードが正しくありません",
            headers={"WWW-Authenticate": "Bearer"},
        )
    
    access_token_expires = timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)
    access_token = create_access_token(
        data={"sub": user.username}, expires_delta=access_token_expires
    )
    
    # ログイン時刻を更新
    from datetime import datetime
    user.last_login = datetime.utcnow()
    db.commit()
    
    return {
        "access_token": access_token,
        "token_type": "bearer", 
        "expires_in": ACCESS_TOKEN_EXPIRE_MINUTES * 60,
        "user": user
    }

@router.get("/me", response_model=UserSchema)
def get_current_user_info(current_user: User = Depends(get_current_active_user)):
    """
    現在のユーザー情報を取得
    
    Args:
        current_user: 認証済みユーザー
        
    Returns:
        現在のユーザー情報
    """
    return current_user

@router.post("/logout")
def logout_user(current_user: User = Depends(get_current_active_user)):
    """
    ユーザーログアウト
    
    Note:
        JWTはステートレスなため、実際のトークン無効化はクライアント側で行う
        必要に応じてブラックリスト機能を実装可能
    
    Args:
        current_user: 認証済みユーザー
        
    Returns:
        ログアウト成功メッセージ
    """
    return {"message": "正常にログアウトしました"}

@router.post("/refresh", response_model=Token)
def refresh_access_token(current_user: User = Depends(get_current_active_user)):
    """
    アクセストークンを更新
    
    Args:
        current_user: 認証済みユーザー
        
    Returns:
        新しいJWTアクセストークン
    """
    access_token_expires = timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)
    access_token = create_access_token(
        data={"sub": current_user.username}, expires_delta=access_token_expires
    )
    
    return {
        "access_token": access_token,
        "token_type": "bearer",
        "expires_in": ACCESS_TOKEN_EXPIRE_MINUTES * 60,
        "user": current_user
    }
