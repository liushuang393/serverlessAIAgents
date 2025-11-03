# backend/app/core/auth.py
# JWT認証システム - ユーザー認証とセッション管理
from datetime import datetime, timedelta
from typing import Optional, Dict, Any
from jose import JWTError, jwt
from passlib.context import CryptContext
from fastapi import HTTPException, status, Depends
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from sqlalchemy.orm import Session
import os
from dotenv import load_dotenv

from app.database import get_db
from app.models.user import User

# 環境変数を読み込み
load_dotenv()

# パスワードハッシュ化設定
pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")

# JWT設定
SECRET_KEY = os.getenv("SECRET_KEY", "your-secret-key-here")
ALGORITHM = "HS256"
ACCESS_TOKEN_EXPIRE_MINUTES = 30

# HTTPベアラー認証
security = HTTPBearer()

def verify_password(plain_password: str, hashed_password: str) -> bool:
    """
    プレーンテキストパスワードとハッシュ化パスワードを検証
    
    Args:
        plain_password: プレーンテキストパスワード
        hashed_password: ハッシュ化されたパスワード
        
    Returns:
        パスワードが一致するかどうか
    """
    return pwd_context.verify(plain_password, hashed_password)

def get_password_hash(password: str) -> str:
    """
    パスワードをハッシュ化
    
    Args:
        password: プレーンテキストパスワード
        
    Returns:
        ハッシュ化されたパスワード
    """
    return pwd_context.hash(password)

def create_access_token(data: Dict[str, Any], expires_delta: Optional[timedelta] = None) -> str:
    """
    JWTアクセストークンを作成
    
    Args:
        data: トークンに含めるデータ
        expires_delta: トークンの有効期限
        
    Returns:
        JWTトークン
    """
    to_encode = data.copy()
    if expires_delta:
        expire = datetime.utcnow() + expires_delta
    else:
        expire = datetime.utcnow() + timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)
    
    to_encode.update({"exp": expire})
    encoded_jwt = jwt.encode(to_encode, SECRET_KEY, algorithm=ALGORITHM)
    return encoded_jwt

def verify_token(token: str) -> Optional[Dict[str, Any]]:
    """
    JWTトークンを検証してペイロードを取得
    
    Args:
        token: JWTトークン
        
    Returns:
        トークンのペイロード（無効な場合はNone）
    """
    try:
        payload = jwt.decode(token, SECRET_KEY, algorithms=[ALGORITHM])
        return payload
    except JWTError:
        return None

def authenticate_user(db: Session, username: str, password: str) -> Optional[User]:
    """
    ユーザー認証
    
    Args:
        db: データベースセッション
        username: ユーザー名
        password: パスワード
        
    Returns:
        認証されたユーザー（認証失敗時はNone）
    """
    user = db.query(User).filter(User.username == username).first()
    if not user:
        return None
    if not verify_password(password, user.hashed_password):
        return None
    return user

def get_current_user(
    credentials: HTTPAuthorizationCredentials = Depends(security),
    db: Session = Depends(get_db)
) -> User:
    """
    現在のユーザーを取得（認証必須）
    
    Args:
        credentials: HTTP認証情報
        db: データベースセッション
        
    Returns:
        現在のユーザー
        
    Raises:
        HTTPException: 認証失敗時
    """
    credentials_exception = HTTPException(
        status_code=status.HTTP_401_UNAUTHORIZED,
        detail="認証情報が無効です",
        headers={"WWW-Authenticate": "Bearer"},
    )
    
    try:
        token = credentials.credentials
        payload = verify_token(token)
        if payload is None:
            raise credentials_exception
            
        username: str = payload.get("sub")
        if username is None:
            raise credentials_exception
            
    except JWTError:
        raise credentials_exception
    
    user = db.query(User).filter(User.username == username).first()
    if user is None:
        raise credentials_exception
        
    return user

def get_current_active_user(current_user: User = Depends(get_current_user)) -> User:
    """
    現在のアクティブユーザーを取得
    
    Args:
        current_user: 現在のユーザー
        
    Returns:
        アクティブなユーザー
        
    Raises:
        HTTPException: ユーザーが非アクティブの場合
    """
    if not current_user.is_active:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="非アクティブなユーザーです"
        )
    return current_user

# オプション：管理者権限チェック
def get_admin_user(current_user: User = Depends(get_current_active_user)) -> User:
    """
    管理者権限を持つユーザーを取得
    
    Args:
        current_user: 現在のユーザー
        
    Returns:
        管理者ユーザー
        
    Raises:
        HTTPException: 管理者権限がない場合
    """
    # 管理者フラグがある場合のチェック（必要に応じて実装）
    # if not current_user.is_admin:
    #     raise HTTPException(
    #         status_code=status.HTTP_403_FORBIDDEN,
    #         detail="管理者権限が必要です"
    #     )
    return current_user
