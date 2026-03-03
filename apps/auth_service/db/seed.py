"""auth_service デフォルトユーザーシード.

DB 再構築時に admin/manager/employee ロールの初期ユーザーを自動作成する。
FAQ System の DEMO_USERS + _seed_demo_users_if_needed() パターンを踏襲。
"""

from __future__ import annotations

import logging
from datetime import UTC, datetime
from typing import Any

from apps.auth_service.core.password import PasswordManager
from apps.auth_service.db.session import get_db_session
from apps.auth_service.models.user import UserAccount
from sqlalchemy import select


logger = logging.getLogger(__name__)

# デフォルトユーザー定義（ローカル DB 初期シード）
DEFAULT_USERS: dict[str, dict[str, Any]] = {
    "admin": {
        "initial_password": "admin123",
        "display_name": "管理者 太郎",
        "department": "情報システム部",
        "position": "部長",
        "role": "admin",
        "email": "admin@example.com",
    },
    "tanaka": {
        "initial_password": "tanaka123",
        "display_name": "田中 一郎",
        "department": "人事部",
        "position": "課長",
        "role": "manager",
        "email": "tanaka@example.com",
    },
    "suzuki": {
        "initial_password": "suzuki123",
        "display_name": "鈴木 花子",
        "department": "営業部",
        "position": "主任",
        "role": "employee",
        "email": "suzuki@example.com",
    },
}


async def seed_default_users() -> None:
    """デフォルトユーザーをシード（冪等）.

    ユーザーが1件でも存在すればスキップする。
    手動登録済み環境に影響を与えない。
    """
    pwd = PasswordManager()

    async with get_db_session() as session:
        # 既存ユーザーが1件でもあればスキップ
        existing = await session.scalar(select(UserAccount.id).limit(1))
        if existing is not None:
            logger.debug("既存ユーザーが存在するためシードをスキップ")
            return

        now = datetime.now(tz=UTC)
        for username, meta in DEFAULT_USERS.items():
            salt = pwd.generate_salt()
            password_hash = pwd.hash_password(str(meta["initial_password"]), salt)
            user = UserAccount(
                id=pwd.build_user_id(username),
                username=username,
                email=str(meta.get("email", "")),
                password_hash=password_hash,
                password_salt=salt,
                display_name=str(meta["display_name"]),
                department=str(meta["department"]),
                position=str(meta["position"]),
                role=str(meta["role"]),
                auth_source="local_db",
                is_active=True,
                created_at=now,
                updated_at=now,
            )
            session.add(user)

        logger.info(
            "デフォルトユーザーを作成しました: %s",
            ", ".join(DEFAULT_USERS.keys()),
        )
