# -*- coding: utf-8 -*-
"""API Key 管理モジュール.

API Key の生成、検証、管理を提供します。

特徴:
- 安全なキー生成（暗号学的乱数）
- ハッシュ保存（プレーンテキスト保存しない）
- 有効期限管理
- スコープベースのアクセス制御
"""

from __future__ import annotations

import hashlib
import hmac
import logging
import secrets
import threading
from dataclasses import dataclass, field
from datetime import datetime, timezone
from typing import Any

logger = logging.getLogger(__name__)


def generate_api_key(prefix: str = "sk", length: int = 32) -> str:
    """API Key を生成.

    Args:
        prefix: キープレフィックス
        length: キー長（バイト数）

    Returns:
        生成された API Key
    """
    random_bytes = secrets.token_urlsafe(length)
    return f"{prefix}-{random_bytes}"


def hash_api_key(key: str, salt: str = "") -> str:
    """API Key をハッシュ化.

    Args:
        key: API Key
        salt: ソルト

    Returns:
        ハッシュ値
    """
    salted_key = f"{salt}{key}"
    return hashlib.sha256(salted_key.encode()).hexdigest()


@dataclass
class APIKey:
    """API Key 情報.

    Attributes:
        id: キー ID
        name: キー名
        key_hash: キーのハッシュ値
        scopes: 許可されたスコープ
        created_at: 作成日時
        expires_at: 有効期限
        last_used_at: 最終使用日時
        metadata: メタデータ
        enabled: 有効フラグ
    """

    id: str
    name: str
    key_hash: str
    scopes: list[str] = field(default_factory=list)
    created_at: datetime = field(default_factory=lambda: datetime.now(timezone.utc))
    expires_at: datetime | None = None
    last_used_at: datetime | None = None
    metadata: dict[str, Any] = field(default_factory=dict)
    enabled: bool = True

    def is_valid(self) -> bool:
        """キーが有効かどうかを確認.

        Returns:
            有効な場合 True
        """
        if not self.enabled:
            return False

        if self.expires_at and datetime.now(timezone.utc) > self.expires_at:
            return False

        return True

    def has_scope(self, scope: str) -> bool:
        """スコープを持っているかを確認.

        Args:
            scope: チェックするスコープ

        Returns:
            スコープを持っている場合 True
        """
        if "*" in self.scopes:
            return True
        return scope in self.scopes

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換（キーハッシュは含めない）.

        Returns:
            辞書
        """
        return {
            "id": self.id,
            "name": self.name,
            "scopes": self.scopes,
            "created_at": self.created_at.isoformat(),
            "expires_at": self.expires_at.isoformat() if self.expires_at else None,
            "last_used_at": self.last_used_at.isoformat() if self.last_used_at else None,
            "metadata": self.metadata,
            "enabled": self.enabled,
        }


@dataclass
class APIKeyConfig:
    """API Key 設定.

    Attributes:
        hash_salt: ハッシュソルト
        key_prefix: キープレフィックス
        key_length: キー長
        default_scopes: デフォルトスコープ
        max_keys_per_user: ユーザーあたりの最大キー数
    """

    hash_salt: str = ""
    key_prefix: str = "sk"
    key_length: int = 32
    default_scopes: list[str] = field(default_factory=lambda: ["read"])
    max_keys_per_user: int = 10


class APIKeyManager:
    """API Key マネージャー.

    API Key の生成、検証、管理を行います。

    Example:
        >>> manager = APIKeyManager()
        >>> key, api_key = manager.create_key("my-key", scopes=["read", "write"])
        >>> print(f"Your API Key: {key}")  # 一度だけ表示
        >>> is_valid = manager.validate(key)
    """

    def __init__(self, config: APIKeyConfig | None = None) -> None:
        """初期化.

        Args:
            config: API Key 設定
        """
        self._config = config or APIKeyConfig()
        self._keys: dict[str, APIKey] = {}  # id -> APIKey
        self._key_hashes: dict[str, str] = {}  # hash -> id
        self._lock = threading.Lock()
        self._logger = logging.getLogger(__name__)

    def create_key(
        self,
        name: str,
        scopes: list[str] | None = None,
        expires_at: datetime | None = None,
        metadata: dict[str, Any] | None = None,
    ) -> tuple[str, APIKey]:
        """新しい API Key を作成.

        Args:
            name: キー名
            scopes: 許可されたスコープ
            expires_at: 有効期限
            metadata: メタデータ

        Returns:
            (生成されたキー, APIKey オブジェクト) のタプル
        """
        # キーを生成
        raw_key = generate_api_key(
            prefix=self._config.key_prefix,
            length=self._config.key_length,
        )

        # ハッシュ化
        key_hash = hash_api_key(raw_key, self._config.hash_salt)

        # ID を生成
        key_id = secrets.token_urlsafe(16)

        # APIKey オブジェクトを作成
        api_key = APIKey(
            id=key_id,
            name=name,
            key_hash=key_hash,
            scopes=scopes or self._config.default_scopes.copy(),
            expires_at=expires_at,
            metadata=metadata or {},
        )

        # 保存
        with self._lock:
            self._keys[key_id] = api_key
            self._key_hashes[key_hash] = key_id

        self._logger.info(f"Created API key: {name} (id={key_id})")

        # 生のキーは一度だけ返す（保存しない）
        return raw_key, api_key

    def validate(
        self,
        key: str,
        required_scope: str | None = None,
    ) -> APIKey | None:
        """API Key を検証.

        Args:
            key: 検証するキー
            required_scope: 必要なスコープ

        Returns:
            有効な場合 APIKey オブジェクト、無効な場合 None
        """
        # ハッシュを計算
        key_hash = hash_api_key(key, self._config.hash_salt)

        with self._lock:
            # ハッシュからキー ID を検索
            key_id = self._key_hashes.get(key_hash)
            if not key_id:
                return None

            # APIKey を取得
            api_key = self._keys.get(key_id)
            if not api_key:
                return None

            # 有効性チェック
            if not api_key.is_valid():
                return None

            # スコープチェック
            if required_scope and not api_key.has_scope(required_scope):
                return None

            # 最終使用日時を更新
            api_key.last_used_at = datetime.now(timezone.utc)

        return api_key

    def revoke(self, key_id: str) -> bool:
        """API Key を無効化.

        Args:
            key_id: キー ID

        Returns:
            成功した場合 True
        """
        with self._lock:
            api_key = self._keys.get(key_id)
            if not api_key:
                return False

            api_key.enabled = False
            self._logger.info(f"Revoked API key: {key_id}")
            return True

    def delete(self, key_id: str) -> bool:
        """API Key を削除.

        Args:
            key_id: キー ID

        Returns:
            成功した場合 True
        """
        with self._lock:
            api_key = self._keys.get(key_id)
            if not api_key:
                return False

            # ハッシュマップからも削除
            del self._key_hashes[api_key.key_hash]
            del self._keys[key_id]

            self._logger.info(f"Deleted API key: {key_id}")
            return True

    def list_keys(self, include_disabled: bool = False) -> list[APIKey]:
        """全ての API Key を一覧.

        Args:
            include_disabled: 無効なキーも含めるか

        Returns:
            APIKey のリスト
        """
        with self._lock:
            keys = list(self._keys.values())

        if not include_disabled:
            keys = [k for k in keys if k.enabled]

        return keys

    def get_key(self, key_id: str) -> APIKey | None:
        """キー ID から APIKey を取得.

        Args:
            key_id: キー ID

        Returns:
            APIKey、または None
        """
        with self._lock:
            return self._keys.get(key_id)

    def update_scopes(self, key_id: str, scopes: list[str]) -> bool:
        """スコープを更新.

        Args:
            key_id: キー ID
            scopes: 新しいスコープ

        Returns:
            成功した場合 True
        """
        with self._lock:
            api_key = self._keys.get(key_id)
            if not api_key:
                return False

            api_key.scopes = scopes
            self._logger.info(f"Updated scopes for API key: {key_id}")
            return True

    def cleanup_expired(self) -> int:
        """期限切れのキーをクリーンアップ.

        Returns:
            削除したキーの数
        """
        now = datetime.now(timezone.utc)
        expired_ids = []

        with self._lock:
            for key_id, api_key in self._keys.items():
                if api_key.expires_at and api_key.expires_at < now:
                    expired_ids.append(key_id)

        count = 0
        for key_id in expired_ids:
            if self.delete(key_id):
                count += 1

        if count > 0:
            self._logger.info(f"Cleaned up {count} expired API keys")

        return count

