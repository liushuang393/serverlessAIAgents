"""Sentry 統合モジュール.

エラー追跡とパフォーマンスモニタリングのための Sentry 統合を提供します。

特徴:
- 自動例外キャプチャ
- パフォーマンストレース
- コンテキスト情報の自動付加
- 環境別設定
"""

from __future__ import annotations

import functools
import logging
import os
from collections.abc import Callable
from typing import Any, TypeVar


logger = logging.getLogger(__name__)

# Sentry SDK の状態
_sentry_initialized = False
_sentry_dsn: str | None = None

F = TypeVar("F", bound=Callable[..., Any])


def setup_sentry(
    dsn: str | None = None,
    environment: str | None = None,
    release: str | None = None,
    traces_sample_rate: float = 0.1,
    profiles_sample_rate: float = 0.1,
    **kwargs: Any,
) -> bool:
    """Sentry を初期化.

    Args:
        dsn: Sentry DSN（省略時は SENTRY_DSN 環境変数）
        environment: 環境名（省略時は SENTRY_ENVIRONMENT 環境変数）
        release: リリースバージョン
        traces_sample_rate: トレースサンプリング率（0.0-1.0）
        profiles_sample_rate: プロファイルサンプリング率（0.0-1.0）
        **kwargs: Sentry SDK への追加引数

    Returns:
        初期化に成功した場合 True
    """
    global _sentry_initialized, _sentry_dsn

    # DSN を決定
    dsn = dsn or os.getenv("SENTRY_DSN")
    if not dsn:
        logger.info("Sentry DSN not provided. Sentry integration disabled.")
        return False

    try:
        import sentry_sdk
        from sentry_sdk.integrations.logging import LoggingIntegration

        sentry_sdk.init(
            dsn=dsn,
            environment=environment or os.getenv("SENTRY_ENVIRONMENT", "development"),
            release=release or os.getenv("SENTRY_RELEASE"),
            traces_sample_rate=traces_sample_rate,
            profiles_sample_rate=profiles_sample_rate,
            integrations=[
                LoggingIntegration(
                    level=logging.INFO,
                    event_level=logging.ERROR,
                ),
            ],
            **kwargs,
        )

        _sentry_initialized = True
        _sentry_dsn = dsn
        logger.info("Sentry initialized successfully")
        return True

    except ImportError:
        logger.warning("Sentry SDK not installed. Run: pip install sentry-sdk")
        return False
    except Exception as e:
        logger.exception(f"Failed to initialize Sentry: {e}")
        return False


def capture_exception(
    error: BaseException | None = None,
    **scope_kwargs: Any,
) -> str | None:
    """例外を Sentry にキャプチャ.

    Args:
        error: 例外（省略時は現在の例外）
        **scope_kwargs: スコープに追加する情報

    Returns:
        イベント ID、または None
    """
    if not _sentry_initialized:
        return None

    try:
        import sentry_sdk

        with sentry_sdk.push_scope() as scope:
            # 追加情報を設定
            for key, value in scope_kwargs.items():
                if key == "user":
                    scope.user = value
                elif key == "tags":
                    for tag_key, tag_value in value.items():
                        scope.set_tag(tag_key, tag_value)
                elif key == "extra":
                    for extra_key, extra_value in value.items():
                        scope.set_extra(extra_key, extra_value)
                else:
                    scope.set_extra(key, value)

            event_id = sentry_sdk.capture_exception(error)
            return str(event_id) if event_id else None

    except Exception as e:
        logger.warning(f"Failed to capture exception in Sentry: {e}")
        return None


def capture_message(
    message: str,
    level: str = "info",
    **scope_kwargs: Any,
) -> str | None:
    """メッセージを Sentry にキャプチャ.

    Args:
        message: メッセージ
        level: レベル（info / warning / error）
        **scope_kwargs: スコープに追加する情報

    Returns:
        イベント ID、または None
    """
    if not _sentry_initialized:
        return None

    try:
        import sentry_sdk

        with sentry_sdk.push_scope() as scope:
            # 追加情報を設定
            for key, value in scope_kwargs.items():
                if key == "user":
                    scope.user = value
                elif key == "tags":
                    for tag_key, tag_value in value.items():
                        scope.set_tag(tag_key, tag_value)
                elif key == "extra":
                    for extra_key, extra_value in value.items():
                        scope.set_extra(extra_key, extra_value)
                else:
                    scope.set_extra(key, value)

            event_id = sentry_sdk.capture_message(message, level=level)
            return str(event_id) if event_id else None

    except Exception as e:
        logger.warning(f"Failed to capture message in Sentry: {e}")
        return None


def set_user(
    user_id: str | None = None,
    email: str | None = None,
    username: str | None = None,
    **extra: Any,
) -> None:
    """Sentry のユーザー情報を設定.

    Args:
        user_id: ユーザー ID
        email: メールアドレス
        username: ユーザー名
        **extra: 追加情報
    """
    if not _sentry_initialized:
        return

    try:
        import sentry_sdk

        user_data: dict[str, Any] = {}
        if user_id:
            user_data["id"] = user_id
        if email:
            user_data["email"] = email
        if username:
            user_data["username"] = username
        user_data.update(extra)

        sentry_sdk.set_user(user_data)

    except Exception as e:
        logger.warning(f"Failed to set user in Sentry: {e}")


def set_tag(key: str, value: str) -> None:
    """Sentry タグを設定.

    Args:
        key: タグキー
        value: タグ値
    """
    if not _sentry_initialized:
        return

    try:
        import sentry_sdk

        sentry_sdk.set_tag(key, value)
    except Exception as e:
        logger.warning(f"Failed to set tag in Sentry: {e}")


def set_context(name: str, data: dict[str, Any]) -> None:
    """Sentry コンテキストを設定.

    Args:
        name: コンテキスト名
        data: コンテキストデータ
    """
    if not _sentry_initialized:
        return

    try:
        import sentry_sdk

        sentry_sdk.set_context(name, data)
    except Exception as e:
        logger.warning(f"Failed to set context in Sentry: {e}")


def sentry_trace(name: str | None = None) -> Callable[[F], F]:
    """Sentry トレーシングデコレータ.

    Args:
        name: トランザクション名（省略時は関数名）

    Returns:
        デコレータ
    """

    def decorator(func: F) -> F:
        transaction_name = name or func.__name__

        @functools.wraps(func)
        async def async_wrapper(*args: Any, **kwargs: Any) -> Any:
            if not _sentry_initialized:
                return await func(*args, **kwargs)

            try:
                import sentry_sdk

                with sentry_sdk.start_transaction(op="function", name=transaction_name):
                    return await func(*args, **kwargs)
            except ImportError:
                return await func(*args, **kwargs)

        @functools.wraps(func)
        def sync_wrapper(*args: Any, **kwargs: Any) -> Any:
            if not _sentry_initialized:
                return func(*args, **kwargs)

            try:
                import sentry_sdk

                with sentry_sdk.start_transaction(op="function", name=transaction_name):
                    return func(*args, **kwargs)
            except ImportError:
                return func(*args, **kwargs)

        # 非同期関数かどうかを判定
        import asyncio

        if asyncio.iscoroutinefunction(func):
            return async_wrapper  # type: ignore
        return sync_wrapper  # type: ignore

    return decorator


def is_sentry_initialized() -> bool:
    """Sentry が初期化されているかを確認.

    Returns:
        初期化されている場合 True
    """
    return _sentry_initialized
