"""
ロギングユーティリティ

このモジュールは、アプリケーション全体で一貫したロギングを提供します。
"""

import logging
import sys

from ..config import get_settings

# from loguru import logger  # loguruは使用しない


def setup_logging() -> None:
    """ログ設定を初期化する"""
    settings = get_settings()

    # ログレベルを設定
    log_level = getattr(logging, settings.log_level.upper(), logging.INFO)

    # ログフォーマットを設定
    formatter = logging.Formatter(
        fmt="%(asctime)s | %(levelname)s | %(name)s:%(funcName)s:%(lineno)d - %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
    )

    # ルートロガーを設定
    root_logger = logging.getLogger()
    root_logger.setLevel(log_level)

    # 既存のハンドラーを削除
    for handler in root_logger.handlers[:]:
        root_logger.removeHandler(handler)

    # コンソールハンドラーを追加
    console_handler = logging.StreamHandler(sys.stderr)
    console_handler.setLevel(log_level)
    console_handler.setFormatter(formatter)
    root_logger.addHandler(console_handler)

    # ファイルハンドラーを追加（指定されている場合）
    if settings.log_file:
        file_handler = logging.FileHandler(settings.log_file)
        file_handler.setLevel(log_level)
        file_handler.setFormatter(formatter)
        root_logger.addHandler(file_handler)


def get_logger(name: str) -> logging.Logger:
    """
    名前付きロガーを取得する

    Args:
        name: ロガー名

    Returns:
        logging.Logger: 設定済みのロガー
    """
    return logging.getLogger(name)


# 初期化時にログ設定を実行
setup_logging()
