# -*- coding: utf-8 -*-
"""構造化ログモジュール.

JSON 形式の構造化ログを提供します。

特徴:
- JSON 形式出力（ログ分析ツール互換）
- コンテキスト情報の自動付加
- 相関 ID（Trace ID）サポート
- マスキング（機密情報の隠蔽）
"""

from __future__ import annotations

import json
import logging
import sys
import threading
from contextlib import contextmanager
from dataclasses import dataclass, field
from datetime import datetime, timezone
from enum import Enum
from typing import Any, Iterator, TextIO

# グローバルコンテキスト
_context_var: dict[str, Any] = {}
_context_lock = threading.Lock()


class LogLevel(str, Enum):
    """ログレベル."""

    DEBUG = "DEBUG"
    INFO = "INFO"
    WARNING = "WARNING"
    ERROR = "ERROR"
    CRITICAL = "CRITICAL"


@dataclass
class LogConfig:
    """ログ設定.

    Attributes:
        level: ログレベル
        format: 出力形式（json / text）
        output: 出力先（stdout / stderr / ファイルパス）
        include_timestamp: タイムスタンプを含めるか
        include_caller: 呼び出し元情報を含めるか
        mask_patterns: マスキングパターン
        context: 固定コンテキスト
    """

    level: LogLevel = LogLevel.INFO
    format: str = "json"
    output: str = "stdout"
    include_timestamp: bool = True
    include_caller: bool = True
    mask_patterns: list[str] = field(
        default_factory=lambda: [
            "password",
            "secret",
            "token",
            "api_key",
            "apikey",
            "authorization",
        ]
    )
    context: dict[str, Any] = field(default_factory=dict)


class JSONFormatter(logging.Formatter):
    """JSON 形式フォーマッター."""

    def __init__(
        self,
        include_timestamp: bool = True,
        include_caller: bool = True,
        mask_patterns: list[str] | None = None,
    ) -> None:
        """初期化."""
        super().__init__()
        self._include_timestamp = include_timestamp
        self._include_caller = include_caller
        self._mask_patterns = [p.lower() for p in (mask_patterns or [])]

    def format(self, record: logging.LogRecord) -> str:
        """ログレコードを JSON 形式に変換."""
        log_data: dict[str, Any] = {
            "level": record.levelname,
            "message": record.getMessage(),
            "logger": record.name,
        }

        # タイムスタンプ
        if self._include_timestamp:
            log_data["timestamp"] = datetime.now(timezone.utc).isoformat()

        # 呼び出し元情報
        if self._include_caller:
            log_data["caller"] = {
                "file": record.pathname,
                "line": record.lineno,
                "function": record.funcName,
            }

        # 例外情報
        if record.exc_info:
            log_data["exception"] = self.formatException(record.exc_info)

        # 追加フィールド（extra）
        for key, value in record.__dict__.items():
            if key not in {
                "name",
                "msg",
                "args",
                "created",
                "filename",
                "funcName",
                "levelname",
                "levelno",
                "lineno",
                "module",
                "msecs",
                "pathname",
                "process",
                "processName",
                "relativeCreated",
                "stack_info",
                "exc_info",
                "exc_text",
                "thread",
                "threadName",
                "taskName",
                "message",
            }:
                log_data[key] = self._mask_value(key, value)

        # グローバルコンテキスト
        with _context_lock:
            if _context_var:
                log_data["context"] = _context_var.copy()

        return json.dumps(log_data, ensure_ascii=False, default=str)

    def _mask_value(self, key: str, value: Any) -> Any:
        """機密情報をマスキング."""
        key_lower = key.lower()
        for pattern in self._mask_patterns:
            if pattern in key_lower:
                return "***MASKED***"
        return value


class TextFormatter(logging.Formatter):
    """テキスト形式フォーマッター."""

    def __init__(self, include_timestamp: bool = True) -> None:
        """初期化."""
        if include_timestamp:
            fmt = "%(asctime)s [%(levelname)s] %(name)s: %(message)s"
        else:
            fmt = "[%(levelname)s] %(name)s: %(message)s"
        super().__init__(fmt=fmt, datefmt="%Y-%m-%d %H:%M:%S")


class AgentFlowLogger:
    """AgentFlow 専用ロガー.

    構造化ログとコンテキスト管理を提供します。

    Example:
        >>> logger = AgentFlowLogger("my-agent")
        >>> logger.info("Processing request", request_id="123")
        >>> with logger.context(user_id="456"):
        ...     logger.info("User action")
    """

    def __init__(
        self,
        name: str,
        config: LogConfig | None = None,
    ) -> None:
        """初期化.

        Args:
            name: ロガー名
            config: ログ設定
        """
        self._name = name
        self._config = config or LogConfig()
        self._logger = logging.getLogger(name)
        self._setup_handler()

    def _setup_handler(self) -> None:
        """ハンドラーを設定."""
        # 既存のハンドラーをクリア
        self._logger.handlers.clear()

        # ログレベル設定
        self._logger.setLevel(getattr(logging, self._config.level.value))

        # 出力先を決定
        output: TextIO
        if self._config.output == "stdout":
            output = sys.stdout
        elif self._config.output == "stderr":
            output = sys.stderr
        else:
            output = open(self._config.output, "a", encoding="utf-8")

        handler = logging.StreamHandler(output)

        # フォーマッターを設定
        if self._config.format == "json":
            formatter = JSONFormatter(
                include_timestamp=self._config.include_timestamp,
                include_caller=self._config.include_caller,
                mask_patterns=self._config.mask_patterns,
            )
        else:
            formatter = TextFormatter(include_timestamp=self._config.include_timestamp)

        handler.setFormatter(formatter)
        self._logger.addHandler(handler)

    def debug(self, message: str, **kwargs: Any) -> None:
        """DEBUG ログ."""
        self._logger.debug(message, extra=kwargs)

    def info(self, message: str, **kwargs: Any) -> None:
        """INFO ログ."""
        self._logger.info(message, extra=kwargs)

    def warning(self, message: str, **kwargs: Any) -> None:
        """WARNING ログ."""
        self._logger.warning(message, extra=kwargs)

    def error(self, message: str, **kwargs: Any) -> None:
        """ERROR ログ."""
        self._logger.error(message, extra=kwargs)

    def critical(self, message: str, **kwargs: Any) -> None:
        """CRITICAL ログ."""
        self._logger.critical(message, extra=kwargs)

    def exception(self, message: str, **kwargs: Any) -> None:
        """例外ログ（スタックトレース付き）."""
        self._logger.exception(message, extra=kwargs)

    @contextmanager
    def context(self, **kwargs: Any) -> Iterator[None]:
        """コンテキストを一時的に追加.

        Example:
            >>> with logger.context(request_id="123"):
            ...     logger.info("Inside context")
        """
        global _context_var
        with _context_lock:
            old_context = _context_var.copy()
            _context_var.update(kwargs)
        try:
            yield
        finally:
            with _context_lock:
                _context_var = old_context


# グローバルロガー
_loggers: dict[str, AgentFlowLogger] = {}


def setup_logging(
    level: LogLevel = LogLevel.INFO,
    format: str = "json",
    output: str = "stdout",
    **kwargs: Any,
) -> None:
    """グローバルログ設定.

    Args:
        level: ログレベル
        format: 出力形式（json / text）
        output: 出力先
        **kwargs: その他の設定
    """
    global _loggers

    config = LogConfig(
        level=level,
        format=format,
        output=output,
        **kwargs,
    )

    # ルートロガーを設定
    root_logger = logging.getLogger()
    root_logger.setLevel(getattr(logging, level.value))

    # 既存のハンドラーをクリア
    root_logger.handlers.clear()

    # 新しいハンドラーを追加
    if output == "stdout":
        handler = logging.StreamHandler(sys.stdout)
    elif output == "stderr":
        handler = logging.StreamHandler(sys.stderr)
    else:
        handler = logging.FileHandler(output, encoding="utf-8")

    if format == "json":
        handler.setFormatter(JSONFormatter())
    else:
        handler.setFormatter(TextFormatter())

    root_logger.addHandler(handler)


def get_logger(name: str = "agentflow") -> AgentFlowLogger:
    """ロガーを取得.

    Args:
        name: ロガー名

    Returns:
        AgentFlowLogger インスタンス
    """
    global _loggers

    if name not in _loggers:
        _loggers[name] = AgentFlowLogger(name)

    return _loggers[name]


def set_context(**kwargs: Any) -> None:
    """グローバルコンテキストを設定.

    Args:
        **kwargs: コンテキスト情報
    """
    global _context_var
    with _context_lock:
        _context_var.update(kwargs)


def get_context() -> dict[str, Any]:
    """グローバルコンテキストを取得."""
    with _context_lock:
        return _context_var.copy()


def replace_context(context: dict[str, Any]) -> None:
    """グローバルコンテキストを置換."""
    global _context_var
    with _context_lock:
        _context_var = context.copy()


def clear_context() -> None:
    """グローバルコンテキストをクリア."""
    global _context_var
    with _context_lock:
        _context_var.clear()
