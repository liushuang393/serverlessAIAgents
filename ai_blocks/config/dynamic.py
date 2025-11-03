"""
動的設定管理システム

このモジュールは、実行時での設定変更、環境別設定管理、設定バリデーション機能を提供します。
設定の変更通知、ロールバック、監査ログ機能を含みます。
"""

import asyncio
import json
import os
from contextlib import asynccontextmanager
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional

import yaml
from pydantic import BaseModel, Field, field_validator

from ..utils.logging import get_logger

logger = get_logger(__name__)


class ConfigSource(str, Enum):
    """設定ソース"""

    FILE = "file"
    ENVIRONMENT = "environment"
    DATABASE = "database"
    REMOTE = "remote"
    RUNTIME = "runtime"


class ConfigChangeType(str, Enum):
    """設定変更タイプ"""

    CREATE = "create"
    UPDATE = "update"
    DELETE = "delete"
    ROLLBACK = "rollback"


@dataclass
class ConfigChange:
    """設定変更記録"""

    timestamp: datetime
    change_type: ConfigChangeType
    key: str
    old_value: Any
    new_value: Any
    source: ConfigSource
    user: Optional[str] = None
    reason: Optional[str] = None


class ConfigValidationError(Exception):
    """設定バリデーションエラー"""

    pass


class DynamicSettings(BaseModel):
    """動的設定クラス"""

    # 基本設定（静的）
    app_name: str = Field(default="AI Blocks", description="アプリケーション名")
    app_version: str = Field(default="0.1.0", description="アプリケーションバージョン")

    # 動的設定可能な項目
    debug: bool = Field(default=False, description="デバッグモード")
    log_level: str = Field(default="INFO", description="ログレベル")

    # LLM設定
    default_model: str = Field(default="gpt-3.5-turbo", description="デフォルトモデル")
    max_tokens: int = Field(default=1000, description="最大トークン数", ge=1, le=32000)
    temperature: float = Field(default=0.7, description="生成温度", ge=0.0, le=2.0)

    # メモリ設定
    memory_max_items: int = Field(default=1000, description="メモリの最大アイテム数", ge=1)
    memory_similarity_threshold: float = Field(
        default=0.7, description="類似度閾値", ge=0.0, le=1.0
    )

    # ツール設定
    tool_timeout: float = Field(default=30.0, description="ツール実行タイムアウト（秒）", ge=1.0)
    max_tool_calls: int = Field(default=10, description="最大ツール呼び出し回数", ge=1)

    # パフォーマンス設定
    enable_caching: bool = Field(default=True, description="キャッシュを有効にする")
    cache_ttl: int = Field(default=3600, description="キャッシュTTL（秒）", ge=1)
    max_concurrent_requests: int = Field(default=10, description="最大同時リクエスト数", ge=1)

    # メトリクス設定
    enable_metrics: bool = Field(default=True, description="メトリクス収集を有効にする")
    metrics_export_interval: int = Field(default=60, description="メトリクス出力間隔（秒）", ge=1)

    # トレーシング設定
    enable_tracing: bool = Field(default=False, description="トレーシングを有効にする")
    trace_sample_rate: float = Field(
        default=0.1, description="トレースサンプリング率", ge=0.0, le=1.0
    )

    @field_validator("log_level")
    @classmethod
    def validate_log_level(cls, v):
        valid_levels = ["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"]
        if v.upper() not in valid_levels:
            raise ValueError(f"ログレベルは {valid_levels} のいずれかである必要があります")
        return v.upper()

    @field_validator("default_model")
    @classmethod
    def validate_model(cls, v):
        # 基本的なモデル名の検証
        if not v or len(v.strip()) == 0:
            raise ValueError("モデル名は空にできません")
        return v.strip()


class ConfigManager:
    """動的設定管理クラス"""

    def __init__(self, config_file: Optional[Path] = None):
        self.config_file = config_file or Path("config.yaml")
        self._settings = DynamicSettings()
        self._change_history: List[ConfigChange] = []
        self._change_listeners: List[Callable[[str, Any, Any], None]] = []
        self._validation_rules: Dict[str, Callable[[Any], bool]] = {}
        self._lock = asyncio.Lock()
        self._file_watcher_task: Optional[asyncio.Task] = None
        self._last_file_mtime: Optional[float] = None

        logger.info("動的設定管理システムを初期化しました")

    async def initialize(self) -> None:
        """設定管理システムを初期化する"""
        # 設定ファイルから読み込み
        await self._load_from_file()

        # 環境変数から読み込み
        await self._load_from_environment()

        # ファイル監視を開始
        await self._start_file_watcher()

        logger.info("動的設定管理システムの初期化が完了しました")

    async def _load_from_file(self) -> None:
        """設定ファイルから読み込む"""
        if not self.config_file.exists():
            logger.info(f"設定ファイルが存在しません: {self.config_file}")
            return

        try:
            with open(self.config_file, "r", encoding="utf-8") as f:
                if (
                    self.config_file.suffix.lower() == ".yaml"
                    or self.config_file.suffix.lower() == ".yml"
                ):
                    data = yaml.safe_load(f)
                else:
                    data = json.load(f)

            if data:
                # 設定を更新
                for key, value in data.items():
                    if hasattr(self._settings, key):
                        await self._update_setting(key, value, ConfigSource.FILE)

            # ファイルの更新時刻を記録
            self._last_file_mtime = self.config_file.stat().st_mtime

            logger.info(f"設定ファイルから読み込みました: {self.config_file}")

        except Exception as e:
            logger.error(f"設定ファイルの読み込みに失敗しました: {e}")

    async def _load_from_environment(self) -> None:
        """環境変数から読み込む"""
        prefix = "AI_BLOCKS_"

        for key, value in os.environ.items():
            if key.startswith(prefix):
                setting_key = key[len(prefix) :].lower()

                if hasattr(self._settings, setting_key):
                    # 型変換
                    field_info = self._settings.__class__.model_fields[setting_key]
                    field_type = field_info.annotation

                    try:
                        converted_value: Any
                        if field_type == bool:
                            converted_value = value.lower() in (
                                "true",
                                "1",
                                "yes",
                                "on",
                            )
                        elif field_type == int:
                            converted_value = int(value)
                        elif field_type == float:
                            converted_value = float(value)
                        else:
                            converted_value = value

                        await self._update_setting(
                            setting_key, converted_value, ConfigSource.ENVIRONMENT
                        )

                    except ValueError as e:
                        logger.warning(f"環境変数の型変換に失敗しました: {key}={value} - {e}")

        logger.debug("環境変数から設定を読み込みました")

    async def _start_file_watcher(self) -> None:
        """ファイル監視を開始する"""
        if self._file_watcher_task:
            return

        self._file_watcher_task = asyncio.create_task(self._watch_file())
        logger.debug("設定ファイル監視を開始しました")

    async def _watch_file(self) -> None:
        """設定ファイルを監視する"""
        while True:
            try:
                await asyncio.sleep(1)  # 1秒間隔でチェック

                if not self.config_file.exists():
                    continue

                current_mtime = self.config_file.stat().st_mtime

                if (
                    self._last_file_mtime is None
                    or current_mtime > self._last_file_mtime
                ):
                    logger.info("設定ファイルの変更を検出しました")
                    await self._load_from_file()

            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"ファイル監視中にエラーが発生しました: {e}")
                await asyncio.sleep(5)  # エラー時は少し長めに待機

    async def get(self, key: str, default: Any = None) -> Any:
        """設定値を取得する"""
        return getattr(self._settings, key, default)

    async def set(
        self,
        key: str,
        value: Any,
        source: ConfigSource = ConfigSource.RUNTIME,
        user: Optional[str] = None,
        reason: Optional[str] = None,
        validate: bool = True,
    ) -> bool:
        """設定値を更新する"""
        async with self._lock:
            if not hasattr(self._settings, key):
                raise ValueError(f"未知の設定キー: {key}")

            old_value = getattr(self._settings, key)

            # バリデーション
            if validate and not await self._validate_setting(key, value):
                raise ConfigValidationError(f"設定値のバリデーションに失敗しました: {key}={value}")

            # 設定を更新
            success = await self._update_setting(key, value, source, user, reason)

            if success:
                # 変更通知
                await self._notify_change(key, old_value, value)

            return success

    async def _update_setting(
        self,
        key: str,
        value: Any,
        source: ConfigSource,
        user: Optional[str] = None,
        reason: Optional[str] = None,
    ) -> bool:
        """設定を更新する内部メソッド"""
        try:
            old_value = getattr(self._settings, key)

            # Pydanticのバリデーションを使用
            setattr(self._settings, key, value)

            # 変更履歴を記録
            change = ConfigChange(
                timestamp=datetime.now(),
                change_type=ConfigChangeType.UPDATE,
                key=key,
                old_value=old_value,
                new_value=value,
                source=source,
                user=user,
                reason=reason,
            )
            self._change_history.append(change)

            logger.info(f"設定を更新しました: {key} = {value} (source: {source})")
            return True

        except Exception as e:
            logger.error(f"設定の更新に失敗しました: {key}={value} - {e}")
            return False

    async def _validate_setting(self, key: str, value: Any) -> bool:
        """設定値をバリデーションする"""
        # カスタムバリデーションルール
        if key in self._validation_rules:
            try:
                return self._validation_rules[key](value)
            except Exception as e:
                logger.error(f"カスタムバリデーションに失敗しました: {key}={value} - {e}")
                return False

        # Pydanticのバリデーション
        try:
            # 一時的なインスタンスでバリデーション
            temp_settings = self._settings.model_copy()
            setattr(temp_settings, key, value)
            return True
        except Exception as e:
            logger.error(f"Pydanticバリデーションに失敗しました: {key}={value} - {e}")
            return False

    async def _notify_change(self, key: str, old_value: Any, new_value: Any) -> None:
        """設定変更を通知する"""
        for listener in self._change_listeners:
            try:
                if asyncio.iscoroutinefunction(listener):
                    await listener(key, old_value, new_value)
                else:
                    listener(key, old_value, new_value)
            except Exception as e:
                logger.error(f"設定変更リスナーでエラーが発生しました: {e}")

    def add_change_listener(self, listener: Callable[[str, Any, Any], None]) -> None:
        """設定変更リスナーを追加する"""
        self._change_listeners.append(listener)
        logger.debug("設定変更リスナーを追加しました")

    def remove_change_listener(self, listener: Callable[[str, Any, Any], None]) -> None:
        """設定変更リスナーを削除する"""
        if listener in self._change_listeners:
            self._change_listeners.remove(listener)
            logger.debug("設定変更リスナーを削除しました")

    def add_validation_rule(self, key: str, validator: Callable[[Any], bool]) -> None:
        """カスタムバリデーションルールを追加する"""
        self._validation_rules[key] = validator
        logger.debug(f"バリデーションルールを追加しました: {key}")

    async def rollback(self, steps: int = 1) -> bool:
        """設定をロールバックする"""
        async with self._lock:
            if len(self._change_history) < steps:
                logger.warning(
                    f"ロールバック可能な履歴が不足しています: {len(self._change_history)} < {steps}"
                )
                return False

            # 指定されたステップ数だけロールバック
            for _ in range(steps):
                if not self._change_history:
                    break

                last_change = self._change_history.pop()

                # 設定を元に戻す
                await self._update_setting(
                    last_change.key,
                    last_change.old_value,
                    ConfigSource.RUNTIME,
                    reason=f"Rollback from {last_change.new_value}",
                )

                logger.info(
                    f"設定をロールバックしました: {last_change.key} = {last_change.old_value}"
                )

            return True

    async def save_to_file(self, file_path: Optional[Path] = None) -> None:
        """設定をファイルに保存する"""
        target_file = file_path or self.config_file

        # 設定を辞書に変換
        config_dict = self._settings.model_dump()

        try:
            with open(target_file, "w", encoding="utf-8") as f:
                if target_file.suffix.lower() in [".yaml", ".yml"]:
                    yaml.dump(
                        config_dict, f, default_flow_style=False, allow_unicode=True
                    )
                else:
                    json.dump(config_dict, f, indent=2, ensure_ascii=False)

            logger.info(f"設定をファイルに保存しました: {target_file}")

        except Exception as e:
            logger.error(f"設定ファイルの保存に失敗しました: {e}")
            raise

    def get_change_history(self, limit: Optional[int] = None) -> List[ConfigChange]:
        """変更履歴を取得する"""
        if limit:
            return self._change_history[-limit:]
        return self._change_history.copy()

    def get_current_settings(self) -> Dict[str, Any]:
        """現在の設定を取得する"""
        return self._settings.model_dump()

    async def cleanup(self) -> None:
        """クリーンアップ処理"""
        if self._file_watcher_task:
            self._file_watcher_task.cancel()
            try:
                await self._file_watcher_task
            except asyncio.CancelledError:
                pass

        logger.info("動的設定管理システムをクリーンアップしました")


# グローバル設定管理インスタンス
_config_manager: Optional[ConfigManager] = None


async def get_config_manager() -> ConfigManager:
    """設定管理インスタンスを取得する（シングルトン）"""
    global _config_manager
    if _config_manager is None:
        _config_manager = ConfigManager()
        await _config_manager.initialize()
    return _config_manager


@asynccontextmanager
async def config_context(**temporary_settings):
    """一時的な設定変更のコンテキストマネージャー"""
    manager = await get_config_manager()

    # 現在の設定を保存
    original_settings = {}
    for key, value in temporary_settings.items():
        original_settings[key] = await manager.get(key)
        await manager.set(
            key, value, source=ConfigSource.RUNTIME, reason="Temporary context"
        )

    try:
        yield manager
    finally:
        # 設定を元に戻す
        for key, original_value in original_settings.items():
            await manager.set(
                key,
                original_value,
                source=ConfigSource.RUNTIME,
                reason="Context cleanup",
            )
