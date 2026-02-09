"""Adapter Factory.

言語アダプターのファクトリークラス。
設定ファイルに基づいて適切なアダプターを生成します。
"""

from pathlib import Path
from typing import Any

import yaml
from apps.code_migration_assistant.adapters.base import (
    SourceLanguageAdapter,
    TargetLanguageAdapter,
)


class AdapterFactory:
    """言語アダプターファクトリー.

    設定ファイルを読み込み、移行タイプに応じたアダプターを生成します。

    Example:
        >>> factory = AdapterFactory()
        >>> source = factory.get_source_adapter("cobol-to-java")
        >>> target = factory.get_target_adapter("cobol-to-java")
    """

    # ソースアダプター登録
    _SOURCE_REGISTRY: dict[str, type[SourceLanguageAdapter]] = {}

    # ターゲットアダプター登録
    _TARGET_REGISTRY: dict[str, type[TargetLanguageAdapter]] = {}

    def __init__(self, config_path: Path | None = None) -> None:
        """初期化.

        Args:
            config_path: 設定ファイルパス（省略時はデフォルト）
        """
        if config_path is None:
            config_path = (
                Path(__file__).parent.parent / "config" / "migration_types.yaml"
            )

        self._config_path = config_path
        self._config: dict[str, Any] = {}
        self._migration_types: dict[str, dict[str, Any]] = {}

        # 設定読み込み
        self._load_config()

        # デフォルトアダプターを登録
        self._register_default_adapters()

    def _load_config(self) -> None:
        """設定ファイルを読み込み."""
        if self._config_path.exists():
            with open(self._config_path, encoding="utf-8") as f:
                self._config = yaml.safe_load(f) or {}

            # 移行タイプをインデックス化
            for mt in self._config.get("migration_types", []):
                self._migration_types[mt["name"]] = mt

    def _register_default_adapters(self) -> None:
        """デフォルトアダプターを登録."""
        # 遅延インポートで循環参照を回避
        from apps.code_migration_assistant.adapters.source.cobol_adapter import (
            CobolAdapter,
        )
        from apps.code_migration_assistant.adapters.target.java_adapter import (
            JavaAdapter,
        )

        self.register_source_adapter("COBOL", CobolAdapter)
        self.register_target_adapter("Java", JavaAdapter)

    @classmethod
    def register_source_adapter(
        cls,
        language: str,
        adapter_class: type[SourceLanguageAdapter],
    ) -> None:
        """ソースアダプターを登録.

        Args:
            language: 言語名
            adapter_class: アダプタークラス
        """
        cls._SOURCE_REGISTRY[language.upper()] = adapter_class

    @classmethod
    def register_target_adapter(
        cls,
        language: str,
        adapter_class: type[TargetLanguageAdapter],
    ) -> None:
        """ターゲットアダプターを登録.

        Args:
            language: 言語名
            adapter_class: アダプタークラス
        """
        cls._TARGET_REGISTRY[language.upper()] = adapter_class

    def get_source_adapter(
        self,
        migration_type: str | None = None,
        language: str | None = None,
    ) -> SourceLanguageAdapter:
        """ソースアダプターを取得.

        Args:
            migration_type: 移行タイプ名（例: "cobol-to-java"）
            language: 言語名（migration_type が指定されていない場合）

        Returns:
            ソース言語アダプター

        Raises:
            ValueError: 未登録の言語/移行タイプ
        """
        if migration_type:
            mt_config = self._migration_types.get(migration_type)
            if not mt_config:
                msg = f"Unknown migration type: {migration_type}"
                raise ValueError(msg)
            language = mt_config["source"]["language"]

        if not language:
            # デフォルト設定を使用
            default_type = self._config.get("defaults", {}).get(
                "migration_type", "cobol-to-java"
            )
            mt_config = self._migration_types.get(default_type, {})
            language = mt_config.get("source", {}).get("language", "COBOL")

        adapter_class = self._SOURCE_REGISTRY.get(language.upper())
        if not adapter_class:
            msg = f"No source adapter registered for: {language}"
            raise ValueError(msg)

        return adapter_class()

    def get_target_adapter(
        self,
        migration_type: str | None = None,
        language: str | None = None,
    ) -> TargetLanguageAdapter:
        """ターゲットアダプターを取得.

        Args:
            migration_type: 移行タイプ名（例: "cobol-to-java"）
            language: 言語名（migration_type が指定されていない場合）

        Returns:
            ターゲット言語アダプター

        Raises:
            ValueError: 未登録の言語/移行タイプ
        """
        if migration_type:
            mt_config = self._migration_types.get(migration_type)
            if not mt_config:
                msg = f"Unknown migration type: {migration_type}"
                raise ValueError(msg)
            language = mt_config["target"]["language"]

        if not language:
            default_type = self._config.get("defaults", {}).get(
                "migration_type", "cobol-to-java"
            )
            mt_config = self._migration_types.get(default_type, {})
            language = mt_config.get("target", {}).get("language", "Java")

        adapter_class = self._TARGET_REGISTRY.get(language.upper())
        if not adapter_class:
            msg = f"No target adapter registered for: {language}"
            raise ValueError(msg)

        return adapter_class()

    def get_migration_config(self, migration_type: str) -> dict[str, Any]:
        """移行タイプの設定を取得.

        Args:
            migration_type: 移行タイプ名

        Returns:
            移行設定辞書

        Raises:
            ValueError: 未登録の移行タイプ
        """
        config = self._migration_types.get(migration_type)
        if not config:
            msg = f"Unknown migration type: {migration_type}"
            raise ValueError(msg)
        return config

    def get_prompt(self, migration_type: str, prompt_type: str) -> str:
        """プロンプトテンプレートを取得.

        Args:
            migration_type: 移行タイプ名
            prompt_type: プロンプト種別（transform, checker, fixer, testgen）

        Returns:
            プロンプトテキスト
        """
        config = self.get_migration_config(migration_type)
        prompts_config = config.get("prompts", {})
        prompt_file = prompts_config.get(prompt_type, "")

        if not prompt_file:
            return ""

        prompt_path = (
            Path(__file__).parent.parent / "config" / "prompts" / prompt_file
        )

        if prompt_path.exists():
            return prompt_path.read_text(encoding="utf-8")

        return ""

    def list_migration_types(self) -> list[str]:
        """利用可能な移行タイプ一覧を取得.

        Returns:
            移行タイプ名のリスト
        """
        return list(self._migration_types.keys())

    def get_type_mapping_config(self, migration_type: str) -> dict[str, Any]:
        """型マッピング設定を取得.

        Args:
            migration_type: 移行タイプ名

        Returns:
            型マッピング設定辞書
        """
        config = self.get_migration_config(migration_type)
        mapping_file = config.get("type_mapping", "")

        if not mapping_file:
            return {}

        mapping_path = (
            Path(__file__).parent.parent / "config" / "type_mappings" / mapping_file
        )

        if mapping_path.exists():
            with open(mapping_path, encoding="utf-8") as f:
                return yaml.safe_load(f) or {}

        return {}


# シングルトンインスタンス
_factory_instance: AdapterFactory | None = None


def get_adapter_factory() -> AdapterFactory:
    """AdapterFactory のシングルトンインスタンスを取得.

    Returns:
        AdapterFactory インスタンス
    """
    global _factory_instance
    if _factory_instance is None:
        _factory_instance = AdapterFactory()
    return _factory_instance

