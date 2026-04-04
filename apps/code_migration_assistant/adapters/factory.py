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
from contracts.runtime.migration_execution import MigrationTaskProfile, StageExecutionPlan


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
            config_path = Path(__file__).parent.parent / "config" / "migration_types.yaml"

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
        from apps.code_migration_assistant.adapters.target.springboot_adapter import (
            SpringBootAdapter,
        )

        self.register_source_adapter("COBOL", CobolAdapter)
        self.register_target_adapter("Java", JavaAdapter)
        self.register_target_adapter("SpringBoot", SpringBootAdapter)

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
            default_type = self._config.get("defaults", {}).get("migration_type", "cobol-to-java")
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
            default_type = self._config.get("defaults", {}).get("migration_type", "cobol-to-java")
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

    def get_task_profile(self, migration_type: str) -> MigrationTaskProfile:
        """移行タイプから正規化 task profile を返す."""
        config = self.get_migration_config(migration_type)

        source_profile_raw = config.get("source_profile", config.get("source", {}))
        source_profile = source_profile_raw if isinstance(source_profile_raw, dict) else {}

        target_profile_raw = config.get("target_profile", config.get("target", {}))
        target_profile = target_profile_raw if isinstance(target_profile_raw, dict) else {}

        preparation_profile_raw = config.get("preparation_profile", {})
        preparation_profile = preparation_profile_raw if isinstance(preparation_profile_raw, dict) else {}

        analysis_capability_set_raw = config.get("analysis_capability_set", [])
        analysis_capability_set = [
            str(item) for item in analysis_capability_set_raw if isinstance(item, str) and item.strip()
        ]

        return MigrationTaskProfile(
            migration_type=migration_type,
            migration_family=str(config.get("migration_family", "custom")),
            source_profile=source_profile,
            target_profile=target_profile,
            preparation_profile=preparation_profile,
            analysis_capability_set=analysis_capability_set,
            transformation_strategy=str(config.get("transformation_strategy", "")),
            verification_strategy=str(config.get("verification_strategy", "")),
            delivery_strategy=str(config.get("delivery_strategy", "")),
        )

    def get_stage_execution_plan(self, migration_type: str, stage: str) -> StageExecutionPlan:
        """ステージ別の実行計画を返す."""
        config = self.get_migration_config(migration_type)
        stage_map_raw = config.get("stage_execution", {})
        stage_map = stage_map_raw if isinstance(stage_map_raw, dict) else {}
        stage_config_raw = stage_map.get(stage, {})
        stage_config = stage_config_raw if isinstance(stage_config_raw, dict) else {}

        required_skills_raw = stage_config.get("required_skills", [])
        required_skills = [str(item) for item in required_skills_raw if isinstance(item, str) and item.strip()]

        evidence_kinds_raw = stage_config.get("evidence_kinds", [])
        evidence_kinds = [str(item) for item in evidence_kinds_raw if isinstance(item, str) and item.strip()]

        fallback_conditions_raw = stage_config.get("fallback_conditions", [])
        fallback_conditions = [str(item) for item in fallback_conditions_raw if isinstance(item, str) and item.strip()]

        return StageExecutionPlan(
            stage=stage,
            capability_id=str(stage_config.get("capability_id", stage)),
            default_executor=str(stage_config.get("default_executor", "native")),
            fallback_executor=str(stage_config.get("fallback_executor", "native")),
            max_retries=int(stage_config.get("max_retries", 1)),
            fallback_conditions=fallback_conditions,
            required_skills=required_skills,
            evidence_kinds=evidence_kinds,
        )

    def get_prompt(self, migration_type: str, prompt_type: str) -> str:
        """プロンプトテンプレートを取得.

        Args:
            migration_type: 移行タイプ名
            prompt_type: プロンプト種別（transform, checker, fixer, testgen）

        Returns:
            プロンプトテキスト
        """
        config = self.get_migration_config(migration_type)
        prompts_config_raw = config.get("prompts", {})
        prompts_config = prompts_config_raw if isinstance(prompts_config_raw, dict) else {}
        prompt_file_value = prompts_config.get(prompt_type, "")
        prompt_file = str(prompt_file_value) if isinstance(prompt_file_value, str) else ""

        if not prompt_file:
            return ""

        prompt_path = Path(__file__).parent.parent / "config" / "prompts" / prompt_file

        if prompt_path.exists():
            return str(prompt_path.read_text(encoding="utf-8"))

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

        mapping_path = Path(__file__).parent.parent / "config" / "type_mappings" / mapping_file

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
