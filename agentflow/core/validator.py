"""Agent Validator.

このモジュールは Agent メタデータの検証機能を提供します。
"""

import re

from agentflow.core.metadata import AgentMetadata


class ValidationResult:
    """検証結果.

    Agent メタデータの検証結果を保持します。

    Attributes:
        is_valid: 検証が成功したかどうか
        errors: エラーメッセージのリスト
        warnings: 警告メッセージのリスト
    """

    def __init__(
        self,
        is_valid: bool = True,
        errors: list[str] | None = None,
        warnings: list[str] | None = None,
    ) -> None:
        """ValidationResult を初期化.

        Args:
            is_valid: 検証が成功したかどうか
            errors: エラーメッセージのリスト
            warnings: 警告メッセージのリスト
        """
        self.is_valid = is_valid
        self.errors = errors or []
        self.warnings = warnings or []

    def add_error(self, message: str) -> None:
        """エラーを追加.

        Args:
            message: エラーメッセージ
        """
        self.errors.append(message)
        self.is_valid = False

    def add_warning(self, message: str) -> None:
        """警告を追加.

        Args:
            message: 警告メッセージ
        """
        self.warnings.append(message)

    def __repr__(self) -> str:
        """文字列表現を返す."""
        status = "Valid" if self.is_valid else "Invalid"
        return (
            f"ValidationResult(status={status}, errors={len(self.errors)}, "
            f"warnings={len(self.warnings)})"
        )


class AgentValidator:
    """Agent Validator.

    Agent メタデータを検証します。

    検証項目:
    - ID 形式 (kebab-case)
    - バージョン形式 (semver)
    - 必須フィールドの存在
    - プロトコル設定の整合性
    - 依存関係の形式

    Example:
        >>> validator = AgentValidator()
        >>> result = validator.validate(metadata)
        >>> if not result.is_valid:
        ...     print(result.errors)
    """

    # 定数定義
    MIN_NAME_LENGTH = 1
    MAX_NAME_LENGTH = 100
    MIN_DESCRIPTION_LENGTH = 10
    MAX_DESCRIPTION_LENGTH = 500

    # ID パターン: kebab-case (小文字、数字、ハイフン)
    ID_PATTERN = re.compile(r"^[a-z0-9]+(-[a-z0-9]+)*$")
    VERSION_PATTERN = re.compile(r"^\d+\.\d+\.\d+$")

    def validate(self, metadata: AgentMetadata) -> ValidationResult:
        """Agent メタデータを検証.

        Args:
            metadata: 検証する AgentMetadata

        Returns:
            ValidationResult: 検証結果

        Example:
            >>> validator = AgentValidator()
            >>> result = validator.validate(metadata)
            >>> print(result.is_valid)
            True
        """
        result = ValidationResult()

        # 基本情報の検証
        self._validate_meta_info(metadata, result)

        # インターフェースの検証
        self._validate_interfaces(metadata, result)

        # プロトコル設定の検証
        self._validate_protocols(metadata, result)

        # 依存関係の検証
        self._validate_dependencies(metadata, result)

        # PocketFlow 設定の検証
        self._validate_pocketflow(metadata, result)

        return result

    def _validate_meta_info(self, metadata: AgentMetadata, result: ValidationResult) -> None:
        """基本情報を検証.

        Args:
            metadata: Agent メタデータ
            result: 検証結果
        """
        meta = metadata.meta

        # ID 形式チェック
        if not self.ID_PATTERN.match(meta.id):
            result.add_error(
                f"Invalid agent ID format: '{meta.id}'. "
                "Must be kebab-case (lowercase, numbers, hyphens only)"
            )

        # バージョン形式チェック
        if not self.VERSION_PATTERN.match(meta.version):
            result.add_error(
                f"Invalid version format: '{meta.version}'. Must be semver format (e.g., 1.0.0)"
            )

        # 名前の長さチェック
        if len(meta.name) < self.MIN_NAME_LENGTH:
            result.add_error("Agent name cannot be empty")
        elif len(meta.name) > self.MAX_NAME_LENGTH:
            result.add_warning(f"Agent name is too long (>{self.MAX_NAME_LENGTH} characters)")

        # 説明の長さチェック
        if len(meta.description) < self.MIN_DESCRIPTION_LENGTH:
            result.add_warning(
                f"Agent description is too short (<{self.MIN_DESCRIPTION_LENGTH} characters)"
            )
        elif len(meta.description) > self.MAX_DESCRIPTION_LENGTH:
            result.add_warning(
                f"Agent description is too long (>{self.MAX_DESCRIPTION_LENGTH} characters)"
            )

    def _validate_interfaces(self, metadata: AgentMetadata, result: ValidationResult) -> None:
        """インターフェースを検証.

        Args:
            metadata: Agent メタデータ
            result: 検証結果
        """
        interfaces = metadata.interfaces

        # 入力フィールドの検証
        if not interfaces.inputs:
            result.add_warning("No input fields defined")

        input_names = set()
        for input_field in interfaces.inputs:
            # 重複チェック
            if input_field.name in input_names:
                result.add_error(f"Duplicate input field name: '{input_field.name}'")
            input_names.add(input_field.name)

            # 型チェック
            if not input_field.type:
                result.add_error(f"Input field '{input_field.name}' has no type")

        # 出力フィールドの検証
        if not interfaces.outputs:
            result.add_warning("No output fields defined")

        output_names = set()
        for output_field in interfaces.outputs:
            # 重複チェック
            if output_field.name in output_names:
                result.add_error(f"Duplicate output field name: '{output_field.name}'")
            output_names.add(output_field.name)

            # 型チェック
            if not output_field.type:
                result.add_error(f"Output field '{output_field.name}' has no type")

    def _validate_protocols(self, metadata: AgentMetadata, result: ValidationResult) -> None:
        """プロトコル設定を検証.

        Args:
            metadata: Agent メタデータ
            result: 検証結果
        """
        protocols = metadata.protocols

        # 少なくとも1つのプロトコルが有効であることを確認
        has_protocol = False
        if protocols.mcp and (protocols.mcp.tools or protocols.mcp.resources):
            has_protocol = True
        if protocols.a2a:
            has_protocol = True
        if protocols.agui:
            has_protocol = True

        if not has_protocol:
            result.add_warning("No protocols enabled")

    def _validate_dependencies(self, metadata: AgentMetadata, result: ValidationResult) -> None:
        """依存関係を検証.

        Args:
            metadata: Agent メタデータ
            result: 検証結果
        """
        if not metadata.dependencies:
            return

        dependencies = metadata.dependencies

        # Agent 依存関係の検証
        if dependencies.agents:
            for agent_id in dependencies.agents:
                if not self.ID_PATTERN.match(agent_id):
                    result.add_error(f"Invalid dependent agent ID format: '{agent_id}'")

        # パッケージ依存関係の検証
        if dependencies.packages:
            for package in dependencies.packages:
                if not package or len(package.strip()) == 0:
                    result.add_error("Empty package dependency")

    def _validate_pocketflow(self, metadata: AgentMetadata, result: ValidationResult) -> None:
        """PocketFlow 設定を検証.

        Args:
            metadata: Agent メタデータ
            result: 検証結果
        """
        pocketflow = metadata.pocketflow

        # エントリーポイント形式チェック
        if ":" not in pocketflow.entry:
            result.add_error(
                f"Invalid PocketFlow entry format: '{pocketflow.entry}'. "
                "Expected format: 'module.py:function'"
            )
        else:
            module, func = pocketflow.entry.split(":", 1)
            if not module.endswith(".py"):
                result.add_warning(f"PocketFlow entry module should be a .py file: '{module}'")
            if not func or len(func.strip()) == 0:
                result.add_error("PocketFlow entry function name is empty")
