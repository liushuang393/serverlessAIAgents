"""AgentFlow テンプレートマネージャー.

このモジュールはテンプレートの管理と生成を提供します。
"""

from __future__ import annotations

import shutil
from pathlib import Path
from typing import Any

import yaml
from jinja2 import Environment, FileSystemLoader
from pydantic import BaseModel, Field


class TemplateMetadata(BaseModel):
    """テンプレートメタデータ.

    テンプレートの情報とパラメーターを定義します。
    """

    id: str = Field(..., description="テンプレート ID")
    name: str = Field(..., description="テンプレート名")
    description: str = Field(..., description="説明")
    category: str = Field(..., description="カテゴリ")
    author: str = Field(..., description="作成者")
    version: str = Field(..., description="バージョン")
    parameters: list[TemplateParameter] = Field(
        default_factory=list, description="カスタマイズパラメーター"
    )


class TemplateParameter(BaseModel):
    """テンプレートパラメーター.

    テンプレートのカスタマイズ可能なパラメーターを定義します。
    """

    name: str = Field(..., description="パラメーター名")
    type: str = Field(..., description="型 (string, int, bool, list)")
    description: str = Field(..., description="説明")
    default: Any = Field(None, description="デフォルト値")
    required: bool = Field(True, description="必須かどうか")
    choices: list[str] | None = Field(None, description="選択肢")


class TemplateManager:
    """テンプレートマネージャー.

    テンプレートの検索、読み込み、生成を管理します。
    """

    def __init__(self, templates_dir: Path | None = None) -> None:
        """テンプレートマネージャーを初期化.

        Args:
            templates_dir: テンプレートディレクトリのパス
        """
        if templates_dir is None:
            # パッケージ内のテンプレートディレクトリを使用
            templates_dir = Path(__file__).parent / "scenarios"

        self.templates_dir = templates_dir
        self.templates_dir.mkdir(parents=True, exist_ok=True)

    def list_templates(self) -> list[TemplateMetadata]:
        """利用可能なテンプレート一覧を取得.

        Returns:
            テンプレートメタデータのリスト
        """
        templates = []

        for template_dir in self.templates_dir.iterdir():
            if not template_dir.is_dir():
                continue

            metadata_path = template_dir / "template.yaml"
            if not metadata_path.exists():
                continue

            try:
                with metadata_path.open("r", encoding="utf-8") as f:
                    data = yaml.safe_load(f)

                metadata = TemplateMetadata.model_validate(data)
                templates.append(metadata)
            except Exception as e:
                print(f"Warning: Failed to load template {template_dir.name}: {e}")
                continue

        return templates

    def get_template(self, template_id: str) -> TemplateMetadata | None:
        """テンプレートメタデータを取得.

        Args:
            template_id: テンプレート ID

        Returns:
            テンプレートメタデータ、存在しない場合 None
        """
        template_dir = self.templates_dir / template_id
        metadata_path = template_dir / "template.yaml"

        if not metadata_path.exists():
            return None

        with metadata_path.open("r", encoding="utf-8") as f:
            data = yaml.safe_load(f)

        return TemplateMetadata.model_validate(data)

    def generate_project(
        self,
        template_id: str,
        output_dir: Path,
        parameters: dict[str, Any],
    ) -> None:
        """テンプレートからプロジェクトを生成.

        Args:
            template_id: テンプレート ID
            output_dir: 出力ディレクトリ
            parameters: カスタマイズパラメーター

        Raises:
            ValueError: テンプレートが存在しない場合
            FileExistsError: 出力ディレクトリが既に存在する場合
        """
        # テンプレートを取得
        metadata = self.get_template(template_id)
        if metadata is None:
            msg = f"Template not found: {template_id}"
            raise ValueError(msg)

        # 出力ディレクトリをチェック
        if output_dir.exists():
            msg = f"Output directory already exists: {output_dir}"
            raise FileExistsError(msg)

        # パラメーターを検証
        validated_params = self._validate_parameters(metadata, parameters)

        # テンプレートディレクトリ
        template_dir = self.templates_dir / template_id

        # Jinja2 環境を作成
        env = Environment(
            loader=FileSystemLoader(template_dir),
            autoescape=False,
        )

        # 出力ディレクトリを作成
        output_dir.mkdir(parents=True, exist_ok=True)

        # テンプレートファイルを処理
        for template_file in template_dir.rglob("*"):
            # template.yaml はスキップ
            if template_file.name == "template.yaml":
                continue

            # ディレクトリの場合
            if template_file.is_dir():
                continue

            # 相対パスを取得
            rel_path = template_file.relative_to(template_dir)
            output_path = output_dir / rel_path

            # 出力ディレクトリを作成
            output_path.parent.mkdir(parents=True, exist_ok=True)

            # ファイルを処理
            if template_file.suffix in [".py", ".yaml", ".md", ".txt", ".json"]:
                # テキストファイルはテンプレートとして処理
                try:
                    template = env.get_template(str(rel_path))
                    content = template.render(**validated_params)
                    output_path.write_text(content, encoding="utf-8")
                except Exception as e:
                    print(f"Warning: Failed to render {rel_path}: {e}")
                    # レンダリング失敗時はコピー
                    shutil.copy2(template_file, output_path)
            else:
                # バイナリファイルはそのままコピー
                shutil.copy2(template_file, output_path)

    def _validate_parameters(
        self,
        metadata: TemplateMetadata,
        parameters: dict[str, Any],
    ) -> dict[str, Any]:
        """パラメーターを検証.

        Args:
            metadata: テンプレートメタデータ
            parameters: カスタマイズパラメーター

        Returns:
            検証済みパラメーター

        Raises:
            ValueError: パラメーターが不正な場合
        """
        validated = {}

        for param in metadata.parameters:
            value = parameters.get(param.name)

            # 必須パラメーターのチェック
            if param.required and value is None:
                if param.default is not None:
                    value = param.default
                else:
                    msg = f"Required parameter missing: {param.name}"
                    raise ValueError(msg)

            # デフォルト値を使用
            if value is None and param.default is not None:
                value = param.default

            # 型チェック
            if value is not None:
                if param.type == "string" and not isinstance(value, str):
                    msg = f"Parameter {param.name} must be string, got {type(value)}"
                    raise ValueError(msg)
                if param.type == "int" and not isinstance(value, int):
                    msg = f"Parameter {param.name} must be int, got {type(value)}"
                    raise ValueError(msg)
                if param.type == "bool" and not isinstance(value, bool):
                    msg = f"Parameter {param.name} must be bool, got {type(value)}"
                    raise ValueError(msg)
                if param.type == "list" and not isinstance(value, list):
                    msg = f"Parameter {param.name} must be list, got {type(value)}"
                    raise ValueError(msg)

                # 選択肢のチェック
                if param.choices and value not in param.choices:
                    msg = f"Parameter {param.name} must be one of {param.choices}, got {value}"
                    raise ValueError(
                        msg
                    )

            validated[param.name] = value

        return validated
