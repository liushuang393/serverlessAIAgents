"""CLI-Native API schema definitions."""

from __future__ import annotations

from typing import Any, Literal

from pydantic import BaseModel, Field

from contracts.skill import CLIHarnessManifest


class CLINativeImportRequest(BaseModel):
    """CLI-Native import API request."""

    harness_path: str = Field(..., description="Import 対象 agent-harness ディレクトリ")
    harness_id: str | None = Field(default=None, description="登録用 harness_id")
    software_name: str | None = Field(default=None, description="表示用 software 名")
    force: bool = Field(default=True, description="既存登録を上書きするか")


class CLINativeBuildRequest(BaseModel):
    """CLI-Native build API request."""

    software_name: str = Field(..., description="対象ソフトウェア名")
    source_path: str = Field(..., description="変換対象ソースのパス")
    runtime_cli: Literal["codex", "claude"] | None = Field(default=None, description="使用する外部 CLI")
    dry_run: bool = Field(default=True, description="build を計画のみで返すか")


class CLINativeBuildResponse(BaseModel):
    """CLI-Native build API response."""

    success: bool = Field(..., description="build 成功可否")
    software_name: str = Field(..., description="対象ソフトウェア名")
    runtime_cli: str = Field(..., description="使用 CLI")
    repo_url: str = Field(..., description="CLI-Anything repo URL")
    repo_ref: str = Field(..., description="pin された ref")
    repo_dir: str = Field(..., description="managed checkout path")
    source_path: str = Field(..., description="対象 source path")
    output_dir: str = Field(..., description="出力 directory")
    planned_command: list[str] = Field(default_factory=list, description="実行予定コマンド")
    dry_run: bool = Field(default=True, description="dry-run かどうか")
    stdout: str = Field(default="", description="実行 stdout")
    stderr: str = Field(default="", description="実行 stderr")
    return_code: int | None = Field(default=None, description="実行 return code")


class CLINativeListResponse(BaseModel):
    """CLI-Native list API response."""

    harnesses: list[CLIHarnessManifest] = Field(default_factory=list, description="登録済み harness 一覧")
    total: int = Field(default=0, description="総件数")


class CLINativeDetailResponse(BaseModel):
    """CLI-Native detail API response."""

    harness: CLIHarnessManifest = Field(..., description="対象 harness")


class CLINativeExecutionPreview(BaseModel):
    """dry-run execution preview."""

    validated: bool = Field(default=True, description="validation 成否")
    harness_id: str = Field(..., description="対象 harness_id")
    command: list[str] = Field(default_factory=list, description="実行コマンド")
    install_state: str = Field(default="", description="install 状態")


class CLINativeExecutionResult(BaseModel):
    """CLI-Native runtime execution result."""

    harness_id: str = Field(..., description="対象 harness_id")
    command: list[str] = Field(default_factory=list, description="実行コマンド")
    return_code: int = Field(default=0, description="終了コード")
    stdout: str = Field(default="", description="標準出力")
    stderr: str = Field(default="", description="標準エラー")
    payload: dict[str, Any] = Field(default_factory=dict, description="JSON decode 済み payload")
    success: bool = Field(default=True, description="成功可否")
