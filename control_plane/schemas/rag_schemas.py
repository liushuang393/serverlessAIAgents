"""RAG 管理 API 用スキーマ."""

from __future__ import annotations

from typing import Any

from pydantic import BaseModel, Field, field_validator, model_validator


_ALLOWED_SOURCE_TYPES = {"web", "file", "database", "api", "s3"}
_ALLOWED_DATABASE_READ_MODES = {"table", "query"}


class RAGDataSourceInput(BaseModel):
    """RAG データソース入力."""

    id: str | None = Field(default=None, min_length=1, description="安定データソースID")
    type: str = Field(..., min_length=1, description="データソース種別")
    uri: str | None = Field(default=None, description="接続先 URI / パス")
    label: str = Field(default="", description="表示ラベル")
    enabled: bool = Field(default=True, description="有効化")
    schedule: str | None = Field(default=None, description="更新スケジュール")
    options: dict[str, Any] = Field(default_factory=dict, description="追加設定")

    @field_validator("type", mode="before")
    @classmethod
    def normalize_type(cls, value: Any) -> str:
        text = _clean_text(value) or "web"
        normalized = text.lower()
        if normalized in {"db", "sql"}:
            normalized = "database"
        if normalized not in _ALLOWED_SOURCE_TYPES:
            msg = f"unsupported data source type: {normalized}"
            raise ValueError(msg)
        return normalized

    @field_validator("id", mode="before")
    @classmethod
    def normalize_id(cls, value: Any) -> str | None:
        return _clean_text(value)

    @field_validator("uri", mode="before")
    @classmethod
    def normalize_uri(cls, value: Any) -> str | None:
        return _clean_text(value)

    @field_validator("schedule", mode="before")
    @classmethod
    def normalize_schedule(cls, value: Any) -> str | None:
        return _clean_text(value)

    @field_validator("options", mode="before")
    @classmethod
    def ensure_options_dict(cls, value: Any) -> dict[str, Any]:
        return value if isinstance(value, dict) else {}

    @model_validator(mode="after")
    def validate_database_options(self) -> RAGDataSourceInput:
        if self.type != "database":
            if not self.uri:
                msg = "uri is required"
                raise ValueError(msg)
            return self

        options = dict(self.options)
        inferred = _infer_db_kind(self.uri)
        database_type = (
            _clean_text(options.get("database_type"))
            or _clean_text(options.get("dialect"))
            or inferred
            or "postgresql"
        ).lower()
        dialect = (_clean_text(options.get("dialect")) or database_type).lower()

        read_mode = (_clean_text(options.get("read_mode")) or "query").lower()
        if read_mode not in _ALLOWED_DATABASE_READ_MODES:
            read_mode = "query"

        has_table = bool(_clean_text(options.get("table"))) or _has_values(options.get("tables"))
        has_query = bool(_clean_text(options.get("query")))
        if read_mode == "table" and not has_table:
            read_mode = "query"
        if read_mode == "query" and not has_query:
            options["query"] = "SELECT 1 AS sample_value"

        row_limit_raw = options.get("row_limit")
        if row_limit_raw is not None and str(row_limit_raw).strip():
            try:
                row_limit = int(row_limit_raw)
            except (TypeError, ValueError) as exc:
                msg = "options.row_limit must be an integer"
                raise ValueError(msg) from exc
            if row_limit < 1 or row_limit > 5000:
                msg = "options.row_limit must be between 1 and 5000"
                raise ValueError(msg)
            options["row_limit"] = row_limit

        time_column = _clean_text(options.get("time_column"))
        if time_column:
            options["time_column"] = time_column

        options["database_type"] = database_type
        options["dialect"] = dialect
        options["read_mode"] = read_mode
        self.options = options
        return self


class RAGConfigPatchRequest(BaseModel):
    """App 単位の RAG 設定更新リクエスト."""

    enabled: bool | None = Field(default=None, description="RAG 有効化")
    pattern: str | None = Field(default=None, description="適用パターン")
    vector_provider: str | None = Field(default=None, description="VectorDB プロバイダ")
    vector_url: str | None = Field(default=None, description="VectorDB URL")
    vector_collection: str | None = Field(default=None, description="VectorDB コレクション")
    embedding_model: str | None = Field(default=None, description="埋め込みモデル")
    chunk_strategy: str | None = Field(default=None, description="チャンク分割方式")
    chunk_size: int | None = Field(default=None, ge=100, le=8000, description="チャンクサイズ")
    chunk_overlap: int | None = Field(default=None, ge=0, le=2000, description="チャンク重複サイズ")
    retrieval_method: str | None = Field(default=None, description="検索方式")
    reranker: str | None = Field(default=None, description="リランカー")
    top_k: int | None = Field(default=None, ge=1, le=100, description="Top K")
    score_threshold: float | None = Field(default=None, ge=0.0, le=1.0, description="スコア閾値")
    indexing_schedule: str | None = Field(default=None, description="インデックス更新スケジュール")
    data_sources: list[RAGDataSourceInput] | None = Field(
        default=None,
        description="データソース一覧",
    )


def _clean_text(value: Any) -> str | None:
    if value is None:
        return None
    text = str(value).strip()
    return text or None


def _has_values(value: Any) -> bool:
    if isinstance(value, list):
        return any(_clean_text(item) for item in value)
    if isinstance(value, str):
        return any(_clean_text(part) for part in value.split(","))
    return False


def _infer_db_kind(uri: str | None) -> str | None:
    value = _clean_text(uri)
    if not value:
        return None
    lowered = value.lower()
    if lowered.startswith("postgres") or "postgres" in lowered:
        return "postgresql"
    if lowered.startswith("mysql") or "mysql" in lowered:
        return "mysql"
    if lowered.startswith("sqlite") or "sqlite" in lowered:
        return "sqlite"
    if lowered.startswith("mssql") or "sqlserver" in lowered:
        return "mssql"
    return None
