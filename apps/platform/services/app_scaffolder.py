# -*- coding: utf-8 -*-
"""App Scaffolder Service.

Platform の「APPを追加」から、最小構成の App 骨格を自動生成する。
"""

from __future__ import annotations

import json
import re
from pathlib import Path
from typing import Any, Literal

from apps.platform.schemas.provisioning_schemas import (
    AgentBlueprintInput,
    AppCreateRequest,
    AppCreateResponse,
)
from apps.platform.services.app_discovery import AppDiscoveryService
from apps.platform.services.framework_env import FrameworkEnvService
from apps.platform.services.port_allocator import PortAllocatorService


class AppScaffolderService:
    """新規 App 作成サービス."""

    _ENGINE_OPTIONS: tuple[dict[str, str], ...] = (
        {
            "value": "flow",
            "label": "Flow Builder",
            "description": "create_flow で宣言的に Agent を接続する標準パターン",
        },
        {
            "value": "pipeline",
            "label": "Pipeline Engine",
            "description": "段階的な業務パイプライン実行に適したパターン",
        },
        {
            "value": "coordinator",
            "label": "Coordinator",
            "description": "複数 Agent の協調制御を重視するパターン",
        },
        {
            "value": "deep_agent",
            "label": "Deep Agent",
            "description": "自己改善や長期タスク処理を想定した高度パターン",
        },
        {
            "value": "simple",
            "label": "Simple Engine",
            "description": "単一 Agent または軽量ユースケース向け",
        },
    )
    _VECTOR_DB_OPTIONS: tuple[dict[str, str], ...] = (
        {"value": "none", "label": "なし"},
        {"value": "qdrant", "label": "Qdrant"},
        {"value": "pinecone", "label": "Pinecone"},
        {"value": "weaviate", "label": "Weaviate"},
        {"value": "pgvector", "label": "PostgreSQL (pgvector)"},
        {"value": "milvus", "label": "Milvus"},
    )
    _LLM_PROVIDER_OPTIONS: tuple[dict[str, str], ...] = (
        {"value": "auto", "label": "Auto"},
        {"value": "openai", "label": "OpenAI"},
        {"value": "anthropic", "label": "Anthropic"},
        {"value": "gemini", "label": "Google Gemini"},
        {"value": "azure_openai", "label": "Azure OpenAI"},
        {"value": "ollama", "label": "Ollama"},
        {"value": "openrouter", "label": "OpenRouter"},
        {"value": "custom", "label": "Custom"},
    )
    _BUSINESS_BASE_OPTIONS: tuple[dict[str, str], ...] = (
        {"value": "custom", "label": "Custom"},
        {"value": "platform", "label": "Platform"},
        {"value": "knowledge", "label": "Knowledge"},
        {"value": "reasoning", "label": "Reasoning"},
        {"value": "interaction", "label": "Interaction"},
        {"value": "integration", "label": "Integration"},
        {"value": "operations", "label": "Operations"},
        {"value": "governance", "label": "Governance"},
        {"value": "media", "label": "Media"},
    )
    _AGENT_PATTERN_OPTIONS: tuple[dict[str, str], ...] = (
        {"value": "specialist", "label": "Specialist"},
        {"value": "coordinator", "label": "Coordinator"},
        {"value": "pipeline_stage", "label": "Pipeline Stage"},
        {"value": "gatekeeper", "label": "Gatekeeper"},
        {"value": "reviewer", "label": "Reviewer"},
        {"value": "analyzer", "label": "Analyzer"},
        {"value": "executor", "label": "Executor"},
        {"value": "router", "label": "Router"},
        {"value": "reporter", "label": "Reporter"},
    )
    _BUSINESS_TEMPLATES: tuple[dict[str, str], ...] = (
        {
            "id": "migration_safe_assessment",
            "studio": "migration",
            "label": "安全性・拡張性評価",
            "description": "リポジトリ解析、リスク一覧、証拠付きレポートを生成",
        },
        {
            "id": "faq_cited_service",
            "studio": "faq",
            "label": "引用付きFAQ",
            "description": "検索＋引用、索引状態、運用レポートを構築",
        },
        {
            "id": "assistant_controlled_ops",
            "studio": "assistant",
            "label": "制御付きアシスタント",
            "description": "承認付きのOS/ブラウザ操作と監査ログを提供",
        },
    )

    def __init__(
        self,
        discovery: AppDiscoveryService,
        apps_dir: Path | None = None,
    ) -> None:
        """初期化.

        Args:
            discovery: App 検出サービス
            apps_dir: apps ルートパス
        """
        self._discovery = discovery
        self._apps_dir = apps_dir or (Path.cwd() / "apps")
        self._port_allocator = PortAllocatorService(discovery)

    @classmethod
    def create_options(
        cls,
        surface_profile: Literal["business", "developer", "operator"] = "developer",
    ) -> dict[str, Any]:
        """App 作成 UI 向け選択肢を返す."""
        if surface_profile == "business":
            return {
                "surface_profile": "business",
                "templates": list(cls._BUSINESS_TEMPLATES),
                "data_source_options": [
                    {"value": "git_repository", "label": "Git Repository"},
                    {"value": "knowledge_base", "label": "Knowledge Base"},
                    {"value": "database", "label": "Database"},
                    {"value": "object_storage", "label": "Object Storage"},
                ],
                "permission_scopes": [
                    {"value": "repo.read", "label": "Repo Read"},
                    {"value": "repo.write", "label": "Repo Write"},
                    {"value": "db.read", "label": "DB Read"},
                    {"value": "network.egress", "label": "Network Egress"},
                    {"value": "os.exec", "label": "OS Command"},
                    {"value": "browser.control", "label": "Browser Control"},
                ],
                "risk_levels": [
                    {"value": "low", "label": "Low"},
                    {"value": "medium", "label": "Medium"},
                    {"value": "high", "label": "High"},
                ],
                "security_modes": [
                    {"value": "read_only", "label": "Read-only"},
                    {"value": "approval_required", "label": "Approval Required"},
                    {"value": "autonomous", "label": "Autonomous"},
                ],
            }

        return {
            "surface_profile": surface_profile,
            "engine_patterns": list(cls._ENGINE_OPTIONS),
            "database_options": [
                {"value": "none", "label": "なし"},
                {"value": "sqlite", "label": "SQLite"},
                {"value": "postgresql", "label": "PostgreSQL"},
            ],
            "vector_database_options": list(cls._VECTOR_DB_OPTIONS),
            "llm_provider_options": list(cls._LLM_PROVIDER_OPTIONS),
            "business_base_options": list(cls._BUSINESS_BASE_OPTIONS),
            "agent_pattern_options": list(cls._AGENT_PATTERN_OPTIONS),
            "visibility_modes": [
                {"value": "private", "label": "Private"},
                {"value": "public", "label": "Public"},
                {"value": "tenant_allowlist", "label": "Tenant Allowlist"},
            ],
        }

    async def create_app(self, request: AppCreateRequest) -> AppCreateResponse:
        """新規 App を作成.

        Args:
            request: 作成リクエスト

        Returns:
            作成結果

        Raises:
            ValueError: 入力が不正な場合
            FileExistsError: 同名 App が存在する場合
        """
        if self._discovery.get_app(request.name) is not None:
            msg = f"App は既に存在します: {request.name}"
            raise FileExistsError(msg)

        app_dir = self._apps_dir / request.name
        if app_dir.exists():
            msg = f"ディレクトリは既に存在します: {app_dir}"
            raise FileExistsError(msg)

        ports = self._port_allocator.allocate_for_new_app(
            frontend_enabled=request.frontend_enabled,
            database=request.database,
            redis_enabled=request.redis_enabled,
        )

        agents = self._normalize_agents(request.agents, request.name)
        llm_api_key_env = self._resolve_llm_api_key_env(request)
        vector_db_api_key_env = self._resolve_vector_db_api_key_env(request)
        app_config = self._build_app_config(
            request=request,
            ports=ports,
            agents=agents,
            llm_api_key_env=llm_api_key_env,
            vector_db_api_key_env=vector_db_api_key_env,
        )

        files_created = self._write_scaffold(
            request=request,
            app_dir=app_dir,
            ports=ports,
            agents=agents,
            app_config=app_config,
        )

        framework_env_file: str | None = None
        framework_env_updated_keys: list[str] = []
        if request.write_framework_env:
            framework_env_file, framework_env_updated_keys = self._sync_framework_env(
                request=request,
                ports=ports,
                llm_api_key_env=llm_api_key_env,
                vector_db_api_key_env=vector_db_api_key_env,
            )

        await self._discovery.scan()

        return AppCreateResponse(
            success=True,
            app_name=request.name,
            app_dir=str(app_dir),
            config_path=str(app_dir / "app_config.json"),
            ports=ports,
            files_created=files_created,
            framework_env_file=framework_env_file,
            framework_env_updated_keys=framework_env_updated_keys,
            app_config=app_config,
        )

    def _build_app_config(
        self,
        *,
        request: AppCreateRequest,
        ports: dict[str, int | None],
        agents: list[dict[str, Any]],
        llm_api_key_env: str | None,
        vector_db_api_key_env: str | None,
    ) -> dict[str, Any]:
        """app_config.json 本体を生成."""
        dependencies_external: list[str] = []
        if request.mcp_servers:
            dependencies_external.append("mcp")
        if request.llm_provider != "auto":
            dependencies_external.append(request.llm_provider)
        if request.vector_database != "none":
            dependencies_external.append(request.vector_database)
        if request.rag_enabled:
            dependencies_external.append("vector_store")

        db_kind = None if request.database == "none" else request.database
        rag_provider: str | None = None
        if request.rag_enabled:
            rag_provider = (
                request.vector_database if request.vector_database != "none" else "qdrant"
            )
        rag_pattern = "balanced_knowledge" if request.rag_enabled else None

        db_url: str | None = None
        db_user: str | None = None
        db_password: str | None = None
        db_password_env: str | None = None
        if request.database == "postgresql" and ports["db"] is not None:
            db_user = request.name
            db_password = f"{request.name}_password"
            db_password_env = f"{request.name.upper()}_DB_PASSWORD"
            db_url = (
                f"postgresql+asyncpg://{db_user}:{db_password}"
                f"@localhost:{ports['db']}/{request.name}"
            )
        elif request.database == "sqlite":
            db_url = "sqlite:///./data/app.db"

        backend_url = None if ports["api"] is None else f"http://localhost:{ports['api']}"
        frontend_url = (
            None if ports["frontend"] is None else f"http://localhost:{ports['frontend']}"
        )
        health_url = None if backend_url is None else f"{backend_url}/health"
        auth_enabled = bool(request.permission_scopes)
        auth_providers = ["rbac"] if auth_enabled else []
        release_require_approval = (
            request.risk_level in {"medium", "high"} if request.risk_level is not None else True
        )

        return {
            "name": request.name,
            "display_name": request.display_name,
            "description": request.description,
            "business_base": request.business_base,
            "version": "0.1.0",
            "icon": request.icon,
            "product_line": request.product_line,
            "surface_profile": request.surface_profile,
            "audit_profile": request.audit_profile,
            "plugin_bindings": [
                {
                    "id": binding.id,
                    "version": binding.version,
                    "config": binding.config,
                }
                for binding in request.plugin_bindings
            ],
            "security_mode": request.security_mode,
            "ports": ports,
            "entry_points": {
                "api_module": f"apps.{request.name}.main:app",
                "health": "/health",
            },
            "agents": [
                {
                    "name": agent["name"],
                    "module": f"apps.{request.name}.agents.{agent['module_name']}",
                    "capabilities": agent["capabilities"],
                    "business_base": agent["business_base"] or request.business_base,
                    "pattern": agent["pattern"],
                }
                for agent in agents
            ],
            "services": {
                "engine": {
                    "pattern": request.engine_pattern,
                    "flow_pattern": request.flow_pattern,
                },
                "business_setup": {
                    "template": request.template,
                    "data_sources": request.data_sources,
                    "risk_level": request.risk_level,
                },
                "mcp": {
                    "servers": request.mcp_servers,
                },
                "llm": {
                    "provider": request.llm_provider,
                    "default_model": request.default_model,
                    "base_url": request.llm_base_url,
                    "api_key_env": llm_api_key_env,
                },
                "vector_db": {
                    "provider": None
                    if request.vector_database == "none"
                    else request.vector_database,
                    "url": request.vector_db_url,
                    "collection": request.vector_db_collection,
                    "api_key_env": vector_db_api_key_env,
                },
                "rag": {
                    "enabled": request.rag_enabled,
                    "pattern": rag_pattern,
                    "chunking": {
                        "strategy": "recursive",
                        "size": 800,
                        "overlap": 120,
                    },
                    "retrieval": {
                        "method": "hybrid",
                        "reranker": None,
                        "top_k": 5,
                        "score_threshold": None,
                    },
                    "data_sources": [],
                    "indexing_schedule": None,
                },
            },
            "dependencies": {
                "database": db_kind,
                "redis": request.redis_enabled,
                "external": sorted(set(dependencies_external)),
            },
            "runtime": {
                "urls": {
                    "backend": backend_url,
                    "frontend": frontend_url,
                    "health": health_url,
                    "database": db_url,
                },
                "database": {
                    "kind": db_kind,
                    "url": db_url,
                    "host": "localhost" if db_kind in {"postgresql", "mysql", "redis"} else None,
                    "port": ports["db"],
                    "name": request.name if db_kind == "postgresql" else None,
                    "user": db_user,
                    "password": db_password,
                    "password_env": db_password_env,
                    "note": "ローカル開発用の既定値。運用環境では環境変数を使用してください。",
                },
                "commands": {
                    "backend_dev": f"python -m apps.{request.name}.main",
                    "frontend_dev": (
                        f"cd apps/{request.name}/frontend && npm run dev"
                        if request.frontend_enabled
                        else None
                    ),
                    "publish": "docker compose up -d --build",
                    "start": "docker compose up -d",
                    "stop": "docker compose down",
                },
            },
            "contracts": {
                "auth": {
                    "enabled": auth_enabled,
                    "providers": auth_providers,
                    "allow_anonymous": not auth_enabled,
                    "required_scopes": request.permission_scopes,
                    "session_ttl_minutes": 60,
                },
                "rag": {
                    "enabled": request.rag_enabled,
                    "pattern": rag_pattern,
                    "provider": rag_provider,
                    "collections": [f"{request.name}_knowledge"] if request.rag_enabled else [],
                    "data_sources": [],
                    "chunk_strategy": "recursive",
                    "chunk_size": 800,
                    "chunk_overlap": 120,
                    "retrieval_method": "hybrid",
                    "embedding_model": "text-embedding-3-small" if request.rag_enabled else None,
                    "rerank_model": None,
                    "default_top_k": 5,
                    "score_threshold": None,
                    "indexing_schedule": None,
                },
                "skills": {
                    "auto_install": bool(request.default_skills),
                    "hot_reload": True,
                    "allowed_sources": ["agentflow/skills/builtin"],
                    "default_skills": request.default_skills,
                },
                "release": {
                    "strategy": "manual",
                    "targets": ["local", "docker"],
                    "environments": ["dev"],
                    "require_approval": release_require_approval,
                },
            },
            "blueprint": {
                "engine_pattern": request.engine_pattern,
                "flow_pattern": request.flow_pattern,
                "system_prompt": request.system_prompt,
                "llm_provider": request.llm_provider,
                "llm_base_url": request.llm_base_url,
                "llm_api_key_env": llm_api_key_env,
                "default_model": request.default_model,
                "default_skills": request.default_skills,
                "vector_db_provider": None
                if request.vector_database == "none"
                else request.vector_database,
                "vector_db_url": request.vector_db_url,
                "vector_db_collection": request.vector_db_collection,
                "vector_db_api_key_env": vector_db_api_key_env,
                "mcp_servers": request.mcp_servers,
                "agents": [
                    {
                        "name": agent["name"],
                        "role": agent["role"],
                        "prompt": agent["prompt"],
                        "capabilities": agent["capabilities"],
                    }
                    for agent in agents
                ],
            },
            "visibility": {
                "mode": request.tenant_visibility_mode,
                "tenants": request.tenant_ids,
            },
            "tags": [
                "platform-generated",
                request.engine_pattern,
                request.database,
                request.llm_provider,
                request.vector_database,
                request.surface_profile,
                *(["risk-low"] if request.risk_level == "low" else []),
                *(["risk-medium"] if request.risk_level == "medium" else []),
                *(["risk-high"] if request.risk_level == "high" else []),
                *(["template"] if request.template else []),
                *(["rag"] if request.rag_enabled else []),
                "app-builder",
            ],
        }

    def _write_scaffold(
        self,
        *,
        request: AppCreateRequest,
        app_dir: Path,
        ports: dict[str, int | None],
        agents: list[dict[str, Any]],
        app_config: dict[str, Any],
    ) -> list[str]:
        """App 骨格ファイルを生成."""
        created: list[str] = []

        app_dir.mkdir(parents=True, exist_ok=False)
        (app_dir / "agents").mkdir(parents=True, exist_ok=False)
        (app_dir / "prompts").mkdir(parents=True, exist_ok=False)

        self._write_file(app_dir / "__init__.py", "", created)
        self._write_file(
            app_dir / "agents" / "__init__.py",
            self._render_agents_init(agents),
            created,
        )
        self._write_file(app_dir / "engine.py", self._render_engine_py(request), created)
        self._write_file(app_dir / "main.py", self._render_main_py(request), created)
        self._write_file(
            app_dir / "prompts" / "system_prompt.md",
            request.system_prompt.strip() or "あなたは業務特化の AI アシスタントです。",
            created,
        )
        self._write_file(
            app_dir / ".env.example",
            self._render_env_example(request, ports),
            created,
        )
        self._write_file(
            app_dir / "README.md",
            self._render_readme(request, ports),
            created,
        )
        self._write_file(
            app_dir / "docker-compose.yml",
            self._render_docker_compose(request, ports),
            created,
        )

        for agent in agents:
            self._write_file(
                app_dir / "agents" / f"{agent['module_name']}.py",
                self._render_agent_file(agent),
                created,
            )

        config_path = app_dir / "app_config.json"
        config_path.write_text(
            json.dumps(app_config, ensure_ascii=False, indent=2) + "\n",
            encoding="utf-8",
        )
        created.append(str(config_path.relative_to(Path.cwd())))

        return created

    def _resolve_llm_api_key_env(self, request: AppCreateRequest) -> str | None:
        """LLM API キー env 名を解決."""
        if request.llm_provider in {"ollama"}:
            return None
        if request.llm_api_key_env:
            return request.llm_api_key_env

        prefix = request.name.upper()
        defaults = {
            "openai": "OPENAI_API_KEY",
            "anthropic": "ANTHROPIC_API_KEY",
            "gemini": "GEMINI_API_KEY",
            "azure_openai": "AZURE_OPENAI_API_KEY",
            "openrouter": "OPENROUTER_API_KEY",
        }
        return defaults.get(request.llm_provider, f"{prefix}_LLM_API_KEY")

    def _resolve_vector_db_api_key_env(self, request: AppCreateRequest) -> str | None:
        """VectorDB API キー env 名を解決."""
        if request.vector_database in {"none", "pgvector"}:
            return None
        if request.vector_db_api_key_env:
            return request.vector_db_api_key_env

        prefix = request.name.upper()
        defaults = {
            "qdrant": "QDRANT_API_KEY",
            "pinecone": "PINECONE_API_KEY",
            "weaviate": "WEAVIATE_API_KEY",
            "milvus": "MILVUS_API_KEY",
        }
        return defaults.get(request.vector_database, f"{prefix}_VECTOR_DB_API_KEY")

    def _sync_framework_env(
        self,
        *,
        request: AppCreateRequest,
        ports: dict[str, int | None],
        llm_api_key_env: str | None,
        vector_db_api_key_env: str | None,
    ) -> tuple[str, list[str]]:
        """フレームワークローカル env へ設定を書き込む."""
        env_path = (Path.cwd() / request.framework_env_file).resolve()
        service = FrameworkEnvService(env_path)
        prefix = request.name.upper()

        values: dict[str, str] = {
            f"{prefix}_DATABASE": request.database,
            f"{prefix}_VECTOR_DB_PROVIDER": request.vector_database,
            f"{prefix}_LLM_PROVIDER": request.llm_provider,
            f"{prefix}_API_PORT": str(ports["api"] or ""),
            f"{prefix}_FRONTEND_PORT": str(ports["frontend"] or ""),
            f"{prefix}_DB_PORT": str(ports["db"] or ""),
            f"{prefix}_REDIS_PORT": str(ports["redis"] or ""),
        }

        if request.default_model:
            values[f"{prefix}_DEFAULT_MODEL"] = request.default_model
        if request.llm_base_url:
            values[f"{prefix}_LLM_BASE_URL"] = request.llm_base_url
        if request.vector_db_url:
            values[f"{prefix}_VECTOR_DB_URL"] = request.vector_db_url
        if request.vector_db_collection:
            values[f"{prefix}_VECTOR_DB_COLLECTION"] = request.vector_db_collection

        if llm_api_key_env and request.llm_api_key:
            values[llm_api_key_env] = request.llm_api_key
        if vector_db_api_key_env and request.vector_db_api_key:
            values[vector_db_api_key_env] = request.vector_db_api_key

        filtered = {key: value for key, value in values.items() if value != ""}
        updated = service.upsert(filtered)
        try:
            relative = str(env_path.relative_to(Path.cwd()))
        except ValueError:
            relative = str(env_path)
        return relative, updated

    @staticmethod
    def _write_file(path: Path, content: str, created: list[str]) -> None:
        """テキストファイルを作成."""
        path.write_text(content.rstrip() + "\n", encoding="utf-8")
        created.append(str(path.relative_to(Path.cwd())))

    @staticmethod
    def _normalize_agents(
        agents: list[AgentBlueprintInput],
        app_name: str,
    ) -> list[dict[str, Any]]:
        """Agent 入力を正規化."""
        normalized: list[dict[str, Any]] = []

        source_agents = agents
        if not source_agents:
            source_agents = [
                AgentBlueprintInput(
                    name=f"{app_name.replace('_', ' ').title().replace(' ', '')}Agent",
                    role="specialist",
                    prompt="ユーザー要求に対し、正確で実用的な応答を返してください。",
                    capabilities=["assistant"],
                )
            ]

        seen_names: set[str] = set()
        for item in source_agents:
            clean_name = item.name.strip()
            if not clean_name:
                continue
            if clean_name in seen_names:
                continue
            seen_names.add(clean_name)

            module_name = AppScaffolderService._to_snake(clean_name)
            normalized.append(
                {
                    "name": clean_name,
                    "class_name": AppScaffolderService._to_class_name(clean_name),
                    "module_name": module_name,
                    "role": item.role.strip() or "specialist",
                    "prompt": item.prompt.strip(),
                    "capabilities": [c.strip() for c in item.capabilities if c.strip()],
                    "business_base": (
                        item.business_base.strip().lower()
                        if isinstance(item.business_base, str) and item.business_base.strip()
                        else None
                    ),
                    "pattern": (
                        item.pattern.strip().lower()
                        if isinstance(item.pattern, str) and item.pattern.strip()
                        else "specialist"
                    ),
                }
            )

        if not normalized:
            msg = "有効な Agent がありません"
            raise ValueError(msg)

        return normalized

    @staticmethod
    def _to_snake(value: str) -> str:
        """文字列を snake_case に変換."""
        sanitized = re.sub(r"[^A-Za-z0-9]+", "_", value)
        sanitized = re.sub(r"([a-z0-9])([A-Z])", r"\1_\2", sanitized)
        sanitized = sanitized.strip("_").lower()
        return sanitized or "agent"

    @staticmethod
    def _to_class_name(value: str) -> str:
        """文字列を Python クラス名に変換."""
        parts = re.split(r"[^A-Za-z0-9]+", value)
        merged = "".join(p[:1].upper() + p[1:] for p in parts if p)
        if not merged:
            return "GeneratedAgent"
        if merged[0].isdigit():
            return f"Agent{merged}"
        return merged

    @staticmethod
    def _render_agents_init(agents: list[dict[str, Any]]) -> str:
        """agents/__init__.py を生成."""
        imports = [f"from .{a['module_name']} import {a['class_name']}" for a in agents]
        export_list = ", ".join([f'"{a["class_name"]}"' for a in agents])
        return "\n".join(
            [
                *imports,
                "",
                f"__all__ = [{export_list}]",
            ]
        )

    @staticmethod
    def _render_engine_py(request: AppCreateRequest) -> str:
        """engine.py を生成."""
        flow_pattern = request.flow_pattern or "default"
        return f'''"""{request.display_name} - App Engine."""

from __future__ import annotations

from typing import Any


class AppEngine:
    """{request.engine_pattern} パターン用の最小エンジン."""

    def __init__(self) -> None:
        self.engine_pattern = "{request.engine_pattern}"
        self.flow_pattern = "{flow_pattern}"

    async def run(self, payload: dict[str, Any]) -> dict[str, Any]:
        """入力を受け、最小レスポンスを返す.

        TODO:
            - AgentFlow の実エンジンに差し替え
            - blueprint.system_prompt / skills / mcp 連携を追加
        """
        return {{
            "status": "ok",
            "engine_pattern": self.engine_pattern,
            "flow_pattern": self.flow_pattern,
            "payload": payload,
        }}
'''

    @staticmethod
    def _render_main_py(request: AppCreateRequest) -> str:
        """main.py を生成."""
        return f'''"""{request.display_name} - API エントリポイント."""

from __future__ import annotations

from typing import Any

from fastapi import FastAPI
from pydantic import BaseModel, Field

from apps.{request.name}.engine import AppEngine


app = FastAPI(
    title="{request.display_name}",
    description="{request.description}",
    version="0.1.0",
)

_engine = AppEngine()


class AgentRunRequest(BaseModel):
    """Agent 実行リクエスト."""

    message: str = Field(..., min_length=1, description="ユーザー入力")
    context: dict[str, Any] = Field(default_factory=dict, description="追加コンテキスト")


@app.get("/")
async def root() -> dict[str, Any]:
    """サービス情報."""
    return {{"name": "{request.name}", "display_name": "{request.display_name}", "version": "0.1.0"}}


@app.get("/health")
async def health() -> dict[str, str]:
    """ヘルスチェック."""
    return {{"status": "healthy", "service": "{request.name}"}}


@app.post("/api/agent/run")
async def run_agent(body: AgentRunRequest) -> dict[str, Any]:
    """最小 Agent 実行 API."""
    return await _engine.run({{"message": body.message, "context": body.context}})
'''

    @staticmethod
    def _render_agent_file(agent: dict[str, Any]) -> str:
        """Agent スタブを生成."""
        prompt = agent["prompt"] or "ユーザー要求を解釈し、行動可能な応答を返してください。"
        caps = ", ".join([f'"{c}"' for c in agent["capabilities"]])

        return f'''"""{agent["name"]} - Agent スタブ."""

from __future__ import annotations

from typing import Any


class {agent["class_name"]}:
    """{agent["role"]} Agent."""

    name = "{agent["name"]}"
    role = "{agent["role"]}"
    business_base = "{agent["business_base"] or "custom"}"
    pattern = "{agent["pattern"]}"
    default_prompt = "{prompt}"
    capabilities = [{caps}]

    async def run(self, payload: dict[str, Any]) -> dict[str, Any]:
        """Agent 実行の最小実装."""
        return {{
            "agent": self.name,
            "role": self.role,
            "business_base": self.business_base,
            "pattern": self.pattern,
            "capabilities": self.capabilities,
            "received": payload,
        }}
'''

    def _render_env_example(
        self,
        request: AppCreateRequest,
        ports: dict[str, int | None],
    ) -> str:
        """.env.example を生成."""
        llm_api_key_env = self._resolve_llm_api_key_env(request)
        vector_db_api_key_env = self._resolve_vector_db_api_key_env(request)

        lines = [
            f"APP_NAME={request.name}",
            "APP_ENV=development",
            f"API_PORT={ports['api']}",
            f"LLM_PROVIDER={request.llm_provider}",
        ]

        if ports["frontend"]:
            lines.append(f"FRONTEND_PORT={ports['frontend']}")
        if request.default_model:
            lines.append(f"DEFAULT_MODEL={request.default_model}")
        if request.llm_base_url:
            lines.append(f"LLM_BASE_URL={request.llm_base_url}")
        if llm_api_key_env:
            lines.append(f"{llm_api_key_env}=<your-api-key>")

        if request.database == "postgresql" and ports["db"] is not None:
            lines.extend(
                [
                    f"DB_PORT={ports['db']}",
                    f"DB_USER={request.name}",
                    f"DB_PASSWORD={request.name}_password",
                    f"DB_NAME={request.name}",
                    (
                        "DATABASE_URL="
                        f"postgresql+asyncpg://{request.name}:{request.name}_password"
                        f"@localhost:{ports['db']}/{request.name}"
                    ),
                ]
            )
        elif request.database == "sqlite":
            lines.append("DATABASE_URL=sqlite:///./data/app.db")

        if request.redis_enabled and ports["redis"] is not None:
            lines.extend(
                [
                    f"REDIS_PORT={ports['redis']}",
                    f"REDIS_URL=redis://localhost:{ports['redis']}/0",
                ]
            )

        if request.vector_database != "none":
            lines.extend(
                [
                    f"VECTOR_DB_PROVIDER={request.vector_database}",
                ]
            )
            if request.vector_db_url:
                lines.append(f"VECTOR_DB_URL={request.vector_db_url}")
            if request.vector_db_collection:
                lines.append(f"VECTOR_DB_COLLECTION={request.vector_db_collection}")
            if vector_db_api_key_env:
                lines.append(f"{vector_db_api_key_env}=<your-vector-db-key>")

        return "\n".join(lines)

    @staticmethod
    def _render_readme(request: AppCreateRequest, ports: dict[str, int | None]) -> str:
        """README.md を生成."""
        return f"""# {request.display_name}

{request.description or "Platform 生成の新規 App"}

## クイックスタート

```bash
python -m apps.{request.name}.main
```

または:

```bash
uvicorn apps.{request.name}.main:app --reload --port {ports["api"]}
```

## エンドポイント

- `GET /health`
- `POST /api/agent/run`

## 設計メモ

- Engine Pattern: `{request.engine_pattern}`
- Business Base: `{request.business_base}`
- Flow Pattern: `{request.flow_pattern or "default"}`
- Database: `{request.database}`
- Vector DB: `{request.vector_database}`
- LLM Provider: `{request.llm_provider}`
- Default Model: `{request.default_model or "auto"}`
- Redis: `{request.redis_enabled}`
- Frontend: `{request.frontend_enabled}`
"""

    @staticmethod
    def _render_docker_compose(
        request: AppCreateRequest,
        ports: dict[str, int | None],
    ) -> str:
        """最小 docker-compose.yml を生成."""
        blocks: list[str] = [
            "services:",
        ]

        if request.database == "postgresql" and ports["db"] is not None:
            blocks.extend(
                [
                    "  db:",
                    "    image: postgres:16-alpine",
                    f"    container_name: {request.name}_db",
                    "    restart: unless-stopped",
                    "    environment:",
                    f"      POSTGRES_DB: {request.name}",
                    f"      POSTGRES_USER: {request.name}",
                    f"      POSTGRES_PASSWORD: {request.name}_password",
                    "    ports:",
                    f'      - "{ports["db"]}:5432"',
                ]
            )

        if request.redis_enabled and ports["redis"] is not None:
            blocks.extend(
                [
                    "  redis:",
                    "    image: redis:7-alpine",
                    f"    container_name: {request.name}_redis",
                    "    restart: unless-stopped",
                    "    ports:",
                    f'      - "{ports["redis"]}:6379"',
                ]
            )

        blocks.extend(
            [
                "  api:",
                "    image: python:3.13-slim",
                f"    container_name: {request.name}_api",
                "    working_dir: /workspace",
                "    command: >",
                (
                    f'      bash -lc "pip install -e .[apps] && '
                    f'uvicorn apps.{request.name}.main:app --host 0.0.0.0 --port 8000"'
                ),
                "    ports:",
                f'      - "{ports["api"]}:8000"',
                "    volumes:",
                "      - ../../:/workspace",
            ]
        )

        return "\n".join(blocks)
