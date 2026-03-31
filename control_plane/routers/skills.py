"""Skills Router — Skill カタログ API エンドポイント.

GET  /api/studios/framework/skills              — 全スキル一覧
GET  /api/studios/framework/skills/stats        — スキル統計
GET  /api/studios/framework/skills/tags         — 全タグ一覧
GET  /api/studios/framework/skills/categories   — カテゴリ一覧
GET  /api/studios/framework/skills/grouped      — カテゴリ別グループ一覧
GET  /api/studios/framework/skills/search       — タグベース検索
GET  /api/studios/framework/skills/category/{category_id} — カテゴリ別スキル一覧
GET  /api/studios/framework/skills/{skill_name} — スキル詳細
"""

from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING, Any

from fastapi import APIRouter, HTTPException, Query

from control_plane.schemas.cli_native_schemas import (
    CLINativeBuildRequest,
    CLINativeBuildResponse,
    CLINativeDetailResponse,
    CLINativeImportRequest,
    CLINativeListResponse,
)
from infrastructure.cli_native import CLINativeService


if TYPE_CHECKING:
    from control_plane.services.skill_catalog import SkillCatalogService


router = APIRouter(prefix="/api/studios/framework/skills", tags=["skills"])

# モジュールレベルのシングルトン（main.py で初期化）
_catalog: SkillCatalogService | None = None
_cli_native_service: CLINativeService | None = None


def init_skill_services(
    catalog: SkillCatalogService,
    cli_native_service: CLINativeService | None = None,
) -> None:
    """サービスインスタンスを設定.

    Args:
        catalog: Skill カタログサービス
        cli_native_service: CLI-Native 管理サービス
    """
    global _catalog
    global _cli_native_service
    _catalog = catalog
    _cli_native_service = cli_native_service or CLINativeService()


def _get_catalog() -> SkillCatalogService:
    """SkillCatalogService を取得（未初期化時はエラー）."""
    if _catalog is None:
        msg = "SkillCatalogService が未初期化です"
        raise RuntimeError(msg)
    return _catalog


def _get_cli_native_service() -> CLINativeService:
    """CLI-Native service を取得する."""
    global _cli_native_service
    if _cli_native_service is None:
        _cli_native_service = CLINativeService()
    return _cli_native_service


# ------------------------------------------------------------------
# エンドポイント
# ------------------------------------------------------------------


@router.get("")
async def list_skills() -> dict[str, Any]:
    """全スキル一覧."""
    catalog = _get_catalog()
    skills = catalog.list_skills()
    return {
        "skills": [s.to_dict() for s in skills],
        "total": len(skills),
    }


@router.get("/stats")
async def get_skill_stats() -> dict[str, Any]:
    """スキル統計情報."""
    return _get_catalog().stats()


@router.get("/tags")
async def list_tags() -> dict[str, Any]:
    """全タグとその出現回数."""
    catalog = _get_catalog()
    tags = catalog.all_tags()
    return {"tags": tags, "total": len(tags)}


@router.get("/categories")
async def list_categories() -> dict[str, Any]:
    """利用可能なカテゴリ一覧."""
    catalog = _get_catalog()
    categories = catalog.get_categories()
    return {"categories": categories, "total": len(categories)}


@router.get("/grouped")
async def list_skills_grouped() -> dict[str, Any]:
    """カテゴリ別にグループ化されたスキル一覧."""
    catalog = _get_catalog()
    groups = catalog.get_skills_grouped_by_category()
    return {"groups": groups, "total_categories": len(groups)}


@router.get("/category/{category_id}")
async def get_skills_by_category(category_id: str) -> dict[str, Any]:
    """指定カテゴリのスキル一覧.

    Args:
        category_id: カテゴリ ID
    """
    catalog = _get_catalog()
    skills = catalog.get_skills_by_category(category_id)
    return {
        "skills": [s.to_dict() for s in skills],
        "total": len(skills),
        "category": category_id,
    }


@router.get("/search")
async def search_skills(
    tag: str = Query(..., min_length=1, description="検索するタグ"),
) -> dict[str, Any]:
    """タグでスキルを検索.

    Args:
        tag: 検索するタグ
    """
    catalog = _get_catalog()
    skills = catalog.search_by_tag(tag)
    return {
        "skills": [s.to_dict() for s in skills],
        "total": len(skills),
        "query": tag,
    }


@router.get("/cli-native", response_model=CLINativeListResponse)
async def list_cli_native_harnesses() -> CLINativeListResponse:
    """登録済み CLI-Native harness 一覧."""
    service = _get_cli_native_service()
    harnesses = service.list_manifests()
    return CLINativeListResponse(harnesses=harnesses, total=len(harnesses))


@router.get("/cli-native/{harness_id}", response_model=CLINativeDetailResponse)
async def get_cli_native_harness_detail(harness_id: str) -> CLINativeDetailResponse:
    """単一 CLI-Native harness を返す."""
    service = _get_cli_native_service()
    harness = service.get_manifest(harness_id)
    if harness is None:
        raise HTTPException(
            status_code=404,
            detail={
                "message": f"CLI-Native harness not found: {harness_id}",
                "error_code": "CLI_NATIVE_NOT_FOUND",
            },
        )
    return CLINativeDetailResponse(harness=harness)


@router.post("/cli-native/import")
async def import_cli_native_harness(payload: CLINativeImportRequest) -> dict[str, Any]:
    """CLI-Native harness を import する."""
    service = _get_cli_native_service()
    manifest = service.import_harness(
        harness_path=Path(payload.harness_path),
        harness_id=payload.harness_id,
        software_name=payload.software_name,
        force=payload.force,
    )
    return {
        "ok": True,
        "harness": manifest.to_payload(),
    }


@router.post("/cli-native/build", response_model=CLINativeBuildResponse)
async def build_cli_native_harness(payload: CLINativeBuildRequest) -> CLINativeBuildResponse:
    """CLI-Anything build を計画または実行する."""
    service = _get_cli_native_service()
    result = service.plan_build(
        software_name=payload.software_name,
        source_path=Path(payload.source_path),
        runtime_cli=payload.runtime_cli,
        dry_run=payload.dry_run,
    )
    return CLINativeBuildResponse.model_validate(result)


@router.get("/{skill_name}")
async def get_skill_detail(skill_name: str) -> dict[str, Any]:
    """スキル詳細情報.

    Args:
        skill_name: スキル名
    """
    catalog = _get_catalog()
    skill = catalog.get_skill(skill_name)
    if skill is None:
        raise HTTPException(
            status_code=404,
            detail={
                "message": f"Skill not found: {skill_name}",
                "error_code": "SKILL_NOT_FOUND",
            },
        )
    return skill.to_dict()
