"""Skills Router — Skill カタログ API エンドポイント.

GET  /api/studios/framework/skills              — 全スキル一覧
GET  /api/studios/framework/skills/stats        — スキル統計
GET  /api/studios/framework/skills/tags         — 全タグ一覧
GET  /api/studios/framework/skills/search       — タグベース検索
GET  /api/studios/framework/skills/{skill_name} — スキル詳細
"""

from __future__ import annotations

from typing import Any

from apps.platform.services.skill_catalog import SkillCatalogService
from fastapi import APIRouter, HTTPException, Query


router = APIRouter(prefix="/api/studios/framework/skills", tags=["skills"])

# モジュールレベルのシングルトン（main.py で初期化）
_catalog: SkillCatalogService | None = None


def init_skill_services(catalog: SkillCatalogService) -> None:
    """サービスインスタンスを設定.

    Args:
        catalog: Skill カタログサービス
    """
    global _catalog
    _catalog = catalog


def _get_catalog() -> SkillCatalogService:
    """SkillCatalogService を取得（未初期化時はエラー）."""
    if _catalog is None:
        msg = "SkillCatalogService が未初期化です"
        raise RuntimeError(msg)
    return _catalog


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
