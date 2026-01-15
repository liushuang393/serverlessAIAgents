# -*- coding: utf-8 -*-
"""パブリッシュAPI ルート.

ワークフローのコード生成とデプロイ機能。
"""

from __future__ import annotations

import json
from typing import Any

from fastapi import APIRouter, HTTPException
from fastapi.responses import Response, StreamingResponse

from agentflow.studio.models import (
    PublishDeployRequest,
    PublishDeployResponse,
    PublishExportRequest,
)


def create_publish_router() -> APIRouter:
    """パブリッシュAPIルーターを作成.

    Returns:
        FastAPI APIRouter
    """
    router = APIRouter(prefix="/api/publish", tags=["publish"])

    @router.post("/export")
    async def publish_export(request: PublishExportRequest) -> Response:
        """ワークフローをコードにエクスポート.

        ZIP ファイルとしてダウンロードできます。
        """
        from agentflow.core.interfaces import CodeGenOptions, CodeOutputType
        from agentflow.services import PublishService

        try:
            service = PublishService()

            type_map = {
                "fastapi": CodeOutputType.BACKEND,
                "cli": CodeOutputType.BACKEND,
                "vercel": CodeOutputType.BACKEND,
                "lambda": CodeOutputType.BACKEND,
                "docker": CodeOutputType.BACKEND,
                "frontend": CodeOutputType.FRONTEND,
                "fullstack": CodeOutputType.FULLSTACK,
            }
            output_type = type_map.get(request.target, CodeOutputType.BACKEND)

            options = CodeGenOptions(
                app_name=request.app_name or "",
                version=request.version,
                include_tests=request.include_tests,
                include_readme=request.include_readme,
            )

            zip_buffer = await service.export_zip(
                request.workflow,
                output_type,
                options,
            )

            workflow_name = request.workflow.get("name", "workflow")
            safe_name = workflow_name.lower().replace(" ", "-")
            filename = f"{safe_name}-{request.target}.zip"

            return Response(
                content=zip_buffer.getvalue(),
                media_type="application/zip",
                headers={"Content-Disposition": f"attachment; filename={filename}"},
            )

        except Exception as e:
            raise HTTPException(status_code=500, detail=str(e))

    @router.post("/preview")
    async def publish_preview(request: PublishExportRequest) -> dict[str, Any]:
        """生成されるコードをプレビュー."""
        from agentflow.core.interfaces import CodeOutputType
        from agentflow.services import PublishService

        try:
            service = PublishService()

            type_map = {
                "fastapi": CodeOutputType.BACKEND,
                "cli": CodeOutputType.BACKEND,
                "vercel": CodeOutputType.BACKEND,
                "lambda": CodeOutputType.BACKEND,
                "docker": CodeOutputType.BACKEND,
                "frontend": CodeOutputType.FRONTEND,
                "fullstack": CodeOutputType.FULLSTACK,
            }
            output_type = type_map.get(request.target, CodeOutputType.BACKEND)

            previews = await service.preview_code(request.workflow, output_type)

            return {
                "status": "success",
                "files": {
                    path: {
                        "content": preview.content_preview,
                        "lines": preview.lines,
                        "size": preview.size,
                    }
                    for path, preview in previews.items()
                },
            }

        except Exception as e:
            raise HTTPException(status_code=500, detail=str(e))

    @router.post("/deploy")
    async def publish_deploy(request: PublishDeployRequest) -> PublishDeployResponse:
        """ワークフローをデプロイ."""
        from agentflow.core.interfaces import CodeOutputType, DeployTarget
        from agentflow.services import PublishService

        logs: list[str] = []

        try:
            service = PublishService()

            target_map = {
                "vercel": DeployTarget.VERCEL,
                "docker": DeployTarget.DOCKER,
                "docker_hub": DeployTarget.DOCKER,
                "aws_lambda": DeployTarget.AWS_LAMBDA,
                "lambda": DeployTarget.AWS_LAMBDA,
                "github_actions": DeployTarget.GITHUB_ACTIONS,
            }
            deploy_target = target_map.get(request.target, DeployTarget.VERCEL)

            config = dict(request.credentials)
            config["project_name"] = request.app_name or request.workflow.get(
                "name",
                "workflow",
            )

            async for event in service.publish(
                workflow=request.workflow,
                output_type=CodeOutputType.BACKEND,
                target=deploy_target,
                config=config,
            ):
                logs.append(event.message)

                if event.type == "success" and event.data:
                    return PublishDeployResponse(
                        status="success",
                        deployment_id=event.data.get("deployment_id"),
                        url=event.data.get("url"),
                        logs=logs,
                    )
                elif event.type == "error":
                    return PublishDeployResponse(
                        status="error",
                        logs=logs,
                        error=event.message,
                    )

            return PublishDeployResponse(status="success", logs=logs)

        except Exception as e:
            logs.append(f"❌ エラー: {str(e)}")
            return PublishDeployResponse(status="error", logs=logs, error=str(e))

    @router.post("/deploy/stream")
    async def publish_deploy_stream(
        request: PublishDeployRequest,
    ) -> StreamingResponse:
        """ワークフローをストリームデプロイ."""
        from agentflow.core.interfaces import CodeOutputType, DeployTarget
        from agentflow.services import PublishService

        async def event_generator():
            try:
                service = PublishService()

                target_map = {
                    "vercel": DeployTarget.VERCEL,
                    "docker": DeployTarget.DOCKER,
                    "docker_hub": DeployTarget.DOCKER,
                    "aws_lambda": DeployTarget.AWS_LAMBDA,
                    "lambda": DeployTarget.AWS_LAMBDA,
                    "github_actions": DeployTarget.GITHUB_ACTIONS,
                }
                deploy_target = target_map.get(request.target, DeployTarget.VERCEL)

                config = dict(request.credentials)
                config["project_name"] = request.app_name or request.workflow.get(
                    "name",
                    "workflow",
                )

                async for event in service.publish(
                    workflow=request.workflow,
                    output_type=CodeOutputType.BACKEND,
                    target=deploy_target,
                    config=config,
                ):
                    yield f"data: {json.dumps(event.to_dict())}\n\n"

            except Exception as e:
                yield f"data: {json.dumps({'type': 'error', 'message': str(e)})}\n\n"

        return StreamingResponse(event_generator(), media_type="text/event-stream")

    @router.get("/targets")
    async def list_publish_targets() -> dict[str, Any]:
        """利用可能なデプロイターゲット一覧."""
        from agentflow.services import PublishService

        service = PublishService()
        output_types = service.get_supported_output_types()
        deploy_targets = service.get_supported_targets()

        return {
            "output_types": output_types,
            "deploy_targets": deploy_targets,
        }

    @router.get("/config-fields/{target}")
    async def get_config_fields(target: str) -> list[dict[str, Any]]:
        """ターゲットに必要な設定フィールドを取得."""
        from agentflow.core.interfaces import DeployTarget
        from agentflow.services import PublishService

        service = PublishService()

        target_map = {
            "vercel": DeployTarget.VERCEL,
            "docker": DeployTarget.DOCKER,
            "aws_lambda": DeployTarget.AWS_LAMBDA,
            "github_actions": DeployTarget.GITHUB_ACTIONS,
        }
        deploy_target = target_map.get(target)

        if deploy_target is None:
            raise HTTPException(status_code=400, detail=f"Unknown target: {target}")

        fields = await service.get_config_fields(deploy_target)
        return [f.to_dict() for f in fields]

    return router

