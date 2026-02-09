# -*- coding: utf-8 -*-
"""PublishAgent - 発布支援Agent.

発布プロセスの支援と自動化を行う。

使用例:
    >>> agent = PublishAgent()
    >>> result = await agent.run({"source": "./my-agent", "target": "docker"})
"""

from __future__ import annotations

from typing import Any

from agentflow.core.agent_block import AgentBlock
from apps.platform.services.publish_orchestrator import PublishOrchestrator
from apps.platform.schemas.publish_schemas import PublishRequest, PublishTarget, PublishStatus


class PublishAgent(AgentBlock):
    """発布支援Agent.

    コンポーネントの発布プロセスを支援し、最適な設定を提案する。
    """

    name = "publish-agent"

    def __init__(self, llm_client: Any = None) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント
        """
        super().__init__()
        self._llm_client = llm_client
        self._orchestrator = PublishOrchestrator()

    def _get_system_prompt(self) -> str:
        """システムプロンプトを取得."""
        return """あなたはAgentFlow発布アシスタントです。

ユーザーのコンポーネント発布を支援します。

タスク:
1. 発布対象の分析
2. 最適な発布ターゲットの提案
3. 必要な設定の確認
4. 発布プロセスの実行と監視
5. 問題発生時のトラブルシューティング

利用可能な発布ターゲット:
- docker: Dockerコンテナとしてビルド・プッシュ
- vercel: Vercel にデプロイ
- aws_lambda: AWS Lambda にデプロイ
- github_actions: GitHub Actions ワークフローを生成
- local: ローカル環境にセットアップ
- gallery: Gallery に登録のみ

発布フロー:
1. Validate - コンポーネントの検証
2. CodeGen - 必要に応じてコード生成
3. Deploy - デプロイ実行
4. Register - Gallery に登録（オプション）

回答は日本語で、簡潔かつ分かりやすく行ってください。"""

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """Agent を実行.

        Args:
            input_data: 入力データ

        Returns:
            発布結果
        """
        action = input_data.get("action", "publish")

        if action == "recommend":
            return await self._recommend_target(input_data)
        elif action == "publish":
            return await self._execute_publish(input_data)
        else:
            return {"success": False, "error": f"Unknown action: {action}"}

    async def _recommend_target(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """発布ターゲットを推薦.

        Args:
            input_data: 入力データ

        Returns:
            推薦結果
        """
        # コンポーネント情報から最適なターゲットを推薦
        component_type = input_data.get("component_type", "agent")
        requirements = input_data.get("requirements", [])

        recommendations = []

        # 簡易的な推薦ロジック
        if "serverless" in requirements:
            recommendations.append({
                "target": "vercel",
                "reason": "サーバーレス要件に最適",
                "priority": 1,
            })
            recommendations.append({
                "target": "aws_lambda",
                "reason": "AWS環境でのサーバーレス実行",
                "priority": 2,
            })

        if "container" in requirements or "docker" in requirements:
            recommendations.append({
                "target": "docker",
                "reason": "コンテナ化による移植性",
                "priority": 1,
            })

        if not recommendations:
            # デフォルト推薦
            recommendations = [
                {"target": "docker", "reason": "汎用的で移植性が高い", "priority": 1},
                {"target": "vercel", "reason": "簡単なデプロイ", "priority": 2},
            ]

        return {
            "success": True,
            "recommendations": recommendations,
            "available_targets": [t.value for t in PublishTarget],
        }

    async def _execute_publish(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """発布を実行.

        Args:
            input_data: 入力データ

        Returns:
            発布結果
        """
        source = input_data.get("source")
        target = input_data.get("target", "docker")

        if not source:
            return {"success": False, "error": "ソースが指定されていません"}

        try:
            publish_target = PublishTarget(target)
        except ValueError:
            return {"success": False, "error": f"Unknown target: {target}"}

        request = PublishRequest(
            source_path=source,
            target=publish_target,
            name=input_data.get("name"),
            description=input_data.get("description"),
            publish_to_gallery=input_data.get("gallery", False),
        )

        # 発布を実行
        events = []
        final_status = PublishStatus.PENDING

        async for event in self._orchestrator.publish(request):
            events.append({
                "phase": event.phase,
                "status": event.status.value,
                "message": event.message,
                "progress": event.progress,
            })
            final_status = event.status

        return {
            "success": final_status == PublishStatus.COMPLETED,
            "status": final_status.value,
            "events": events,
        }


__all__ = ["PublishAgent"]
