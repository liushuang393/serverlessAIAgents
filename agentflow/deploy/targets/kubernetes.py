"""KubernetesTarget - Kubernetes デプロイターゲット.

【機能】
- Deployment/Service/ConfigMap/Secret生成
- kubectl apply による自動デプロイ
- HPA（Horizontal Pod Autoscaler）対応
- Ingress設定生成

使用例:
    >>> from agentflow.deploy.targets import KubernetesTarget
    >>> target = KubernetesTarget()
    >>> async for event in target.deploy(source_path, config):
    ...     print(event)
"""

from __future__ import annotations

import asyncio
import logging
from typing import TYPE_CHECKING, Any

from agentflow.core.interfaces import (
    ConfigField,
    DeployConfig,
    DeployEvent,
    ValidationResult,
)
from agentflow.deploy.targets.base import BaseDeployTarget


if TYPE_CHECKING:
    from collections.abc import AsyncIterator
    from pathlib import Path


logger = logging.getLogger(__name__)


class KubernetesTarget(BaseDeployTarget):
    """Kubernetes デプロイターゲット."""

    @property
    def name(self) -> str:
        return "Kubernetes"

    @property
    def description(self) -> str:
        return "Deploy to Kubernetes cluster"

    async def deploy(
        self,
        source_path: Path,
        config: DeployConfig,
    ) -> AsyncIterator[DeployEvent]:
        """Kubernetesにデプロイ."""
        app_name = config.settings.get("app_name", "agentflow-app")
        namespace = config.settings.get("namespace", "default")
        replicas = config.settings.get("replicas", 2)
        image = config.settings.get("image", "agentflow:latest")
        port = config.settings.get("port", 8000)

        yield DeployEvent(
            type="progress",
            message="Generating Kubernetes manifests...",
            progress=10,
            phase="init",
        )

        try:
            # マニフェスト生成
            manifests = self._generate_manifests(
                app_name=app_name,
                namespace=namespace,
                replicas=replicas,
                image=image,
                port=port,
                config=config,
            )

            # マニフェストファイル出力
            manifest_dir = source_path / "k8s"
            manifest_dir.mkdir(parents=True, exist_ok=True)

            for filename, content in manifests.items():
                manifest_path = manifest_dir / filename
                manifest_path.write_text(content, encoding="utf-8")

            yield DeployEvent(
                type="progress",
                message=f"Generated {len(manifests)} manifest files",
                progress=40,
                phase="manifest_generated",
            )

            # kubectl apply
            if config.settings.get("auto_apply", False):
                yield DeployEvent(
                    type="progress",
                    message=f"Applying manifests to namespace: {namespace}",
                    progress=60,
                    phase="applying",
                )

                apply_process = await asyncio.create_subprocess_exec(
                    "kubectl",
                    "apply",
                    "-f",
                    str(manifest_dir),
                    "-n",
                    namespace,
                    stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.PIPE,
                )

                _stdout, stderr = await apply_process.communicate()

                if apply_process.returncode != 0:
                    yield DeployEvent(
                        type="error",
                        message=f"kubectl apply failed: {stderr.decode()}",
                    )
                    return

                yield DeployEvent(
                    type="progress",
                    message="Waiting for deployment to be ready...",
                    progress=80,
                    phase="waiting",
                )

                # デプロイメント完了待機
                wait_process = await asyncio.create_subprocess_exec(
                    "kubectl",
                    "rollout",
                    "status",
                    f"deployment/{app_name}",
                    "-n",
                    namespace,
                    "--timeout=300s",
                    stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.PIPE,
                )

                await wait_process.communicate()

            yield DeployEvent(
                type="success",
                message=f"Deployed to Kubernetes: {namespace}/{app_name}",
                progress=100,
                phase="complete",
                data={
                    "namespace": namespace,
                    "app_name": app_name,
                    "replicas": replicas,
                    "manifests": list(manifests.keys()),
                },
            )

        except FileNotFoundError:
            yield DeployEvent(type="error", message="kubectl not found")
        except Exception as e:
            logger.exception("Kubernetes deployment failed")
            yield DeployEvent(type="error", message=f"Deployment failed: {e}")

    def _generate_manifests(
        self,
        app_name: str,
        namespace: str,
        replicas: int,
        image: str,
        port: int,
        config: DeployConfig,
    ) -> dict[str, str]:
        """Kubernetesマニフェストを生成."""
        manifests: dict[str, str] = {}

        # Deployment
        manifests["deployment.yaml"] = f"""apiVersion: apps/v1
kind: Deployment
metadata:
  name: {app_name}
  namespace: {namespace}
  labels:
    app: {app_name}
spec:
  replicas: {replicas}
  selector:
    matchLabels:
      app: {app_name}
  template:
    metadata:
      labels:
        app: {app_name}
    spec:
      containers:
      - name: {app_name}
        image: {image}
        ports:
        - containerPort: {port}
        env:
        - name: PORT
          value: "{port}"
        resources:
          requests:
            memory: "256Mi"
            cpu: "100m"
          limits:
            memory: "512Mi"
            cpu: "500m"
        livenessProbe:
          httpGet:
            path: /health
            port: {port}
          initialDelaySeconds: 10
          periodSeconds: 30
        readinessProbe:
          httpGet:
            path: /health
            port: {port}
          initialDelaySeconds: 5
          periodSeconds: 10
"""

        # Service
        manifests["service.yaml"] = f"""apiVersion: v1
kind: Service
metadata:
  name: {app_name}
  namespace: {namespace}
spec:
  selector:
    app: {app_name}
  ports:
  - port: 80
    targetPort: {port}
  type: ClusterIP
"""

        # HPA（オプション）
        if config.settings.get("enable_hpa", False):
            min_replicas = config.settings.get("hpa_min_replicas", 2)
            max_replicas = config.settings.get("hpa_max_replicas", 10)
            manifests["hpa.yaml"] = f"""apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: {app_name}-hpa
  namespace: {namespace}
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: {app_name}
  minReplicas: {min_replicas}
  maxReplicas: {max_replicas}
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
"""

        # Ingress（オプション）
        if config.settings.get("enable_ingress", False):
            host = config.settings.get("ingress_host", f"{app_name}.example.com")
            manifests["ingress.yaml"] = f"""apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: {app_name}-ingress
  namespace: {namespace}
  annotations:
    kubernetes.io/ingress.class: nginx
spec:
  rules:
  - host: {host}
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: {app_name}
            port:
              number: 80
"""

        return manifests

    def get_config_fields(self) -> list[ConfigField]:
        """Kubernetes用の設定フィールドを取得."""
        return [
            ConfigField(
                name="app_name",
                label="App Name",
                type="string",
                required=True,
                description="Application name",
                group="settings",
            ),
            ConfigField(
                name="namespace",
                label="Namespace",
                type="string",
                required=False,
                default="default",
                description="Kubernetes namespace",
                group="settings",
            ),
            ConfigField(
                name="image",
                label="Docker Image",
                type="string",
                required=True,
                description="Docker image to deploy",
                group="settings",
            ),
            ConfigField(
                name="replicas",
                label="Replicas",
                type="number",
                required=False,
                default=2,
                description="Number of replicas",
                group="settings",
            ),
            ConfigField(
                name="port",
                label="Port",
                type="number",
                required=False,
                default=8000,
                description="Container port",
                group="settings",
            ),
            ConfigField(
                name="auto_apply",
                label="Auto Apply",
                type="boolean",
                required=False,
                default=False,
                description="Apply manifests automatically",
                group="settings",
            ),
            ConfigField(
                name="enable_hpa",
                label="Enable HPA",
                type="boolean",
                required=False,
                default=False,
                description="Enable Horizontal Pod Autoscaler",
                group="scaling",
            ),
            ConfigField(
                name="enable_ingress",
                label="Enable Ingress",
                type="boolean",
                required=False,
                default=False,
                description="Enable Ingress",
                group="networking",
            ),
        ]

    def validate_config(self, config: dict[str, Any]) -> ValidationResult:
        """設定を検証."""
        errors: dict[str, str] = {}
        warnings: list[str] = []

        if not config.get("app_name"):
            errors["app_name"] = "App name is required"
        if not config.get("image"):
            errors["image"] = "Docker image is required"

        replicas = config.get("replicas", 2)
        if replicas < 1:
            errors["replicas"] = "Replicas must be at least 1"
        elif replicas < 2:
            warnings.append("Consider using at least 2 replicas for high availability")

        return ValidationResult(valid=len(errors) == 0, errors=errors, warnings=warnings)


__all__ = ["KubernetesTarget"]
