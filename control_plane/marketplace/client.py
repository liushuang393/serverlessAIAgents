"""BizCore マーケットプレイスクライアント.

このモジュールはマーケットプレイス API との通信を提供します。
"""

import shutil
import tarfile
import tempfile
import zipfile
from datetime import UTC, datetime
from pathlib import Path
from typing import Any
from urllib.parse import urlparse

import httpx
from pydantic import BaseModel, Field

from control_plane.marketplace.registry import AgentRegistryEntry, LocalRegistry


class MarketplaceAgent(BaseModel):
    """マーケットプレイスエージェント情報."""

    id: str = Field(..., description="エージェント ID")
    name: str = Field(..., description="エージェント名")
    version: str = Field(..., description="バージョン")
    author: str = Field(..., description="作成者")
    category: str = Field(..., description="カテゴリ")
    description: str = Field(..., description="説明")
    protocols: list[str] = Field(default_factory=list, description="サポートプロトコル")
    download_url: str = Field(..., description="ダウンロード URL")
    dependencies: list[str] = Field(default_factory=list, description="依存エージェント")


class MarketplaceClient:
    """マーケットプレイスクライアント.

    エージェントの検索、インストール、アンインストールを提供します。
    """

    def __init__(
        self,
        marketplace_url: str = "https://marketplace.bizcore.dev",
        install_dir: Path | None = None,
        registry: LocalRegistry | None = None,
    ) -> None:
        """クライアントを初期化.

        Args:
            marketplace_url: マーケットプレイス API の URL
            install_dir: エージェントインストールディレクトリ
            registry: ローカルレジストリ
        """
        self.marketplace_url = marketplace_url

        # ディレクトリ作成は install() などの実際の使用時まで遅延する
        # これにより初期化時のパーミッションエラーを防ぐ
        if install_dir is None:
            primary = Path.home() / ".bizcore" / "agents"
            legacy = Path.home() / ".agentflow" / "agents"
            install_dir = primary if primary.exists() or not legacy.exists() else legacy
        self.install_dir = install_dir

        self.registry = registry or LocalRegistry()
        self.client = httpx.Client(timeout=30.0)

    def _ensure_install_dir(self) -> None:
        """インストールディレクトリが存在することを確保する（遅延作成）。"""
        self.install_dir.mkdir(parents=True, exist_ok=True)

    def search(
        self,
        query: str | None = None,
        category: str | None = None,
        protocols: list[str] | None = None,
        limit: int = 10,
    ) -> list[MarketplaceAgent]:
        """エージェントを検索.

        Args:
            query: 検索クエリ
            category: カテゴリフィルター
            protocols: プロトコルフィルター
            limit: 最大結果数

        Returns:
            マーケットプレイスエージェントのリスト
        """
        results = self._request_marketplace_agents(
            query=query,
            category=category,
            protocols=protocols,
            limit=limit,
        )

        if query:
            query_lower = query.lower()
            results = [
                agent
                for agent in results
                if query_lower in agent.id.lower()
                or query_lower in agent.name.lower()
                or query_lower in agent.description.lower()
            ]

        if category:
            results = [agent for agent in results if agent.category == category]

        if protocols:
            results = [agent for agent in results if any(p in agent.protocols for p in protocols)]

        return results[:limit]

    def _request_marketplace_agents(
        self,
        *,
        query: str | None,
        category: str | None,
        protocols: list[str] | None,
        limit: int,
    ) -> list[MarketplaceAgent]:
        """マーケットプレイスAPIへ問い合わせる."""
        params: dict[str, Any] = {"limit": max(1, min(limit, 100))}
        if query:
            params["q"] = query
        if category:
            params["category"] = category
        if protocols:
            params["protocols"] = ",".join(protocols)

        endpoints = [f"{self.marketplace_url.rstrip('/')}/api/v1/agents", f"{self.marketplace_url.rstrip('/')}/agents"]
        for endpoint in endpoints:
            try:
                response = self.client.get(endpoint, params=params)
                response.raise_for_status()
            except Exception:
                continue

            payload = response.json()
            agents = self._parse_agents_payload(payload)
            if agents:
                return agents
        return []

    def _parse_agents_payload(self, payload: Any) -> list[MarketplaceAgent]:
        """APIレスポンスを MarketplaceAgent に変換する."""
        rows: list[dict[str, Any]] = []
        if isinstance(payload, list):
            rows = [item for item in payload if isinstance(item, dict)]
        elif isinstance(payload, dict):
            if isinstance(payload.get("agents"), list):
                rows = [item for item in payload["agents"] if isinstance(item, dict)]
            elif isinstance(payload.get("items"), list):
                rows = [item for item in payload["items"] if isinstance(item, dict)]

        agents: list[MarketplaceAgent] = []
        for row in rows:
            agent_id = str(row.get("id") or row.get("agent_id") or "").strip()
            download_url = str(row.get("download_url") or row.get("artifact_url") or "").strip()
            if not agent_id or not download_url:
                continue
            try:
                agents.append(
                    MarketplaceAgent(
                        id=agent_id,
                        name=str(row.get("name") or agent_id),
                        version=str(row.get("version") or "latest"),
                        author=str(row.get("author") or "unknown"),
                        category=str(row.get("category") or "general"),
                        description=str(row.get("description") or ""),
                        protocols=[str(item) for item in row.get("protocols", []) if str(item).strip()],
                        download_url=download_url,
                        dependencies=[str(item) for item in row.get("dependencies", []) if str(item).strip()],
                    )
                )
            except Exception:
                continue
        return agents

    def install(
        self,
        agent_id: str,
        _version: str | None = None,
        force: bool = False,
    ) -> Path:
        """エージェントをインストール.

        Args:
            agent_id: エージェント ID
            _version: バージョン (None の場合は最新、未使用)
            force: 既存エージェントを上書き

        Returns:
            インストールパス

        Raises:
            ValueError: エージェントが見つからない、または既にインストール済み
        """
        # インストール済みチェック
        if self.registry.is_installed(agent_id) and not force:
            msg = f"Agent already installed: {agent_id}"
            raise ValueError(msg)

        # エージェント情報を取得
        agents = self.search(query=agent_id)
        agent = next((a for a in agents if a.id == agent_id), None)

        if not agent:
            msg = f"Agent not found: {agent_id}"
            raise ValueError(msg)

        # インストールディレクトリを作成
        self._ensure_install_dir()
        install_path = self.install_dir / agent_id
        if install_path.exists() and force:
            shutil.rmtree(install_path)
        install_path.mkdir(parents=True, exist_ok=True)

        with tempfile.TemporaryDirectory() as tmpdir:
            parsed = urlparse(agent.download_url)
            suffixes = "".join(Path(parsed.path).suffixes)
            artifact_name = f"artifact{suffixes}" if suffixes else "artifact.bin"
            artifact_path = Path(tmpdir) / artifact_name
            response = self.client.get(agent.download_url, follow_redirects=True)
            response.raise_for_status()
            artifact_path.write_bytes(response.content)
            self._extract_artifact(artifact_path, install_path)

        if not self._has_manifest(install_path):
            msg = f"Invalid package (agent.yaml missing): {agent.download_url}"
            raise ValueError(msg)

        # レジストリに追加
        entry = AgentRegistryEntry(
            id=agent.id,
            name=agent.name,
            version=agent.version,
            author=agent.author,
            category=agent.category,
            description=agent.description,
            install_path=str(install_path),
            installed_at=datetime.now(UTC).isoformat(),
        )
        self.registry.add_agent(entry)

        return install_path

    def _extract_artifact(self, artifact_path: Path, install_path: Path) -> None:
        """アーティファクトを解凍・配置する."""
        suffixes = "".join(artifact_path.suffixes).lower()
        if suffixes.endswith(".zip"):
            with zipfile.ZipFile(artifact_path, "r") as archive:
                for member in archive.infolist():
                    target = (install_path / member.filename).resolve()
                    if not str(target).startswith(str(install_path.resolve())):
                        msg = f"Unsafe zip member path: {member.filename}"
                        raise ValueError(msg)
                archive.extractall(install_path)
            return

        if suffixes.endswith(".tar.gz") or suffixes.endswith(".tgz") or suffixes.endswith(".tar"):
            with tarfile.open(artifact_path, "r:*") as archive:
                for member in archive.getmembers():
                    target = (install_path / member.name).resolve()
                    if not str(target).startswith(str(install_path.resolve())):
                        msg = f"Unsafe tar member path: {member.name}"
                        raise ValueError(msg)
                archive.extractall(install_path)
            return

        # 単一ファイルをそのまま配置
        output = install_path / "agent.yaml"
        try:
            text = artifact_path.read_text(encoding="utf-8")
        except UnicodeDecodeError:
            text = artifact_path.read_bytes().decode("utf-8", errors="replace")
        output.write_text(text, encoding="utf-8")

    @staticmethod
    def _has_manifest(install_path: Path) -> bool:
        """インストール済みディレクトリに manifest があるか確認."""
        candidates = ["agent.yaml", "agent.yml", "app_config.json"]
        for name in candidates:
            if (install_path / name).is_file():
                return True
        return False

    def uninstall(self, agent_id: str) -> bool:
        """エージェントをアンインストール.

        Args:
            agent_id: エージェント ID

        Returns:
            アンインストールに成功した場合 True
        """
        # レジストリから情報を取得
        entry = self.registry.get_agent(agent_id)
        if not entry:
            return False

        # インストールディレクトリを削除
        install_path = Path(entry.install_path)
        if install_path.exists():
            shutil.rmtree(install_path)

        # レジストリから削除
        return self.registry.remove_agent(agent_id)

    def list_installed(self) -> list[AgentRegistryEntry]:
        """インストール済みエージェントを一覧取得.

        Returns:
            エージェントエントリのリスト
        """
        return self.registry.list_agents()

    def close(self) -> None:
        """クライアントをクローズ."""
        self.client.close()
