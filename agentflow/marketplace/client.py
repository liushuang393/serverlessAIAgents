"""AgentFlow マーケットプレイスクライアント.

このモジュールはマーケットプレイス API との通信を提供します。
"""

import shutil
from datetime import datetime
from pathlib import Path
from typing import Any

import httpx
from pydantic import BaseModel, Field

from agentflow.core.schemas import SchemaLoader
from agentflow.marketplace.registry import AgentRegistryEntry, LocalRegistry


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
        marketplace_url: str = "https://marketplace.agentflow.dev",
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
        
        if install_dir is None:
            install_dir = Path.home() / ".agentflow" / "agents"
        self.install_dir = install_dir
        self.install_dir.mkdir(parents=True, exist_ok=True)
        
        self.registry = registry or LocalRegistry()
        self.client = httpx.Client(timeout=30.0)

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
        # TODO: 実際の API 実装
        # 現在はモックデータを返す
        mock_agents = [
            MarketplaceAgent(
                id="pdf-processor",
                name="PDF Processor",
                version="1.0.0",
                author="AgentFlow Team",
                category="document",
                description="Process PDF documents",
                protocols=["mcp", "a2a"],
                download_url="https://example.com/pdf-processor.zip",
                dependencies=[],
            ),
            MarketplaceAgent(
                id="text-analyzer",
                name="Text Analyzer",
                version="1.2.0",
                author="Community",
                category="text",
                description="Analyze text content",
                protocols=["mcp", "agui"],
                download_url="https://example.com/text-analyzer.zip",
                dependencies=[],
            ),
        ]
        
        # フィルタリング
        results = mock_agents

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
            results = [
                agent
                for agent in results
                if any(p in agent.protocols for p in protocols)
            ]
        
        return results[:limit]

    def install(
        self,
        agent_id: str,
        version: str | None = None,
        force: bool = False,
    ) -> Path:
        """エージェントをインストール.
        
        Args:
            agent_id: エージェント ID
            version: バージョン (None の場合は最新)
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
        install_path = self.install_dir / agent_id
        install_path.mkdir(parents=True, exist_ok=True)
        
        # TODO: 実際のダウンロードとインストール
        # 現在はモックとして agent.yaml を作成
        agent_yaml = install_path / "agent.yaml"
        agent_yaml.write_text(
            f"""meta:
  id: {agent.id}
  name: {agent.name}
  version: {agent.version}
  author: {agent.author}
  icon: 📦
  category: {agent.category}
  description: {agent.description}

interfaces:
  inputs: []
  outputs: []

protocols:
  mcp:
    enabled: {"true" if "mcp" in agent.protocols else "false"}
  a2a:
    enabled: {"true" if "a2a" in agent.protocols else "false"}
  agui:
    enabled: {"true" if "agui" in agent.protocols else "false"}

dependencies:
  agents: {agent.dependencies}
  tools: []
  packages: []

pocketflow:
  entry_point: main.py
  flow_name: MainFlow

visual:
  color: "#3B82F6"
  ports: {{}}
""",
            encoding="utf-8",
        )
        
        # レジストリに追加
        entry = AgentRegistryEntry(
            id=agent.id,
            name=agent.name,
            version=agent.version,
            author=agent.author,
            category=agent.category,
            description=agent.description,
            install_path=str(install_path),
            installed_at=datetime.now().isoformat(),
        )
        self.registry.add_agent(entry)
        
        return install_path

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

