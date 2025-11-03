"""AgentFlow マーケットプレイスレジストリ.

このモジュールはローカルエージェントレジストリの管理を提供します。
"""

from pathlib import Path
from typing import Any

import yaml
from pydantic import BaseModel, Field


class AgentRegistryEntry(BaseModel):
    """エージェントレジストリエントリ.
    
    インストール済みエージェントの情報を保持します。
    """

    id: str = Field(..., description="エージェント ID")
    name: str = Field(..., description="エージェント名")
    version: str = Field(..., description="バージョン")
    author: str = Field(..., description="作成者")
    category: str = Field(..., description="カテゴリ")
    description: str = Field(..., description="説明")
    install_path: str = Field(..., description="インストールパス")
    installed_at: str = Field(..., description="インストール日時")


class LocalRegistry:
    """ローカルエージェントレジストリ.
    
    インストール済みエージェントの情報を管理します。
    """

    def __init__(self, registry_path: Path | None = None) -> None:
        """レジストリを初期化.
        
        Args:
            registry_path: レジストリファイルのパス (デフォルト: ~/.agentflow/registry.yaml)
        """
        if registry_path is None:
            registry_path = Path.home() / ".agentflow" / "registry.yaml"
        
        self.registry_path = registry_path
        self._ensure_registry_exists()

    def _ensure_registry_exists(self) -> None:
        """レジストリファイルが存在することを確認."""
        self.registry_path.parent.mkdir(parents=True, exist_ok=True)
        
        if not self.registry_path.exists():
            self.registry_path.write_text("agents: []\n", encoding="utf-8")

    def _load_registry(self) -> dict[str, Any]:
        """レジストリを読み込み.
        
        Returns:
            レジストリデータ
        """
        with self.registry_path.open("r", encoding="utf-8") as f:
            data = yaml.safe_load(f)
        
        return data or {"agents": []}

    def _save_registry(self, data: dict[str, Any]) -> None:
        """レジストリを保存.
        
        Args:
            data: レジストリデータ
        """
        with self.registry_path.open("w", encoding="utf-8") as f:
            yaml.safe_dump(data, f, allow_unicode=True, sort_keys=False)

    def add_agent(self, entry: AgentRegistryEntry) -> None:
        """エージェントをレジストリに追加.
        
        Args:
            entry: レジストリエントリ
        """
        data = self._load_registry()
        
        # 既存エントリを削除
        data["agents"] = [
            agent for agent in data["agents"] if agent.get("id") != entry.id
        ]
        
        # 新しいエントリを追加
        data["agents"].append(entry.model_dump())
        
        self._save_registry(data)

    def remove_agent(self, agent_id: str) -> bool:
        """エージェントをレジストリから削除.
        
        Args:
            agent_id: エージェント ID
            
        Returns:
            削除に成功した場合 True
        """
        data = self._load_registry()
        original_count = len(data["agents"])
        
        data["agents"] = [
            agent for agent in data["agents"] if agent.get("id") != agent_id
        ]
        
        if len(data["agents"]) < original_count:
            self._save_registry(data)
            return True
        
        return False

    def get_agent(self, agent_id: str) -> AgentRegistryEntry | None:
        """エージェント情報を取得.
        
        Args:
            agent_id: エージェント ID
            
        Returns:
            エージェントエントリ、存在しない場合 None
        """
        data = self._load_registry()
        
        for agent in data["agents"]:
            if agent.get("id") == agent_id:
                return AgentRegistryEntry.model_validate(agent)
        
        return None

    def list_agents(self) -> list[AgentRegistryEntry]:
        """インストール済みエージェントを一覧取得.
        
        Returns:
            エージェントエントリのリスト
        """
        data = self._load_registry()
        
        return [
            AgentRegistryEntry.model_validate(agent) for agent in data["agents"]
        ]

    def is_installed(self, agent_id: str) -> bool:
        """エージェントがインストール済みかを確認.
        
        Args:
            agent_id: エージェント ID
            
        Returns:
            インストール済みの場合 True
        """
        return self.get_agent(agent_id) is not None

