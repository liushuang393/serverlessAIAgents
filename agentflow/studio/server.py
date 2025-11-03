"""AgentFlow Studio Server.

FastAPI サーバーを起動・管理するためのクラスです。
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

import uvicorn
from rich.console import Console

from agentflow.studio.api import create_app


class StudioServer:
    """AgentFlow Studio サーバー.

    FastAPI アプリケーションを起動・管理します。
    """

    def __init__(
        self,
        host: str = "0.0.0.0",
        port: int = 8000,
        agents_dir: Path | None = None,
        workflows_dir: Path | None = None,
        reload: bool = False,
    ) -> None:
        """初期化.

        Args:
            host: ホストアドレス
            port: ポート番号
            agents_dir: エージェントディレクトリ
            workflows_dir: ワークフローディレクトリ
            reload: ホットリロード有効化
        """
        self.host = host
        self.port = port
        self.agents_dir = agents_dir
        self.workflows_dir = workflows_dir
        self.reload = reload
        self.console = Console()

        # FastAPI アプリを作成
        self.app = create_app(
            agents_dir=agents_dir,
            workflows_dir=workflows_dir,
        )

    def run(self) -> None:
        """サーバーを起動 (ブロッキング)."""
        self.console.print("[bold green]🚀 AgentFlow Studio starting...[/bold green]")
        self.console.print(f"[cyan]Host:[/cyan] {self.host}")
        self.console.print(f"[cyan]Port:[/cyan] {self.port}")
        self.console.print(f"[cyan]API Docs:[/cyan] http://{self.host}:{self.port}/api/docs")
        self.console.print(f"[cyan]Agents:[/cyan] {self.agents_dir or '~/.agentflow/agents'}")
        self.console.print(
            f"[cyan]Workflows:[/cyan] {self.workflows_dir or '~/.agentflow/workflows'}"
        )
        self.console.print()

        uvicorn.run(
            self.app,
            host=self.host,
            port=self.port,
            reload=self.reload,
            log_level="info",
        )

    async def start(self) -> None:
        """サーバーを起動 (非同期)."""
        config = uvicorn.Config(
            self.app,
            host=self.host,
            port=self.port,
            reload=self.reload,
            log_level="info",
        )
        server = uvicorn.Server(config)
        await server.serve()

    def get_app(self) -> Any:
        """FastAPI アプリを取得.

        Returns:
            FastAPI アプリケーション
        """
        return self.app


def main() -> None:
    """CLI エントリーポイント."""
    import argparse

    parser = argparse.ArgumentParser(description="AgentFlow Studio Server")
    parser.add_argument(
        "--host",
        default="0.0.0.0",
        help="Host address (default: 0.0.0.0)",
    )
    parser.add_argument(
        "--port",
        type=int,
        default=8000,
        help="Port number (default: 8000)",
    )
    parser.add_argument(
        "--agents-dir",
        type=Path,
        help="Agents directory (default: ~/.agentflow/agents)",
    )
    parser.add_argument(
        "--workflows-dir",
        type=Path,
        help="Workflows directory (default: ~/.agentflow/workflows)",
    )
    parser.add_argument(
        "--reload",
        action="store_true",
        help="Enable hot reload",
    )

    args = parser.parse_args()

    server = StudioServer(
        host=args.host,
        port=args.port,
        agents_dir=args.agents_dir,
        workflows_dir=args.workflows_dir,
        reload=args.reload,
    )
    server.run()


if __name__ == "__main__":
    main()
