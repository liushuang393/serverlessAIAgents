# -*- coding: utf-8 -*-
"""ポート管理ツール.

Docker コンテナと agentflow/apps のポート衝突を自動検出し、
空きポートを割り当てる。

機能:
    - システム上の使用中ポートを検出
    - Docker コンテナの使用ポートを検出
    - apps/ 配下の全 App の設定ポートを検出
    - 空きポートを自動割り当て
    - .env ファイルを自動生成/更新
"""
from __future__ import annotations

import os
import re
import socket
import subprocess
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

import yaml


@dataclass
class PortAllocation:
    """ポート割り当て情報."""
    
    db_main_port: int = 5432        # PostgreSQL メイン
    db_history_port: int = 5433     # PostgreSQL 履歴
    redis_port: int = 6379          # Redis
    api_port: int = 8000            # FastAPI
    frontend_port: int = 3000       # React


@dataclass
class PortManager:
    """ポート管理クラス.
    
    自動的に空きポートを検出し、衝突を回避する。
    """
    
    workspace_root: Path = field(default_factory=lambda: Path.cwd())
    
    # ポート範囲設定
    DB_PORT_START: int = 5432
    DB_PORT_END: int = 5500
    API_PORT_START: int = 8000
    API_PORT_END: int = 8100
    REDIS_PORT_START: int = 6379
    REDIS_PORT_END: int = 6400
    FRONTEND_PORT_START: int = 3000
    FRONTEND_PORT_END: int = 3100
    
    def get_system_used_ports(self, ports_to_check: set[int] | None = None) -> set[int]:
        """システムで使用中のポートを取得.

        Args:
            ports_to_check: チェック対象ポート（省略時は主要ポートのみ）

        Returns:
            使用中ポートのセット
        """
        used_ports: set[int] = set()

        # チェック対象を絞る（全範囲スキャンは遅い）
        if ports_to_check is None:
            ports_to_check = set()
            # DB ポート範囲
            ports_to_check.update(range(self.DB_PORT_START, self.DB_PORT_START + 20))
            # API ポート範囲
            ports_to_check.update(range(self.API_PORT_START, self.API_PORT_START + 20))
            # Redis/Frontend
            ports_to_check.update(range(self.REDIS_PORT_START, self.REDIS_PORT_START + 10))
            ports_to_check.update(range(self.FRONTEND_PORT_START, self.FRONTEND_PORT_START + 10))

        for port in ports_to_check:
            if self._is_port_in_use(port):
                used_ports.add(port)

        return used_ports
    
    def _is_port_in_use(self, port: int) -> bool:
        """ポートが使用中か確認."""
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            try:
                s.bind(("127.0.0.1", port))
                return False
            except OSError:
                return True
    
    def get_docker_used_ports(self) -> set[int]:
        """Docker コンテナで使用中のポートを取得."""
        used_ports: set[int] = set()
        
        try:
            # docker ps でコンテナ一覧取得
            result = subprocess.run(
                ["docker", "ps", "--format", "{{.Ports}}"],
                capture_output=True,
                text=True,
                timeout=10,
            )
            if result.returncode == 0:
                # ポートをパース: 0.0.0.0:5432->5432/tcp
                for line in result.stdout.strip().split("\n"):
                    matches = re.findall(r":(\d+)->", line)
                    for m in matches:
                        used_ports.add(int(m))
        except (subprocess.TimeoutExpired, FileNotFoundError):
            pass  # Docker がない場合は無視
        
        return used_ports
    
    def get_apps_used_ports(self) -> dict[str, PortAllocation]:
        """apps/ 配下の全 App の設定ポートを取得."""
        apps_ports: dict[str, PortAllocation] = {}
        apps_dir = self.workspace_root / "apps"
        
        if not apps_dir.exists():
            return apps_ports
        
        for app_dir in apps_dir.iterdir():
            if not app_dir.is_dir():
                continue
            
            # .env ファイルからポート取得
            env_file = app_dir / ".env"
            if env_file.exists():
                ports = self._parse_env_ports(env_file)
                if ports:
                    apps_ports[app_dir.name] = ports
                    continue
            
            # docker-compose.yml からポート取得
            compose_file = app_dir / "docker-compose.yml"
            if compose_file.exists():
                ports = self._parse_compose_ports(compose_file)
                if ports:
                    apps_ports[app_dir.name] = ports
        
        return apps_ports
    
    def _parse_env_ports(self, env_file: Path) -> PortAllocation | None:
        """env ファイルからポートを解析."""
        allocation = PortAllocation()
        content = env_file.read_text(encoding="utf-8")
        
        # DB_MAIN_PORT=5432
        if m := re.search(r"DB_MAIN_PORT=(\d+)", content):
            allocation.db_main_port = int(m.group(1))
        if m := re.search(r"DB_HISTORY_PORT=(\d+)", content):
            allocation.db_history_port = int(m.group(1))
        if m := re.search(r"REDIS_PORT=(\d+)", content):
            allocation.redis_port = int(m.group(1))
        if m := re.search(r"API_PORT=(\d+)", content):
            allocation.api_port = int(m.group(1))
        if m := re.search(r"FRONTEND_PORT=(\d+)", content):
            allocation.frontend_port = int(m.group(1))
        
        return allocation
    
    def _parse_compose_ports(self, compose_file: Path) -> PortAllocation | None:
        """docker-compose.yml からポートを解析."""
        try:
            content = yaml.safe_load(compose_file.read_text(encoding="utf-8"))
            allocation = PortAllocation()
            
            services = content.get("services", {})
            for name, config in services.items():
                ports = config.get("ports", [])
                for p in ports:
                    if isinstance(p, str) and ":" in p:
                        host_port = int(p.split(":")[0].strip('"'))
                        if "postgres" in name.lower() and "history" in name.lower():
                            allocation.db_history_port = host_port
                        elif "postgres" in name.lower():
                            allocation.db_main_port = host_port
                        elif "redis" in name.lower():
                            allocation.redis_port = host_port
                        elif "api" in name.lower():
                            allocation.api_port = host_port
            
            return allocation
        except Exception:
            return None

    def find_available_port(
        self,
        start: int,
        end: int,
        used_ports: set[int],
    ) -> int:
        """空きポートを検索.

        Args:
            start: 開始ポート
            end: 終了ポート
            used_ports: 使用中ポート

        Returns:
            空きポート番号

        Raises:
            RuntimeError: 空きポートがない場合
        """
        for port in range(start, end):
            if port not in used_ports and not self._is_port_in_use(port):
                return port
        raise RuntimeError(f"No available port in range {start}-{end}")

    def allocate_ports_for_app(self, app_name: str) -> PortAllocation:
        """App 用のポートを自動割り当て.

        Args:
            app_name: アプリ名

        Returns:
            割り当てられたポート情報
        """
        # 全ての使用中ポートを収集
        used_ports: set[int] = set()
        used_ports.update(self.get_system_used_ports())
        used_ports.update(self.get_docker_used_ports())

        # 他の App のポートも収集
        for name, allocation in self.get_apps_used_ports().items():
            if name != app_name:
                used_ports.add(allocation.db_main_port)
                used_ports.add(allocation.db_history_port)
                used_ports.add(allocation.redis_port)
                used_ports.add(allocation.api_port)
                used_ports.add(allocation.frontend_port)

        # 空きポートを割り当て（順番に追加して衝突回避）
        db_main = self.find_available_port(
            self.DB_PORT_START, self.DB_PORT_END, used_ports
        )
        used_ports.add(db_main)

        db_history = self.find_available_port(
            self.DB_PORT_START, self.DB_PORT_END, used_ports
        )
        used_ports.add(db_history)

        redis = self.find_available_port(
            self.REDIS_PORT_START, self.REDIS_PORT_END, used_ports
        )
        used_ports.add(redis)

        api = self.find_available_port(
            self.API_PORT_START, self.API_PORT_END, used_ports
        )
        used_ports.add(api)

        frontend = self.find_available_port(
            self.FRONTEND_PORT_START, self.FRONTEND_PORT_END, used_ports
        )

        return PortAllocation(
            db_main_port=db_main,
            db_history_port=db_history,
            redis_port=redis,
            api_port=api,
            frontend_port=frontend,
        )

    def generate_env_file(
        self,
        app_dir: Path,
        app_name: str,
        allocation: PortAllocation,
        *,
        host: str = "localhost",
        db_user: str = "app_user",
        db_password: str = "app_password",
        db_name: str | None = None,
    ) -> Path:
        """App 用 .env ファイルを生成.

        Args:
            app_dir: App ディレクトリ
            app_name: アプリ名
            allocation: ポート割り当て
            host: ホスト名
            db_user: DB ユーザー名
            db_password: DB パスワード
            db_name: DB 名（デフォルトは app_name）

        Returns:
            生成された .env ファイルのパス
        """
        if db_name is None:
            db_name = app_name.replace("-", "_")

        env_content = f'''# -*- coding: utf-8 -*-
# {app_name} 環境設定ファイル
# 自動生成: ポート管理ツール
#
# 注意: このファイルは Git にコミットしないでください
# 本番環境では環境変数で上書きしてください

# ===========================================
# 環境識別
# ===========================================
APP_ENV=development
# APP_ENV=staging
# APP_ENV=production

# ===========================================
# ホスト設定
# ===========================================
# localhost / 127.0.0.1 / 実際の IP
HOST={host}
# Docker 内から見たホスト（docker-compose で使用）
DOCKER_HOST=host.docker.internal

# ===========================================
# データベース設定
# ===========================================
DB_MAIN_PORT={allocation.db_main_port}
DB_HISTORY_PORT={allocation.db_history_port}
DB_USER={db_user}
DB_PASSWORD={db_password}
DB_NAME={db_name}

# 接続 URL（アプリ用 - asyncpg）
DATABASE_URL=postgresql+asyncpg://{db_user}:{db_password}@${{HOST}}:{allocation.db_main_port}/{db_name}
# 接続 URL（Alembic 用 - psycopg2）
DATABASE_URL_SYNC=postgresql+psycopg2://{db_user}:{db_password}@${{HOST}}:{allocation.db_main_port}/{db_name}
# 履歴 DB
DATABASE_HISTORY_URL=postgresql+asyncpg://{db_user}:{db_password}@${{HOST}}:{allocation.db_history_port}/{db_name}_history

# ===========================================
# Redis 設定
# ===========================================
REDIS_PORT={allocation.redis_port}
REDIS_URL=redis://${{HOST}}:{allocation.redis_port}/0

# ===========================================
# API 設定
# ===========================================
API_PORT={allocation.api_port}
API_HOST=0.0.0.0

# ===========================================
# フロントエンド設定
# ===========================================
FRONTEND_PORT={allocation.frontend_port}
# フロントエンドから見た API URL
VITE_API_URL=http://${{HOST}}:{allocation.api_port}
NEXT_PUBLIC_API_URL=http://${{HOST}}:{allocation.api_port}

# ===========================================
# 外部サービス（本番環境で設定）
# ===========================================
# OPENAI_API_KEY=sk-xxx
# ANTHROPIC_API_KEY=sk-ant-xxx
'''

        env_file = app_dir / ".env"
        env_file.write_text(env_content, encoding="utf-8")

        # .env.example も生成（Git 用）
        example_content = env_content.replace(db_password, "your_password_here")
        example_file = app_dir / ".env.example"
        example_file.write_text(example_content, encoding="utf-8")

        return env_file


def setup_app_ports(app_name: str, app_dir: Path | str | None = None) -> PortAllocation:
    """App のポートをセットアップ（CLI 用エントリポイント）.

    Args:
        app_name: アプリ名
        app_dir: App ディレクトリ（デフォルトは apps/{app_name}）

    Returns:
        割り当てられたポート情報
    """
    manager = PortManager()

    if app_dir is None:
        app_dir = manager.workspace_root / "apps" / app_name
    else:
        app_dir = Path(app_dir)

    # ポート割り当て
    allocation = manager.allocate_ports_for_app(app_name)

    # .env 生成
    manager.generate_env_file(app_dir, app_name, allocation)

    print(f"✓ ポート割り当て完了: {app_name}")
    print(f"  DB Main:    {allocation.db_main_port}")
    print(f"  DB History: {allocation.db_history_port}")
    print(f"  Redis:      {allocation.redis_port}")
    print(f"  API:        {allocation.api_port}")
    print(f"  Frontend:   {allocation.frontend_port}")
    print(f"  .env:       {app_dir / '.env'}")

    return allocation


if __name__ == "__main__":
    import sys
    if len(sys.argv) < 2:
        print("Usage: python port_manager.py <app_name> [app_dir]")
        sys.exit(1)

    app_name = sys.argv[1]
    app_dir = sys.argv[2] if len(sys.argv) > 2 else None
    setup_app_ports(app_name, app_dir)

