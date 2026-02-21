"""Port Allocator Service.

App 間のポート重複検出と、新規 App 用の安全な自動割り当てを提供する。
"""

from __future__ import annotations

import socket
from typing import TYPE_CHECKING, Literal

from apps.platform.schemas.provisioning_schemas import PortConflictItem, PortConflictReport


if TYPE_CHECKING:
    from apps.platform.services.app_discovery import AppDiscoveryService


PortType = Literal["api", "frontend", "db", "redis"]


class PortAllocatorService:
    """ポート割り当てサービス."""

    _PORT_TYPES: tuple[PortType, ...] = ("api", "frontend", "db", "redis")
    _START_PORTS: dict[PortType, int] = {
        "api": 8100,
        "frontend": 3100,
        "db": 5500,
        "redis": 6400,
    }

    def __init__(self, discovery: AppDiscoveryService) -> None:
        """初期化.

        Args:
            discovery: App 検出サービス
        """
        self._discovery = discovery

    def build_conflict_report(self) -> PortConflictReport:
        """現在の app_config ベースでポート重複を検出."""
        index = self._collect_port_index()
        conflicts: list[PortConflictItem] = []

        for port_type in self._PORT_TYPES:
            for port, apps in sorted(index[port_type].items()):
                if len(apps) > 1:
                    conflicts.append(
                        PortConflictItem(
                            port_type=port_type,
                            port=port,
                            apps=sorted(apps),
                        )
                    )

        return PortConflictReport(
            has_conflicts=bool(conflicts),
            conflicts=conflicts,
        )

    def allocate_for_new_app(
        self,
        *,
        frontend_enabled: bool,
        database: Literal["none", "sqlite", "postgresql"],
        redis_enabled: bool,
    ) -> dict[str, int | None]:
        """新規 App 用に未使用ポートを自動割り当て.

        Args:
            frontend_enabled: Frontend ポートが必要か
            database: DB 種別
            redis_enabled: Redis ポートが必要か

        Returns:
            ports 辞書
        """
        taken = self._collect_taken_ports()

        api_port = self._find_available(
            start=self._START_PORTS["api"],
            taken=taken,
        )
        taken.add(api_port)

        frontend_port: int | None = None
        if frontend_enabled:
            frontend_port = self._find_available(
                start=self._START_PORTS["frontend"],
                taken=taken,
            )
            taken.add(frontend_port)

        db_port: int | None = None
        if database == "postgresql":
            db_port = self._find_available(
                start=self._START_PORTS["db"],
                taken=taken,
            )
            taken.add(db_port)

        redis_port: int | None = None
        if redis_enabled:
            redis_port = self._find_available(
                start=self._START_PORTS["redis"],
                taken=taken,
            )

        return {
            "api": api_port,
            "frontend": frontend_port,
            "db": db_port,
            "redis": redis_port,
        }

    def plan_conflict_resolution(self) -> dict[str, dict[str, int]]:
        """重複ポートを解消するための再割当プランを作成.

        Returns:
            app_name -> {"api"|"frontend"|"db"|"redis": new_port}
        """
        index = self._collect_port_index()
        taken = self._collect_taken_ports()
        updates: dict[str, dict[str, int]] = {}

        for port_type in self._PORT_TYPES:
            for _port, apps in sorted(index[port_type].items()):
                if len(apps) <= 1:
                    continue

                # 最初の 1 件は維持し、2件目以降を再割当
                keep_app = sorted(apps)[0]
                for app_name in sorted(apps):
                    if app_name == keep_app:
                        continue
                    new_port = self._find_available(
                        start=self._START_PORTS[port_type],
                        taken=taken,
                    )
                    taken.add(new_port)
                    updates.setdefault(app_name, {})[port_type] = new_port

        return updates

    def _collect_port_index(self) -> dict[PortType, dict[int, list[str]]]:
        """ポート種別ごとの使用 App を集計."""
        index: dict[PortType, dict[int, list[str]]] = {
            "api": {},
            "frontend": {},
            "db": {},
            "redis": {},
        }

        for app in self._discovery.list_apps():
            port_map = app.ports.model_dump()
            for port_type in self._PORT_TYPES:
                port = port_map.get(port_type)
                if port is None:
                    continue
                index[port_type].setdefault(port, []).append(app.name)

        return index

    def _collect_taken_ports(self) -> set[int]:
        """全 App 設定から使用済みポート集合を取得."""
        taken: set[int] = set()
        for app in self._discovery.list_apps():
            port_map = app.ports.model_dump()
            for port_type in self._PORT_TYPES:
                port = port_map.get(port_type)
                if isinstance(port, int):
                    taken.add(port)
        return taken

    def _find_available(self, *, start: int, taken: set[int]) -> int:
        """利用可能ポートを探索."""
        for port in range(start, 65535):
            if port in taken:
                continue
            if self._is_bindable(port):
                return port

        msg = "利用可能なポートが見つかりません"
        raise RuntimeError(msg)

    @staticmethod
    def _is_bindable(port: int) -> bool:
        """ローカルで bind 可能かを確認."""
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
            sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            try:
                sock.bind(("127.0.0.1", port))
                return True
            except OSError:
                return False
