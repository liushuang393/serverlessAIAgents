"""Layer 5 API 定義サービス."""

from __future__ import annotations

from dataclasses import dataclass, field


@dataclass(frozen=True, slots=True)
class RouteDefinition:
    """Platform API の公開ルート定義."""

    method: str
    path: str
    description: str
    tags: tuple[str, ...] = ()


@dataclass(slots=True)
class PlatformAPISurface:
    """Platform API ルート一覧を管理する."""

    routes: list[RouteDefinition] = field(default_factory=list)

    def register(self, route: RouteDefinition) -> None:
        """ルート定義を追加する。"""
        self.routes.append(route)

    def list_routes(self) -> list[RouteDefinition]:
        """登録済みルートを返す。"""
        return sorted(self.routes, key=lambda item: (item.path, item.method))
