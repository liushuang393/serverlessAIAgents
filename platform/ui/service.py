"""Layer 5 UI bundle 管理サービス."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from pathlib import Path


@dataclass(frozen=True, slots=True)
class FrontendBundle:
    """配信対象 UI bundle の定義."""

    name: str
    source_dir: Path
    entrypoint: str = "index.html"


@dataclass(slots=True)
class PlatformUIService:
    """UI bundle を登録・列挙する."""

    bundles: dict[str, FrontendBundle] = field(default_factory=dict)

    def register(self, bundle: FrontendBundle) -> None:
        """UI bundle を登録する。"""
        self.bundles[bundle.name] = bundle

    def get(self, name: str) -> FrontendBundle | None:
        """名前で bundle を取得する。"""
        return self.bundles.get(name)

    def list_bundles(self) -> list[FrontendBundle]:
        """登録済み bundle 一覧を返す。"""
        return sorted(self.bundles.values(), key=lambda item: item.name)
