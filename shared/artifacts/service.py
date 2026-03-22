"""Layer 2 の Artifact Store サービス.

ArtifactManifest 契約を一元管理するインメモリストア。
上位層（kernel / harness / platform / apps）は
このサービス経由で成果物メタデータの保存・検索を行う。
"""

from __future__ import annotations

from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from contracts.artifact import ArtifactManifest


class ArtifactStore:
    """ArtifactManifest のインメモリ保存・検索サービス.

    永続化が必要な場合は infrastructure 層の storage adapter と
    組み合わせて拡張する。
    """

    def __init__(self) -> None:
        self._store: dict[str, ArtifactManifest] = {}

    # -- 書き込み -------------------------------------------------------

    def save(self, manifest: ArtifactManifest) -> ArtifactManifest:
        """成果物メタデータを保存する.

        Args:
            manifest: 保存対象の ArtifactManifest

        Returns:
            保存済みの ArtifactManifest
        """
        self._store[manifest.artifact_id] = manifest
        return manifest

    # -- 読み取り -------------------------------------------------------

    def get(self, artifact_id: str) -> ArtifactManifest | None:
        """artifact_id で単一の成果物を取得する."""
        return self._store.get(artifact_id)

    def list_by_flow(self, flow_id: str) -> list[ArtifactManifest]:
        """flow_id に紐づく成果物一覧を返す."""
        return [m for m in self._store.values() if m.flow_id == flow_id]

    def list_by_trace(self, trace_id: str) -> list[ArtifactManifest]:
        """trace_id に紐づく成果物一覧を返す."""
        return [m for m in self._store.values() if m.trace_id == trace_id]

    def list_by_type(self, artifact_type: str) -> list[ArtifactManifest]:
        """artifact_type で成果物を絞り込む."""
        return [m for m in self._store.values() if m.artifact_type == artifact_type]

    def list_all(self) -> list[ArtifactManifest]:
        """全成果物を返す."""
        return list(self._store.values())

    # -- 削除 -----------------------------------------------------------

    def delete(self, artifact_id: str) -> bool:
        """成果物を削除する. 存在しなければ False."""
        if artifact_id in self._store:
            del self._store[artifact_id]
            return True
        return False

    # -- ユーティリティ -------------------------------------------------

    def count(self) -> int:
        """保存件数を返す."""
        return len(self._store)

    def resolve(self, artifact_ids: list[str]) -> list[ArtifactManifest]:
        """ID リストから一括解決する. 存在しないIDは無視."""
        return [self._store[aid] for aid in artifact_ids if aid in self._store]
