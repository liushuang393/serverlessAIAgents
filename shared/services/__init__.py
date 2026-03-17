"""services パッケージ — shared 層サービス統合エントリポイント.

platform/services/ から移設。kernel/ や shared/ 内部から
dependency-violation なしにインポートできる正規パス。
"""

from shared.services.workflow_service import WorkflowService

__all__ = ["WorkflowService"]
