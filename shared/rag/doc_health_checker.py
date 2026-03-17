"""ドキュメント健康度チェッカー.

知識ベースのドキュメント品質と健康度を監視するモジュール。

機能:
- 期限切れドキュメント検出
- 重複ドキュメント検出
- 欠落セクション検出
- 更新推奨通知

使用例:
    >>> from shared.rag.doc_health_checker import DocHealthChecker
    >>>
    >>> checker = DocHealthChecker()
    >>>
    >>> # 健康度チェック
    >>> report = await checker.check_health(documents)
    >>> print(report.expired_count)
    >>> print(report.recommendations)
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any


logger = logging.getLogger(__name__)


class HealthStatus(str, Enum):
    """健康度ステータス."""

    HEALTHY = "healthy"  # 健康
    WARNING = "warning"  # 警告
    CRITICAL = "critical"  # 重大
    EXPIRED = "expired"  # 期限切れ


class IssueType(str, Enum):
    """問題タイプ."""

    EXPIRED = "expired"  # 期限切れ
    EXPIRING_SOON = "expiring_soon"  # 期限切れ間近
    DUPLICATE = "duplicate"  # 重複
    MISSING_SECTION = "missing_section"  # セクション欠落
    OUTDATED_REFERENCE = "outdated_reference"  # 古い参照
    LOW_QUALITY = "low_quality"  # 低品質
    NO_OWNER = "no_owner"  # オーナー不在
    STALE = "stale"  # 長期未更新


class IssueSeverity(str, Enum):
    """問題深刻度."""

    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"


@dataclass
class Document:
    """ドキュメント.

    Attributes:
        document_id: ドキュメントID
        title: タイトル
        content: 内容
        source: ソース
        version: バージョン
        created_at: 作成日時
        updated_at: 更新日時
        effective_date: 有効開始日
        expiry_date: 有効期限
        owner_id: オーナーID
        owner_department: オーナー部門
        metadata: メタデータ
    """

    document_id: str
    title: str
    content: str = ""
    source: str = ""
    version: str = "1.0"
    created_at: datetime = field(default_factory=datetime.now)
    updated_at: datetime = field(default_factory=datetime.now)
    effective_date: datetime | None = None
    expiry_date: datetime | None = None
    owner_id: str = ""
    owner_department: str = ""
    category: str = ""
    tags: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass
class HealthIssue:
    """健康度問題.

    Attributes:
        issue_type: 問題タイプ
        severity: 深刻度
        document_id: 対象ドキュメントID
        title: タイトル
        description: 説明
        recommendation: 推奨アクション
    """

    issue_type: IssueType
    severity: IssueSeverity
    document_id: str
    title: str
    description: str
    recommendation: str
    related_documents: list[str] = field(default_factory=list)
    detected_at: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "issue_type": self.issue_type.value,
            "severity": self.severity.value,
            "document_id": self.document_id,
            "title": self.title,
            "description": self.description,
            "recommendation": self.recommendation,
            "related_documents": self.related_documents,
            "detected_at": self.detected_at.isoformat(),
        }


@dataclass
class HealthReport:
    """健康度レポート.

    Attributes:
        total_documents: 総ドキュメント数
        healthy_count: 健康なドキュメント数
        warning_count: 警告ドキュメント数
        critical_count: 重大問題ドキュメント数
        expired_count: 期限切れドキュメント数
        issues: 問題リスト
        recommendations: 推奨アクションリスト
        checked_at: チェック日時
    """

    total_documents: int = 0
    healthy_count: int = 0
    warning_count: int = 0
    critical_count: int = 0
    expired_count: int = 0
    duplicate_count: int = 0
    no_owner_count: int = 0
    stale_count: int = 0
    issues: list[HealthIssue] = field(default_factory=list)
    recommendations: list[str] = field(default_factory=list)
    checked_at: datetime = field(default_factory=datetime.now)
    health_score: float = 100.0  # 0-100

    @property
    def status(self) -> HealthStatus:
        """全体ステータス."""
        if self.critical_count > 0 or self.expired_count > 0:
            return HealthStatus.CRITICAL
        if self.warning_count > 0:
            return HealthStatus.WARNING
        return HealthStatus.HEALTHY

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "total_documents": self.total_documents,
            "healthy_count": self.healthy_count,
            "warning_count": self.warning_count,
            "critical_count": self.critical_count,
            "expired_count": self.expired_count,
            "duplicate_count": self.duplicate_count,
            "no_owner_count": self.no_owner_count,
            "stale_count": self.stale_count,
            "status": self.status.value,
            "health_score": self.health_score,
            "issues_count": len(self.issues),
            "checked_at": self.checked_at.isoformat(),
        }


@dataclass
class DocHealthConfig:
    """健康度チェック設定."""

    # 期限チェック
    expiry_warning_days: int = 30  # 期限切れ警告日数
    stale_days: int = 365  # 長期未更新の日数

    # 重複検出
    similarity_threshold: float = 0.85  # 重複判定閾値
    min_content_length: int = 100  # 最小コンテンツ長

    # 品質チェック
    required_sections: list[str] = field(
        default_factory=lambda: [
            "目的",
            "概要",
            "適用範囲",
        ]
    )
    min_word_count: int = 50

    # オーナーチェック
    require_owner: bool = True


class DocHealthChecker:
    """ドキュメント健康度チェッカー.

    知識ベースのドキュメント品質と健康度を監視。

    Example:
        >>> checker = DocHealthChecker()
        >>>
        >>> # 健康度チェック
        >>> report = await checker.check_health(documents)
        >>> print(f"健康スコア: {report.health_score}")
        >>>
        >>> # 期限切れ通知
        >>> expiring = checker.get_expiring_documents(documents, days=30)
    """

    def __init__(
        self,
        config: DocHealthConfig | None = None,
    ) -> None:
        """初期化.

        Args:
            config: 設定
        """
        self._config = config or DocHealthConfig()
        self._logger = logging.getLogger(__name__)

    async def check_health(
        self,
        documents: list[Document],
    ) -> HealthReport:
        """健康度をチェック.

        Args:
            documents: ドキュメントリスト

        Returns:
            健康度レポート
        """
        report = HealthReport(total_documents=len(documents))

        if not documents:
            return report

        # 各種チェックを実行
        expired_issues = await self._check_expiry(documents)
        duplicate_issues = await self._check_duplicates(documents)
        quality_issues = await self._check_quality(documents)
        owner_issues = await self._check_owners(documents)
        stale_issues = await self._check_staleness(documents)

        # 問題を集約
        all_issues = expired_issues + duplicate_issues + quality_issues + owner_issues + stale_issues
        report.issues = all_issues

        # カウントを更新
        report.expired_count = len([i for i in all_issues if i.issue_type == IssueType.EXPIRED])
        report.duplicate_count = len([i for i in all_issues if i.issue_type == IssueType.DUPLICATE])
        report.no_owner_count = len([i for i in all_issues if i.issue_type == IssueType.NO_OWNER])
        report.stale_count = len([i for i in all_issues if i.issue_type == IssueType.STALE])

        # 深刻度別カウント
        report.critical_count = len([i for i in all_issues if i.severity == IssueSeverity.CRITICAL])
        report.warning_count = len([i for i in all_issues if i.severity in (IssueSeverity.MEDIUM, IssueSeverity.HIGH)])

        # 健康なドキュメント数
        problem_doc_ids = {i.document_id for i in all_issues}
        report.healthy_count = len(documents) - len(problem_doc_ids)

        # 健康スコアを計算
        report.health_score = self._calculate_health_score(report)

        # 推奨アクションを生成
        report.recommendations = self._generate_recommendations(report)

        return report

    async def _check_expiry(
        self,
        documents: list[Document],
    ) -> list[HealthIssue]:
        """期限切れチェック.

        Args:
            documents: ドキュメントリスト

        Returns:
            問題リスト
        """
        issues: list[HealthIssue] = []
        now = datetime.now()
        warning_threshold = now + timedelta(days=self._config.expiry_warning_days)

        for doc in documents:
            if not doc.expiry_date:
                continue

            if doc.expiry_date < now:
                # 期限切れ
                issues.append(
                    HealthIssue(
                        issue_type=IssueType.EXPIRED,
                        severity=IssueSeverity.CRITICAL,
                        document_id=doc.document_id,
                        title=doc.title,
                        description=f"有効期限切れ: {doc.expiry_date.strftime('%Y-%m-%d')}",
                        recommendation="ドキュメントの更新または廃止をご検討ください。",
                    )
                )
            elif doc.expiry_date < warning_threshold:
                # 期限切れ間近
                days_until = (doc.expiry_date - now).days
                issues.append(
                    HealthIssue(
                        issue_type=IssueType.EXPIRING_SOON,
                        severity=IssueSeverity.HIGH,
                        document_id=doc.document_id,
                        title=doc.title,
                        description=f"有効期限まで{days_until}日: {doc.expiry_date.strftime('%Y-%m-%d')}",
                        recommendation="有効期限の延長または内容の更新をご検討ください。",
                    )
                )

        return issues

    async def _check_duplicates(
        self,
        documents: list[Document],
    ) -> list[HealthIssue]:
        """重複チェック.

        Args:
            documents: ドキュメントリスト

        Returns:
            問題リスト
        """
        issues: list[HealthIssue] = []
        processed: set[str] = set()

        for i, doc1 in enumerate(documents):
            if doc1.document_id in processed:
                continue

            if len(doc1.content) < self._config.min_content_length:
                continue

            duplicates = []

            for doc2 in documents[i + 1 :]:
                if doc2.document_id in processed:
                    continue

                similarity = self._calculate_similarity(doc1.content, doc2.content)

                if similarity >= self._config.similarity_threshold:
                    duplicates.append(doc2.document_id)
                    processed.add(doc2.document_id)

            if duplicates:
                issues.append(
                    HealthIssue(
                        issue_type=IssueType.DUPLICATE,
                        severity=IssueSeverity.MEDIUM,
                        document_id=doc1.document_id,
                        title=doc1.title,
                        description=f"{len(duplicates)}件の重複が検出されました。",
                        recommendation="重複ドキュメントの統合をご検討ください。",
                        related_documents=duplicates,
                    )
                )

        return issues

    async def _check_quality(
        self,
        documents: list[Document],
    ) -> list[HealthIssue]:
        """品質チェック.

        Args:
            documents: ドキュメントリスト

        Returns:
            問題リスト
        """
        issues: list[HealthIssue] = []

        for doc in documents:
            # コンテンツ長チェック
            word_count = len(doc.content.split())
            if word_count < self._config.min_word_count:
                issues.append(
                    HealthIssue(
                        issue_type=IssueType.LOW_QUALITY,
                        severity=IssueSeverity.LOW,
                        document_id=doc.document_id,
                        title=doc.title,
                        description=f"コンテンツが短すぎます（{word_count}語）。",
                        recommendation="より詳細な情報の追加をご検討ください。",
                    )
                )

            # 必須セクションチェック
            missing_sections = []
            for section in self._config.required_sections:
                if section not in doc.content:
                    missing_sections.append(section)

            if missing_sections:
                issues.append(
                    HealthIssue(
                        issue_type=IssueType.MISSING_SECTION,
                        severity=IssueSeverity.MEDIUM,
                        document_id=doc.document_id,
                        title=doc.title,
                        description=f"セクション欠落: {', '.join(missing_sections)}",
                        recommendation="必須セクションの追加をご検討ください。",
                    )
                )

        return issues

    async def _check_owners(
        self,
        documents: list[Document],
    ) -> list[HealthIssue]:
        """オーナーチェック.

        Args:
            documents: ドキュメントリスト

        Returns:
            問題リスト
        """
        issues: list[HealthIssue] = []

        if not self._config.require_owner:
            return issues

        for doc in documents:
            if not doc.owner_id and not doc.owner_department:
                issues.append(
                    HealthIssue(
                        issue_type=IssueType.NO_OWNER,
                        severity=IssueSeverity.HIGH,
                        document_id=doc.document_id,
                        title=doc.title,
                        description="オーナーが設定されていません。",
                        recommendation="ドキュメントオーナーの設定をお願いします。",
                    )
                )

        return issues

    async def _check_staleness(
        self,
        documents: list[Document],
    ) -> list[HealthIssue]:
        """長期未更新チェック.

        Args:
            documents: ドキュメントリスト

        Returns:
            問題リスト
        """
        issues = []
        stale_threshold = datetime.now() - timedelta(days=self._config.stale_days)

        for doc in documents:
            if doc.updated_at < stale_threshold:
                days_since = (datetime.now() - doc.updated_at).days
                issues.append(
                    HealthIssue(
                        issue_type=IssueType.STALE,
                        severity=IssueSeverity.LOW,
                        document_id=doc.document_id,
                        title=doc.title,
                        description=f"{days_since}日間更新されていません。",
                        recommendation="内容の確認・更新をご検討ください。",
                    )
                )

        return issues

    def _calculate_similarity(self, text1: str, text2: str) -> float:
        """テキスト類似度を計算.

        Args:
            text1: テキスト1
            text2: テキスト2

        Returns:
            類似度（0.0-1.0）
        """
        # 簡易実装: Jaccard類似度
        words1 = set(text1.lower().split())
        words2 = set(text2.lower().split())

        if not words1 or not words2:
            return 0.0

        intersection = words1 & words2
        union = words1 | words2

        return len(intersection) / len(union)

    def _calculate_health_score(self, report: HealthReport) -> float:
        """健康スコアを計算.

        Args:
            report: レポート

        Returns:
            健康スコア（0-100）
        """
        if report.total_documents == 0:
            return 100.0

        score = 100.0

        # 期限切れペナルティ
        expired_ratio = report.expired_count / report.total_documents
        score -= expired_ratio * 30

        # 重複ペナルティ
        duplicate_ratio = report.duplicate_count / report.total_documents
        score -= duplicate_ratio * 10

        # オーナー不在ペナルティ
        no_owner_ratio = report.no_owner_count / report.total_documents
        score -= no_owner_ratio * 15

        # 長期未更新ペナルティ
        stale_ratio = report.stale_count / report.total_documents
        score -= stale_ratio * 5

        return max(0.0, score)

    def _generate_recommendations(
        self,
        report: HealthReport,
    ) -> list[str]:
        """推奨アクションを生成.

        Args:
            report: レポート

        Returns:
            推奨アクションリスト
        """
        recommendations = []

        if report.expired_count > 0:
            recommendations.append(
                f"⚠️ {report.expired_count}件のドキュメントが期限切れです。早急に更新または廃止をご検討ください。"
            )

        if report.no_owner_count > 0:
            recommendations.append(
                f"📋 {report.no_owner_count}件のドキュメントにオーナーが設定されていません。"
                "責任者の設定をお願いします。"
            )

        if report.duplicate_count > 0:
            recommendations.append(
                f"📑 {report.duplicate_count}件の重複ドキュメントが検出されました。統合をご検討ください。"
            )

        if report.stale_count > report.total_documents * 0.3:
            recommendations.append(
                "📅 多くのドキュメントが長期間更新されていません。定期的なレビュープロセスの導入をご検討ください。"
            )

        if not recommendations:
            recommendations.append("✅ 知識ベースは健康な状態です。")

        return recommendations

    def get_expiring_documents(
        self,
        documents: list[Document],
        days: int = 30,
    ) -> list[Document]:
        """期限切れ間近のドキュメントを取得.

        Args:
            documents: ドキュメントリスト
            days: 日数

        Returns:
            期限切れ間近のドキュメントリスト
        """
        threshold = datetime.now() + timedelta(days=days)
        now = datetime.now()

        return [doc for doc in documents if doc.expiry_date and now < doc.expiry_date <= threshold]

    def get_documents_by_owner(
        self,
        documents: list[Document],
        owner_id: str | None = None,
        department: str | None = None,
    ) -> list[Document]:
        """オーナー別ドキュメントを取得.

        Args:
            documents: ドキュメントリスト
            owner_id: オーナーID
            department: 部門

        Returns:
            ドキュメントリスト
        """
        results = documents

        if owner_id:
            results = [d for d in results if d.owner_id == owner_id]

        if department:
            results = [d for d in results if d.owner_department == department]

        return results


__all__ = [
    "DocHealthChecker",
    "DocHealthConfig",
    "Document",
    "HealthIssue",
    "HealthReport",
    "HealthStatus",
    "IssueSeverity",
    "IssueType",
]
