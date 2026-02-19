"""APPI（個人情報保護法）準拠サービス.

日本の個人情報保護法（APPI）に準拠したデータ処理。

機能:
- PII（個人情報）検出
- データ保持ポリシー
- 漏洩報告支援

使用例:
    >>> from apps.faq_system.backend.security import APPIComplianceChecker
    >>>
    >>> checker = APPIComplianceChecker()
    >>> detections = checker.detect_pii("山田太郎のマイナンバーは123456789012です")
    >>> print(detections)  # [PIIDetection(type=MYNUMBER, ...), ...]
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any


logger = logging.getLogger(__name__)


class PIIType(str, Enum):
    """個人情報タイプ."""

    # 特定個人情報（厳格管理）
    MYNUMBER = "mynumber"  # マイナンバー（個人番号）

    # 要配慮個人情報
    HEALTH_INFO = "health_info"  # 健康情報
    CRIMINAL_RECORD = "criminal_record"  # 犯罪歴
    RACE_ETHNICITY = "race_ethnicity"  # 人種・民族
    RELIGION = "religion"  # 宗教・信条
    UNION_MEMBERSHIP = "union_membership"  # 労働組合

    # 一般個人情報
    NAME = "name"  # 氏名
    ADDRESS = "address"  # 住所
    PHONE = "phone"  # 電話番号
    EMAIL = "email"  # メールアドレス
    BIRTHDAY = "birthday"  # 生年月日
    PASSPORT = "passport"  # パスポート番号
    DRIVER_LICENSE = "driver_license"  # 運転免許証番号
    CREDIT_CARD = "credit_card"  # クレジットカード番号
    BANK_ACCOUNT = "bank_account"  # 銀行口座


class PIISeverity(str, Enum):
    """PII 深刻度."""

    CRITICAL = "critical"  # 特定個人情報
    HIGH = "high"  # 要配慮個人情報
    MEDIUM = "medium"  # 一般個人情報
    LOW = "low"  # 軽微


@dataclass
class PIIDetection:
    """PII 検出結果.

    Attributes:
        pii_type: PIIタイプ
        severity: 深刻度
        value_masked: マスク済み値
        position: 位置（開始, 終了）
        context: コンテキスト
        confidence: 信頼度
    """

    pii_type: PIIType
    severity: PIISeverity
    value_masked: str
    position: tuple[int, int]
    context: str = ""
    confidence: float = 1.0


@dataclass
class DataRetentionPolicy:
    """データ保持ポリシー.

    Attributes:
        category: データカテゴリ
        retention_days: 保持日数
        requires_consent: 同意が必要か
        deletion_method: 削除方法
        legal_basis: 法的根拠
    """

    category: str
    retention_days: int
    requires_consent: bool = True
    deletion_method: str = "secure_delete"
    legal_basis: str = ""


@dataclass
class BreachReport:
    """漏洩報告.

    Attributes:
        report_id: 報告ID
        incident_type: インシデントタイプ
        severity: 深刻度
        affected_count: 影響人数
        pii_types: 漏洩したPIIタイプ
        discovered_at: 発見日時
        reported_at: 報告日時
        description: 説明
        actions_taken: 対応措置
        regulatory_notification: 規制当局通知
    """

    report_id: str
    incident_type: str
    severity: PIISeverity
    affected_count: int
    pii_types: list[PIIType]
    discovered_at: datetime
    reported_at: datetime | None = None
    description: str = ""
    actions_taken: list[str] = field(default_factory=list)
    regulatory_notification: dict[str, Any] = field(default_factory=dict)


@dataclass
class APPIConfig:
    """APPI 設定."""

    # 検出設定
    detect_mynumber: bool = True
    detect_health_info: bool = True
    detect_financial: bool = True
    detect_contact: bool = True

    # マスク設定
    mask_character: str = "*"
    mask_partial: bool = True  # 部分マスク（例: 1234****9012）

    # 保持ポリシー
    default_retention_days: int = 365 * 5  # 5年
    mynumber_retention_days: int = 365 * 7  # 7年（法定）

    # 漏洩通知
    breach_notification_threshold: int = 1  # 通知閾値（人数）
    notification_deadline_hours: int = 72  # 通知期限（時間）


class APPIComplianceChecker:
    """APPI準拠チェッカー.

    日本の個人情報保護法に準拠したデータ処理。

    Example:
        >>> checker = APPIComplianceChecker()
        >>>
        >>> # PII検出
        >>> detections = checker.detect_pii(text)
        >>>
        >>> # マスク処理
        >>> masked = checker.mask_pii(text)
        >>>
        >>> # 漏洩報告
        >>> report = checker.create_breach_report(
        ...     incident_type="unauthorized_access",
        ...     affected_count=100,
        ...     pii_types=[PIIType.NAME, PIIType.EMAIL],
        ... )
    """

    # PII 検出パターン
    PII_PATTERNS = {
        PIIType.MYNUMBER: (
            r"\b\d{12}\b",  # 12桁の数字（マイナンバー）
            PIISeverity.CRITICAL,
        ),
        PIIType.PHONE: (
            r"\b0\d{1,4}[-\s]?\d{1,4}[-\s]?\d{4}\b",  # 電話番号
            PIISeverity.MEDIUM,
        ),
        PIIType.EMAIL: (
            r"\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b",
            PIISeverity.MEDIUM,
        ),
        PIIType.CREDIT_CARD: (
            r"\b(?:\d{4}[-\s]?){3}\d{4}\b",  # クレジットカード（16桁）
            PIISeverity.HIGH,
        ),
        PIIType.BIRTHDAY: (
            r"\b(?:19|20)\d{2}[-/年]\d{1,2}[-/月]\d{1,2}日?\b",  # 生年月日
            PIISeverity.MEDIUM,
        ),
        PIIType.PASSPORT: (
            r"\b[A-Z]{2}\d{7}\b",  # パスポート番号
            PIISeverity.HIGH,
        ),
    }

    # デフォルト保持ポリシー
    DEFAULT_RETENTION_POLICIES = [
        DataRetentionPolicy(
            category="特定個人情報（マイナンバー）",
            retention_days=365 * 7,
            requires_consent=True,
            deletion_method="secure_overwrite",
            legal_basis="番号法（マイナンバー法）",
        ),
        DataRetentionPolicy(
            category="要配慮個人情報",
            retention_days=365 * 5,
            requires_consent=True,
            deletion_method="secure_delete",
            legal_basis="APPI 第20条",
        ),
        DataRetentionPolicy(
            category="一般個人情報",
            retention_days=365 * 5,
            requires_consent=True,
            deletion_method="secure_delete",
            legal_basis="APPI 第19条",
        ),
        DataRetentionPolicy(
            category="問合せログ",
            retention_days=365 * 3,
            requires_consent=False,
            deletion_method="logical_delete",
            legal_basis="業務上必要な範囲",
        ),
    ]

    def __init__(
        self,
        config: APPIConfig | None = None,
    ) -> None:
        """初期化.

        Args:
            config: 設定
        """
        self._config = config or APPIConfig()
        self._retention_policies = self.DEFAULT_RETENTION_POLICIES.copy()
        self._breach_reports: list[BreachReport] = []
        self._counter = 0
        self._logger = logging.getLogger(__name__)

    def detect_pii(
        self,
        text: str,
        include_context: bool = True,
    ) -> list[PIIDetection]:
        """PII を検出.

        Args:
            text: テキスト
            include_context: コンテキストを含めるか

        Returns:
            PII検出結果リスト
        """
        detections = []

        for pii_type, (pattern, severity) in self.PII_PATTERNS.items():
            # 設定でスキップ
            if pii_type == PIIType.MYNUMBER and not self._config.detect_mynumber:
                continue

            for match in re.finditer(pattern, text):
                value = match.group()
                start, end = match.span()

                # コンテキスト抽出
                context = ""
                if include_context:
                    ctx_start = max(0, start - 20)
                    ctx_end = min(len(text), end + 20)
                    context = text[ctx_start:ctx_end]

                detection = PIIDetection(
                    pii_type=pii_type,
                    severity=severity,
                    value_masked=self._mask_value(value, pii_type),
                    position=(start, end),
                    context=context,
                    confidence=self._calculate_confidence(value, pii_type),
                )
                detections.append(detection)

        return detections

    def mask_pii(self, text: str) -> str:
        """PII をマスク.

        Args:
            text: テキスト

        Returns:
            マスク済みテキスト
        """
        result = text
        detections = self.detect_pii(text, include_context=False)

        # 位置が後ろのものから置換（位置ずれ防止）
        detections.sort(key=lambda d: d.position[0], reverse=True)

        for detection in detections:
            start, end = detection.position
            original = result[start:end]
            masked = self._mask_value(original, detection.pii_type)
            result = result[:start] + masked + result[end:]

        return result

    def _mask_value(self, value: str, pii_type: PIIType) -> str:
        """値をマスク.

        Args:
            value: 値
            pii_type: PIIタイプ

        Returns:
            マスク済み値
        """
        if not self._config.mask_partial:
            return self._config.mask_character * len(value)

        # 部分マスク
        if pii_type == PIIType.MYNUMBER:
            # マイナンバーは全マスク
            return self._config.mask_character * len(value)
        if pii_type == PIIType.CREDIT_CARD:
            # カード番号は末尾4桁のみ表示
            cleaned = re.sub(r"[-\s]", "", value)
            return self._config.mask_character * (len(cleaned) - 4) + cleaned[-4:]
        if pii_type == PIIType.PHONE:
            # 電話番号は中間をマスク
            cleaned = re.sub(r"[-\s]", "", value)
            if len(cleaned) > 6:
                return cleaned[:3] + self._config.mask_character * (len(cleaned) - 7) + cleaned[-4:]
        elif pii_type == PIIType.EMAIL:
            # メールはドメイン部分を残す
            parts = value.split("@")
            if len(parts) == 2:
                local = parts[0]
                domain = parts[1]
                masked_local = local[0] + self._config.mask_character * (len(local) - 1)
                return f"{masked_local}@{domain}"

        # デフォルト: 全マスク
        return self._config.mask_character * len(value)

    def _calculate_confidence(self, value: str, pii_type: PIIType) -> float:
        """検出信頼度を計算.

        Args:
            value: 値
            pii_type: PIIタイプ

        Returns:
            信頼度（0.0-1.0）
        """
        # マイナンバーのチェックディジット検証
        if pii_type == PIIType.MYNUMBER:
            return self._validate_mynumber_checksum(value)

        # クレジットカードの Luhn アルゴリズム
        if pii_type == PIIType.CREDIT_CARD:
            cleaned = re.sub(r"[-\s]", "", value)
            return 0.9 if self._luhn_check(cleaned) else 0.5

        return 0.8  # デフォルト

    def _validate_mynumber_checksum(self, value: str) -> float:
        """マイナンバーのチェックサムを検証.

        Args:
            value: 12桁の数字

        Returns:
            信頼度
        """
        if not re.match(r"^\d{12}$", value):
            return 0.5

        # チェックディジット計算
        digits = [int(d) for d in value]
        weights_1 = [6, 5, 4, 3, 2, 7, 6, 5, 4, 3, 2]

        remainder = sum(d * w for d, w in zip(digits[:11], weights_1, strict=False)) % 11
        check_digit = 0 if remainder <= 1 else 11 - remainder

        if digits[11] == check_digit:
            return 0.95
        return 0.6

    def _luhn_check(self, card_number: str) -> bool:
        """Luhn アルゴリズムでカード番号を検証."""
        if not card_number.isdigit():
            return False

        digits = [int(d) for d in card_number]
        checksum = 0

        for i, digit in enumerate(reversed(digits)):
            if i % 2 == 1:
                digit *= 2
                if digit > 9:
                    digit -= 9
            checksum += digit

        return checksum % 10 == 0

    def check_retention_compliance(
        self,
        data_category: str,
        created_at: datetime,
    ) -> dict[str, Any]:
        """保持期間コンプライアンスをチェック.

        Args:
            data_category: データカテゴリ
            created_at: 作成日時

        Returns:
            チェック結果
        """
        policy = next(
            (p for p in self._retention_policies if p.category == data_category),
            None,
        )

        if not policy:
            return {
                "compliant": False,
                "reason": f"Unknown category: {data_category}",
            }

        retention_end = created_at + timedelta(days=policy.retention_days)
        days_until_deletion = (retention_end - datetime.now()).days

        return {
            "compliant": True,
            "policy": policy.category,
            "retention_days": policy.retention_days,
            "legal_basis": policy.legal_basis,
            "deletion_required": days_until_deletion <= 0,
            "days_until_deletion": max(0, days_until_deletion),
            "deletion_method": policy.deletion_method,
        }

    def create_breach_report(
        self,
        incident_type: str,
        affected_count: int,
        pii_types: list[PIIType],
        description: str = "",
        discovered_at: datetime | None = None,
    ) -> BreachReport:
        """漏洩報告を作成.

        Args:
            incident_type: インシデントタイプ
            affected_count: 影響人数
            pii_types: 漏洩したPIIタイプ
            description: 説明
            discovered_at: 発見日時

        Returns:
            漏洩報告
        """
        self._counter += 1
        report_id = f"BR-{datetime.now().strftime('%Y%m%d')}-{self._counter:04d}"

        # 深刻度を判定
        severity = self._determine_breach_severity(pii_types, affected_count)

        report = BreachReport(
            report_id=report_id,
            incident_type=incident_type,
            severity=severity,
            affected_count=affected_count,
            pii_types=pii_types,
            discovered_at=discovered_at or datetime.now(),
            description=description,
        )

        # 通知期限を設定
        notification_deadline = report.discovered_at + timedelta(
            hours=self._config.notification_deadline_hours
        )

        report.regulatory_notification = {
            "required": affected_count >= self._config.breach_notification_threshold,
            "deadline": notification_deadline.isoformat(),
            "authorities": self._get_notification_authorities(pii_types),
            "affected_individuals_notification": True,
        }

        self._breach_reports.append(report)

        self._logger.critical(
            "BREACH REPORT CREATED: %s, affected=%d, severity=%s",
            report_id,
            affected_count,
            severity.value,
        )

        return report

    def _determine_breach_severity(
        self,
        pii_types: list[PIIType],
        affected_count: int,
    ) -> PIISeverity:
        """漏洩深刻度を判定."""
        # マイナンバー漏洩は常に CRITICAL
        if PIIType.MYNUMBER in pii_types:
            return PIISeverity.CRITICAL

        # 要配慮個人情報は HIGH
        sensitive_types = {
            PIIType.HEALTH_INFO,
            PIIType.CRIMINAL_RECORD,
            PIIType.RACE_ETHNICITY,
            PIIType.RELIGION,
        }
        if any(t in sensitive_types for t in pii_types):
            return PIISeverity.HIGH

        # 大量漏洩は HIGH
        if affected_count >= 1000:
            return PIISeverity.HIGH

        return PIISeverity.MEDIUM

    def _get_notification_authorities(
        self,
        pii_types: list[PIIType],
    ) -> list[str]:
        """通知先当局を取得."""
        authorities = ["個人情報保護委員会"]

        if PIIType.MYNUMBER in pii_types:
            authorities.append("国税庁")
            authorities.append("厚生労働省")

        if PIIType.CREDIT_CARD in pii_types:
            authorities.append("経済産業省")

        return authorities

    def get_retention_policies(self) -> list[DataRetentionPolicy]:
        """保持ポリシー一覧を取得."""
        return self._retention_policies.copy()

    def get_breach_reports(
        self,
        since: datetime | None = None,
        severity: PIISeverity | None = None,
    ) -> list[BreachReport]:
        """漏洩報告を取得."""
        reports = self._breach_reports.copy()

        if since:
            reports = [r for r in reports if r.discovered_at >= since]
        if severity:
            reports = [r for r in reports if r.severity == severity]

        return reports


__all__ = [
    "APPIComplianceChecker",
    "APPIConfig",
    "BreachReport",
    "DataRetentionPolicy",
    "PIIDetection",
    "PIISeverity",
    "PIIType",
]
