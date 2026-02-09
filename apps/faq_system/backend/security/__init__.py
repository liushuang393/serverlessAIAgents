"""FAQ System Security.

FAQ システム固有のセキュリティ機能。

- PermissionConfig: 権限設定
- AuditLogger: 監査ログ
- APPICompliance: APPI（日本個人情報保護法）準拠
"""

from apps.faq_system.backend.security.appi_compliance import (
    APPIComplianceChecker,
    APPIConfig,
    BreachReport,
    DataRetentionPolicy,
    PIIDetection,
    PIIType,
)
from apps.faq_system.backend.security.audit_logger import (
    AuditEvent,
    AuditEventType,
    AuditLogger,
    AuditLoggerConfig,
    AuditSeverity,
)
from apps.faq_system.backend.security.permission_config import (
    FieldRestriction,
    KBPermission,
    PermissionConfig,
    PermissionLevel,
    RolePermissions,
)


__all__ = [
    # APPI
    "APPIComplianceChecker",
    "APPIConfig",
    "AuditEvent",
    "AuditEventType",
    # 監査ログ
    "AuditLogger",
    "AuditLoggerConfig",
    "AuditSeverity",
    "BreachReport",
    "DataRetentionPolicy",
    "FieldRestriction",
    "KBPermission",
    "PIIDetection",
    "PIIType",
    # 権限設定
    "PermissionConfig",
    "PermissionLevel",
    "RolePermissions",
]
