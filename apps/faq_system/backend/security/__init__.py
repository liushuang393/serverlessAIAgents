# -*- coding: utf-8 -*-
"""FAQ System Security.

FAQ システム固有のセキュリティ機能。

- PermissionConfig: 権限設定
- AuditLogger: 監査ログ
- APPICompliance: APPI（日本個人情報保護法）準拠
"""

from apps.faq_system.backend.security.permission_config import (
    PermissionConfig,
    RolePermissions,
    KBPermission,
    FieldRestriction,
    PermissionLevel,
)
from apps.faq_system.backend.security.audit_logger import (
    AuditLogger,
    AuditLoggerConfig,
    AuditEvent,
    AuditEventType,
    AuditSeverity,
)
from apps.faq_system.backend.security.appi_compliance import (
    APPIComplianceChecker,
    APPIConfig,
    PIIType,
    PIIDetection,
    DataRetentionPolicy,
    BreachReport,
)

__all__ = [
    # 権限設定
    "PermissionConfig",
    "RolePermissions",
    "KBPermission",
    "FieldRestriction",
    "PermissionLevel",
    # 監査ログ
    "AuditLogger",
    "AuditLoggerConfig",
    "AuditEvent",
    "AuditEventType",
    "AuditSeverity",
    # APPI
    "APPIComplianceChecker",
    "APPIConfig",
    "PIIType",
    "PIIDetection",
    "DataRetentionPolicy",
    "BreachReport",
]
