# -*- coding: utf-8 -*-
"""数据脱敏器 - AI安全与隐私保护.

解决LLM应用中的安全与隐私问题:
- 提示注入检测与防护
- 敏感数据脱敏
- 权限边界控制
- 输出审计

参考文献:
- Microsoft Copilot Security Analysis (2024)
- Prompt Injection Attacks (2023)
- LLM Application Security Best Practices (2024)

使用例:
    >>> from agentflow.security.data_sanitizer import DataSanitizer
    >>>
    >>> sanitizer = DataSanitizer()
    >>> 
    >>> # 检测提示注入
    >>> is_safe = sanitizer.check_prompt_injection(user_input)
    >>> 
    >>> # 脱敏处理
    >>> sanitized = sanitizer.sanitize_pii(sensitive_text)
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Callable

from pydantic import BaseModel, Field


class ThreatType(str, Enum):
    """威胁类型."""
    
    PROMPT_INJECTION = "prompt_injection"  # 提示注入
    DATA_EXFILTRATION = "data_exfiltration"  # 数据泄露
    PRIVILEGE_ESCALATION = "privilege_escalation"  # 权限提升
    JAILBREAK = "jailbreak"  # 越狱攻击
    PII_EXPOSURE = "pii_exposure"  # 个人信息暴露


class PIIType(str, Enum):
    """个人识别信息类型."""
    
    EMAIL = "email"
    PHONE = "phone"
    ID_CARD = "id_card"
    CREDIT_CARD = "credit_card"
    ADDRESS = "address"
    NAME = "name"
    IP_ADDRESS = "ip_address"
    PASSWORD = "password"
    API_KEY = "api_key"


@dataclass
class ThreatDetection:
    """威胁检测结果.
    
    Attributes:
        threat_type: 威胁类型
        severity: 严重程度（0.0-1.0）
        description: 描述
        location: 位置
        blocked: 是否已阻止
    """
    
    threat_type: ThreatType
    severity: float
    description: str
    location: str = ""
    blocked: bool = False
    
    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "type": self.threat_type.value,
            "severity": self.severity,
            "description": self.description,
            "location": self.location,
            "blocked": self.blocked,
        }


@dataclass
class SanitizationResult:
    """脱敏结果.
    
    Attributes:
        original_text: 原始文本
        sanitized_text: 脱敏后文本
        detections: 检测到的敏感信息
        is_safe: 是否安全
    """
    
    original_text: str
    sanitized_text: str
    detections: list[dict[str, Any]] = field(default_factory=list)
    is_safe: bool = True
    processed_at: datetime = field(default_factory=datetime.now)
    
    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "sanitized_text": self.sanitized_text,
            "is_safe": self.is_safe,
            "detections_count": len(self.detections),
            "processed_at": self.processed_at.isoformat(),
        }


class SanitizerConfig(BaseModel):
    """脱敏配置."""

    # 是否启用各类检测
    detect_prompt_injection: bool = Field(default=True)
    detect_pii: bool = Field(default=True)
    detect_api_keys: bool = Field(default=True)

    # 脱敏行为
    mask_pii: bool = Field(default=True)
    block_injection: bool = Field(default=True)

    # 自定义敏感词
    custom_sensitive_words: list[str] = Field(default_factory=list)

    # 白名单域名（不脱敏）
    whitelist_domains: list[str] = Field(default_factory=list)

    # 严格模式
    strict_mode: bool = Field(default=False)


class DataSanitizer:
    """数据脱敏器.

    保护AI应用免受安全威胁，确保数据隐私。

    核心功能:
    1. 提示注入检测：识别恶意提示注入攻击
    2. PII脱敏：自动检测和遮盖个人识别信息
    3. API密钥检测：防止API密钥泄露
    4. 输出审计：审计LLM输出中的敏感信息

    Example:
        >>> sanitizer = DataSanitizer()
        >>>
        >>> # 检查用户输入
        >>> threats = sanitizer.check_prompt_injection(user_input)
        >>> if threats:
        ...     print("检测到威胁!")
        >>>
        >>> # 脱敏处理
        >>> result = sanitizer.sanitize(text)
    """

    # 提示注入模式
    INJECTION_PATTERNS = [
        # 直接指令覆盖
        (r"忽略.{0,20}指令", "instruction_override"),
        (r"ignore.{0,20}instructions?", "instruction_override"),
        (r"disregard.{0,20}(previous|above)", "instruction_override"),
        # 角色扮演攻击
        (r"你现在是", "role_hijack"),
        (r"pretend.{0,10}(you|to).{0,10}(are|be)", "role_hijack"),
        (r"act as.{0,10}(a|an|the)", "role_hijack"),
        # 系统提示泄露
        (r"显示.{0,10}系统提示", "system_leak"),
        (r"(show|reveal|print).{0,10}(system|initial).{0,10}prompt", "system_leak"),
        # 越狱
        (r"DAN.{0,10}mode", "jailbreak"),
        (r"developer.{0,10}mode", "jailbreak"),
    ]

    # PII模式
    PII_PATTERNS = {
        PIIType.EMAIL: r"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}",
        PIIType.PHONE: r"(?:\+?86)?1[3-9]\d{9}|(?:\d{3,4}-?)?\d{7,8}",
        PIIType.ID_CARD: r"\d{17}[\dXx]|\d{15}",
        PIIType.CREDIT_CARD: r"\d{4}[- ]?\d{4}[- ]?\d{4}[- ]?\d{4}",
        PIIType.IP_ADDRESS: r"\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}",
    }

    # API密钥模式
    API_KEY_PATTERNS = [
        (r"sk-[a-zA-Z0-9]{20,}", "openai"),
        (r"AKIA[0-9A-Z]{16}", "aws"),
        (r"AIza[0-9A-Za-z_-]{35}", "google"),
        (r"ghp_[a-zA-Z0-9]{36}", "github"),
        (r"Bearer [a-zA-Z0-9_-]{20,}", "bearer_token"),
    ]

    def __init__(
        self,
        config: SanitizerConfig | None = None,
    ) -> None:
        """初始化.

        Args:
            config: 脱敏配置
        """
        self._config = config or SanitizerConfig()
        self._logger = logging.getLogger(__name__)

    def check_prompt_injection(
        self,
        text: str,
    ) -> list[ThreatDetection]:
        """检测提示注入.

        Args:
            text: 输入文本

        Returns:
            检测到的威胁列表
        """
        if not self._config.detect_prompt_injection:
            return []

        threats = []
        text_lower = text.lower()

        for pattern, injection_type in self.INJECTION_PATTERNS:
            matches = re.findall(pattern, text_lower, re.IGNORECASE)
            for match in matches:
                threats.append(ThreatDetection(
                    threat_type=ThreatType.PROMPT_INJECTION,
                    severity=0.8,
                    description=f"检测到提示注入: {injection_type}",
                    location=match if isinstance(match, str) else str(match),
                    blocked=self._config.block_injection,
                ))

        return threats

    def check_jailbreak(self, text: str) -> list[ThreatDetection]:
        """检测越狱攻击.

        Args:
            text: 输入文本

        Returns:
            检测到的威胁列表
        """
        threats = []

        # 越狱关键词
        jailbreak_keywords = [
            "DAN", "jailbreak", "bypass", "hack",
            "developer mode", "unrestricted",
            "无限制", "越狱", "绕过限制",
        ]

        text_lower = text.lower()
        for keyword in jailbreak_keywords:
            if keyword.lower() in text_lower:
                threats.append(ThreatDetection(
                    threat_type=ThreatType.JAILBREAK,
                    severity=0.9,
                    description=f"检测到越狱关键词: {keyword}",
                    location=keyword,
                    blocked=self._config.strict_mode,
                ))

        return threats

    def sanitize_pii(self, text: str) -> SanitizationResult:
        """脱敏个人识别信息.

        Args:
            text: 输入文本

        Returns:
            SanitizationResult
        """
        result = SanitizationResult(
            original_text=text,
            sanitized_text=text,
        )

        if not self._config.detect_pii:
            return result

        sanitized = text
        detections = []

        for pii_type, pattern in self.PII_PATTERNS.items():
            matches = re.findall(pattern, text)
            for match in matches:
                detections.append({
                    "type": pii_type.value,
                    "value": match[:4] + "..." if len(match) > 4 else "***",
                })

                if self._config.mask_pii:
                    # 根据类型选择遮盖方式
                    if pii_type == PIIType.EMAIL:
                        parts = match.split("@")
                        masked = parts[0][:2] + "***@" + parts[1] if len(parts) == 2 else "***"
                    elif pii_type == PIIType.PHONE:
                        masked = match[:3] + "****" + match[-4:]
                    elif pii_type == PIIType.CREDIT_CARD:
                        masked = match[:4] + " **** **** " + match[-4:]
                    else:
                        masked = match[:2] + "*" * (len(match) - 4) + match[-2:]

                    sanitized = sanitized.replace(match, masked)

        result.sanitized_text = sanitized
        result.detections = detections
        result.is_safe = len(detections) == 0

        return result

    def sanitize_api_keys(self, text: str) -> SanitizationResult:
        """脱敏API密钥.

        Args:
            text: 输入文本

        Returns:
            SanitizationResult
        """
        result = SanitizationResult(
            original_text=text,
            sanitized_text=text,
        )

        if not self._config.detect_api_keys:
            return result

        sanitized = text
        detections = []

        for pattern, key_type in self.API_KEY_PATTERNS:
            matches = re.findall(pattern, text)
            for match in matches:
                detections.append({
                    "type": "api_key",
                    "provider": key_type,
                    "value": match[:8] + "...",
                })

                # 始终遮盖API密钥
                masked = match[:8] + "*" * (len(match) - 8)
                sanitized = sanitized.replace(match, masked)

        result.sanitized_text = sanitized
        result.detections = detections
        result.is_safe = len(detections) == 0

        return result

    def sanitize(self, text: str) -> SanitizationResult:
        """综合脱敏处理.

        Args:
            text: 输入文本

        Returns:
            SanitizationResult
        """
        # 先检测提示注入
        injection_threats = self.check_prompt_injection(text)
        jailbreak_threats = self.check_jailbreak(text)

        all_threats = injection_threats + jailbreak_threats

        if all_threats and self._config.block_injection:
            return SanitizationResult(
                original_text=text,
                sanitized_text="[内容已屏蔽：检测到安全威胁]",
                detections=[t.to_dict() for t in all_threats],
                is_safe=False,
            )

        # PII脱敏
        result = self.sanitize_pii(text)

        # API密钥脱敏
        api_result = self.sanitize_api_keys(result.sanitized_text)

        # 合并结果
        result.sanitized_text = api_result.sanitized_text
        result.detections.extend(api_result.detections)
        result.is_safe = result.is_safe and api_result.is_safe

        # 自定义敏感词
        for word in self._config.custom_sensitive_words:
            if word in result.sanitized_text:
                result.sanitized_text = result.sanitized_text.replace(word, "***")
                result.detections.append({"type": "custom", "value": word[:2] + "..."})

        return result

    def audit_output(
        self,
        output: str,
        context: str | None = None,
    ) -> dict[str, Any]:
        """审计LLM输出.

        Args:
            output: LLM输出
            context: 上下文（用于检测数据泄露）

        Returns:
            审计结果
        """
        result = {
            "timestamp": datetime.now().isoformat(),
            "output_length": len(output),
            "threats": [],
            "pii_detected": [],
            "is_safe": True,
        }

        # 检测输出中的PII
        pii_result = self.sanitize_pii(output)
        if not pii_result.is_safe:
            result["pii_detected"] = pii_result.detections
            result["is_safe"] = False

        # 检测API密钥泄露
        api_result = self.sanitize_api_keys(output)
        if not api_result.is_safe:
            result["threats"].append({
                "type": "api_key_leak",
                "severity": "critical",
                "details": api_result.detections,
            })
            result["is_safe"] = False

        # 如果有上下文，检测潜在的数据泄露
        if context:
            # 检查输出中是否包含上下文中的敏感信息
            context_pii = self.sanitize_pii(context)
            for detection in context_pii.detections:
                pii_value = detection.get("value", "")
                if pii_value and pii_value in output:
                    result["threats"].append({
                        "type": "data_exfiltration",
                        "severity": "high",
                        "description": "输出可能包含上下文中的敏感信息",
                    })
                    result["is_safe"] = False

        return result

    def add_sensitive_word(self, word: str) -> None:
        """添加自定义敏感词.

        Args:
            word: 敏感词
        """
        if word not in self._config.custom_sensitive_words:
            self._config.custom_sensitive_words.append(word)

    def add_injection_pattern(self, pattern: str, injection_type: str) -> None:
        """添加自定义注入模式.

        Args:
            pattern: 正则表达式模式
            injection_type: 注入类型标识
        """
        self.INJECTION_PATTERNS.append((pattern, injection_type))

