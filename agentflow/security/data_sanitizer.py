# -*- coding: utf-8 -*-
"""データマスカー（Data Sanitizer）- AI セキュリティとプライバシー保護.

LLM アプリにおけるセキュリティ/プライバシー課題に対応:
- プロンプトインジェクションの検知・防御
- 機微データのマスキング
- 権限境界の制御
- 出力監査

参考:
- Microsoft Copilot Security Analysis (2024)
- Prompt Injection Attacks (2023)
- LLM Application Security Best Practices (2024)

使用例:
    >>> from agentflow.security.data_sanitizer import DataSanitizer
    >>>
    >>> sanitizer = DataSanitizer()
    >>>
    >>> # プロンプトインジェクションを検知
    >>> threats = sanitizer.check_prompt_injection(user_input)
    >>> if threats:
    ...     print("脅威を検知")
    >>>
    >>> # マスキング処理
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
    """脅威タイプ."""
    
    PROMPT_INJECTION = "prompt_injection"  # プロンプトインジェクション
    DATA_EXFILTRATION = "data_exfiltration"  # データ持ち出し
    PRIVILEGE_ESCALATION = "privilege_escalation"  # 権限昇格
    JAILBREAK = "jailbreak"  # 脱獄（制限回避）
    PII_EXPOSURE = "pii_exposure"  # PII 露出


class PIIType(str, Enum):
    """PII（個人識別情報）の種別."""
    
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
    """脅威検知結果.
    
    Attributes:
        threat_type: 脅威タイプ
        severity: 深刻度（0.0-1.0）
        description: 説明
        location: 位置
        blocked: ブロックしたか
    """
    
    threat_type: ThreatType
    severity: float
    description: str
    location: str = ""
    blocked: bool = False
    
    def to_dict(self) -> dict[str, Any]:
        """dict に変換する."""
        return {
            "type": self.threat_type.value,
            "severity": self.severity,
            "description": self.description,
            "location": self.location,
            "blocked": self.blocked,
        }


@dataclass
class SanitizationResult:
    """マスキング結果.
    
    Attributes:
        original_text: 元のテキスト
        sanitized_text: マスキング後のテキスト
        detections: 検知されたセンシティブ情報
        is_safe: 安全と判断できるか
    """
    
    original_text: str
    sanitized_text: str
    detections: list[dict[str, Any]] = field(default_factory=list)
    is_safe: bool = True
    processed_at: datetime = field(default_factory=datetime.now)
    
    def to_dict(self) -> dict[str, Any]:
        """dict に変換する."""
        return {
            "sanitized_text": self.sanitized_text,
            "is_safe": self.is_safe,
            "detections_count": len(self.detections),
            "processed_at": self.processed_at.isoformat(),
        }


class SanitizerConfig(BaseModel):
    """マスキング設定."""

    # 各種検知を有効化するか
    detect_prompt_injection: bool = Field(default=True)
    detect_pii: bool = Field(default=True)
    detect_api_keys: bool = Field(default=True)

    # マスキング動作
    mask_pii: bool = Field(default=True)
    block_injection: bool = Field(default=True)

    # カスタムのセンシティブワード
    custom_sensitive_words: list[str] = Field(default_factory=list)

    # ホワイトリスト（マスキングしないドメイン）
    whitelist_domains: list[str] = Field(default_factory=list)

    # 厳格モード
    strict_mode: bool = Field(default=False)


class DataSanitizer:
    """データマスカー.

    AI アプリをセキュリティ脅威から守り、データプライバシーを保護する。

    主な機能:
    1. プロンプトインジェクション検知: 悪意のある指示上書きを検知
    2. PII マスキング: 個人識別情報の検知と遮蔽
    3. API キー検知: API キー漏洩の検知と遮蔽
    4. 出力監査: LLM 出力に含まれるセンシティブ情報を監査

    Example:
        >>> sanitizer = DataSanitizer()
        >>>
        >>> # ユーザー入力をチェック
        >>> threats = sanitizer.check_prompt_injection(user_input)
        >>> if threats:
        ...     print("脅威を検知")
        >>>
        >>> # マスキング処理
        >>> result = sanitizer.sanitize(text)
    """

    # プロンプトインジェクションのパターン
    INJECTION_PATTERNS = [
        # 直接的な指示の上書き
        (r"(?:前|上)の.{0,10}(?:指示|命令)を.{0,10}無視", "instruction_override"),
        (r"ignore.{0,20}instructions?", "instruction_override"),
        (r"disregard.{0,20}(previous|above)", "instruction_override"),
        # 役割の乗っ取り（なりすまし）
        (r"(?:あなた|君)は(?:今から|いま).{0,10}(?:として|になって|で)", "role_hijack"),
        (r"pretend.{0,10}(you|to).{0,10}(are|be)", "role_hijack"),
        (r"act as.{0,10}(a|an|the)", "role_hijack"),
        # システムプロンプトの開示要求
        (r"(?:システム|初期).{0,10}プロンプトを.{0,10}(?:表示|見せ|開示)", "system_leak"),
        (r"(show|reveal|print).{0,10}(system|initial).{0,10}prompt", "system_leak"),
        # 脱獄
        (r"DAN.{0,10}mode", "jailbreak"),
        (r"developer.{0,10}mode", "jailbreak"),
    ]

    # PII パターン
    PII_PATTERNS = {
        PIIType.EMAIL: r"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}",
        PIIType.PHONE: r"(?:\+?86)?1[3-9]\d{9}|(?:\d{3,4}-?)?\d{7,8}",
        PIIType.ID_CARD: r"\d{17}[\dXx]|\d{15}",
        PIIType.CREDIT_CARD: r"\d{4}[- ]?\d{4}[- ]?\d{4}[- ]?\d{4}",
        PIIType.IP_ADDRESS: r"\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}",
    }

    # API キーのパターン
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
        """初期化.

        Args:
            config: マスキング設定
        """
        self._config = config or SanitizerConfig()
        self._logger = logging.getLogger(__name__)

    def check_prompt_injection(
        self,
        text: str,
    ) -> list[ThreatDetection]:
        """プロンプトインジェクションを検知する.

        Args:
            text: 入力テキスト

        Returns:
            検知した脅威の一覧
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
                    description=f"プロンプトインジェクションを検知: {injection_type}",
                    location=match if isinstance(match, str) else str(match),
                    blocked=self._config.block_injection,
                ))

        return threats

    def check_jailbreak(self, text: str) -> list[ThreatDetection]:
        """脱獄（制限回避）を検知する.

        Args:
            text: 入力テキスト

        Returns:
            検知した脅威の一覧
        """
        threats = []

        # 脱獄キーワード
        jailbreak_keywords = [
            "DAN", "jailbreak", "bypass", "hack",
            "developer mode", "unrestricted",
            "無制限", "脱獄", "制限回避", "制限を回避",
        ]

        text_lower = text.lower()
        for keyword in jailbreak_keywords:
            if keyword.lower() in text_lower:
                threats.append(ThreatDetection(
                    threat_type=ThreatType.JAILBREAK,
                    severity=0.9,
                    description=f"脱獄キーワードを検知: {keyword}",
                    location=keyword,
                    blocked=self._config.strict_mode,
                ))

        return threats

    def sanitize_pii(self, text: str) -> SanitizationResult:
        """PII（個人識別情報）をマスキングする.

        Args:
            text: 入力テキスト

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
                    # 種別に応じてマスク方法を選択
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
        """API キーをマスキングする.

        Args:
            text: 入力テキスト

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

                # API キーは常にマスクする
                masked = match[:8] + "*" * (len(match) - 8)
                sanitized = sanitized.replace(match, masked)

        result.sanitized_text = sanitized
        result.detections = detections
        result.is_safe = len(detections) == 0

        return result

    def sanitize(self, text: str) -> SanitizationResult:
        """総合マスキング処理.

        Args:
            text: 入力テキスト

        Returns:
            SanitizationResult
        """
        # 先にプロンプトインジェクションを検知
        injection_threats = self.check_prompt_injection(text)
        jailbreak_threats = self.check_jailbreak(text)

        all_threats = injection_threats + jailbreak_threats

        if all_threats and self._config.block_injection:
            return SanitizationResult(
                original_text=text,
                sanitized_text="[内容をブロックしました: セキュリティ脅威を検知]",
                detections=[t.to_dict() for t in all_threats],
                is_safe=False,
            )

        # PII マスキング
        result = self.sanitize_pii(text)

        # API キー マスキング
        api_result = self.sanitize_api_keys(result.sanitized_text)

        # 結果をマージ
        result.sanitized_text = api_result.sanitized_text
        result.detections.extend(api_result.detections)
        result.is_safe = result.is_safe and api_result.is_safe

        # カスタムセンシティブワード
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
        """LLM 出力を監査する.

        Args:
            output: LLM 出力
            context: コンテキスト（データ持ち出し検知用）

        Returns:
            監査結果
        """
        result = {
            "timestamp": datetime.now().isoformat(),
            "output_length": len(output),
            "threats": [],
            "pii_detected": [],
            "is_safe": True,
        }

        # 出力中の PII を検知
        pii_result = self.sanitize_pii(output)
        if not pii_result.is_safe:
            result["pii_detected"] = pii_result.detections
            result["is_safe"] = False

        # API キー漏洩を検知
        api_result = self.sanitize_api_keys(output)
        if not api_result.is_safe:
            result["threats"].append({
                "type": "api_key_leak",
                "severity": "critical",
                "details": api_result.detections,
            })
            result["is_safe"] = False

        # context がある場合、潜在的なデータ持ち出しを検知
        if context:
            # 出力に context 内のセンシティブ情報が含まれていないか確認
            context_pii = self.sanitize_pii(context)
            for detection in context_pii.detections:
                pii_value = detection.get("value", "")
                if pii_value and pii_value in output:
                    result["threats"].append({
                        "type": "data_exfiltration",
                        "severity": "high",
                        "description": "出力がコンテキスト内のセンシティブ情報を含む可能性があります",
                    })
                    result["is_safe"] = False

        return result

    def add_sensitive_word(self, word: str) -> None:
        """カスタムセンシティブワードを追加する.

        Args:
            word: センシティブワード
        """
        if word not in self._config.custom_sensitive_words:
            self._config.custom_sensitive_words.append(word)

    def add_injection_pattern(self, pattern: str, injection_type: str) -> None:
        """カスタムのインジェクションパターンを追加する.

        Args:
            pattern: 正規表現パターン
            injection_type: インジェクション種別識別子
        """
        self.INJECTION_PATTERNS.append((pattern, injection_type))
