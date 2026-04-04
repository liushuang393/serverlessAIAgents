"""YAMLファイルからポリシーを読み込むローダー.

PolicyEngine が受け付ける Policy オブジェクトを
YAML 定義から生成するユーティリティ。

Example YAML:
    policies:
      - id: "deny-guest-write"
        name: "ゲストの書き込み禁止"
        priority: 100
        effect: "deny"
        subject_conditions:
          role: "guest"
        action_conditions:
          - "write"
          - "delete"

Example:
    >>> loader = PolicyLoader()
    >>> policies = loader.load_from_file("policies.yaml")
    >>> for p in policies:
    ...     engine.add_policy(p)
"""

from __future__ import annotations

import logging
from pathlib import Path
from typing import Any

import yaml

from harness.security.policy_engine import AuthDecision, Policy

logger = logging.getLogger(__name__)

# effect 文字列と AuthDecision のマッピング
_EFFECT_MAP: dict[str, AuthDecision] = {
    "allow": AuthDecision.ALLOW,
    "deny": AuthDecision.DENY,
    "not_applicable": AuthDecision.NOT_APPLICABLE,
}


class PolicyLoadError(Exception):
    """ポリシー読み込み時のエラー."""


class PolicyLoader:
    """YAMLファイルからポリシーを読み込むローダー.

    YAML 定義を Policy dataclass に変換し、
    PolicyEngine に登録可能な形式で返す。
    """

    def load_from_file(self, path: str | Path) -> list[Policy]:
        """ファイルパスからポリシーを読み込む.

        Args:
            path: YAMLファイルのパス

        Returns:
            読み込まれた Policy オブジェクトのリスト

        Raises:
            PolicyLoadError: ファイル読み込みまたはパースに失敗した場合
            FileNotFoundError: ファイルが存在しない場合
        """
        file_path = Path(path)
        if not file_path.exists():
            raise FileNotFoundError(f"ポリシーファイルが見つかりません: {file_path}")

        try:
            content = file_path.read_text(encoding="utf-8")
        except OSError as e:
            raise PolicyLoadError(f"ファイル読み込みエラー: {e}") from e

        return self.load_from_string(content)

    def load_from_string(self, yaml_str: str) -> list[Policy]:
        """YAML文字列からポリシーを読み込む.

        Args:
            yaml_str: YAML形式のポリシー定義文字列

        Returns:
            読み込まれた Policy オブジェクトのリスト

        Raises:
            PolicyLoadError: YAMLパースまたはポリシー変換に失敗した場合
        """
        try:
            data = yaml.safe_load(yaml_str)
        except yaml.YAMLError as e:
            raise PolicyLoadError(f"YAMLパースエラー: {e}") from e

        if data is None:
            return []

        if not isinstance(data, dict):
            raise PolicyLoadError("YAMLのトップレベルは辞書である必要があります")

        raw_policies = data.get("policies")
        if raw_policies is None:
            return []

        if not isinstance(raw_policies, list):
            raise PolicyLoadError("'policies' キーはリストである必要があります")

        policies: list[Policy] = []
        for i, raw in enumerate(raw_policies):
            try:
                policy = self._parse_policy(raw)
                policies.append(policy)
            except (KeyError, ValueError, TypeError) as e:
                raise PolicyLoadError(
                    f"ポリシー定義 #{i} の変換エラー: {e}"
                ) from e

        logger.info("YAMLから %d 件のポリシーを読み込みました", len(policies))
        return policies

    def _parse_policy(self, raw: dict[str, Any]) -> Policy:
        """辞書から Policy オブジェクトを生成する.

        Args:
            raw: YAML由来の辞書データ

        Returns:
            変換された Policy オブジェクト

        Raises:
            KeyError: 必須フィールドが不足している場合
            ValueError: effect の値が不正な場合
        """
        # 必須フィールドの検証
        policy_id = raw["id"]
        name = raw["name"]

        # effect の変換
        effect_str = str(raw.get("effect", "allow")).lower()
        effect = _EFFECT_MAP.get(effect_str)
        if effect is None:
            valid = ", ".join(_EFFECT_MAP.keys())
            raise ValueError(
                f"不正な effect 値: '{effect_str}'（有効値: {valid}）"
            )

        return Policy(
            policy_id=str(policy_id),
            name=str(name),
            description=str(raw.get("description", "")),
            priority=int(raw.get("priority", 0)),
            effect=effect,
            subject_conditions=raw.get("subject_conditions", {}),
            resource_conditions=raw.get("resource_conditions", {}),
            action_conditions=raw.get("action_conditions", []),
            environment_conditions=raw.get("environment_conditions", {}),
        )
