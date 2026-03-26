"""App コンプライアンスチェッカーのテスト."""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
SCRIPT = ROOT / "scripts" / "check_app_compliance.py"


def test_app_compliance_script_runs_without_crash() -> None:
    """スクリプトがクラッシュせずに実行できること."""
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--json"],
        capture_output=True,
        text=True,
        cwd=str(ROOT),
    )
    assert result.returncode in (0, 1), f"Unexpected exit: {result.stderr}"


def test_app_compliance_known_good_apps() -> None:
    """成熟した app は全項目パスすること."""
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--json", "--app", "faq_system"],
        capture_output=True,
        text=True,
        cwd=str(ROOT),
    )
    import json

    report = json.loads(result.stdout)
    faq = report["apps"]["faq_system"]
    assert faq["app_config_exists"] is True
    assert faq["has_entry_point"] is True
    assert faq["has_tests"] is True


def test_app_compliance_market_trend_monitor_uses_configured_entry_point() -> None:
    """app_config.json の api_module から入口を解決できること."""
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--json", "--app", "market_trend_monitor"],
        capture_output=True,
        text=True,
        cwd=str(ROOT),
    )
    import json

    report = json.loads(result.stdout)
    market_trend = report["apps"]["market_trend_monitor"]
    assert market_trend["app_config_exists"] is True
    assert market_trend["has_entry_point"] is True


def test_app_compliance_removed_legacy_auth_service_is_not_reported() -> None:
    """削除済み legacy app は結果一覧に現れないこと."""
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--json"],
        capture_output=True,
        text=True,
        cwd=str(ROOT),
    )
    import json

    report = json.loads(result.stdout)
    assert "auth_service" not in report["apps"]
