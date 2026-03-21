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


def test_app_compliance_known_bad_apps() -> None:
    """構造不足の app が正しく非準拠と判定されること."""
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--json", "--app", "auth_service"],
        capture_output=True,
        text=True,
        cwd=str(ROOT),
    )
    import json

    report = json.loads(result.stdout)
    auth = report["apps"]["auth_service"]
    assert auth["compliant"] is False
    assert len(auth["issues"]) > 0
