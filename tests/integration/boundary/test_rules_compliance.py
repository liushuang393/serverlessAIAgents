"""ルールコンプライアンス総合チェッカーのテスト."""
from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
SCRIPT = ROOT / "scripts" / "check_rules_compliance.py"


def test_rules_compliance_script_runs() -> None:
    """スクリプトがクラッシュせず実行できること."""
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--json"],
        capture_output=True,
        text=True,
        cwd=str(ROOT),
    )
    assert result.returncode in (0, 1), f"Unexpected: {result.stderr}"


def test_rules_compliance_has_all_categories() -> None:
    """レポートに全カテゴリが含まれること."""
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--json"],
        capture_output=True,
        text=True,
        cwd=str(ROOT),
    )
    report = json.loads(result.stdout)
    expected_keys = {
        "layer_boundary_violations",
        "provider_direct_imports",
        "file_size_violations",
        "type_ignore_without_reason",
        "bare_any_usage",
        "cast_usage",
        "total_files_scanned",
    }
    assert expected_keys.issubset(set(report.keys()))
