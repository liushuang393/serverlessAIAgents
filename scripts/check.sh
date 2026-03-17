#!/usr/bin/env bash
# 7コア層 + apps 外層の境界チェック統合スクリプト.
#
# 実行方法:
#   bash scripts/check.sh
#
# 終了コード:
#   0 = 全チェック合格
#   1 = いずれかのチェックで違反検出
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
PYTHON="${PYTHON:-python3}"

# プロジェクトルートを PYTHONPATH に追加（分層パッケージ解決用）
export PYTHONPATH="${ROOT_DIR}:${PYTHONPATH:-}"

EXIT_CODE=0

echo "============================================"
echo " 7コア層アーキテクチャ境界チェック"
echo "============================================"
echo ""

# --- 1. レイヤー境界チェック ---
echo "[1/3] レイヤー境界チェック (check_layer_boundaries.py)"
if "$PYTHON" "$SCRIPT_DIR/check_layer_boundaries.py"; then
    echo "  -> PASS"
else
    echo "  -> FAIL"
    EXIT_CODE=1
fi
echo ""

# --- 2. プロバイダ直接呼び出し禁止チェック ---
echo "[2/3] プロバイダ直接呼び出し禁止チェック (check_no_direct_provider_calls.py)"
if "$PYTHON" "$SCRIPT_DIR/check_no_direct_provider_calls.py"; then
    echo "  -> PASS"
else
    echo "  -> FAIL"
    EXIT_CODE=1
fi
echo ""

# --- 3. 正規公開入口確認 ---
echo "[3/3] 正規公開入口インポート確認"
SHIM_FAIL=0
"$PYTHON" -c "
from apps.faq_system.backend.agents.faq_agent import FAQAgent
from shared.agents.sales_agent import SalesAgent
from kernel.skills.builtin.design_skills.engine import DesignSkillsEngine
from control_plane import AppRegistryService, DiscoveryService, LifecycleService

assert FAQAgent.__name__ == 'FAQAgent'
assert SalesAgent.__name__ == 'SalesAgent'
assert DesignSkillsEngine.__name__ == 'DesignSkillsEngine'
assert AppRegistryService.__name__ == 'AppRegistryService'
assert DiscoveryService.__name__ == 'DiscoveryService'
assert LifecycleService.__name__ == 'LifecycleService'

print('All canonical entry-point imports passed.')
" || SHIM_FAIL=1

if [ "$SHIM_FAIL" -eq 0 ]; then
    echo "  -> PASS"
else
    echo "  -> FAIL"
    EXIT_CODE=1
fi
echo ""

# --- 結果サマリ ---
echo "============================================"
if [ "$EXIT_CODE" -eq 0 ]; then
    echo " 結果: 全チェック合格"
else
    echo " 結果: 違反あり (終了コード=$EXIT_CODE)"
fi
echo "============================================"

exit $EXIT_CODE
