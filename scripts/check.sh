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

# プロジェクトルートを PYTHONPATH に追加（agentflow shim パッケージ解決用）
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

# --- 3. 旧パス互換 shim 確認 ---
echo "[3/3] 旧パス互換 shim インポート確認"
SHIM_FAIL=0
"$PYTHON" -c "
# kernel shims
from agentflow.orchestration.executor import ExecutorAgent
from agentflow.orchestration.planner import PlannerAgent
from agentflow.engines.report_builder import ReportBuilder
from agentflow.pev.result_verifier import ResultVerifier
from agentflow.routing.intent_router import IntentRouter

# harness shims
from agentflow.context import ContextEngineer
from agentflow.context.budget_manager import TokenBudgetManager
from agentflow.governance.engine import GovernanceEngine

# control plane
from control_plane.registry import AppRegistryService
from control_plane.lifecycle import LifecycleService
from control_plane.discovery import DiscoveryService

# identity checks
from kernel import ExecutorAgent as EA2
assert EA2 is ExecutorAgent, 'ExecutorAgent identity mismatch'
from harness.context import ContextEngineer as CE2
assert CE2 is ContextEngineer, 'ContextEngineer identity mismatch'
from harness.guardrails import GovernanceEngine as GE2
assert GE2 is GovernanceEngine, 'GovernanceEngine identity mismatch'

print('All shim imports and identity checks passed.')
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
