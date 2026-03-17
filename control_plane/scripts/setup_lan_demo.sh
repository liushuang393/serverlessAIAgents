#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PS_SCRIPT="${SCRIPT_DIR}/setup_lan_demo.ps1"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"

HOST_IP=""
MODE="firewall-only"
DRY_RUN=0
ALL_APPS=0
APPS=("platform" "auth_service" "decision_governance_engine" "faq_system" "market_trend_monitor" "messaging_hub")

while (($# > 0)); do
  case "$1" in
    -HostIP)
      HOST_IP="${2:-}"
      shift 2
      ;;
    -Mode)
      MODE="${2:-auto}"
      shift 2
      ;;
    -DryRun)
      DRY_RUN=1
      shift
      ;;
    -AllApps)
      ALL_APPS=1
      shift
      ;;
    -Apps)
      IFS=',' read -r -a APPS <<< "${2:-}"
      shift 2
      ;;
    *)
      echo "Unsupported argument: $1" >&2
      exit 1
      ;;
  esac
done

if ! command -v powershell.exe >/dev/null 2>&1; then
  echo "powershell.exe not found. Run the PowerShell script directly on Windows:" >&2
  echo "  ${PS_SCRIPT}" >&2
  exit 1
fi

PORTS="$(
  python3 - "$REPO_ROOT" "$ALL_APPS" "$(IFS=,; echo "${APPS[*]}")" <<'PY'
import json
import pathlib
import sys

repo_root = pathlib.Path(sys.argv[1])
include_all = sys.argv[2] == "1"
selected = {item.strip() for item in sys.argv[3].split(",") if item.strip()}
ports: set[int] = set()

for config_path in repo_root.glob("apps/*/app_config.json"):
    app_name = config_path.parent.name
    if not include_all and app_name not in selected:
        continue
    data = json.loads(config_path.read_text(encoding="utf-8"))
    for key in ("api", "frontend"):
        value = data.get("ports", {}).get(key)
        if isinstance(value, int) and value > 0:
            ports.add(value)

print(",".join(str(port) for port in sorted(ports)))
PY
)"

if [[ -z "${PORTS}" ]]; then
  echo "No ports resolved from app_config.json" >&2
  exit 1
fi

CMD=(powershell.exe -ExecutionPolicy Bypass -File "$(wslpath -w "${PS_SCRIPT}")" -PortsCsv "$PORTS" -Mode "$MODE")
if [[ -n "${HOST_IP}" ]]; then
  CMD+=(-HostIP "$HOST_IP")
fi
if [[ "${DRY_RUN}" == "1" ]]; then
  CMD+=(-DryRun)
fi

"${CMD[@]}"
