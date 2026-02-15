#!/bin/bash

# Determine the project root (2 levels up from this script)
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
PROJECT_ROOT="$(dirname "$(dirname "$SCRIPT_DIR")")"
APP_CONFIG="$SCRIPT_DIR/app_config.json"

API_PORT=$(python - "$APP_CONFIG" <<'PY'
import json
import sys
from pathlib import Path

config_path = Path(sys.argv[1])
port = 8003
if config_path.is_file():
    try:
        data = json.loads(config_path.read_text("utf-8"))
        port = int(data.get("ports", {}).get("api", port))
    except Exception:
        pass
print(port)
PY
)

export PYTHONPATH=$PYTHONPATH:$PROJECT_ROOT

echo "Starting Code Migration Assistant Server..."
echo "Project Root: $PROJECT_ROOT"
echo "Access the dashboard at http://localhost:${API_PORT}"

# Run from project root to ensure module imports work
cd "$PROJECT_ROOT"
exec uvicorn apps.code_migration_assistant.backend.app:app --host 0.0.0.0 --port "$API_PORT" --reload
