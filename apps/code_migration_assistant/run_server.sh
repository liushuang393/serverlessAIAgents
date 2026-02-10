#!/bin/bash

# Determine the project root (2 levels up from this script)
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
PROJECT_ROOT="$(dirname "$(dirname "$SCRIPT_DIR")")"

export PYTHONPATH=$PYTHONPATH:$PROJECT_ROOT

echo "Starting Code Migration Assistant Server..."
echo "Project Root: $PROJECT_ROOT"
echo "Access the dashboard at http://localhost:8003"

# Run from project root to ensure module imports work
cd "$PROJECT_ROOT"
exec uvicorn apps.code_migration_assistant.backend.app:app --host 0.0.0.0 --port 8003 --reload
