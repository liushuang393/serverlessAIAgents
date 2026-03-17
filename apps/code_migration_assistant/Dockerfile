FROM python:3.13-slim

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PIP_NO_CACHE_DIR=1 \
    PIP_DISABLE_PIP_VERSION_CHECK=1 \
    PYTHONPATH=/app

WORKDIR /app

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

COPY pyproject.toml README.md /app/
COPY contracts /app/contracts
COPY infrastructure /app/infrastructure
COPY shared /app/shared
COPY kernel /app/kernel
COPY harness /app/harness
COPY control_plane /app/control_plane
COPY domain /app/domain
COPY apps/code_migration_assistant /app/apps/code_migration_assistant

RUN pip install --no-cache-dir -e ".[apps]"

EXPOSE 8003

HEALTHCHECK --interval=30s --timeout=10s --start-period=15s --retries=3 \
  CMD python -c "import urllib.request; urllib.request.urlopen('http://localhost:8003/api/health')" || exit 1

CMD ["python", "-m", "apps.code_migration_assistant.backend.app"]
