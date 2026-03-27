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
COPY apps/messaging_hub /app/apps/messaging_hub

RUN pip install --no-cache-dir -e ".[apps]"

CMD ["python", "-m", "kernel.runtime.uvicorn_launcher", "--config", "apps/messaging_hub/app_config.json", "--app", "apps.messaging_hub.main:app", "--host-env", "MSGHUB_HOST", "--port-env", "MSGHUB_PORT"]
