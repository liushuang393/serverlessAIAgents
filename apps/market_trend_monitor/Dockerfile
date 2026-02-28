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
COPY agentflow /app/agentflow
COPY apps/market_trend_monitor /app/apps/market_trend_monitor

RUN pip install --no-cache-dir -e ".[apps]" \
    && pip install --no-cache-dir -r /app/apps/market_trend_monitor/backend/requirements.txt

EXPOSE 8002

HEALTHCHECK --interval=30s --timeout=10s --start-period=20s --retries=3 \
  CMD python -c "import urllib.request; urllib.request.urlopen('http://localhost:8002/health')" || exit 1

CMD ["python", "-m", "apps.market_trend_monitor.backend.api.main"]
