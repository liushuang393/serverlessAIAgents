FROM node:20-slim AS frontend-builder

WORKDIR /frontend
COPY apps/legacy_modernization_geo_platform/frontend/package*.json ./
RUN npm ci
COPY apps/legacy_modernization_geo_platform/frontend/ ./
RUN npm run build

FROM python:3.13-slim AS python-deps

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PIP_NO_CACHE_DIR=1 \
    PIP_DISABLE_PIP_VERSION_CHECK=1

WORKDIR /app
RUN apt-get update && apt-get install -y --no-install-recommends build-essential && rm -rf /var/lib/apt/lists/*
COPY pyproject.toml README.md ./
COPY agentflow/ /app/agentflow/
RUN pip install --no-cache-dir -e ".[apps]"

FROM python:3.13-slim AS runtime

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PYTHONPATH=/app

WORKDIR /app
RUN groupadd -r appgroup && useradd -r -g appgroup appuser
COPY --from=python-deps /usr/local/lib/python3.13/site-packages /usr/local/lib/python3.13/site-packages
COPY --from=python-deps /usr/local/bin /usr/local/bin
COPY agentflow/ /app/agentflow/
COPY apps/legacy_modernization_geo_platform/ /app/apps/legacy_modernization_geo_platform/
COPY --from=frontend-builder /frontend/dist /app/apps/legacy_modernization_geo_platform/frontend/dist
RUN chown -R appuser:appgroup /app
USER appuser
EXPOSE 8100
HEALTHCHECK --interval=30s --timeout=10s --start-period=10s --retries=3 \
  CMD python -c "import urllib.request; urllib.request.urlopen('http://localhost:8100/api/health')" || exit 1
CMD ["python", "-m", "apps.legacy_modernization_geo_platform.main"]
