# .github/workflows/

## Overview
- CI/CD workflows, including reusable deploy workflows for multiple platforms.

## Structure
```
.github/workflows/
├── lint.yml
├── test.yml
├── publish.yml
├── release.yml
└── reusable-deploy-*.yml
```

## Where To Look
| Need | Location | Notes |
|------|----------|-------|
| Linting | `.github/workflows/lint.yml` | Ruff + mypy checks.
| Tests | `.github/workflows/test.yml` | Cross-platform tests.
| Release automation | `.github/workflows/release.yml` | Versioning + changelog + release.
| Publish to PyPI | `.github/workflows/publish.yml` | Build + upload.
| Reusable Python CI | `.github/workflows/reusable-python-ci.yml` | Shared lint/test workflow.
| Reusable deploy templates | `.github/workflows/reusable-deploy-*.yml` | AWS Lambda, Vercel, GCP, Azure, etc.

## Notes
- These workflows correspond closely to deploy targets in `agentflow/deploy/targets/`.

## Anti-Patterns
- Editing reusable deploy workflows without updating the corresponding deploy target implementation.
