# API Overview

## Canonical Routes

- `/api/studios/*`
- `/api/studios/framework/*`

## Control Plane Entrypoint

- Python module: `control_plane.main`
- FastAPI app: `control_plane.main:app`

## Notes

- 旧 `/api/apps/*` は正規経路ではない
- control-plane の実装パスは `control_plane/` が正本
