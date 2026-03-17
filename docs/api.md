# API Overview

## Canonical Routes

- `/api/studios/*`
- `/api/studios/framework/apps/*`

## Control Plane Entrypoint

- Python module: `control_plane.main`
- FastAPI app: `control_plane.main:app`

## Notes

- 旧 `/api/apps/*` は正規経路ではない
- framework app 管理面は `/api/studios/framework/apps/*` を使う
- control-plane の実装パスは `control_plane/` が正本
