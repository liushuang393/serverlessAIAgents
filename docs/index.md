# BizCore Docs

BizCore の現行ドキュメント入口です。

## 構成

- [対外ドキュメント](external/README.md)
- [対内ドキュメント](internal/README.md)
- [Studios 概要](studios.md)
- [API 概要](api.md)

## アーキテクチャ基準

- 7コア層: `contracts / infrastructure / shared / kernel / harness / control_plane / domain`
- 製品層: `apps/`
- `domain` は `control_plane` に依存しない
- `contracts / infrastructure / shared / kernel / harness / domain` は `apps/` に依存しない
