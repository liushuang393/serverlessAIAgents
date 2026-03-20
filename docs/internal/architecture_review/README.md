# Architecture Review Pack

七层核心架构与代表应用的只读架构评审交付物。

## Deliverables

- [Baseline](architecture_review_baseline.md)
- [Boundary Review](review_boundary.md)
- [Tech-Fit Review](review_tech_fit.md)
- [Plugin-Modularity Review](review_plugin_modularity.md)
- [Orchestration Review](review_orchestration.md)
- [Contrarian Review](review_contrarian.md)
- [Summary](architecture_review_summary.md)

## Scope

- 核心层: `contracts / infrastructure / shared / kernel / harness / control_plane / domain`
- 产品层: `apps/`
- 代表应用: `code_migration_assistant`、`messaging_hub`、`faq_system`

## Review Rules

- 不评业务正确性，只评层次、职责、依赖方向、模式适配、扩展方式。
- 文档只作对照，事实以代码、目录、入口和 import 扫描为准。
- 本轮不修改 public API、schema 或 manifest 契约。

## 2026-03-20 Update

- 已追加横向深评，补入跨层依赖矩阵、manifest 完整性矩阵、plugin/skill/provider 对照和热点大文件 inventory。
- 已按 review 结果实施第一轮全仓整改，重点覆盖 runtime bootstrap、websocket hub、publish service、plugin registry、domain executable templates。
- 兼容层保留：`control_plane.bootstrap.*`、`control_plane.api.websocket_hub`、`shared.services.publish_service` 仍可用，但已降为 shim / facade。
