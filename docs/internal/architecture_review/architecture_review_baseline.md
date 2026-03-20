# Architecture Review Baseline

## Review Target

- 核心层: `contracts / infrastructure / shared / kernel / harness / control_plane / domain`
- 产品层: `apps/`
- 代表应用: `code_migration_assistant`、`messaging_hub`、`faq_system`

## Method

本基线包只使用只读证据：

- 架构规则与索引: `code-rules/project/architecture.md`, `docs/index.md`
- 目录结构扫描: 顶层目录与 `apps/*/app_config.json`
- 粗粒度 import 扫描: 统计 Python 源码中的跨层 import 关系
- 热点源码抽样: 聚焦反向依赖、层职责漂移、插件化入口

## Declared Vs Actual Boundary

### 声明边界

仓库文档明确声明：

- 7 核心层 + `apps/` 外层
- `domain` 不依赖 `control_plane`
- `contracts / infrastructure / shared / kernel / harness / domain` 不依赖 `apps/`

### 实际边界

实际代码显示概念架构存在，但边界已经漂移：

- `shared -> control_plane` 与 `shared -> apps` 已存在，违反文档中的核心层隔离目标。
- `kernel -> control_plane` 已出现，说明“稳定内核”开始感知控制面。
- `control_plane -> apps` 已出现，说明平台层开始持有产品层知识。
- `domain -> kernel/shared` 已大量存在，说明领域层混入可执行运行时模板。
- 插件模型分散在 `contracts`、`harness`、`kernel` 三处，语义没有闭环统一。

## Real Architecture Snapshot

从运行路径看，仓库当前更接近下面这张“真实架构图”：

```text
apps
  -> kernel / shared / harness / infrastructure
  -> 部分直接触碰 control_plane

control_plane
  -> contracts / shared / kernel / harness / infrastructure
  -> 少量直接触碰 apps

harness
  -> contracts / shared / kernel / infrastructure

kernel
  -> contracts / shared / infrastructure
  -> 少量触碰 harness / control_plane

shared
  -> contracts / infrastructure
  -> 少量触碰 kernel / harness / control_plane / apps

domain
  -> contracts
  -> 模板子树直接触碰 kernel / shared

infrastructure
  -> contracts
  -> 少量触碰 harness / kernel / shared
```

## Cross-Layer Import Hotspots

以下数据来自粗粒度 import 扫描，表示“跨层 import 语句数 / 涉及文件数”：

| Source | Target | Count | Files | Baseline |
| --- | --- | ---: | ---: | --- |
| `apps` | `kernel` | 203 | 137 | expected-heavy |
| `apps` | `shared` | 71 | 43 | expected-heavy |
| `control_plane` | `kernel` | 62 | 43 | expected-heavy |
| `kernel` | `infrastructure` | 49 | 24 | suspicious |
| `apps` | `infrastructure` | 45 | 33 | acceptable |
| `kernel` | `shared` | 43 | 31 | suspicious |
| `shared` | `infrastructure` | 37 | 27 | acceptable |
| `infrastructure` | `contracts` | 36 | 20 | expected |
| `control_plane` | `infrastructure` | 30 | 21 | expected |
| `shared` | `kernel` | 22 | 14 | suspicious |
| `apps` | `harness` | 21 | 16 | acceptable |
| `control_plane` | `shared` | 20 | 10 | expected |
| `domain` | `kernel` | 18 | 9 | suspicious |
| `harness` | `infrastructure` | 14 | 10 | acceptable-with-adapters |
| `shared` | `control_plane` | 4 | 3 | violation |
| `apps` | `control_plane` | 3 | 3 | suspicious |
| `control_plane` | `apps` | 2 | 1 | suspicious |
| `kernel` | `control_plane` | 1 | 1 | violation |
| `shared` | `apps` | 1 | 1 | violation |

## Horizontal Expansion

### Cross-Layer Violation Matrix

| Pattern | Representative Files | Horizontal Spread | Current Status |
| --- | --- | --- | --- |
| `shared -> control_plane` | `shared/services/publish_service.py`, `shared/channels/gateway.py` | 聚集在消息通道与发布链路 | 已收口为 runtime facade / lazy compat |
| `kernel -> control_plane` | `kernel/agents/mixins/rag_mixin.py` | 主要集中在 capability bootstrap 类型感知 | 已切到 `kernel.runtime` |
| `apps -> control_plane internals` | `apps/messaging_hub/main.py`, `apps/faq_system/main.py` | 代表 app 启动路径 | 已切到 `kernel.runtime` facade |
| `domain -> runtime/shared helpers` | `domain/templates/*` 9 个模板文件 | finance / healthcare / manufacturing 三类模板均存在 | 已切到 `kernel.templates.template_runtime` 与 `domain.templates.template_helpers` |
| `control_plane -> shared -> apps/control_plane` 回环 | `control_plane/services/publish_orchestrator.py` + `shared/services/publish_service.py` | 发布链主路径 | 已将 canonical 实现迁入 `control_plane.publish.service` |

### `apps/*/app_config.json` Completeness Matrix

| Area | Current Observation | Risk | Action |
| --- | --- | --- | --- |
| canonical loader | `shared.config.manifest` 已存在并可统一读取 | 低 | 已作为 bootstrap / plugin registry 真源 |
| plugin bindings | 业务 app 覆盖较好，framework app 不均匀 | 中 | 继续在治理与审计上补齐 |
| product line / profile | 多数 manifest 已具备最小字段 | 中 | `AppConfig` 明确降为 projection |
| runtime commands / urls | 完整性随 app 差异较大 | 中 | 保持由 canonical manifest 承载 |

### Plugin / Skill / Provider Comparison

| Kind | Canonical Layer | Role | Current Implementation Status |
| --- | --- | --- | --- |
| `plugin` | `contracts.plugin` + `harness.governance` | 可声明、可治理、可绑定的安装单元 | 已统一默认 packs 根目录与 canonical manifest loader |
| `skill` | `kernel.skills` | app-facing capability composition | 保持为 runtime-facing composition |
| `provider` | `infrastructure` | 外部后端适配 | 维持现状，未做破坏式迁移 |

### Hot File Inventory

| File | Approx Role | Review Result |
| --- | --- | --- |
| `control_plane/services/app_lifecycle.py` | lifecycle + repair + runtime management | 仍为 P2 热点 |
| `control_plane/services/app_discovery.py` | discovery + normalize + validate | 已补 projection 真源说明，后续继续拆分 |
| `control_plane/services/publish_orchestrator.py` | publish facade | 已切换到 canonical publish service |
| `harness/governance/plugin_registry.py` | plugin governance facade | 已拆 loader / binding resolver / policy evaluator 协作对象 |

## Implemented Remediation Snapshot

- `kernel.runtime` 新增稳定入口：`AppCapabilityBootstrapper`、`CapabilityBundle`、`WebSocketHub`、`ConfigWatcher`。
- `control_plane.bootstrap.*` 与 `control_plane.api.websocket_hub` 降为兼容 re-export。
- `control_plane.publish.service` 成为 canonical publish 实现；`shared.services.publish_service` 仅保留 lazy compat wrapper。
- `PluginRegistry` 默认 manifest 根切换到 `kernel/plugins/packs`，并通过 canonical app manifest loader 读取 bindings。
- `scripts/check_layer_boundaries.py` 在本轮整改后返回 `No layer boundary violations detected.`。

## Evidence Anchors

以下文件被作为后续 reviewer 共用证据：

- `shared/services/publish_service.py`
  - `shared` 直接延迟加载 `apps.dev_studio.codegen` 与 `control_plane.deploy.*`
- `kernel/agents/mixins/rag_mixin.py`
  - `kernel` 在类型层感知 `control_plane.bootstrap.capability_bundle`
- `domain/templates/finance/financial_analysis_template.py`
  - `domain` 模板直接继承 `kernel.ResilientAgent` 并调用 `shared.utils`
- `control_plane/services/app_scaffolder.py`
  - `control_plane` 持有 app 生成模板与引擎模式知识
- `apps/messaging_hub/main.py`
  - app 运行入口直接引入 `control_plane.api.websocket_hub.WebSocketHub`
- `control_plane/schemas/app_config_schemas.py`
  - `control_plane` 又维护一套 manifest 模型，和 `contracts.app.AppManifest` 形成双轨
- `harness/governance/plugin_registry.py`
  - 插件治理、签名、兼容性与 app binding 检查聚合在同一服务
- `kernel/plugins/registry.py`
  - kernel plugin registry 只有最薄的 dict 注册语义

## Hot Modules

按 Python 文件行数统计的热点模块：

| LOC | Path |
| ---: | --- |
| 3294 | `kernel/patterns/deep_agent.py` |
| 2873 | `apps/faq_system/backend/agents/faq_agent.py` |
| 2679 | `control_plane/services/app_lifecycle.py` |
| 2590 | `apps/messaging_hub/main.py` |
| 2128 | `apps/faq_system/backend/services/rag_ingestion.py` |
| 1314 | `shared/services/semantic_layer.py` |
| 1295 | `apps/code_migration_assistant/engine.py` |
| 1149 | `control_plane/services/app_scaffolder.py` |
| 1122 | `control_plane/routers/apps.py` |
| 1042 | `control_plane/services/framework_audit.py` |

这些文件不一定都是问题，但它们同时具有“复杂度高 + 边界敏感”的特征，值得在评审中优先阅读。

## Representative App Selection

| App | Why It Was Chosen |
| --- | --- |
| `code_migration_assistant` | `product_line=migration`，10 个 agents，3 个 plugin bindings，编排与 workflow 密度最高 |
| `messaging_hub` | `product_line=assistant`，2 个 plugin bindings，直接触碰 `control_plane`，最能暴露控制面与通道耦合 |
| `faq_system` | `product_line=faq`，7 个 agents，2 个 plugin bindings，`shared/kernel/infrastructure` 复用最重 |

代表应用的粗粒度依赖画像：

- `code_migration_assistant`: `kernel=37`, `shared=13`, `harness=10`
- `messaging_hub`: `kernel=18`, `control_plane=1`, `harness=1`
- `faq_system`: `shared=32`, `kernel=31`, `infrastructure=24`

## Baseline Conclusion

- 概念上的七层架构仍然可识别，但已经不是严格的单向分层。
- `contracts` 仍是最稳定的锚点，`harness` 也具备独立横切层雏形。
- 当前最大风险不是层数不够，而是 `shared`、`kernel`、`control_plane` 过胖且互相渗透。
- 后续 reviewer 应优先验证边界收口、插件闭环和 control plane 瘦身这三条主线。
