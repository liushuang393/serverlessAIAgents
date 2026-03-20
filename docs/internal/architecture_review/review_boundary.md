# Boundary Review

## 结论

当前设计层架构是“概念上合理、实现上边界漂移明显”。七层模型仍可识别，但还没有真正落成高内聚、低耦合的单向分层体系。

## Findings

### 1. `shared` 已从共享层漂移为跨层中枢

- Evidence
  - 依赖证据：基线扫描中存在 `shared -> control_plane` 与 `shared -> apps`
  - 源码证据：`shared/services/publish_service.py` 延迟加载 `apps.dev_studio.codegen`、`control_plane.deploy.executor`、`control_plane.deploy.config.manager`
- Impact
  - `shared` 不再是“横向通用能力层”，而变成可以向上感知产品层和平台层的收容层
  - 新增共享能力时，边界进一步失守的概率很高
- Recommendation
  - 明确限制 `shared` 只能依赖 `contracts` 与 `infrastructure`
  - 将发布编排类能力从 `shared` 移出，回归 `control_plane` 或独立 tooling 层
- Conservative Note
  - `shared/gateway/*` 这种面向应用的轻 facade 可以保留，不需要一刀切拆空 `shared`

### 2. `kernel` 不再是稳定内核，而开始感知控制面和基础设施细节

- Evidence
  - 依赖证据：`kernel -> infrastructure` 49 条，`kernel -> shared` 43 条，且已有 `kernel -> control_plane`
  - 源码证据：`kernel/agents/mixins/rag_mixin.py` 感知 `control_plane.bootstrap.capability_bundle`
- Impact
  - 内核 API 的稳定性下降
  - 更换 runtime bootstrap、provider 装配或 control plane 契约时会牵动 kernel
- Recommendation
  - 收缩 `kernel` 到 runtime contract、flow orchestration、agent/tool SPI
  - 将 capability bundle 之类装配结构从 kernel 可见范围移到 adapter 层
- Conservative Note
  - 与基础设施的少量桥接可以保留，但要通过明确的 ports 或 adapter 语义，而不是控制面类型

### 3. `domain` 被可执行模板污染，领域边界不纯

- Evidence
  - 依赖证据：`domain -> kernel` 18 条，`domain -> shared` 9 条
  - 源码证据：`domain/templates/finance/financial_analysis_template.py` 直接继承 `kernel.ResilientAgent` 并使用 `shared.utils`
- Impact
  - 领域资产难以脱离运行时独立复用
  - 模板层与执行层耦合，未来做插件化或独立发布成本升高
- Recommendation
  - `domain` 只保留模型、规则、模板元数据
  - 可执行 agent/template runtime 下沉到 app 或 plugin 层
- Conservative Note
  - `domain/commerce` 这类纯模型与接口可以保留原状，是正向资产

### 4. `control_plane` 已经开始持有产品层知识

- Evidence
  - 依赖证据：基线扫描中已有 `control_plane -> apps`
  - 源码证据：`control_plane/services/app_scaffolder.py` 内建 app 模板、业务模板和引擎模式知识
- Impact
  - 平台层与产品层边界变模糊
  - 特例逻辑会越来越容易堆进 `control_plane`
- Recommendation
  - 将工程期 app scaffolding 与运行期平台 API 拆开
  - `control_plane` 保留平台运行期治理、发现、注册和观察面
- Conservative Note
  - `control_plane` 持有 manifest 扫描与平台级审计是合理的，不应全部外移

### 5. `apps` 外层并不只是产品壳，已经直接穿透多层

- Evidence
  - 依赖证据：`apps -> kernel/shared/harness/infrastructure` 全部很重，且有 `apps -> control_plane`
  - 源码证据：`apps/messaging_hub/main.py` 直接引入 `control_plane.api.websocket_hub.WebSocketHub`
- Impact
  - 产品层复用核心能力的同时，也把控制面和运行时内部细节拉进来了
  - app 迁移或独立部署难度升高
- Recommendation
  - 为 app 提供更薄、更稳定的 runtime facade
  - 控制面能力只能通过稳定 adapter 进入 app，而不是直接 import control plane internals
- Conservative Note
  - apps 作为外层对 kernel/shared/harness 的依赖本身是正常的，问题在于直接碰 `control_plane`

### 6. `contracts` 作为真源的地位开始被二次模型稀释

- Evidence
  - 依赖证据：`control_plane` 保留独立的 app config schema 子系统
  - 源码证据：`control_plane/schemas/app_config_schemas.py` 在 `contracts.app.AppManifest` 之外又维护平台自己的完整 manifest 模型
- Impact
  - manifest 语义会分叉
  - 配置、发现、治理、运行时读取出现 schema drift 风险
- Recommendation
  - 明确 `contracts.app.AppManifest` 为 canonical source of truth
  - `control_plane` 只做平台投影和补充校验，不再拥有第二套主模型
- Conservative Note
  - 平台面保留只读投影视图是合理的，但不能演化成第二真源

## 建议的允许依赖矩阵

| Source | Allowed | Avoid / Suspicious | Violation |
| --- | --- | --- | --- |
| `contracts` | none | - | any outward dependency |
| `infrastructure` | `contracts` | `shared`, `harness`, `kernel` only via adapters | `apps`, `control_plane` runtime knowledge |
| `shared` | `contracts`, `infrastructure` | `kernel`, `harness` | `control_plane`, `apps` |
| `kernel` | `contracts` | `shared`, `infrastructure`, `harness` via ports | `control_plane` |
| `harness` | `contracts`, `shared`, `kernel`, `infrastructure` | deep product knowledge | `apps` direct governance knowledge |
| `control_plane` | `contracts`, `shared`, `kernel`, `harness`, `infrastructure` | `apps` only for tooling/scaffolding | app runtime internals |
| `domain` | `contracts` | `shared` helper use | `control_plane`; direct executable runtime inheritance |
| `apps` | core layers through stable facades | `control_plane` | control plane internals in runtime path |

## 边界评审结论

- 七层模型值得保留，不需要重画架构。
- 真正的问题是边界纪律没有落实到实现层。
- 优先级最高的边界修复点是：`shared` 收口、`kernel` 收缩、`control_plane` 去产品化、`domain` 去执行化。

## Horizontal Expansion

- `shared -> control_plane` 并不是单点问题，主要扩散在消息通道和发布链路；本轮已分别收口到 `kernel.runtime` facade 与 `control_plane.publish.service`。
- `apps -> control_plane internals` 主要出现在 app 启动热路径；`messaging_hub` 与 `faq_system` 已迁出，后续新增同类依赖由边界脚本拦截。
- `domain` 的 executable template smell 横向分布在 finance / healthcare / manufacturing 三个模板族，不是单文件例外；本轮已统一改到稳定 runtime helper。

## Remediation Status

- `kernel/agents/mixins/rag_mixin.py` 已改为依赖 `kernel.runtime.CapabilityBundle`。
- `apps/messaging_hub/main.py` 与 `shared/channels/gateway.py` 已改为依赖 `kernel.runtime.WebSocketHub`。
- `apps/faq_system/main.py` 已改为依赖 `kernel.runtime.AppCapabilityBootstrapper`。
- `tests/contracts/test_layer_boundaries.py` 已追加 `apps -> control_plane` 直接依赖检查。
