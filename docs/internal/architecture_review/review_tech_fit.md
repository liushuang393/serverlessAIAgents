# Tech-Fit Review

## 结论

每一层的技术职责大体有方向，但实现上存在明显错位。`contracts`、`infrastructure`、`harness` 的定位相对清晰；`shared`、`kernel`、`control_plane`、`domain` 的职责则发生了不同程度的混合。

## 七层逐层短评

### `contracts`

- 正向判断：仍然是最稳定的 canonical 契约层，`contracts/app/contracts.py` 与 `contracts/plugin/contracts.py` 的方向正确。
- 问题：部分契约在平台层被二次包装，真源开始分裂。

### `infrastructure`

- 正向判断：provider、storage、sandbox、observability 这些外部依赖适配职责放在这里是合理的。
- 问题：少量实现开始知道 `kernel` 或 `harness` 运行时细节，适配层纯度不足。

### `shared`

- 正向判断：`shared/gateway/*` 这类轻量 facade 适合作为横向复用层。
- 问题：目录内容过宽，已经混入 service locator、业务复用、运行时桥接和应用级服务。

### `kernel`

- 正向判断：flow、runtime、router、tool executor、agent abstractions 都应当属于内核能力。
- 问题：内核既持有抽象，又持有具体 provider 假设，导致稳定内核与装配逻辑混在一起。

### `harness`

- 正向判断：approval、evaluation、gating、guardrails 作为横切关注点集中在这一层是合理的。
- 问题：`HarnessedToolRuntime` 仍直接知道 `KernelToolExecutor` 与基础设施类型，隔离层还不够彻底。

### `control_plane`

- 正向判断：app discovery、framework audit、registry、平台 API 放在这里符合控制面定位。
- 问题：同时承载 bootstrap、publish、deploy、codegen、scaffolding，工程期与运行期职责混在一起。

### `domain`

- 正向判断：`domain/commerce` 这种模型和接口资产符合领域层定位。
- 问题：`domain/templates/*` 直接生成可执行 agent，使领域层混入运行时实现。

## 技术架构错位点

### 1. `shared/services/publish_service.py` 的职责不属于 `shared`

- Why Misplaced
  - 该服务不仅是共享服务，还承担 code generation、deploy executor、config manager 装配
  - 它直接依赖 `apps.dev_studio.codegen` 和 `control_plane.deploy.*`
- Target Layer
  - 应迁回 `control_plane` 的工程期发布子系统，或独立为 tooling/application service 层

### 2. `control_plane/bootstrap/*` 处在 app runtime 热路径

- Why Misplaced
  - `AppCapabilityBootstrapper` 负责读取 app manifest 并构建 capability bundle，这属于 app runtime 装配
  - 这使 app runtime 对 control plane 实现产生直接依赖语义
- Target Layer
  - 应保留 manifest 解析契约在 `contracts`
  - runtime 装配应进入 app-facing adapter 或独立 runtime bootstrap 层

### 3. `control_plane/schemas/app_config_schemas.py` 与 `contracts.app.AppManifest` 形成双轨

- Why Misplaced
  - 平台层不应再拥有一套 manifest 主模型
- Target Layer
  - canonical schema 统一回 `contracts.app`
  - `control_plane` 保留投影、校验和 UI 友好的平台视图

### 4. `domain/templates/*` 的 executable template 语义放错层

- Why Misplaced
  - 这些模板直接继承 `kernel.ResilientAgent`，已经是运行时可执行实现
- Target Layer
  - `domain` 保留模板描述与规则
  - 可执行 template runtime 放到 app/plugin 层

### 5. `infrastructure` 与 `kernel/harness` 的耦合超过 adapter 边界

- Why Misplaced
  - 当适配器直接了解 runtime context 或 guardrails 细节时，infra 不再是纯粹后端适配器
- Target Layer
  - 通过 `contracts` 定义 ports
  - 运行时协调逻辑留在 `kernel` 或 `harness`

## Conservative Note

- 不建议为了“纯粹分层”而打散所有运行时桥接点。
- `harness` 保留作为 `kernel` 外挂治理管线是合理的。
- `control_plane` 保留平台级 discovery、audit、registry 也合理，问题不在它存在，而在它过宽。

## 技术架构评审结论

- `contracts`, `infrastructure`, `harness` 基本合理。
- `shared`, `kernel`, `control_plane`, `domain` 处于“局部失衡，需要收口”的状态。
- 若目标是可维护与插件化，优先级应放在职责归位，而不是继续叠加抽象层。

## Horizontal Expansion

- manifest 双轨不是 `AppDiscoveryService` 单点问题，`bootstrap`、`plugin governance`、app startup path 都会消费 `app_config.json`；本轮已统一回 canonical manifest loader。
- runtime bootstrap 错位不是 `faq_system` 特例，`CapabilityBundle`、`ConfigWatcher`、RAG/skills builder 原本都挂在 `control_plane.bootstrap`；本轮已整体迁入 `kernel.runtime`。
- publish 错位不仅影响 `shared/services/publish_service.py`，还牵动 `publish_orchestrator` 与 studio publish routes；本轮已把 canonical 实现收回 `control_plane.publish.service`。

## Implemented Tech-Fit Changes

- `AppConfig` 现在明确声明为 `contracts.app.AppManifest` 的 control-plane projection，并新增 `from_manifest()`。
- `AppDiscoveryService` 已改为先读取 canonical manifest，再生成平台 projection。
- `domain/templates/*` 已改走 `kernel.templates.template_runtime` 与 `domain.templates.template_helpers`，减少对 `kernel` 根导出与 `shared.utils` 的直接耦合，同时让入口命名更直白。
