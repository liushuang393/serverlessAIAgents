# Orchestration Review

## 结论

`control_plane` 仍然是平台控制面的中心，但它已经同时承担平台运行期、工程期工具链、app runtime bootstrap 和发布编排四类职责。它不是失效了，而是变胖了。

## Findings

### 1. `control_plane/bootstrap/*` 已进入 app runtime 热路径

- Evidence
  - `control_plane/bootstrap/app_bootstrapper.py` 负责从 `app_config.json` 解析 contracts 并构建 `CapabilityBundle`
  - 该路径不是后台运维工具，而是 app 启动和能力接线的一部分
- Impact
  - control plane 从“控制面”滑向“运行时装配器”
  - app runtime 与平台实现耦合增强
- Recommendation
  - 将 runtime capability bootstrap 从 control plane 分离为 app-facing adapter 层
- Conservative Note
  - central manifest parsing 可以保留，但应以稳定接口提供，而不是让 control plane 类型进入 runtime

### 2. `AppDiscoveryService` 同时承担发现、规范化、推断和注册

- Evidence
  - `control_plane/services/app_discovery.py` 不只扫描 manifest，还做字段推断、规范化与模块检查
- Impact
  - discovery 与 normalization 混合，服务边界过宽
  - manifest 修复策略会和平台 registry 生命周期耦合
- Recommendation
  - 拆为 manifest loader、normalizer、registry 三段

### 3. 发布链路跨层穿透严重

- Evidence
  - `control_plane/services/publish_orchestrator.py` 依赖 `shared.services.publish_service.PublishService`
  - `shared/services/publish_service.py` 又回头依赖 `apps.dev_studio.codegen` 与 `control_plane.deploy.*`
- Impact
  - 发布编排形成 `control_plane -> shared -> apps/control_plane` 的回环
  - 难以判断发布流程究竟属于平台运行期还是工程工具链
- Recommendation
  - 将发布相关能力收拢到一个工程期发布子系统
  - `shared` 不再承载发布链核心服务

### 4. `FrameworkAuditService` 持有过多治理与源码检查职责

- Evidence
  - `control_plane/services/framework_audit.py` 同时读取源码、校验 engine markers、检查插件绑定、协议表面和安全基线
  - 它还直接持有 `harness.governance.plugin_registry.PluginRegistry`
- Impact
  - 平台审计逻辑与治理实现绑定过深
  - 后续扩展审计策略和治理策略时会互相牵连
- Recommendation
  - 保留 control plane 作为 audit entrypoint
  - 但将规则执行拆给更细的 inspector/auditor 组件

### 5. `AppScaffolderService` 更像 tooling，不像运行期 control plane

- Evidence
  - `control_plane/services/app_scaffolder.py` 内含业务模板、引擎模式选项、端口分配与 app skeleton 知识
- Impact
  - 工程期 app 生成逻辑挤占 control plane 的职责边界
- Recommendation
  - 将 scaffolding 与 codegen 收敛到 `dev_studio` 或独立 tooling 子域

## 平台运行期职责 Vs 工程期职责

### 平台运行期职责，建议保留在 `control_plane`

- app discovery 与 registry
- framework audit 入口
- platform API、lifecycle 状态、观察面
- capability 配置分发的控制接口

### 工程期职责，建议外移或拆分

- app scaffolding
- code generation
- publish / deploy orchestration
- component library 的开发期管理

## Control Plane 瘦身建议

1. 先把工程期 tooling 从运行期 API 逻辑里分离出来
2. 再把 bootstrap/discovery/audit 分拆为更窄的服务对象
3. 最后为 app runtime 提供稳定 adapter，避免直接 import control plane internals

## Conservative Note

- 不建议把所有平台能力都拆散到 apps。
- `control_plane` 保留统一 registry、审计入口和平台 API 是必要的。
- 真正要避免的是 control plane 继续增长为“平台+runtime+tooling+generator”的总装层。

## Horizontal Expansion

- `control_plane/bootstrap/*` 的运行时越权不是 `AppCapabilityBootstrapper` 单点，而是 bootstrapper、watcher、rag builder、skill builder 整个子树都处在 app runtime 路径中；本轮已整体搬到 `kernel.runtime`，control-plane 只留兼容 facade。
- publish 链过厚也不是 orchestrator 单点，studio routes、orchestrator、shared publish service 构成完整回环；本轮已把 canonical publish service 放回 control-plane 自身。

## Remediation Status

- app-facing bootstrap API 已在 `kernel.runtime` 落地。
- `control_plane.bootstrap.*` 与 `control_plane.api.websocket_hub` 已降级为 shim。
- `control_plane/services/publish_orchestrator.py` 已直接依赖 `control_plane.publish.service.PublishService`。
