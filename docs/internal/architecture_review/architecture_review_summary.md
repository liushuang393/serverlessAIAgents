# Architecture Review Summary

## Executive Answer

### 1. 现在的设计层架构是否合理

结论：`基本合理，但边界漂移明显。`

- 七层核心架构 + `apps/` 外层作为概念模型是成立的。
- 真正的问题在于实现层没有守住单向依赖和职责边界。

### 2. 每一层的技术架构是否合理

结论：`局部失衡，需要收口。`

- `contracts`、`infrastructure`、`harness` 基本合理。
- `shared`、`kernel`、`control_plane`、`domain` 有不同程度的职责错位。

### 3. 每个技术架构内部设计是否合理

结论：`部分合理，但要支撑插件化与长期维护仍需重构。`

- 当前已经有契约、治理、manifest、registry、audit 等良好基础。
- 问题主要集中在抽象重复、对象过厚、插件生命周期不闭环。

## Scorecard

| Dimension | Score | Comment |
| --- | ---: | --- |
| Cohesion | 56 | `shared`、`control_plane`、`kernel` 都有职责堆积 |
| Coupling | 44 | 已出现 `shared -> control_plane/apps`, `kernel -> control_plane` |
| Pattern Fit | 61 | governance pipeline 与 contract-first 是优点，部分 registry 过薄或过厚 |
| Extensibility | 58 | provider 扩展较清晰，plugin 扩展成本偏高 |
| Maintainability | 49 | 多个热点文件过大，边界模糊提升认知成本 |
| Plugin / Modularity | 52 | 有骨架，但缺统一 plugin lifecycle |
| Overall | 53 | 架构可救且值得保留，但必须做收口 |

## Top 10 Problems

1. `shared` 反向依赖 `control_plane` 与 `apps`，共享层失去边界
2. `kernel` 直接感知基础设施与控制面细节，稳定内核被侵蚀
3. `control_plane` 同时承担运行期、工程期、发布期与 app scaffolding 职责
4. `domain/templates/*` 直接继承 runtime agent，领域层被执行实现污染
5. `contracts.app.AppManifest` 与 `control_plane` manifest schema 双轨并存
6. plugin governance、plugin runtime、plugin registry 没有统一生命周期
7. `KernelPluginRegistry` 过薄，无法成为真正的 plugin SPI
8. 发布链路形成 `control_plane -> shared -> apps/control_plane` 回环
9. app runtime 直接 import control plane internals，如 `apps/messaging_hub/main.py`
10. 多个热点文件已经超过“边界不清 + 复杂度高”的双重阈值

## What Is Real, What Is Acceptable

### 真问题

- 反向依赖与跨层直连
- manifest 真源分裂
- plugin/skill/provider 三类扩展点边界混乱
- control plane 过宽

### 可接受复杂度

- `harness` 作为横切治理层
- 平台集中 discovery / audit / registry
- 迁移期 lazy import 与兼容 alias

### 过度设计

- 过薄的 registry 只提供名字，不提供生命周期语义
- 过厚的治理对象把发现、签名、绑定、政策全塞在一起

### 应保留扩展点

- `contracts` 作为 canonical contract source
- app manifest 驱动的能力声明
- harness 的 approval / evaluation / gating 管线
- infrastructure provider registry 模式

## P0 / P1 / P2 Route

### P0: 先修边界

- 统一 `AppManifest` 真源，停止 schema 双轨演化
- 明确 `shared` 禁止依赖 `control_plane/apps`
- 禁止 `kernel -> control_plane`
- 规定 app runtime 不直接 import control plane internals
- 划清 plugin / skill / provider 三类扩展点的定义

### P1: 再修抽象

- 拆分 `PluginRegistry` 的加载、绑定、签名、政策职责
- 为 `kernel` 补足最小 plugin runtime SPI
- 将 `control_plane` 的工程期能力拆出运行期核心路径
- 将 `domain/templates/*` 去执行化

### P2: 最后修模式

- 逐步拆分热点大文件
- 将 runtime bootstrap 收敛到稳定 adapter
- 优化跨层 facade，减少隐式 service locator 用法

## 30 / 60 / 90 Day Route

### 30 Days

- 出台允许依赖矩阵
- 固定 manifest 真源与 plugin/skill/provider 定义
- 把最明显的反向依赖列入 lint / audit 规则

### 60 Days

- 拆出 plugin governance 子组件
- 将 `shared/services/publish_service.py` 从 `shared` 移出
- 收缩 `control_plane` 的工程期职责

### 90 Days

- 完成 domain executable templates 的迁移
- 补齐 kernel plugin lifecycle
- 按热点文件优先级分批做实现拆分

## 2026-03-20 Implementation Update

### 已落地

- `kernel.runtime` 已成为 app-facing bootstrap 与 websocket 的稳定入口。
- `control_plane.bootstrap.*`、`control_plane.api.websocket_hub`、`shared.services.publish_service` 已保留兼容层，但不再是 canonical 实现。
- `control_plane.publish.service` 已成为发布链的 canonical service。
- `PluginRegistry` 已统一默认 plugin packs 根目录，并改用 canonical app manifest loader。
- `domain/templates/*` 第一批已完成 runtime helper 收口。

### 仍在 P2 / 后续波次

- `control_plane/services/app_lifecycle.py`、`app_discovery.py`、`app_scaffolder.py` 的深度拆分
- plugin lifecycle 更完整的 activation / hooks 语义
- tooling 子边界从 `control_plane` 的进一步分离

## Verification Note

- 已通过 `python3 -m py_compile` 对本轮变更文件做语法校验。
- 已通过 `python3 scripts/check_layer_boundaries.py`，结果为 `No layer boundary violations detected.`。
- 当前环境缺少 `pytest` / 仓库测试依赖，未能执行完整 pytest 与 `./check.sh all`。

## Keep As-Is

- 保留七层模型
- 保留 `contracts` 与 `harness` 的核心定位
- 保留 manifest 驱动的配置策略
- 保留平台集中 audit/discovery 能力

## Final Recommendation

这个仓库不需要“推倒重画”，但确实需要一次边界收口。  
如果目标是高内聚、低耦合、易扩展、易维护、插件化，最有效的顺序不是“大拆文件”，而是：

1. 先统一边界与真源
2. 再统一插件生命周期
3. 最后再做实现层拆分
