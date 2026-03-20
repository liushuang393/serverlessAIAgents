# Plugin-Modularity Review

## 结论

仓库已经具备插件化的骨架，但尚未形成统一的扩展模型。`contracts` 提供了清晰的 canonical 契约，`harness` 负责运行时治理，`kernel` 暴露了 plugin registry 名称；问题在于这三者没有形成闭环，导致“有插件概念，但没有统一插件生命周期”。

## Findings

### 1. `contracts.plugin` 是清晰的 canonical 契约层

- Evidence
  - `contracts/plugin/contracts.py` 定义了 `PluginBinding`、`PluginDescriptor`、`PluginRuntimeAssessment` 与 `PluginRegistryProtocol`
  - 契约覆盖了 manifest、app binding、运行时评估三种核心对象
- Impact
  - 为后续统一插件生命周期提供了稳定语义锚点
- Recommendation
  - 明确所有 plugin 子系统都以 `contracts.plugin` 为唯一契约源

### 2. `harness/governance/plugin_registry.py` 聚合了太多职责

- Evidence
  - 同一服务同时负责 manifest 读取、签名校验、kernel 兼容性检查、product line 校验、app binding 对照、side-effect tool 审核
- Impact
  - 插件治理逻辑强大，但也变成单点复杂体
  - 后续扩展新的治理策略、来源或生命周期事件时改动面过大
- Recommendation
  - 拆为 manifest loader、binding resolver、policy evaluator、signature verifier 四类协作对象

### 3. `kernel/plugins/registry.py` 的 plugin 语义过薄

- Evidence
  - `KernelPluginRegistry` 只有 `register/get/list_ids` 三个 dict 语义方法
  - 它不理解 manifest、版本、兼容性、生命周期、治理结果
- Impact
  - `kernel` 暴露了插件入口名称，但没有稳定的 plugin SPI
  - 容易出现“governed plugin”和“registered plugin”两套并行概念
- Recommendation
  - 为 `kernel` 定义最小 plugin runtime SPI：discovery、activation、capability expose、lifecycle hooks

### 4. `plugin_bindings` 已进入 app manifest，但采用不一致

- Evidence
  - `apps/code_migration_assistant/app_config.json` 有 3 个 plugin bindings
  - `apps/faq_system/app_config.json` 有 2 个 plugin bindings
  - `apps/messaging_hub/app_config.json` 有 2 个 plugin bindings
  - 多数 framework 型 app 没有 bindings，`dev_studio` manifest 也不完整
- Impact
  - 插件机制已被业务 app 采用，但并不是全平台一致的扩展主线
- Recommendation
  - 明确哪些能力必须走 plugin bindings，哪些仍是内置 runtime capability

### 5. “plugin / skill / provider” 三类扩展点边界不够清楚

- Evidence
  - `kernel/plugins/*`、`kernel/skills/*`、`infrastructure/* provider` 都是扩展点
  - 但 app manifest、kernel runtime 与 governance 层没有统一说明三者边界
- Impact
  - 新增能力时，开发者很难判断应该做 plugin、skill 还是 provider
- Recommendation
  - 固定判断标准：
    - provider: 外部后端适配
    - skill: app-facing capability composition
    - plugin: 可声明、可治理、可绑定的安装单元

## 好的模式

- `contracts.plugin.*` 作为 canonical contract source of truth
- `app_config.json` 用 `plugin_bindings` 显式声明 app 与 plugin 的关系
- `PluginRuntimeAssessment` 将 manifest、binding、permissions、warnings/errors 聚合为统一评估对象
- 对副作用工具做签名、版本、权限、product line 校验，这个治理方向是正确的

## 坏的抽象

- `KernelPluginRegistry` 过薄，无法承载真正的 plugin runtime 语义
- `PluginRegistry` 过厚，把发现、解析、校验、政策执行全部耦合在一起
- plugin/skill/provider 三套扩展模型并存，但没有稳定分工文档与 runtime 边界

## 最小改动路径评价

### 新增 app

- 当前路径：新增 `app_config.json`，若需要插件再补 `plugin_bindings`
- 评价：中等清晰，manifest 驱动方向正确，但平台与运行时读取链条仍偏分散

### 新增 plugin

- 当前路径：新增 plugin manifest、更新 trust store、补 app binding、确保 tool metadata 带 `plugin_id/plugin_version`
- 评价：偏重，治理能力完整，但接入面偏多

### 新增 skill

- 当前路径：通常进入 `kernel.skills` 或 `shared.skills`
- 评价：比 plugin 更轻，但语义容易与 plugin 重叠

### 新增 provider

- 当前路径：进入 `infrastructure` registry/backend，并由上层 gateway 调用
- 评价：相对清晰，是当前最成熟的扩展路径

## 插件架构最小闭环建议

建议固定下面这个最小闭环：

1. `contracts.plugin`
   - 定义 `PluginDescriptor`、`PluginBinding`、`PluginRuntimeAssessment`、`PluginRuntimeProtocol`
2. `kernel`
   - 只负责 plugin runtime SPI 与 lifecycle hooks
   - 不负责签名、产品线策略、app binding 校验
3. `harness`
   - 负责治理：签名、权限、版本、product line、side-effect policy
4. `apps`
   - 只通过 `plugin_bindings` 声明启用关系与 config
5. `infrastructure`
   - 负责 tool/provider 执行，并附带 plugin metadata 进入 runtime

执行链应为：

`manifest discover -> app binding resolve -> kernel activate plugin capability -> harness govern side effects -> infrastructure execute`

## 插件化评审结论

## Horizontal Expansion

- plugin manifest source-of-truth 问题不是单个 pack 路径配置错误，而是 runtime 默认扫描根和仓库真实 pack 根长期不一致；本轮已统一默认根到 `kernel/plugins/packs`，并保留 env override。
- app binding source-of-truth 也不是单个 app 的问题，`PluginRegistry` 之前自行解析 JSON，绕过 canonical manifest loader；本轮已统一改为 `shared.config.manifest.load_app_manifest()`。
- plugin runtime SPI 过薄不是个别插件包问题，而是 `kernel.plugins` 层级本身缺少最小语义；本轮已补 `KernelPlugin` typed SPI。

## Implemented Modularity Changes

- `harness.governance.plugin_registry` 现已引入 `PluginManifestLoader`、`AppBindingResolver`、`PluginPolicyEvaluator` 三个协作对象。
- `PluginRegistry` facade 名称保持不变，外部调用方式不破坏。
- `kernel.plugins.registry` 不再是裸 `dict[str, Any]`，而是最小 typed runtime registry。

- 插件化基础存在，方向并不差。
- 当前最大问题不是“没有插件”，而是没有统一 plugin lifecycle。
- 如果目标是模块化与低耦合，P0 不应该是继续加 registry，而是先统一 plugin/skill/provider 边界，并把 runtime 与 governance 解耦。
