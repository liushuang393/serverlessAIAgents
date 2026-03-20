# Contrarian Review

## 结论

仓库确实存在边界漂移，但并非所有复杂度都应该被视为坏味道。当前代码库同时承担框架演进、兼容层过渡和多产品线支持，一部分“看起来不纯”的设计其实是在买迁移成本，而不是单纯过度设计。

## 谨慎改动结论

### 1. 七层模型本身值得保留，不要因实现偏差而重画架构

- Why Keep
  - `contracts`、`harness` 已经展现出真实价值
  - 当前问题更像边界纪律失守，而不是分层模型错误
- Not P0
  - 不要优先做“重新命名层级”或“重写架构图”

### 2. 懒加载与兼容 alias 不能一概视为坏抽象

- Why Keep
  - 在迁移期，lazy import 能避免重依赖与启动成本
  - 兼容层有助于逐步收口，而不是一次性重构爆炸
- Not P0
  - 不建议把所有 lazy import 当作第一优先级整改对象

### 3. `harness` 作为 `kernel` 外挂治理层是正确方向

- Why Keep
  - `HarnessedToolRuntime` 把 gating/approval/evaluation 挂在执行器外侧，这是合理模式
- Not P0
  - 不建议为了追求绝对纯度，把治理能力塞回 kernel 或拆碎成过多微对象

### 4. `control_plane` 保留 app discovery、framework audit 与 registry 是必要成本

- Why Keep
  - 平台没有统一 discovery 和 audit，就无法提供运维与治理能力
- Not P0
  - 不建议把发现和审计能力下放到各 app

### 5. 不是所有大文件都应该立即拆分

- Why Keep
  - 大文件是风险信号，不是自动重构指令
  - 若边界未先厘清，先拆文件只会把复杂度复制到更多文件
- Not P0
  - `deep_agent.py`、`app_lifecycle.py` 等应先做职责界定，再决定如何拆

## 最值得反驳的激进观点

- “让 `kernel` 完全不依赖 `shared` 或 `infrastructure`”
  - 反驳：运行时内核与执行环境之间需要桥接，问题在于桥接方式不稳定，不在于完全存在桥接
- “把所有 control plane 能力都外移到 apps”
  - 反驳：平台的发现、审计、注册、观察面必须集中
- “先全面拆大文件”
  - 反驳：先修边界，再拆实现，收益更大

## 不建议进入 P0 的区域

- 命名层面的美化式重构
- 单纯为“纯净架构”而拆散兼容层
- 全面替换 lazy import
- 对所有 app 做等深重构
- 在统一 plugin lifecycle 之前先重写所有 plugin packs

## 保留不动建议

- 保留 `contracts` 作为 canonical 契约锚点
- 保留 `harness` 作为横切治理层
- 保留 `control_plane` 的平台入口地位
- 保留 app manifest 驱动的配置模式

## 反方评审结论

- 当前最需要的不是“大重写”，而是有限度地收紧边界。
- P0 应聚焦边界与真源统一，不应扩大为全仓结构重写。

## Horizontal Expansion Not In P0

- `control_plane/services/app_lifecycle.py`、`app_scaffolder.py` 等超大文件仍然是热点，但本轮未直接做文件级大拆分。
- provider registry 与 infrastructure 适配层的进一步纯化仍保留到后续波次。
- plugin packs 全量规范化与 trust store 治理仍有后续空间，但不在本轮做破坏式迁移。
