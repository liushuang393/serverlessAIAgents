# AgentFlow CLI / Skill / Workflow 改造详细设计

最終更新: 2026-02-12  
対象: `agentflow/cli`、`agentflow/skills`、`docs/*`

## 1. 背景与问题定义

当前框架在能力上较强，但在“统一操作入口”和“技能挂载闭环”上存在割裂：

1. CLI 对 workflow 文件直跑缺失  
   - `agentflow run` 仅针对 agent 目录或 `@agent`（`agentflow/cli/main.py`）。  
   - 官方文档已明确“workflow.yaml 不能直接从 CLI 执行”（`docs/guide-cli.md`）。
2. Skill 外部接入路径不完整  
   - `skills` 子命令缺少“挂载外部 skills 目录”的标准入口。
3. `SkillRouter` 默认加载行为不一致  
   - 从 `agentflow.skills` 导出的 `SkillRouter` 走 `skills/core/router.py`；  
   - 该实现默认目录指向 `skills/core/builtin`，与真实目录 `skills/builtin` 不一致，导致默认初始化可能加载 0 技能。
4. 文档与实际能力不一致  
   - CLI 手册、Skills 指南与当前可执行能力存在差异，影响可发现性。

## 2. 改造目标

### 2.1 功能目标

1. 新增 `agentflow flow run`，支持 workflow YAML 直接执行。
2. 新增 `agentflow skills mount`，支持将外部 Skill 包挂载到项目或全局目录。
3. 修复 `agentflow.skills.SkillRouter` 默认技能目录加载问题，确保开箱即用。
4. 更新受影响文档，保证“命令可用性”和“文档示例”一致。

### 2.2 非目标

1. 不改造 Studio 的 workflow 执行器（当前仍为轻量/模拟执行路径）。
2. 不引入远程 Git 下载型 mount（网络依赖与安全策略另行设计）。
3. 不重构所有 `skills/*` 与 `skills/core/*` 重复模块（本次先修关键路径与导出一致性）。

## 3. 总体方案

## 3.1 CLI 分层补齐

新增命令组：

```bash
agentflow flow run WORKFLOW_PATH [--input ...] [--output ...] [--json] [--stream]
```

执行路径：

1. 解析 YAML 工作流文件。
2. 将文件转换为 `WorkflowService.execute(...)` 所需参数：
   - `workflow_type`
   - `task`
   - `input_data`
   - `config`
3. 支持 `--input` 叠加输入（命令行优先于文件中的 `input_data`）。
4. 输出行为与 `agentflow run` 对齐（stdout / `--json` / `--output`）。

### Workflow YAML v1 约定

支持以下字段（最小集）：

```yaml
workflow_type: deep_agent | pipeline | reflection
task: "..."
input_data: {}
config: {}
```

兼容字段：

- `type` 可作为 `workflow_type` 别名。

错误策略：

- 缺少 `workflow_type/type` 时报错并退出。
- `workflow_type` 非法时，透传 `WorkflowService` 错误信息。

## 3.2 Skills 挂载设计

新增命令：

```bash
agentflow skills mount SOURCE [--scope project|global] [--name NAME] [--force]
```

语义：

1. `SOURCE` 可为：
   - 单 skill 目录（目录下含 `SKILL.md`）
   - skills 根目录（包含多个 `*/SKILL.md` 子目录）
2. 目标目录：
   - `project` -> `.agentflow/skills`
   - `global` -> `~/.agentflow/skills`
3. 挂载策略：
   - 默认复制目录（非软链），确保可移植。
   - 默认不覆盖；`--force` 允许覆盖。
4. 校验策略：
   - 载入前执行 `Skill.load` + `SkillValidator.validate`。
   - 校验失败直接拒绝，保证挂载后可被 runtime 使用。

## 3.3 SkillRouter 修复

修复 `agentflow/skills/core/router.py` 默认目录解析：

- 从错误目录 `agentflow/skills/core/builtin` 调整为真实目录 `agentflow/skills/builtin`。
- 统一 loader 引用到 `agentflow.skills.core.loader`，避免新旧路径混用。

兼容性：

- 不修改 `agentflow.skills.router` 老模块行为。
- 仅确保 `from agentflow.skills import SkillRouter` 的默认初始化可加载内置技能。

## 4. 变更清单（文件级）

### 4.1 新增

1. `agentflow/cli/commands/flow.py`
2. `tests/unit/test_cli_flow.py`
3. 本设计文档 `docs/design/CLI_SKILL_WORKFLOW_UNIFICATION_DESIGN.md`

### 4.2 修改

1. `agentflow/cli/main.py`  
   - 注册 `flow` 子命令组。
2. `agentflow/cli/commands/skills.py`  
   - 增加 `mount` 命令实现。
3. `agentflow/skills/core/router.py`  
   - 修正 builtin 目录与 loader 引用。
4. 测试文件  
   - `tests/unit/skills/test_skill_router_exports.py` 增补导出路径回归测试。
5. 文档文件  
   - `docs/cli.md`
   - `docs/guide-cli.md`
   - `docs/guide-skills.md`
   - `docs/skills-guide.md`
   - `docs/quickstart.md`（补充 workflow CLI 执行入口）

## 5. 兼容性与迁移

1. 现有 `agentflow run` 行为不变。
2. 新增 `agentflow flow run` 为增量能力，不破坏既有脚本。
3. `skills mount` 为新增命令，不影响已有 `list/show/create/validate/search/delete`。
4. `SkillRouter` 修复后，`agentflow.skills` 导出的默认行为从“可能 0 skill”变为“可加载内置 skill”，属于修复性增强。

## 6. 测试设计

单元测试维度：

1. `flow run`
   - help 可用
   - 解析合法 yaml 并成功执行
   - 缺失 `workflow_type/type` 报错
   - `--input` JSON 字符串/文件覆盖行为
2. `skills mount`
   - 单 skill 目录挂载成功
   - 多 skill 批量挂载成功
   - 冲突未 `--force` 报错
   - 无效目录或缺失 `SKILL.md` 报错
3. `SkillRouter`
   - `from agentflow.skills import SkillRouter` 初始化后 `skill_count > 0`

## 7. 风险与回滚

风险：

1. CLI 新命令引入后帮助文本与文档不一致。  
   - 通过测试覆盖 `--help` 并在文档更新中统一。
2. `skills mount` 误覆盖现有技能。  
   - 默认禁止覆盖，需显式 `--force`。
3. `WorkflowService` 行为与用户预期“真实 DAG 执行”有差距。  
   - 文档明确当前 `flow run` 执行的是 `WorkflowService` 的 workflow 类型执行模型。

回滚策略：

1. `flow` 子命令组可独立回滚（删除命令注册与文件）。
2. `skills mount` 可独立回滚（删除单命令实现）。
3. `SkillRouter` 修复回滚仅涉及单文件路径变更。

## 8. 验收标准

1. `agentflow flow run` 可在本地通过示例 yaml 跑通并输出结果。
2. `agentflow skills mount` 可将外部技能挂载到 `.agentflow/skills` 并被 `skills list` 发现。
3. `from agentflow.skills import SkillRouter` 初始化后能加载内置技能。
4. 相关单元测试通过。
5. 上述能力在文档中均有可执行示例。
