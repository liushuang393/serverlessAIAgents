# Code Migration Assistant - MCP工具设计

## 📋 概要

Code Migration Assistantは**編排器（Orchestrator）**として設計され、実際の処理は各種MCP工具を呼び出して実行します。これにより、松耦合、可复用、可扩展的なアーキテクチャを実現します。

---

## 🏗️ MCP工具化アーキテクチャ

```
┌─────────────────────────────────────────────────────────────┐
│         Code Migration Assistant (Orchestrator)              │
│                                                               │
│  ┌─────────────────────────────────────────────────────┐   │
│  │           Workflow Orchestration Logic               │   │
│  │  - 工具调用顺序管理                                   │   │
│  │  - 数据流转控制                                       │   │
│  │  - 错误处理和重试                                     │   │
│  │  - 结果聚合                                           │   │
│  └─────────────────────────────────────────────────────┘   │
│                            │                                 │
│                            ▼                                 │
│  ┌─────────────────────────────────────────────────────┐   │
│  │              MCP Protocol Layer                      │   │
│  └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
                             │
        ┌────────────────────┼────────────────────┐
        ▼                    ▼                    ▼
┌──────────────┐    ┌──────────────┐    ┌──────────────┐
│  核心工具层   │    │  辅助工具层   │    │  基盤工具层   │
├──────────────┤    ├──────────────┤    ├──────────────┤
│ COBOLParser  │    │SyntaxChecker │    │ Reflection   │
│ JavaGenerator│    │StyleChecker  │    │ Memory       │
│CodeValidator │    │TestGenerator │    │ LLM Client   │
│              │    │Complexity    │    │              │
└──────────────┘    └──────────────┘    └──────────────┘
```

---

## 🎯 设计原则

### 1. **松耦合**
- 各工具独立开发、测试、部署
- 工具之间通过标准MCP协议通信
- 工具可以独立升级，不影响其他工具

### 2. **可复用**
- 工具可以被多个应用使用
- 例如：COBOLParser可以用于代码分析、文档生成等

### 3. **可扩展**
- 容易添加新工具
- 支持工具的热插拔
- 支持工具的版本管理

### 4. **标准化**
- 统一的输入输出格式（JSON）
- 统一的错误处理机制
- 统一的日志格式

---

## 🧩 MCP工具分类

### 1. 核心工具层（Core Tools）

#### 1.1 COBOLParser MCP Tool

**职责:** COBOL源代码解析

**MCP接口:**
```json
{
  "tool": "cobol_parser",
  "version": "1.0.0",
  "input": {
    "cobol_code": "string",
    "file_name": "string (optional)",
    "encoding": "string (default: utf-8)",
    "parse_options": {
      "strict_mode": "boolean (default: false)",
      "expand_copy": "boolean (default: true)"
    }
  },
  "output": {
    "success": "boolean",
    "ast": {
      "program_id": "string",
      "divisions": "object"
    },
    "metadata": {
      "variables": "array",
      "procedures": "array",
      "file_controls": "array"
    },
    "errors": "array",
    "warnings": "array"
  }
}
```

#### 1.2 JavaGenerator MCP Tool

**职责:** Java代码生成

**MCP接口:**
```json
{
  "tool": "java_generator",
  "version": "1.0.0",
  "input": {
    "ast": "object",
    "metadata": "object",
    "patterns": "array (optional)",
    "best_practices": "array (optional)",
    "generation_options": {
      "class_name": "string (optional)",
      "package_name": "string (default: com.migration)",
      "style": "string (default: standard)",
      "add_comments": "boolean (default: true)",
      "add_javadoc": "boolean (default: true)"
    }
  },
  "output": {
    "success": "boolean",
    "java_code": "string",
    "class_name": "string",
    "package_name": "string",
    "imports": "array",
    "report": {
      "lines_of_code": "integer",
      "methods_count": "integer",
      "fields_count": "integer",
      "complexity": "float"
    },
    "warnings": "array",
    "mappings": "object"
  }
}
```

#### 1.3 CodeValidator MCP Tool

**职责:** 代码验证和质量评估

**MCP接口:**
```json
{
  "tool": "code_validator",
  "version": "1.0.0",
  "input": {
    "cobol_code": "string",
    "java_code": "string",
    "ast": "object",
    "metadata": "object",
    "validation_options": {
      "check_syntax": "boolean (default: true)",
      "check_semantics": "boolean (default: true)",
      "check_style": "boolean (default: true)",
      "strict_mode": "boolean (default: false)"
    }
  },
  "output": {
    "is_valid": "boolean",
    "score": "float (0-100)",
    "scores_breakdown": {
      "syntax": "float (0-30)",
      "semantics": "float (0-40)",
      "style": "float (0-20)",
      "performance": "float (0-10)"
    },
    "errors": "array",
    "warnings": "array",
    "suggestions": "array",
    "feedback": "string"
  }
}
```

---

### 2. 辅助工具层（Auxiliary Tools）

#### 2.1 SyntaxChecker MCP Tool

**职责:** 语法检查（Java编译检查）

**MCP接口:**
```json
{
  "tool": "syntax_checker",
  "version": "1.0.0",
  "input": {
    "java_code": "string",
    "language": "string (default: java)"
  },
  "output": {
    "has_errors": "boolean",
    "errors": "array",
    "warnings": "array"
  }
}
```

#### 2.2 StyleChecker MCP Tool

**职责:** 代码风格检查

**MCP接口:**
```json
{
  "tool": "style_checker",
  "version": "1.0.0",
  "input": {
    "java_code": "string",
    "style_guide": "string (default: google)"
  },
  "output": {
    "score": "float (0-100)",
    "violations": "array",
    "suggestions": "array"
  }
}
```

#### 2.3 TestGenerator MCP Tool

**职责:** 测试代码生成

**MCP接口:**
```json
{
  "tool": "test_generator",
  "version": "1.0.0",
  "input": {
    "java_code": "string",
    "test_framework": "string (default: junit5)"
  },
  "output": {
    "test_code": "string",
    "test_cases_count": "integer",
    "coverage_estimate": "float"
  }
}
```

#### 2.4 ComplexityAnalyzer MCP Tool

**职责:** 代码复杂度分析

**MCP接口:**
```json
{
  "tool": "complexity_analyzer",
  "version": "1.0.0",
  "input": {
    "code": "string",
    "language": "string"
  },
  "output": {
    "cyclomatic_complexity": "integer",
    "cognitive_complexity": "integer",
    "lines_of_code": "integer",
    "maintainability_index": "float"
  }
}
```

---

### 3. 基盤工具层（Foundation Tools）

#### 3.1 ReflectionPattern MCP Tool

**职责:** 反射模式编排（Generate → Evaluate → Improve）

**MCP接口:**
```json
{
  "tool": "reflection_pattern",
  "version": "1.0.0",
  "input": {
    "generator_tool": "string",
    "evaluator_tool": "string",
    "improver_tool": "string",
    "initial_input": "object",
    "max_iterations": "integer (default: 3)",
    "acceptance_threshold": "float (default: 85.0)"
  },
  "output": {
    "final_output": "object",
    "final_score": "float",
    "iterations": "integer",
    "history": "array",
    "is_acceptable": "boolean"
  }
}
```

#### 3.2 MemorySystem MCP Tool

**职责:** 记忆系统（存储和检索）

**MCP接口:**
```json
{
  "tool": "memory_system",
  "version": "1.0.0",
  "operations": {
    "remember": {
      "input": {
        "content": "string",
        "topic": "string",
        "metadata": "object"
      },
      "output": {
        "memory_id": "string",
        "success": "boolean"
      }
    },
    "recall": {
      "input": {
        "topic": "string (optional)",
        "query": "string (optional)",
        "limit": "integer (default: 10)",
        "min_similarity": "float (default: 0.0)"
      },
      "output": {
        "memories": "array",
        "count": "integer"
      }
    }
  }
}
```

#### 3.3 LLMClient MCP Tool

**职责:** LLM调用（用于代码改进、反馈生成等）

**MCP接口:**
```json
{
  "tool": "llm_client",
  "version": "1.0.0",
  "input": {
    "prompt": "string",
    "model": "string (optional)",
    "temperature": "float (default: 0.7)",
    "max_tokens": "integer (optional)"
  },
  "output": {
    "response": "string",
    "usage": {
      "prompt_tokens": "integer",
      "completion_tokens": "integer",
      "total_tokens": "integer"
    }
  }
}
```

---

## 🔄 工具调用流程

### 基本流程

```
1. Orchestrator接收COBOL代码
   ↓
2. 调用 COBOLParser MCP Tool
   ↓ (AST + Metadata)
3. 调用 MemorySystem MCP Tool (recall patterns)
   ↓ (Patterns)
4. 调用 ReflectionPattern MCP Tool
   ├─ 内部调用 JavaGenerator MCP Tool (generate)
   ├─ 内部调用 CodeValidator MCP Tool (evaluate)
   └─ 内部调用 JavaGenerator MCP Tool (improve)
   ↓ (Final Java Code + Score)
5. 调用 MemorySystem MCP Tool (remember result)
   ↓
6. 返回最终结果
```

---

## 🎯 下一步

1. ✅ MCP工具设计完成
2. ⏭️ 更新ARCHITECTURE.md（添加MCP层）
3. ⏭️ 实现各个MCP工具
4. ⏭️ 实现Orchestrator
5. ⏭️ 集成测试

