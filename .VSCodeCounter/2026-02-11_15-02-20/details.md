# Details

Date : 2026-02-11 15:02:20

Directory /home/liush/projects/serverlessAIAgents/apps

Total : 411 files,  75177 codes, 13261 comments, 11428 blanks, all 99866 lines

[Summary](results.md) / Details / [Diff Summary](diff.md) / [Diff Details](diff-details.md)

## Files
| filename | language | code | comment | blank | total |
| :--- | :--- | ---: | ---: | ---: | ---: |
| [apps/AGENTS.md](/apps/AGENTS.md) | Markdown | 7 | 0 | 5 | 12 |
| [apps/code\_migration\_assistant/AGENTS.md](/apps/code_migration_assistant/AGENTS.md) | Markdown | 7 | 0 | 5 | 12 |
| [apps/code\_migration\_assistant/README.md](/apps/code_migration_assistant/README.md) | Markdown | 253 | 0 | 77 | 330 |
| [apps/code\_migration\_assistant/\_\_init\_\_.py](/apps/code_migration_assistant/__init__.py) | Python | 6 | 17 | 5 | 28 |
| [apps/code\_migration\_assistant/adapters/\_\_init\_\_.py](/apps/code_migration_assistant/adapters/__init__.py) | Python | 30 | 21 | 4 | 55 |
| [apps/code\_migration\_assistant/adapters/base.py](/apps/code_migration_assistant/adapters/base.py) | Python | 53 | 94 | 23 | 170 |
| [apps/code\_migration\_assistant/adapters/factory.py](/apps/code_migration_assistant/adapters/factory.py) | Python | 135 | 103 | 42 | 280 |
| [apps/code\_migration\_assistant/adapters/skills.py](/apps/code_migration_assistant/adapters/skills.py) | Python | 39 | 3 | 4 | 46 |
| [apps/code\_migration\_assistant/adapters/source/\_\_init\_\_.py](/apps/code_migration_assistant/adapters/source/__init__.py) | Python | 2 | 1 | 5 | 8 |
| [apps/code\_migration\_assistant/adapters/source/cobol\_adapter.py](/apps/code_migration_assistant/adapters/source/cobol_adapter.py) | Python | 122 | 59 | 32 | 213 |
| [apps/code\_migration\_assistant/adapters/source/fortran\_adapter.py](/apps/code_migration_assistant/adapters/source/fortran_adapter.py) | Python | 291 | 74 | 53 | 418 |
| [apps/code\_migration\_assistant/adapters/source/pli\_adapter.py](/apps/code_migration_assistant/adapters/source/pli_adapter.py) | Python | 300 | 73 | 55 | 428 |
| [apps/code\_migration\_assistant/adapters/source/rpg\_adapter.py](/apps/code_migration_assistant/adapters/source/rpg_adapter.py) | Python | 323 | 75 | 62 | 460 |
| [apps/code\_migration\_assistant/adapters/target/\_\_init\_\_.py](/apps/code_migration_assistant/adapters/target/__init__.py) | Python | 2 | 1 | 5 | 8 |
| [apps/code\_migration\_assistant/adapters/target/java\_adapter.py](/apps/code_migration_assistant/adapters/target/java_adapter.py) | Python | 177 | 60 | 42 | 279 |
| [apps/code\_migration\_assistant/adapters/target/springboot\_adapter.py](/apps/code_migration_assistant/adapters/target/springboot_adapter.py) | Python | 83 | 29 | 15 | 127 |
| [apps/code\_migration\_assistant/agent.py](/apps/code_migration_assistant/agent.py) | Python | 179 | 145 | 56 | 380 |
| [apps/code\_migration\_assistant/agents/\_\_init\_\_.py](/apps/code_migration_assistant/agents/__init__.py) | Python | 36 | 2 | 2 | 40 |
| [apps/code\_migration\_assistant/agents/cards.py](/apps/code_migration_assistant/agents/cards.py) | Python | 171 | 8 | 8 | 187 |
| [apps/code\_migration\_assistant/agents/code\_transformation\_agent.py](/apps/code_migration_assistant/agents/code_transformation_agent.py) | Python | 93 | 11 | 17 | 121 |
| [apps/code\_migration\_assistant/agents/compliance\_reporter\_agent.py](/apps/code_migration_assistant/agents/compliance_reporter_agent.py) | Python | 47 | 7 | 8 | 62 |
| [apps/code\_migration\_assistant/agents/differential\_verification\_agent.py](/apps/code_migration_assistant/agents/differential_verification_agent.py) | Python | 263 | 12 | 41 | 316 |
| [apps/code\_migration\_assistant/agents/legacy\_analysis\_agent.py](/apps/code_migration_assistant/agents/legacy_analysis_agent.py) | Python | 85 | 10 | 19 | 114 |
| [apps/code\_migration\_assistant/agents/limited\_fixer\_agent.py](/apps/code_migration_assistant/agents/limited_fixer_agent.py) | Python | 81 | 10 | 21 | 112 |
| [apps/code\_migration\_assistant/agents/migration\_design\_agent.py](/apps/code_migration_assistant/agents/migration_design_agent.py) | Python | 74 | 6 | 17 | 97 |
| [apps/code\_migration\_assistant/agents/prompts.py](/apps/code_migration_assistant/agents/prompts.py) | Python | 39 | 19 | 14 | 72 |
| [apps/code\_migration\_assistant/agents/quality\_gate\_agent.py](/apps/code_migration_assistant/agents/quality_gate_agent.py) | Python | 140 | 6 | 26 | 172 |
| [apps/code\_migration\_assistant/agents/test\_synthesis\_agent.py](/apps/code_migration_assistant/agents/test_synthesis_agent.py) | Python | 53 | 4 | 14 | 71 |
| [apps/code\_migration\_assistant/api.py](/apps/code_migration_assistant/api.py) | Python | 576 | 116 | 70 | 762 |
| [apps/code\_migration\_assistant/backend/app.py](/apps/code_migration_assistant/backend/app.py) | Python | 177 | 22 | 38 | 237 |
| [apps/code\_migration\_assistant/cli.py](/apps/code_migration_assistant/cli.py) | Python | 96 | 24 | 29 | 149 |
| [apps/code\_migration\_assistant/config/migration\_types.yaml](/apps/code_migration_assistant/config/migration_types.yaml) | YAML | 44 | 56 | 6 | 106 |
| [apps/code\_migration\_assistant/config/type\_mappings/cobol\_java\_types.yaml](/apps/code_migration_assistant/config/type_mappings/cobol_java_types.yaml) | YAML | 78 | 14 | 19 | 111 |
| [apps/code\_migration\_assistant/docs/DESIGN.md](/apps/code_migration_assistant/docs/DESIGN.md) | Markdown | 130 | 0 | 38 | 168 |
| [apps/code\_migration\_assistant/docs/architecture.md](/apps/code_migration_assistant/docs/architecture.md) | Markdown | 46 | 0 | 13 | 59 |
| [apps/code\_migration\_assistant/docs/plan/agents改善.md](/apps/code_migration_assistant/docs/plan/agents%E6%94%B9%E5%96%84.md) | Markdown | 210 | 0 | 85 | 295 |
| [apps/code\_migration\_assistant/engine.py](/apps/code_migration_assistant/engine.py) | Python | 721 | 81 | 117 | 919 |
| [apps/code\_migration\_assistant/examples/Calculator.java](/apps/code_migration_assistant/examples/Calculator.java) | Java | 9 | 16 | 3 | 28 |
| [apps/code\_migration\_assistant/examples/EmployeeManager.java](/apps/code_migration_assistant/examples/EmployeeManager.java) | Java | 18 | 38 | 4 | 60 |
| [apps/code\_migration\_assistant/frontend/app.js](/apps/code_migration_assistant/frontend/app.js) | JavaScript | 170 | 10 | 29 | 209 |
| [apps/code\_migration\_assistant/frontend/index.html](/apps/code_migration_assistant/frontend/index.html) | HTML | 173 | 7 | 12 | 192 |
| [apps/code\_migration\_assistant/frontend/style.css](/apps/code_migration_assistant/frontend/style.css) | PostCSS | 452 | 11 | 74 | 537 |
| [apps/code\_migration\_assistant/frontend/webapp.py](/apps/code_migration_assistant/frontend/webapp.py) | Python | 110 | 16 | 29 | 155 |
| [apps/code\_migration\_assistant/i18n.py](/apps/code_migration_assistant/i18n.py) | Python | 27 | 2 | 9 | 38 |
| [apps/code\_migration\_assistant/locales/en.json](/apps/code_migration_assistant/locales/en.json) | JSON | 13 | 0 | 0 | 13 |
| [apps/code\_migration\_assistant/locales/ja.json](/apps/code_migration_assistant/locales/ja.json) | JSON | 13 | 0 | 0 | 13 |
| [apps/code\_migration\_assistant/locales/zh.json](/apps/code_migration_assistant/locales/zh.json) | JSON | 13 | 0 | 0 | 13 |
| [apps/code\_migration\_assistant/mcp\_tools/\_\_init\_\_.py](/apps/code_migration_assistant/mcp_tools/__init__.py) | Python | 16 | 12 | 5 | 33 |
| [apps/code\_migration\_assistant/mcp\_tools/cobol\_parser.py](/apps/code_migration_assistant/mcp_tools/cobol_parser.py) | Python | 162 | 131 | 45 | 338 |
| [apps/code\_migration\_assistant/mcp\_tools/code\_validator.py](/apps/code_migration_assistant/mcp_tools/code_validator.py) | Python | 189 | 131 | 52 | 372 |
| [apps/code\_migration\_assistant/mcp\_tools/java\_generator.py](/apps/code_migration_assistant/mcp_tools/java_generator.py) | Python | 202 | 154 | 57 | 413 |
| [apps/code\_migration\_assistant/mcp\_tools/memory\_system.py](/apps/code_migration_assistant/mcp_tools/memory_system.py) | Python | 77 | 77 | 25 | 179 |
| [apps/code\_migration\_assistant/mcp\_tools/pdf\_exporter.py](/apps/code_migration_assistant/mcp_tools/pdf_exporter.py) | Python | 40 | 16 | 11 | 67 |
| [apps/code\_migration\_assistant/mcp\_tools/reflection\_pattern.py](/apps/code_migration_assistant/mcp_tools/reflection_pattern.py) | Python | 126 | 82 | 31 | 239 |
| [apps/code\_migration\_assistant/orchestrator.py](/apps/code_migration_assistant/orchestrator.py) | Python | 138 | 142 | 28 | 308 |
| [apps/code\_migration\_assistant/parsers/\_\_init\_\_.py](/apps/code_migration_assistant/parsers/__init__.py) | Python | 2 | 4 | 5 | 11 |
| [apps/code\_migration\_assistant/parsers/ply\_cobol\_parser.py](/apps/code_migration_assistant/parsers/ply_cobol_parser.py) | Python | 94 | 65 | 27 | 186 |
| [apps/code\_migration\_assistant/run\_server.sh](/apps/code_migration_assistant/run_server.sh) | Shell Script | 8 | 3 | 5 | 16 |
| [apps/code\_migration\_assistant/skills/\_\_init\_\_.py](/apps/code_migration_assistant/skills/__init__.py) | Python | 3 | 9 | 5 | 17 |
| [apps/code\_migration\_assistant/skills/business-semantics/SKILL.md](/apps/code_migration_assistant/skills/business-semantics/SKILL.md) | Markdown | 107 | 0 | 19 | 126 |
| [apps/code\_migration\_assistant/skills/cobol-migration/SKILL.md](/apps/code_migration_assistant/skills/cobol-migration/SKILL.md) | Markdown | 110 | 0 | 32 | 142 |
| [apps/code\_migration\_assistant/skills/cobol-migration/\_\_init\_\_.py](/apps/code_migration_assistant/skills/cobol-migration/__init__.py) | Python | 126 | 57 | 34 | 217 |
| [apps/code\_migration\_assistant/skills/compliance-reporter/SKILL.md](/apps/code_migration_assistant/skills/compliance-reporter/SKILL.md) | Markdown | 136 | 0 | 25 | 161 |
| [apps/code\_migration\_assistant/skills/legacy-ingestion/SKILL.md](/apps/code_migration_assistant/skills/legacy-ingestion/SKILL.md) | Markdown | 110 | 0 | 15 | 125 |
| [apps/code\_migration\_assistant/skills/modernization-generator/SKILL.md](/apps/code_migration_assistant/skills/modernization-generator/SKILL.md) | Markdown | 128 | 0 | 18 | 146 |
| [apps/code\_migration\_assistant/skills/pdf-export/SKILL.md](/apps/code_migration_assistant/skills/pdf-export/SKILL.md) | Markdown | 17 | 0 | 5 | 22 |
| [apps/code\_migration\_assistant/specs/TaskSpec.yaml](/apps/code_migration_assistant/specs/TaskSpec.yaml) | YAML | 10 | 2 | 1 | 13 |
| [apps/code\_migration\_assistant/specs/known\_legacy\_issues.json](/apps/code_migration_assistant/specs/known_legacy_issues.json) | JSON | 1 | 0 | 1 | 2 |
| [apps/code\_migration\_assistant/specs/schemas/differential\_verification.schema.json](/apps/code_migration_assistant/specs/schemas/differential_verification.schema.json) | JSON | 26 | 0 | 1 | 27 |
| [apps/code\_migration\_assistant/specs/schemas/legacy\_analysis.schema.json](/apps/code_migration_assistant/specs/schemas/legacy_analysis.schema.json) | JSON | 30 | 0 | 1 | 31 |
| [apps/code\_migration\_assistant/specs/schemas/limited\_fix.schema.json](/apps/code_migration_assistant/specs/schemas/limited_fix.schema.json) | JSON | 24 | 0 | 1 | 25 |
| [apps/code\_migration\_assistant/specs/schemas/migration\_design.schema.json](/apps/code_migration_assistant/specs/schemas/migration_design.schema.json) | JSON | 28 | 0 | 1 | 29 |
| [apps/code\_migration\_assistant/specs/schemas/quality\_gate.schema.json](/apps/code_migration_assistant/specs/schemas/quality_gate.schema.json) | JSON | 26 | 0 | 1 | 27 |
| [apps/code\_migration\_assistant/specs/schemas/test\_synthesis.schema.json](/apps/code_migration_assistant/specs/schemas/test_synthesis.schema.json) | JSON | 22 | 0 | 1 | 23 |
| [apps/code\_migration\_assistant/specs/schemas/transformation.schema.json](/apps/code_migration_assistant/specs/schemas/transformation.schema.json) | JSON | 24 | 0 | 1 | 25 |
| [apps/code\_migration\_assistant/test\_cli\_integration.py](/apps/code_migration_assistant/test_cli_integration.py) | Python | 113 | 18 | 42 | 173 |
| [apps/code\_migration\_assistant/tests/\_\_init\_\_.py](/apps/code_migration_assistant/tests/__init__.py) | Python | 0 | 1 | 2 | 3 |
| [apps/code\_migration\_assistant/tests/test\_cobol\_parser.py](/apps/code_migration_assistant/tests/test_cobol_parser.py) | Python | 116 | 7 | 35 | 158 |
| [apps/code\_migration\_assistant/tests/test\_integration.py](/apps/code_migration_assistant/tests/test_integration.py) | Python | 164 | 9 | 40 | 213 |
| [apps/code\_migration\_assistant/tests/test\_pipeline\_v4.py](/apps/code_migration_assistant/tests/test_pipeline_v4.py) | Python | 96 | 5 | 19 | 120 |
| [apps/code\_migration\_assistant/tests/test\_unit\_v4.py](/apps/code_migration_assistant/tests/test_unit_v4.py) | Python | 78 | 7 | 27 | 112 |
| [apps/code\_migration\_assistant/tools/\_\_init\_\_.py](/apps/code_migration_assistant/tools/__init__.py) | Python | 10 | 4 | 5 | 19 |
| [apps/code\_migration\_assistant/tools/junit\_runner.py](/apps/code_migration_assistant/tools/junit_runner.py) | Python | 226 | 57 | 53 | 336 |
| [apps/code\_migration\_assistant/verify\_implementation.py](/apps/code_migration_assistant/verify_implementation.py) | Python | 117 | 24 | 49 | 190 |
| [apps/code\_migration\_assistant/workflow/\_\_init\_\_.py](/apps/code_migration_assistant/workflow/__init__.py) | Python | 24 | 2 | 3 | 29 |
| [apps/code\_migration\_assistant/workflow/artifacts.py](/apps/code_migration_assistant/workflow/artifacts.py) | Python | 90 | 23 | 18 | 131 |
| [apps/code\_migration\_assistant/workflow/models.py](/apps/code_migration_assistant/workflow/models.py) | Python | 100 | 21 | 47 | 168 |
| [apps/decision\_governance\_engine/.github/workflows/ci.yml](/apps/decision_governance_engine/.github/workflows/ci.yml) | YAML | 66 | 50 | 10 | 126 |
| [apps/decision\_governance\_engine/AGENTS.md](/apps/decision_governance_engine/AGENTS.md) | Markdown | 7 | 0 | 5 | 12 |
| [apps/decision\_governance\_engine/CHANGELOG\_v2.0.md](/apps/decision_governance_engine/CHANGELOG_v2.0.md) | Markdown | 199 | 0 | 54 | 253 |
| [apps/decision\_governance\_engine/CHANGELOG\_v3.0.md](/apps/decision_governance_engine/CHANGELOG_v3.0.md) | Markdown | 220 | 0 | 62 | 282 |
| [apps/decision\_governance\_engine/Dockerfile](/apps/decision_governance_engine/Dockerfile) | Docker | 27 | 33 | 18 | 78 |
| [apps/decision\_governance\_engine/README.md](/apps/decision_governance_engine/README.md) | Markdown | 353 | 0 | 109 | 462 |
| [apps/decision\_governance\_engine/\_\_init\_\_.py](/apps/decision_governance_engine/__init__.py) | Python | 4 | 21 | 6 | 31 |
| [apps/decision\_governance\_engine/agent.yaml](/apps/decision_governance_engine/agent.yaml) | YAML | 313 | 9 | 16 | 338 |
| [apps/decision\_governance\_engine/agents/\_\_init\_\_.py](/apps/decision_governance_engine/agents/__init__.py) | Python | 18 | 5 | 4 | 27 |
| [apps/decision\_governance\_engine/agents/agent\_definitions.yaml](/apps/decision_governance_engine/agents/agent_definitions.yaml) | YAML | 128 | 15 | 12 | 155 |
| [apps/decision\_governance\_engine/agents/clarification\_agent.py](/apps/decision_governance_engine/agents/clarification_agent.py) | Python | 315 | 30 | 18 | 363 |
| [apps/decision\_governance\_engine/agents/cognitive\_gate\_agent.py](/apps/decision_governance_engine/agents/cognitive_gate_agent.py) | Python | 225 | 124 | 17 | 366 |
| [apps/decision\_governance\_engine/agents/dao\_agent.py](/apps/decision_governance_engine/agents/dao_agent.py) | Python | 640 | 85 | 31 | 756 |
| [apps/decision\_governance\_engine/agents/fa\_agent.py](/apps/decision_governance_engine/agents/fa_agent.py) | Python | 544 | 78 | 22 | 644 |
| [apps/decision\_governance\_engine/agents/gatekeeper\_agent.py](/apps/decision_governance_engine/agents/gatekeeper_agent.py) | Python | 202 | 53 | 31 | 286 |
| [apps/decision\_governance\_engine/agents/qi\_agent.py](/apps/decision_governance_engine/agents/qi_agent.py) | Python | 155 | 120 | 28 | 303 |
| [apps/decision\_governance\_engine/agents/rag\_config\_helper.py](/apps/decision_governance_engine/agents/rag_config_helper.py) | Python | 53 | 47 | 21 | 121 |
| [apps/decision\_governance\_engine/agents/review\_agent.py](/apps/decision_governance_engine/agents/review_agent.py) | Python | 246 | 39 | 25 | 310 |
| [apps/decision\_governance\_engine/agents/shu\_agent.py](/apps/decision_governance_engine/agents/shu_agent.py) | Python | 502 | 129 | 35 | 666 |
| [apps/decision\_governance\_engine/alembic.ini](/apps/decision_governance_engine/alembic.ini) | Ini | 125 | 0 | 28 | 153 |
| [apps/decision\_governance\_engine/api.py](/apps/decision_governance_engine/api.py) | Python | 59 | 31 | 22 | 112 |
| [apps/decision\_governance\_engine/config/\_\_init\_\_.py](/apps/decision_governance_engine/config/__init__.py) | Python | 71 | 44 | 39 | 154 |
| [apps/decision\_governance\_engine/config/decision\_weights.yaml](/apps/decision_governance_engine/config/decision_weights.yaml) | YAML | 190 | 35 | 29 | 254 |
| [apps/decision\_governance\_engine/config/human\_review\_policy.yaml](/apps/decision_governance_engine/config/human_review_policy.yaml) | YAML | 7 | 3 | 4 | 14 |
| [apps/decision\_governance\_engine/db/init/01\_schema.sql](/apps/decision_governance_engine/db/init/01_schema.sql) | MS SQL | 75 | 25 | 23 | 123 |
| [apps/decision\_governance\_engine/design/decision-agent-spec.md](/apps/decision_governance_engine/design/decision-agent-spec.md) | Markdown | 863 | 0 | 127 | 990 |
| [apps/decision\_governance\_engine/design/decision-input-ui.tsx](/apps/decision_governance_engine/design/decision-input-ui.tsx) | TypeScript JSX | 284 | 14 | 23 | 321 |
| [apps/decision\_governance\_engine/design/decision-processing-ui.tsx](/apps/decision_governance_engine/design/decision-processing-ui.tsx) | TypeScript JSX | 216 | 9 | 18 | 243 |
| [apps/decision\_governance\_engine/design/decision-report-ui.tsx](/apps/decision_governance_engine/design/decision-report-ui.tsx) | TypeScript JSX | 313 | 10 | 27 | 350 |
| [apps/decision\_governance\_engine/docker-compose.dev.yml](/apps/decision_governance_engine/docker-compose.dev.yml) | YAML | 33 | 37 | 4 | 74 |
| [apps/decision\_governance\_engine/docker-compose.yml](/apps/decision_governance_engine/docker-compose.yml) | YAML | 107 | 48 | 8 | 163 |
| [apps/decision\_governance\_engine/docs/SCORING\_SYSTEM.md](/apps/decision_governance_engine/docs/SCORING_SYSTEM.md) | Markdown | 289 | 0 | 75 | 364 |
| [apps/decision\_governance\_engine/engine.py](/apps/decision_governance_engine/engine.py) | Python | 126 | 121 | 30 | 277 |
| [apps/decision\_governance\_engine/flow\_config.py](/apps/decision_governance_engine/flow_config.py) | Python | 22 | 22 | 14 | 58 |
| [apps/decision\_governance\_engine/frontend/Dockerfile](/apps/decision_governance_engine/frontend/Dockerfile) | Docker | 25 | 35 | 18 | 78 |
| [apps/decision\_governance\_engine/frontend/eslint.config.js](/apps/decision_governance_engine/frontend/eslint.config.js) | JavaScript | 61 | 15 | 12 | 88 |
| [apps/decision\_governance\_engine/frontend/index.html](/apps/decision_governance_engine/frontend/index.html) | HTML | 13 | 0 | 2 | 15 |
| [apps/decision\_governance\_engine/frontend/nginx/nginx.conf](/apps/decision_governance_engine/frontend/nginx/nginx.conf) | Properties | 82 | 45 | 28 | 155 |
| [apps/decision\_governance\_engine/frontend/package-lock.json](/apps/decision_governance_engine/frontend/package-lock.json) | JSON | 5,316 | 0 | 1 | 5,317 |
| [apps/decision\_governance\_engine/frontend/package.json](/apps/decision_governance_engine/frontend/package.json) | JSON | 41 | 0 | 1 | 42 |
| [apps/decision\_governance\_engine/frontend/postcss.config.js](/apps/decision_governance_engine/frontend/postcss.config.js) | JavaScript | 6 | 0 | 2 | 8 |
| [apps/decision\_governance\_engine/frontend/public/favicon.svg](/apps/decision_governance_engine/frontend/public/favicon.svg) | XML | 3 | 0 | 2 | 5 |
| [apps/decision\_governance\_engine/frontend/src/App.tsx](/apps/decision_governance_engine/frontend/src/App.tsx) | TypeScript JSX | 108 | 24 | 19 | 151 |
| [apps/decision\_governance\_engine/frontend/src/\_\_tests\_\_/api/client.test.ts](/apps/decision_governance_engine/frontend/src/__tests__/api/client.test.ts) | TypeScript | 137 | 17 | 32 | 186 |
| [apps/decision\_governance\_engine/frontend/src/\_\_tests\_\_/setup.ts](/apps/decision_governance_engine/frontend/src/__tests__/setup.ts) | TypeScript | 33 | 15 | 13 | 61 |
| [apps/decision\_governance\_engine/frontend/src/\_\_tests\_\_/store/useDecisionStore.test.ts](/apps/decision_governance_engine/frontend/src/__tests__/store/useDecisionStore.test.ts) | TypeScript | 104 | 8 | 23 | 135 |
| [apps/decision\_governance\_engine/frontend/src/api/client.ts](/apps/decision_governance_engine/frontend/src/api/client.ts) | TypeScript | 347 | 137 | 67 | 551 |
| [apps/decision\_governance\_engine/frontend/src/components/DecisionInputPage.tsx](/apps/decision_governance_engine/frontend/src/components/DecisionInputPage.tsx) | TypeScript JSX | 416 | 35 | 35 | 486 |
| [apps/decision\_governance\_engine/frontend/src/components/HankoSeal.tsx](/apps/decision_governance_engine/frontend/src/components/HankoSeal.tsx) | TypeScript JSX | 224 | 45 | 20 | 289 |
| [apps/decision\_governance\_engine/frontend/src/components/HistoryPage.tsx](/apps/decision_governance_engine/frontend/src/components/HistoryPage.tsx) | TypeScript JSX | 276 | 27 | 27 | 330 |
| [apps/decision\_governance\_engine/frontend/src/components/KnowledgePage.tsx](/apps/decision_governance_engine/frontend/src/components/KnowledgePage.tsx) | TypeScript JSX | 191 | 18 | 16 | 225 |
| [apps/decision\_governance\_engine/frontend/src/components/LoginPage.tsx](/apps/decision_governance_engine/frontend/src/components/LoginPage.tsx) | TypeScript JSX | 188 | 20 | 18 | 226 |
| [apps/decision\_governance\_engine/frontend/src/components/ProcessingPage.tsx](/apps/decision_governance_engine/frontend/src/components/ProcessingPage.tsx) | TypeScript JSX | 345 | 41 | 34 | 420 |
| [apps/decision\_governance\_engine/frontend/src/components/ReportPage.tsx](/apps/decision_governance_engine/frontend/src/components/ReportPage.tsx) | TypeScript JSX | 1,578 | 99 | 114 | 1,791 |
| [apps/decision\_governance\_engine/frontend/src/components/SettingsModal.tsx](/apps/decision_governance_engine/frontend/src/components/SettingsModal.tsx) | TypeScript JSX | 211 | 24 | 21 | 256 |
| [apps/decision\_governance\_engine/frontend/src/hooks/useDecisionStream.ts](/apps/decision_governance_engine/frontend/src/hooks/useDecisionStream.ts) | TypeScript | 522 | 77 | 55 | 654 |
| [apps/decision\_governance\_engine/frontend/src/index.css](/apps/decision_governance_engine/frontend/src/index.css) | PostCSS | 3 | 0 | 2 | 5 |
| [apps/decision\_governance\_engine/frontend/src/main.tsx](/apps/decision_governance_engine/frontend/src/main.tsx) | TypeScript JSX | 9 | 3 | 4 | 16 |
| [apps/decision\_governance\_engine/frontend/src/store/useAuthStore.ts](/apps/decision_governance_engine/frontend/src/store/useAuthStore.ts) | TypeScript | 136 | 23 | 17 | 176 |
| [apps/decision\_governance\_engine/frontend/src/store/useDecisionStore.ts](/apps/decision_governance_engine/frontend/src/store/useDecisionStore.ts) | TypeScript | 161 | 37 | 35 | 233 |
| [apps/decision\_governance\_engine/frontend/src/types/index.ts](/apps/decision_governance_engine/frontend/src/types/index.ts) | TypeScript | 360 | 71 | 55 | 486 |
| [apps/decision\_governance\_engine/frontend/src/vite-env.d.ts](/apps/decision_governance_engine/frontend/src/vite-env.d.ts) | TypeScript | 6 | 8 | 4 | 18 |
| [apps/decision\_governance\_engine/frontend/tailwind.config.js](/apps/decision_governance_engine/frontend/tailwind.config.js) | JavaScript | 7 | 1 | 2 | 10 |
| [apps/decision\_governance\_engine/frontend/tests/e2e/helpers/assertions.ts](/apps/decision_governance_engine/frontend/tests/e2e/helpers/assertions.ts) | TypeScript | 31 | 0 | 7 | 38 |
| [apps/decision\_governance\_engine/frontend/tests/e2e/helpers/mocks.ts](/apps/decision_governance_engine/frontend/tests/e2e/helpers/mocks.ts) | TypeScript | 564 | 0 | 76 | 640 |
| [apps/decision\_governance\_engine/frontend/tests/e2e/history.spec.ts](/apps/decision_governance_engine/frontend/tests/e2e/history.spec.ts) | TypeScript | 30 | 0 | 14 | 44 |
| [apps/decision\_governance\_engine/frontend/tests/e2e/knowledge.spec.ts](/apps/decision_governance_engine/frontend/tests/e2e/knowledge.spec.ts) | TypeScript | 45 | 0 | 18 | 63 |
| [apps/decision\_governance\_engine/frontend/tests/e2e/processing.spec.ts](/apps/decision_governance_engine/frontend/tests/e2e/processing.spec.ts) | TypeScript | 131 | 0 | 24 | 155 |
| [apps/decision\_governance\_engine/frontend/tests/e2e/report.spec.ts](/apps/decision_governance_engine/frontend/tests/e2e/report.spec.ts) | TypeScript | 128 | 0 | 22 | 150 |
| [apps/decision\_governance\_engine/frontend/tests/e2e/settings.spec.ts](/apps/decision_governance_engine/frontend/tests/e2e/settings.spec.ts) | TypeScript | 32 | 0 | 14 | 46 |
| [apps/decision\_governance\_engine/frontend/tests/e2e/smoke-login.spec.ts](/apps/decision_governance_engine/frontend/tests/e2e/smoke-login.spec.ts) | TypeScript | 29 | 0 | 11 | 40 |
| [apps/decision\_governance\_engine/frontend/tsconfig.json](/apps/decision_governance_engine/frontend/tsconfig.json) | JSON with Comments | 24 | 0 | 2 | 26 |
| [apps/decision\_governance\_engine/frontend/tsconfig.node.json](/apps/decision_governance_engine/frontend/tsconfig.node.json) | JSON | 11 | 0 | 2 | 13 |
| [apps/decision\_governance\_engine/frontend/vite.config.ts](/apps/decision_governance_engine/frontend/vite.config.ts) | TypeScript | 53 | 21 | 4 | 78 |
| [apps/decision\_governance\_engine/knowledge/case\_studies/README.md](/apps/decision_governance_engine/knowledge/case_studies/README.md) | Markdown | 15 | 0 | 7 | 22 |
| [apps/decision\_governance\_engine/knowledge/compliance/README.md](/apps/decision_governance_engine/knowledge/compliance/README.md) | Markdown | 16 | 0 | 7 | 23 |
| [apps/decision\_governance\_engine/knowledge/industry\_practices/README.md](/apps/decision_governance_engine/knowledge/industry_practices/README.md) | Markdown | 15 | 0 | 7 | 22 |
| [apps/decision\_governance\_engine/knowledge/tech\_docs/README.md](/apps/decision_governance_engine/knowledge/tech_docs/README.md) | Markdown | 15 | 0 | 7 | 22 |
| [apps/decision\_governance\_engine/main.py](/apps/decision_governance_engine/main.py) | Python | 116 | 31 | 30 | 177 |
| [apps/decision\_governance\_engine/migrations/env.py](/apps/decision_governance_engine/migrations/env.py) | Python | 43 | 45 | 18 | 106 |
| [apps/decision\_governance\_engine/migrations/versions/20260118\_0001\_initial\_schema.py](/apps/decision_governance_engine/migrations/versions/20260118_0001_initial_schema.py) | Python | 90 | 23 | 15 | 128 |
| [apps/decision\_governance\_engine/migrations/versions/20260202\_1842\_8bf2ccdc3fc1\_add\_progressive\_save\_fields.py](/apps/decision_governance_engine/migrations/versions/20260202_1842_8bf2ccdc3fc1_add_progressive_save_fields.py) | Python | 31 | 18 | 10 | 59 |
| [apps/decision\_governance\_engine/migrations/versions/20260210\_2100\_add\_report\_case\_id\_and\_human\_review\_records.py](/apps/decision_governance_engine/migrations/versions/20260210_2100_add_report_case_id_and_human_review_records.py) | Python | 20 | 32 | 10 | 62 |
| [apps/decision\_governance\_engine/prompts/\_\_init\_\_.py](/apps/decision_governance_engine/prompts/__init__.py) | Python | 28 | 46 | 18 | 92 |
| [apps/decision\_governance\_engine/prompts/cognitive\_gate.md](/apps/decision_governance_engine/prompts/cognitive_gate.md) | Markdown | 59 | 0 | 17 | 76 |
| [apps/decision\_governance\_engine/prompts/common\_system.md](/apps/decision_governance_engine/prompts/common_system.md) | Markdown | 21 | 0 | 10 | 31 |
| [apps/decision\_governance\_engine/prompts/dao.md](/apps/decision_governance_engine/prompts/dao.md) | Markdown | 62 | 0 | 22 | 84 |
| [apps/decision\_governance\_engine/prompts/fa.md](/apps/decision_governance_engine/prompts/fa.md) | Markdown | 91 | 0 | 21 | 112 |
| [apps/decision\_governance\_engine/prompts/qi.md](/apps/decision_governance_engine/prompts/qi.md) | Markdown | 102 | 0 | 20 | 122 |
| [apps/decision\_governance\_engine/prompts/review.md](/apps/decision_governance_engine/prompts/review.md) | Markdown | 101 | 0 | 23 | 124 |
| [apps/decision\_governance\_engine/prompts/shu.md](/apps/decision_governance_engine/prompts/shu.md) | Markdown | 87 | 0 | 20 | 107 |
| [apps/decision\_governance\_engine/repositories/\_\_init\_\_.py](/apps/decision_governance_engine/repositories/__init__.py) | Python | 22 | 5 | 5 | 32 |
| [apps/decision\_governance\_engine/repositories/database.py](/apps/decision_governance_engine/repositories/database.py) | Python | 86 | 37 | 31 | 154 |
| [apps/decision\_governance\_engine/repositories/decision\_repository.py](/apps/decision_governance_engine/repositories/decision_repository.py) | Python | 243 | 58 | 32 | 333 |
| [apps/decision\_governance\_engine/repositories/models.py](/apps/decision_governance_engine/repositories/models.py) | Python | 78 | 35 | 27 | 140 |
| [apps/decision\_governance\_engine/routers/\_\_init\_\_.py](/apps/decision_governance_engine/routers/__init__.py) | Python | 26 | 12 | 5 | 43 |
| [apps/decision\_governance\_engine/routers/auth.py](/apps/decision_governance_engine/routers/auth.py) | Python | 110 | 33 | 41 | 184 |
| [apps/decision\_governance\_engine/routers/config.py](/apps/decision_governance_engine/routers/config.py) | Python | 260 | 77 | 87 | 424 |
| [apps/decision\_governance\_engine/routers/decision.py](/apps/decision_governance_engine/routers/decision.py) | Python | 601 | 122 | 109 | 832 |
| [apps/decision\_governance\_engine/routers/human\_review.py](/apps/decision_governance_engine/routers/human_review.py) | Python | 397 | 55 | 52 | 504 |
| [apps/decision\_governance\_engine/routers/knowledge.py](/apps/decision_governance_engine/routers/knowledge.py) | Python | 92 | 48 | 40 | 180 |
| [apps/decision\_governance\_engine/routers/product\_launch.py](/apps/decision_governance_engine/routers/product_launch.py) | Python | 97 | 42 | 33 | 172 |
| [apps/decision\_governance\_engine/routers/report.py](/apps/decision_governance_engine/routers/report.py) | Python | 223 | 65 | 60 | 348 |
| [apps/decision\_governance\_engine/routers/workflow.py](/apps/decision_governance_engine/routers/workflow.py) | Python | 63 | 38 | 29 | 130 |
| [apps/decision\_governance\_engine/schemas/\_\_init\_\_.py](/apps/decision_governance_engine/schemas/__init__.py) | Python | 70 | 8 | 5 | 83 |
| [apps/decision\_governance\_engine/schemas/agent\_schemas.py](/apps/decision_governance_engine/schemas/agent_schemas.py) | Python | 416 | 148 | 192 | 756 |
| [apps/decision\_governance\_engine/schemas/contract\_schemas.py](/apps/decision_governance_engine/schemas/contract_schemas.py) | Python | 70 | 40 | 32 | 142 |
| [apps/decision\_governance\_engine/schemas/input\_schemas.py](/apps/decision_governance_engine/schemas/input_schemas.py) | Python | 109 | 18 | 23 | 150 |
| [apps/decision\_governance\_engine/schemas/output\_schemas.py](/apps/decision_governance_engine/schemas/output_schemas.py) | Python | 255 | 71 | 45 | 371 |
| [apps/decision\_governance\_engine/services/agent\_registry.py](/apps/decision_governance_engine/services/agent_registry.py) | Python | 162 | 167 | 45 | 374 |
| [apps/decision\_governance\_engine/services/decision\_contract\_builder.py](/apps/decision_governance_engine/services/decision_contract_builder.py) | Python | 103 | 54 | 24 | 181 |
| [apps/decision\_governance\_engine/services/decision\_report\_builder.py](/apps/decision_governance_engine/services/decision_report_builder.py) | Python | 88 | 55 | 24 | 167 |
| [apps/decision\_governance\_engine/services/deep\_agent\_adapter.py](/apps/decision_governance_engine/services/deep_agent_adapter.py) | Python | 194 | 135 | 24 | 353 |
| [apps/decision\_governance\_engine/services/human\_review\_policy.py](/apps/decision_governance_engine/services/human_review_policy.py) | Python | 48 | 9 | 17 | 74 |
| [apps/decision\_governance\_engine/services/intelligence\_service.py](/apps/decision_governance_engine/services/intelligence_service.py) | Python | 217 | 89 | 62 | 368 |
| [apps/decision\_governance\_engine/services/pdf\_generator.py](/apps/decision_governance_engine/services/pdf_generator.py) | Python | 958 | 76 | 73 | 1,107 |
| [apps/decision\_governance\_engine/services/report\_generator.py](/apps/decision_governance_engine/services/report_generator.py) | Python | 112 | 74 | 23 | 209 |
| [apps/decision\_governance\_engine/services/rich\_report\_builder.py](/apps/decision_governance_engine/services/rich_report_builder.py) | Python | 106 | 246 | 28 | 380 |
| [apps/decision\_governance\_engine/services/scoring\_engine.py](/apps/decision_governance_engine/services/scoring_engine.py) | Python | 165 | 76 | 52 | 293 |
| [apps/decision\_governance\_engine/services/ui\_components.py](/apps/decision_governance_engine/services/ui_components.py) | Python | 277 | 64 | 71 | 412 |
| [apps/decision\_governance\_engine/skills/clarification/SKILL.md](/apps/decision_governance_engine/skills/clarification/SKILL.md) | Markdown | 213 | 0 | 28 | 241 |
| [apps/decision\_governance\_engine/skills/dao/SKILL.md](/apps/decision_governance_engine/skills/dao/SKILL.md) | Markdown | 306 | 0 | 40 | 346 |
| [apps/decision\_governance\_engine/skills/fa/SKILL.md](/apps/decision_governance_engine/skills/fa/SKILL.md) | Markdown | 301 | 0 | 39 | 340 |
| [apps/decision\_governance\_engine/skills/gatekeeper/SKILL.md](/apps/decision_governance_engine/skills/gatekeeper/SKILL.md) | Markdown | 97 | 0 | 11 | 108 |
| [apps/decision\_governance\_engine/skills/qi/SKILL.md](/apps/decision_governance_engine/skills/qi/SKILL.md) | Markdown | 181 | 0 | 19 | 200 |
| [apps/decision\_governance\_engine/skills/review/SKILL.md](/apps/decision_governance_engine/skills/review/SKILL.md) | Markdown | 187 | 0 | 26 | 213 |
| [apps/decision\_governance\_engine/skills/shu/SKILL.md](/apps/decision_governance_engine/skills/shu/SKILL.md) | Markdown | 325 | 0 | 31 | 356 |
| [apps/decision\_governance\_engine/skills/utils/SKILL.md](/apps/decision_governance_engine/skills/utils/SKILL.md) | Markdown | 244 | 0 | 55 | 299 |
| [apps/decision\_governance\_engine/startup.py](/apps/decision_governance_engine/startup.py) | Python | 31 | 25 | 15 | 71 |
| [apps/decision\_governance\_engine/tests/\_\_init\_\_.py](/apps/decision_governance_engine/tests/__init__.py) | Python | 0 | 0 | 1 | 1 |
| [apps/decision\_governance\_engine/tests/integration/\_\_init\_\_.py](/apps/decision_governance_engine/tests/integration/__init__.py) | Python | 0 | 0 | 1 | 1 |
| [apps/decision\_governance\_engine/tests/unit/\_\_init\_\_.py](/apps/decision_governance_engine/tests/unit/__init__.py) | Python | 0 | 0 | 1 | 1 |
| [apps/decision\_governance\_engine/tests/unit/test\_clarification\_agent.py](/apps/decision_governance_engine/tests/unit/test_clarification_agent.py) | Python | 97 | 21 | 31 | 149 |
| [apps/decision\_governance\_engine/tests/unit/test\_cognitive\_gate\_agent.py](/apps/decision_governance_engine/tests/unit/test_cognitive_gate_agent.py) | Python | 127 | 23 | 38 | 188 |
| [apps/decision\_governance\_engine/tests/unit/test\_dao\_agent.py](/apps/decision_governance_engine/tests/unit/test_dao_agent.py) | Python | 156 | 31 | 54 | 241 |
| [apps/decision\_governance\_engine/tests/unit/test\_fa\_agent.py](/apps/decision_governance_engine/tests/unit/test_fa_agent.py) | Python | 129 | 22 | 34 | 185 |
| [apps/decision\_governance\_engine/tests/unit/test\_gatekeeper\_agent.py](/apps/decision_governance_engine/tests/unit/test_gatekeeper_agent.py) | Python | 119 | 26 | 32 | 177 |
| [apps/decision\_governance\_engine/tests/unit/test\_human\_review\_router.py](/apps/decision_governance_engine/tests/unit/test_human_review_router.py) | Python | 108 | 5 | 19 | 132 |
| [apps/decision\_governance\_engine/tests/unit/test\_qi\_agent.py](/apps/decision_governance_engine/tests/unit/test_qi_agent.py) | Python | 151 | 23 | 34 | 208 |
| [apps/decision\_governance\_engine/tests/unit/test\_report\_persistence.py](/apps/decision_governance_engine/tests/unit/test_report_persistence.py) | Python | 46 | 0 | 11 | 57 |
| [apps/decision\_governance\_engine/tests/unit/test\_review\_agent.py](/apps/decision_governance_engine/tests/unit/test_review_agent.py) | Python | 278 | 29 | 46 | 353 |
| [apps/decision\_governance\_engine/tests/unit/test\_scoring\_engine.py](/apps/decision_governance_engine/tests/unit/test_scoring_engine.py) | Python | 206 | 23 | 48 | 277 |
| [apps/decision\_governance\_engine/tests/unit/test\_shu\_agent.py](/apps/decision_governance_engine/tests/unit/test_shu_agent.py) | Python | 152 | 23 | 33 | 208 |
| [apps/design\_skills\_engine/README.md](/apps/design_skills_engine/README.md) | Markdown | 321 | 0 | 94 | 415 |
| [apps/design\_skills\_engine/\_\_init\_\_.py](/apps/design_skills_engine/__init__.py) | Python | 2 | 17 | 4 | 23 |
| [apps/design\_skills\_engine/docker-compose.cpu.yml](/apps/design_skills_engine/docker-compose.cpu.yml) | YAML | 12 | 12 | 2 | 26 |
| [apps/design\_skills\_engine/docker-compose.yml](/apps/design_skills_engine/docker-compose.yml) | YAML | 30 | 26 | 4 | 60 |
| [apps/design\_skills\_engine/scripts/setup\_comfyui.sh](/apps/design_skills_engine/scripts/setup_comfyui.sh) | Shell Script | 27 | 19 | 10 | 56 |
| [apps/faq\_system/.github/workflows/ci.yml](/apps/faq_system/.github/workflows/ci.yml) | YAML | 54 | 46 | 9 | 109 |
| [apps/faq\_system/AGENTS.md](/apps/faq_system/AGENTS.md) | Markdown | 7 | 0 | 5 | 12 |
| [apps/faq\_system/DESIGN.md](/apps/faq_system/DESIGN.md) | Markdown | 1,859 | 0 | 446 | 2,305 |
| [apps/faq\_system/Dockerfile](/apps/faq_system/Dockerfile) | Docker | 25 | 20 | 16 | 61 |
| [apps/faq\_system/README.md](/apps/faq_system/README.md) | Markdown | 346 | 0 | 110 | 456 |
| [apps/faq\_system/\_\_init\_\_.py](/apps/faq_system/__init__.py) | Python | 2 | 18 | 4 | 24 |
| [apps/faq\_system/backend/\_\_init\_\_.py](/apps/faq_system/backend/__init__.py) | Python | 2 | 1 | 4 | 7 |
| [apps/faq\_system/backend/agents/\_\_init\_\_.py](/apps/faq_system/backend/agents/__init__.py) | Python | 54 | 14 | 4 | 72 |
| [apps/faq\_system/backend/agents/analytics\_agent.py](/apps/faq_system/backend/agents/analytics_agent.py) | Python | 637 | 140 | 113 | 890 |
| [apps/faq\_system/backend/agents/enhanced\_faq\_agent.py](/apps/faq_system/backend/agents/enhanced_faq_agent.py) | Python | 330 | 93 | 89 | 512 |
| [apps/faq\_system/backend/agents/external\_kb\_agent.py](/apps/faq_system/backend/agents/external_kb_agent.py) | Python | 148 | 42 | 50 | 240 |
| [apps/faq\_system/backend/agents/internal\_kb\_agent.py](/apps/faq_system/backend/agents/internal_kb_agent.py) | Python | 351 | 152 | 91 | 594 |
| [apps/faq\_system/backend/agents/maintenance\_agent.py](/apps/faq_system/backend/agents/maintenance_agent.py) | Python | 393 | 101 | 103 | 597 |
| [apps/faq\_system/backend/security/\_\_init\_\_.py](/apps/faq_system/backend/security/__init__.py) | Python | 40 | 11 | 4 | 55 |
| [apps/faq\_system/backend/security/appi\_compliance.py](/apps/faq_system/backend/security/appi_compliance.py) | Python | 329 | 172 | 83 | 584 |
| [apps/faq\_system/backend/security/audit\_logger.py](/apps/faq_system/backend/security/audit_logger.py) | Python | 365 | 165 | 71 | 601 |
| [apps/faq\_system/backend/security/permission\_config.py](/apps/faq_system/backend/security/permission_config.py) | Python | 265 | 129 | 47 | 441 |
| [apps/faq\_system/backend/services/\_\_init\_\_.py](/apps/faq_system/backend/services/__init__.py) | Python | 66 | 16 | 6 | 88 |
| [apps/faq\_system/backend/services/citation\_service.py](/apps/faq_system/backend/services/citation_service.py) | Python | 251 | 137 | 68 | 456 |
| [apps/faq\_system/backend/services/coverage\_dashboard.py](/apps/faq_system/backend/services/coverage_dashboard.py) | Python | 309 | 142 | 80 | 531 |
| [apps/faq\_system/backend/services/faq\_service.py](/apps/faq_system/backend/services/faq_service.py) | Python | 356 | 67 | 76 | 499 |
| [apps/faq\_system/backend/services/feedback\_service.py](/apps/faq_system/backend/services/feedback_service.py) | Python | 276 | 176 | 71 | 523 |
| [apps/faq\_system/backend/services/glossary\_service.py](/apps/faq_system/backend/services/glossary_service.py) | Python | 255 | 166 | 60 | 481 |
| [apps/faq\_system/frontend/index.html](/apps/faq_system/frontend/index.html) | HTML | 11 | 0 | 1 | 12 |
| [apps/faq\_system/frontend/src/components/ChatWindow.tsx](/apps/faq_system/frontend/src/components/ChatWindow.tsx) | TypeScript JSX | 61 | 0 | 7 | 68 |
| [apps/faq\_system/main.py](/apps/faq_system/main.py) | Python | 273 | 73 | 71 | 417 |
| [apps/faq\_system/main\_enhanced.py](/apps/faq_system/main_enhanced.py) | Python | 456 | 48 | 62 | 566 |
| [apps/faq\_system/tests/\_\_init\_\_.py](/apps/faq_system/tests/__init__.py) | Python | 0 | 8 | 1 | 9 |
| [apps/faq\_system/tests/data/analytics\_test\_cases.yaml](/apps/faq_system/tests/data/analytics_test_cases.yaml) | YAML | 247 | 38 | 24 | 309 |
| [apps/faq\_system/tests/data/expected\_answers.json](/apps/faq_system/tests/data/expected_answers.json) | JSON | 134 | 0 | 7 | 141 |
| [apps/faq\_system/tests/data/internal\_faq\_test\_cases.yaml](/apps/faq_system/tests/data/internal_faq_test_cases.yaml) | YAML | 228 | 41 | 27 | 296 |
| [apps/faq\_system/tests/data/maintenance\_test\_cases.yaml](/apps/faq_system/tests/data/maintenance_test_cases.yaml) | YAML | 229 | 42 | 24 | 295 |
| [apps/market\_trend\_monitor/AGENTS.md](/apps/market_trend_monitor/AGENTS.md) | Markdown | 7 | 0 | 5 | 12 |
| [apps/market\_trend\_monitor/DESIGN.md](/apps/market_trend_monitor/DESIGN.md) | Markdown | 613 | 0 | 95 | 708 |
| [apps/market\_trend\_monitor/README.md](/apps/market_trend_monitor/README.md) | Markdown | 154 | 0 | 55 | 209 |
| [apps/market\_trend\_monitor/app\_config.json](/apps/market_trend_monitor/app_config.json) | JSON | 5 | 0 | 1 | 6 |
| [apps/market\_trend\_monitor/backend/\_\_init\_\_.py](/apps/market_trend_monitor/backend/__init__.py) | Python | 1 | 1 | 3 | 5 |
| [apps/market\_trend\_monitor/backend/agents/\_\_init\_\_.py](/apps/market_trend_monitor/backend/agents/__init__.py) | Python | 42 | 8 | 5 | 55 |
| [apps/market\_trend\_monitor/backend/agents/analyzer\_agent.py](/apps/market_trend_monitor/backend/agents/analyzer_agent.py) | Python | 316 | 79 | 49 | 444 |
| [apps/market\_trend\_monitor/backend/agents/collector\_agent.py](/apps/market_trend_monitor/backend/agents/collector_agent.py) | Python | 312 | 50 | 42 | 404 |
| [apps/market\_trend\_monitor/backend/agents/evidence\_ledger\_agent.py](/apps/market_trend_monitor/backend/agents/evidence_ledger_agent.py) | Python | 87 | 48 | 29 | 164 |
| [apps/market\_trend\_monitor/backend/agents/notifier\_agent.py](/apps/market_trend_monitor/backend/agents/notifier_agent.py) | Python | 135 | 99 | 38 | 272 |
| [apps/market\_trend\_monitor/backend/agents/prediction\_review\_agent.py](/apps/market_trend_monitor/backend/agents/prediction_review_agent.py) | Python | 104 | 54 | 36 | 194 |
| [apps/market\_trend\_monitor/backend/agents/redteam\_agent.py](/apps/market_trend_monitor/backend/agents/redteam_agent.py) | Python | 128 | 53 | 35 | 216 |
| [apps/market\_trend\_monitor/backend/agents/reporter\_agent.py](/apps/market_trend_monitor/backend/agents/reporter_agent.py) | Python | 299 | 96 | 34 | 429 |
| [apps/market\_trend\_monitor/backend/agents/signal\_scorer\_agent.py](/apps/market_trend_monitor/backend/agents/signal_scorer_agent.py) | Python | 107 | 49 | 31 | 187 |
| [apps/market\_trend\_monitor/backend/api/\_\_init\_\_.py](/apps/market_trend_monitor/backend/api/__init__.py) | Python | 0 | 1 | 2 | 3 |
| [apps/market\_trend\_monitor/backend/api/main.py](/apps/market_trend_monitor/backend/api/main.py) | Python | 73 | 27 | 21 | 121 |
| [apps/market\_trend\_monitor/backend/api/main\_enhanced.py](/apps/market_trend_monitor/backend/api/main_enhanced.py) | Python | 485 | 144 | 69 | 698 |
| [apps/market\_trend\_monitor/backend/api/routes/\_\_init\_\_.py](/apps/market_trend_monitor/backend/api/routes/__init__.py) | Python | 16 | 1 | 4 | 21 |
| [apps/market\_trend\_monitor/backend/api/routes/collect.py](/apps/market_trend_monitor/backend/api/routes/collect.py) | Python | 124 | 8 | 33 | 165 |
| [apps/market\_trend\_monitor/backend/api/routes/evidence.py](/apps/market_trend_monitor/backend/api/routes/evidence.py) | Python | 85 | 9 | 24 | 118 |
| [apps/market\_trend\_monitor/backend/api/routes/predictions.py](/apps/market_trend_monitor/backend/api/routes/predictions.py) | Python | 63 | 8 | 21 | 92 |
| [apps/market\_trend\_monitor/backend/api/routes/settings.py](/apps/market_trend_monitor/backend/api/routes/settings.py) | Python | 44 | 4 | 12 | 60 |
| [apps/market\_trend\_monitor/backend/api/routes/signals.py](/apps/market_trend_monitor/backend/api/routes/signals.py) | Python | 24 | 4 | 10 | 38 |
| [apps/market\_trend\_monitor/backend/api/routes/sources.py](/apps/market_trend_monitor/backend/api/routes/sources.py) | Python | 54 | 6 | 18 | 78 |
| [apps/market\_trend\_monitor/backend/api/routes/trends.py](/apps/market_trend_monitor/backend/api/routes/trends.py) | Python | 93 | 9 | 30 | 132 |
| [apps/market\_trend\_monitor/backend/api/state.py](/apps/market_trend_monitor/backend/api/state.py) | Python | 15 | 5 | 5 | 25 |
| [apps/market\_trend\_monitor/backend/config.py](/apps/market_trend_monitor/backend/config.py) | Python | 109 | 47 | 53 | 209 |
| [apps/market\_trend\_monitor/backend/db/\_\_init\_\_.py](/apps/market_trend_monitor/backend/db/__init__.py) | Python | 3 | 1 | 4 | 8 |
| [apps/market\_trend\_monitor/backend/db/base.py](/apps/market_trend_monitor/backend/db/base.py) | Python | 2 | 2 | 5 | 9 |
| [apps/market\_trend\_monitor/backend/db/models.py](/apps/market_trend_monitor/backend/db/models.py) | Python | 45 | 5 | 20 | 70 |
| [apps/market\_trend\_monitor/backend/db/session.py](/apps/market_trend_monitor/backend/db/session.py) | Python | 38 | 3 | 14 | 55 |
| [apps/market\_trend\_monitor/backend/integrations/\_\_init\_\_.py](/apps/market_trend_monitor/backend/integrations/__init__.py) | Python | 5 | 4 | 4 | 13 |
| [apps/market\_trend\_monitor/backend/integrations/arxiv\_api.py](/apps/market_trend_monitor/backend/integrations/arxiv_api.py) | Python | 35 | 4 | 13 | 52 |
| [apps/market\_trend\_monitor/backend/integrations/github\_api.py](/apps/market_trend_monitor/backend/integrations/github_api.py) | Python | 54 | 4 | 16 | 74 |
| [apps/market\_trend\_monitor/backend/integrations/news\_api.py](/apps/market_trend_monitor/backend/integrations/news_api.py) | Python | 143 | 70 | 22 | 235 |
| [apps/market\_trend\_monitor/backend/integrations/rss\_fetcher.py](/apps/market_trend_monitor/backend/integrations/rss_fetcher.py) | Python | 31 | 4 | 13 | 48 |
| [apps/market\_trend\_monitor/backend/integrations/test\_news\_api.py](/apps/market_trend_monitor/backend/integrations/test_news_api.py) | Python | 104 | 23 | 32 | 159 |
| [apps/market\_trend\_monitor/backend/requirements.txt](/apps/market_trend_monitor/backend/requirements.txt) | pip requirements | 15 | 9 | 9 | 33 |
| [apps/market\_trend\_monitor/backend/services/\_\_init\_\_.py](/apps/market_trend_monitor/backend/services/__init__.py) | Python | 30 | 8 | 4 | 42 |
| [apps/market\_trend\_monitor/backend/services/evidence\_service.py](/apps/market_trend_monitor/backend/services/evidence_service.py) | Python | 282 | 35 | 44 | 361 |
| [apps/market\_trend\_monitor/backend/services/market\_store.py](/apps/market_trend_monitor/backend/services/market_store.py) | Python | 200 | 25 | 35 | 260 |
| [apps/market\_trend\_monitor/backend/services/prediction\_service.py](/apps/market_trend_monitor/backend/services/prediction_service.py) | Python | 138 | 56 | 27 | 221 |
| [apps/market\_trend\_monitor/backend/services/redteam\_service.py](/apps/market_trend_monitor/backend/services/redteam_service.py) | Python | 140 | 68 | 28 | 236 |
| [apps/market\_trend\_monitor/backend/services/registry.py](/apps/market_trend_monitor/backend/services/registry.py) | Python | 8 | 4 | 4 | 16 |
| [apps/market\_trend\_monitor/backend/services/report\_export\_service.py](/apps/market_trend_monitor/backend/services/report_export_service.py) | Python | 252 | 23 | 41 | 316 |
| [apps/market\_trend\_monitor/backend/services/signal\_service.py](/apps/market_trend_monitor/backend/services/signal_service.py) | Python | 118 | 57 | 27 | 202 |
| [apps/market\_trend\_monitor/backend/services/source\_registry.py](/apps/market_trend_monitor/backend/services/source_registry.py) | Python | 138 | 12 | 22 | 172 |
| [apps/market\_trend\_monitor/backend/workflow.py](/apps/market_trend_monitor/backend/workflow.py) | Python | 175 | 42 | 49 | 266 |
| [apps/market\_trend\_monitor/backend/workflow\_engine.py](/apps/market_trend_monitor/backend/workflow_engine.py) | Python | 59 | 49 | 21 | 129 |
| [apps/market\_trend\_monitor/frontend/.eslintrc.cjs](/apps/market_trend_monitor/frontend/.eslintrc.cjs) | JavaScript | 20 | 0 | 2 | 22 |
| [apps/market\_trend\_monitor/frontend/eslint.config.js](/apps/market_trend_monitor/frontend/eslint.config.js) | JavaScript | 61 | 15 | 12 | 88 |
| [apps/market\_trend\_monitor/frontend/index.html](/apps/market_trend_monitor/frontend/index.html) | HTML | 18 | 0 | 3 | 21 |
| [apps/market\_trend\_monitor/frontend/package-lock.json](/apps/market_trend_monitor/frontend/package-lock.json) | JSON | 7,066 | 0 | 1 | 7,067 |
| [apps/market\_trend\_monitor/frontend/package.json](/apps/market_trend_monitor/frontend/package.json) | JSON | 49 | 0 | 1 | 50 |
| [apps/market\_trend\_monitor/frontend/public/favicon.svg](/apps/market_trend_monitor/frontend/public/favicon.svg) | XML | 3 | 0 | 2 | 5 |
| [apps/market\_trend\_monitor/frontend/src/App.tsx](/apps/market_trend_monitor/frontend/src/App.tsx) | TypeScript JSX | 230 | 5 | 5 | 240 |
| [apps/market\_trend\_monitor/frontend/src/\_\_tests\_\_/integration.test.tsx](/apps/market_trend_monitor/frontend/src/__tests__/integration.test.tsx) | TypeScript JSX | 84 | 12 | 20 | 116 |
| [apps/market\_trend\_monitor/frontend/src/api/\_\_tests\_\_/client.test.ts](/apps/market_trend_monitor/frontend/src/api/__tests__/client.test.ts) | TypeScript | 13 | 5 | 5 | 23 |
| [apps/market\_trend\_monitor/frontend/src/api/client.ts](/apps/market_trend_monitor/frontend/src/api/client.ts) | TypeScript | 119 | 33 | 22 | 174 |
| [apps/market\_trend\_monitor/frontend/src/components/Dashboard.tsx](/apps/market_trend_monitor/frontend/src/components/Dashboard.tsx) | TypeScript JSX | 252 | 11 | 13 | 276 |
| [apps/market\_trend\_monitor/frontend/src/components/Layout.tsx](/apps/market_trend_monitor/frontend/src/components/Layout.tsx) | TypeScript JSX | 189 | 5 | 10 | 204 |
| [apps/market\_trend\_monitor/frontend/src/components/Reports.tsx](/apps/market_trend_monitor/frontend/src/components/Reports.tsx) | TypeScript JSX | 469 | 9 | 54 | 532 |
| [apps/market\_trend\_monitor/frontend/src/components/Settings.tsx](/apps/market_trend_monitor/frontend/src/components/Settings.tsx) | TypeScript JSX | 195 | 11 | 17 | 223 |
| [apps/market\_trend\_monitor/frontend/src/components/TrendChart.tsx](/apps/market_trend_monitor/frontend/src/components/TrendChart.tsx) | TypeScript JSX | 82 | 9 | 7 | 98 |
| [apps/market\_trend\_monitor/frontend/src/components/TrendList.tsx](/apps/market_trend_monitor/frontend/src/components/TrendList.tsx) | TypeScript JSX | 157 | 8 | 10 | 175 |
| [apps/market\_trend\_monitor/frontend/src/components/\_\_tests\_\_/TrendChart.test.tsx](/apps/market_trend_monitor/frontend/src/components/__tests__/TrendChart.test.tsx) | TypeScript JSX | 54 | 7 | 11 | 72 |
| [apps/market\_trend\_monitor/frontend/src/components/\_\_tests\_\_/TrendList.test.tsx](/apps/market_trend_monitor/frontend/src/components/__tests__/TrendList.test.tsx) | TypeScript JSX | 62 | 5 | 16 | 83 |
| [apps/market\_trend\_monitor/frontend/src/main.tsx](/apps/market_trend_monitor/frontend/src/main.tsx) | TypeScript JSX | 8 | 0 | 3 | 11 |
| [apps/market\_trend\_monitor/frontend/src/store/\_\_tests\_\_/useAppStore.test.ts](/apps/market_trend_monitor/frontend/src/store/__tests__/useAppStore.test.ts) | TypeScript | 111 | 7 | 26 | 144 |
| [apps/market\_trend\_monitor/frontend/src/store/useAppStore.ts](/apps/market_trend_monitor/frontend/src/store/useAppStore.ts) | TypeScript | 147 | 22 | 33 | 202 |
| [apps/market\_trend\_monitor/frontend/src/test/setup.ts](/apps/market_trend_monitor/frontend/src/test/setup.ts) | TypeScript | 12 | 8 | 6 | 26 |
| [apps/market\_trend\_monitor/frontend/src/test/vitest.d.ts](/apps/market_trend_monitor/frontend/src/test/vitest.d.ts) | TypeScript | 5 | 6 | 4 | 15 |
| [apps/market\_trend\_monitor/frontend/src/types/index.ts](/apps/market_trend_monitor/frontend/src/types/index.ts) | TypeScript | 90 | 8 | 16 | 114 |
| [apps/market\_trend\_monitor/frontend/src/vite-env.d.ts](/apps/market_trend_monitor/frontend/src/vite-env.d.ts) | TypeScript | 7 | 1 | 4 | 12 |
| [apps/market\_trend\_monitor/frontend/tsconfig.json](/apps/market_trend_monitor/frontend/tsconfig.json) | JSON with Comments | 32 | 3 | 5 | 40 |
| [apps/market\_trend\_monitor/frontend/tsconfig.node.json](/apps/market_trend_monitor/frontend/tsconfig.node.json) | JSON | 10 | 0 | 2 | 12 |
| [apps/market\_trend\_monitor/frontend/vite.config.ts](/apps/market_trend_monitor/frontend/vite.config.ts) | TypeScript | 92 | 1 | 12 | 105 |
| [apps/market\_trend\_monitor/tests/\_\_init\_\_.py](/apps/market_trend_monitor/tests/__init__.py) | Python | 0 | 1 | 2 | 3 |
| [apps/market\_trend\_monitor/tests/conftest.py](/apps/market_trend_monitor/tests/conftest.py) | Python | 4 | 2 | 5 | 11 |
| [apps/market\_trend\_monitor/tests/test\_agents.py](/apps/market_trend_monitor/tests/test_agents.py) | Python | 247 | 31 | 69 | 347 |
| [apps/market\_trend\_monitor/tests/test\_evidence.py](/apps/market_trend_monitor/tests/test_evidence.py) | Python | 329 | 43 | 63 | 435 |
| [apps/market\_trend\_monitor/tests/test\_integration.py](/apps/market_trend_monitor/tests/test_integration.py) | Python | 143 | 26 | 48 | 217 |
| [apps/market\_trend\_monitor/tests/test\_predictions.py](/apps/market_trend_monitor/tests/test_predictions.py) | Python | 356 | 56 | 74 | 486 |
| [apps/market\_trend\_monitor/tests/test\_red\_team.py](/apps/market_trend_monitor/tests/test_red_team.py) | Python | 312 | 58 | 80 | 450 |
| [apps/market\_trend\_monitor/tests/test\_signals.py](/apps/market_trend_monitor/tests/test_signals.py) | Python | 237 | 52 | 56 | 345 |
| [apps/market\_trend\_monitor/tests/test\_workflow.py](/apps/market_trend_monitor/tests/test_workflow.py) | Python | 27 | 9 | 15 | 51 |
| [apps/messaging\_hub/AGENTS.md](/apps/messaging_hub/AGENTS.md) | Markdown | 7 | 0 | 5 | 12 |
| [apps/messaging\_hub/README.md](/apps/messaging_hub/README.md) | Markdown | 241 | 0 | 84 | 325 |
| [apps/messaging\_hub/\_\_init\_\_.py](/apps/messaging_hub/__init__.py) | Python | 1 | 1 | 2 | 4 |
| [apps/messaging\_hub/admin\_ui/index.html](/apps/messaging_hub/admin_ui/index.html) | HTML | 13 | 0 | 2 | 15 |
| [apps/messaging\_hub/admin\_ui/package-lock.json](/apps/messaging_hub/admin_ui/package-lock.json) | JSON | 4,056 | 0 | 1 | 4,057 |
| [apps/messaging\_hub/admin\_ui/package.json](/apps/messaging_hub/admin_ui/package.json) | JSON | 36 | 0 | 2 | 38 |
| [apps/messaging\_hub/admin\_ui/postcss.config.js](/apps/messaging_hub/admin_ui/postcss.config.js) | JavaScript | 6 | 0 | 2 | 8 |
| [apps/messaging\_hub/admin\_ui/src/App.tsx](/apps/messaging_hub/admin_ui/src/App.tsx) | TypeScript JSX | 21 | 3 | 4 | 28 |
| [apps/messaging\_hub/admin\_ui/src/api/client.ts](/apps/messaging_hub/admin_ui/src/api/client.ts) | TypeScript | 62 | 33 | 15 | 110 |
| [apps/messaging\_hub/admin\_ui/src/components/Approvals.tsx](/apps/messaging_hub/admin_ui/src/components/Approvals.tsx) | TypeScript JSX | 344 | 10 | 26 | 380 |
| [apps/messaging\_hub/admin\_ui/src/components/Conversations.tsx](/apps/messaging_hub/admin_ui/src/components/Conversations.tsx) | TypeScript JSX | 81 | 8 | 11 | 100 |
| [apps/messaging\_hub/admin\_ui/src/components/Dashboard.tsx](/apps/messaging_hub/admin_ui/src/components/Dashboard.tsx) | TypeScript JSX | 116 | 12 | 10 | 138 |
| [apps/messaging\_hub/admin\_ui/src/components/FileOrganizer.tsx](/apps/messaging_hub/admin_ui/src/components/FileOrganizer.tsx) | TypeScript JSX | 353 | 0 | 21 | 374 |
| [apps/messaging\_hub/admin\_ui/src/components/Layout.tsx](/apps/messaging_hub/admin_ui/src/components/Layout.tsx) | TypeScript JSX | 51 | 10 | 6 | 67 |
| [apps/messaging\_hub/admin\_ui/src/components/Platforms.tsx](/apps/messaging_hub/admin_ui/src/components/Platforms.tsx) | TypeScript JSX | 91 | 6 | 7 | 104 |
| [apps/messaging\_hub/admin\_ui/src/components/Sessions.tsx](/apps/messaging_hub/admin_ui/src/components/Sessions.tsx) | TypeScript JSX | 81 | 5 | 5 | 91 |
| [apps/messaging\_hub/admin\_ui/src/components/Settings.tsx](/apps/messaging_hub/admin_ui/src/components/Settings.tsx) | TypeScript JSX | 96 | 11 | 11 | 118 |
| [apps/messaging\_hub/admin\_ui/src/components/SkillsManager.tsx](/apps/messaging_hub/admin_ui/src/components/SkillsManager.tsx) | TypeScript JSX | 397 | 10 | 35 | 442 |
| [apps/messaging\_hub/admin\_ui/src/components/Timeline.tsx](/apps/messaging_hub/admin_ui/src/components/Timeline.tsx) | TypeScript JSX | 258 | 15 | 24 | 297 |
| [apps/messaging\_hub/admin\_ui/src/hooks/useWebSocket.ts](/apps/messaging_hub/admin_ui/src/hooks/useWebSocket.ts) | TypeScript | 47 | 9 | 15 | 71 |
| [apps/messaging\_hub/admin\_ui/src/index.css](/apps/messaging_hub/admin_ui/src/index.css) | PostCSS | 10 | 0 | 3 | 13 |
| [apps/messaging\_hub/admin\_ui/src/main.tsx](/apps/messaging_hub/admin_ui/src/main.tsx) | TypeScript JSX | 23 | 0 | 4 | 27 |
| [apps/messaging\_hub/admin\_ui/tailwind.config.js](/apps/messaging_hub/admin_ui/tailwind.config.js) | JavaScript | 22 | 1 | 2 | 25 |
| [apps/messaging\_hub/admin\_ui/tsconfig.json](/apps/messaging_hub/admin_ui/tsconfig.json) | JSON with Comments | 25 | 0 | 2 | 27 |
| [apps/messaging\_hub/admin\_ui/tsconfig.node.json](/apps/messaging_hub/admin_ui/tsconfig.node.json) | JSON | 10 | 0 | 2 | 12 |
| [apps/messaging\_hub/admin\_ui/vite.config.ts](/apps/messaging_hub/admin_ui/vite.config.ts) | TypeScript | 22 | 0 | 3 | 25 |
| [apps/messaging\_hub/agents/\_\_init\_\_.py](/apps/messaging_hub/agents/__init__.py) | Python | 6 | 4 | 4 | 14 |
| [apps/messaging\_hub/agents/file\_organizer\_agent.py](/apps/messaging_hub/agents/file_organizer_agent.py) | Python | 417 | 133 | 77 | 627 |
| [apps/messaging\_hub/agents/meeting\_agent.py](/apps/messaging_hub/agents/meeting_agent.py) | Python | 312 | 142 | 42 | 496 |
| [apps/messaging\_hub/approval\_manager.py](/apps/messaging_hub/approval_manager.py) | Python | 307 | 133 | 68 | 508 |
| [apps/messaging\_hub/coordinator.py](/apps/messaging_hub/coordinator.py) | Python | 417 | 94 | 63 | 574 |
| [apps/messaging\_hub/execution\_tracker.py](/apps/messaging_hub/execution_tracker.py) | Python | 245 | 138 | 51 | 434 |
| [apps/messaging\_hub/main.py](/apps/messaging_hub/main.py) | Python | 395 | 153 | 124 | 672 |
| [apps/messaging\_hub/skills\_manager.py](/apps/messaging_hub/skills_manager.py) | Python | 285 | 335 | 54 | 674 |
| [apps/messaging\_hub/tests/\_\_init\_\_.py](/apps/messaging_hub/tests/__init__.py) | Python | 0 | 2 | 1 | 3 |
| [apps/messaging\_hub/tests/unit/\_\_init\_\_.py](/apps/messaging_hub/tests/unit/__init__.py) | Python | 0 | 2 | 1 | 3 |
| [apps/messaging\_hub/tests/unit/test\_meeting\_agent.py](/apps/messaging_hub/tests/unit/test_meeting_agent.py) | Python | 82 | 13 | 20 | 115 |
| [apps/platform/\_\_init\_\_.py](/apps/platform/__init__.py) | Python | 20 | 22 | 5 | 47 |
| [apps/platform/agents/\_\_init\_\_.py](/apps/platform/agents/__init__.py) | Python | 8 | 4 | 4 | 16 |
| [apps/platform/agents/analytics\_agent.py](/apps/platform/agents/analytics_agent.py) | Python | 142 | 65 | 35 | 242 |
| [apps/platform/agents/gallery\_agent.py](/apps/platform/agents/gallery_agent.py) | Python | 58 | 28 | 17 | 103 |
| [apps/platform/agents/publish\_agent.py](/apps/platform/agents/publish_agent.py) | Python | 108 | 46 | 27 | 181 |
| [apps/platform/engine.py](/apps/platform/engine.py) | Python | 137 | 177 | 42 | 356 |
| [apps/platform/main.py](/apps/platform/main.py) | Python | 218 | 35 | 61 | 314 |
| [apps/platform/routers/\_\_init\_\_.py](/apps/platform/routers/__init__.py) | Python | 10 | 4 | 4 | 18 |
| [apps/platform/routers/components.py](/apps/platform/routers/components.py) | Python | 179 | 94 | 34 | 307 |
| [apps/platform/routers/dashboard.py](/apps/platform/routers/dashboard.py) | Python | 63 | 58 | 19 | 140 |
| [apps/platform/routers/gallery.py](/apps/platform/routers/gallery.py) | Python | 83 | 67 | 19 | 169 |
| [apps/platform/routers/publish.py](/apps/platform/routers/publish.py) | Python | 90 | 68 | 24 | 182 |
| [apps/platform/schemas/\_\_init\_\_.py](/apps/platform/schemas/__init__.py) | Python | 34 | 7 | 4 | 45 |
| [apps/platform/schemas/component\_schemas.py](/apps/platform/schemas/component_schemas.py) | Python | 76 | 8 | 27 | 111 |
| [apps/platform/schemas/gallery\_schemas.py](/apps/platform/schemas/gallery_schemas.py) | Python | 70 | 8 | 27 | 105 |
| [apps/platform/schemas/publish\_schemas.py](/apps/platform/schemas/publish_schemas.py) | Python | 84 | 18 | 38 | 140 |
| [apps/platform/services/\_\_init\_\_.py](/apps/platform/services/__init__.py) | Python | 18 | 4 | 4 | 26 |
| [apps/platform/services/component\_library.py](/apps/platform/services/component_library.py) | Python | 253 | 169 | 74 | 496 |
| [apps/platform/services/gallery\_service.py](/apps/platform/services/gallery_service.py) | Python | 227 | 139 | 44 | 410 |
| [apps/platform/services/publish\_orchestrator.py](/apps/platform/services/publish_orchestrator.py) | Python | 336 | 141 | 59 | 536 |
| [apps/platform/services/tenant\_dashboard.py](/apps/platform/services/tenant_dashboard.py) | Python | 193 | 85 | 42 | 320 |

[Summary](results.md) / Details / [Diff Summary](diff.md) / [Diff Details](diff-details.md)