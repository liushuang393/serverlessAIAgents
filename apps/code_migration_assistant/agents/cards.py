from agentflow.protocols.a2a_card import AgentCard, AgentSkill

# 1. Legacy Analysis Agent
legacy_analysis_card = AgentCard(
    name="legacy-analysis-agent",
    description="Analyzes legacy code (COBOL, etc.) to extract structure, IO, and dependencies.",
    version="1.0.0",
    skills=[
        AgentSkill(
            name="analyze_code",
            description="Parse and analyze source code.",
            input_schema={
                "type": "object",
                "properties": {
                    "source_code": {"type": "string"},
                    "task_spec": {"type": "object"}
                },
                "required": ["source_code"]
            },
            output_schema={
                "type": "object",
                "properties": {
                    "programs": {"type": "array"},
                    "data_structures": {"type": "object"}
                }
            }
        )
    ]
)

# 2. Migration Design Agent
migration_design_card = AgentCard(
    name="migration-design-agent",
    description="Designs modern architecture (Java/Spring) based on legacy analysis.",
    version="1.0.0",
    skills=[
        AgentSkill(
            name="design_architecture",
            description="Generate migration design and mapping rules.",
            input_schema={
                "type": "object",
                "properties": {
                    "legacy_analysis": {"type": "object"}
                },
                "required": ["legacy_analysis"]
            },
            output_schema={
                "type": "object",
                "properties": {
                    "package_structure": {"type": "array"},
                    "mapping_rules": {"type": "array"}
                }
            }
        )
    ]
)

# 3. Code Transformation Agent
code_transformation_card = AgentCard(
    name="code-transformation-agent",
    description="Transforms legacy code to modern code based on design.",
    version="1.0.0",
    skills=[
        AgentSkill(
            name="transform_code",
            description="Generate target code from source and design.",
            input_schema={
                "type": "object",
                "properties": {
                    "source_code": {"type": "string"},
                    "migration_design": {"type": "object"}
                },
                "required": ["source_code", "migration_design"]
            },
            output_schema={
                "type": "object",
                "properties": {
                    "target_code": {"type": "string"}
                }
            }
        )
    ]
)

# 4. Test Synthesis Agent
test_synthesis_card = AgentCard(
    name="test-synthesis-agent",
    description="Synthesizes test cases for the migrated code.",
    version="1.0.0",
    skills=[
        AgentSkill(
            name="synthesize_tests",
            description="Generate JUnit tests.",
            input_schema={
                "type": "object",
                "properties": {
                    "legacy_analysis": {"type": "object"},
                    "expected_outputs": {"type": "object"}
                }
            }
        )
    ]
)

# 5. Differential Verification Agent
differential_verification_card = AgentCard(
    name="differential-verification-agent",
    description="Verifies behavior equivalence between legacy and new code.",
    version="1.0.0",
    skills=[
        AgentSkill(
            name="verify_diff",
            description="Compare execution results.",
            input_schema={
                "type": "object",
                "properties": {
                    "transformation": {"type": "object"},
                    "test_synthesis": {"type": "object"}
                }
            }
        )
    ]
)

# 6. Quality Gate Agent
quality_gate_card = AgentCard(
    name="quality-gate-agent",
    description="Evaluates code quality and compliance.",
    version="1.0.0",
    skills=[
        AgentSkill(
            name="evaluate_quality",
            description="Check for known issues and code standards.",
            input_schema={
                "type": "object",
                "properties": {
                    "differential": {"type": "object"},
                    "known_legacy_issues": {"type": "array"}
                }
            }
        )
    ]
)

# 7. Limited Fixer Agent
limited_fixer_card = AgentCard(
    name="limited-fixer-agent",
    description="Applies limited fixes to resolve quality or correctness issues.",
    version="1.0.0",
    skills=[
        AgentSkill(
            name="apply_fix",
            description="Generate fixed code.",
            input_schema={
                "type": "object",
                "properties": {
                    "quality_gate": {"type": "object"},
                    "transformation": {"type": "object"}
                }
            }
        )
    ]
)
# 8. Compliance Reporter Agent
compliance_reporter_card = AgentCard(
    name="compliance-reporter-agent",
    description="Generates professional modernization and compliance reports.",
    version="1.0.0",
    skills=[
        AgentSkill(
            name="generate_report",
            description="Create a comprehensive Markdown report of the migration process.",
            input_schema={
                "type": "object",
                "properties": {
                    "task_id": {"type": "string"},
                    "analysis": {"type": "object"},
                    "design": {"type": "object"},
                    "transformation": {"type": "object"},
                    "quality": {"type": "object"}
                },
                "required": ["task_id", "analysis", "design", "quality"]
            }
        )
    ]
)
