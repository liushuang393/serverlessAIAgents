from agentflow.protocols.a2a_card import AgentSkill

# Source Language Skills
cobol_analysis_skill = AgentSkill(
    name="parse_cobol",
    description="Parse COBOL source code into AST.",
    input_schema={
        "type": "object",
        "properties": {
            "source_code": {"type": "string"}
        },
        "required": ["source_code"]
    },
    output_schema={
        "type": "object",
        "properties": {
            "ast": {"type": "object"}
        }
    }
)

# Target Language Skills
java_generation_skill = AgentSkill(
    name="generate_java",
    description="Generate Java source code from migration artifacts.",
    input_schema={
        "type": "object",
        "properties": {
            "migration_design": {"type": "object"},
            "transformation_rules": {"type": "array"}
        }
    },
    output_schema={
        "type": "object",
        "properties": {
            "source_code": {"type": "string"}
        }
    }
)

# Registry of all adapter skills
ADAPTER_SKILLS = [
    cobol_analysis_skill,
    java_generation_skill
]
