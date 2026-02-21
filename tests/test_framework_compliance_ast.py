import ast
import os
import sys


def check_file_imports(file_path, expected_imports):
    """Check if file imports specific names."""
    try:
        with open(file_path) as f:
            tree = ast.parse(f.read())
    except Exception as e:
        print(f"Error parsing {file_path}: {e}")
        return False

    imports = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.ImportFrom):
            for alias in node.names:
                imports.add(alias.name)

    missing = [imp for imp in expected_imports if imp not in imports]
    if missing:
        print(f"Missing imports in {file_path}: {missing}")
        return False
    return True


def check_card_definitions(file_path, expected_cards):
    """Check if file defines specific AgentCard variables."""
    try:
        with open(file_path) as f:
            tree = ast.parse(f.read())
    except Exception as e:
        print(f"Error parsing {file_path}: {e}")
        return False

    defined_vars = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.Assign):
            for target in node.targets:
                if isinstance(target, ast.Name):
                    defined_vars.add(target.id)

    missing = [card for card in expected_cards if card not in defined_vars]
    if missing:
        print(f"Missing card definitions in {file_path}: {missing}")
        return False
    return True


def check_agui_usage(file_path):
    """Check if AG-UI events are instantiated."""
    try:
        with open(file_path) as f:
            tree = ast.parse(f.read())
    except Exception as e:
        print(f"Error parsing {file_path}: {e}")
        return False

    events_used = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.Call) and isinstance(node.func, ast.Name):
            events_used.add(node.func.id)

    expected_usage = ["FlowStartEvent", "NodeStartEvent", "NodeCompleteEvent"]
    # Check if at least some are used
    found_any = any(e in events_used for e in expected_usage)
    if not found_any:
        print(f"No AG-UI events found in {file_path}. Found calls: {events_used}")
        return False
    return True


def main():
    base_dir = os.getcwd()

    # 1. Check backend/app.py for AG-UI imports and usage
    web_main = os.path.join(base_dir, "apps/code_migration_assistant/backend/app.py")
    if not check_file_imports(web_main, ["FlowStartEvent", "NodeStartEvent", "LogEvent"]):
        print("FAIL: AG-UI imports missing")
        sys.exit(1)
    if not check_agui_usage(web_main):
        print("FAIL: AG-UI events not used")
        sys.exit(1)
    print("AG-UI Protocol Compliance: PASS")

    # 2. Check agents/cards.py for definitions
    cards_file = os.path.join(base_dir, "apps/code_migration_assistant/agents/cards.py")
    expected_cards = [
        "legacy_analysis_card",
        "migration_design_card",
        "code_transformation_card",
        "test_synthesis_card",
        "differential_verification_card",
        "quality_gate_card",
        "limited_fixer_card",
    ]
    if not check_card_definitions(cards_file, expected_cards):
        print("FAIL: A2A cards missing")
        sys.exit(1)
    print("A2A Card Definitions: PASS")

    # 3. Check adapters/skills.py for definitions
    skills_file = os.path.join(base_dir, "apps/code_migration_assistant/adapters/skills.py")
    expected_skills = ["cobol_analysis_skill", "java_generation_skill"]
    if not check_card_definitions(skills_file, expected_skills):
        print("FAIL: Adapter skills missing")
        sys.exit(1)
    print("Skill Formalization: PASS")

    print("Framework Compliance Verification: SUCCESS")


if __name__ == "__main__":
    main()
