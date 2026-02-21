import os
import sys
import unittest


# Add project root to path
sys.path.append(os.getcwd())


class TestFrameworkCompliance(unittest.TestCase):
    def test_agui_protocol_usage(self):
        """Verify web/main.py imports and uses AG-UI events."""
        from apps.code_migration_assistant.web import main

        # Check if imports exist (if main loaded without error, good start)
        self.assertTrue(hasattr(main, "FlowStartEvent"))
        self.assertTrue(hasattr(main, "NodeStartEvent"))

    def test_a2a_readiness_cards(self):
        """Verify all agents have AgentCard definitions exported."""
        import apps.code_migration_assistant.agents as agents_pkg

        from agentflow.protocols.a2a_card import AgentCard

        expected_cards = [
            "legacy_analysis_card",
            "migration_design_card",
            "code_transformation_card",
            "test_synthesis_card",
            "differential_verification_card",
            "quality_gate_card",
            "limited_fixer_card",
        ]

        for card_name in expected_cards:
            self.assertTrue(hasattr(agents_pkg, card_name), f"Missing export: {card_name}")
            card = getattr(agents_pkg, card_name)
            self.assertIsInstance(card, AgentCard)
            self.assertTrue(len(card.skills) > 0, f"Card {card_name} has no skills")

            # Validate schema
            skill = card.skills[0]
            self.assertIsNotNone(skill.input_schema)
            self.assertIsNotNone(skill.output_schema)


if __name__ == "__main__":
    unittest.main()
