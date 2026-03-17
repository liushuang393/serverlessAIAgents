"""後方互換: plugins/infrastructure-skills/stripe-payment/ からの re-export."""
from kernel.skills.builtin._compat import register_submodule_alias

register_submodule_alias(
    "kernel.skills.builtin.stripe_payment",
    "plugins.infrastructure_skills.stripe_payment",
    ["exceptions", "payment"],
)

