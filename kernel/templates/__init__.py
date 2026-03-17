"""後方互換: plugins/official.industry-template-pack/ に移動済み。
直接 import は非推奨。SkillEngine の template loader 経由でアクセスしてください。"""

import warnings

warnings.warn(
    "kernel.templates は plugins/official.industry-template-pack/ に移動しました。",
    DeprecationWarning,
    stacklevel=2,
)
