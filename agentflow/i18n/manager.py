"""AgentFlow 国際化マネージャー。

JSON ロケールファイルを読み込み、ドット区切りキーで翻訳テキストを取得する。
各 App は独自の locale_dir を指定して上書き可能。

使用例::

    from agentflow.i18n import get_i18n
    i18n = get_i18n("ja")
    print(i18n.t("error.not_found"))          # → "見つかりません"
    print(i18n.t("status.hello", name="太郎")) # → "こんにちは、太郎さん"
"""

import json
import logging
from pathlib import Path


logger = logging.getLogger(__name__)

# デフォルトロケールファイルディレクトリ
_DEFAULT_LOCALE_DIR: Path = Path(__file__).parent / "locales"


class I18nManager:
    """国際化マネージャー。

    ロケール JSON ファイルから翻訳テキストを読み込み、
    `t()` メソッドでドット区切りキーから文字列を返す。
    ネストした JSON キー（例: "error.not_found"）に対応。
    プレースホルダー置換（{name} 形式）をサポートする。
    """

    def __init__(
        self,
        locale: str = "ja",
        locale_dir: Path | None = None,
    ) -> None:
        """初期化。

        Args:
            locale: デフォルトロケール（"ja" / "en" / "zh"）。
            locale_dir: ロケール JSON ファイルのディレクトリ。
                        None の場合は agentflow/i18n/locales/ を使用。
        """
        self._locale: str = locale
        self._locale_dir: Path = locale_dir if locale_dir is not None else _DEFAULT_LOCALE_DIR
        self._translations: dict[str, object] = {}
        self._load(locale)

    # ------------------------------------------------------------------
    # 公開インターフェース
    # ------------------------------------------------------------------

    @property
    def locale(self) -> str:
        """現在のロケール識別子。"""
        return self._locale

    def set_locale(self, locale: str) -> None:
        """ランタイムでロケールを切り替える。

        Args:
            locale: 切り替え先のロケール（"ja" / "en" / "zh" 等）。
        """
        self._locale = locale
        self._load(locale)

    def t(self, key: str, default: str | None = None, **kwargs: object) -> str:
        """翻訳テキストを返す。

        Args:
            key: ドット区切りキー（例: "error.not_found", "app.title"）。
            default: キーが見つからない場合のフォールバック文字列。
                     省略時はキー文字列をそのまま返す。
            **kwargs: {placeholder} 形式のプレースホルダー置換パラメータ。
                      例: ``t("greeting", name="太郎")``

        Returns:
            翻訳済みテキスト。キー未発見時は default または key。
        """
        node: object = self._translations
        for part in key.split("."):
            if isinstance(node, dict) and part in node:
                node = node[part]
            else:
                fallback = default if default is not None else key
                return fallback.format(**kwargs) if kwargs else fallback
        text = str(node)
        return text.format(**kwargs) if kwargs else text

    # ------------------------------------------------------------------
    # 内部メソッド
    # ------------------------------------------------------------------

    def _load(self, locale: str) -> None:
        """指定ロケールの JSON ファイルを読み込む。

        Args:
            locale: ロケール識別子（ファイル名の stem と一致）。

        Note:
            ファイルが存在しない場合は空の翻訳辞書を設定し警告を出力する。
        """
        path = self._locale_dir / f"{locale}.json"
        if path.exists():
            with path.open(encoding="utf-8") as fh:
                loaded = json.load(fh)
                self._translations = loaded if isinstance(loaded, dict) else {}
        else:
            logger.warning(
                "i18n: ロケールファイルが見つかりません: %s（フォールバック: キーをそのまま返します）",
                path,
            )
            self._translations = {}
