"""AgentFlow 国際化（i18n）パッケージ。

フレームワーク全体および各 App が共通して使える軽量 i18n ユーティリティ。
外部依存ゼロ・純 Python 実装で、JSON ロケールファイルから翻訳テキストを取得する。

基本的な使い方::

    from agentflow.i18n import get_i18n, t

    # デフォルトロケール（ja）で翻訳
    msg = t("error.not_found")            # → "見つかりません"
    msg = t("greeting", name="太郎")      # → "こんにちは、太郎さん"

    # ロケールを明示
    i18n = get_i18n("en")
    msg = i18n.t("error.not_found")       # → "Not found"

App 固有翻訳の追加::

    from pathlib import Path
    from agentflow.i18n import get_i18n

    # App のロケールディレクトリを指定（agentflow デフォルトを上書き）
    i18n = get_i18n("ja", locale_dir=Path(__file__).parent / "locales")
    msg = i18n.t("report.title")          # App 独自の翻訳を取得
"""

from pathlib import Path

from agentflow.i18n.manager import I18nManager


# ---------------------------------------------------------------------------
# シングルトン・キャッシュ（locale_dir がデフォルトのもの）
# ---------------------------------------------------------------------------
_default_instances: dict[str, I18nManager] = {}


def get_i18n(
    locale: str = "ja",
    locale_dir: Path | None = None,
) -> I18nManager:
    """I18nManager インスタンスを返す。

    デフォルト locale_dir を使う場合はシングルトンをキャッシュして返す。
    App 固有の locale_dir を指定した場合は毎回新しいインスタンスを生成する。

    Args:
        locale: ロケール識別子（"ja" / "en" / "zh"）。
        locale_dir: ロケール JSON ファイルのディレクトリ。
                    None の場合は agentflow/i18n/locales/ を使用。

    Returns:
        指定ロケールの I18nManager インスタンス。
    """
    if locale_dir is not None:
        # App 固有ディレクトリ指定時はキャッシュしない
        return I18nManager(locale=locale, locale_dir=locale_dir)

    if locale not in _default_instances:
        _default_instances[locale] = I18nManager(locale=locale)
    return _default_instances[locale]


def t(key: str, locale: str = "ja", default: str | None = None, **kwargs: object) -> str:
    """モジュールレベルの翻訳ショートカット関数。

    ``get_i18n(locale).t(key, ...)`` の省略形。
    モジュールトップレベルでサクッと使いたい場合に利用する。

    Args:
        key: ドット区切り翻訳キー（例: "error.not_found"）。
        locale: ロケール（デフォルト: "ja"）。
        default: キー未発見時のフォールバック文字列。
        **kwargs: プレースホルダー置換パラメータ。

    Returns:
        翻訳済み文字列。
    """
    return get_i18n(locale).t(key, default=default, **kwargs)


__all__ = [
    "I18nManager",
    "get_i18n",
    "t",
]
