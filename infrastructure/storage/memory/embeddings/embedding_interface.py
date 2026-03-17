"""埋め込みエンジンのインターフェース定義."""

from abc import ABC, abstractmethod


class EmbeddingEngine(ABC):
    """埋め込みエンジンの抽象基底クラス.

    目的:
    - テキストをベクトル埋め込みに変換するインターフェースを定義
    - OpenAI/Sentence Transformers/その他のエンジンを統一的に扱う

    実装必須メソッド:
    - embed_text(): テキストをベクトル埋め込みに変換
    - embed_batch(): 複数のテキストを一括変換
    - get_dimension(): 埋め込みベクトルの次元数を取得
    """

    @abstractmethod
    async def embed_text(self, text: str) -> list[float]:
        """テキストをベクトル埋め込みに変換.

        Args:
            text: 入力テキスト

        Returns:
            埋め込みベクトル

        Raises:
            ValueError: 無効なテキストの場合
            RuntimeError: 埋め込み生成に失敗した場合
        """

    @abstractmethod
    async def embed_batch(self, texts: list[str]) -> list[list[float]]:
        """複数のテキストを一括変換.

        Args:
            texts: 入力テキストのリスト

        Returns:
            埋め込みベクトルのリスト

        Raises:
            ValueError: 無効なテキストの場合
            RuntimeError: 埋め込み生成に失敗した場合
        """

    @abstractmethod
    def get_dimension(self) -> int:
        """埋め込みベクトルの次元数を取得.

        Returns:
            次元数
        """

    @abstractmethod
    def get_model_name(self) -> str:
        """モデル名を取得.

        Returns:
            モデル名
        """
