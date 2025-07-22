"""
ドキュメントローダーインターフェース

このモジュールは、様々な形式のドキュメント（PDF、HTML、画像等）を
テキストに変換するための統一されたインターフェースを提供します。
"""

import mimetypes
from abc import ABC, abstractmethod
from io import BytesIO
from pathlib import Path
from typing import Any, Optional, Union

from ..config import get_settings
from ..core.models import ParsedDocument
from ..utils.logging import get_logger

logger = get_logger(__name__)


class DocumentLoader(ABC):
    """ドキュメントローダーの抽象基底クラス"""

    def __init__(self, **kwargs):
        """
        ドキュメントローダーを初期化する

        Args:
            **kwargs: ローダー固有の設定
        """
        self.config = kwargs
        self.settings = get_settings()

    @abstractmethod
    async def load(self, source: Union[str, Path, bytes]) -> ParsedDocument:
        """
        ドキュメントを読み込んでテキストに変換する

        Args:
            source: ドキュメントのソース（ファイルパス、URL、バイナリデータ）

        Returns:
            ParsedDocument: パース済みドキュメント
        """
        pass

    @abstractmethod
    def supports(self, source: Union[str, Path]) -> bool:
        """
        このローダーがソースをサポートするかチェックする

        Args:
            source: チェック対象のソース

        Returns:
            bool: サポートする場合True
        """
        pass

    def _get_mime_type(self, source: Union[str, Path]) -> Optional[str]:
        """ソースのMIMEタイプを取得する"""
        return mimetypes.guess_type(str(source))[0]


class PDFLoader(DocumentLoader):
    """PDFドキュメントローダー"""

    def __init__(self, extract_images: bool = False, **kwargs):
        """
        PDFローダーを初期化する

        Args:
            extract_images: 画像も抽出するかどうか
            **kwargs: その他の設定
        """
        super().__init__(**kwargs)
        self.extract_images = extract_images

        logger.info("PDFLoader を初期化しました")

    def supports(self, source: Union[str, Path]) -> bool:
        """PDFファイルをサポートするかチェック"""
        mime_type = self._get_mime_type(source)
        return mime_type == "application/pdf" or str(source).lower().endswith(".pdf")

    async def load(self, source: Union[str, Path, bytes]) -> ParsedDocument:
        """PDFファイルを読み込んでテキストに変換する"""
        try:
            import pypdf
        except ImportError:
            raise ImportError("pypdfパッケージがインストールされていません: pip install pypdf")

        try:
            # ソースの種類に応じて処理
            if isinstance(source, bytes):
                pdf_file: Union[BytesIO, Any] = BytesIO(source)
                source_info = "binary_data"
            else:
                pdf_file = open(source, "rb")
                source_info = str(source)

            reader = pypdf.PdfReader(pdf_file)

            # メタデータを取得
            metadata = {
                "source": source_info,
                "source_type": "pdf",
                "page_count": len(reader.pages),
                "title": reader.metadata.get("/Title", "") if reader.metadata else "",
                "author": reader.metadata.get("/Author", "") if reader.metadata else "",
                "subject": reader.metadata.get("/Subject", "")
                if reader.metadata
                else "",
            }

            # 全ページのテキストを抽出
            text_content = []
            for page_num, page in enumerate(reader.pages):
                try:
                    page_text = page.extract_text()
                    if page_text.strip():
                        text_content.append(f"[Page {page_num + 1}]\n{page_text}")
                except Exception as e:
                    logger.warning(f"ページ {page_num + 1} の抽出に失敗: {e}")
                    continue

            # ファイルを閉じる
            if not isinstance(source, bytes):
                pdf_file.close()

            full_text = "\n\n".join(text_content)

            logger.info(f"PDFを正常に読み込みました: {len(text_content)}ページ, {len(full_text)}文字")

            return ParsedDocument(
                text=full_text, metadata=metadata, chunks=None, source_type="pdf"
            )

        except Exception as e:
            logger.error(f"PDF読み込みエラー: {e}")
            raise


class HTMLLoader(DocumentLoader):
    """HTMLドキュメントローダー"""

    def __init__(
        self, remove_scripts: bool = True, remove_styles: bool = True, **kwargs
    ):
        """
        HTMLローダーを初期化する

        Args:
            remove_scripts: スクリプトタグを除去するかどうか
            remove_styles: スタイルタグを除去するかどうか
            **kwargs: その他の設定
        """
        super().__init__(**kwargs)
        self.remove_scripts = remove_scripts
        self.remove_styles = remove_styles

        logger.info("HTMLLoader を初期化しました")

    def supports(self, source: Union[str, Path]) -> bool:
        """HTMLファイルをサポートするかチェック"""
        mime_type = self._get_mime_type(source)
        return mime_type in ["text/html", "application/xhtml+xml"] or str(
            source
        ).lower().endswith((".html", ".htm", ".xhtml"))

    async def load(self, source: Union[str, Path, bytes]) -> ParsedDocument:
        """HTMLファイルを読み込んでテキストに変換する"""
        try:
            from bs4 import BeautifulSoup
        except ImportError:
            raise ImportError(
                "beautifulsoup4パッケージがインストールされていません: pip install beautifulsoup4"
            )

        try:
            # ソースの種類に応じて処理
            if isinstance(source, bytes):
                html_content = source.decode("utf-8", errors="ignore")
                source_info = "binary_data"
            elif str(source).startswith(("http://", "https://")):
                # URLの場合
                import aiohttp

                async with aiohttp.ClientSession() as session:
                    async with session.get(str(source)) as response:
                        html_content = await response.text()
                source_info = str(source)
            else:
                # ファイルパスの場合
                with open(source, "r", encoding="utf-8", errors="ignore") as f:
                    html_content = f.read()
                source_info = str(source)

            # BeautifulSoupでパース
            soup = BeautifulSoup(html_content, "html.parser")

            # 不要な要素を除去
            if self.remove_scripts:
                for script in soup.find_all("script"):
                    script.decompose()

            if self.remove_styles:
                for style in soup.find_all("style"):
                    style.decompose()

            # メタデータを抽出
            title = soup.find("title")
            title_text = title.get_text().strip() if title else ""

            meta_description = soup.find("meta", attrs={"name": "description"})
            description = (
                meta_description.get("content", "") if meta_description else ""
            )

            metadata = {
                "source": source_info,
                "source_type": "html",
                "title": title_text,
                "description": description,
                "language": soup.get("lang", ""),
            }

            # テキストを抽出
            text_content = soup.get_text(separator="\n", strip=True)

            # 空行を整理
            lines = [line.strip() for line in text_content.split("\n") if line.strip()]
            clean_text = "\n".join(lines)

            logger.info(f"HTMLを正常に読み込みました: {len(clean_text)}文字")

            return ParsedDocument(
                text=clean_text, metadata=metadata, chunks=None, source_type="html"
            )

        except Exception as e:
            logger.error(f"HTML読み込みエラー: {e}")
            raise


class ImageLoader(DocumentLoader):
    """画像ドキュメントローダー（OCR機能付き）"""

    def __init__(self, ocr_engine: str = "tesseract", **kwargs):
        """
        画像ローダーを初期化する

        Args:
            ocr_engine: 使用するOCRエンジン（tesseract等）
            **kwargs: その他の設定
        """
        super().__init__(**kwargs)
        self.ocr_engine = ocr_engine

        logger.info(f"ImageLoader を初期化しました (OCR: {ocr_engine})")

    def supports(self, source: Union[str, Path]) -> bool:
        """画像ファイルをサポートするかチェック"""
        mime_type = self._get_mime_type(source)
        return (
            mime_type
            and mime_type.startswith("image/")
            or str(source)
            .lower()
            .endswith((".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff"))
        )

    async def load(self, source: Union[str, Path, bytes]) -> ParsedDocument:
        """画像ファイルを読み込んでOCRでテキストに変換する"""
        try:
            import pytesseract
            from PIL import Image
        except ImportError:
            raise ImportError(
                "Pillow と pytesseract パッケージが必要です: pip install Pillow pytesseract"
            )

        try:
            # ソースの種類に応じて処理
            if isinstance(source, bytes):
                image = Image.open(BytesIO(source))
                source_info = "binary_data"
            else:
                image = Image.open(source)
                source_info = str(source)

            # 画像のメタデータを取得
            metadata = {
                "source": source_info,
                "source_type": "image",
                "format": image.format,
                "mode": image.mode,
                "size": image.size,
                "ocr_engine": self.ocr_engine,
            }

            # OCRでテキストを抽出
            if self.ocr_engine == "tesseract":
                # 日本語と英語の両方を認識
                text_content = pytesseract.image_to_string(
                    image, lang="jpn+eng", config="--psm 6"
                )
            else:
                raise ValueError(f"サポートされていないOCRエンジン: {self.ocr_engine}")

            # テキストを整理
            lines = [line.strip() for line in text_content.split("\n") if line.strip()]
            clean_text = "\n".join(lines)

            logger.info(f"画像OCRを正常に完了しました: {len(clean_text)}文字")

            return ParsedDocument(
                text=clean_text, metadata=metadata, chunks=None, source_type="image"
            )

        except Exception as e:
            logger.error(f"画像OCRエラー: {e}")
            raise


class TextLoader(DocumentLoader):
    """プレーンテキストドキュメントローダー"""

    def __init__(self, encoding: str = "utf-8", **kwargs):
        """
        テキストローダーを初期化する

        Args:
            encoding: ファイルエンコーディング
            **kwargs: その他の設定
        """
        super().__init__(**kwargs)
        self.encoding = encoding

        logger.info(f"TextLoader を初期化しました (encoding: {encoding})")

    def supports(self, source: Union[str, Path]) -> bool:
        """テキストファイルをサポートするかチェック"""
        mime_type = self._get_mime_type(source)
        return (
            mime_type
            and mime_type.startswith("text/")
            or str(source).lower().endswith((".txt", ".md", ".rst", ".log"))
        )

    async def load(self, source: Union[str, Path, bytes]) -> ParsedDocument:
        """テキストファイルを読み込む"""
        try:
            # ソースの種類に応じて処理
            if isinstance(source, bytes):
                text_content = source.decode(self.encoding, errors="ignore")
                source_info = "binary_data"
            else:
                with open(source, "r", encoding=self.encoding, errors="ignore") as f:
                    text_content = f.read()
                source_info = str(source)

            # メタデータを設定
            metadata = {
                "source": source_info,
                "source_type": "text",
                "encoding": self.encoding,
                "character_count": len(text_content),
                "line_count": text_content.count("\n") + 1,
            }

            logger.info(f"テキストファイルを正常に読み込みました: {len(text_content)}文字")

            return ParsedDocument(
                text=text_content, metadata=metadata, chunks=None, source_type="text"
            )

        except Exception as e:
            logger.error(f"テキスト読み込みエラー: {e}")
            raise


class DocumentLoaderFactory:
    """ドキュメントローダーファクトリー"""

    def __init__(self):
        """ファクトリーを初期化する"""
        self.loaders = [
            PDFLoader(),
            HTMLLoader(),
            ImageLoader(),
            TextLoader(),
        ]

        logger.info("DocumentLoaderFactory を初期化しました")

    def get_loader(self, source: Union[str, Path]) -> Optional[DocumentLoader]:
        """
        ソースに適したローダーを取得する

        Args:
            source: ドキュメントのソース

        Returns:
            Optional[DocumentLoader]: 適切なローダー、見つからない場合はNone
        """
        for loader in self.loaders:
            if loader.supports(source):
                return loader
        return None

    async def load_document(self, source: Union[str, Path, bytes]) -> ParsedDocument:
        """
        ドキュメントを自動的に適切なローダーで読み込む

        Args:
            source: ドキュメントのソース

        Returns:
            ParsedDocument: パース済みドキュメント
        """
        if isinstance(source, bytes):
            # バイナリデータの場合、すべてのローダーを試す
            for loader in self.loaders:
                try:
                    return await loader.load(source)
                except Exception:
                    continue
            raise ValueError("適切なローダーが見つかりませんでした")

        # この時点でsourceはstr | Pathのみ
        loader = self.get_loader(source)  # type: ignore[assignment]
        if loader is None:
            raise ValueError(f"サポートされていないファイル形式: {source}")

        # この時点でloaderはNoneではない
        return await loader.load(source)
