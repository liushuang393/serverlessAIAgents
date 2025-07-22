"""
インベントリエージェント

Gitリポジトリの清単作成と依存関係バージョンスキャンを担当するエージェント。
ai_blocks.core.tool.ToolManagerを活用してファイルスキャンツールを管理します。

主要機能:
- リポジトリ構造の解析
- ファイルタイプの分類
- 依存関係の抽出
- manifest.jsonの生成
"""

import asyncio
import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List

from ai_blocks.core.memory import VectorMemory
from ai_blocks.core.tool import ToolManager, tool
from ai_blocks.utils.logging import get_logger

logger = get_logger(__name__)


@dataclass
class FileInfo:
    """ファイル情報を格納するデータクラス"""

    path: str
    type: str  # html, css, js, jsp, etc.
    size: int
    last_modified: float
    dependencies: List[str]


@dataclass
class RepositoryManifest:
    """リポジトリマニフェストを格納するデータクラス"""

    project_name: str
    total_files: int
    file_types: Dict[str, int]
    files: List[FileInfo]
    dependencies: Dict[str, str]  # package_name -> version
    framework_info: Dict[str, Any]


class InventoryAgent:
    """
    リポジトリインベントリを作成するエージェント

    ai_blocks.core.tool.ToolManagerを使用してファイルスキャンツールを管理し、
    ai_blocks.core.memory.VectorMemoryを使用してスキャン結果を記憶します。
    """

    def __init__(self, llm_provider=None):
        """
        インベントリエージェントを初期化

        Args:
            llm_provider: LLMプロバイダー（オプション）
        """
        self.tool_manager = ToolManager()
        self.memory = VectorMemory()
        self.llm_provider = llm_provider

        # ファイルスキャン用ツールを登録
        self._register_scan_tools()

        logger.info("InventoryAgentを初期化しました")

    def _register_scan_tools(self) -> None:
        """ファイルスキャン用のツールを登録"""

        @tool(name="scan_directory", description="ディレクトリを再帰的にスキャンしてファイル情報を取得")
        def scan_directory(directory_path: str) -> List[Dict[str, Any]]:
            """
            ディレクトリを再帰的にスキャンしてファイル情報を取得

            Args:
                directory_path: スキャン対象のディレクトリパス

            Returns:
                ファイル情報のリスト
            """
            files: List[Dict[str, Any]] = []
            path = Path(directory_path)

            if not path.exists():
                logger.error(f"ディレクトリが存在しません: {directory_path}")
                return files

            for file_path in path.rglob("*"):
                if file_path.is_file():
                    # 隠しファイルやnode_modules等を除外
                    if self._should_skip_file(file_path):
                        continue

                    file_info = {
                        "path": str(file_path.relative_to(path)),
                        "type": self._get_file_type(file_path),
                        "size": file_path.stat().st_size,
                        "last_modified": file_path.stat().st_mtime,
                    }
                    files.append(file_info)

            logger.info(f"ディレクトリスキャン完了: {len(files)}個のファイルを発見")
            return files

        @tool(name="extract_dependencies", description="package.jsonやpom.xmlから依存関係を抽出")
        def extract_dependencies(project_path: str) -> Dict[str, str]:
            """
            プロジェクトの依存関係を抽出

            Args:
                project_path: プロジェクトのルートパス

            Returns:
                依存関係の辞書（パッケージ名 -> バージョン）
            """
            dependencies = {}
            path = Path(project_path)

            # package.jsonをチェック
            package_json = path / "package.json"
            if package_json.exists():
                try:
                    with open(package_json, "r", encoding="utf-8") as f:
                        data = json.load(f)
                        dependencies.update(data.get("dependencies", {}))
                        dependencies.update(data.get("devDependencies", {}))
                except Exception as e:
                    logger.warning(f"package.json読み込みエラー: {e}")

            # pom.xmlをチェック（Maven）
            pom_xml = path / "pom.xml"
            if pom_xml.exists():
                # 簡単なXML解析（実際の実装ではより堅牢な解析が必要）
                try:
                    with open(pom_xml, "r", encoding="utf-8") as f:
                        _ = f.read()
                        # Maven依存関係の抽出ロジックをここに実装
                        logger.info("Maven依存関係を検出しました")
                except Exception as e:
                    logger.warning(f"pom.xml読み込みエラー: {e}")

            logger.info(f"依存関係抽出完了: {len(dependencies)}個のパッケージ")
            return dependencies

        # ツールマネージャーに登録
        self.tool_manager.register_function(scan_directory)
        self.tool_manager.register_function(extract_dependencies)

    def _should_skip_file(self, file_path: Path) -> bool:
        """
        ファイルをスキップするかどうかを判定

        Args:
            file_path: ファイルパス

        Returns:
            スキップする場合True
        """
        skip_patterns = [
            ".git",
            "node_modules",
            ".vscode",
            ".idea",
            "__pycache__",
            ".pytest_cache",
            "dist",
            "build",
            ".DS_Store",
            "Thumbs.db",
        ]

        # パスの一部にスキップパターンが含まれているかチェック
        path_str = str(file_path)
        for pattern in skip_patterns:
            if pattern in path_str:
                return True

        # 隠しファイルをスキップ
        if file_path.name.startswith("."):
            return True

        return False

    def _get_file_type(self, file_path: Path) -> str:
        """
        ファイルタイプを判定

        Args:
            file_path: ファイルパス

        Returns:
            ファイルタイプ文字列
        """
        suffix = file_path.suffix.lower()

        type_mapping = {
            ".html": "html",
            ".htm": "html",
            ".jsp": "jsp",
            ".css": "css",
            ".scss": "scss",
            ".sass": "sass",
            ".js": "javascript",
            ".jsx": "jsx",
            ".ts": "typescript",
            ".tsx": "tsx",
            ".json": "json",
            ".xml": "xml",
            ".java": "java",
            ".py": "python",
            ".md": "markdown",
            ".txt": "text",
        }

        return type_mapping.get(suffix, "unknown")

    async def scan_repository(self, repo_path: str) -> RepositoryManifest:
        """
        リポジトリをスキャンしてマニフェストを生成

        Args:
            repo_path: リポジトリのパス

        Returns:
            RepositoryManifest: 生成されたマニフェスト
        """
        logger.info(f"リポジトリスキャンを開始: {repo_path}")

        # ディレクトリスキャンを実行
        scan_result = await self.tool_manager.execute(
            "scan_directory", {"directory_path": repo_path}
        )
        if not scan_result.success:
            raise RuntimeError(f"ディレクトリスキャンに失敗: {scan_result.error_message}")

        files_data = scan_result.result

        # 依存関係抽出を実行
        deps_result = await self.tool_manager.execute(
            "extract_dependencies", {"project_path": repo_path}
        )
        dependencies = deps_result.result if deps_result.success else {}

        # ファイル情報を構造化
        files: List[FileInfo] = []
        file_types: Dict[str, int] = {}

        for file_data in files_data:
            file_info = FileInfo(
                path=file_data["path"],
                type=file_data["type"],
                size=file_data["size"],
                last_modified=file_data["last_modified"],
                dependencies=[],  # 後で詳細解析で追加
            )
            files.append(file_info)

            # ファイルタイプ統計を更新
            file_type = file_info.type
            file_types[file_type] = file_types.get(file_type, 0) + 1

        # フレームワーク情報を推測
        framework_info = self._detect_framework(files, dependencies)

        # マニフェストを作成
        manifest = RepositoryManifest(
            project_name=Path(repo_path).name,
            total_files=len(files),
            file_types=file_types,
            files=files,
            dependencies=dependencies,
            framework_info=framework_info,
        )

        # メモリに保存
        await self.memory.store(
            content=f"Repository scan result for {repo_path}",
            metadata={
                "type": "repository_manifest",
                "project_name": manifest.project_name,
                "total_files": manifest.total_files,
                "file_types": manifest.file_types,
            },
        )

        logger.info(
            f"リポジトリスキャン完了: {manifest.total_files}個のファイル、{len(manifest.dependencies)}個の依存関係"
        )
        return manifest

    def _detect_framework(
        self, files: List[FileInfo], dependencies: Dict[str, str]
    ) -> Dict[str, Any]:
        """
        使用されているフレームワークを検出

        Args:
            files: ファイル情報のリスト
            dependencies: 依存関係

        Returns:
            フレームワーク情報
        """
        framework_info: Dict[str, List[str]] = {
            "frontend": [],
            "backend": [],
            "build_tools": [],
            "css_frameworks": [],
        }

        # 依存関係からフレームワークを検出
        if "react" in dependencies:
            framework_info["frontend"].append("React")
        if "vue" in dependencies:
            framework_info["frontend"].append("Vue.js")
        if "angular" in dependencies:
            framework_info["frontend"].append("Angular")
        if "jquery" in dependencies:
            framework_info["frontend"].append("jQuery")

        # ビルドツールを検出
        if "webpack" in dependencies:
            framework_info["build_tools"].append("Webpack")
        if "vite" in dependencies:
            framework_info["build_tools"].append("Vite")
        if "gulp" in dependencies:
            framework_info["build_tools"].append("Gulp")

        # CSSフレームワークを検出
        if "bootstrap" in dependencies:
            framework_info["css_frameworks"].append("Bootstrap")
        if "tailwindcss" in dependencies:
            framework_info["css_frameworks"].append("Tailwind CSS")

        # ファイル拡張子からも推測
        file_extensions = [f.type for f in files]
        if "jsx" in file_extensions or "tsx" in file_extensions:
            if "React" not in framework_info["frontend"]:
                framework_info["frontend"].append("React (inferred)")

        if "jsp" in file_extensions:
            framework_info["backend"].append("JSP")

        return framework_info

    async def save_manifest(
        self, manifest: RepositoryManifest, output_path: str
    ) -> None:
        """
        マニフェストをJSONファイルに保存

        Args:
            manifest: 保存するマニフェスト
            output_path: 出力ファイルパス
        """
        # DataClassをJSONシリアライズ可能な形式に変換
        manifest_dict = {
            "project_name": manifest.project_name,
            "total_files": manifest.total_files,
            "file_types": manifest.file_types,
            "files": [
                {
                    "path": f.path,
                    "type": f.type,
                    "size": f.size,
                    "last_modified": f.last_modified,
                    "dependencies": f.dependencies,
                }
                for f in manifest.files
            ],
            "dependencies": manifest.dependencies,
            "framework_info": manifest.framework_info,
        }

        with open(output_path, "w", encoding="utf-8") as f:
            json.dump(manifest_dict, f, indent=2, ensure_ascii=False)

        logger.info(f"マニフェストを保存しました: {output_path}")


# 使用例とテスト用のメイン関数
async def main():
    """
    InventoryAgentの使用例
    """
    agent = InventoryAgent()

    # サンプルリポジトリをスキャン
    repo_path = "/path/to/sample/repository"
    try:
        manifest = await agent.scan_repository(repo_path)
        await agent.save_manifest(manifest, "manifest.json")

        print("スキャン完了:")
        print(f"  プロジェクト名: {manifest.project_name}")
        print(f"  総ファイル数: {manifest.total_files}")
        print(f"  ファイルタイプ: {manifest.file_types}")
        print(f"  依存関係数: {len(manifest.dependencies)}")
        print(f"  フレームワーク: {manifest.framework_info}")

    except Exception as e:
        logger.error(f"スキャンエラー: {e}")


if __name__ == "__main__":
    asyncio.run(main())
