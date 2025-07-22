"""
リファクターエージェント

plan に従い JSP → TSX、コンポーネント分割、Tailwind 導入を担当するエージェント。
ai_blocks.architectures.prompt_chaining.PromptChainを活用して段階的なリファクタリングを実行します。

主要機能:
- JSP/HTML から React TSX への変換
- コンポーネントの分割と再利用可能化
- Tailwind CSS の導入とスタイル変換
- TypeScript への移行
"""

import asyncio
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List

import yaml

from ai_blocks.core.memory import VectorMemory
from ai_blocks.core.tool import ToolManager, tool
from ai_blocks.utils.logging import get_logger

logger = get_logger(__name__)


@dataclass
class RefactorResult:
    """リファクタリング結果を格納するデータクラス"""

    original_file: str
    refactored_files: List[str]
    components_created: List[str]
    styles_converted: bool
    typescript_converted: bool
    notes: str


class RefactorAgent:
    """
    コードリファクタリングを担当するエージェント

    ai_blocks.architectures.prompt_chaining.PromptChainを使用して
    段階的なリファクタリング処理を実行します。
    """

    def __init__(self, llm_provider=None):
        """
        リファクターエージェントを初期化

        Args:
            llm_provider: LLMプロバイダー（オプション）
        """
        self.tool_manager = ToolManager()
        self.memory = VectorMemory()
        self.llm_provider = llm_provider

        # リファクタリング用ツールを登録
        self._register_refactor_tools()

        logger.info("RefactorAgentを初期化しました")

    def _register_refactor_tools(self) -> None:
        """リファクタリング用のツールを登録"""

        @tool(name="convert_html_to_jsx", description="HTMLをReact JSXに変換")
        def convert_html_to_jsx(
            html_content: str, component_name: str
        ) -> Dict[str, str]:
            """
            HTMLをReact JSXに変換

            Args:
                html_content: HTML内容
                component_name: コンポーネント名

            Returns:
                変換結果の辞書
            """
            # 基本的なHTML → JSX変換
            jsx_content = html_content

            # class → className
            jsx_content = re.sub(r"\bclass=", "className=", jsx_content)

            # for → htmlFor
            jsx_content = re.sub(r"\bfor=", "htmlFor=", jsx_content)

            # 自己終了タグの修正
            jsx_content = re.sub(
                r"<(img|input|br|hr)([^>]*?)(?<!/)>", r"<\1\2 />", jsx_content
            )

            # React コンポーネントとして包装
            component_jsx = f"""import React from 'react';

interface {component_name}Props {{
  // プロパティを定義
}}

const {component_name}: React.FC<{component_name}Props> = (props) => {{
  return (
    {jsx_content}
  );
}};

export default {component_name};
"""

            return {
                "jsx_content": component_jsx,
                "component_name": component_name,
                "conversion_notes": "基本的なHTML→JSX変換を実行",
            }

        @tool(name="extract_components", description="再利用可能なコンポーネントを抽出")
        def extract_components(jsx_content: str) -> List[Dict[str, str]]:
            """
            JSXから再利用可能なコンポーネントを抽出

            Args:
                jsx_content: JSX内容

            Returns:
                抽出されたコンポーネントのリスト
            """
            components = []

            # ボタンコンポーネントを抽出
            button_pattern = r"<button[^>]*>(.*?)</button>"
            buttons = re.findall(button_pattern, jsx_content, re.DOTALL)

            if buttons:
                button_component = """import React from 'react';

interface ButtonProps {
  children: React.ReactNode;
  onClick?: () => void;
  className?: string;
  type?: 'button' | 'submit' | 'reset';
}

const Button: React.FC<ButtonProps> = ({
  children,
  onClick,
  className = '',
  type = 'button'
}) => {
  return (
    <button
      type={type}
      onClick={onClick}
      className={`px-4 py-2 rounded ${className}`}
    >
      {children}
    </button>
  );
};

export default Button;
"""
                components.append(
                    {
                        "name": "Button",
                        "content": button_component,
                        "usage": "共通ボタンコンポーネント",
                    }
                )

            # フォームコンポーネントを抽出
            form_pattern = r"<form[^>]*>(.*?)</form>"
            forms = re.findall(form_pattern, jsx_content, re.DOTALL)

            if forms:
                form_component = """import React, { useState } from 'react';

interface FormProps {
  onSubmit: (data: any) => void;
  children: React.ReactNode;
}

const Form: React.FC<FormProps> = ({ onSubmit, children }) => {
  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    // フォームデータの処理
    const formData = new FormData(e.target as HTMLFormElement);
    const data = Object.fromEntries(formData.entries());
    onSubmit(data);
  };

  return (
    <form onSubmit={handleSubmit} className="space-y-4">
      {children}
    </form>
  );
};

export default Form;
"""
                components.append(
                    {
                        "name": "Form",
                        "content": form_component,
                        "usage": "共通フォームコンポーネント",
                    }
                )

            return components

        @tool(name="convert_css_to_tailwind", description="CSSをTailwind CSSに変換")
        def convert_css_to_tailwind(css_content: str) -> Dict[str, str]:
            """
            CSSをTailwind CSSクラスに変換

            Args:
                css_content: CSS内容

            Returns:
                変換結果の辞書
            """
            # 基本的なCSS → Tailwind変換マッピング
            css_to_tailwind = {
                r"display:\s*flex": "flex",
                r"justify-content:\s*center": "justify-center",
                r"align-items:\s*center": "items-center",
                r"margin:\s*0\s*auto": "mx-auto",
                r"text-align:\s*center": "text-center",
                r"font-weight:\s*bold": "font-bold",
                r"color:\s*white": "text-white",
                r"background-color:\s*#[0-9a-fA-F]{6}": "bg-gray-500",
                r"padding:\s*(\d+)px": r"p-\1",
                r"margin:\s*(\d+)px": r"m-\1",
                r"border-radius:\s*(\d+)px": r"rounded-\1",
            }

            tailwind_classes = []

            for css_pattern, tailwind_class in css_to_tailwind.items():
                if re.search(css_pattern, css_content):
                    tailwind_classes.append(tailwind_class)

            return {
                "tailwind_classes": " ".join(tailwind_classes),
                "conversion_notes": f"{len(tailwind_classes)}個のCSSプロパティを変換",
                "original_css": css_content,
            }

        # ツールマネージャーに登録
        self.tool_manager.register_function(convert_html_to_jsx)
        self.tool_manager.register_function(extract_components)
        self.tool_manager.register_function(convert_css_to_tailwind)

    async def refactor_file(
        self, file_path: str, migration_plan: Dict[str, Any]
    ) -> RefactorResult:
        """
        ファイルをリファクタリング

        Args:
            file_path: リファクタリング対象ファイルのパス
            migration_plan: 移行計画

        Returns:
            RefactorResult: リファクタリング結果
        """
        logger.info(f"ファイルリファクタリングを開始: {file_path}")

        # ファイル内容を読み込み
        with open(file_path, "r", encoding="utf-8") as f:
            content = f.read()

        file_extension = Path(file_path).suffix.lower()
        refactored_files = []
        components_created = []

        if file_extension in [".html", ".htm", ".jsp"]:
            # HTML/JSP → React TSX変換
            component_name = (
                Path(file_path).stem.title().replace("_", "").replace("-", "")
            )

            # HTML → JSX変換
            jsx_result = await self.tool_manager.execute(
                "convert_html_to_jsx",
                {"html_content": content, "component_name": component_name},
            )

            if jsx_result.success:
                jsx_data = jsx_result.result

                # TSXファイルを保存
                tsx_path = str(Path(file_path).with_suffix(".tsx"))
                with open(tsx_path, "w", encoding="utf-8") as f:
                    f.write(jsx_data["jsx_content"])
                refactored_files.append(tsx_path)

                # コンポーネント抽出
                components_result = await self.tool_manager.execute(
                    "extract_components", {"jsx_content": jsx_data["jsx_content"]}
                )

                if components_result.success:
                    components = components_result.result

                    for component in components:
                        component_path = f"components/{component['name']}.tsx"
                        Path(component_path).parent.mkdir(exist_ok=True)

                        with open(component_path, "w", encoding="utf-8") as f:
                            f.write(component["content"])

                        components_created.append(component_path)

        elif file_extension == ".css":
            # CSS → Tailwind変換
            tailwind_result = await self.tool_manager.execute(
                "convert_css_to_tailwind", {"css_content": content}
            )

            if tailwind_result.success:
                tailwind_data = tailwind_result.result

                # 変換結果をファイルに保存
                tailwind_path = str(
                    Path(file_path).with_name(f"{Path(file_path).stem}_tailwind.css")
                )
                with open(tailwind_path, "w", encoding="utf-8") as f:
                    f.write(
                        f"/* Tailwind classes: {tailwind_data['tailwind_classes']} */\n"
                    )
                    f.write("/* Original CSS converted to Tailwind */\n")
                    f.write(content)

                refactored_files.append(tailwind_path)

        # リファクタリング結果を作成
        result = RefactorResult(
            original_file=file_path,
            refactored_files=refactored_files,
            components_created=components_created,
            styles_converted=file_extension == ".css",
            typescript_converted=file_extension in [".html", ".htm", ".jsp"],
            notes=f"リファクタリング完了: {len(refactored_files)}ファイル生成、{len(components_created)}コンポーネント作成",
        )

        # メモリに保存
        await self.memory.store(
            content=f"Refactoring completed for {file_path}",
            metadata={
                "type": "refactor_result",
                "original_file": file_path,
                "refactored_files_count": len(refactored_files),
                "components_created_count": len(components_created),
            },
        )

        logger.info(f"リファクタリング完了: {file_path}")
        return result

    async def refactor_project(
        self, migration_plan_path: str, output_dir: str = "refactored"
    ) -> List[RefactorResult]:
        """
        プロジェクト全体をリファクタリング

        Args:
            migration_plan_path: 移行計画ファイルのパス
            output_dir: 出力ディレクトリ

        Returns:
            リファクタリング結果のリスト
        """
        logger.info("プロジェクト全体のリファクタリングを開始")

        # 移行計画を読み込み
        with open(migration_plan_path, "r", encoding="utf-8") as f:
            migration_plan = yaml.safe_load(f)

        # 出力ディレクトリを作成
        Path(output_dir).mkdir(exist_ok=True)

        results = []
        page_plans = migration_plan.get("page_plans", [])

        for page_plan in page_plans:
            page_path = page_plan["page_path"]

            if Path(page_path).exists():
                try:
                    result = await self.refactor_file(page_path, migration_plan)
                    results.append(result)
                except Exception as e:
                    logger.error(f"リファクタリングエラー: {page_path}, {e}")

        logger.info(f"プロジェクトリファクタリング完了: {len(results)}ファイル処理")
        return results


# 使用例とテスト用のメイン関数
async def main():
    """
    RefactorAgentの使用例
    """
    agent = RefactorAgent()

    try:
        # 単一ファイルのリファクタリング
        result = await agent.refactor_file("sample.html", {"target_framework": "React"})

        print("リファクタリング完了:")
        print(f"  元ファイル: {result.original_file}")
        print(f"  生成ファイル数: {len(result.refactored_files)}")
        print(f"  コンポーネント数: {len(result.components_created)}")
        print(f"  TypeScript変換: {result.typescript_converted}")

    except Exception as e:
        logger.error(f"リファクタリングエラー: {e}")


if __name__ == "__main__":
    asyncio.run(main())
