"""
テストエージェント

Jest + React Testing Library を使った自動テスト生成を担当するエージェント。
移行されたコンポーネントに対して包括的なテストスイートを自動生成します。

主要機能:
- React コンポーネントの単体テスト生成
- インテグレーションテストの作成
- アクセシビリティテストの追加
- パフォーマンステストの実装
"""

import asyncio
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List

from ai_blocks.core.memory import VectorMemory
from ai_blocks.core.tool import ToolManager, tool
from ai_blocks.utils.logging import get_logger

logger = get_logger(__name__)


@dataclass
class TestCase:
    """テストケースを表すデータクラス"""

    name: str
    description: str
    test_type: str  # unit, integration, accessibility, performance
    test_code: str
    dependencies: List[str]


@dataclass
class TestSuite:
    """テストスイートを格納するデータクラス"""

    component_name: str
    test_file_path: str
    test_cases: List[TestCase]
    setup_code: str
    teardown_code: str
    coverage_target: int


class TestAgent:
    """
    自動テスト生成を担当するエージェント

    ai_blocks.core.tool.ToolManagerを使用してテスト生成ツールを管理し、
    ai_blocks.core.memory.VectorMemoryを使用してテストパターンを記憶します。
    """

    def __init__(self, llm_provider=None):
        """
        テストエージェントを初期化

        Args:
            llm_provider: LLMプロバイダー（オプション）
        """
        self.tool_manager = ToolManager()
        self.memory = VectorMemory()
        self.llm_provider = llm_provider

        # テスト生成用ツールを登録
        self._register_test_tools()

        logger.info("TestAgentを初期化しました")

    def _register_test_tools(self) -> None:
        """テスト生成用のツールを登録"""

        @tool(name="generate_unit_tests", description="React コンポーネントの単体テストを生成")
        def generate_unit_tests(
            component_code: str, component_name: str
        ) -> List[Dict[str, Any]]:
            """
            React コンポーネントの単体テストを生成

            Args:
                component_code: コンポーネントのコード
                component_name: コンポーネント名

            Returns:
                生成されたテストケースのリスト
            """
            test_cases = []

            # 基本レンダリングテスト
            basic_render_test = f"""
test('renders {component_name} without crashing', () => {{
  render(<{component_name} />);
  expect(screen.getByRole('main')).toBeInTheDocument();
}});
"""
            test_cases.append(
                {
                    "name": f"renders_{component_name.lower()}_without_crashing",
                    "description": f"{component_name}が正常にレンダリングされることを確認",
                    "test_type": "unit",
                    "test_code": basic_render_test,
                    "dependencies": [
                        "@testing-library/react",
                        "@testing-library/jest-dom",
                    ],
                }
            )

            # プロパティテスト
            if "Props" in component_code:
                props_test = f"""
test('renders {component_name} with props', () => {{
  const testProps = {{
    title: 'Test Title',
    onClick: jest.fn()
  }};

  render(<{component_name} {{...testProps}} />);

  expect(screen.getByText('Test Title')).toBeInTheDocument();
}});
"""
                test_cases.append(
                    {
                        "name": f"renders_{component_name.lower()}_with_props",
                        "description": f"{component_name}がプロパティを正しく表示することを確認",
                        "test_type": "unit",
                        "test_code": props_test,
                        "dependencies": [
                            "@testing-library/react",
                            "@testing-library/jest-dom",
                        ],
                    }
                )

            # イベントハンドリングテスト
            if "onClick" in component_code or "onSubmit" in component_code:
                event_test = f"""
test('handles {component_name} events correctly', () => {{
  const mockHandler = jest.fn();

  render(<{component_name} onClick={{mockHandler}} />);

  const element = screen.getByRole('button');
  fireEvent.click(element);

  expect(mockHandler).toHaveBeenCalledTimes(1);
}});
"""
                test_cases.append(
                    {
                        "name": f"handles_{component_name.lower()}_events",
                        "description": f"{component_name}のイベントハンドリングを確認",
                        "test_type": "unit",
                        "test_code": event_test,
                        "dependencies": [
                            "@testing-library/react",
                            "@testing-library/user-event",
                        ],
                    }
                )

            return test_cases

        @tool(name="generate_accessibility_tests", description="アクセシビリティテストを生成")
        def generate_accessibility_tests(component_name: str) -> List[Dict[str, Any]]:
            """
            アクセシビリティテストを生成

            Args:
                component_name: コンポーネント名

            Returns:
                生成されたアクセシビリティテストのリスト
            """
            a11y_tests = []

            # axe-core を使用したアクセシビリティテスト
            axe_test = f"""
test('{component_name} has no accessibility violations', async () => {{
  const {{ container }} = render(<{component_name} />);
  const results = await axe(container);

  expect(results).toHaveNoViolations();
}});
"""
            a11y_tests.append(
                {
                    "name": f"{component_name.lower()}_accessibility_violations",
                    "description": f"{component_name}にアクセシビリティ違反がないことを確認",
                    "test_type": "accessibility",
                    "test_code": axe_test,
                    "dependencies": ["jest-axe", "@testing-library/react"],
                }
            )

            # キーボードナビゲーションテスト
            keyboard_test = f"""
test('{component_name} supports keyboard navigation', () => {{
  render(<{component_name} />);

  const focusableElement = screen.getByRole('button');
  focusableElement.focus();

  expect(focusableElement).toHaveFocus();

  fireEvent.keyDown(focusableElement, {{ key: 'Enter', code: 'Enter' }});
  // Enter キーでの動作を確認
}});
"""
            a11y_tests.append(
                {
                    "name": f"{component_name.lower()}_keyboard_navigation",
                    "description": f"{component_name}のキーボードナビゲーションを確認",
                    "test_type": "accessibility",
                    "test_code": keyboard_test,
                    "dependencies": [
                        "@testing-library/react",
                        "@testing-library/user-event",
                    ],
                }
            )

            # ARIA属性テスト
            aria_test = f"""
test('{component_name} has proper ARIA attributes', () => {{
  render(<{component_name} />);

  const element = screen.getByRole('main');
  expect(element).toHaveAttribute('aria-label');
  expect(element).toHaveAttribute('role', 'main');
}});
"""
            a11y_tests.append(
                {
                    "name": f"{component_name.lower()}_aria_attributes",
                    "description": f"{component_name}が適切なARIA属性を持つことを確認",
                    "test_type": "accessibility",
                    "test_code": aria_test,
                    "dependencies": [
                        "@testing-library/react",
                        "@testing-library/jest-dom",
                    ],
                }
            )

            return a11y_tests

        @tool(name="generate_integration_tests", description="インテグレーションテストを生成")
        def generate_integration_tests(
            component_name: str, dependencies: List[str]
        ) -> List[Dict[str, Any]]:
            """
            インテグレーションテストを生成

            Args:
                component_name: コンポーネント名
                dependencies: 依存コンポーネントのリスト

            Returns:
                生成されたインテグレーションテストのリスト
            """
            integration_tests = []

            # コンポーネント間の連携テスト
            if dependencies:
                integration_test = f"""
test('{component_name} integrates with child components', () => {{
  render(
    <{component_name}>
      {' '.join([f'<{dep} />' for dep in dependencies])}
    </{component_name}>
  );

  // 子コンポーネントが正しくレンダリングされることを確認
  {chr(10).join([
      f'  expect(screen.getByTestId("{dep.lower()}")).toBeInTheDocument();'
      for dep in dependencies
  ])}
}});
"""
                integration_tests.append(
                    {
                        "name": f"{component_name.lower()}_child_component_integration",
                        "description": f"{component_name}と子コンポーネントの連携を確認",
                        "test_type": "integration",
                        "test_code": integration_test,
                        "dependencies": [
                            "@testing-library/react",
                            "@testing-library/jest-dom",
                        ],
                    }
                )

            # データフローテスト
            data_flow_test = f"""
test('{component_name} handles data flow correctly', async () => {{
  const mockData = {{ id: 1, name: 'Test Data' }};

  render(<{component_name} data={{mockData}} />);

  // データが正しく表示されることを確認
  await waitFor(() => {{
    expect(screen.getByText('Test Data')).toBeInTheDocument();
  }});
}});
"""
            integration_tests.append(
                {
                    "name": f"{component_name.lower()}_data_flow",
                    "description": f"{component_name}のデータフローを確認",
                    "test_type": "integration",
                    "test_code": data_flow_test,
                    "dependencies": [
                        "@testing-library/react",
                        "@testing-library/jest-dom",
                    ],
                }
            )

            return integration_tests

        @tool(name="generate_performance_tests", description="パフォーマンステストを生成")
        def generate_performance_tests(component_name: str) -> List[Dict[str, Any]]:
            """
            パフォーマンステストを生成

            Args:
                component_name: コンポーネント名

            Returns:
                生成されたパフォーマンステストのリスト
            """
            performance_tests = []

            # レンダリング時間テスト
            render_performance_test = f"""
test('{component_name} renders within acceptable time', () => {{
  const startTime = performance.now();

  render(<{component_name} />);

  const endTime = performance.now();
  const renderTime = endTime - startTime;

  // 100ms以内でレンダリングされることを確認
  expect(renderTime).toBeLessThan(100);
}});
"""
            performance_tests.append(
                {
                    "name": f"{component_name.lower()}_render_performance",
                    "description": f"{component_name}のレンダリング時間を確認",
                    "test_type": "performance",
                    "test_code": render_performance_test,
                    "dependencies": ["@testing-library/react"],
                }
            )

            # メモリリークテスト
            memory_test = f"""
test('{component_name} does not cause memory leaks', () => {{
  const {{ unmount }} = render(<{component_name} />);

  // コンポーネントをアンマウント
  unmount();

  // メモリリークがないことを確認（簡易版）
  expect(document.body.innerHTML).toBe('');
}});
"""
            performance_tests.append(
                {
                    "name": f"{component_name.lower()}_memory_leak",
                    "description": f"{component_name}がメモリリークを起こさないことを確認",
                    "test_type": "performance",
                    "test_code": memory_test,
                    "dependencies": ["@testing-library/react"],
                }
            )

            return performance_tests

        @tool(name="generate_test_setup", description="テストセットアップコードを生成")
        def generate_test_setup(
            component_name: str, dependencies: List[str]
        ) -> Dict[str, str]:
            """
            テストセットアップコードを生成

            Args:
                component_name: コンポーネント名
                dependencies: 依存関係のリスト

            Returns:
                セットアップコード
            """
            imports = [
                "import React from 'react';",
                "import { render, screen, fireEvent, waitFor } from '@testing-library/react';",
                "import '@testing-library/jest-dom';",
                "import userEvent from '@testing-library/user-event';",
                f"import {component_name} from '../{component_name}';",
            ]

            # 依存関係のインポートを追加
            for dep in dependencies:
                if "jest-axe" in dep:
                    imports.append(
                        "import { axe, toHaveNoViolations } from 'jest-axe';"
                    )
                elif dep not in ["@testing-library/react", "@testing-library/jest-dom"]:
                    imports.append(f"import '{dep}';")

            setup_code = f"""
{chr(10).join(imports)}

// jest-axe のマッチャーを拡張
expect.extend(toHaveNoViolations);

// テスト用のモックデータ
const mockProps = {{
  title: 'Test Title',
  onClick: jest.fn(),
  data: {{ id: 1, name: 'Test Data' }}
}};

// 各テスト前の共通セットアップ
beforeEach(() => {{
  jest.clearAllMocks();
}});

// 各テスト後のクリーンアップ
afterEach(() => {{
  jest.restoreAllMocks();
}});
"""

            return {
                "setup_code": setup_code,
                "teardown_code": "// テスト後のクリーンアップは afterEach で実行",
                "mock_data": "mockProps",
            }

        # ツールマネージャーに登録
        self.tool_manager.register_function(generate_unit_tests)
        self.tool_manager.register_function(generate_accessibility_tests)
        self.tool_manager.register_function(generate_integration_tests)
        self.tool_manager.register_function(generate_performance_tests)
        self.tool_manager.register_function(generate_test_setup)

    async def generate_test_suite(
        self, component_file_path: str, test_types: List[str] = None
    ) -> TestSuite:
        """
        コンポーネントのテストスイートを生成

        Args:
            component_file_path: コンポーネントファイルのパス
            test_types: 生成するテストタイプのリスト

        Returns:
            TestSuite: 生成されたテストスイート
        """
        logger.info(f"テストスイート生成を開始: {component_file_path}")

        test_types = test_types or [
            "unit",
            "accessibility",
            "integration",
            "performance",
        ]

        # コンポーネントファイルを読み込み
        with open(component_file_path, "r", encoding="utf-8") as f:
            component_code = f.read()

        component_name = Path(component_file_path).stem
        all_test_cases = []
        all_dependencies = set()

        # 各テストタイプを生成
        for test_type in test_types:
            if test_type == "unit":
                unit_result = await self.tool_manager.execute(
                    "generate_unit_tests",
                    {
                        "component_code": component_code,
                        "component_name": component_name,
                    },
                )

                if unit_result.success:
                    for test_data in unit_result.result:
                        test_case = TestCase(
                            name=test_data["name"],
                            description=test_data["description"],
                            test_type=test_data["test_type"],
                            test_code=test_data["test_code"],
                            dependencies=test_data["dependencies"],
                        )
                        all_test_cases.append(test_case)
                        all_dependencies.update(test_data["dependencies"])

            elif test_type == "accessibility":
                a11y_result = await self.tool_manager.execute(
                    "generate_accessibility_tests", {"component_name": component_name}
                )

                if a11y_result.success:
                    for test_data in a11y_result.result:
                        test_case = TestCase(
                            name=test_data["name"],
                            description=test_data["description"],
                            test_type=test_data["test_type"],
                            test_code=test_data["test_code"],
                            dependencies=test_data["dependencies"],
                        )
                        all_test_cases.append(test_case)
                        all_dependencies.update(test_data["dependencies"])

            elif test_type == "integration":
                integration_result = await self.tool_manager.execute(
                    "generate_integration_tests",
                    {
                        "component_name": component_name,
                        "dependencies": ["Button", "Form"],  # 例として
                    },
                )

                if integration_result.success:
                    for test_data in integration_result.result:
                        test_case = TestCase(
                            name=test_data["name"],
                            description=test_data["description"],
                            test_type=test_data["test_type"],
                            test_code=test_data["test_code"],
                            dependencies=test_data["dependencies"],
                        )
                        all_test_cases.append(test_case)
                        all_dependencies.update(test_data["dependencies"])

            elif test_type == "performance":
                perf_result = await self.tool_manager.execute(
                    "generate_performance_tests", {"component_name": component_name}
                )

                if perf_result.success:
                    for test_data in perf_result.result:
                        test_case = TestCase(
                            name=test_data["name"],
                            description=test_data["description"],
                            test_type=test_data["test_type"],
                            test_code=test_data["test_code"],
                            dependencies=test_data["dependencies"],
                        )
                        all_test_cases.append(test_case)
                        all_dependencies.update(test_data["dependencies"])

        # セットアップコードを生成
        setup_result = await self.tool_manager.execute(
            "generate_test_setup",
            {"component_name": component_name, "dependencies": list(all_dependencies)},
        )

        setup_code = ""
        teardown_code = ""
        if setup_result.success:
            setup_data = setup_result.result
            setup_code = setup_data["setup_code"]
            teardown_code = setup_data["teardown_code"]

        # テストファイルパスを決定
        test_file_path = str(
            Path(component_file_path).parent / f"{component_name}.test.tsx"
        )

        # テストスイートを作成
        test_suite = TestSuite(
            component_name=component_name,
            test_file_path=test_file_path,
            test_cases=all_test_cases,
            setup_code=setup_code,
            teardown_code=teardown_code,
            coverage_target=90,
        )

        # メモリに保存
        await self.memory.store(
            content=f"Test suite generated for {component_name}",
            metadata={
                "type": "test_suite",
                "component_name": component_name,
                "test_cases_count": len(all_test_cases),
                "test_types": test_types,
            },
        )

        logger.info(f"テストスイート生成完了: {component_name}, {len(all_test_cases)}テストケース")
        return test_suite

    async def save_test_suite(self, test_suite: TestSuite) -> None:
        """
        テストスイートをファイルに保存

        Args:
            test_suite: 保存するテストスイート
        """
        # テストファイルの内容を構築
        test_content = test_suite.setup_code + "\n\n"

        # 各テストケースを追加
        for test_case in test_suite.test_cases:
            test_content += f"// {test_case.description}\n"
            test_content += test_case.test_code + "\n\n"

        test_content += test_suite.teardown_code

        # テストファイルを保存
        with open(test_suite.test_file_path, "w", encoding="utf-8") as f:
            f.write(test_content)

        logger.info(f"テストスイートを保存しました: {test_suite.test_file_path}")


# 使用例とテスト用のメイン関数
async def main():
    """
    TestAgentの使用例
    """
    agent = TestAgent()

    try:
        # テストスイートを生成
        test_suite = await agent.generate_test_suite(
            "components/Button.tsx", test_types=["unit", "accessibility"]
        )

        # テストスイートを保存
        await agent.save_test_suite(test_suite)

        print("テストスイート生成完了:")
        print(f"  コンポーネント: {test_suite.component_name}")
        print(f"  テストケース数: {len(test_suite.test_cases)}")
        print(f"  テストファイル: {test_suite.test_file_path}")
        print(f"  カバレッジ目標: {test_suite.coverage_target}%")

    except Exception as e:
        logger.error(f"テスト生成エラー: {e}")


if __name__ == "__main__":
    asyncio.run(main())
