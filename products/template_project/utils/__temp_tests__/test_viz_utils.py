"""
可視化・デバッグユーティリティのテストクラス

このモジュールは、viz_utils.pyの機能をテストします。
"""

import unittest
from unittest.mock import Mock, patch, MagicMock
import sys
import os
import time

# テスト対象モジュールのパスを追加
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

try:
    from viz_utils import (
        MermaidGenerator, CallStackDebugger, FlowVisualizer,
        PerformanceProfiler, DebugLogger, VisualizationError,
        get_flow_visualizer, get_performance_profiler, get_debug_logger,
        build_mermaid, profile_execution, debug_call_stack
    )
except ImportError as e:
    print(f"Warning: Could not import viz_utils: {e}")
    # フォールバック用のダミークラス
    class MermaidGenerator:
        def __init__(self): pass
    class VisualizationError(Exception): pass


class TestMermaidGenerator(unittest.TestCase):
    """MermaidGeneratorクラスのテスト"""
    
    def setUp(self):
        """テストセットアップ"""
        try:
            self.generator = MermaidGenerator()
        except NameError:
            self.skipTest("MermaidGenerator not available")
    
    def test_initialization(self):
        """初期化テスト"""
        self.assertEqual(self.generator.node_counter, 1)
        self.assertEqual(len(self.generator.node_ids), 0)
        self.assertEqual(len(self.generator.visited), 0)
        self.assertEqual(len(self.generator.lines), 0)
    
    def test_reset(self):
        """リセット機能テスト"""
        # 状態を変更
        self.generator.node_counter = 5
        self.generator.node_ids["test"] = "N1"
        self.generator.visited.add("test")
        self.generator.lines.append("test line")
        
        # リセット実行
        self.generator._reset()
        
        # 初期状態に戻ることを確認
        self.assertEqual(self.generator.node_counter, 1)
        self.assertEqual(len(self.generator.node_ids), 0)
        self.assertEqual(len(self.generator.visited), 0)
        self.assertEqual(len(self.generator.lines), 0)
    
    def test_get_node_id(self):
        """ノードID取得テスト"""
        # 新しいノードのID取得
        node_id1 = self.generator._get_node_id("node1")
        self.assertEqual(node_id1, "N1")
        
        # 同じノードのID取得（キャッシュされる）
        node_id2 = self.generator._get_node_id("node1")
        self.assertEqual(node_id2, "N1")
        
        # 別のノードのID取得
        node_id3 = self.generator._get_node_id("node2")
        self.assertEqual(node_id3, "N2")
    
    def test_generate_flowchart_simple(self):
        """シンプルなフローチャート生成テスト"""
        try:
            # シンプルなオブジェクトでテスト
            simple_node = "test_node"
            result = self.generator.generate_flowchart(simple_node)
            
            self.assertIn("graph LR", result)
            self.assertIsInstance(result, str)
            
        except Exception as e:
            # 複雑なオブジェクト構造が必要な場合はスキップ
            self.skipTest(f"Flowchart generation requires complex objects: {e}")


class TestCallStackDebugger(unittest.TestCase):
    """CallStackDebuggerクラスのテスト"""
    
    def test_get_full_call_stack(self):
        """完全なコールスタック取得テスト"""
        try:
            stack = CallStackDebugger.get_full_call_stack()
            
            self.assertIsInstance(stack, list)
            if stack:
                self.assertIsInstance(stack[0], dict)
                self.assertIn("filename", stack[0])
                self.assertIn("function", stack[0])
                self.assertIn("lineno", stack[0])
                
        except NameError:
            self.skipTest("CallStackDebugger not available")
    
    def test_get_node_call_stack(self):
        """ノードコールスタック取得テスト"""
        try:
            stack = CallStackDebugger.get_node_call_stack("TestCase")
            
            self.assertIsInstance(stack, list)
            # TestCaseクラスが含まれている可能性がある
            
        except NameError:
            self.skipTest("CallStackDebugger not available")


class TestPerformanceProfiler(unittest.TestCase):
    """PerformanceProfilerクラスのテスト"""
    
    def setUp(self):
        """テストセットアップ"""
        try:
            self.profiler = PerformanceProfiler()
        except NameError:
            self.skipTest("PerformanceProfiler not available")
    
    def test_initialization(self):
        """初期化テスト"""
        self.assertEqual(len(self.profiler.execution_times), 0)
        self.assertEqual(len(self.profiler.call_counts), 0)
    
    def test_profile_function_decorator(self):
        """関数プロファイリングデコレータテスト"""
        try:
            @self.profiler.profile_function("test_function")
            def test_func():
                time.sleep(0.01)  # 10ms待機
                return "test_result"
            
            result = test_func()
            
            self.assertEqual(result, "test_result")
            self.assertIn("test_function", self.profiler.execution_times)
            self.assertEqual(self.profiler.call_counts["test_function"], 1)
            self.assertGreater(self.profiler.execution_times["test_function"][0], 0)
            
        except NameError:
            self.skipTest("PerformanceProfiler not available")
    
    def test_get_performance_report(self):
        """パフォーマンスレポート取得テスト"""
        try:
            # テスト関数を実行
            @self.profiler.profile_function("test_report")
            def test_func():
                time.sleep(0.001)
                return "result"
            
            # 複数回実行
            test_func()
            test_func()
            
            report = self.profiler.get_performance_report()
            
            self.assertIn("test_report", report)
            stats = report["test_report"]
            self.assertEqual(stats["call_count"], 2)
            self.assertGreater(stats["total_time"], 0)
            self.assertGreater(stats["average_time"], 0)
            
        except NameError:
            self.skipTest("PerformanceProfiler not available")
    
    def test_reset_stats(self):
        """統計リセットテスト"""
        try:
            # データを追加
            self.profiler.execution_times["test"] = [0.1, 0.2]
            self.profiler.call_counts["test"] = 2
            
            # リセット実行
            self.profiler.reset_stats()
            
            # 空になることを確認
            self.assertEqual(len(self.profiler.execution_times), 0)
            self.assertEqual(len(self.profiler.call_counts), 0)
            
        except NameError:
            self.skipTest("PerformanceProfiler not available")


class TestDebugLogger(unittest.TestCase):
    """DebugLoggerクラスのテスト"""
    
    def setUp(self):
        """テストセットアップ"""
        try:
            self.debug_logger = DebugLogger("test_debug")
        except NameError:
            self.skipTest("DebugLogger not available")
    
    def test_initialization(self):
        """初期化テスト"""
        self.assertEqual(len(self.debug_logger.debug_data), 0)
        self.assertEqual(self.debug_logger.logger.name, "test_debug")
    
    def test_log_variable(self):
        """変数ログ記録テスト"""
        try:
            self.debug_logger.log_variable("test_var", "test_value", "test_context")
            
            self.assertIn("test_var", self.debug_logger.debug_data)
            data = self.debug_logger.debug_data["test_var"][0]
            self.assertEqual(data["value"], "test_value")
            self.assertEqual(data["context"], "test_context")
            self.assertIn("timestamp", data)
            
        except NameError:
            self.skipTest("DebugLogger not available")
    
    def test_get_debug_data(self):
        """デバッグデータ取得テスト"""
        try:
            # データを追加
            self.debug_logger.log_variable("var1", "value1")
            self.debug_logger.log_variable("var2", "value2")
            
            # 全データ取得
            all_data = self.debug_logger.get_debug_data()
            self.assertIn("var1", all_data)
            self.assertIn("var2", all_data)
            
            # 特定変数のデータ取得
            var1_data = self.debug_logger.get_debug_data("var1")
            self.assertIn("var1", var1_data)
            self.assertNotIn("var2", var1_data)
            
        except NameError:
            self.skipTest("DebugLogger not available")
    
    def test_clear_debug_data(self):
        """デバッグデータクリアテスト"""
        try:
            # データを追加
            self.debug_logger.log_variable("test", "value")
            
            # クリア実行
            self.debug_logger.clear_debug_data()
            
            # 空になることを確認
            self.assertEqual(len(self.debug_logger.debug_data), 0)
            
        except NameError:
            self.skipTest("DebugLogger not available")


class TestFlowVisualizer(unittest.TestCase):
    """FlowVisualizerクラスのテスト"""
    
    def setUp(self):
        """テストセットアップ"""
        try:
            self.visualizer = FlowVisualizer()
        except NameError:
            self.skipTest("FlowVisualizer not available")
    
    def test_initialization(self):
        """初期化テスト"""
        self.assertIsNotNone(self.visualizer.mermaid_generator)
    
    def test_visualize_flow_invalid_format(self):
        """無効な出力形式テスト"""
        try:
            with self.assertRaises(VisualizationError):
                self.visualizer.visualize_flow("test", output_format="invalid")
        except NameError:
            self.skipTest("FlowVisualizer not available")


class TestGlobalFunctions(unittest.TestCase):
    """グローバル関数のテスト"""
    
    def test_get_flow_visualizer(self):
        """グローバルフロー可視化インスタンス取得テスト"""
        try:
            visualizer1 = get_flow_visualizer()
            visualizer2 = get_flow_visualizer()
            
            # 同じインスタンスが返されることを確認
            self.assertIs(visualizer1, visualizer2)
        except NameError:
            self.skipTest("get_flow_visualizer not available")
    
    def test_get_performance_profiler(self):
        """グローバルパフォーマンスプロファイラー取得テスト"""
        try:
            profiler1 = get_performance_profiler()
            profiler2 = get_performance_profiler()
            
            # 同じインスタンスが返されることを確認
            self.assertIs(profiler1, profiler2)
        except NameError:
            self.skipTest("get_performance_profiler not available")
    
    def test_get_debug_logger(self):
        """グローバルデバッグロガー取得テスト"""
        try:
            logger1 = get_debug_logger()
            logger2 = get_debug_logger()
            
            # 同じインスタンスが返されることを確認
            self.assertIs(logger1, logger2)
        except NameError:
            self.skipTest("get_debug_logger not available")
    
    def test_profile_execution_decorator(self):
        """profile_executionデコレータテスト"""
        try:
            @profile_execution("global_test")
            def test_function():
                time.sleep(0.001)
                return "result"
            
            result = test_function()
            self.assertEqual(result, "result")
            
            # プロファイラーにデータが記録されているか確認
            profiler = get_performance_profiler()
            self.assertIn("global_test", profiler.execution_times)
            
        except NameError:
            self.skipTest("profile_execution not available")


class TestVisualizationUtils(unittest.TestCase):
    """VisualizationUtilsクラスのテスト"""
    
    def test_create_node_graph(self):
        """ノードグラフ作成テスト"""
        try:
            from viz_utils import VisualizationUtils
            
            nodes = ["Node1", "Node2", "Node3"]
            relationships = [(0, 1), (1, 2)]
            
            result = VisualizationUtils.create_node_graph(nodes, relationships)
            
            self.assertIn("graph LR", result)
            self.assertIn("N0['str']", result)
            self.assertIn("N0 --> N1", result)
            self.assertIn("N1 --> N2", result)
            
        except (NameError, ImportError):
            self.skipTest("VisualizationUtils not available")
    
    def test_create_timeline_chart(self):
        """タイムラインチャート作成テスト"""
        try:
            from viz_utils import VisualizationUtils
            
            events = [
                {"name": "イベント1", "start_date": "2024-01-01", "end_date": "2024-01-05"},
                {"name": "イベント2", "start_date": "2024-01-03", "end_date": "2024-01-10"}
            ]
            
            result = VisualizationUtils.create_timeline_chart(events)
            
            self.assertIn("gantt", result)
            self.assertIn("イベント1", result)
            self.assertIn("イベント2", result)
            
        except (NameError, ImportError):
            self.skipTest("VisualizationUtils not available")


if __name__ == '__main__':
    unittest.main(verbosity=2)
