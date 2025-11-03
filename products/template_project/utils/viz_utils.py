"""
可視化・デバッグユーティリティ

このモジュールは、Mermaid図生成、コールスタックデバッグ、フロー可視化などの
デバッグ・可視化機能を提供します。
"""

import inspect
from typing import Dict, List, Any, Optional, Set
import logging

logger = logging.getLogger(__name__)


class VisualizationError(Exception):
    """可視化関連のエラー"""
    pass


class MermaidGenerator:
    """Mermaid図生成クラス"""
    
    def __init__(self):
        self.node_counter = 1
        self.node_ids: Dict[Any, str] = {}
        self.visited: Set[Any] = set()
        self.lines: List[str] = []
    
    def generate_flowchart(self, start_node: Any, direction: str = "LR") -> str:
        """フローチャートを生成"""
        self._reset()
        self.lines.append(f"graph {direction}")
        
        try:
            self._walk_node(start_node)
        except Exception as e:
            logger.error(f"フローチャート生成エラー: {e}")
            raise VisualizationError(f"フローチャート生成に失敗しました: {e}")
        
        return "\n".join(self.lines)
    
    def _reset(self):
        """状態をリセット"""
        self.node_counter = 1
        self.node_ids.clear()
        self.visited.clear()
        self.lines = []
    
    def _get_node_id(self, node: Any) -> str:
        """ノードIDを取得または生成"""
        if node not in self.node_ids:
            self.node_ids[node] = f"N{self.node_counter}"
            self.node_counter += 1
        return self.node_ids[node]
    
    def _add_link(self, from_node: Any, to_node: Any):
        """ノード間のリンクを追加"""
        from_id = self._get_node_id(from_node)
        to_id = self._get_node_id(to_node)
        self.lines.append(f"    {from_id} --> {to_id}")
    
    def _walk_node(self, node: Any, parent: Optional[Any] = None):
        """ノードを再帰的に処理"""
        if node in self.visited:
            if parent:
                self._add_link(parent, node)
            return
        
        self.visited.add(node)
        
        # ノードの種類を判定
        node_type = type(node).__name__
        
        # Flowノードの場合
        if hasattr(node, 'start_node') and hasattr(node, 'successors'):
            self._handle_flow_node(node, parent)
        # 通常のノードの場合
        elif hasattr(node, 'successors'):
            self._handle_regular_node(node, parent)
        else:
            # 単純なオブジェクトの場合
            node_id = self._get_node_id(node)
            self.lines.append(f"    {node_id}['{node_type}']")
            if parent:
                self._add_link(parent, node)
    
    def _handle_flow_node(self, node: Any, parent: Any):
        """Flowノードを処理"""
        node_id = self._get_node_id(node)
        node_type = type(node).__name__
        
        # サブグラフとして表示
        self.lines.append(f"\n    subgraph sub_flow_{node_id}[{node_type}]")
        
        # 開始ノードを処理
        if hasattr(node, 'start_node') and node.start_node:
            if parent:
                self._add_link(parent, node.start_node)
            self._walk_node(node.start_node)
            
            # 後続ノードを処理
            if hasattr(node, 'successors'):
                for successor in node.successors.values():
                    self._walk_node(successor, node.start_node)
        
        self.lines.append("    end\n")
    
    def _handle_regular_node(self, node: Any, parent: Any):
        """通常のノードを処理"""
        node_id = self._get_node_id(node)
        node_type = type(node).__name__
        
        self.lines.append(f"    {node_id}['{node_type}']")
        
        if parent:
            self._add_link(parent, node)
        
        # 後続ノードを処理
        if hasattr(node, 'successors'):
            for successor in node.successors.values():
                self._walk_node(successor, node)


class CallStackDebugger:
    """コールスタックデバッガー"""
    
    @staticmethod
    def get_node_call_stack(base_class_name: str = "BaseNode") -> List[str]:
        """ノードのコールスタックを取得"""
        stack = inspect.stack()
        node_names = []
        seen_ids = set()
        
        for frame_info in stack[1:]:  # 現在のフレームをスキップ
            local_vars = frame_info.frame.f_locals
            
            if 'self' in local_vars:
                caller_self = local_vars['self']
                
                # BaseNodeのサブクラスかチェック
                if (hasattr(caller_self, '__class__') and 
                    id(caller_self) not in seen_ids):
                    
                    class_name = type(caller_self).__name__
                    
                    # 基底クラス名が含まれているかチェック
                    if (base_class_name in [cls.__name__ for cls in type(caller_self).__mro__] or
                        base_class_name.lower() in class_name.lower()):
                        
                        seen_ids.add(id(caller_self))
                        node_names.append(class_name)
        
        return node_names
    
    @staticmethod
    def get_full_call_stack() -> List[Dict[str, Any]]:
        """完全なコールスタック情報を取得"""
        stack = inspect.stack()
        call_stack = []
        
        for frame_info in stack[1:]:  # 現在のフレームをスキップ
            frame_data = {
                "filename": frame_info.filename,
                "function": frame_info.function,
                "lineno": frame_info.lineno,
                "code_context": frame_info.code_context[0].strip() if frame_info.code_context else None,
                "local_vars": {}
            }
            
            # ローカル変数の情報を追加（selfがある場合）
            local_vars = frame_info.frame.f_locals
            if 'self' in local_vars:
                frame_data["local_vars"]["self_type"] = type(local_vars['self']).__name__  # type: ignore
            
            call_stack.append(frame_data)
        
        return call_stack
    
    @staticmethod
    def print_call_stack(base_class_name: str = "BaseNode"):
        """コールスタックを出力"""
        stack = CallStackDebugger.get_node_call_stack(base_class_name)
        if stack:
            print(f"Call stack: {stack}")
        else:
            print("No call stack found")


class FlowVisualizer:
    """フロー可視化クラス"""
    
    def __init__(self):
        self.mermaid_generator = MermaidGenerator()
    
    def visualize_flow(
        self,
        flow_object: Any,
        output_format: str = "mermaid",
        direction: str = "LR"
    ) -> str:
        """フローを可視化"""
        if output_format == "mermaid":
            return self.mermaid_generator.generate_flowchart(flow_object, direction)
        else:
            raise VisualizationError(f"サポートされていない出力形式: {output_format}")
    
    def save_visualization(
        self,
        flow_object: Any,
        output_path: str,
        output_format: str = "mermaid",
        direction: str = "LR"
    ):
        """可視化結果をファイルに保存"""
        try:
            visualization = self.visualize_flow(flow_object, output_format, direction)
            
            with open(output_path, 'w', encoding='utf-8') as f:
                f.write(visualization)
            
            logger.info(f"可視化結果を保存しました: {output_path}")
            
        except Exception as e:
            logger.error(f"可視化保存エラー: {e}")
            raise VisualizationError(f"可視化の保存に失敗しました: {e}")


class PerformanceProfiler:
    """パフォーマンスプロファイラー"""
    
    def __init__(self):
        self.execution_times: Dict[str, List[float]] = {}
        self.call_counts: Dict[str, int] = {}
    
    def profile_function(self, func_name: Optional[str] = None):
        """関数実行時間を測定するデコレータ"""
        def decorator(func):
            import functools
            import time
            
            name = func_name or f"{func.__module__}.{func.__name__}"
            
            @functools.wraps(func)
            def wrapper(*args, **kwargs):
                start_time = time.time()
                try:
                    result = func(*args, **kwargs)
                    return result
                finally:
                    end_time = time.time()
                    execution_time = end_time - start_time
                    
                    # 実行時間を記録
                    if name not in self.execution_times:
                        self.execution_times[name] = []
                        self.call_counts[name] = 0
                    
                    self.execution_times[name].append(execution_time)
                    self.call_counts[name] += 1
                    
                    logger.debug(f"関数実行時間: {name} = {execution_time:.4f}秒")
            
            return wrapper
        return decorator
    
    def get_performance_report(self) -> Dict[str, Dict[str, Any]]:
        """パフォーマンスレポートを取得"""
        report = {}
        
        for func_name, times in self.execution_times.items():
            if times:
                report[func_name] = {
                    "call_count": self.call_counts[func_name],
                    "total_time": sum(times),
                    "average_time": sum(times) / len(times),
                    "min_time": min(times),
                    "max_time": max(times)
                }
        
        return report
    
    def print_performance_report(self):
        """パフォーマンスレポートを出力"""
        report = self.get_performance_report()
        
        print("\n=== パフォーマンスレポート ===")
        for func_name, stats in report.items():
            print(f"\n関数: {func_name}")
            print(f"  呼び出し回数: {stats['call_count']}")
            print(f"  総実行時間: {stats['total_time']:.4f}秒")
            print(f"  平均実行時間: {stats['average_time']:.4f}秒")
            print(f"  最小実行時間: {stats['min_time']:.4f}秒")
            print(f"  最大実行時間: {stats['max_time']:.4f}秒")
    
    def reset_stats(self):
        """統計をリセット"""
        self.execution_times.clear()
        self.call_counts.clear()


class DebugLogger:
    """デバッグロガー"""
    
    def __init__(self, logger_name: str = "debug"):
        self.logger = logging.getLogger(logger_name)
        self.debug_data: Dict[str, List[Any]] = {}
    
    def log_variable(self, name: str, value: Any, context: str = ""):
        """変数の値をログに記録"""
        log_message = f"変数 {name}: {value}"
        if context:
            log_message += f" (コンテキスト: {context})"
        
        self.logger.debug(log_message)
        
        # デバッグデータとして保存
        if name not in self.debug_data:
            self.debug_data[name] = []
        self.debug_data[name].append({
            "value": value,
            "context": context,
            "timestamp": __import__('time').time()
        })
    
    def log_function_entry(self, func_name: str, args: tuple, kwargs: dict):
        """関数の開始をログに記録"""
        self.logger.debug(f"関数開始: {func_name}(args={args}, kwargs={kwargs})")
    
    def log_function_exit(self, func_name: str, result: Any):
        """関数の終了をログに記録"""
        self.logger.debug(f"関数終了: {func_name} -> {result}")
    
    def get_debug_data(self, variable_name: Optional[str] = None) -> Dict[str, List[Any]]:
        """デバッグデータを取得"""
        if variable_name:
            return {variable_name: self.debug_data.get(variable_name, [])}
        return self.debug_data.copy()
    
    def clear_debug_data(self):
        """デバッグデータをクリア"""
        self.debug_data.clear()


# グローバルインスタンス
_flow_visualizer: Optional[FlowVisualizer] = None
_performance_profiler: Optional[PerformanceProfiler] = None
_debug_logger: Optional[DebugLogger] = None


def get_flow_visualizer() -> FlowVisualizer:
    """グローバルフロー可視化インスタンスを取得"""
    global _flow_visualizer
    if _flow_visualizer is None:
        _flow_visualizer = FlowVisualizer()
    return _flow_visualizer


def get_performance_profiler() -> PerformanceProfiler:
    """グローバルパフォーマンスプロファイラーを取得"""
    global _performance_profiler
    if _performance_profiler is None:
        _performance_profiler = PerformanceProfiler()
    return _performance_profiler


def get_debug_logger() -> DebugLogger:
    """グローバルデバッグロガーを取得"""
    global _debug_logger
    if _debug_logger is None:
        _debug_logger = DebugLogger()
    return _debug_logger


# 便利関数
def build_mermaid(start_node: Any, direction: str = "LR") -> str:
    """Mermaid図を生成する便利関数"""
    visualizer = get_flow_visualizer()
    return visualizer.visualize_flow(start_node, "mermaid", direction)


def profile_execution(func_name: Optional[str] = None):
    """実行時間プロファイリングデコレータ"""
    profiler = get_performance_profiler()
    return profiler.profile_function(func_name)


def debug_call_stack(base_class_name: str = "BaseNode"):
    """コールスタックをデバッグ出力"""
    CallStackDebugger.print_call_stack(base_class_name)


def log_debug_info(name: str, value: Any, context: str = ""):
    """デバッグ情報をログに記録"""
    debug_logger = get_debug_logger()
    debug_logger.log_variable(name, value, context)


class VisualizationUtils:
    """可視化ユーティリティクラス"""
    
    @staticmethod
    def create_node_graph(nodes: List[Any], relationships: List[tuple]) -> str:
        """ノードと関係からグラフを作成"""
        lines = ["graph LR"]
        
        # ノードを追加
        for i, node in enumerate(nodes):
            node_id = f"N{i}"
            node_name = type(node).__name__ if hasattr(node, '__class__') else str(node)
            lines.append(f"    {node_id}['{node_name}']")
        
        # 関係を追加
        for from_idx, to_idx in relationships:
            if 0 <= from_idx < len(nodes) and 0 <= to_idx < len(nodes):
                lines.append(f"    N{from_idx} --> N{to_idx}")
        
        return "\n".join(lines)
    
    @staticmethod
    def create_timeline_chart(events: List[Dict[str, Any]]) -> str:
        """タイムラインチャートを作成"""
        lines = ["gantt"]
        lines.append("    title タイムライン")
        lines.append("    dateFormat  YYYY-MM-DD")
        lines.append("    section イベント")
        
        for event in events:
            name = event.get("name", "イベント")
            start_date = event.get("start_date", "2024-01-01")
            end_date = event.get("end_date", "2024-01-02")
            lines.append(f"    {name} : {start_date}, {end_date}")
        
        return "\n".join(lines)
    
    @staticmethod
    def export_to_html(
        mermaid_code: str,
        title: str = "フロー図",
        output_path: str = "flow_diagram.html"
    ):
        """MermaidコードをHTMLファイルとしてエクスポート"""
        html_template = f"""
<!DOCTYPE html>
<html>
<head>
    <title>{title}</title>
    <script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>
</head>
<body>
    <h1>{title}</h1>
    <div class="mermaid">
{mermaid_code}
    </div>
    <script>
        mermaid.initialize({{startOnLoad: true}});
    </script>
</body>
</html>
"""
        
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(html_template)
        
        logger.info(f"HTMLファイルを作成しました: {output_path}")


# デバッグ用デコレータ
def debug_trace(func):
    """関数の実行をトレースするデコレータ"""
    import functools
    
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        debug_logger = get_debug_logger()
        func_name = f"{func.__module__}.{func.__name__}"
        
        debug_logger.log_function_entry(func_name, args, kwargs)
        
        try:
            result = func(*args, **kwargs)
            debug_logger.log_function_exit(func_name, result)
            return result
        except Exception as e:
            debug_logger.logger.error(f"関数エラー: {func_name} - {e}")
            raise
    
    return wrapper
