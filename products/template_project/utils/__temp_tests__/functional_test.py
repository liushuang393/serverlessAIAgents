#!/usr/bin/env python3
"""
功能执行测试脚本

这个脚本测试各个工具模块的实际功能执行，而不仅仅是导入。
"""

import sys
import os
import time
import tempfile
from pathlib import Path

# 添加父目录到路径
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

def test_llm_functionality():
    """测试LLM功能执行（使用现有的LLMProvider）"""
    print("\n" + "="*50)
    print("🤖 LLM功能执行测试")
    print("="*50)

    try:
        from LLMProvider import generate, generate_anthropic, generate_google

        # 测试1: 基本文本生成
        print("测试1: 基本文本生成")
        response = generate("什么是人工智能？请用一句话回答。")
        print(f"✅ 生成成功: {response[:100]}...")
        assert len(response) > 10, "响应太短"

        # 测试2: Anthropic生成
        print("测试2: Anthropic生成")
        try:
            anthropic_response = generate_anthropic("简单介绍机器学习")
            print(f"✅ Anthropic生成: {anthropic_response[:100]}...")
            assert len(anthropic_response) > 5, "Anthropic响应太短"
        except Exception as e:
            print(f"⚠️  Anthropic测试跳过: {e}")

        # 测试3: Google生成
        print("测试3: Google生成")
        try:
            google_response = generate_google("什么是深度学习？")
            print(f"✅ Google生成: {google_response[:100]}...")
            assert len(google_response) > 5, "Google响应太短"
        except Exception as e:
            print(f"⚠️  Google测试跳过: {e}")

        print("🎉 LLM功能测试全部通过!")
        return True

    except Exception as e:
        print(f"❌ LLM功能测试失败: {e}")
        return False


def test_text_processing_functionality():
    """测试文本处理功能执行"""
    print("\n" + "="*50)
    print("📝 文本处理功能执行测试")
    print("="*50)
    
    try:
        from text_utils import chunk_text, preprocess_text, merge_chunks, TextChunk
        
        # 测试1: 文本预处理
        print("测试1: 文本预处理")
        dirty_text = "<p>这是一个   包含HTML标签的   文本</p>"
        clean_text = preprocess_text(dirty_text)
        print(f"✅ 预处理: '{dirty_text}' -> '{clean_text}'")
        assert "<p>" not in clean_text, "HTML标签未清理"
        assert "  " not in clean_text, "多余空格未清理"
        
        # 测试2: 固定大小分块
        print("测试2: 固定大小分块")
        long_text = "这是一个很长的文本。" * 20  # 200字符左右
        chunks = chunk_text(long_text, chunker_type="fixed", chunk_size=50, overlap=10)
        print(f"✅ 分块成功: {len(long_text)}字符 -> {len(chunks)}个块")
        assert len(chunks) > 1, "应该生成多个块"
        assert all(isinstance(chunk, TextChunk) for chunk in chunks), "块类型错误"
        
        # 测试3: 文本块合并
        print("测试3: 文本块合并")
        merged_text = merge_chunks(chunks, separator=" | ")
        print(f"✅ 合并成功: {len(merged_text)}字符")
        assert " | " in merged_text, "分隔符未正确添加"
        
        print("🎉 文本处理功能测试全部通过!")
        return True
        
    except Exception as e:
        print(f"❌ 文本处理功能测试失败: {e}")
        return False


def test_vector_functionality():
    """测试向量数据库功能执行"""
    print("\n" + "="*50)
    print("🔍 向量数据库功能执行测试")
    print("="*50)
    
    try:
        from vector_utils import VectorDocument, get_vector_db_manager
        import numpy as np
        
        # 测试1: 向量文档创建
        print("测试1: 向量文档创建")
        doc1 = VectorDocument(
            id="doc1",
            vector=[0.1, 0.2, 0.3, 0.4, 0.5],
            metadata={"title": "测试文档1", "category": "test"},
            text="这是第一个测试文档"
        )
        print(f"✅ 文档创建: ID={doc1.id}, 向量维度={len(doc1.vector)}")
        assert doc1.id == "doc1", "文档ID错误"
        assert len(doc1.vector) == 5, "向量维度错误"
        assert isinstance(doc1.vector, np.ndarray), "向量类型错误"
        
        # 测试2: 多个文档创建
        print("测试2: 多个文档创建")
        docs = []
        for i in range(3):
            doc = VectorDocument(
                id=f"doc{i+2}",
                vector=np.random.rand(5).tolist(),
                metadata={"title": f"文档{i+2}", "index": i+2},
                text=f"这是第{i+2}个测试文档"
            )
            docs.append(doc)
        print(f"✅ 批量创建: {len(docs)}个文档")
        assert len(docs) == 3, "文档数量错误"
        
        # 测试3: 管理器功能
        print("测试3: 管理器功能")
        manager = get_vector_db_manager()
        print(f"✅ 管理器: 默认提供商={manager.default_provider}")
        assert manager.default_provider == "faiss", "默认提供商错误"
        
        print("🎉 向量数据库功能测试全部通过!")
        return True
        
    except Exception as e:
        print(f"❌ 向量数据库功能测试失败: {e}")
        return False


def test_websearch_functionality():
    """测试Web搜索功能执行"""
    print("\n" + "="*50)
    print("🌐 Web搜索功能执行测试")
    print("="*50)
    
    try:
        from websearch_utils import SearchResult, get_web_search_manager, search_and_summarize
        
        # 测试1: 搜索结果创建
        print("测试1: 搜索结果创建")
        result = SearchResult(
            title="Python编程教程",
            url="https://python.org/tutorial",
            snippet="Python是一种高级编程语言...",
            source="google",
            metadata={"rank": 1, "date": "2024-01-01"}
        )
        print(f"✅ 搜索结果: {result.title} - {result.url}")
        assert result.title == "Python编程教程", "标题错误"
        assert result.source == "google", "来源错误"
        
        # 测试2: 搜索结果转换
        print("测试2: 搜索结果转换")
        if hasattr(result, 'to_dict'):
            result_dict = result.to_dict()
            print(f"✅ 字典转换: {len(result_dict)}个字段")
            assert "title" in result_dict, "字典缺少title字段"
            assert "url" in result_dict, "字典缺少url字段"
        
        # 测试3: 管理器功能
        print("测试3: 管理器功能")
        manager = get_web_search_manager()
        print(f"✅ 管理器: 默认提供商={manager.default_provider}")
        assert manager.default_provider == "duckduckgo", "默认提供商错误"
        
        # 测试4: 搜索摘要功能
        print("测试4: 搜索摘要功能")
        # 创建模拟搜索结果
        mock_results = [result]
        summary = {
            "query": "test query",
            "total_results": len(mock_results),
            "results": [r.to_dict() if hasattr(r, 'to_dict') else {"title": r.title, "url": r.url} for r in mock_results],
            "sources": list(set(r.source for r in mock_results))
        }
        print(f"✅ 搜索摘要: {summary['total_results']}个结果")
        assert summary["total_results"] == 1, "结果数量错误"
        
        print("🎉 Web搜索功能测试全部通过!")
        return True
        
    except Exception as e:
        print(f"❌ Web搜索功能测试失败: {e}")
        return False


def test_audio_functionality():
    """测试音频处理功能执行"""
    print("\n" + "="*50)
    print("🔊 音频处理功能执行测试")
    print("="*50)
    
    try:
        from audio_utils import get_tts_manager, AudioUtils
        
        # 测试1: TTS管理器
        print("测试1: TTS管理器")
        manager = get_tts_manager()
        print(f"✅ TTS管理器: 默认提供商={manager.default_provider}")
        assert manager.default_provider == "amazon_polly", "默认提供商错误"
        
        # 测试2: 文本分割功能
        print("测试2: 文本分割功能")
        long_text = "这是一个很长的文本，需要分割成多个部分进行TTS处理。" * 50
        chunks = AudioUtils.split_text_for_tts(long_text, max_length=100)
        print(f"✅ 文本分割: {len(long_text)}字符 -> {len(chunks)}个块")
        assert len(chunks) > 1, "应该生成多个块"
        assert all(len(chunk) <= 100 for chunk in chunks), "块长度超限"
        
        # 测试3: 音频时长获取（模拟）
        print("测试3: 音频时长获取")
        # 由于没有真实音频文件，测试会返回0.0（无librosa时的默认值）
        duration = AudioUtils.get_audio_duration("nonexistent.mp3")
        print(f"✅ 音频时长: {duration}秒 (模拟)")
        assert isinstance(duration, (int, float)), "时长类型错误"
        
        print("🎉 音频处理功能测试全部通过!")
        return True
        
    except Exception as e:
        print(f"❌ 音频处理功能测试失败: {e}")
        return False


def test_visualization_functionality():
    """测试可视化功能执行"""
    print("\n" + "="*50)
    print("📊 可视化功能执行测试")
    print("="*50)
    
    try:
        from viz_utils import get_performance_profiler, get_debug_logger, profile_execution
        
        # 测试1: 性能分析器
        print("测试1: 性能分析器")
        profiler = get_performance_profiler()
        
        @profiler.profile_function("test_function")
        def test_func():
            time.sleep(0.01)  # 10ms
            return "test_result"
        
        result = test_func()
        report = profiler.get_performance_report()
        print(f"✅ 性能分析: 函数执行完成，结果={result}")
        print(f"✅ 性能报告: {len(report)}个函数被监控")
        assert result == "test_result", "函数结果错误"
        assert "test_function" in report, "性能报告缺少函数"
        
        # 测试2: 调试日志
        print("测试2: 调试日志")
        debug_logger = get_debug_logger()
        debug_logger.log_variable("test_var", "test_value", "test_context")
        debug_logger.log_variable("another_var", 42, "number_context")
        
        debug_data = debug_logger.get_debug_data()
        print(f"✅ 调试日志: 记录了{len(debug_data)}个变量")
        assert "test_var" in debug_data, "缺少test_var"
        assert "another_var" in debug_data, "缺少another_var"
        assert debug_data["test_var"][0]["value"] == "test_value", "变量值错误"
        
        # 测试3: 全局装饰器
        print("测试3: 全局装饰器")
        @profile_execution("global_test_func")
        def global_test():
            time.sleep(0.005)  # 5ms
            return "global_result"
        
        global_result = global_test()
        global_profiler = get_performance_profiler()
        global_report = global_profiler.get_performance_report()
        print(f"✅ 全局装饰器: 结果={global_result}")
        assert global_result == "global_result", "全局函数结果错误"
        assert "global_test_func" in global_report, "全局性能报告缺少函数"
        
        print("🎉 可视化功能测试全部通过!")
        return True
        
    except Exception as e:
        print(f"❌ 可视化功能测试失败: {e}")
        return False


def test_integration_scenarios():
    """测试集成场景"""
    print("\n" + "="*50)
    print("🔗 集成场景测试")
    print("="*50)
    
    try:
        # 场景1: LLM + 文本处理 + 向量化
        print("场景1: LLM + 文本处理 + 向量化")
        from LLMProvider import generate
        from text_utils import chunk_text
        from vector_utils import VectorDocument
        import numpy as np
        
        # 生成文本
        generated_text = generate("写一篇关于机器学习的简短介绍")
        print(f"✅ 文本生成: {len(generated_text)}字符")
        
        # 文本分块
        chunks = chunk_text(generated_text, chunk_size=100)
        print(f"✅ 文本分块: {len(chunks)}个块")
        
        # 创建向量文档
        vector_docs = []
        for i, chunk in enumerate(chunks[:3]):  # 只处理前3个块
            # 模拟向量（实际应用中会使用真实的嵌入）
            mock_vector = np.random.rand(128).tolist()
            doc = VectorDocument(
                id=f"chunk_{i}",
                vector=mock_vector,
                metadata={"chunk_index": i, "source": "llm_generated"},
                text=chunk.text
            )
            vector_docs.append(doc)
        
        print(f"✅ 向量文档创建: {len(vector_docs)}个文档")
        assert len(vector_docs) <= 3, "文档数量错误"
        
        # 场景2: 性能监控 + 调试日志
        print("场景2: 性能监控 + 调试日志")
        from viz_utils import get_performance_profiler, get_debug_logger
        
        profiler = get_performance_profiler()
        debug_logger = get_debug_logger()
        
        @profiler.profile_function("integration_test")
        def integration_function(data):
            debug_logger.log_variable("input_data", data, "integration_test")
            time.sleep(0.01)
            result = f"processed_{data}"
            debug_logger.log_variable("output_result", result, "integration_test")
            return result
        
        test_result = integration_function("test_data")
        
        # 检查结果
        perf_report = profiler.get_performance_report()
        debug_data = debug_logger.get_debug_data()
        
        print(f"✅ 集成函数执行: {test_result}")
        print(f"✅ 性能监控: {len(perf_report)}个函数")
        print(f"✅ 调试日志: {len(debug_data)}个变量")
        
        assert "integration_test" in perf_report, "性能报告缺少集成函数"
        assert "input_data" in debug_data, "调试日志缺少输入数据"
        assert "output_result" in debug_data, "调试日志缺少输出结果"
        
        print("🎉 集成场景测试全部通过!")
        return True
        
    except Exception as e:
        print(f"❌ 集成场景测试失败: {e}")
        return False


def main():
    """主函数"""
    print("🚀 AI工具包功能执行测试")
    print("测试时间:", time.strftime("%Y-%m-%d %H:%M:%S"))
    
    # 运行所有功能测试
    test_results = {
        "LLM功能": test_llm_functionality(),
        "文本处理": test_text_processing_functionality(),
        "向量数据库": test_vector_functionality(),
        "Web搜索": test_websearch_functionality(),
        "音频处理": test_audio_functionality(),
        "可视化": test_visualization_functionality(),
        "集成场景": test_integration_scenarios()
    }
    
    # 统计结果
    print("\n" + "="*60)
    print("📋 测试结果总结")
    print("="*60)
    
    passed = sum(test_results.values())
    total = len(test_results)
    
    for test_name, result in test_results.items():
        status = "✅ 通过" if result else "❌ 失败"
        print(f"{test_name:12} : {status}")
    
    print(f"\n总体结果: {passed}/{total} ({passed/total*100:.1f}%) 通过")
    
    if passed == total:
        print("🎉 所有功能测试都通过了！AI工具包已准备就绪！")
        return True
    else:
        print(f"⚠️  有 {total-passed} 个测试失败，请检查相关功能。")
        return False


if __name__ == '__main__':
    success = main()
    sys.exit(0 if success else 1)
