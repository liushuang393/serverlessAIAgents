#!/usr/bin/env python3
"""
导入测试脚本

测试从 utils 包的 __init__.py 导入所有公开的功能
"""

import sys
import os

# 添加父目录到路径
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../..'))

def test_utils_imports():
    """测试从utils包导入所有功能"""
    print("🔍 测试从 utils 包导入功能")
    print("="*50)
    
    try:
        # 测试LLM相关导入
        print("测试LLM相关导入...")
        from utils import generate, generate_anthropic, generate_google, generate_huggingface
        print("✅ LLM功能导入成功")
        
        # 测试嵌入相关导入
        print("测试嵌入相关导入...")
        from utils import embed, get_embedding_manager, EmbeddingError
        print("✅ 嵌入功能导入成功")
        
        # 测试向量数据库相关导入
        print("测试向量数据库相关导入...")
        from utils import create_collection, VectorDocument, get_vector_db_manager, SearchResult
        print("✅ 向量数据库功能导入成功")

        # 测试Web搜索相关导入
        print("测试Web搜索相关导入...")
        from utils import search_web, get_web_search_manager, WebSearchResult
        print("✅ Web搜索功能导入成功")

        # 测试文本处理相关导入
        print("测试文本处理相关导入...")
        from utils import chunk_text, preprocess_text, TextChunk, get_chunker_manager
        print("✅ 文本处理功能导入成功")
        
        # 测试音频处理相关导入
        print("测试音频处理相关导入...")
        from utils import text_to_speech, get_tts_manager, AudioUtils
        print("✅ 音频处理功能导入成功")
        
        # 测试可视化相关导入
        print("测试可视化相关导入...")
        from utils import build_mermaid, profile_execution, get_performance_profiler
        print("✅ 可视化功能导入成功")
        
        print("\n🎉 所有功能都可以从 utils 包正常导入！")
        return True
        
    except ImportError as e:
        print(f"❌ 导入失败: {e}")
        return False
    except Exception as e:
        print(f"❌ 其他错误: {e}")
        return False


def test_functionality_through_utils():
    """通过utils包测试基本功能"""
    print("\n" + "="*50)
    print("🧪 通过 utils 包测试基本功能")
    print("="*50)
    
    try:
        # 导入所需功能
        from utils import (
            generate, chunk_text, VectorDocument,
            get_performance_profiler, AudioUtils
        )
        
        # 测试LLM功能
        print("测试LLM功能...")
        response = generate("测试提示")
        print(f"✅ LLM生成: {response[:50]}...")
        
        # 测试文本处理
        print("测试文本处理...")
        chunks = chunk_text("这是一个测试文本。" * 10, chunk_size=50)
        print(f"✅ 文本分块: {len(chunks)}个块")
        
        # 测试向量文档
        print("测试向量文档...")
        doc = VectorDocument("test", [0.1, 0.2, 0.3], {"test": True}, "测试文档")
        print(f"✅ 向量文档: ID={doc.id}, 维度={len(doc.vector)}")
        
        # 测试性能监控
        print("测试性能监控...")
        profiler = get_performance_profiler()
        
        @profiler.profile_function("utils_test")
        def test_func():
            return "utils_test_result"
        
        result = test_func()
        print(f"✅ 性能监控: {result}")
        
        # 测试音频工具
        print("测试音频工具...")
        chunks = AudioUtils.split_text_for_tts("这是一个很长的测试文本。" * 20, max_length=50)
        print(f"✅ 音频文本分割: {len(chunks)}个块")
        
        print("\n🎉 通过 utils 包的所有功能测试都通过了！")
        return True
        
    except Exception as e:
        print(f"❌ 功能测试失败: {e}")
        return False


def main():
    """主函数"""
    print("📦 Utils 包导入和功能测试")
    print("测试时间:", __import__('time').strftime("%Y-%m-%d %H:%M:%S"))
    
    # 运行测试
    import_success = test_utils_imports()
    function_success = test_functionality_through_utils()
    
    # 总结
    print("\n" + "="*60)
    print("📋 测试总结")
    print("="*60)
    
    print(f"导入测试: {'✅ 通过' if import_success else '❌ 失败'}")
    print(f"功能测试: {'✅ 通过' if function_success else '❌ 失败'}")
    
    overall_success = import_success and function_success
    
    if overall_success:
        print("\n🎉 Utils 包已完全准备就绪！")
        print("✅ 所有功能都可以通过 'from utils import ...' 正常使用")
        print("✅ 所有核心功能都能正常执行")
    else:
        print("\n⚠️  Utils 包存在问题，请检查相关配置")
    
    return overall_success


if __name__ == '__main__':
    success = main()
    sys.exit(0 if success else 1)
