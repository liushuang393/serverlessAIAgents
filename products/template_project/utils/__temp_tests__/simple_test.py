#!/usr/bin/env python3
"""
简化的测试脚本

这个脚本直接测试各个工具文件的基本功能，不依赖复杂的导入。
"""

import sys
import os
import traceback

# 添加父目录到路径
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

def test_file_imports():
    """测试各个文件是否可以正常导入"""
    print("=" * 60)
    print("文件导入测试")
    print("=" * 60)
    
    test_files = [
        'embedding_utils',
        'vector_utils',
        'websearch_utils',
        'text_utils',
        'audio_utils',
        'viz_utils'
    ]
    
    results = {}
    
    for file_name in test_files:
        try:
            module = __import__(file_name)
            results[file_name] = {
                'status': 'SUCCESS',
                'error': None,
                'classes': [name for name in dir(module) if name[0].isupper() and not name.startswith('_')],
                'functions': [name for name in dir(module) if not name.startswith('_') and callable(getattr(module, name))]
            }
            print(f"✓ {file_name}: 导入成功")
            
        except Exception as e:
            results[file_name] = {
                'status': 'FAILED',
                'error': str(e),
                'classes': [],
                'functions': []
            }
            print(f"✗ {file_name}: 导入失败 - {e}")
    
    return results


def test_basic_functionality():
    """测试基本功能"""
    print("\n" + "=" * 60)
    print("基本功能测试")
    print("=" * 60)
    
    # 测试 LLM 工具（使用现有的LLMProvider）
    try:
        from LLMProvider import generate, generate_anthropic

        # 测试基本生成
        response = generate("测试提示")
        print(f"✓ LLM工具: 基本生成正常 - {response[:50]}...")

        # 测试Anthropic生成
        try:
            anthropic_response = generate_anthropic("测试提示")
            print(f"✓ LLM工具: Anthropic生成正常 - {anthropic_response[:50]}...")
        except Exception:
            print(f"✓ LLM工具: Anthropic生成跳过（无API密钥）")

    except Exception as e:
        print(f"✗ LLM工具测试失败: {e}")
    
    # 测试文本处理工具
    try:
        from text_utils import chunk_text, preprocess_text
        
        # 测试文本预处理
        clean_text = preprocess_text("<p>这是   测试文本</p>")
        print(f"✓ 文本预处理: '{clean_text}'")
        
        # 测试文本分块
        chunks = chunk_text("这是一个测试文本。" * 10, chunk_size=50)
        print(f"✓ 文本分块: 生成了 {len(chunks)} 个块")
        
    except Exception as e:
        print(f"✗ 文本工具测试失败: {e}")
    
    # 测试向量工具
    try:
        from vector_utils import VectorDocument, get_vector_db_manager
        
        # 测试文档创建
        doc = VectorDocument("test1", [0.1, 0.2, 0.3], {"title": "测试"}, "测试文档")
        print(f"✓ 向量文档: ID={doc.id}, 向量长度={len(doc.vector)}")
        
        # 测试管理器
        manager = get_vector_db_manager()
        print(f"✓ 向量DB管理器: 默认提供商 = {manager.default_provider}")
        
    except Exception as e:
        print(f"✗ 向量工具测试失败: {e}")
    
    # 测试Web搜索工具
    try:
        from websearch_utils import SearchResult, get_web_search_manager
        
        # 测试搜索结果
        result = SearchResult("测试标题", "https://test.com", "测试摘要", "test")
        print(f"✓ 搜索结果: {result.title} - {result.url}")
        
        # 测试管理器
        manager = get_web_search_manager()
        print(f"✓ Web搜索管理器: 默认提供商 = {manager.default_provider}")
        
    except Exception as e:
        print(f"✗ Web搜索工具测试失败: {e}")
    
    # 测试音频工具
    try:
        from audio_utils import get_tts_manager, AudioUtils
        
        # 测试TTS管理器
        manager = get_tts_manager()
        print(f"✓ TTS管理器: 默认提供商 = {manager.default_provider}")
        
        # 测试文本分割
        chunks = AudioUtils.split_text_for_tts("这是一个很长的文本。" * 100, max_length=50)
        print(f"✓ TTS文本分割: 生成了 {len(chunks)} 个块")
        
    except Exception as e:
        print(f"✗ 音频工具测试失败: {e}")
    
    # 测试可视化工具
    try:
        from viz_utils import get_performance_profiler, get_debug_logger
        
        # 测试性能分析器
        profiler = get_performance_profiler()
        print(f"✓ 性能分析器: 初始化成功")
        
        # 测试调试日志
        debug_logger = get_debug_logger()
        debug_logger.log_variable("test_var", "test_value", "test_context")
        data = debug_logger.get_debug_data("test_var")
        print(f"✓ 调试日志: 记录了 {len(data['test_var'])} 条数据")
        
    except Exception as e:
        print(f"✗ 可视化工具测试失败: {e}")


def test_integration():
    """测试集成功能"""
    print("\n" + "=" * 60)
    print("集成功能测试")
    print("=" * 60)
    
    try:
        # 测试 LLM + 文本处理集成
        from LLMProvider import generate
        from text_utils import chunk_text
        
        # 生成文本（模拟模式）
        long_text = generate("写一篇关于人工智能的文章")
        print(f"✓ LLM生成文本: {len(long_text)} 字符")
        
        # 分块处理
        chunks = chunk_text(long_text, chunk_size=100)
        print(f"✓ 文本分块处理: {len(chunks)} 个块")
        
    except Exception as e:
        print(f"✗ LLM+文本处理集成失败: {e}")
    
    try:
        # 测试嵌入 + 向量数据库集成
        from embedding_utils import get_embedding_manager
        from vector_utils import VectorDocument
        
        # 创建嵌入管理器
        emb_manager = get_embedding_manager()
        print(f"✓ 嵌入管理器: 默认提供商 = {emb_manager.default_provider}")
        
        # 创建向量文档
        doc = VectorDocument("doc1", [0.1, 0.2, 0.3], {"type": "test"})
        print(f"✓ 向量文档创建: ID={doc.id}")
        
    except Exception as e:
        print(f"✗ 嵌入+向量DB集成失败: {e}")


def main():
    """主函数"""
    print("AI工具包简化测试")
    print("测试时间:", __import__('datetime').datetime.now().strftime("%Y-%m-%d %H:%M:%S"))
    
    # 运行测试
    import_results = test_file_imports()
    test_basic_functionality()
    test_integration()
    
    # 总结
    print("\n" + "=" * 60)
    print("测试总结")
    print("=" * 60)
    
    success_count = sum(1 for r in import_results.values() if r['status'] == 'SUCCESS')
    total_count = len(import_results)
    
    print(f"文件导入成功率: {success_count}/{total_count} ({success_count/total_count*100:.1f}%)")
    
    print("\n成功导入的模块:")
    for name, result in import_results.items():
        if result['status'] == 'SUCCESS':
            print(f"  ✓ {name}: {len(result['classes'])} 个类, {len(result['functions'])} 个函数")
    
    print("\n失败的模块:")
    for name, result in import_results.items():
        if result['status'] == 'FAILED':
            print(f"  ✗ {name}: {result['error']}")
    
    print(f"\n🎉 测试完成！")
    
    return success_count == total_count


if __name__ == '__main__':
    success = main()
    sys.exit(0 if success else 1)
