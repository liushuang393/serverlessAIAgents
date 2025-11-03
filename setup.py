"""
AI Blocks - サーバーレスAIエージェント基盤のセットアップスクリプト
"""

import os

from setuptools import find_packages, setup


#
# README.mdの内容を読み込み
def read_readme():
    readme_path = os.path.join(os.path.dirname(__file__), "README.md")
    if os.path.exists(readme_path):
        with open(readme_path, "r", encoding="utf-8") as f:
            return f.read()
    return ""


# requirements.txtから依存関係を読み込み
#冒泡排序法
def read_requirements():
    requirements_path = os.path.join(os.path.dirname(__file__), "requirements.txt")
    if os.path.exists(requirements_path):
        with open(requirements_path, "r", encoding="utf-8") as f:
            lines = f.readlines()
        # コメント行と空行を除外
        requirements = []
        for line in lines:
            line = line.strip()
            if line and not line.startswith("#"):
                requirements.append(line)
        return requirements 
    return []


setup(
    name="ai-blocks",
    version="0.0.1",
    author="AI Blocks Team",
    author_email="contact@aiblocks.dev",
    description="軽量で柔軟なAIエージェント構築ライブラリ",
    long_description=read_readme(),
    long_description_content_type="text/markdown",
    url="https://github.com/ai-blocks/ai-blocks",
    packages=find_packages(),
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Programming Language :: Python :: 3.12",
        "Topic :: Scientific/Engineering :: Artificial Intelligence",
        "Topic :: Software Development :: Libraries :: Python Modules",
    ],
    python_requires=">=3.8",
    install_requires=read_requirements(),
    extras_require={
        "dev": [
            "pytest>=7.0.0",
            "pytest-asyncio>=0.21.0",
            "black>=23.0.0",
            "isort>=5.12.0",
            "mypy>=1.0.0",
            "pre-commit>=3.0.0",
        ],
        "docs": [
            "sphinx>=5.0.0",
            "sphinx-rtd-theme>=1.0.0",
            "myst-parser>=0.18.0",
        ],
        "all": [
            "pinecone-client>=2.0.0",
            "weaviate-client>=3.0.0",
            "redis>=4.0.0",
        ],
    },
    entry_points={
        "console_scripts": [
            "ai-blocks=ai_blocks.cli:main",
        ],
    },
    include_package_data=True,
    zip_safe=False,
)
