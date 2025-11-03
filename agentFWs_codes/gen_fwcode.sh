#!/bin/bash

# ディレクトリを作成
mkdir -p fwcode

# 文件 1: mcp_rag_server.py
cat > fwcode/mcp_rag_server.py <<'EOF'
import asyncio
import json
import hashlib
from typing import Dict, Any, Optional, List
from pathlib import Path
import logging
from mcp.server.fastmcp import FastMCP
from llama_index.core import VectorStoreIndex, Document, Settings
from llama_index.core.node_parser import SentenceSplitter
from llama_index.embeddings.openai import OpenAIEmbedding
from llama_index.llms.openai import OpenAI
from llama_index.readers.file import PDFReader, CSVReader
# ログ設定
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)
class RAGServer:
    def __init__(self):
        self.app = FastMCP("RAG-Server")
        self.indices: Dict[str, VectorStoreIndex] = {}
        self.document_cache: Dict[str, List[Document]] = {}
        self.config = self._load_config()

        # LlamaIndex設定を初期化
        Settings.llm = OpenAI(model="gpt-4o-mini")
        Settings.embed_model = OpenAIEmbedding(model="text-embedding-3-small")

        self._register_tools()

    def _load_config(self) -> Dict[str, Any]:
        """設定ファイルを読み込み"""
        config_path = Path("doc_config.json")
        if config_path.exists():
            with open(config_path, 'r', encoding='utf-8') as f:
                return json.load(f)
        return {"default_chunk_size": 1024, "default_chunk_overlap": 200}

    def _get_document_hash(self, file_path: str, chunk_size: int, chunk_overlap: int) -> str:
        """ドキュメント+パラメータのハッシュ値を計算、キャッシュ判定に使用"""
        file_stat = Path(file_path).stat()
        content = f"{file_path}_{file_stat.st_size}_{file_stat.st_mtime}_{chunk_size}_{chunk_overlap}"
        return hashlib.md5(content.encode()).hexdigest()

    def _parse_documents(self, file_path: str, chunk_size: int, chunk_overlap: int) -> List[Document]:
        """ドキュメントを解析、スマートキャッシュをサポート"""
        doc_hash = self._get_document_hash(file_path, chunk_size, chunk_overlap)

        # キャッシュをチェック
        if doc_hash in self.document_cache:
            logger.info(f"キャッシュされたドキュメントを使用: {file_path}")
            return self.document_cache[doc_hash]

        # ファイルタイプに応じてパーサーを選択
        file_path_obj = Path(file_path)
        if file_path_obj.suffix.lower() == '.pdf':
            reader = PDFReader()
        elif file_path_obj.suffix.lower() == '.csv':
            reader = CSVReader()
        else:
            raise ValueError(f"サポートされていないファイル形式: {file_path_obj.suffix}")

        # ドキュメントを解析
        documents = reader.load_data(file_path)

        # チャンク処理
        splitter = SentenceSplitter(
            chunk_size=chunk_size,
            chunk_overlap=chunk_overlap
        )

        nodes = splitter.get_nodes_from_documents(documents)
        processed_docs = [Document(text=node.text, metadata=node.metadata) for node in nodes]

        # 結果をキャッシュ
        self.document_cache[doc_hash] = processed_docs
        logger.info(f"ドキュメント解析完了: {file_path}, チャンク数: {len(processed_docs)}")

        return processed_docs

    def _register_tools(self):
        """MCPツールを登録"""

        @self.app.tool()
        def create_vector_index(
            file_path: str,
            index_name: str,
            chunk_size: int = None,
            chunk_overlap: int = None,
            force_recreate: bool = False
        ) -> str:
            """
            ベクトルインデックスを作成

            Args:
                file_path: ドキュメントパス
                index_name: インデックス名
                chunk_size: チャンクサイズ
                chunk_overlap: チャンクオーバーラップ
                force_recreate: 強制再構築
            """
            try:
                # デフォルト設定を使用
                chunk_size = chunk_size or self.config["default_chunk_size"]
                chunk_overlap = chunk_overlap or self.config["default_chunk_overlap"]

                # 再構築が必要かチェック
                if not force_recreate and index_name in self.indices:
                    return f"インデックス {index_name} は既に存在します。force_recreate=True で強制再構築してください"

                # ドキュメントを解析
                documents = self._parse_documents(file_path, chunk_size, chunk_overlap)

                # インデックスを作成
                index = VectorStoreIndex.from_documents(documents)
                self.indices[index_name] = index

                return f"インデックス {index_name} の作成に成功しました。{len(documents)} 個のドキュメントブロックを含みます"

            except Exception as e:
                logger.error(f"インデックス作成失敗: {str(e)}")
                return f"インデックス作成失敗: {str(e)}"

        @self.app.tool()
        def query_document(
            index_name: str,
            query: str,
            top_k: int = 5
        ) -> str:
            """
            ドキュメントを検索

            Args:
                index_name: インデックス名
                query: 検索クエリ
                top_k: 返却結果数
            """
            try:
                if index_name not in self.indices:
                    return f"インデックス {index_name} が存在しません。まずインデックスを作成してください"

                # クエリを実行
                query_engine = self.indices[index_name].as_query_engine(
                    similarity_top_k=top_k
                )
                response = query_engine.query(query)

                return str(response)

            except Exception as e:
                logger.error(f"検索失敗: {str(e)}")
                return f"検索失敗: {str(e)}"

        @self.app.tool()
        def get_document_summary(
            index_name: str,
            summary_type: str = "brief"
        ) -> str:
            """
            ドキュメント要約を取得

            Args:
                index_name: インデックス名
                summary_type: 要約タイプ (brief/detailed)
            """
            try:
                if index_name not in self.indices:
                    return f"インデックス {index_name} が存在しません"

                # 要約を生成
                summary_engine = self.indices[index_name].as_query_engine()

                if summary_type == "brief":
                    query = "このドキュメントの主要な内容を3-5文で要約してください"
                else:
                    query = "このドキュメントの核心的な観点と重要な情報を詳細に要約してください"

                response = summary_engine.query(query)
                return str(response)

            except Exception as e:
                logger.error(f"要約生成失敗: {str(e)}")
                return f"要約生成失敗: {str(e)}"

        @self.app.tool()
        def list_indices() -> str:
            """利用可能なすべてのインデックスをリスト表示"""
            if not self.indices:
                return "現在利用可能なインデックスがありません"

            index_info = []
            for name, index in self.indices.items():
                doc_count = len(index.docstore.docs)
                index_info.append(f"- {name}: {doc_count} 個のドキュメントブロック")

            return "利用可能なインデックス：\n" + "\n".join(index_info)
# サーバー起動スクリプト
async def main():
    server = RAGServer()
    await server.app.run()
if __name__ == "__main__":
    asyncio.run(main())
EOF

# 文件 2: mcp_rag_client.py
cat > fwcode/mcp_rag_client.py <<'EOF'
import asyncio
import json
from typing import Dict, List, Any
from pathlib import Path
from langgraph.graph import Graph, END
from langgraph.prebuilt import ToolExecutor
from langchain.agents import AgentExecutor
from langchain.schema import SystemMessage, HumanMessage
from langchain_openai import ChatOpenAI
class RAGAgent:
    def __init__(self, mcp_server_url: str = "http://localhost:8000"):
        self.mcp_server_url = mcp_server_url
        self.llm = ChatOpenAI(model="gpt-4o-mini", temperature=0)
        self.tools = self._load_mcp_tools()
        self.tool_executor = ToolExecutor(self.tools)
        self.config = self._load_config()

        # ワークフローグラフを構築
        self.graph = self._build_workflow()

    def _load_config(self) -> Dict[str, Any]:
        """MCP設定を読み込み"""
        config_path = Path("mcp_config.json")
        if config_path.exists():
            with open(config_path, 'r', encoding='utf-8') as f:
                return json.load(f)
        return {"available_indices": [], "document_descriptions": {}}

    def _load_mcp_tools(self) -> List[Any]:
        """MCPサーバーからツールを読み込み"""
        # ここは簡略化処理、実際の実装ではMCPプロトコルを通じてツールリストを取得する必要がある
        # モックツールリストを返す
        return []

    def _build_workflow(self) -> Graph:
        """LangGraphワークフローを構築"""
        workflow = Graph()

        # ノードを追加
        workflow.add_node("planner", self.planning_node)
        workflow.add_node("executor", self.execution_node)
        workflow.add_node("reviewer", self.review_node)

        # エッジを追加
        workflow.add_edge("planner", "executor")
        workflow.add_edge("executor", "reviewer")

        # 条件付きエッジ
        workflow.add_conditional_edges(
            "reviewer",
            self.should_continue,
            {
                "continue": "planner",
                "end": END
            }
        )

        # エントリーポイントを設定
        workflow.set_entry_point("planner")

        return workflow.compile()

    def planning_node(self, state: Dict[str, Any]) -> Dict[str, Any]:
        """タスク計画ノード"""
        messages = state.get("messages", [])

        # ドキュメント認識情報を含むシステムプロンプトを構築
        system_prompt = self._build_system_prompt()

        # LLMを呼び出して計画を立てる
        planning_messages = [
            SystemMessage(content=system_prompt),
            HumanMessage(content=f"以下のタスクを分析し、実行計画を立ててください：{messages[-1].content}")
        ]

        response = self.llm.invoke(planning_messages)

        # 計画結果を解析
        plan = self._parse_plan(response.content)

        state["current_plan"] = plan
        state["plan_step"] = 0

        return state

    def execution_node(self, state: Dict[str, Any]) -> Dict[str, Any]:
        """タスク実行ノード"""
        plan = state.get("current_plan", [])
        step = state.get("plan_step", 0)

        if step >= len(plan):
            state["execution_complete"] = True
            return state

        # 現在のステップを実行
        current_step = plan[step]
        result = self._execute_step(current_step)

        # 実行結果を保存
        if "execution_results" not in state:
            state["execution_results"] = []
        state["execution_results"].append(result)

        return state

    def review_node(self, state: Dict[str, Any]) -> Dict[str, Any]:
        """結果レビューノード"""
        results = state.get("execution_results", [])

        # 実行結果を評価
        review_prompt = f"""
        以下の実行結果の品質と完全性を評価してください：

        実行結果：{results}

        さらなる実行や計画の調整が必要かどうかを判断してください。
        """

        response = self.llm.invoke([HumanMessage(content=review_prompt)])

        # レビュー結果を解析
        state["review_result"] = response.content
        state["needs_continue"] = "継続が必要" in response.content

        return state

    def should_continue(self, state: Dict[str, Any]) -> str:
        """実行を継続するかどうかを判断"""
        if state.get("execution_complete", False):
            return "end"

        if state.get("needs_continue", False):
            state["plan_step"] += 1
            return "continue"

        return "end"

    def _build_system_prompt(self) -> str:
        """ドキュメント認識情報を含むシステムプロンプトを構築"""
        available_indices = self.config.get("available_indices", [])
        doc_descriptions = self.config.get("document_descriptions", {})

        prompt = """
        あなたは専門的なドキュメント分析アシスタントで、以下の能力を持っています：
        1. ドキュメント検索と分析
        2. 複数ドキュメントの比較分析
        3. ドキュメント要約生成
        4. インデックス管理
        利用可能なドキュメントインデックス：
        """

        for index_name in available_indices:
            description = doc_descriptions.get(index_name, "説明なし")
            prompt += f"- {index_name}: {description}\n"

        prompt += """

        利用可能なツール：
        - create_vector_index: ドキュメントベクトルインデックスを作成
        - query_document: 特定のドキュメントを検索
        - get_document_summary: ドキュメント要約を取得
        - list_indices: すべてのインデックスをリスト表示

        ユーザーのニーズに応じて、適切なツールとインデックスを賢く選択してタスクを実行してください。
        """

        return prompt

    def _parse_plan(self, plan_text: str) -> List[Dict[str, Any]]:
        """実行計画を解析"""
        # 簡略化実装、実際にはより複雑な解析ロジックが必要
        steps = []
        lines = plan_text.split('\n')

        for line in lines:
            if line.strip().startswith('-') or line.strip().startswith('1.'):
                steps.append({
                    "action": line.strip(),
                    "tool": "query_document",  # 例
                    "parameters": {}
                })

        return steps

    def _execute_step(self, step: Dict[str, Any]) -> str:
        """単一ステップを実行"""
        # ここでMCPツールを呼び出すべき
        # 簡略化実装
        return f"ステップを実行：{step['action']}"

    async def run(self, query: str) -> str:
        """エージェントを実行"""
        initial_state = {
            "messages": [HumanMessage(content=query)],
            "execution_results": [],
            "execution_complete": False
        }

        # ワークフローを実行
        final_state = await self.graph.ainvoke(initial_state)

        # 最終回答を生成
        results = final_state.get("execution_results", [])

        final_answer = f"""
        タスク実行完了！

        実行結果：
        {chr(10).join(results)}

        より詳細な分析が必要でしたら、具体的な要求をお知らせください。
        """

        return final_answer
# クライアント起動スクリプト
async def main():
    agent = RAGAgent()

    # サンプルクエリ
    queries = [
        "北京と上海の税収政策の違いを分析してください",
        "最新のAI発展レポートを要約してください",
        "市場分析に関する新しいインデックスを作成してください"
    ]

    for query in queries:
        print(f"\nクエリ: {query}")
        result = await agent.run(query)
        print(f"結果: {result}")
if __name__ == "__main__":
    asyncio.run(main())
EOF

# 文件 3: mcp_config.json
cat > fwcode/mcp_config.json <<'EOF'
{
  "server_url": "http://localhost:8000",
  "available_indices": [
    "tax-beijing",
    "tax-shanghai",
    "ai-report-2025",
    "market-analysis"
  ],
  "document_descriptions": {
    "tax-beijing": "北京市税収政策ファイル集合",
    "tax-shanghai": "上海市税収政策ファイル集合",
    "ai-report-2025": "2025年人工知能発展レポート",
    "market-analysis": "市場分析関連ドキュメント"
  },
  "tools_permissions": {
    "create_vector_index": true,
    "query_document": true,
    "get_document_summary": true,
    "list_indices": true
  }
}
EOF

# 文件 4: doc_config.json
cat > fwcode/doc_config.json <<'EOF'
{
  "default_chunk_size": 1024,
  "default_chunk_overlap": 200,
  "supported_formats": ["pdf", "csv", "txt", "docx"],
  "embedding_model": "text-embedding-3-small",
  "llm_model": "gpt-4o-mini",
  "max_cache_size": 1000
}
EOF

# 文件 5: Dockerfile
cat > fwcode/Dockerfile <<'EOF'
FROM python:3.11-slim
WORKDIR /app
COPY requirements.txt .
RUN pip install -r requirements.txt
COPY . .
EXPOSE 8000
CMD ["uvicorn", "mcp_rag_server:app", "--host", "0.0.0.0", "--port", "8000"]
EOF

# 文件 6: docker-compose.yml
cat > fwcode/docker-compose.yml <<'EOF'
version: '3.8'
services:
  rag-server:
    build: .
    ports:
      - "8000:8000"
    environment:
      - OPENAI_API_KEY=${OPENAI_API_KEY}
    volumes:
      - ./documents:/app/documents
      - ./config:/app/config

  rag-client:
    build: .
    depends_on:
      - rag-server
    environment:
      - MCP_SERVER_URL=http://rag-server:8000
    command: python mcp_rag_client.py
EOF

# 文件 7: requirements.txt
cat > fwcode/requirements.txt <<'EOF'
fastapi==0.104.1
uvicorn==0.24.0
llama-index==0.9.15
langgraph==0.0.25
langchain==0.1.0
langchain-openai==0.0.5
mcp-server==0.1.0
pymupdf==1.23.8
pandas==2.1.4
numpy==1.24.3
EOF

echo "✅ すべてのコードファイルが ./fwcode ディレクトリに生成されました。"
