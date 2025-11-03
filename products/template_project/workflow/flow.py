from pocketflow import Flow

from ..core.node.nodes import ChatNode

# Create the flow with self-loop
chat_node = ChatNode()
# ノードの自己ループを設定（"continue"アクションで自分自身に戻る）
chat_node.next(chat_node, "continue")

flow = Flow(start=chat_node)


# flow.py
# from pocketflow import Flow
# from nodes import GetQuestionNode, AnswerNode

# def create_qa_flow():
#     """Create and return a question-answering flow."""
#     # Create nodes
#     get_question_node = GetQuestionNode()
#     answer_node = AnswerNode()

#     # Connect nodes in sequence
#     get_question_node >> answer_node

#     # Create flow starting with input node
#     return Flow(start=get_question_node)
