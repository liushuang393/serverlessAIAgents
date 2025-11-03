from .workflow.flow import flow


# メイン関数の例
# 独自のメイン関数に置き換えてください
def main():
    """
    メイン実行関数
    質問応答フローを実行し、結果を表示します
    """
    shared = {
        "question": None,  # Will be populated by GetQuestionNode from user input
        "answer": None,  # Will be populated by AnswerNode
    }

    flow.run(shared)
    print(f"Question: {shared['question']}")
    print(f"Answer: {shared['answer']}")


if __name__ == "__main__":
    main()
    # python -m products.template_project.main
