from products.pocketflow_helloTest.flow import qa_flow


# メイン関数の例
# 独自のメイン関数に置き換えてください
def main():
    """
    メイン実行関数
    質問応答フローを実行し、結果を表示します
    """
    shared = {"question": "一文で、宇宙の終わりとは何ですか？", "answer": None}

    qa_flow.run(shared)
    print("質問:", shared["question"])
    print("回答:", shared["answer"])


if __name__ == "__main__":
    main()
    # python -m products.pocketflow_helloTest.main
