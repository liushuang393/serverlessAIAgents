from .flow import flow


# メイン関数の例
# 独自のメイン関数に置き換えてください
def main():
    """
    メイン実行関数
    質問応答フローを実行し、結果を表示します
    """
    shared = {"question": "一文で、宇宙の終わりとは何ですか？", "answer": None}

    flow.run(shared)


if __name__ == "__main__":
    main()
    # python -m products.template_project.main
