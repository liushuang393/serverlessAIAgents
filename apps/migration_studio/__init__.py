"""Migration Studio - COBOL → Java Spring Boot 移行自動化アプリ.

Claude CLI (claude-agent-sdk) を主体として、
COBOL プロジェクトを Java Spring Boot REST API へ変換する。

機能:
- 6ステージパイプライン (分析→設計→変換→テスト生成→検証→品質ゲート)
- 自己適応進化ループ (失敗時にプロンプト自動改善)
- SSEリアルタイム進捗ストリーミング
- 成果物の整理されたフォルダ出力 + zipダウンロード
"""
