CREATE DATABASE IF NOT EXISTS ai_learning_platform CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
USE ai_learning_platform;

CREATE TABLE users (
    id INT AUTO_INCREMENT PRIMARY KEY,
    username VARCHAR(50) UNIQUE NOT NULL,
    email VARCHAR(100) UNIQUE NOT NULL,
    hashed_password VARCHAR(255) NOT NULL,
    is_active BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
);

CREATE TABLE chapters (
    id INT AUTO_INCREMENT PRIMARY KEY,
    title VARCHAR(255) NOT NULL,
    description TEXT,
    order_index INT UNIQUE, -- チャプター順序
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE skill_categories (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(100) UNIQUE NOT NULL,
    description TEXT
);

CREATE TABLE skill_questions (
    id INT AUTO_INCREMENT PRIMARY KEY,
    category_id INT NOT NULL,
    question_text TEXT NOT NULL,
    options JSON NOT NULL, -- 選択肢、例：{"A": "選択肢1", "B": "選択肢2", ...}
    correct_answer VARCHAR(10) NOT NULL, -- 正解キー、例："B"
    difficulty ENUM('beginner', 'intermediate', 'advanced') DEFAULT 'beginner',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (category_id) REFERENCES skill_categories(id) ON DELETE CASCADE
);

CREATE TABLE user_answers (
    id INT AUTO_INCREMENT PRIMARY KEY,
    user_id INT NOT NULL,
    question_id INT NOT NULL,
    selected_option VARCHAR(10) NOT NULL, -- ユーザーが選択した回答キー、例："B"
    is_correct BOOLEAN,
    answered_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
    FOREIGN KEY (question_id) REFERENCES skill_questions(id) ON DELETE CASCADE
);

CREATE TABLE user_skill_profiles (
    id INT AUTO_INCREMENT PRIMARY KEY,
    user_id INT NOT NULL UNIQUE,
    overall_score DECIMAL(5,2), -- 総合スコア (0-100)
    strengths JSON,             -- 強み一覧、例：["Prompt Engineering", "Text Classification"]
    weaknesses JSON,            -- 改善項目一覧
    recommendations JSON,       -- 学習提案、例：[{"chapter_id": 3, "reason": "強化が必要..."}]
    generated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

CREATE TABLE learning_contents (
    id INT AUTO_INCREMENT PRIMARY KEY,
    chapter_id INT NOT NULL,
    title VARCHAR(255) NOT NULL,
    content_type ENUM('markdown', 'video', 'quiz', 'simulation') NOT NULL,
    content_path VARCHAR(500), -- ファイルパスまたはURL
    order_index INT,           -- チャプター内のコンテンツ順序
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (chapter_id) REFERENCES chapters(id) ON DELETE CASCADE
);

CREATE TABLE user_learning_progress (
    id INT AUTO_INCREMENT PRIMARY KEY,
    user_id INT NOT NULL,
    content_id INT NOT NULL,
    status ENUM('not_started', 'in_progress', 'completed') DEFAULT 'not_started',
    started_at TIMESTAMP NULL,
    completed_at TIMESTAMP NULL,
    last_accessed TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    UNIQUE KEY unique_user_content (user_id, content_id),
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
    FOREIGN KEY (content_id) REFERENCES learning_contents(id) ON DELETE CASCADE
);

CREATE TABLE user_submissions (
    id INT AUTO_INCREMENT PRIMARY KEY,
    user_id INT NOT NULL,
    content_type ENUM('email', 'report', 'presentation') NOT NULL,
    title VARCHAR(255),
    content TEXT NOT NULL, -- ユーザーが提出した原文内容
    submitted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

CREATE TABLE ai_feedbacks (
    id INT AUTO_INCREMENT PRIMARY KEY,
    submission_id INT NOT NULL,
    feedback_text TEXT NOT NULL, -- AI生成のフィードバック内容
    generated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (submission_id) REFERENCES user_submissions(id) ON DELETE CASCADE
);

CREATE TABLE learning_reports (
    id INT AUTO_INCREMENT PRIMARY KEY,
    user_id INT NOT NULL,
    report_type ENUM('individual', 'manager') NOT NULL,
    data JSON, -- レポートデータ、例：進捗、スコアなど
    generated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

INSERT INTO chapters (title, description, order_index) VALUES
('Introduction to Language Models', '言語モデルの基本概念と応用について理解する。', 1),
('Tokens and Embeddings', 'トークン化とベクトル表現の基礎知識を学習する。', 2),
('Looking Inside Transformer LLMs', 'Transformerアーキテクチャと大規模言語モデルの動作原理を深く理解する。', 3),
('Text Classification', 'テキスト分類タスクとその応用を習得する。', 4),
('Text Clustering and Topic Modeling', 'テキストクラスタリングとトピックモデリング技術を学習する。', 5),
('Advanced Text Generation Techniques and Tools', '高度なテキスト生成技術とツールを探索する。', 6),
('Multimodal Large Language Models', 'マルチモーダル大規模言語モデルについて理解する。', 7),
('Semantic Search and Retrieval-Augmented Generation', 'セマンティック検索と検索拡張生成技術を学習する。', 8),
('Multimodal Large Language Models', 'マルチモーダル大規模言語モデルの応用を深く理解する。', 9),
('Creating Text Embedding Models', 'テキスト埋め込みモデルの作成方法を学習する。', 10),
('Fine-tuning Representation Models for Classification', '分類タスクのための表現モデルの微調整方法を習得する。', 11),
('Fine-tuning Generation Models', '生成モデルの微調整方法を学習する。', 12);

INSERT INTO skill_categories (name, description) VALUES
('Prompt Engineering', 'より良いモデル出力を得るためのプロンプトの設計と最適化。'),
('Text Classification', 'テキストを事前定義されたカテゴリに自動分類する。'),
('Text Generation', 'モデルを使用して一貫性のある関連テキストを自動生成する。'),
('Information Retrieval', '大量のデータから関連情報を検索する。'),
('Model Fine-tuning', '特定のタスクに応じて事前訓練済みモデルを調整する。');