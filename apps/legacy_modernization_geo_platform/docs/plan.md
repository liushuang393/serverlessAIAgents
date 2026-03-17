外部の最新公開情報、さらにあなたの社内 AgentFlow 実装の現状を前提に再設計した、**旧システム刷新向け GEO / AI 検索獲得プラットフォームの完成版**です。

今回は、単なる「Agent 一覧」ではなく、最初から最後まで **市場前提 → 全体構造 → Agent 詳細 → Agent 間通信 → Agent と画面の相互作用
 → イベント設計 → 成果物 schema → 実行コマンド → System Prompt / Agent Prompt** まで一気通しで落とします。
設計思想は明確で、**Push 型マーケティングではなく、AI 検索上の“需要捕捉インフラ”を作る** です。
これは、Gushwork が 2026 年 2 月に公表した「AI 検索時代に勝つ」方針、7 つの専門 Agent、AI 向け CMS、構造化データ、継続更新、リード追跡という構造と整合しています。

---

# 1. この企画の前提

ご提示の補足情報が示している本質は正しいです。
AI 検索時代の企業マーケティングは、「広告で押す」よりも「見込み客が AI に聞いた瞬間に、候補として出現する」ことが重要になります。Gushwork 自身も、買い手は Google ではなく ChatGPT / Claude / Gemini / Perplexity などで、ユースケース・比較・仕様・業界条件まで含んだ非常に具体的な質問を投げていると述べています。さらに、同社はそのために 7 つの専門 Agent、AI Feed、構造化データ、継続更新、リード計測を組み合わせています。
Google の公式文書も、AI Overviews や AI Mode に出るための特別な裏技はないと明言しており、基本は通常の SEO と同じく、**インデックス可能で、スニペット表示可能で、helpful / reliable / people-first な内容** であることだと説明しています。加えて、AI Features ではページが Google Search に出せる技術要件を満たしていること、重要情報がテキストで読めること、構造化データと可視テキストが一致していることが重要だとしています。 ([developers.google.com][2])

Microsoft 側も 2026 年 2 月に Bing Webmaster Tools の **AI Performance** を公開し、AI 回答で自社ページがどれだけ引用されたか、どの URL が引用されたか、どの grounding query が使われたかを見えるようにしました。つまり、GEO は抽象概念ではなく、すでに **引用・露出・被参照の可視化対象** になり始めています。 ([blogs.bing.com][3])

さらに、Princeton の KDD 2024 論文は GEO を正式に定義し、生成型検索エンジンにおける可視性向上を扱う新しい枠組みとして提示しています。 ([collaborate.princeton.edu][4])

---

# 2. あなたの会社向けに置き換えた時の勝ち筋

あなたの会社の商材は「旧システム翻新」です。
つまり、AI 検索で取りに行くべき需要は、一般的なマーケ需要ではなく、以下です。

* COBOL → Java にしたい
* RPG / PL/I / AS/400 をどう現代化するか悩んでいる
* Struts / 旧 Java フレームワークを Spring Boot 化したい
* 2025 の崖、保守要員不足、運用属人化、調査困難、改修コスト肥大化に悩んでいる
* 「一括刷新」ではなく「可視化 → 影響分析 → 段階移行」が必要
* 技術責任者・情報システム部・経営層で見たい情報が違う

ここで勝つには、「会社紹介サイト」では足りません。
必要なのは **AI が企業の能力を理解しやすい“受け皿”** です。Gushwork が AI Feed を “your website’s protocol for the AI era” と説明しているのと同じで、御社にも **Legacy Modernization Feed** が必要です。AI Feed の考え方として、AI エージェントは人間のようにホームページを眺めるのではなく、構造化された個別回答ページ、機械可読データ、継続更新を必要とする、と Gushwork は説明しています。 ([Gushwork][5])

そして、御社の内部基盤はこの方向にかなり向いています。
社内の AgentFlow 実装では、すでに **3 つのパッケージ入口（Assessment / Modernization / Agent Platform）**、**Skill-first capability contract**、**Business Semantics 工程**、**HITL**、**Governance**、**Kill Switch**、**AG-UI / A2A** の発想が入っています。さらにフロントは `/api/migration/execute`、`/api/ws/{task_id}`、承認イベント、コマンド結果イベント、artifact URL 抽出まで実装済みです。つまり、マーケティング用 GEO Agent 群は、新しい思想をゼロから起こす必要はなく、**既存 AgentFlow を “移行工場” から “需要捕捉工場” に写像すればよい** 状態です。   

---

# 3. 最終プロダクト定義

## 名称

**Legacy Modernization GEO Platform**

## 目的

AI 検索時代において、
**「旧システム刷新を検討している企業が AI に質問したとき、御社が回答候補として引用・推薦・比較対象に入る」**
状態を継続的に作り、そこから **診断 → MQL → SQL → 商談 → 提案** までつなぐこと。

## 価値

このプラットフォームは以下の 4 つを同時に実現します。

1. **需要発見**
   どの企業・業界・技術テーマに刷新需要があるかを抽出する。

2. **AI 検索最適化された資産生成**
   単なるブログではなく、AI が引用しやすい構造化コンテンツ群を継続生成する。

3. **人間営業に渡せるリード化**
   AI 検索露出を、問い合わせ・診断・営業ブリーフ・提案骨子へつなぐ。

4. **継続学習**
   AI 検索で何が引用されたか、どの質問で商談が起きたかを学習し、次の生成へ戻す。

---

# 4. 全体アーキテクチャ

御社内部の 3 パッケージ構造を、そのまま営業・マーケティング用に変換します。
社内では `assess()`、`modernize()`、`platform_mode()` の 3 入口があり、Assessment は診断、Modernization は 7 工程、Platform は持続運用です。これを集客に置き換えると、最も自然な形は次です。 

## Package A: Demand Assessment

ターゲット企業・市場・技術需要を診断する。
営業リストの優先順位付けと、何を訴求すべきかの仮説を作る。

## Package B: GEO Campaign Execution

AI 検索向けコンテンツ群、FAQ、比較ページ、業界別 LP、診断導線を生成・公開する。

## Package C: Revenue Agent Platform

公開後の引用監視、被参照分析、リード判定、営業準備、継続更新を行う。

---

# 5. 画面構成

ここはかなり重要です。
あなたが明示した「agent 間交互」「agent と画面交互」を実装可能な粒度で定義します。

## 5.1 Campaign Console

全体ダッシュボード。
役割は “開始点” ではなく “オーケストレーション監視点” です。

表示:

* 実行中キャンペーン
* 対象市場 / 対象アカウント
* 今どの Agent が動いているか
* 保留中承認
* 被引用トレンド
* MQL/SQL 進捗
* 異常警告

受け取るイベント:

* `flow.start`
* `node.start`
* `node.complete`
* `approval_required`
* `approval_submitted`
* `command_result`
* `flow.complete`
* `flow.error`

御社の既存フロントが node 単位の進捗ステップ表示と WebSocket / approval / command_result を扱っているので、この実装様式をそのまま流用できます。 

## 5.2 Account Workspace

1 社単位で深掘りする画面。
企業シグナル、技術仮説、想定課題、生成済み質問群、生成済みページ候補を管理する。

画面入力:

* 会社名
* 業界
* 地域
* 旧技術候補
* 公開 URL / IR / 採用情報
* 営業メモ

画面出力:

* modernization_fit_score
* urgency_hypothesis
* legacy_stack_hypothesis
* persona_map
* objection_map

## 5.3 Question Map Board

AI 検索で買い手が投げる質問の一覧とクラスタを可視化する画面。
役割別、ファネル別、技術別、業界別でマッピングする。

表示:

* 経営層の質問
* 情シスの質問
* 技術責任者の質問
* 現場運用の質問
* 比較検討フェーズ
* 稟議フェーズ
* ベンダー選定フェーズ

## 5.4 Evidence Board

根拠収集結果を、主張単位で可視化する画面。
Agent が集めた根拠のうち、人間が “使う/捨てる/保留” を決める。

表示:

* claim
* source
* trust_level
* applicability
* quote summary
* competitor overlap
* internal note

## 5.5 Content Studio

生成された LP / FAQ / 比較ページ / 業界ページ / 診断ページを編集・承認する画面。
ここで AI の出力をそのまま出すのではなく、**構造・根拠・CTA** が揃っているかを確認する。

表示:

* ページ目的
* 対応質問
* ペルソナ
* evidence map
* GEO QA 結果
* CTA
* 公開ステータス

## 5.6 Approval Center

HITL 承認専用画面。
社内実装で既に「承認必要」「承認提出」「タイムアウト」がイベント化されているので、その考え方を流用します。 

承認対象:

* 重要ページ公開
* 比較記事公開
* 直接営業メッセージ送信
* 提案骨子生成
* ブランド表現が強いページ
* 数字を含む記述

## 5.7 Lead Desk

問い合わせ・診断・ページ閲覧・企業属性から MQL/SQL を判定する営業画面。

表示:

* lead_stage
* score
* reason_codes
* recommended_offer
* suggested_next_action
* proposal brief

## 5.8 Report Center

キャンペーンレポート、AI 引用レポート、業界別成果、営業接続率を出す画面。
御社の既存基盤は report artifact を URL で配信する流れがあるため、ここも artifact 中心で組みます。 

---

# 6. Agent 一覧と役割

ここでは **12 Agent + 1 Governance Layer** に固定します。
Gushwork の 7 Agent 構成を参考にしつつ、旧システム刷新という B2B 高単価・複雑商材向けに再設計します。Gushwork は Memory / Research / Strategy / Content & Design / Publishing / Backlinking / Auto-Update の 7 Agent を中核にしています。 ([Gushwork][1])

## 6.1 Supervisor Agent

全体の工程制御を行う。
本文生成はしない。
次にどの Agent を呼ぶか、承認が必要か、再試行か停止かだけを決定する。

入力:

* campaign_spec
* current_artifacts
* pending_approval_state
* quality_flags

出力:

* next_agent
* reason
* required_inputs
* approval_required
* stop_or_continue

## 6.2 Brand Memory Agent

御社の能力、対応技術、実績、業界知識、用語、訴求禁止表現、差別化軸を 1 つの記憶ソースにまとめる。
これは Gushwork の Memory Agent に相当します。 ([Gushwork][1])

入力:

* company_profile
* services_catalog
* case_studies
* terminology_rules
* forbidden_claims

出力:

* brand_memory_artifact.json

## 6.3 Demand Signal Agent

対象企業・市場から「刷新需要が起きているシグナル」を抽出する。

抽出対象:

* 保守要員募集
* DX / 基幹刷新発信
* 障害情報
* EOS/EOL
* 採用条件
* 技術負債の兆候
* 旧技術キーワード

出力:

* account_signal_artifact.json

## 6.4 ICP Scoring Agent

企業ごとの優先度を算出する。

評価軸:

* 技術負債強度
* urgency
* 予算可能性
* 案件化可能性
* 商材一致度
* 競合混雑度

出力:

* account_score_artifact.json

## 6.5 Buyer Question Map Agent

見込み客が AI に投げる質問を設計する。
御社向けでは最重要 Agent の一つです。

分類:

* 経営
* 情シス
* アーキテクト
* 開発責任者
* 現場運用

フェーズ:

* 課題認識
* 比較
* 稟議
* PoC
* ベンダー選定

出力:

* question_graph_artifact.json

## 6.6 Legacy Semantics Agent

ここが御社固有の強みです。
社内実装でも Business Semantics Agent が、legacy analysis から business_processes / business_events / state_model / business_rules を抽出する責務を持っています。これをマーケティングでも使います。つまり、単に「COBOL を Java に変換します」ではなく、**“どの業務イベント・状態遷移・業務ルールを保持して移行できるか” を説明できる** ようにする Agent です。  

出力:

* legacy_semantics_artifact.json

## 6.7 Evidence Collector Agent

主張ごとに根拠を集める。

優先順:

1. 官公庁
2. 研究
3. 公式ベンダー
4. 顧客事例
5. 自社一次情報

出力:

* evidence_matrix.json

## 6.8 Strategy Agent

質問をページ構造へ変換する。
Gushwork の Strategy Agent に相当します。 ([Gushwork][1])

出力例:

* `financial-cobol-modernization-lp`
* `rpg-to-java-comparison-page`
* `legacy-visibility-diagnostic-page`
* `strangler-pattern-faq`
* `executive-brief-why-now-page`

## 6.9 Content Composition Agent

本文生成。
ただし自由作文は禁止。
固定構造で生成する。

固定構造:

1. 結論
2. 典型課題
3. 適用条件
4. 比較
5. リスク
6. 御社の支援範囲
7. CTA

## 6.10 GEO QA Agent

AI 検索用品質検査。

検査項目:

* 質問とタイトルの対応
* 冒頭結論
* entity 明確性
* AI が抽出しやすい構造
* 根拠漏れ
* 誇張表現
* 重複ページ
* CTA の自然さ
* 公開可否

## 6.11 Publishing Agent

公開担当。
Gushwork の Publishing Agent と同様に、生成物を AI 向け資産群へ配備する役割です。 ([Gushwork][1])

公開先:

* /feeds/
* 業界別 LP
* FAQ hub
* use-case hub
* diagnostic LP
* comparison hub

## 6.12 Refresh / Intent Drift Agent

AI モデルや検索傾向の変化に合わせてページを更新する。
これは Gushwork の Auto-Update Agent の考え方を取り込んだものです。 ([Gushwork][1])

監視対象:

* grounding query の変化
* AI 引用減少
* 競合出現
* CMS 古さ
* モデルの引用傾向変化

## 6.13 Lead Qualification Agent

閲覧・診断・問い合わせをもとに MQL/SQL 判定する。

出力:

* lead_score_artifact.json

## 6.14 Proposal Prep Agent

営業前 1 枚ブリーフを作る。

内容:

* 顧客現状仮説
* 技術仮説
* 刺さる訴求
* 地雷訴求
* 初回質問
* 推奨入口
* 次アクション

## 6.15 Governance Layer

独立 Agent というより横断層です。

責務:

* 承認管理
* リスク判定
* 監査ログ
* kill switch
* publish guard
* outreach guard

社内実装でも GovernanceEngine、監査、HITL、Kill Switch が重視されています。 

---

# 7. Agent 間の相互作用設計

ここは artifact 駆動で固定します。
御社の既存設計でも artifacts 配下に JSON を保持し、工程間通信を成果物中心にしているため、この方針を維持します。さらに business_semantics / human_feedback / report など専用ステージもすでにあります。 

## 7.1 基本原則

* Agent 同士は会話しない
* 受け渡しは artifact のみ
* 各 artifact は `meta / evidence / unknowns / extensions` を持つ
* Supervisor だけがルーティング権を持つ
* UI は artifact を直接編集せず command で変更要求する
* 人間操作も HumanFeedbackArtifact として保存する

## 7.2 主な artifact

* `brand_memory_artifact.json`
* `account_signal_artifact.json`
* `account_score_artifact.json`
* `question_graph_artifact.json`
* `legacy_semantics_artifact.json`
* `evidence_matrix.json`
* `content_blueprint_artifact.json`
* `content_draft_artifact.json`
* `geo_qa_report.json`
* `publish_manifest.json`
* `lead_score_artifact.json`
* `proposal_brief_artifact.json`
* `human_feedback_artifact.json`
* `campaign_report.md`

## 7.3 代表的な接続

Brand Memory → Demand Signal
Demand Signal → ICP Scoring
ICP Scoring → Buyer Question Map
Buyer Question Map + Legacy Semantics + Evidence → Strategy
Strategy → Content Composition
Content Composition → GEO QA
GEO QA → Publishing
Publishing → Lead Qualification
Lead Qualification → Proposal Prep
Refresh Agent は Publishing 後の全資産を継続監視

---

# 8. Agent と画面の相互作用

## 8.1 画面は Agent に直接命令しない

画面は command を Orchestrator に送る。
Orchestrator が権限・状態・承認条件を見て Agent に割り振る。
これは既存の `approval_required` / `command_result` / `approval_submitted` のイベント設計と相性がよいです。 

## 8.2 画面から送る command 種別

* `campaign.start`
* `account.enrich`
* `question_map.regenerate`
* `evidence.approve`
* `content.rewrite`
* `content.publish`
* `approval.submit`
* `lead.reclassify`
* `proposal.generate`
* `campaign.pause`
* `campaign.kill`

## 8.3 画面は 3 種類の情報だけを扱う

1. 状態
2. 承認
3. 人間追加入力

## 8.4 人間追加入力は HumanFeedbackArtifact 化

社内設計でも human_feedback 用 artifact があり、変換反復や人間コマンドを保持できる構造があります。これをそのまま流用して、マーケティングでも

* NG 表現
* 追加訴求
* 競合差別化メモ
* 顧客事情
  を保存します。 

---

# 9. 実行フロー

## 9.1 Demand Assessment Flow

1. Campaign Console で市場を指定
2. `campaign.start`
3. Supervisor 起動
4. Brand Memory Agent
5. Demand Signal Agent
6. ICP Scoring Agent
7. Buyer Question Map Agent
8. Evidence Collector Agent
9. Assessment Report 生成
10. Approval Center へ送付

## 9.2 GEO Campaign Execution Flow

1. 承認済みアカウント/テーマを選択
2. Strategy Agent でページ群設計
3. Legacy Semantics Agent で業務意味を抽出
4. Content Composition Agent で原稿生成
5. GEO QA Agent で品質検査
6. Approval Center
7. Publishing Agent
8. Report Center に配信結果保存

## 9.3 Revenue Flow

1. Leads Dashboard / Lead Desk で反応確認
2. Lead Qualification Agent
3. SQL 判定なら Proposal Prep Agent
4. 初回商談ブリーフ生成
5. 営業へ handoff

## 9.4 Refresh Flow

1. Bing AI Performance / Search / Analytics / CRM シグナル監視
2. 引用減少または intent drift 検知
3. Refresh Agent
4. 必要なら Strategy → Content → QA へ再投入

---

# 10. イベント設計

御社の既存実装では WebSocket / SSE 的なイベント進行がすでにあり、AG-UI 連携の思想も入っています。フレームワーク設計資料でも、AG-UI Event Emitter が flow events を SSE に変換し、`RUN_STARTED / TOOL_CALL_START / TOOL_CALL_END / STATE_DELTA / RUN_FINISHED / ERROR` を扱う前提になっています。 

これを営業 GEO 用に置き換えます。

## 10.1 必須イベント

* `flow.start`
* `node.start`
* `node.complete`
* `artifact.created`
* `artifact.updated`
* `approval_required`
* `approval_submitted`
* `approval_timeout`
* `command_result`
* `lead_detected`
* `citation_delta`
* `publish_complete`
* `flow.complete`
* `flow.error`

## 10.2 イベント payload

```json
{
  "event_type": "artifact.created",
  "timestamp": "2026-03-10T10:00:00Z",
  "task_id": "geo-20260310-001",
  "agent": "buyer-question-map-agent",
  "artifact_type": "question_graph_artifact",
  "artifact_path": "/artifacts/question_graph/task001.json",
  "summary": "金融業向けの高意図質問を42件生成"
}
```

## 10.3 approval_required payload

```json
{
  "event_type": "approval_required",
  "task_id": "geo-20260310-001",
  "stage": "publish_review",
  "object_type": "content_page",
  "object_id": "financial-cobol-modernization-lp",
  "risk_level": "HIGH",
  "reason": "比較表現と移行効果の数値表現を含むため",
  "actions": ["approve", "reject", "rewrite"]
}
```

## 10.4 command_result payload

```json
{
  "event_type": "command_result",
  "task_id": "geo-20260310-001",
  "command": "content.publish",
  "status": "accepted",
  "applied": true,
  "message": "publish_manifest を生成し公開キューへ投入"
}
```

---

# 11. API 設計

既存の `/api/migration/execute`、`/api/ws/{task_id}`、artifact URL 抽出、承認ルートの考え方を踏襲して、マーケティング専用に切ります。既存フロントは `/api/migration/execute` に POST し、WebSocket を開き、承認イベント・コマンド結果・artifact report URL を扱っています。 

## API

* `POST /api/geo/execute`
* `GET /api/ws/{task_id}`
* `POST /api/geo/{task_id}/commands`
* `GET /api/geo/{task_id}/state`
* `GET /api/geo/{task_id}/artifacts/{stage}/{filename}`
* `POST /api/geo/{task_id}/approval`
* `POST /api/geo/{task_id}/checkpoint/apply`
* `GET /api/geo/{task_id}/report`

## POST /api/geo/execute body

```json
{
  "campaign_name": "legacy-modernization-japan-b2b",
  "package": "assessment",
  "targets": {
    "industries": ["manufacturing", "finance", "distribution"],
    "legacy_stacks": ["COBOL", "RPG", "PL/I", "Struts", "Old Java"],
    "regions": ["Japan"]
  },
  "options": {
    "skill_mode": "skill_first",
    "human_policy": "risk_based",
    "acceptance_threshold": 85.0,
    "max_auto_iterations": 3
  }
}
```

---

# 12. 成果物 schema

## 12.1 account_signal_artifact

```json
{
  "meta": {
    "task_id": "",
    "trace_id": "",
    "stage": "demand_signal"
  },
  "company": "",
  "signals": [
    {
      "type": "job_posting|tech_stack|dx_program|security_refresh|eol",
      "description": "",
      "source": "",
      "confidence": 0.0
    }
  ],
  "urgency_hypothesis": "",
  "modernization_fit_score": 0,
  "unknowns": [],
  "extensions": {}
}
```

## 12.2 question_graph_artifact

```json
{
  "meta": {
    "task_id": "",
    "trace_id": "",
    "stage": "question_map"
  },
  "personas": [
    {
      "role": "cio",
      "questions": [],
      "high_intent_questions": []
    }
  ],
  "funnel_clusters": [
    {
      "stage": "comparison",
      "questions": [],
      "recommended_page_type": "comparison_page"
    }
  ],
  "content_clusters": [],
  "unknowns": [],
  "extensions": {}
}
```

## 12.3 legacy_semantics_artifact

これは社内の BusinessSemanticsArtifact の思想をそのまま踏襲します。社内では `business_processes / business_events / state_model / business_rules` が中核です。 

```json
{
  "meta": {
    "task_id": "",
    "trace_id": "",
    "stage": "legacy_semantics"
  },
  "business_processes": [],
  "business_events": [],
  "state_model": {},
  "business_rules": [],
  "unknowns": [],
  "extensions": {}
}
```

## 12.4 geo_qa_report

```json
{
  "meta": {
    "task_id": "",
    "trace_id": "",
    "stage": "geo_qa"
  },
  "pass_or_fail": "PASS",
  "issues": [],
  "fix_instructions": [],
  "publish_ready": true,
  "risk_level": "LOW|MEDIUM|HIGH",
  "unknowns": []
}
```

## 12.5 lead_score_artifact

```json
{
  "meta": {
    "task_id": "",
    "trace_id": "",
    "stage": "lead_scoring"
  },
  "lead_stage": "MQL|SQL|NURTURE|DISQUALIFY",
  "score": 0,
  "reason_codes": [],
  "recommended_offer": "",
  "sales_brief_needed": true,
  "unknowns": []
}
```

---

# 13. 共通 System Prompt

```text
あなたは Legacy Modernization GEO Platform に属する B2B Growth Agent である。
目的は、旧システム刷新を検討している見込み顧客の需要を捕捉し、
AI検索・通常検索・営業活動で一貫して利用できる高信頼の情報資産を生成し、
商談化率を高めることである。

必須原則:
- 正確性を優先し、不明点は unknowns に残す
- 主張には根拠候補を紐づける
- 断定できない内容は仮説として扱う
- 出力は artifact schema に従う
- Agent間通信は自然言語会話ではなく artifact で行う
- 人間承認が必要な工程では停止する
- 誇張表現、比較誤認、出典不明の数値を出さない
- 旧システム刷新では「技術変換」だけでなく「業務意味の保持」を重視する
- コンテンツは AI に引用されやすい構造と、人間の意思決定に耐える具体性を両立する
```

---

# 14. Agent 個別 Prompt

## 14.1 Supervisor Agent

```text
あなたは Supervisor Agent である。
役割は工程遷移の決定のみであり、本文生成は行わない。

判断基準:
- 根拠不足なら Evidence Collector へ戻す
- 質問粒度が弱いなら Buyer Question Map へ戻す
- 業務意味が弱いなら Legacy Semantics へ戻す
- GEO QA が FAIL なら公開禁止
- SQL 以上のみ Proposal Prep を実行
- HIGH リスクは approval_required を発行する

出力:
- next_agent
- reason
- required_inputs
- approval_required
- stop_or_continue
```

## 14.2 Brand Memory Agent

```text
あなたは Brand Memory Agent である。
会社の能力、対象技術、差別化軸、禁止表現、実績を一貫したブランド記憶へ統合する。

必須項目:
- 支援可能な旧技術
- 移行方式
- 強み
- 業界知識
- 禁止表現
- CTA 方針

出力は brand_memory_artifact.json に従う。
```

## 14.3 Demand Signal Agent

```text
あなたは Demand Signal Agent である。
対象企業に旧システム刷新需要が存在する兆候を抽出する。

重点観点:
- COBOL / RPG / PL/I / AS/400 / VB / 旧Java / Struts
- 保守要員不足
- DX / クラウド / 基幹刷新
- 障害・遅延・EOL/EOS
- 規制・セキュリティ対応
- 求人要件と運用負荷

出力:
- company
- signals
- urgency_hypothesis
- modernization_fit_score
- unknowns
```

## 14.4 ICP Scoring Agent

```text
あなたは ICP Scoring Agent である。
対象企業の案件化可能性と優先順位を数値化する。

評価軸:
- 技術負債強度
- 緊急度
- 商材一致度
- 組織複雑度
- 予算可能性
- 商談化可能性

出力:
- total_score
- dimension_scores
- tier
- reasoning
```

## 14.5 Buyer Question Map Agent

```text
あなたは Buyer Question Map Agent である。
対象顧客が AI 検索や通常検索で投げる質問を体系化する。

必須分類:
- 経営層
- 情シス責任者
- 技術責任者
- 現場運用担当

必須フェーズ:
- 課題認識
- 比較検討
- 稟議準備
- PoC
- ベンダー選定

各質問には:
- intent
- business_value
- urgency
- recommended_page_type
を付ける。
```

## 14.6 Legacy Semantics Agent

```text
あなたは Legacy Semantics Agent である。
旧システム刷新の訴求に必要な「業務意味」を整理する。

目的:
- 技術話だけでなく業務イベント・状態遷移・業務ルールを抽出する
- 「何を守って移行するか」を明確にする
- ビジネス観点からの説明可能性を高める

出力:
- business_processes
- business_events
- state_model
- business_rules
- unknowns
```

## 14.7 Evidence Collector Agent

```text
あなたは Evidence Collector Agent である。
主張を支える根拠を収集・整理する。

優先ソース:
1. 官公庁
2. 学術研究
3. 公式ベンダー
4. 顧客事例
5. 自社一次情報

各 claim ごとに:
- source
- confidence
- applicability
- notes
を付ける。
```

## 14.8 Strategy Agent

```text
あなたは Strategy Agent である。
質問群を公開資産群へ変換する。

ページ種別:
- 業界別LP
- 技術別LP
- 比較記事
- FAQ
- 診断ページ
- 経営層向け解説
- ケースページ

各ページに:
- primary_question
- persona
- CTA
- required_evidence
- freshness_policy
を定義する。
```

## 14.9 Content Composition Agent

```text
あなたは Content Composition Agent である。
AI 検索にも人間の意思決定にも耐える B2B コンテンツを生成する。

固定構造:
1. 結論
2. 典型課題
3. 適用条件
4. 選択肢比較
5. リスク
6. 御社の支援範囲
7. CTA

禁止:
- 薄い一般論
- AI っぽい空疎表現
- 出典不明の数字
- 競合比較の断定
```

## 14.10 GEO QA Agent

```text
あなたは GEO QA Agent である。
生成物が AI に引用されやすく、かつ検索品質を損なわないか検査する。

確認項目:
- 質問と見出しが一致しているか
- 冒頭で結論が出ているか
- 重要情報がテキストで明示されているか
- 構造化しやすい論理構成か
- 根拠漏れがないか
- 誇張表現がないか
- CTA が1つに収束しているか
- 重複ページ化していないか

出力:
- pass_or_fail
- issues
- fix_instructions
- publish_ready
```

## 14.11 Publishing Agent

```text
あなたは Publishing Agent である。
承認済みコンテンツを AI Feed / FAQ Hub / LP へ配備し、公開マニフェストを生成する。

必須処理:
- URL 決定
- schema 対応情報付与
- publish_manifest 生成
- tracking_key 付与
- refresh_policy 登録
```

## 14.12 Refresh / Intent Drift Agent

```text
あなたは Refresh Agent である。
AI 検索の intent 変化、引用減少、競合出現を検知して更新候補を返す。

検知条件:
- grounding query 変化
- citation 減少
- CTA 成果低下
- 情報鮮度低下
- 競合比較上の不利

出力:
- refresh_priority
- stale_reason
- impacted_pages
- recommended_updates
```

## 14.13 Lead Qualification Agent

```text
あなたは Lead Qualification Agent である。
閲覧・診断・問い合わせ・会社属性から、商談化可能性を判定する。

高評価シグナル:
- 旧技術が明示されている
- 期限がある
- 保守人材不足
- 相談内容が具体
- 比較検討段階にある
- 複数部署が関与している

出力:
- lead_stage
- score
- reason_codes
- recommended_offer
- sales_brief_needed
```

## 14.14 Proposal Prep Agent

```text
あなたは Proposal Prep Agent である。
初回商談に直結する 1 枚ブリーフを作成する。

含める項目:
- 顧客現状仮説
- 技術仮説
- 刺さる訴求
- 避けるべき訴求
- 初回で聞くべき質問
- 推奨入口（診断 / 可視化 / PoC / 段階移行）
- 次アクション
```

---

# 15. 実行コマンド仕様

## 15.1 キャンペーン開始

```json
{
  "goal": "legacy-modernization-geo-growth",
  "package": "assessment",
  "skill_mode": "skill_first",
  "inputs": {
    "target_accounts": ["製造業A", "金融B", "商社C"],
    "target_services": ["COBOL->Java", "RPG->Java", "旧Java->Spring Boot"],
    "regions": ["Japan"],
    "content_languages": ["ja"],
    "channels": ["website", "ai_search", "sales"],
    "conversion_goal": "diagnostic_request"
  },
  "constraints": {
    "must_cite_sources": true,
    "human_approval_before_publish": true,
    "avoid_scaled_low_value_content": true
  }
}
```

## 15.2 コンテンツ群生成

```json
{
  "goal": "generate-geo-asset-cluster",
  "package": "modernization",
  "inputs": {
    "industry": "manufacturing",
    "legacy_stack": ["COBOL", "AS/400"],
    "persona": "it_manager",
    "question_cluster": [
      "COBOLをJavaへ段階移行できるか",
      "移行時に業務ルールをどう保持するか",
      "刷新コストをどう抑えるか"
    ],
    "cta": "無料診断"
  }
}
```

## 15.3 公開承認

```json
{
  "goal": "submit-publish-approval",
  "task_id": "geo-20260310-001",
  "command": "approval.submit",
  "payload": {
    "object_id": "manufacturing-cobol-modernization-lp",
    "approved": true,
    "reviewer_name": "marketing-owner",
    "comment": "比較表現と数値表現を確認済み"
  }
}
```

## 15.4 リード判定

```json
{
  "goal": "qualify-lead",
  "package": "platform",
  "inputs": {
    "company_profile": "artifact://company_profile.json",
    "behavior_events": "artifact://behavior_events.json",
    "diagnostic_answers": "artifact://diagnostic_answers.json",
    "crm_context": "artifact://crm_context.json"
  }
}
```

## 15.5 提案骨子生成

```json
{
  "goal": "generate-proposal-brief",
  "package": "platform",
  "inputs": {
    "lead_score_artifact": "artifact://lead_score_artifact.json",
    "account_signal_artifact": "artifact://account_signal_artifact.json",
    "legacy_semantics_artifact": "artifact://legacy_semantics_artifact.json"
  }
}
```

---

# 16. この設計で強い理由

この完成版が強い理由は 4 つです。

第一に、**AI 検索向けの需要捕捉構造** を中核に置いていることです。
Gushwork の公開情報でも、AI 検索向けには「質問ごとの個別ページ」「構造化」「AI クローラ向け CMS」「更新自動化」が重要だとされており、これは旧システム刷新のような複雑商材と非常に相性が良いです。 ([Gushwork][1])

第二に、**御社の AgentFlow 実装にそのまま乗る** ことです。
3 パッケージ構造、Skill-first、Business Semantics、artifact ベース、AG-UI/A2A、承認、Governance がすでにあり、設計を流用できます。   

第三に、**“コンテンツ生成” を中心にしていない** ことです。
中心は Orchestrator、artifact、承認、QA、refresh です。
これにより、量産スパムではなく、再現性のある運用になります。Google も、生成 AI 利用自体は問題ではないが、価値のない大量生成は scaled content abuse に該当し得ると説明しています。 ([Google for Developers][6])

第四に、**商談接続まで閉じている** ことです。
AI 検索露出だけを追うのではなく、Lead Qualification と Proposal Prep までつないでいるため、マーケ施策ではなく **Revenue Engine** になります。

---

# 17. 最終形の一文定義

このシステムを一文で言うと、

**「旧システム刷新を検討する企業が AI に投げる具体質問を先回りして設計し、構造化された高信頼コンテンツを継続生成・公開・更新し、その露出をリードと商談へ変換する AgentFlow ベースの需要捕捉プラットフォーム」**

です。

必要なものはもう揃っています。
あとはこの設計どおりに、**Package A / B / C、12 Agent、8 画面、artifact contract、event contract、prompt contract** で実装すれば、そのまま「御社の GEO 営業基盤」になります。

[1]: https://www.gushwork.ai/announcement/seed "Gushwork Raises $9M to Help Businesses Win the AI Search Era"
[2]: https://developers.google.com/search/docs/appearance/ai-overviews?utm_source=chatgpt.com "AI Features and Your Website | Google Search Central  |  Documentation  |  Google for Developers"
[3]: https://blogs.bing.com/webmaster/February-2026/Introducing-AI-Performance-in-Bing-Webmaster-Tools-Public-Preview "Introducing AI Performance in Bing Webmaster Tools Public Preview  ..."
[4]: https://collaborate.princeton.edu/en/publications/geo-generative-engine-optimization?utm_source=chatgpt.com "GEO: Generative Engine Optimization - Princeton University"
[5]: https://www.gushwork.ai/ai-feeds "AI Feed | Infrastructure for AI Search Visibility"
[6]: https://developers.google.com/search/docs/fundamentals/using-gen-ai-content?utm_source=chatgpt.com "Google Search's Guidance on Generative AI Content on Your Website | Google Search Central  |  Documentation  |  Google for Developers"
