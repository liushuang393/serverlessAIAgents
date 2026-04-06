# FAQ 多文書サンプル投入・精度改善 タスク表

| Task | Owner | Status | Evidence | Blocker | Next |
| --- | --- | --- | --- | --- | --- |
| DocumentManager に group/tags metadata 永続化を追加 | Codex | Done | `tests/unit/knowledge/test_collection_manager.py` の metadata テスト | なし | ルータと UI に metadata を通す |
| `/api/collections/{name}/documents` multipart 拡張 | Codex | Done | `apps/faq_system/tests/test_knowledge_panel_integration.py` の upload metadata テスト | なし | UI から scenario/tags/group を送る |
| `PanelDocuments` の batch metadata UI 実装 | Codex | Done | `apps/faq_system/frontend/src/__tests__/PanelDocuments.test.tsx` | なし | fixture と評価資産を追加 |
| 4 種サンプル文書の fixture 生成 | Codex | Done | `apps/faq_system/tests/fixtures/knowledge_samples/hr_travel_bundle/` | なし | parser 抽出テストを追加 |
| 評価クエリ定義追加 | Codex | Done | `query_evaluation.json` | なし | parser / related expansion テストを追加 |
| parser 抽出テスト | Codex | Done | `conda run -n agentflow pytest apps/faq_system/tests/test_knowledge_sample_bundle.py -v --no-cov` | なし | live 評価へ進む |
| 関連文書展開テスト | Codex | Done | `apps/faq_system/tests/test_knowledge_sample_bundle.py::test_related_expansion_returns_group_documents_without_duplicates` | なし | UI / live query に反映する |
| UI E2E を sample upload + retrieval 検証へ更新 | Codex | Done | `cd apps/faq_system/frontend && npx playwright test ../tests/test_rag_ui_e2e.spec.ts` | backend 起動待ちが必要 | related docs 表示も観測する |
| `test-query` に metadata + related docs context を反映 | Codex | Done | `apps/faq_system/routers/collections.py`, `shared/services/rag_service.py` | なし | live query を再測定 |
| live query ベースライン測定 | Codex | Done | `FAQ_EVAL_BASE_URL=http://127.0.0.1:8018 conda run -n agentflow python apps/faq_system/tests/evaluate_knowledge_bundle_live.py` で 5/6 pass | q5 の conflict 解決だけ未達 | q5 を追加改善する |
| 精度改善の根本原因整理 | Codex | In Progress | q2/q3/q5 の失敗原因を切り分け済み | q5 の synthesis 取りこぼし | lexical fallback / hybrid 整合 / answer synthesis を詰める |
| q5 conflict 解決の最終改善 | Codex | In Progress | sources は取得済み、answer で `latest update notice` / `effective date` の明示が不安定 | `test-query` が collection の retrieval_method を完全には反映していない | hybrid/keyword fallback を route から service へ昇格する設計を決める |

## 評価メモ

- 初回 live 評価: 1/6 pass
  - 問題: `test-query` が `RAGService.query` 直結で metadata を落とし、group expansion を通らない。
- 改善後 live 評価: 5/6 pass
  - 改善内容:
    - 検索結果に metadata を残す
    - `test-query` で related docs を merged context に入れて再合成
    - sibling document preview を group fallback として補完
    - primary 0 件時に lexical fallback を実施
- 残課題:
  - `q5_rule_conflict_resolution` は sources 取得済みだが、最終 answer で `latest update notice` と `effective date` の両方を安定的に明示できていない
  - 根本原因は `test-query` が collection の `retrieval_method=hybrid` を本質的には再現しておらず、semantic-first の route 補完に留まっていること
