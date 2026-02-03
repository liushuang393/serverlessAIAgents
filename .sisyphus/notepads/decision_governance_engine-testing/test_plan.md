## 2026-02-03 Decision Governance Engine E2E Test Plan (Coverage Matrix)

### Scope
- Target app: Decision Governance Engine SPA
  - URL: http://localhost:5174
  - Vite: `apps/decision_governance_engine/frontend/vite.config.ts` (port 5174, proxy `/api` -> `http://localhost:8001`)
- Navigation model (state machine, NOT react-router):
  - `apps/decision_governance_engine/frontend/src/App.tsx`
  - `useDecisionStore().currentPage`: input, processing, report, history, knowledge-shu, knowledge-qi

### Deterministic testing strategy (no LLM keys required)
Goal: Tests run without backend / without external keys.

We will mock in the browser test runtime:
- `fetch` (via Playwright `page.route`) for:
  - `/api/auth/*`  (Login/Logout/Me)
  - `/api/config/*` (RAG settings)
  - `/api/knowledge/*` (Knowledge docs)
  - `/api/decision/history*` (History list/detail/delete)
  - `/api/report/*` (PDF export / sign / signature)
- `EventSource` (via Playwright `page.addInitScript`) for:
  - `/api/decision/stream` (SSE progress + final report)

### Global UI stability / beauty checks (apply to every page)
For every test page state:
- [ ] Not blank: key heading or primary container exists (e.g., "Decision Agent" or page title)
- [ ] No ErrorBoundary fallback visible: ensure text "アプリケーションエラー" is NOT shown
- [ ] No console errors: fail test on `page.on('pageerror')` or console error events
- [ ] Key controls visible & enabled states correct (buttons/inputs)
- [ ] Layout not broken: basic viewport checks
  - Desktop: 1280x720
  - Mobile: 390x844

### Selector strategy
Prefer stable selectors:
1) Role-based: `getByRole()` (button, textbox, heading, combobox)
2) Label-based: `getByLabel()` (e.g., "ユーザー名", "パスワード")
3) Text-based: `getByText()` for stable Japanese labels
Fallback:
- `aria-label` present in some places (e.g., DecisionInputPage rejection close button "閉じる")
- Avoid CSS selectors tied to Tailwind classes.

### Test data fixtures
- Users (from UI + backend auth router):
  - admin / admin123
  - tanaka / tanaka123
  - suzuki / suzuki123
  - yamamoto / yamamoto123
  - Source:
    - UI hint: `apps/decision_governance_engine/frontend/src/components/LoginPage.tsx`
    - Backend: `apps/decision_governance_engine/routers/auth.py`

- Questions:
  - valid_question: "新規事業AとBのどちらに投資すべきか。予算と期限も考慮して判断したい"
  - too_short: "短い"
  - instant_reject_weather: "今日の天気はどうですか？教えてください"
  - instant_reject_system: "このシステムの仕組みを教えて"

### Coverage Matrix

#### 1) LoginPage (unauthenticated entry)
Source: `apps/decision_governance_engine/frontend/src/components/LoginPage.tsx`

| Area | User events | Backend calls | Assertions | Negative cases |
|------|-------------|---------------|------------|----------------|
| Initial render | page load | GET `/api/auth/me` (via `useAuthStore.checkAuth`) | Title "Decision Agent" visible; login button disabled when inputs empty | `/api/auth/me` returns unauthenticated => stays on login |
| Form typing | type username/password | none | login button enabled only when both non-empty | whitespace-only => button disabled |
| Login submit | click "ログイン" or submit form | POST `/api/auth/login` | on success transitions away from login (input page visible); no error banner | invalid creds => error banner shown; remains on login |
| Demo login | click "デモアカウントを使用する" -> select user | POST `/api/auth/login` | succeeds; demonstrates dropdown open/close and user selection | API failure => error banner visible |
| Password toggle | click eye button | none | input type toggles between password/text | none |
| Stability | load + interactions | n/a | no console errors, not blank, layout ok on mobile | none |

#### 2) DecisionInputPage (main entry after auth)
Sources:
- UI: `apps/decision_governance_engine/frontend/src/components/DecisionInputPage.tsx`
- State: `apps/decision_governance_engine/frontend/src/store/useDecisionStore.ts`

| Area | User events | Backend calls | Assertions | Negative cases |
|------|-------------|---------------|------------|----------------|
| Authenticated render | navigate after login | GET `/api/auth/me` (optional) | Header shows user display_name + dept; main CTA visible "決策分析を開始する" | if auth lost => back to LoginPage |
| Question validation | type question | none | helper text shows "最低10文字以上入力してください" until >=10; CTA disabled/enabled | too_short cannot proceed |
| Instant reject patterns | type reject text then click start | none | rejection card visible, category badge, close button works | ensure no navigation to ProcessingPage |
| Constraints inputs | fill budget/timeline/team + add/remove tags | none | tags added on Enter; remove works; numeric inputs accept strings | invalid numeric string should not crash |
| Navigate: History | click "履歴" | none | currentPage becomes history (HistoryPage visible) | none |
| Open Settings | click "⚙️" | GET `/api/config/rag` | modal opens and shows loading then tabs | API error => error banner in modal |
| Navigate: Knowledge | click "知識追加" for 術/器 | GET `/api/knowledge/{shu|qi}` | correct KnowledgePage shown with agent label | API error => error UI shown |
| Logout | click logout icon | POST `/api/auth/logout` | returns to LoginPage; decision state reset | logout API failure still clears local state |
| Start analysis | click "決策分析を開始する" | (SSE starts on ProcessingPage) | page switches to ProcessingPage | reject patterns prevent switch |

#### 3) ProcessingPage (SSE progress)
Sources:
- UI: `apps/decision_governance_engine/frontend/src/components/ProcessingPage.tsx`
- Stream: `apps/decision_governance_engine/frontend/src/hooks/useDecisionStream.ts`

| Area | User events | Backend calls | Assertions | Negative cases |
|------|-------------|---------------|------------|----------------|
| Auto-start stream | enter processing | EventSource `/api/decision/stream?...` (mocked) | connection indicator toggles to connected; agent cards transition waiting->running->completed | connection timeout -> error banner + retry |
| Progress updates | observe | SSE events | overall progress % updates; agent cards show progress and completion marks | malformed event -> no crash; error message shown if parse fails |
| Thinking log | expand panel | none | logs appear; toggle open/close works | none |
| Cancel | click "キャンセル" | none (stopStream) | returns to input page with question preserved | none |
| Error + retry | simulate SSE error | none | error banner shown + retry and back buttons visible; retry replays stream | repeated failure should keep UI stable |
| Complete -> view report | click "📄 決策レポートを表示" | none | transitions to ReportPage; report stored in state | missing report => button not shown |

#### 4) ReportPage (view + export + sign)
Source: `apps/decision_governance_engine/frontend/src/components/ReportPage.tsx`

| Area | User events | Backend calls | Assertions | Negative cases |
|------|-------------|---------------|------------|----------------|
| Guard | open without report | none | auto redirects to input | none |
| Tabs | click tabs: サマリー/道/法/術/器/品質保証 | none | active tab content changes; no runtime errors | missing fields uses safe defaults |
| Export PDF | click export button | GET `/api/report/{id}/pdf` | download triggered; success notification appears | API error -> error notification |
| Sign | click sign button -> confirm dialog OK | POST `/api/report/{id}/sign` | signed status set; signature area shows hanko; success notification | cancel confirm -> remains unsigned; 401 -> asks to login |
| Logout | click logout | POST `/api/auth/logout` | back to login; state cleared | logout API failure still clears local state |
| New question | click new question | none | back to input with empty state | none |
| Visual | desktop/mobile | n/a | layout stable; key elements visible | none |

#### 5) HistoryPage (list, filter, detail, delete)
Source: `apps/decision_governance_engine/frontend/src/components/HistoryPage.tsx`

| Area | User events | Backend calls | Assertions | Negative cases |
|------|-------------|---------------|------------|----------------|
| Initial load | open page | GET `/api/decision/history?limit=...` | list renders; loading state then items | status=disabled -> error "履歴機能が無効" |
| Filters | change result/mode filters; change limit | GET history with query | query reflected; list updates | API error -> error banner |
| Detail | click detail for an item | GET `/api/decision/history/{id}` | detail modal renders sections | error -> detail error shown |
| Download PDF | click download | GET `/api/report/{id}/pdf` | download triggered | error -> error banner |
| Delete | click delete -> confirm -> delete | DELETE `/api/decision/history/{id}` | item removed from list | delete failure -> error banner |
| Navigation | click "⚡ 新規分析" or logo | none | back to input | none |

#### 6) KnowledgePage (shu/qi knowledge CRUD)
Source: `apps/decision_governance_engine/frontend/src/components/KnowledgePage.tsx`

| Area | User events | Backend calls | Assertions | Negative cases |
|------|-------------|---------------|------------|----------------|
| Initial load | open shu / qi | GET `/api/knowledge/{agentType}` | header shows correct agent label; list renders | fetch fails -> error banner |
| Add knowledge | select topic, type >=10 chars, click add | POST `/api/knowledge/{agentType}` | item appears in list; input cleared | <10 chars -> inline error; POST fail -> error banner |
| Delete knowledge | click delete, confirm | DELETE `/api/knowledge/{agentType}/{id}` | item removed | cancel confirm => no delete |
| Back nav | click "← 戻る" | none | returns to input | none |
| Visual risk check | render topic pills and buttons | n/a | verify styles not missing (Tailwind dynamic class risk) | if styles missing, capture screenshot + flag |

#### 7) SettingsModal (RAG config)
Source: `apps/decision_governance_engine/frontend/src/components/SettingsModal.tsx`

| Area | User events | Backend calls | Assertions | Negative cases |
|------|-------------|---------------|------------|----------------|
| Open/close | open from input; close (X/閉じる) | GET `/api/config/rag` | modal appears; tabs show agent list | GET fail -> error shown |
| Agent tab switch | click agent tab | none | selected agent changes | none |
| Toggle RAG | click toggle | PUT `/api/config/rag/{agentId}` | state toggles; re-fetch updates | PUT fail -> error shown |
| Adjust params | move sliders | PUT `/api/config/rag/{agentId}` | displayed values update | PUT fail -> error shown |
| Visual | modal layout | n/a | modal not overflowing; scroll works | none |

### Non-scope / notes
- Python unit test failure (engine rejection status) is tracked separately and not a blocker for UI E2E.
- E2E tests should avoid depending on long-running real SSE/LLM calls; keep mocks deterministic.
