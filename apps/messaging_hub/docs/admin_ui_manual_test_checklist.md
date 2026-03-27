# Admin UI Manual Test Checklist

## Preconditions

- Backend: `uvicorn apps.messaging_hub.main:app --reload --host 0.0.0.0 --port 8004`
- Frontend: `cd apps/messaging_hub/admin_ui && npm run dev`
- Access: `http://localhost:3001/conversations`

## 1. Auth Guidance and Recovery

- Open UI with empty `MESSAGING_HUB_API_KEY` in browser localStorage.
- Confirm top banner appears and explains auth requirement.
- Enter API key in banner and click `保存`.
- Verify protected API requests recover without full page crash.

## 2. Conversations Primary Flow

- Create a new conversation and send a message.
- Confirm assistant reply appears and list count/time updates.
- Edit one assistant message and confirm updated content is visible.
- Upload one file via `ファイルアップロード記録` and confirm success message.
- Export selected conversation in `JSON/CSV/Markdown` and verify downloaded content is text payload (not raw JSON envelope).
- Confirm websocket status indicator and event status updates are visible.

## 3. Full Feature Reachability

- Navigate to each route from sidebar and mobile nav:
  - `/platforms`
  - `/sessions`
  - `/approvals`
  - `/timeline`
  - `/skills`
  - `/file-organizer`
  - `/settings`
- Confirm every route renders and can fetch data.

## 4. File Organizer Approval Loop

- In `/file-organizer`, run `Dry Run (Preview)` and confirm preview result is shown.
- Click `Execute` and confirm approval-required notice with `request_id` appears.
- Open `/approvals`, approve the request.
- Return to `/file-organizer`, click `Execute` again, and confirm non-dry-run completion state.

## 5. UX Quality Checks

- Error visibility: 401/503/network failures show explicit messages.
- Empty/loading states are understandable and recoverable.
- Desktop and mobile navigation both expose new routes.
- Main actions use short click paths and provide completion feedback.

## 6. PWA Installability Checks

- Open `http://localhost:3001/` on desktop Chrome/Edge and confirm install icon/button is shown.
- Click install and confirm app is added as standalone window with app icon.
- On Android Chrome, verify `Install app` / `Add to Home screen` flow works.
- On iOS Safari, verify manual hint is shown and `Add to Home Screen` works via share menu.
- Re-open installed app and confirm `/api/*` business functions still work (no behavior change).
