/**
 * Migration Studio — フロントエンドロジック
 * SSE でパイプライン進捗を受信し、UIを更新する。
 */

// ---------- 定数 ----------
const API_BASE = window.location.origin;  // 同一オリジン

// 互換マッピング（既存契約テスト/イベント命名との整合維持用）
const LEGACY_STAGE_INDEX_MAP = {
  "migration.analyze_code": 0,
  "migration.extract_business_semantics": 1,
  "migration.design_architecture": 2,
};
const LEGACY_STAGES = ['analysis', 'business_semantics', 'design'];

const STAGE_LABELS = {
  analyzer:      { icon: "1", label: "COBOL解析" },
  designer:      { icon: "2", label: "Spring Boot設計" },
  transformer:   { icon: "3", label: "Java変換" },
  test_generator:{ icon: "4", label: "テスト生成" },
  verifier:      { icon: "5", label: "検証" },
  quality_gate:  { icon: "6", label: "品質ゲート" },
  pipeline:      { icon: "★", label: "パイプライン" },
};

const PIPELINE_STAGES = [
  "analyzer", "designer", "transformer", "test_generator", "verifier", "quality_gate",
];

// ---------- アプリ状態 ----------
let currentTaskId = null;
let currentEventSource = null;
let pendingHITLRequestId = null;
let stageStates = {};    // stage => "pending" | "running" | "complete" | "error"

// ---------- 認証ヘルパー ----------
function getApiKey() {
  return (
    localStorage.getItem("CODE_MIGRATION_API_KEY")
    || new URLSearchParams(window.location.search).get("api_key")
    || ""
  );
}

function buildAuthHeaders(extra = {}) {
  const headers = { ...extra };
  const apiKey = getApiKey();
  if (apiKey) {
    headers["x-api-key"] = apiKey;
  }
  return headers;
}

function withApiKey(url) {
  const apiKey = getApiKey();
  if (!apiKey) return url;
  const sep = url.includes("?") ? "&" : "?";
  return `${url}${sep}api_key=${encodeURIComponent(apiKey)}`;
}

// ---------- ファイル選択 ----------
const fileInput = document.getElementById("file-input");
const selectedFileEl = document.getElementById("selected-file");
const startBtn = document.getElementById("start-btn");
const dropZone = document.getElementById("drop-zone");

fileInput.addEventListener("change", () => {
  const f = fileInput.files[0];
  if (f) {
    selectedFileEl.textContent = `選択済み: ${f.name} (${formatBytes(f.size)})`;
    startBtn.disabled = false;
  }
});

// ドラッグ&ドロップ
dropZone.addEventListener("dragover", (e) => { e.preventDefault(); dropZone.classList.add("dragover"); });
dropZone.addEventListener("dragleave", () => { dropZone.classList.remove("dragover"); });
dropZone.addEventListener("drop", (e) => {
  e.preventDefault();
  dropZone.classList.remove("dragover");
  const f = e.dataTransfer.files[0];
  if (f) {
    const dt = new DataTransfer();
    dt.items.add(f);
    fileInput.files = dt.files;
    selectedFileEl.textContent = `選択済み: ${f.name} (${formatBytes(f.size)})`;
    startBtn.disabled = false;
  }
});

// ---------- 移行開始 ----------
async function startMigration() {
  const file = fileInput.files[0];
  if (!file) return;

  const fastMode = document.getElementById("fast-mode").checked;
  const model = document.getElementById("model-select").value;

  // UIリセット
  resetUI();
  initStageList();

  startBtn.disabled = true;
  document.getElementById("progress-section").style.display = "block";
  document.getElementById("result-section").style.display = "none";

  appendLog(`アップロード開始: ${file.name}`, "info");

  // アップロード
  const formData = new FormData();
  formData.append("file", file);

  let uploadData;
  try {
    const res = await fetch(
      `${API_BASE}/api/migrate/upload?fast=${fastMode}&model=${encodeURIComponent(model)}`,
      {
        method: "POST",
        headers: buildAuthHeaders(),
        body: formData,
      }
    );
    if (!res.ok) {
      const err = await res.json().catch(() => ({ detail: res.statusText }));
      throw new Error(err.detail || res.statusText);
    }
    uploadData = await res.json();
  } catch (e) {
    appendLog(`アップロードエラー: ${e.message}`, "error");
    showResult(false, "アップロード失敗", e.message);
    startBtn.disabled = false;
    return;
  }

  currentTaskId = uploadData.task_id;
  appendLog(`タスクID: ${currentTaskId}`, "info");
  appendLog(`対象プログラム: ${uploadData.program_names.join(", ")}`, "info");

  // SSE ストリーム接続
  connectSSE(uploadData.stream_url);
}

// ---------- SSE接続 ----------
function connectSSE(streamUrl) {
  if (currentEventSource) {
    currentEventSource.close();
  }

  const es = new EventSource(withApiKey(`${API_BASE}${streamUrl}`));
  currentEventSource = es;

  es.onmessage = (e) => {
    const data = JSON.parse(e.data);
    handleEvent(data);
  };

  es.onerror = () => {
    appendLog("SSE接続エラー（再接続中...）", "warn");
    // EventSource は自動再接続するが、タスク完了後はクローズ
    if (currentEventSource) {
      const task = currentTaskId;
      setTimeout(() => checkStatusAndClose(task), 3000);
    }
  };
}

async function checkStatusAndClose(taskId) {
  if (taskId !== currentTaskId) return;
  try {
    const res = await fetch(
      withApiKey(`${API_BASE}/api/migrate/${taskId}/status`),
      { headers: buildAuthHeaders() }
    );
    const data = await res.json();
    if (data.status === "complete" || data.status === "error") {
      if (currentEventSource) {
        currentEventSource.close();
        currentEventSource = null;
      }
    }
  } catch (_) {}
}

// ---------- イベントハンドリング ----------
function handleEvent(event) {
  const type = event.type;
  const stage = event.stage;

  switch (type) {
    case "stage_start":
      updateStage(stage, "running");
      appendLog(`▶ ${stageLabel(stage)}: ${event.message || "開始"}`, "info");
      break;

    case "progress":
      appendLog(`  ${stageLabel(stage)}: ${event.message || ""}${event.current ? ` (${event.current}/${event.total})` : ""}`, "info");
      break;

    case "stage_complete": {
      updateStage(stage, "complete");
      const decision = event.decision ? ` [${event.decision}]` : "";
      appendLog(`✓ ${stageLabel(stage)} 完了${decision}`, "success");
      break;
    }

    case "evolution": {
      const iterNum = event.iteration || "";
      appendLog(`⟳ Evolution #${iterNum}: ${event.problem || ""} → ${event.fix || "プロンプト改善"}`, "warn");
      // 対象ステージをリセット
      if (stage) resetStageFrom(stage);
      break;
    }

    case "hitl_required":
      showHITLDialog(event);
      appendLog(`⚠ HITL要求: ${stageLabel(stage)}`, "warn");
      break;

    case "complete":
      PIPELINE_STAGES.forEach(s => {
        if (stageStates[s] !== "complete") updateStage(s, "complete");
      });
      appendLog("✓ 移行完了！", "success");
      if (currentEventSource) { currentEventSource.close(); currentEventSource = null; }
      showResult(true, "移行完了", `プログラム: ${event.program_name || ""}  バージョン: v${event.version || 1}`);
      if (currentTaskId) document.getElementById("download-btn").style.display = "inline-flex";
      startBtn.disabled = false;
      break;

    case "error": {
      const errStage = stage || "pipeline";
      updateStage(errStage, "error");
      appendLog(`✗ エラー: ${event.message || "不明なエラー"}`, "error");
      if (currentEventSource) { currentEventSource.close(); currentEventSource = null; }
      showResult(false, "移行失敗", event.message || "エラーが発生しました");
      startBtn.disabled = false;
      break;
    }

    default:
      appendLog(`[${type}] ${JSON.stringify(event)}`, "info");
  }
}

// ---------- ステージUI ----------
function initStageList() {
  stageStates = {};
  const list = document.getElementById("stage-list");
  list.innerHTML = "";
  PIPELINE_STAGES.forEach(s => {
    stageStates[s] = "pending";
    const item = document.createElement("div");
    item.className = "stage-item pending";
    item.id = `stage-${s}`;
    item.innerHTML = `
      <div class="stage-icon">${STAGE_LABELS[s]?.icon || "?"}</div>
      <div style="flex:1">
        <span>${STAGE_LABELS[s]?.label || s}</span>
      </div>
      <div class="stage-status" style="font-size:0.8rem;color:var(--muted)">待機中</div>
    `;
    list.appendChild(item);
  });
}

function updateStage(stage, status) {
  if (!stage || stage === "pipeline") return;
  stageStates[stage] = status;
  const el = document.getElementById(`stage-${stage}`);
  if (!el) return;
  el.className = `stage-item ${status}`;
  const statusLabels = { pending: "待機中", running: "実行中", complete: "完了", error: "エラー" };
  el.querySelector(".stage-status").textContent = statusLabels[status] || status;
}

function resetStageFrom(startStage) {
  const idx = PIPELINE_STAGES.indexOf(startStage);
  if (idx === -1) return;
  for (let i = idx; i < PIPELINE_STAGES.length; i++) {
    updateStage(PIPELINE_STAGES[i], "pending");
  }
}

// ---------- ログ ----------
function appendLog(msg, level = "info") {
  const log = document.getElementById("log");
  const line = document.createElement("div");
  line.className = `log-${level}`;
  const now = new Date().toLocaleTimeString("ja-JP");
  line.textContent = `[${now}] ${msg}`;
  log.appendChild(line);
  log.scrollTop = log.scrollHeight;
}

// ---------- 結果表示 ----------
function showResult(success, title, desc) {
  const section = document.getElementById("result-section");
  const banner = document.getElementById("result-banner");
  section.style.display = "block";
  banner.className = `result-banner ${success ? "success" : "error"}`;
  document.getElementById("result-title").textContent = title;
  document.getElementById("result-desc").textContent = desc;
}

// ---------- ダウンロード ----------
async function downloadResult() {
  if (!currentTaskId) return;
  const url = withApiKey(`${API_BASE}/api/migrate/${currentTaskId}/download`);
  const a = document.createElement("a");
  a.href = url;
  a.download = "";
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
}

// ---------- HITL ----------
function showHITLDialog(event) {
  pendingHITLRequestId = event.request_id;
  document.getElementById("hitl-question").textContent = event.question || "設計内容を確認してください。";

  const unknowns = event.unknowns || [];
  const ul = document.getElementById("hitl-unknowns");
  if (unknowns.length > 0) {
    ul.innerHTML = "<strong>不明点リスト:</strong><ul>" +
      unknowns.map(u => `<li>${escapeHtml(u)}</li>`).join("") + "</ul>";
    ul.style.display = "block";
  } else {
    ul.style.display = "none";
  }

  document.getElementById("hitl-comment").value = "";
  document.getElementById("hitl-dialog").classList.add("open");
}

async function submitHITL(approved) {
  if (!currentTaskId || !pendingHITLRequestId) return;
  const comment = document.getElementById("hitl-comment").value;

  document.getElementById("hitl-dialog").classList.remove("open");

  try {
    const res = await fetch(`${API_BASE}/api/migrate/${currentTaskId}/hitl`, {
      method: "POST",
      headers: buildAuthHeaders({ "Content-Type": "application/json" }),
      body: JSON.stringify({
        request_id: pendingHITLRequestId,
        approved,
        comment,
        modifications: {},
      }),
    });
    if (!res.ok) {
      const err = await res.json().catch(() => ({}));
      appendLog(`HITL送信エラー: ${err.detail || res.statusText}`, "error");
    } else {
      appendLog(`HITL応答送信: ${approved ? "承認" : "拒否"} "${comment}"`, approved ? "success" : "warn");
    }
  } catch (e) {
    appendLog(`HITL送信失敗: ${e.message}`, "error");
  }

  pendingHITLRequestId = null;
}

// ---------- UIリセット ----------
function resetUI() {
  document.getElementById("log").innerHTML = "";
  document.getElementById("result-section").style.display = "none";
  document.getElementById("download-btn").style.display = "none";
  currentTaskId = null;
  pendingHITLRequestId = null;
  if (currentEventSource) { currentEventSource.close(); currentEventSource = null; }
}

// ---------- ユーティリティ ----------
function stageLabel(stage) {
  return STAGE_LABELS[stage]?.label || stage;
}

function formatBytes(bytes) {
  if (bytes < 1024) return `${bytes} B`;
  if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)} KB`;
  return `${(bytes / 1024 / 1024).toFixed(1)} MB`;
}

function escapeHtml(str) {
  return str.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
}
