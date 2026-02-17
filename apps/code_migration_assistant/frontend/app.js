/**
 * Code Migration Assistant - UI Controller with AG-UI Standard Protocol
 */

class MigrationUI {
    constructor() {
        this.currentTaskId = null;
        this.socket = null;
        this.isRunning = false;

        this.apiBase = "http://localhost:8003";
        this.steps = ['analysis', 'design', 'transform', 'testing', 'diff', 'quality', 'fix'];

        this.initElements();
        this.attachListeners();
    }

    getApiKey() {
        return (
            localStorage.getItem("CODE_MIGRATION_API_KEY")
            || new URLSearchParams(window.location.search).get("api_key")
            || ""
        );
    }

    buildHeaders(extraHeaders = {}) {
        const headers = { ...extraHeaders };
        const apiKey = this.getApiKey();
        if (apiKey) {
            headers["x-api-key"] = apiKey;
        }
        return headers;
    }

    buildWebSocketUrl(taskId) {
        const apiKey = this.getApiKey();
        const params = new URLSearchParams();
        if (apiKey) {
            params.set("api_key", apiKey);
        }
        const suffix = params.toString() ? `?${params.toString()}` : "";
        return `ws://localhost:8003/api/ws/${taskId}${suffix}`;
    }

    parseSocketPayload(rawPayload) {
        const payload = (rawPayload || "").trim();
        if (!payload) return null;

        if (payload.startsWith("data:")) {
            const jsonPart = payload.replace(/^data:\s*/, "").trim();
            return JSON.parse(jsonPart);
        }
        return JSON.parse(payload);
    }

    initElements() {
        this.startBtn = document.getElementById('startBtn');
        this.logContent = document.getElementById('logContent');
        this.sourceCode = document.getElementById('sourceCode');
        this.targetCode = document.getElementById('targetCode');
        this.taskInfo = document.getElementById('taskInfo');
        this.stepsEls = document.querySelectorAll('.step');

        // HITL Elements
        this.approvalModal = document.getElementById('approvalModal');
        this.approveBtn = document.getElementById('approveBtn');
        this.rejectBtn = document.getElementById('rejectBtn');
    }

    attachListeners() {
        this.startBtn.addEventListener('click', () => this.startMigration());
        this.approveBtn.addEventListener('click', () => this.handleApproval(true));
        this.rejectBtn.addEventListener('click', () => this.handleApproval(false));
    }

    addLog(message, type = "") {
        const entry = document.createElement('div');
        entry.className = `log-entry ${type}`;
        entry.textContent = `[${new Date().toLocaleTimeString()}] ${message}`;
        this.logContent.appendChild(entry);
        this.logContent.scrollTop = this.logContent.scrollHeight;
    }

    async startMigration() {
        if (this.isRunning) return;

        const sourceCode = this.sourceCode.textContent;

        this.currentTaskId = null;
        this.isRunning = true;
        this.startBtn.disabled = true;
        this.startBtn.textContent = "Connecting...";

        try {
            // 1. Start Task via API
            const response = await fetch(`${this.apiBase}/api/migration/start`, {
                method: "POST",
                headers: this.buildHeaders({ "Content-Type": "application/json" }),
                body: JSON.stringify({
                    source_code: sourceCode,
                    migration_type: "cobol-to-springboot"
                })
            });

            if (!response.ok) {
                const message = response.status === 401
                    ? "Unauthorized. Set CODE_MIGRATION_API_KEY in localStorage."
                    : "Failed to start migration";
                throw new Error(message);
            }

            const data = await response.json();
            this.currentTaskId = data.task_id;
            this.taskInfo.textContent = `Task: ${this.currentTaskId.substring(0, 8)}`;

            // 2. Connect WebSocket
            this.connectWebSocket(this.currentTaskId);

        } catch (e) {
            this.addLog(`Error: ${e.message}`, "danger");
            this.isRunning = false;
            this.startBtn.disabled = false;
        }
    }

    connectWebSocket(taskId) {
        this.socket = new WebSocket(this.buildWebSocketUrl(taskId));

        this.socket.onopen = () => {
            this.addLog("Connected to Migration Engine stream.", "success");
        };

        this.socket.onmessage = (event) => {
            try {
                const data = this.parseSocketPayload(event.data);
                if (data) {
                    this.handleEvent(data);
                }
            } catch (e) {
                this.addLog(`Failed to parse stream event: ${e.message}`, "danger");
            }
        };

        this.socket.onclose = () => {
            this.addLog("Stream connection closed.", "warning");
            this.isRunning = false;
            this.startBtn.disabled = false;
            this.startBtn.textContent = "Start Migration Pipeline";
        };
    }

    handleEvent(data) {
        // Handle AG-UI Standard Events
        const eventType = data.event_type || data.type;

        switch (eventType) {
            case "flow.start":
                this.addLog(`Flow Started: ${data.flow_id}`, "system");
                break;
            case "node.start":
                this.addLog(`Starting Node: ${data.node_name}`, "system");
                this.updateStepUI(data.node_name, "active");
                break;
            case "node.complete":
                this.addLog(`Completed Node: ${data.node_name}`, "success");
                this.updateStepUI(data.node_name, "completed");
                break;
            case "log":
                const levelClass = data.level === "ERROR" ? "danger" : "";
                this.addLog(data.message, levelClass);
                break;
            case "approval.required":
            case "approval_required":
                this.showApprovalModal(data);
                break;
            case "flow.complete":
                this.addLog("Flow Completed Successfully.", "success");
                if (data.result && data.result.report_url) {
                    this.addReportLink(data.result.report_url);
                }
                break;
            case "flow.error":
                this.addLog(`Flow Error: ${data.error_message}`, "danger");
                break;
            default:
                console.log("Unknown event:", data);
        }
    }

    updateStepUI(nodeName, state) {
        // Map node names to UI steps (simple heuristic)
        // Adjust these mappings based on actual node names from engine
        const map = {
            "migration.analyze_code": 0,
            "migration.design_architecture": 1,
            "migration.transform_code": 2,
            "migration.synthesize_tests": 3,
            "migration.verify_diff": 4,
            "migration.evaluate_quality": 5,
            "migration.apply_fix": 6,
            "migration.generate_report": 7
        };

        const index = map[nodeName];
        if (index !== undefined) {
            const el = this.stepsEls[index];
            if (state === "active") {
                this.stepsEls.forEach(s => s.classList.remove('active'));
                el.classList.add('active');
            } else if (state === "completed") {
                el.classList.add('completed');
            }
        }
    }

    showApprovalModal(request) {
        this.approvalRequestId = request.request_id;
        this.addLog(`Approval Required: ${request.reason}`, "warning");
        this.approvalModal.classList.remove('hidden');
    }

    async handleApproval(approved) {
        if (!this.approvalRequestId) return;

        try {
            await fetch(`${this.apiBase}/api/approvals/${this.currentTaskId}/${this.approvalRequestId}`, {
                method: "POST",
                headers: this.buildHeaders({ "Content-Type": "application/json" }),
                body: JSON.stringify({
                    approved: approved,
                    comment: approved ? "Approved via Dashboard" : "Rejected via Dashboard"
                })
            });
            this.approvalModal.classList.add('hidden');
            this.addLog(approved ? "Approval Submitted." : "Rejection Submitted.", "success");
        } catch (e) {
            this.addLog(`Failed to submit approval: ${e.message}`, "danger");
        }
    }

    addReportLink(url) {
        const entry = document.createElement('div');
        entry.className = `log-entry success`;
        const link = document.createElement('a');
        link.href = `${this.apiBase}${url}`;
        link.target = "_blank";
        link.className = "report-link";
        link.textContent = "📄 View Professional Migration Report (Markdown)";
        entry.appendChild(link);
        this.logContent.appendChild(entry);
        this.logContent.scrollTop = this.logContent.scrollHeight;
    }
}

// Initialize on load
window.addEventListener('DOMContentLoaded', () => {
    window.migrationApp = new MigrationUI();
});
