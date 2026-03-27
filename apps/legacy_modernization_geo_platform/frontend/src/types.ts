export type TaskStatus =
  | "queued"
  | "running"
  | "waiting_approval"
  | "completed"
  | "failed"
  | "cancelled";

export interface GeoExecuteRequest {
  campaign_name: string;
  package: string;
  targets: {
    industries: string[];
    legacy_stacks: string[];
    regions: string[];
  };
  inputs?: {
    content_languages?: string[];
  };
}

export interface TaskEvent {
  event_type: string;
  timestamp: string;
  task_id: string;
  stage?: string | null;
  agent?: string | null;
  message?: string | null;
  payload: Record<string, unknown>;
}

export interface ArtifactRecord {
  artifact_name: string;
  stage: string;
  path: string;
  summary: string;
  created_at: string;
}

export interface ApprovalRecord {
  request_id: string;
  task_id: string;
  stage: string;
  object_id: string;
  risk_level: string;
  reason: string;
  status: "pending" | "approved" | "rejected" | "rewrite";
  actions: string[];
  comment?: string | null;
  reviewer_name?: string | null;
  created_at: string;
  updated_at: string;
}

export interface PublishedPageRecord {
  slug: string;
  title: string;
  page_url: string;
  html_path: string;
}

export interface ReportPayload {
  task_id: string;
  markdown: string;
  summary: Record<string, unknown>;
  created_at: string;
  updated_at: string;
}

export interface TaskStateResponse {
  task_id: string;
  status: TaskStatus;
  current_stage?: string | null;
  campaign_name: string;
  package: string;
  request: GeoExecuteRequest;
  events: TaskEvent[];
  artifacts: ArtifactRecord[];
  approvals: ApprovalRecord[];
  report?: ReportPayload | null;
  published_pages: PublishedPageRecord[];
  error_message?: string | null;
}

export interface ContentDraftPage {
  slug: string;
  title: string;
  summary: string;
  body_markdown: string;
  cta: string;
  faq_entries: { question: string; answer: string }[];
}

export interface ContentDraftArtifact {
  pages: ContentDraftPage[];
  target_language: string;
  unknowns: string[];
}

export interface GeoQAReport {
  pass_or_fail: string;
  issues: string[];
  fix_instructions: string[];
  publish_ready: boolean;
  risk_level: string;
  metrics: Record<string, number>;
}

export interface AccountSignalArtifact {
  company: string;
  signals: {
    type: string;
    description: string;
    source: string;
    confidence: number;
  }[];
  urgency_hypothesis: string;
  modernization_fit_score: number;
}

export interface QuestionGraphArtifact {
  personas: {
    role: string;
    questions: string[];
    high_intent_questions: string[];
  }[];
}

export interface EvidenceMatrixArtifact {
  entries: {
    claim: string;
    question_ref: string;
    source_url: string;
    title: string;
    publisher: string;
    summary: string;
    reliability: string;
    citation_ready: boolean;
    fresh: boolean;
  }[];
}
