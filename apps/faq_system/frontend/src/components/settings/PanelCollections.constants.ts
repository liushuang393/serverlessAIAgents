/**
 * PanelCollections のプリセット定義.
 *
 * react-refresh/only-export-components 対応のため、
 * 非コンポーネントエクスポートを分離。
 */

export interface PresetValues {
    chunk_strategy: string;
    chunk_size: number;
    chunk_overlap: number;
    retrieval_method: string;
    reranker: string;
    top_k: number;
    min_similarity: number;
}

export type PresetKey =
    | "faq_precision"
    | "balanced_knowledge"
    | "long_doc_reasoning"
    | "custom";

export interface PresetDef {
    key: PresetKey;
    labelKey: string;
    descriptionKey: string;
    values: PresetValues | null;
}

export const PRESETS: PresetDef[] = [
    {
        key: "faq_precision",
        labelKey: "knowledge_panel.pattern_faq_precision",
        descriptionKey: "knowledge_panel.pattern_faq_precision_desc",
        values: {
            chunk_strategy: "sentence",
            chunk_size: 500,
            chunk_overlap: 80,
            retrieval_method: "hybrid",
            reranker: "cohere",
            top_k: 8,
            min_similarity: 0.25,
        },
    },
    {
        key: "balanced_knowledge",
        labelKey: "knowledge_panel.pattern_balanced",
        descriptionKey: "knowledge_panel.pattern_balanced_desc",
        values: {
            chunk_strategy: "recursive",
            chunk_size: 800,
            chunk_overlap: 120,
            retrieval_method: "hybrid",
            reranker: "bm25",
            top_k: 6,
            min_similarity: 0.2,
        },
    },
    {
        key: "long_doc_reasoning",
        labelKey: "knowledge_panel.pattern_long_doc",
        descriptionKey: "knowledge_panel.pattern_long_doc_desc",
        values: {
            chunk_strategy: "markdown",
            chunk_size: 1200,
            chunk_overlap: 180,
            retrieval_method: "multi_query",
            reranker: "cross_encoder",
            top_k: 10,
            min_similarity: 0.3,
        },
    },
    {
        key: "custom",
        labelKey: "knowledge_panel.pattern_custom",
        descriptionKey: "knowledge_panel.pattern_custom_desc",
        values: null,
    },
];
