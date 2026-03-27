"""Reranker モジュール."""

from __future__ import annotations

import importlib
import json
import logging
import math
import os
import re
from abc import ABC, abstractmethod
from collections.abc import Callable
from dataclasses import dataclass, field
from enum import Enum, StrEnum
from typing import Any


logger = logging.getLogger(__name__)
DocumentInput = str | dict[str, Any]


class RerankerType(StrEnum):
    COHERE = "cohere"
    CROSS_ENCODER = "cross_encoder"
    CROSS_ENCODER_RURI = "cross_encoder_ruri"
    LLM_LISTWISE = "llm_listwise"
    BM25 = "bm25"


@dataclass
class RankedDocument:
    content: str
    score: float
    original_index: int
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass
class _DocPayload:
    doc_id: str
    text: str
    original_index: int
    metadata: dict[str, Any]


class BaseReranker(ABC):
    def __init__(self) -> None:
        self._logger = logging.getLogger(self.__class__.__name__)

    @property
    @abstractmethod
    def reranker_type(self) -> RerankerType: ...

    @abstractmethod
    async def rerank(
        self,
        query: str,
        documents: list[DocumentInput],
        top_k: int = 5,
    ) -> list[RankedDocument]: ...

    def recommended_search_k(self, top_k: int) -> int:
        """推奨の初期検索件数.

        デフォルトは従来互換の 3x 拡張。
        """
        return max(top_k, top_k * 3)

    def _normalize_docs(self, documents: list[DocumentInput]) -> list[str]:
        doc_texts: list[str] = []
        for doc in documents:
            if isinstance(doc, str):
                doc_texts.append(doc)
            else:
                content = doc.get("content", doc.get("document", str(doc)))
                doc_texts.append(str(content))
        return doc_texts

    def _normalize_payloads(self, documents: list[DocumentInput]) -> list[_DocPayload]:
        payloads: list[_DocPayload] = []
        for index, doc in enumerate(documents):
            if isinstance(doc, str):
                payloads.append(
                    _DocPayload(
                        doc_id=f"doc-{index}",
                        text=doc,
                        original_index=index,
                        metadata={},
                    )
                )
                continue

            content = str(doc.get("content", doc.get("document", str(doc))))
            raw_id = _clean_text(doc.get("id")) or f"doc-{index}"
            metadata = {k: v for k, v in doc.items() if k not in {"id", "content", "document"}}
            payloads.append(
                _DocPayload(
                    doc_id=raw_id,
                    text=content,
                    original_index=index,
                    metadata=metadata,
                )
            )
        return payloads


class CohereReranker(BaseReranker):
    def __init__(
        self,
        api_key: str | None = None,
        model: str | None = None,
    ) -> None:
        super().__init__()
        self._api_key = api_key or os.getenv("COHERE_API_KEY")
        self._model = model or os.getenv("RERANKER_COHERE_MODEL") or "rerank-multilingual-v3.0"
        self._client: Any = None

    @property
    def reranker_type(self) -> RerankerType:
        return RerankerType.COHERE

    async def rerank(self, query: str, documents: list[DocumentInput], top_k: int = 5) -> list[RankedDocument]:
        if not self._api_key:
            msg = "COHERE_API_KEY is required"
            raise ValueError(msg)

        payloads = self._normalize_payloads(documents)
        doc_texts = [payload.text for payload in payloads]

        try:
            if self._client is None:
                cohere_module = importlib.import_module("cohere")
                self._client = cohere_module.Client(self._api_key)

            response = self._client.rerank(
                query=query,
                documents=doc_texts,
                top_n=max(1, top_k),
                model=self._model,
            )

            results: list[RankedDocument] = []
            for result in response.results:
                if result.index >= len(payloads):
                    continue
                payload = payloads[result.index]
                results.append(
                    RankedDocument(
                        content=payload.text,
                        score=float(result.relevance_score),
                        original_index=payload.original_index,
                        metadata=payload.metadata,
                    )
                )
            return results
        except ImportError:
            msg = "cohere package required: pip install cohere"
            raise ImportError(msg)


class CrossEncoderReranker(BaseReranker):
    def __init__(self, model_name: str | None = None) -> None:
        super().__init__()
        self._model_name = (
            model_name or os.getenv("RERANKER_CROSS_ENCODER_MODEL") or "cross-encoder/ms-marco-MiniLM-L-6-v2"
        )
        self._model: Any = None

    @property
    def reranker_type(self) -> RerankerType:
        return RerankerType.CROSS_ENCODER

    async def rerank(self, query: str, documents: list[DocumentInput], top_k: int = 5) -> list[RankedDocument]:
        payloads = self._normalize_payloads(documents)
        doc_texts = [payload.text for payload in payloads]

        try:
            if self._model is None:
                sentence_transformers = importlib.import_module("sentence_transformers")
                self._model = sentence_transformers.CrossEncoder(self._model_name)

            pairs = [(query, doc) for doc in doc_texts]
            scores = self._model.predict(pairs)
            scored_docs = list(zip(payloads, scores, strict=False))
            scored_docs.sort(key=lambda item: item[1], reverse=True)

            results: list[RankedDocument] = []
            for payload, score in scored_docs[: max(1, top_k)]:
                results.append(
                    RankedDocument(
                        content=payload.text,
                        score=float(score),
                        original_index=payload.original_index,
                        metadata=payload.metadata,
                    )
                )
            return results
        except ImportError:
            msg = "sentence-transformers required: pip install sentence-transformers"
            raise ImportError(msg)


class CrossEncoderRuriReranker(CrossEncoderReranker):
    def __init__(self, model_name: str | None = None) -> None:
        super().__init__(
            model_name=model_name or os.getenv("RERANKER_CROSS_ENCODER_RURI_MODEL") or "cl-nagoya/ruri-v3-reranker-310m"
        )

    @property
    def reranker_type(self) -> RerankerType:
        return RerankerType.CROSS_ENCODER_RURI


class BM25Reranker(BaseReranker):
    @property
    def reranker_type(self) -> RerankerType:
        return RerankerType.BM25

    async def rerank(self, query: str, documents: list[DocumentInput], top_k: int = 5) -> list[RankedDocument]:
        payloads = self._normalize_payloads(documents)
        query_terms = set(query.lower().split())

        scores: list[tuple[_DocPayload, float]] = []
        for payload in payloads:
            doc_terms = payload.text.lower().split()
            doc_term_set = set(doc_terms)
            common_terms = query_terms.intersection(doc_term_set)
            tf = len(common_terms) / max(len(query_terms), 1)
            doc_length_factor = 1.0 / (1.0 + len(doc_terms) / 100)
            score = tf * doc_length_factor
            scores.append((payload, score))

        scores.sort(key=lambda item: item[1], reverse=True)

        results: list[RankedDocument] = []
        for payload, score in scores[: max(1, top_k)]:
            results.append(
                RankedDocument(
                    content=payload.text,
                    score=float(score),
                    original_index=payload.original_index,
                    metadata=payload.metadata,
                )
            )
        return results


class LLMListwiseReranker(BaseReranker):
    """汎用 LLM による Listwise reranker.

    既定で Partial-Filtering と Adaptive Early Stop を有効化し、
    失敗時は BM25 へフォールバックする。
    """

    def __init__(self, options: dict[str, Any] | None = None) -> None:
        super().__init__()
        cfg = options or {}

        self._candidate_pool_size = _coerce_int(
            _pick_config_value(cfg, "candidate_pool_size", "RERANKER_LLM_CANDIDATE_POOL_SIZE"),
            default=100,
            min_value=1,
            max_value=500,
        )
        self._partial_keep_n = _coerce_int(
            _pick_config_value(cfg, "partial_keep_n", "RERANKER_LLM_PARTIAL_KEEP_N"),
            default=60,
            min_value=1,
            max_value=300,
        )
        self._partial_rescue_k = _coerce_int(
            _pick_config_value(cfg, "partial_rescue_k", "RERANKER_LLM_PARTIAL_RESCUE_K"),
            default=10,
            min_value=1,
            max_value=100,
        )
        self._early_stop_blocks = _coerce_int(
            _pick_config_value(cfg, "early_stop_blocks", "RERANKER_LLM_EARLY_STOP_BLOCKS"),
            default=2,
            min_value=1,
            max_value=10,
        )
        self._max_doc_chars = _coerce_int(
            _pick_config_value(cfg, "max_doc_chars", "RERANKER_LLM_MAX_DOC_CHARS"),
            default=400,
            min_value=80,
            max_value=2000,
        )
        self._enable_partial_filtering = _coerce_bool(
            _pick_config_value(
                cfg,
                "enable_partial_filtering",
                "RERANKER_LLM_ENABLE_PARTIAL_FILTERING",
            ),
            default=True,
        )
        self._enable_early_stop = _coerce_bool(
            _pick_config_value(
                cfg,
                "enable_early_stop",
                "RERANKER_LLM_ENABLE_EARLY_STOP",
            ),
            default=True,
        )
        self._heavy_model = _clean_text(_pick_config_value(cfg, "model", "RERANKER_LLM_MODEL")) or ""
        self._light_model = (
            _clean_text(_pick_config_value(cfg, "light_model", "RERANKER_LLM_LIGHT_MODEL")) or self._heavy_model
        )

        self._llm: Any = None

    @property
    def reranker_type(self) -> RerankerType:
        return RerankerType.LLM_LISTWISE

    def recommended_search_k(self, top_k: int) -> int:
        return max(top_k, self._candidate_pool_size)

    async def rerank(self, query: str, documents: list[DocumentInput], top_k: int = 5) -> list[RankedDocument]:
        payloads = self._normalize_payloads(documents)
        if not payloads:
            return []

        limit = max(1, min(len(payloads), self._candidate_pool_size))
        candidates = payloads[:limit]

        try:
            if self._enable_partial_filtering and len(candidates) > max(top_k, self._partial_keep_n):
                candidates = await self._apply_partial_filtering(query, candidates, top_k)

            if self._enable_early_stop and len(candidates) > top_k:
                ranked_payloads = await self._apply_adaptive_early_stop(query, candidates, top_k)
            else:
                ranked_payloads, _, _ = await self._rank_with_llm(
                    query,
                    candidates,
                    top_k,
                    model=self._heavy_model,
                    include_stop_signal=False,
                    phase="final",
                )

            if not ranked_payloads:
                return await self._fallback_bm25(query, candidates, top_k)

            return self._to_ranked_documents(ranked_payloads, top_k)
        except Exception as exc:
            self._logger.warning("LLM listwise rerank failed. fallback=bm25 error=%s", exc)
            return await self._fallback_bm25(query, candidates, top_k)

    async def _apply_partial_filtering(
        self,
        query: str,
        candidates: list[_DocPayload],
        top_k: int,
    ) -> list[_DocPayload]:
        rescue_k = min(self._partial_rescue_k, max(0, len(candidates) - top_k))
        if rescue_k <= 0:
            return candidates

        upper_keep = min(len(candidates), max(top_k, self._partial_keep_n - rescue_k))
        upper_cluster = candidates[:upper_keep]
        lower_cluster = candidates[upper_keep:]

        if not lower_cluster:
            return candidates

        rescued, _, _ = await self._rank_with_llm(
            query,
            lower_cluster,
            rescue_k,
            model=self._light_model,
            include_stop_signal=False,
            phase="partial",
        )

        merged: list[_DocPayload] = []
        seen_ids: set[str] = set()
        for payload in upper_cluster + rescued:
            if payload.doc_id in seen_ids:
                continue
            seen_ids.add(payload.doc_id)
            merged.append(payload)

        for payload in candidates:
            if len(merged) >= max(top_k, self._partial_keep_n):
                break
            if payload.doc_id in seen_ids:
                continue
            seen_ids.add(payload.doc_id)
            merged.append(payload)

        return merged

    async def _apply_adaptive_early_stop(
        self,
        query: str,
        candidates: list[_DocPayload],
        top_k: int,
    ) -> list[_DocPayload]:
        block_count = max(1, min(self._early_stop_blocks, len(candidates)))
        block_size = max(1, math.ceil(len(candidates) / block_count))

        working: list[_DocPayload] = []
        for block_index in range(block_count):
            start = block_index * block_size
            end = min(len(candidates), start + block_size)
            if start >= end:
                break

            working.extend(candidates[start:end])
            ranked, sufficient, _ = await self._rank_with_llm(
                query,
                working,
                top_k,
                model=self._heavy_model,
                include_stop_signal=True,
                phase=f"early_stop_{block_index}",
            )

            if ranked:
                keep_limit = max(top_k, top_k * 2)
                working = ranked[:keep_limit]

            if sufficient and len(working) >= top_k:
                break

        final_ranked, _, _ = await self._rank_with_llm(
            query,
            working,
            top_k,
            model=self._heavy_model,
            include_stop_signal=False,
            phase="final",
        )
        return final_ranked or working[:top_k]

    async def _rank_with_llm(
        self,
        query: str,
        candidates: list[_DocPayload],
        top_k: int,
        *,
        model: str,
        include_stop_signal: bool,
        phase: str,
    ) -> tuple[list[_DocPayload], bool, list[str]]:
        if not candidates:
            return [], False, []

        prompt = self._build_listwise_prompt(
            query,
            candidates,
            top_k,
            include_stop_signal=include_stop_signal,
            phase=phase,
        )
        raw_response = await self._call_llm(prompt=prompt, model=model)
        allowed_ids = {payload.doc_id for payload in candidates}
        ranked_ids, sufficient, required_info = self._parse_ranking_response(
            raw_response,
            allowed_ids=allowed_ids,
            include_stop_signal=include_stop_signal,
        )
        if not ranked_ids:
            return [], sufficient, required_info

        by_id = {payload.doc_id: payload for payload in candidates}
        ordered: list[_DocPayload] = []
        seen: set[str] = set()

        for doc_id in ranked_ids:
            payload = by_id.get(doc_id)
            if payload is None or doc_id in seen:
                continue
            seen.add(doc_id)
            ordered.append(payload)

        for payload in candidates:
            if payload.doc_id in seen:
                continue
            seen.add(payload.doc_id)
            ordered.append(payload)

        return ordered[: max(1, top_k)], sufficient, required_info

    async def _call_llm(self, *, prompt: str, model: str) -> str:
        from infrastructure.llm.providers import get_llm

        if self._llm is None:
            self._llm = get_llm(temperature=0.0)

        kwargs: dict[str, Any] = {
            "temperature": 0.0,
            "max_tokens": 1200,
        }
        if model:
            kwargs["model"] = model

        response = await self._llm.chat(
            [
                {
                    "role": "system",
                    "content": ("You are a reranker. Return compact, deterministic JSON only."),
                },
                {
                    "role": "user",
                    "content": prompt,
                },
            ],
            **kwargs,
        )

        content = response.get("content")
        if isinstance(content, str):
            return content
        return ""

    def _build_listwise_prompt(
        self,
        query: str,
        candidates: list[_DocPayload],
        top_k: int,
        *,
        include_stop_signal: bool,
        phase: str,
    ) -> str:
        docs_text: list[str] = []
        for payload in candidates:
            snippet = _truncate_text(payload.text, self._max_doc_chars)
            docs_text.append(f"- id={payload.doc_id}\n  text={snippet}")

        stop_instruction = (
            "- sufficient: boolean\n- required_info: string[] (missing facts)\n" if include_stop_signal else ""
        )

        return (
            f"phase: {phase}\n"
            f"include_stop_signal={'true' if include_stop_signal else 'false'}\n"
            f"query: {query}\n"
            f"top_k: {top_k}\n"
            "documents:\n"
            f"{os.linesep.join(docs_text)}\n\n"
            "Return strict JSON with this shape:\n"
            "{\n"
            '  "ranked_ids": ["doc-id", "..."],\n'
            f"{stop_instruction}"
            "}\n"
            "Only use provided ids."
        )

    def _parse_ranking_response(
        self,
        response_text: str,
        *,
        allowed_ids: set[str],
        include_stop_signal: bool,
    ) -> tuple[list[str], bool, list[str]]:
        ranked_ids: list[str] = []
        sufficient = False
        required_info: list[str] = []

        json_blob = _extract_json_blob(response_text)
        if json_blob:
            try:
                payload = json.loads(json_blob)
                if isinstance(payload, dict):
                    ranked_ids = _normalize_ranked_ids(payload.get("ranked_ids"), allowed_ids)
                    if include_stop_signal:
                        sufficient = _coerce_bool(payload.get("sufficient"), default=False)
                        required_info = _normalize_required_info(payload.get("required_info"))
            except json.JSONDecodeError:
                pass

        if not ranked_ids:
            for token in re.findall(r"[A-Za-z0-9][A-Za-z0-9_:\\-]*", response_text):
                if token in allowed_ids and token not in ranked_ids:
                    ranked_ids.append(token)

        if include_stop_signal and not sufficient:
            lowered = response_text.lower()
            if "sufficient: true" in lowered or "sufficient=true" in lowered:
                sufficient = True

        return ranked_ids, sufficient, required_info

    async def _fallback_bm25(
        self,
        query: str,
        candidates: list[_DocPayload],
        top_k: int,
    ) -> list[RankedDocument]:
        bm25 = BM25Reranker()
        bm25_inputs: list[DocumentInput] = [
            {
                "id": payload.doc_id,
                "content": payload.text,
                **payload.metadata,
            }
            for payload in candidates
        ]

        bm25_ranked = await bm25.rerank(query, bm25_inputs, top_k)
        ordered: list[_DocPayload] = []
        score_map: dict[str, float] = {}

        for item in bm25_ranked:
            if item.original_index < 0 or item.original_index >= len(candidates):
                continue
            payload = candidates[item.original_index]
            ordered.append(payload)
            score_map[payload.doc_id] = float(item.score)

        return self._to_ranked_documents(ordered, top_k, score_map=score_map)

    def _to_ranked_documents(
        self,
        ordered_docs: list[_DocPayload],
        top_k: int,
        *,
        score_map: dict[str, float] | None = None,
    ) -> list[RankedDocument]:
        if not ordered_docs:
            return []

        limited = ordered_docs[: max(1, top_k)]
        total = max(1, len(limited))
        results: list[RankedDocument] = []

        for index, payload in enumerate(limited):
            if score_map is not None and payload.doc_id in score_map:
                score = float(score_map[payload.doc_id])
            else:
                score = max(0.0, 1.0 - (index / total))
            results.append(
                RankedDocument(
                    content=payload.text,
                    score=score,
                    original_index=payload.original_index,
                    metadata=payload.metadata,
                )
            )
        return results


RerankerFactory = Callable[[dict[str, Any] | None], BaseReranker]
_RERANKER_REGISTRY: dict[str, RerankerFactory] = {}


def register_reranker(name: str, factory: RerankerFactory, *, overwrite: bool = False) -> None:
    normalized = _normalize_registry_name(name)
    if not normalized:
        msg = "reranker name is required"
        raise ValueError(msg)

    if not overwrite and normalized in _RERANKER_REGISTRY:
        msg = f"reranker already registered: {normalized}"
        raise ValueError(msg)

    _RERANKER_REGISTRY[normalized] = factory


def unregister_reranker(name: str) -> None:
    normalized = _normalize_registry_name(name)
    if not normalized:
        return
    _RERANKER_REGISTRY.pop(normalized, None)


def list_registered_rerankers() -> list[str]:
    return sorted(_RERANKER_REGISTRY.keys())


def get_reranker(
    reranker_type: RerankerType | str | None = None,
    options: dict[str, Any] | None = None,
) -> BaseReranker:
    name = _resolve_reranker_name(reranker_type)
    factory = _RERANKER_REGISTRY.get(name)

    if factory is None:
        logger.warning("Unknown reranker '%s'. fallback=bm25", name)
        factory = _RERANKER_REGISTRY.get(RerankerType.BM25.value)

    if factory is None:
        return BM25Reranker()

    try:
        return factory(options)
    except Exception as exc:
        logger.warning("Failed to create reranker '%s': %s", name, exc)
        return BM25Reranker()


def _resolve_reranker_name(reranker_type: RerankerType | str | None) -> str:
    if isinstance(reranker_type, RerankerType):
        return reranker_type.value

    if isinstance(reranker_type, str):
        normalized = _normalize_registry_name(reranker_type)
        if normalized:
            return normalized

    env_type = _clean_text(os.getenv("RERANKER_TYPE"))
    if env_type:
        return env_type.lower()

    if os.getenv("COHERE_API_KEY"):
        return RerankerType.COHERE.value

    try:
        importlib.import_module("sentence_transformers")
        return RerankerType.CROSS_ENCODER.value
    except ImportError:
        return RerankerType.BM25.value


def _register_builtin_rerankers() -> None:
    def _create_bm25(_options: dict[str, Any] | None) -> BaseReranker:
        return BM25Reranker()

    register_reranker(
        RerankerType.BM25.value,
        _create_bm25,
        overwrite=True,
    )
    register_reranker(
        RerankerType.COHERE.value,
        lambda options: CohereReranker(
            api_key=_clean_text((options or {}).get("api_key")),
            model=_clean_text((options or {}).get("model")),
        ),
        overwrite=True,
    )
    register_reranker(
        RerankerType.CROSS_ENCODER.value,
        lambda options: CrossEncoderReranker(
            model_name=_clean_text((options or {}).get("model_name")) or _clean_text((options or {}).get("model")),
        ),
        overwrite=True,
    )
    register_reranker(
        RerankerType.CROSS_ENCODER_RURI.value,
        lambda options: CrossEncoderRuriReranker(
            model_name=_clean_text((options or {}).get("model_name")) or _clean_text((options or {}).get("model")),
        ),
        overwrite=True,
    )
    register_reranker(
        RerankerType.LLM_LISTWISE.value,
        lambda options: LLMListwiseReranker(options=options),
        overwrite=True,
    )


def _normalize_registry_name(value: str | None) -> str:
    if value is None:
        return ""
    return value.strip().lower()


def _clean_text(value: Any) -> str | None:
    if value is None:
        return None
    text = str(value).strip()
    return text or None


def _pick_config_value(cfg: dict[str, Any], key: str, env_var: str) -> Any:
    if key in cfg and cfg[key] is not None:
        return cfg[key]
    return os.getenv(env_var)


def _coerce_int(value: Any, *, default: int, min_value: int, max_value: int) -> int:
    try:
        resolved = int(str(value).strip())
    except (TypeError, ValueError):
        resolved = default
    if resolved < min_value:
        return min_value
    if resolved > max_value:
        return max_value
    return resolved


def _coerce_bool(value: Any, *, default: bool) -> bool:
    if isinstance(value, bool):
        return value
    if value is None:
        return default

    lowered = str(value).strip().lower()
    if lowered in {"1", "true", "yes", "on"}:
        return True
    if lowered in {"0", "false", "no", "off"}:
        return False
    return default


def _truncate_text(text: str, max_chars: int) -> str:
    if len(text) <= max_chars:
        return text.replace("\n", " ")
    return text[: max_chars - 3].replace("\n", " ") + "..."


def _extract_json_blob(text: str) -> str | None:
    stripped = text.strip()
    if stripped.startswith("{") and stripped.endswith("}"):
        return stripped

    match = re.search(r"\{[\s\S]*\}", stripped)
    if match:
        return match.group(0)
    return None


def _normalize_ranked_ids(value: Any, allowed_ids: set[str]) -> list[str]:
    ranked_ids: list[str] = []

    if isinstance(value, str):
        candidates = [item.strip() for item in value.split(",") if item.strip()]
    elif isinstance(value, list):
        candidates = [str(item).strip() for item in value if str(item).strip()]
    else:
        candidates = []

    for candidate in candidates:
        if candidate not in allowed_ids:
            continue
        if candidate in ranked_ids:
            continue
        ranked_ids.append(candidate)

    return ranked_ids


def _normalize_required_info(value: Any) -> list[str]:
    if isinstance(value, list):
        return [str(item).strip() for item in value if str(item).strip()]
    if isinstance(value, str) and value.strip():
        return [value.strip()]
    return []


_register_builtin_rerankers()


__all__ = [
    "BM25Reranker",
    "BaseReranker",
    "CohereReranker",
    "CrossEncoderReranker",
    "CrossEncoderRuriReranker",
    "LLMListwiseReranker",
    "RankedDocument",
    "RerankerType",
    "get_reranker",
    "list_registered_rerankers",
    "register_reranker",
    "unregister_reranker",
]
