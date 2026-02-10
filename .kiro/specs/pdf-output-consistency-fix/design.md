# Design Document: PDF Output Consistency Fix

## Overview

This design addresses the PDF output consistency issue in the decision_governance_engine application where PDF exports do not match the UI display. The root cause is incomplete HTML generation in the fallback mode due to truncated method implementations. The fix ensures complete content rendering in both ReportLab PDF mode and HTML fallback mode.

### Problem Statement

Users report that PDF exports are missing content and have different styling compared to what's displayed in the UI. Investigation reveals:

1. The `_build_shu_html` method in `pdf_generator.py` was incomplete (truncated at line 759)
2. This causes incomplete HTML generation for the 術 (shu) section
3. The PDF generator has two modes: ReportLab (true PDF) and HTML fallback
4. Both modes need to render all v3.0 fields consistently with the UI

### Goals

1. Ensure PDF output contains all sections visible in the UI (道/法/術/器/検証)
2. Ensure all data fields are present in the PDF (no missing content)
3. Ensure both ReportLab PDF and HTML fallback modes work correctly
4. Maintain styling consistency between UI and PDF output
5. Complete all truncated method implementations

## Architecture

### Current Architecture

```
┌─────────────────┐
│   ReportPage    │  (React UI Component)
│   (Frontend)    │
└────────┬────────┘
         │ GET /api/report/{id}/pdf
         ▼
┌─────────────────┐
│  report.py      │  (FastAPI Router)
│  export_pdf()   │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ PDFGenerator    │
│   Service       │
├─────────────────┤
│ • generate_pdf()│
│ • _generate_    │
│   with_reportlab│
│ • _generate_    │
│   html_fallback │
│ • _build_*_html │
└─────────────────┘
```

### Data Flow

1. User clicks "PDF出力" button in ReportPage
2. Frontend calls `GET /api/report/{report_id}/pdf`
3. Router retrieves DecisionReport from cache or DB
4. PDFGeneratorService checks if ReportLab is available
5. If available: generates PDF using ReportLab
6. If not: generates HTML fallback
7. Returns PDF/HTML as StreamingResponse

### Component Responsibilities

**PDFGeneratorService**:
- Check ReportLab availability
- Generate PDF using ReportLab (if available)
- Generate HTML fallback (if ReportLab unavailable)
- Build section-specific HTML for each report section
- Handle errors and log failures

**Section Builder Methods**:
- `_build_summary_html()`: Executive summary section
- `_build_dao_html()`: 道 (essence analysis) section
- `_build_fa_html()`: 法 (strategy selection) section
- `_build_shu_html()`: 術 (execution plan) section
- `_build_qi_html()`: 器 (technical implementation) section
- `_build_review_html()`: 検証 (verification) section

## Components and Interfaces

### PDFGeneratorService

```python
class PDFGeneratorService:
    """Decision レポート PDF 生成サービス v3.0."""
    
    def __init__(self) -> None:
        """初期化."""
        
    def _check_reportlab(self) -> bool:
        """ReportLabが利用可能か確認."""
        
    def generate_pdf(self, report: DecisionReport) -> bytes:
        """PDFを生成."""
        
    def _to_dict(self, obj: Any) -> dict:
        """Pydanticオブジェクトまたはdictをdictに変換."""
        
    def _generate_with_reportlab(self, report: DecisionReport) -> bytes:
        """ReportLabでPDF生成 v3.0（CJK対応・全フィールド出力）."""
        
    def _generate_html_fallback(self, report: DecisionReport) -> bytes:
        """HTML形式での提案書出力 v3.1."""
        
    def _build_summary_html(self, summary: Any) -> str:
        """エグゼクティブサマリーHTMLを構築."""
        
    def _build_dao_html(self, dao: dict) -> str:
        """道セクションHTMLを構築 v3.0."""
        
    def _build_fa_html(self, fa: dict) -> str:
        """法セクションHTMLを構築 v3.0."""
        
    def _build_shu_html(self, shu: dict) -> str:
        """術セクションHTMLを構築 v3.0."""
        
    def _build_qi_html(self, qi: dict) -> str:
        """器セクションHTMLを構築 v3.0."""
        
    def _build_review_html(self, review: dict) -> str:
        """検証セクションHTMLを構築."""
```

### Key Interfaces

**Input**: `DecisionReport` (Pydantic model)
- Contains all sections: dao, fa, shu, qi, review, executive_summary
- Includes v3.0 fields: essence_derivation, strategic_prohibitions, exit_criteria
- Includes v3.1 fields: proposal_title, signature_block

**Output**: `bytes`
- PDF binary data (ReportLab mode)
- HTML UTF-8 encoded bytes (fallback mode)

**Error Handling**:
- `ValueError`: Invalid input (report is None)
- `RuntimeError`: PDF generation failure
- Logging: All errors logged with context

## Data Models

### DecisionReport Structure

```python
DecisionReport:
  - report_id: str
  - created_at: datetime
  - version: str
  - proposal_title: ProposalTitle | None
  - original_question: str
  - dao: DaoOutput | dict
  - fa: FaOutput | dict
  - shu: ShuOutput | dict
  - qi: QiOutput | dict
  - review: ReviewOutput | dict
  - executive_summary: ExecutiveSummary
  - signature_block: SignatureBlock | None
```

### Section Data Models

**DaoOutput** (道 - Essence Analysis):
- problem_type: str
- problem_nature: str
- essence: str
- essence_derivation: dict (v3.0)
  - surface_problem: str
  - underlying_why: str
  - root_constraint: str
  - essence_statement: str
- existing_alternatives: list[dict]
- immutable_constraints: list[str]
- hidden_assumptions: list[str]
- causal_gears: list[dict]
- bottleneck_gear: str
- death_traps: list[dict]

**FaOutput** (法 - Strategy Selection):
- recommended_paths: list[dict]
- rejected_paths: list[dict]
- decision_criteria: list[str]
- path_comparison: dict
- strategic_prohibitions: list[dict] (v3.0)
- differentiation_axis: dict (v3.0)
- why_existing_fails: str (v3.0)

**ShuOutput** (術 - Execution Plan):
- phases: list[dict]
- first_action: str
- dependencies: list[str]
- rhythm_control: dict
- cut_list: list[str] (v3.0)
- context_specific_actions: list[dict] (v3.0)
- single_validation_point: dict (v3.0)
- exit_criteria: dict (v3.0)

**QiOutput** (器 - Technical Implementation):
- implementations: list[dict]
- tool_recommendations: list[str]
- integration_points: list[str]
- technical_debt_warnings: list[str]
- domain_technologies: list[dict] (v3.0)
- regulatory_considerations: list[dict] (v3.0)
- geographic_considerations: list[dict] (v3.0)

**ReviewOutput** (検証 - Verification):
- overall_verdict: str (PASS/REVISE/REJECT)
- confidence_score: float
- findings: list[dict]
- final_warnings: list[str]

### HTML Structure

The HTML fallback mode generates a complete business proposal document with:

1. Cover page (表紙)
   - Title (Japanese/English)
   - Case ID
   - Date and author information

2. Table of contents (目次)

3. Executive summary (エグゼクティブサマリー)

4. Section 2: Current issues (現状の課題・問題点) - 道

5. Section 3: Proposed solution (提案内容・解決策) - 法

6. Section 4: Execution plan (実行計画・スケジュール) - 術

7. Section 5: Technical implementation (技術的な実装) - 器

8. Section 6: Verification results (根拠・検証結果) - 検証

9. Section 7: Signature block (署名欄)

10. Footer with metadata

## Correctness Properties


*A property is a characteristic or behavior that should hold true across all valid executions of a system—essentially, a formal statement about what the system should do. Properties serve as the bridge between human-readable specifications and machine-verifiable correctness guarantees.*

### Property 1: Complete Section Field Coverage

*For any* DecisionReport object with populated section data, generating a PDF (in either ReportLab or HTML mode) should produce output that contains all specified fields from each section (executive_summary, dao, fa, shu, qi, review), including v3.0 fields (essence_derivation, strategic_prohibitions, cut_list, context_specific_actions, single_validation_point, exit_criteria, domain_technologies, regulatory_considerations, geographic_considerations).

**Validates: Requirements 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 2.1, 2.2**

### Property 2: Graceful Null Handling

*For any* DecisionReport object with null or undefined fields, generating a PDF should complete successfully without exceptions, and the output should contain appropriate fallback text (e.g., "N/A", "未設定", empty string) for missing fields rather than error messages or broken formatting.

**Validates: Requirements 2.3**

### Property 3: V3.0 Field Inclusion

*For any* DecisionReport object containing v3.0 fields (essence_statement in executive_summary, strategic_prohibition_summary in executive_summary, exit_criteria_summary in executive_summary, essence_derivation in dao, strategic_prohibitions in fa, cut_list in shu, context_specific_actions in shu, single_validation_point in shu, exit_criteria in shu, domain_technologies in qi, regulatory_considerations in qi, geographic_considerations in qi), generating a PDF should include these fields in the output with their values rendered.

**Validates: Requirements 2.4, 8.4**

### Property 4: HTML Builder Method Completeness

*For any* valid section data (dao, fa, shu, qi, review), calling the corresponding builder method (_build_dao_html, _build_fa_html, _build_shu_html, _build_qi_html, _build_review_html) should return a complete HTML string that contains properly closed tags, includes all expected section markers (h2, h3 headings), and does not end prematurely or with syntax errors.

**Validates: Requirements 3.1, 3.2, 3.3, 3.4**

### Property 5: Styling Consistency

*For any* DecisionReport object, the generated PDF output should apply equivalent styling for status indicators (success/emerald, warning/amber, prohibition/red CSS classes or color codes), preserve hierarchical structure (nested divs, lists, tables), include visual indicators (emoji icons like 🎯, ⚠️, ✅), and render tables with proper HTML table structure (<table>, <tr>, <td> tags).

**Validates: Requirements 4.1, 4.2, 4.3, 4.4**

### Property 6: ReportLab Mode Completeness

*For any* DecisionReport object, when ReportLab is available and ReportLab mode is used, generating a PDF should produce a valid PDF binary that contains all section content (executive summary, dao, fa, shu, qi, review), properly renders CJK characters using registered fonts, and includes page breaks at appropriate section boundaries.

**Validates: Requirements 5.1, 5.2, 5.4**

### Property 7: Error Logging with Context

*For any* error that occurs during PDF generation (ValueError for invalid input, RuntimeError for generation failure, exceptions in builder methods), the PDF_Generator should log the error with contextual information including report_id, section name (if applicable), error type, and error message, and should raise a descriptive exception that can be caught by the caller.

**Validates: Requirements 6.1, 6.2, 6.3**

### Property 8: ReportLab Fallback Behavior

*For any* DecisionReport object, when ReportLab is not available (ImportError on reportlab.lib.pagesizes), the PDF_Generator should log a warning message indicating fallback to HTML mode, set _has_reportlab to False, and successfully generate HTML output instead of raising an exception.

**Validates: Requirements 6.4**

### Property 9: Round-Trip Information Preservation

*For any* DecisionReport object with key fields populated (report_id, executive_summary.one_line_decision, dao.essence, fa.recommended_paths, shu.first_action, qi.implementations, review.overall_verdict), generating a PDF and extracting its text content (for HTML mode) or inspecting the generated elements (for ReportLab mode) should preserve all these key information fields in a human-readable format.

**Validates: Requirements 8.1**

## Error Handling

### Error Categories

1. **Input Validation Errors**
   - `ValueError`: Raised when report is None or invalid
   - Logged with error context
   - Propagated to caller with descriptive message

2. **PDF Generation Errors**
   - `RuntimeError`: Raised when PDF generation fails
   - Logged with full exception traceback
   - Includes original exception as cause (`raise ... from e`)

3. **ReportLab Import Errors**
   - `ImportError`: Caught during ReportLab availability check
   - Logged as warning (not error)
   - Triggers fallback to HTML mode

4. **Section Builder Errors**
   - Any exception in builder methods is caught
   - Logged with section name and error details
   - Wrapped in RuntimeError with context

### Error Handling Strategy

```python
def generate_pdf(self, report: DecisionReport) -> bytes:
    # Input validation
    if report is None:
        raise ValueError("report cannot be None")
    
    try:
        # Attempt generation
        if self._has_reportlab:
            return self._generate_with_reportlab(report)
        return self._generate_html_fallback(report)
    except Exception as e:
        # Log with context
        self._logger.error(
            f"PDF generation failed: {type(e).__name__}: {e}",
            exc_info=True,
        )
        # Raise with context
        raise RuntimeError(f"PDF生成に失敗しました: {e}") from e
```

### Logging Standards

All error logs must include:
- Error type (`type(e).__name__`)
- Error message (`str(e)`)
- Context (report_id, section name if applicable)
- Full traceback (`exc_info=True`)

Example:
```python
self._logger.error(
    f"Section builder failed: section=shu, report_id={report.report_id}, error={type(e).__name__}: {e}",
    exc_info=True,
)
```

## Testing Strategy

### Dual Testing Approach

This bugfix requires both unit testing and property-based testing to ensure comprehensive coverage:

**Unit Tests**: Verify specific examples, edge cases, and error conditions
- Test with sample DecisionReport objects
- Test null/undefined field handling
- Test error conditions (None input, missing sections)
- Test ReportLab availability detection
- Test fallback behavior

**Property Tests**: Verify universal properties across all inputs
- Generate random DecisionReport objects with various field combinations
- Verify all properties hold for generated reports
- Test with 100+ iterations per property
- Cover edge cases through randomization (empty lists, null fields, long strings)

### Property-Based Testing Configuration

**Library**: Use `hypothesis` for Python property-based testing

**Test Configuration**:
- Minimum 100 iterations per property test
- Use `@given` decorator with custom strategies
- Tag each test with feature and property reference

**Example Test Structure**:
```python
from hypothesis import given, strategies as st
import pytest

# Feature: pdf-output-consistency-fix, Property 1: Complete Section Field Coverage
@given(report=decision_report_strategy())
def test_complete_section_field_coverage(report):
    """全セクションのフィールドがPDF出力に含まれることを検証."""
    generator = PDFGeneratorService()
    pdf_bytes = generator.generate_pdf(report)
    
    # HTML mode: check text content
    if not generator._has_reportlab:
        html_content = pdf_bytes.decode('utf-8')
        assert report.dao.essence in html_content
        assert report.executive_summary.one_line_decision in html_content
        # ... check all required fields
```

### Custom Strategies

Define Hypothesis strategies for generating test data:

```python
from hypothesis import strategies as st

@st.composite
def decision_report_strategy(draw):
    """DecisionReportオブジェクトを生成する戦略."""
    return DecisionReport(
        report_id=draw(st.text(min_size=1, max_size=50)),
        dao=draw(dao_output_strategy()),
        fa=draw(fa_output_strategy()),
        shu=draw(shu_output_strategy()),
        qi=draw(qi_output_strategy()),
        review=draw(review_output_strategy()),
        executive_summary=draw(executive_summary_strategy()),
    )

@st.composite
def dao_output_strategy(draw):
    """DaoOutputオブジェクトを生成する戦略."""
    return {
        "problem_type": draw(st.sampled_from(["TRADE_OFF", "TIMING", "RESOURCE"])),
        "essence": draw(st.text(min_size=1, max_size=200)),
        "essence_derivation": draw(st.one_of(st.none(), essence_derivation_strategy())),
        # ... other fields
    }
```

### Unit Test Coverage

**Specific Examples**:
1. Test with complete DecisionReport (all fields populated)
2. Test with minimal DecisionReport (only required fields)
3. Test with v3.0 fields present
4. Test with v3.0 fields absent (backward compatibility)
5. Test with null/undefined fields
6. Test with empty lists and empty strings

**Edge Cases**:
1. Very long text fields (>1000 characters)
2. Special characters in text (CJK, emoji, HTML entities)
3. Empty sections (empty lists, empty dicts)
4. Missing optional fields

**Error Conditions**:
1. None input → ValueError
2. Invalid report structure → RuntimeError
3. ReportLab unavailable → HTML fallback
4. Builder method exception → RuntimeError with context

### Integration Testing

Test the complete flow from API endpoint to PDF generation:

1. Call `GET /api/report/{report_id}/pdf`
2. Verify response status code (200)
3. Verify content type (application/pdf or text/html)
4. Verify content length > 0
5. Verify PDF/HTML is valid (can be parsed)

### Test Execution

```bash
# Run all PDF generator tests
pytest --no-cov tests/unit/services/test_pdf_generator.py

# Run property tests only
pytest --no-cov -k "property" tests/unit/services/test_pdf_generator.py

# Run with coverage
pytest tests/unit/services/test_pdf_generator.py

# Run integration tests
pytest --no-cov -m integration tests/integration/test_pdf_export.py
```

## Implementation Notes

### Code Completeness Fix

The primary fix is to ensure all builder methods are complete:

1. **_build_shu_html**: Complete the method implementation
   - Add all missing HTML generation code
   - Ensure proper tag closing
   - Include all v3.0 fields (cut_list, context_specific_actions, single_validation_point, exit_criteria)

2. **_build_qi_html**: Verify completeness
   - Ensure all fields are rendered
   - Include v3.0 fields (domain_technologies, regulatory_considerations, geographic_considerations)

3. **_build_review_html**: Verify completeness
   - Ensure verdict, confidence, findings, warnings are all rendered

### ReportLab Mode Enhancements

Ensure ReportLab mode includes all v3.0 fields:

1. Add essence_derivation rendering in dao section
2. Add strategic_prohibitions rendering in fa section
3. Add cut_list, context_specific_actions, single_validation_point, exit_criteria in shu section
4. Add domain_technologies, regulatory_considerations, geographic_considerations in qi section

### HTML Fallback Mode Enhancements

Ensure HTML mode matches ReportLab mode in content:

1. Verify all v3.0 fields are rendered
2. Ensure styling matches UI (CSS classes, colors)
3. Ensure proper HTML structure (nested divs, tables)
4. Ensure all icons/emoji are included

### Backward Compatibility

Maintain compatibility with older DecisionReport versions:

1. Use `hasattr()` checks for v3.0 fields
2. Provide fallback values for missing fields
3. Handle both dict and Pydantic model inputs
4. Use `_to_dict()` helper for safe conversion

### Performance Considerations

1. PDF generation should complete in < 5 seconds for typical reports
2. HTML generation should complete in < 1 second
3. Memory usage should be reasonable (< 100MB for typical reports)
4. No memory leaks (proper cleanup of BytesIO objects)

## Deployment Considerations

### Prerequisites

- Python 3.10+
- ReportLab library (optional, for PDF mode)
- FastAPI for API endpoints
- Pydantic for data validation

### Configuration

No configuration changes required. The fix is backward compatible.

### Rollout Plan

1. Deploy code fix to staging environment
2. Run integration tests against staging
3. Verify PDF exports match UI display
4. Deploy to production
5. Monitor error logs for PDF generation failures

### Monitoring

Monitor the following metrics:

1. PDF generation success rate
2. PDF generation latency (p50, p95, p99)
3. HTML fallback usage rate
4. Error rate by error type (ValueError, RuntimeError)

### Rollback Plan

If issues are detected:

1. Revert to previous version
2. Investigate root cause
3. Fix and redeploy

The fix is low-risk as it only affects PDF generation, not core decision analysis functionality.
