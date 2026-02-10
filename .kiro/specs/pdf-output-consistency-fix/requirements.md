# Requirements Document

## Introduction

This document specifies the requirements for fixing the PDF output consistency issue in the decision_governance_engine application. The current PDF export functionality produces output that does not match the content and styling displayed in the UI, resulting in incomplete reports for users.

## Glossary

- **PDF_Generator**: The service responsible for generating PDF reports from DecisionReport objects
- **ReportLab_Mode**: PDF generation using the ReportLab library for true PDF output
- **HTML_Fallback_Mode**: PDF generation using HTML format when ReportLab is unavailable
- **UI_Display**: The React frontend component (ReportPage) that renders the decision report
- **DecisionReport**: The data model containing all sections (道/法/術/器/検証) of the analysis
- **Section**: A major component of the report (dao, fa, shu, qi, review, executive_summary)
- **Content_Parity**: The state where PDF output contains the same data as UI display

## Requirements

### Requirement 1: Complete Section Coverage

**User Story:** As a user, I want the PDF export to include all sections visible in the UI, so that I receive a complete report.

#### Acceptance Criteria

1. WHEN a PDF is generated, THE PDF_Generator SHALL include the executive summary section with all fields
2. WHEN a PDF is generated, THE PDF_Generator SHALL include the 道 (dao) section with essence derivation, alternatives, constraints, assumptions, causal gears, and death traps
3. WHEN a PDF is generated, THE PDF_Generator SHALL include the 法 (fa) section with strategic prohibitions, differentiation axis, recommended paths, and rejected paths
4. WHEN a PDF is generated, THE PDF_Generator SHALL include the 術 (shu) section with first action, cut list, context-specific actions, validation point, exit criteria, and phases
5. WHEN a PDF is generated, THE PDF_Generator SHALL include the 器 (qi) section with domain technologies, regulatory considerations, geographic considerations, and implementations
6. WHEN a PDF is generated, THE PDF_Generator SHALL include the 検証 (review) section with verdict, confidence score, findings, and warnings

### Requirement 2: Data Field Completeness

**User Story:** As a user, I want all data fields from the UI to appear in the PDF, so that no information is lost during export.

#### Acceptance Criteria

1. FOR ALL fields displayed in the UI_Display, THE PDF_Generator SHALL include those fields in the PDF output
2. WHEN a field has a value in the DecisionReport, THE PDF_Generator SHALL render that value in the PDF
3. WHEN a field is null or undefined, THE PDF_Generator SHALL handle it gracefully with appropriate fallback text
4. WHEN v3.0 fields (essence_statement, strategic_prohibition_summary, exit_criteria_summary) exist, THE PDF_Generator SHALL include them in the output

### Requirement 3: HTML Fallback Mode Completeness

**User Story:** As a user, I want the HTML fallback mode to produce complete output, so that I get full reports even without ReportLab installed.

#### Acceptance Criteria

1. WHEN HTML_Fallback_Mode is used, THE PDF_Generator SHALL generate complete HTML for all sections
2. WHEN the _build_shu_html method is called, THE PDF_Generator SHALL return complete HTML without truncation
3. WHEN the _build_qi_html method is called, THE PDF_Generator SHALL return complete HTML without truncation
4. WHEN the _build_review_html method is called, THE PDF_Generator SHALL return complete HTML without truncation
5. FOR ALL section builder methods, THE PDF_Generator SHALL complete execution without premature termination

### Requirement 4: Styling Consistency

**User Story:** As a user, I want the PDF styling to match or closely approximate the UI styling, so that the report maintains its professional appearance.

#### Acceptance Criteria

1. WHEN a section uses color coding in the UI (success/warning/prohibition), THE PDF_Generator SHALL apply equivalent styling in the PDF
2. WHEN the UI displays hierarchical information, THE PDF_Generator SHALL preserve that hierarchy in the PDF layout
3. WHEN the UI uses icons or visual indicators, THE PDF_Generator SHALL include equivalent visual cues in the PDF
4. WHEN tables are displayed in the UI, THE PDF_Generator SHALL render them as properly formatted tables in the PDF

### Requirement 5: ReportLab Mode Completeness

**User Story:** As a user, I want the ReportLab PDF mode to produce complete output, so that I get properly formatted PDF files.

#### Acceptance Criteria

1. WHEN ReportLab_Mode is used, THE PDF_Generator SHALL generate complete PDF content for all sections
2. WHEN CJK fonts are required, THE PDF_Generator SHALL register and use appropriate fonts for Japanese text
3. WHEN building PDF elements, THE PDF_Generator SHALL include all data fields from the DecisionReport
4. WHEN page breaks are needed, THE PDF_Generator SHALL insert them at appropriate section boundaries

### Requirement 6: Error Handling and Validation

**User Story:** As a developer, I want proper error handling during PDF generation, so that failures are logged and reported clearly.

#### Acceptance Criteria

1. WHEN PDF generation fails, THE PDF_Generator SHALL log the error with context (report_id, section, error type)
2. WHEN a section builder method encounters an error, THE PDF_Generator SHALL raise a descriptive exception
3. WHEN input validation fails, THE PDF_Generator SHALL raise ValueError with a clear message
4. IF ReportLab is unavailable, THE PDF_Generator SHALL log a warning and fall back to HTML mode

### Requirement 7: Code Completeness

**User Story:** As a developer, I want all code methods to be complete and not truncated, so that the system functions correctly.

#### Acceptance Criteria

1. THE PDF_Generator SHALL have complete implementations of all section builder methods
2. WHEN the _build_shu_html method is defined, THE PDF_Generator SHALL include the complete method body
3. WHEN the _build_qi_html method is defined, THE PDF_Generator SHALL include the complete method body
4. WHEN the _build_review_html method is defined, THE PDF_Generator SHALL include the complete method body
5. FOR ALL methods in the PDF_Generator class, THE implementation SHALL be syntactically complete

### Requirement 8: Round-Trip Consistency

**User Story:** As a user, I want the PDF to accurately represent the data in the DecisionReport, so that exported reports are trustworthy.

#### Acceptance Criteria

1. FOR ANY DecisionReport object, generating a PDF and extracting its text content SHALL preserve all key information
2. WHEN a field exists in the DecisionReport, THE PDF_Generator SHALL include that field in the output
3. WHEN comparing UI display to PDF output, THE content SHALL be equivalent (allowing for formatting differences)
4. WHEN v3.0 fields are present, THE PDF_Generator SHALL render them in both ReportLab and HTML modes
