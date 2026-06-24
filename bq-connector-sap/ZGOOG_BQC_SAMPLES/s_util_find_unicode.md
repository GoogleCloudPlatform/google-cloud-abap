# SAP ABAP Utility: Unicode & Non-Printable Character Finder

This repository contains a generic ABAP utility to identify non-printable or problematic Unicode characters in SAP database tables.

## Program: `ZR_UTIL_FIND_UNICODE`

### Purpose

During data replication from SAP to Google Cloud BigQuery, records containing non-printable characters or invalid Unicode sequences (e.g., UTF-8 non-characters like U+FFFE or U+FFFF) can cause ingestion failures or data corruption.

This utility allows you to scan any database table or view to proactively identify these problematic characters and locate the specific records (by key fields) that need correction.

---

### Selection Screen Parameters

*   **Table Name (`p_table`)**: The technical name of the database table or view you want to scan. This field is **OBLIGATORY**.
*   **Scan Non-Printable Chars (`p_nonprt`)**: Radio button. If selected, the program scans for any character that is not defined as printable (regex: `[^[:print:]]`).
*   **Scan Non-Unicode Chars (`p_nonuni`)**: Radio button (Default). If selected, the program scans specifically for problematic Unicode non-characters (UTF-8 hex `EFBFBEEFBFBF` representing U+FFFE and U+FFFF).

---

### Key Features

1.  **Generic Table Support**: Works dynamically with any transparent table or database view.
2.  **Safety & Validation**:
    *   Verifies that the input table name is valid and exists in the system (prevents SQL injection).
    *   Performs authority checks (`VIEW_AUTHORITY_CHECK`) to ensure the executing user has authorization to view the table data.
3.  **Performance Optimized**:
    *   Uses a database cursor (`OPEN CURSOR`) to fetch data in packages (default package size: 10,000 rows) to avoid memory overflows on large tables.
    *   Only scans character-like fields (`CHAR` and `STRING` types), ignoring numeric or binary fields to optimize speed.
4.  **Actionable Results**:
    *   Outputs the field name and the actual value where the non-compliant character was found.
    *   Dynamically retrieves and prints the key fields of the offending record so you can easily locate and fix the data in the source system.
