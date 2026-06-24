*&---------------------------------------------------------------------*
*& Report ZR_UTIL_FIND_NON_UNICODE
*&---------------------------------------------------------------------*
*& Description: Generic utility to find non-printable or problematic
*& Unicode characters in a table.
*&---------------------------------------------------------------------*
REPORT zr_util_find_non_unicode.

* Selection Screen (Variables g_title, lbl_tab, etc. are implicitly declared by screen processor)
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE g_title.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) lbl_tab FOR FIELD p_table.
PARAMETERS: p_table TYPE tabname OBLIGATORY.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_nonprt RADIOBUTTON GROUP grp1.
SELECTION-SCREEN COMMENT 5(30) lbl_prt FOR FIELD p_nonprt.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_nonuni RADIOBUTTON GROUP grp1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 5(30) lbl_uni FOR FIELD p_nonuni.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

* Global Data
DATA: lo_descr          TYPE REF TO cl_abap_structdescr,
      lt_keys           TYPE STANDARD TABLE OF fieldname,
      lv_regex_active   TYPE string,
      lv_table_upper    TYPE tabname,
      lv_xstr           TYPE xstring,
      lv_uni_chars      TYPE string,
      lo_conv           TYPE REF TO cl_abap_conv_in_ce,
      lr_table          TYPE REF TO data,
      lr_line           TYPE REF TO data,
      ls_cursor          TYPE cursor,
      lt_comp           TYPE abap_compdescr_tab,
      ls_comp           TYPE abap_compdescr,
      lt_match          TYPE match_result_tab,
      lv_found          TYPE abap_bool,
      lv_key            TYPE fieldname.

* Constants
CONSTANTS: lc_package_size TYPE i VALUE 10000,
           lc_regex_nonprt TYPE string VALUE '[^[:print:]]'.
* Field Symbols
FIELD-SYMBOLS: <lt_data>   TYPE STANDARD TABLE,
               <ls_data>   TYPE any,
               <fs_val>    TYPE any,
               <fs_keyval> TYPE any.

INITIALIZATION.
  g_title = 'Scan Mode'.
  lbl_tab = 'Table Name'.
  lbl_prt = 'Scan Non-Printable Chars'.
  lbl_uni = 'Scan Non-Unicode Chars'.

START-OF-SELECTION.

  " Convert table name to uppercase to ensure consistency
  lv_table_upper = p_table.
  TRANSLATE lv_table_upper TO UPPER CASE.

  IF p_nonprt = abap_true.
    lv_regex_active = lc_regex_nonprt.
  ELSE.
    " Hex values in UTF-8 for: U+FFFE U+FFFF (Non-characters)
    lv_xstr = 'EFBFBEEFBFBF'.

    TRY.
        lo_conv = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).
        lo_conv->convert( EXPORTING input = lv_xstr IMPORTING data = lv_uni_chars ).
        lv_regex_active = '[' && lv_uni_chars && ']'.
      CATCH cx_root.
        " Fallback if conversion fails (no search in this mode)
        CLEAR lv_regex_active.
    ENDTRY.
  ENDIF.

  " Validate Table Name
  TRY.
      cl_abap_dyn_prg=>check_table_name_str(
        val      = lv_table_upper
        packages = '' ).
    CATCH cx_abap_not_in_package.
      " Table exists and name is valid, but not in empty package (expected)
    CATCH cx_abap_not_a_table cx_abap_invalid_name.
      WRITE: / 'Input is not a database table or view:', lv_table_upper.
      RETURN.
    CATCH cx_root.
      WRITE: / 'Input is not a database table or view:', lv_table_upper.
      RETURN.
  ENDTRY.

  TRY.
      lo_descr ?= cl_abap_typedescr=>describe_by_name( lv_table_upper ).
    CATCH cx_root.
      WRITE: / 'Invalid table name:', lv_table_upper.
      RETURN.
  ENDTRY.

  IF lo_descr IS NOT BOUND.
    WRITE: / 'Invalid table name:', lv_table_upper.
    RETURN.
  ENDIF.

  IF lo_descr->kind <> cl_abap_typedescr=>kind_struct.
    WRITE: / 'Input is not a structure/table:', lv_table_upper.
    RETURN.
  ENDIF.

  " Authority Check
  CALL FUNCTION 'VIEW_AUTHORITY_CHECK'
    EXPORTING
      view_action                    = 'S'
      view_name                      = lv_table_upper
      no_warning_for_clientindep     = 'X'
    EXCEPTIONS
      invalid_action                 = 1
      no_authority                   = 2
      no_clientindependent_authority = 3
      table_not_found                = 4
      no_linedependent_authority     = 5
      OTHERS                         = 6.
  IF sy-subrc <> 0.
    WRITE: / 'No authority to view table', lv_table_upper.
    RETURN.
  ENDIF.

  " Get Key Fields from DD03L
  SELECT fieldname
    FROM dd03l
    INTO TABLE lt_keys
    WHERE tabname = lv_table_upper
      AND keyflag = 'X'.
  IF sy-subrc <> 0.
    WRITE: / 'Warning: No key fields found for table', lv_table_upper.
  ENDIF.

  IF lv_regex_active IS INITIAL.
    WRITE: / 'Error: Active regex is empty. Cannot perform scan.'.
    RETURN.
  ENDIF.

  TRY.
      CREATE DATA lr_table TYPE STANDARD TABLE OF (lv_table_upper).
      ASSIGN lr_table->* TO <lt_data>.

      CREATE DATA lr_line TYPE (lv_table_upper).
      ASSIGN lr_line->* TO <ls_data>.
    CATCH cx_sy_create_data_error.
      WRITE: / 'Error creating dynamic data structures for', lv_table_upper.
      RETURN.
  ENDTRY.

  lt_comp = lo_descr->components.

  TRY.
      OPEN CURSOR WITH HOLD ls_cursor FOR
        SELECT * FROM (lv_table_upper).
    CATCH cx_sy_dynamic_osql_semantics.
      WRITE: / 'Database error opening cursor for', lv_table_upper.
      RETURN.
  ENDTRY.

  WRITE: / 'Scanning table:', lv_table_upper.
  WRITE: / '---------------------------------------------'.

  DO.
    FETCH NEXT CURSOR ls_cursor INTO TABLE <lt_data> PACKAGE SIZE lc_package_size.
    IF sy-subrc <> 0.
      CLOSE CURSOR ls_cursor.
      EXIT.
    ENDIF.

    LOOP AT <lt_data> INTO <ls_data>.
      LOOP AT lt_comp INTO ls_comp.
        " Only check character-like fields
        IF ls_comp-type_kind = cl_abap_typedescr=>typekind_char OR
           ls_comp-type_kind = cl_abap_typedescr=>typekind_string.

          ASSIGN COMPONENT ls_comp-name OF STRUCTURE <ls_data> TO <fs_val>.
          IF sy-subrc = 0 AND <fs_val> IS ASSIGNED AND <fs_val> IS NOT INITIAL.
            " Find non-printable or problematic Unicode characters based on selection
            FIND ALL OCCURRENCES OF REGEX lv_regex_active IN <fs_val> RESULTS lt_match.

            IF sy-subrc = 0.
              lv_found = abap_true.
              WRITE: / 'Non-compliant character found in Field:', ls_comp-name.
              WRITE: / 'Value:', <fs_val>.
              WRITE: / 'Key Fields:'.
              LOOP AT lt_keys INTO lv_key.
                ASSIGN COMPONENT lv_key OF STRUCTURE <ls_data> TO <fs_keyval>.
                IF sy-subrc = 0.
                  WRITE: lv_key, '=', <fs_keyval>.
                ENDIF.
              ENDLOOP.
              WRITE: / '---------------------------------------------'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDDO.

  IF lv_found = abap_false.
    WRITE: / 'No matching characters found in table', lv_table_upper.
  ENDIF.

  WRITE: / 'Execution Completed'.
