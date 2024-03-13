**********************************************************************
*  Copyright 2024 Google LLC                                         *
*                                                                    *
*  Licensed under the Apache License, Version 2.0 (the "License");   *
*  you may not use this file except in compliance with the License.  *
*  You may obtain a copy of the License at                           *
*      https://www.apache.org/licenses/LICENSE-2.0                   *
*  Unless required by applicable law or agreed to in writing,        *
*  software distributed under the License is distributed on an       *
*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,      *
*  either express or implied.                                        *
*  See the License for the specific language governing permissions   *
*  and limitations under the License.                                *
**********************************************************************
**********************************************************************
*  REPORT zr_util_mass_convert_field_map
**********************************************************************
REPORT zr_util_mass_convert_field_map.

TABLES: /goog/bq_mastr,
        /goog/bq_table,
        /goog/bq_field.

TYPES:
  BEGIN OF t_conv_table,
    mass_tr_id TYPE /goog/mt_id,
    transf_key TYPE /goog/trkey,
    tabname_s4 TYPE /goog/tabnm_s4,
  END OF t_conv_table,

  BEGIN OF lty_op,
    transf_key TYPE /goog/trkey,
    tabname    TYPE /goog/tabnm_s4,
  END OF lty_op.

DATA: lt_tables     TYPE STANDARD TABLE OF t_conv_table WITH DEFAULT KEY,
      lt_bq_field   TYPE STANDARD TABLE OF /goog/bq_field WITH DEFAULT KEY,
      ls_tables     TYPE t_conv_table,
      ls_output     TYPE lty_op,
      lt_output     TYPE STANDARD TABLE OF lty_op,
      ls_bq_field   TYPE /goog/bq_field,
      lo_slt_config TYPE REF TO /goog/cl_slt_config,
      lt_dd03p      TYPE dmc_dd03p_tab,
      lv_enque_name TYPE enqu_name,
      ls_dd03p      TYPE dd03p.


FIELD-SYMBOLS:  <lr_bq_field> TYPE  /goog/bq_field.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
  PARAMETERS :     p_trkey TYPE /goog/bq_mastr-transf_key MATCHCODE OBJECT /goog/sh_transf_key OBLIGATORY.
  SELECT-OPTIONS:  s_table FOR /goog/bq_table-tabname_s4 MATCHCODE OBJECT /goog/sh_tabname_s4 NO INTERVALS.
  PARAMETERS :     p_tr_typ TYPE /goog/bq_field-rolname_ex DEFAULT 'STRING' MODIF ID ro.
  SELECTION-SCREEN COMMENT /1(60) comm1 MODIF ID mg1.
  SELECTION-SCREEN COMMENT /1(60) comm2 MODIF ID mg1.
  SELECTION-SCREEN COMMENT /1(70) comm3 MODIF ID mg1.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.

  comm1 = 'Be cautious while using this program!!'.
  comm2 = 'As it can impact current tables in load or replication'.
  comm3 = 'Use it only for tables for which load or replication has not started'.

  LOOP AT SCREEN.
    IF screen-group1 = 'RO'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.

    IF screen-group1 = 'MG1'.
      screen-intensified = '1'.
      screen-color = '0' .
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

START-OF-SELECTION.

  SELECT a~mass_tr_id
  a~transf_key
  b~tabname_s4
  FROM /goog/bq_mastr AS a INNER JOIN
  /goog/bq_table AS b
  ON a~transf_key = b~transf_key
  INTO TABLE lt_tables
  WHERE a~transf_key = p_trkey
  AND   b~tabname_s4 IN s_table[].

  IF lt_tables IS INITIAL.
    MESSAGE ' No Table specified in the Mass transfer ID(s). Check Tcode /GOOG/SLT_SETTINGS ' TYPE 'E' DISPLAY LIKE 'I'.
    RETURN.
  ENDIF.

  SELECT *
  FROM /goog/bq_field
  INTO TABLE lt_bq_field
  FOR ALL ENTRIES IN lt_tables
  WHERE transf_key = lt_tables-transf_key
  AND   tabname_s4 = lt_tables-tabname_s4.

  ls_bq_field-changed_by = sy-uname.
  ls_bq_field-changed_on = sy-datum.
  ls_bq_field-changed_at = sy-uzeit.

  LOOP AT lt_tables INTO ls_tables.
    READ TABLE lt_bq_field WITH KEY transf_key = ls_tables-transf_key tabname_s4 = ls_tables-tabname_s4 TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      LOOP AT lt_bq_field ASSIGNING <lr_bq_field> WHERE transf_key = ls_tables-transf_key AND tabname_s4 = ls_tables-tabname_s4.
        <lr_bq_field>-rolname_ex = /goog/cl_slt_utility=>mc_rolnm_string.
        <lr_bq_field>-changed_by = ls_bq_field-changed_by.
        <lr_bq_field>-changed_on = ls_bq_field-changed_on.
        <lr_bq_field>-changed_at = ls_bq_field-changed_at.
      ENDLOOP.
    ELSE.
      TRY.
          CREATE OBJECT lo_slt_config
            EXPORTING
              iv_mass_tr_id = ls_tables-mass_tr_id.
        CATCH cx_iuuc_repl_configuration .
      ENDTRY.


      CALL METHOD lo_slt_config->get_source_fields
        EXPORTING
          iv_tabname_s4 = ls_tables-tabname_s4
        RECEIVING
          rt_dd03p      = lt_dd03p.

      LOOP AT lt_dd03p INTO ls_dd03p.

        ls_bq_field-transf_key = ls_tables-transf_key.
        ls_bq_field-tabname_s4 = ls_tables-tabname_s4.
        ls_bq_field-fldname_s4 = ls_dd03p-fieldname.
        ls_bq_field-rolname_ex = /goog/cl_slt_utility=>mc_rolnm_string.
        ls_bq_field-fldname_ex = ls_dd03p-scrtext_m.
        ls_bq_field-fldname_ex = ls_dd03p-ddtext.
        ls_bq_field-fldname_ex = ls_dd03p-fieldname.
        ls_bq_field-field_desc =  ls_dd03p-ddtext.
        ls_bq_field-is_set_act = 'X'.

        /goog/cl_slt_utility=>prepare_text( CHANGING cv_text = ls_bq_field-fldname_ex ).

        APPEND ls_bq_field TO lt_bq_field.
      ENDLOOP.
    ENDIF.
    IF lt_bq_field IS NOT INITIAL.
      CALL FUNCTION 'ENQUEUE_/GOOG/EBQ_FIELD'
        EXPORTING
          transf_key     = ls_tables-transf_key
          tabname_s4     = ls_tables-tabname_s4
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc EQ 0.

        MODIFY /goog/bq_field FROM TABLE lt_bq_field.
        IF sy-subrc EQ 0.
          COMMIT WORK.
          DELETE lt_bq_field WHERE transf_key = ls_tables-transf_key AND tabname_s4 = ls_tables-tabname_s4.
          ls_output-tabname = ls_tables-tabname_s4.
          ls_output-transf_key = ls_tables-transf_key.
          APPEND ls_output TO lt_output.
        ELSE.
          ROLLBACK WORK.
        ENDIF.

        CALL FUNCTION 'DEQUEUE_/GOOG/EBQ_FIELD'
          EXPORTING
            transf_key = ls_tables-transf_key
            tabname_s4 = ls_tables-tabname_s4.
      ENDIF.
    ENDIF.
    CLEAR lo_slt_config.
  ENDLOOP.

  IF lt_output IS NOT INITIAL.
    DATA: lt_field_cat TYPE slis_t_fieldcat_alv,
          ls_field_cat TYPE slis_fieldcat_alv,
          ls_layout    TYPE slis_layout_alv.

    ls_field_cat-fieldname = 'TRANSF_KEY'.
    ls_field_cat-col_pos = 1.
    ls_field_cat-rollname = '/GOOG/TRKEY'.
    APPEND ls_field_cat TO lt_field_cat.

    ls_field_cat-fieldname = 'TABNAME'.
    ls_field_cat-col_pos = 2.
    ls_field_cat-rollname = '/GOOG/TABNM_S4'.
    APPEND ls_field_cat TO lt_field_cat.

    ls_layout-colwidth_optimize = 'X'.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_grid_title       = 'Mass update completed for below tables'
        i_callback_program = sy-repid
        it_fieldcat        = lt_field_cat
        is_layout          = ls_layout
      TABLES
        t_outtab           = lt_output
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


  ELSE.
    MESSAGE ' No field updated ' TYPE 'E' DISPLAY LIKE 'I'.

  ENDIF.
