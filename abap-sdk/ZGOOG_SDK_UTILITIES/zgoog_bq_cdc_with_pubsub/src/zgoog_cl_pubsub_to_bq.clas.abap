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
class ZGOOG_CL_PUBSUB_TO_BQ definition
  public
  final
  create public .

public section.

  types:
    tt_config type HASHED TABLE OF ZGOOG_PUBSUB_BQ WITH UNIQUE KEY datasource .

  class-methods CLASS_CONSTRUCTOR .
  class-methods SEND_TO_PUBSUB
    importing
      !IT_DATA type DATA
      !IV_DATASOURCE type ZGOOG_PUBSUB_BQ-DATASOURCE
      !IV_SET_CDC_FIELDS type FLAG optional
      !IV_CDC_FIELD type STRING optional
      !IV_CDC_DEL_VALUE type STRING optional
      !IV_CHANGE_SEQUENCE_NUMBER_FLD type STRING optional
    exporting
      !EV_RET_CODE type I
      !EV_ERR_TEXT type STRING
      !EV_ERR_RESP type /GOOG/ERR_RESP .
protected section.
private section.

  class-data GO_PUBSUB type ref to /GOOG/CL_PUBSUB_V1 .
  class-data GS_CONFIG type ZGOOG_PUBSUB_BQ .
  class-data GT_CONFIG type TT_CONFIG .
ENDCLASS.



CLASS ZGOOG_CL_PUBSUB_TO_BQ IMPLEMENTATION.


  METHOD class_constructor.

    SELECT *
      FROM ZGOOG_PUBSUB_BQ
      INTO TABLE @gt_config.

  ENDMETHOD.


  METHOD send_to_pubsub.

    " The table below shows the list of supported BQ Data types and
    " how the data in IATB IT_DATA should be formatted

    " IMPORTANT - Note special requirement for DATE, TIME and TIMESTAMP fields
    " Data must be already in BigQuery format in the itab, different from SAP data types

    "------------------------------------------------------------------------------------
    " BigQuery Data Type | Itab Field Type (format)       | Sample Values               |
    "------------------------------------------------------------------------------------
    " INTEGER            | INT1, INT2, INT4, INT8         | 0                           |
    "------------------------------------------------------------------------------------
    " FLOAT              | FLTP                           | 1.2                         |
    "-----------------------------------------------------------------------------------
    " NUMERIC            | CURR, DEC, QUAN, DECFLOAT16    | 300.45                      |
    "------------------------------------------------------------------------------------
    " STRING             | NUMC, RAW, CHAR, STRING        | This is a string            |
    "                    | RAWSTRING, LRAW                |                             |
    "------------------------------------------------------------------------------------
    " DATE               | CHAR, STRING (YYYY-MM-DD)      | 2024-04-01                  |
    "------------------------------------------------------------------------------------
    " TIME               | CHAR, STRING (HH:MM:SS)        | 23:10:09                    |
    "------------------------------------------------------------------------------------
    " TIMESTAMP          | CHAR, STRING                   |                             |
    "                    | (YYYY-MM-DDTHH:MM:SS.µµµµµµZ)  | 2024-09-14T11:00:00.123456Z |
    "------------------------------------------------------------------------------------

    IF gs_config-datasource <> iv_datasource.

      READ TABLE gt_config INTO gs_config
           WITH TABLE KEY datasource = iv_datasource.
      IF sy-subrc IS NOT INITIAL.
        ev_ret_code = 461.
        ev_err_text = 'Config not found in table ZGOOG_PUBSUB_BQ'.
        RETURN.
      ENDIF.

      DATA: lo_root TYPE REF TO cx_root.

      TRY.
          CREATE OBJECT go_pubsub
            EXPORTING
              iv_key_name = gs_config-keyname.

        CATCH cx_root INTO lo_root.
          ev_ret_code = 461.
          ev_err_text = lo_root->get_text( ).
          RETURN.
      ENDTRY.

    ENDIF.

    IF go_pubsub IS INITIAL.
      RETURN.
    ENDIF.

    DATA:
      ls_input_sap   TYPE /goog/cl_pubsub_v1=>ty_023,
      ls_message_sap TYPE /goog/cl_pubsub_v1=>ty_025,
      ls_input_bq    TYPE /goog/cl_pubsub_v1=>ty_023,
      ls_message_bq  TYPE /goog/cl_pubsub_v1=>ty_025.

    FIELD-SYMBOLS: <ls_row> TYPE any.

    DATA: lv_json_obj_sap TYPE string.

    DATA: lv_timestamp TYPE timestampl.
    GET TIME STAMP FIELD lv_timestamp.

    DATA:  lv_sequence_number TYPE string.
    lv_sequence_number = lv_timestamp.

    REPLACE ALL OCCURRENCES OF '.' IN  lv_sequence_number WITH space.
    lv_sequence_number = lv_sequence_number(19).
    CONDENSE lv_sequence_number NO-GAPS.

    LOOP AT it_data ASSIGNING <ls_row>.
      lv_json_obj_sap = /ui2/cl_json=>serialize( data = <ls_row>
                         pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

      IF iv_set_cdc_fields IS NOT INITIAL.
        IF iv_cdc_field IS INITIAL AND
           iv_cdc_del_value IS INITIAL.
          ASSIGN 'UPSERT' TO FIELD-SYMBOL(<lv_change_type>).
        ELSE.
          ASSIGN COMPONENT iv_cdc_field OF STRUCTURE <ls_row>
          TO FIELD-SYMBOL(<lv_cdc_field>).

          IF <lv_cdc_field> = iv_cdc_del_value.
            ASSIGN 'DELETE' TO <lv_change_type>.
          ELSE.
            ASSIGN 'UPSERT' TO <lv_change_type>.
          ENDIF.
        ENDIF.

        IF iv_change_sequence_number_fld IS NOT INITIAL.
          ASSIGN COMPONENT iv_change_sequence_number_fld OF STRUCTURE <ls_row>
          TO FIELD-SYMBOL(<lv_change_sequence_number>).
        ELSE.
          ASSIGN lv_sequence_number TO <lv_change_sequence_number>.
        ENDIF.


        DATA: lv_json_len TYPE i.

        lv_json_len = strlen( lv_json_obj_sap ) - 1.

        lv_json_obj_sap = lv_json_obj_sap(lv_json_len) &&
                          ',"_CHANGE_TYPE":"' &&
                          <lv_change_type> &&
                          '","_CHANGE_SEQUENCE_NUMBER":' &&
                          <lv_change_sequence_number> && '}'.

      ENDIF.

      ls_message_sap-data = cl_http_utility=>encode_base64( unencoded = lv_json_obj_sap ).
      APPEND ls_message_sap TO ls_input_sap-messages.
    ENDLOOP.


    TRY.
        CALL METHOD go_pubsub->publish_topics
          EXPORTING
            iv_p_projects_id = CONV #( go_pubsub->gv_project_id )
            iv_p_topics_id   = CONV #( gs_config-pubsub_topic )
            is_input         = ls_input_sap
          IMPORTING
            ev_ret_code      = ev_ret_code
            ev_err_text      = ev_err_text
            es_err_resp      = ev_err_resp.
      CATCH cx_root INTO lo_root.
        ev_ret_code = 461.
        ev_err_text = lo_root->get_text( ).
    ENDTRY.


  ENDMETHOD.
ENDCLASS.
