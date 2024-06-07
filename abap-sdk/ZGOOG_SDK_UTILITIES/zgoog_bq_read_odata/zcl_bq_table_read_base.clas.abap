class ZCL_BQ_TABLE_READ_BASE definition
  public
  create public .

public section.
  PROTECTED SECTION.
  METHODS select IMPORTING iv_key type /goog/keyname
                           iv_table type string
                           iv_dataset type string
                           io_request  TYPE REF TO if_rap_query_request
                           io_response TYPE REF TO if_rap_query_response
                 RAISING   cx_rap_query_prov_not_impl
                           cx_rap_query_provider.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_BQ_TABLE_READ_BASE IMPLEMENTATION.


  METHOD select.
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

    DATA(lv_top)     = io_request->get_paging( )->get_page_size( ).
    DATA(lv_skip)    = io_request->get_paging( )->get_offset( ).
    DATA(lt_requested_fields)  = io_request->get_requested_elements( ).
    DATA(lt_sort_order)    = io_request->get_sort_elements( ).

    DATA(lv_filter_condition_string) = io_request->get_filter(  )->get_as_sql_string(  ).

    DATA:
      lv_p_project_id TYPE string,
      ls_input        TYPE /goog/cl_bigquery_v2=>ty_103,
      ls_raw          TYPE REF TO data,
      ls_output       TYPE /goog/cl_bigquery_v2=>ty_104,
      lo_exception    TYPE REF TO /goog/cx_sdk.

    TRY.
        DATA(lo_bq) = NEW /goog/cl_bigquery_v2( iv_key_name = iv_key ).
      CATCH /goog/cx_sdk INTO lo_exception.
    ENDTRY.

    DATA(lv_entity) = io_request->get_entity_id( ).

    lv_p_project_id = lo_bq->gv_project_id.
    ls_input-query =
       COND #( WHEN lv_filter_condition_string IS INITIAL THEN
                |SELECT * FROM { iv_table } |
                ELSE |SELECT * FROM { iv_table } Where { lv_filter_condition_string } | ).
    ls_input-max_results = lv_top + lv_skip.
    ls_input-use_legacy_sql  = abap_false.
    ls_input-use_query_cache = abap_false.

    ls_input-default_dataset-dataset_id = iv_dataset.
    ls_input-default_dataset-project_id = lo_bq->gv_project_id.

    TRY.
        lo_bq->query_jobs(
          EXPORTING
            iv_p_projects_id = lv_p_project_id
            is_input        = ls_input
          IMPORTING
            es_output       = ls_output ).

      CATCH /goog/cx_sdk INTO lo_exception.
    ENDTRY.

    DATA: lr_result_tab TYPE REF TO data.
    DATA: lr_result_wa TYPE REF TO data.

    CREATE DATA lr_result_wa TYPE (lv_entity).
    CREATE DATA lr_result_tab TYPE STANDARD TABLE OF (lv_entity).

    FIELD-SYMBOLS: <ls_result> TYPE any.
    FIELD-SYMBOLS: <lt_result> TYPE STANDARD TABLE.

    ASSIGN lr_result_wa->* TO <ls_result>.
    ASSIGN lr_result_tab->* TO <lt_result>.

    LOOP AT ls_output-rows REFERENCE INTO DATA(ls_rows).
      CLEAR: <ls_result>.
      LOOP AT ls_rows->f ASSIGNING FIELD-SYMBOL(<ls_field>).
        DATA(lv_tabix_field) = sy-tabix.
        ASSIGN COMPONENT lv_tabix_field OF STRUCTURE <ls_result>
        TO FIELD-SYMBOL(<ls_target_field>).

        IF <ls_field>-v IS BOUND AND
           <ls_target_field> IS ASSIGNED.

          CASE cl_abap_typedescr=>describe_by_data( <ls_target_field> )->type_kind.
            WHEN cl_abap_typedescr=>typekind_date.
              IF strlen( <ls_field>-v->* ) = 10.
                <ls_target_field> = <ls_field>-v->*(4) && <ls_field>-v->*+5(2) && <ls_field>-v->*+8(2).
              ENDIF.
            WHEN cl_abap_typedescr=>typekind_time.
              IF strlen( <ls_field>-v->* ) = 8.
                <ls_target_field> = <ls_field>-v->*(2) && <ls_field>-v->*+3(2) && <ls_field>-v->*+6(2).
              ENDIF.
            WHEN cl_abap_typedescr=>typekind_utclong.
              IF strlen( <ls_field>-v->* ) > 26.
                <ls_target_field> = <ls_field>-v->*(26).
              ENDIF.
            WHEN OTHERS.
              <ls_target_field> = <ls_field>-v->*.
          ENDCASE.
        ENDIF.

      ENDLOOP.

      APPEND <ls_result> TO <lt_result>.
    ENDLOOP.

    IF lv_skip IS NOT INITIAL.
      DELETE <lt_result> TO lv_skip.
    ENDIF.

    io_response->set_total_number_of_records( lines( <lt_result> ) ).
    io_response->set_data( <lt_result> ).

  ENDMETHOD.
ENDCLASS.
