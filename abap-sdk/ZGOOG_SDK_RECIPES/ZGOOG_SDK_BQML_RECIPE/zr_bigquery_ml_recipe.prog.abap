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
REPORT zr_bigquery_ml_recipe.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_dset  TYPE string DEFAULT 'BQML_DEMO',
              p_table TYPE string DEFAULT 'SAP_TIMESERIES_DATA',
              p_model TYPE string DEFAULT 'ARIMA_PLUS_DEMO'.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.

  PARAMETERS:
    p_key   TYPE /goog/client_key-name OBLIGATORY DEFAULT 'ABAP_SDK_DEV',
    rb_crea RADIOBUTTON GROUP g1,
    rb_load RADIOBUTTON GROUP g1,
    rb_ml   RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b2.

CLASS lcl_main DEFINITION FINAL.

  PUBLIC SECTION.

    CONSTANTS:
      c_nl TYPE c VALUE cl_abap_char_utilities=>newline.

    TYPES:
      BEGIN OF t_timeseries,
        date  TYPE string,   "YYYY-MM-DD
        value TYPE string,   "Numeric value
      END OF t_timeseries,

      tt_timeseries TYPE STANDARD TABLE OF t_timeseries WITH EMPTY KEY.

    TYPES:
      BEGIN OF t_prediction,
        forecast_timestamp_unix TYPE string,
        forecast_value          TYPE string,
        standard_error          TYPE string,
        confidence_level        TYPE string,
        prediction_lower_bound  TYPE string,
        prediction_upper_bound  TYPE string,
        confidence_lower_bound  TYPE string,
        confidence_upper_bound  TYPE string,
      END OF t_prediction,

      tt_prediction TYPE STANDARD TABLE OF t_prediction WITH EMPTY KEY.

    CLASS-METHODS: process.

  PRIVATE SECTION.
    CLASS-METHODS:
      bq_client RETURNING VALUE(ro_bq) TYPE REF TO /goog/cl_bigquery_v2,
      handle_create,
      handle_load_train,
      handle_ml,
      get_timeseries_data RETURNING VALUE(rt_data) TYPE tt_timeseries,

      get_date IMPORTING iv_date        TYPE string
               RETURNING VALUE(rv_date) TYPE string.

ENDCLASS.

START-OF-SELECTION.
  lcl_main=>process( ).

CLASS lcl_main IMPLEMENTATION.

  METHOD process.

    CASE abap_true.
      WHEN rb_crea.
        handle_create( ).
      WHEN rb_load.
        handle_load_train( ).
      WHEN rb_ml.
        handle_ml( ).
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

  METHOD bq_client.

    TRY.
        ro_bq = NEW /goog/cl_bigquery_v2( iv_key_name = p_key ).
      CATCH /goog/cx_sdk INTO DATA(lo_ex).
        MESSAGE lo_ex->get_text( ) TYPE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD handle_create.

    DATA(lo_client) = bq_client( ).

    TRY.

        lo_client->insert_datasets(
          EXPORTING
            iv_p_projects_id = CONV #( lo_client->gv_project_id )
            is_input         = VALUE #( dataset_reference =
                                   VALUE #( dataset_id = p_dset
                                            project_id = lo_client->gv_project_id ) )
          IMPORTING
            ev_ret_code      = DATA(lv_ret_code)
            ev_err_text      = DATA(lv_err_text) ).

        IF lo_client->is_success( lv_ret_code ).
          WRITE: / |Dataset { p_dset } created|.
        ELSE.
          MESSAGE lv_err_text TYPE 'E'.
        ENDIF.


        DATA: ls_input TYPE /goog/cl_bigquery_v2=>ty_131.

        ls_input-id = p_table.
        ls_input-table_reference-dataset_id = p_dset.
        ls_input-table_reference-project_id = lo_client->gv_project_id.
        ls_input-table_reference-table_id = p_table.

        ls_input-schema-fields = VALUE #( ( name = 'date' type = 'DATE' )
                                          ( name = 'value' type = 'NUMERIC' ) ).

        lo_client->insert_tables(
          EXPORTING
            iv_p_datasets_id = p_dset
            iv_p_projects_id = CONV #( lo_client->gv_project_id )
            is_input         = ls_input
          IMPORTING
            ev_ret_code      = lv_ret_code
            ev_err_text      = lv_err_text ).

        IF lo_client->is_success( lv_ret_code ) = abap_true.
          WRITE: / |Table { p_table } created|.
        ELSE.
          MESSAGE lv_err_text TYPE 'E'.
        ENDIF.




      CATCH /goog/cx_sdk INTO DATA(lo_ex).
        MESSAGE lo_ex->get_text( ) TYPE 'E'.
    ENDTRY.


  ENDMETHOD.

  METHOD handle_load_train.

    DATA(lo_client) = bq_client( ).

    " TODO
    " For the purposes of this demo, The methpd get_timeseries_data
    " populates a random number along with the date into lt_data
    " For actual enterprise scenarios, you'd populate lt_data with
    " real business timeseries data such as total quanity sold per day
    DATA(lt_data) = get_timeseries_data( ).

    TRY.
        DATA(ls_input_load) = VALUE /goog/cl_bigquery_v2=>ty_133( ).

        LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<ls_data>).
          APPEND INITIAL LINE TO ls_input_load-rows ASSIGNING FIELD-SYMBOL(<ls_row>).
          CREATE DATA <ls_row>-json TYPE t_timeseries.
          FIELD-SYMBOLS: <lfs_json> TYPE t_timeseries.
          ASSIGN <ls_row>-json->* TO <lfs_json> CASTING.
          <lfs_json> = <ls_data>.
        ENDLOOP.

        "Call API method: bigquery.tabledata.insertAll
        CALL METHOD lo_client->insert_all_tabledata
          EXPORTING
            iv_p_dataset_id = p_dset
            iv_p_project_id = CONV #( lo_client->gv_project_id )
            iv_p_table_id   = p_table
            is_input        = ls_input_load
          IMPORTING
            ev_ret_code     = DATA(lv_ret_code)
            ev_err_text     = DATA(lv_err_text).

        IF lo_client->is_success( lv_ret_code ) = abap_true.
          WRITE: / |Records inserted into table { p_table }|.
        ELSE.
          MESSAGE lv_err_text TYPE 'E'.
        ENDIF.



        DATA(ls_input_query) = VALUE /goog/cl_bigquery_v2=>ty_103( ).

        ls_input_query-query =
           | CREATE OR REPLACE MODEL `{ lo_client->gv_project_id }.{ p_dset }.{ p_model }` |  && c_nl &&
           |   OPTIONS |  && c_nl &&
           |     (model_type = 'ARIMA_PLUS', |  && c_nl &&
           |    time_series_timestamp_col = 'parsed_timestamp', |  && c_nl &&
           |    time_series_data_col = 'value', |  && c_nl &&
           |      auto_arima = TRUE, |  && c_nl &&
           |      data_frequency = 'AUTO_FREQUENCY', |  && c_nl &&
           |      decompose_time_series = TRUE |  && c_nl &&
           |        ) AS |  && c_nl &&
           |     SELECT |  && c_nl &&
           |       TIMESTAMP(date) AS parsed_timestamp, |  && c_nl &&
           |       value |  && c_nl &&
           |    FROM |  && c_nl &&
           |      `{ lo_client->gv_project_id }.{ p_dset }.{ p_table }` |.

        lo_client->query_jobs(
          EXPORTING
            iv_p_projects_id = CONV #( lo_client->gv_project_id )
            is_input         = ls_input_query
          IMPORTING
            ev_ret_code      = lv_ret_code
            ev_err_text      = lv_err_text ).

        IF lo_client->is_success( lv_ret_code ) = abap_true.
          WRITE: / |Model { p_model } created|.
        ELSE.
          MESSAGE lv_err_text TYPE 'E'.
        ENDIF.
      CATCH /goog/cx_sdk INTO DATA(lo_ex).
        MESSAGE lo_ex->get_text( ) TYPE 'E'.
    ENDTRY.


  ENDMETHOD.

  METHOD handle_ml.

    DATA(lo_client) = bq_client( ).

    DATA(ls_input_query) = VALUE /goog/cl_bigquery_v2=>ty_103( ).

    ls_input_query-query =
     | SELECT |  && c_nl &&
     | * |  && c_nl &&
     | FROM |  && c_nl &&
     | ML.FORECAST(MODEL `{ lo_client->gv_project_id }.{ p_dset }.{ p_model }`,  |  && c_nl &&
     |  STRUCT(60 AS horizon, 0.8 AS confidence_level));  |.


    TRY.
        lo_client->query_jobs(
          EXPORTING
            iv_p_projects_id = CONV #( lo_client->gv_project_id )
            is_input         = ls_input_query
          IMPORTING
            es_output        = DATA(ls_output)
            ev_ret_code      = DATA(lv_ret_code)
            ev_err_text      = DATA(lv_err_text) ).

        IF lo_client->is_success( lv_ret_code ) <> abap_true.
          MESSAGE lv_err_text TYPE 'E'.
        ENDIF.
      CATCH /goog/cx_sdk INTO DATA(lo_ex).
        MESSAGE lo_ex->get_text( ) TYPE 'E'.
    ENDTRY.

    FIELD-SYMBOLS: <ls_field> TYPE /goog/cl_bigquery_v2=>ty_132.

    DATA(lt_prediction) = VALUE tt_prediction( ).
    LOOP AT ls_output-rows REFERENCE INTO DATA(ls_row).

      DATA(ls_prediction) = VALUE t_prediction( ).
      LOOP AT ls_row->f ASSIGNING <ls_field> .
        DATA(lv_index) = sy-tabix.

        ASSIGN COMPONENT lv_index OF STRUCTURE ls_prediction
            TO FIELD-SYMBOL(<ls_target>).
        <ls_target> = <ls_field>-v->*.
      ENDLOOP.

      APPEND ls_prediction TO lt_prediction.
    ENDLOOP.

    cl_demo_output=>display( lt_prediction ).

  ENDMETHOD.

  METHOD get_timeseries_data.

    DATA(lv_date) = sy-datum.
    lv_date = lv_date - 90.

    DATA(lo_random) = cl_abap_random_int=>create( seed = CONV i( sy-uzeit )
                                                                  min  = 5000
                                                                  max  = 10000 ).
    DO 90 TIMES.

      APPEND VALUE #( date = get_date( CONV #( lv_date ) )
                      value = lo_random->get_next( )  )
                      TO rt_data.

      lv_date  = lv_date + 1.
    ENDDO.

  ENDMETHOD.

  METHOD get_date.

    rv_date = iv_date(4) && '-' && iv_date+4(2) && '-' && iv_date+6(2).

  ENDMETHOD.

ENDCLASS.
