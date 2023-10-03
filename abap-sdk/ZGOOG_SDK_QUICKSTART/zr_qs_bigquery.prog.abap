**********************************************************************
*  Copyright 2023 Google LLC                                         *
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
REPORT zr_qs_bigquery.

TYPES:
  BEGIN OF lty_query_result,
    day_of_week     TYPE i,
    number_of_rides TYPE i,
    avg_duration    TYPE string,
  END OF lty_query_result,
  ltt_query_result TYPE STANDARD TABLE OF lty_query_result.

DATA:
  lv_project_id TYPE string,
  ls_input      TYPE /goog/cl_bigquery_v2=>ty_103,
  ls_output     TYPE lty_query_result,
  lt_output     TYPE ltt_query_result.

CONSTANTS:
  lc_newline TYPE c VALUE cl_abap_char_utilities=>newline.

TRY.
    "Initialize Bigquery object, pass the client key name that you have configured in /GOOG/CLIENT_KEY table
    DATA(lo_bq) = NEW /goog/cl_bigquery_v2( iv_key_name = 'DEMO_BQ' ).

    "Populate relevant parameters
    lv_project_id = lo_bq->gv_project_id.

    ls_input-default_dataset-project_id = 'bigquery-public-data'.     "Project ID that contains the public datasets
    ls_input-default_dataset-dataset_id = 'london_bicycles'.          "Dataset ID

    "This query fetches the data of number of rides and average durations by day of week?
    ls_input-query = | SELECT EXTRACT(DAYOFWEEK FROM start_date) AS day_of_week,| && lc_newline &&
                     | COUNT(*) number_of_rides,| && lc_newline &&
                     | AVG(duration) AS avg_duration | && lc_newline &&
                     | FROM bigquery-public-data.london_bicycles.cycle_hire | && lc_newline &&
                     | GROUP BY day_of_week ORDER BY day_of_week ASC |.

    "Call API method: bigquery.jobs.query
    CALL METHOD lo_bq->query_jobs
      EXPORTING
        iv_p_project_id = lv_project_id
        is_input        = ls_input
      IMPORTING
        es_output       = DATA(ls_response)
        ev_ret_code     = DATA(lv_ret_code)
        ev_err_text     = DATA(lv_err_text)
        es_err_resp     = DATA(ls_err_resp).

    IF lo_bq->is_success( lv_ret_code ).
      "API Call successful, loop through the data & display the result
      IF ls_response-job_complete = abap_true.
        LOOP AT ls_response-rows ASSIGNING FIELD-SYMBOL(<ls_row>).
          LOOP AT <ls_row>-f ASSIGNING FIELD-SYMBOL(<ls_value>).
            CASE sy-tabix.
              WHEN 1.
                ls_output-day_of_week = <ls_value>-v->*.
              WHEN 2.
                ls_output-number_of_rides = <ls_value>-v->*.
              WHEN 3.
                ls_output-avg_duration = <ls_value>-v->*.
            ENDCASE.
          ENDLOOP.
          APPEND ls_output TO lt_output.
          CLEAR ls_output.
        ENDLOOP.
        IF lt_output IS NOT INITIAL.
          cl_demo_output=>new( )->begin_section( 'Query Details'
                               )->write_text( ls_input-query
                               )->write_text( 'Dataset: bigquery-public-data.london_bicycles'
                               )->end_section(
                               )->begin_section( 'Query Results'
                               )->write_data( lt_output
                               )->end_section(
                               )->display( ).
        ENDIF.
      ENDIF.
    ELSE.
      "Display error message in case the API call fails
      MESSAGE lv_err_text TYPE 'E'.
    ENDIF.

    "Close HTTP Connection
    lo_bq->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
