***********************************************************************
**  Copyright 2025 Google LLC                                         *
**                                                                    *
**  Licensed under the Apache License, Version 2.0 (the "License");   *
**  you may not use this file except in compliance with the License.  *
**  You may obtain a copy of the License at                           *
**      https://www.apache.org/licenses/LICENSE-2.0                   *
**  Unless required by applicable law or agreed to in writing,        *
**  software distributed under the License is distributed on an       *
**  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,      *
**  either express or implied.                                        *
**  See the License for the specific language governing permissions   *
**  and limitations under the License.                                *
***********************************************************************
REPORT zr_qs_bigquery_with_pagination.

DATA:
  lv_project_id         TYPE string,
  lv_client_key         TYPE /goog/keyname,
  lv_max_wait_times     TYPE i,
  lv_job_state          TYPE string,
  lv_page_token         TYPE string,
  lv_err_message        TYPE string,
  lv_row_count          TYPE i,
  lv_job_id             TYPE string,
  lr_all_rows           TYPE REF TO data,
  ls_input              TYPE /goog/cl_bigquery_v2=>ty_103,
  ls_query_request      TYPE /goog/cl_bigquery_v2=>ty_103,
  ls_query_response     TYPE /goog/cl_bigquery_v2=>ty_104,
  ls_get_results_output TYPE /goog/cl_bigquery_v2=>ty_056.

FIELD-SYMBOLS:
  <fs_any>              TYPE any,
  <lt_all_rows>         TYPE ANY TABLE,
  <lt_rows_from_output> TYPE ANY TABLE.

CONSTANTS:
  lc_newline TYPE c VALUE cl_abap_char_utilities=>newline.

"TODO: Set Client Key that you have configured in /GOOG/CLIENT_KEY table
"lv_client_key = ' '.

"TODO: Set the maximum times polling should be done to get job status
"lv_max_wait_times = 100.

TRY.
    "Initialize Bigquery object, pass the client key name that you have configured in /GOOG/CLIENT_KEY table
    DATA(lo_bq) = NEW /goog/cl_bigquery_v2( iv_key_name = lv_client_key ).

    "Populate relevant parameters
    lv_project_id = lo_bq->gv_project_id.

    ls_input-default_dataset-project_id = 'bigquery-public-data'.     "Project ID that contains the public datasets
    ls_input-default_dataset-dataset_id = 'london_bicycles'.          "Dataset ID

    "This query fetches the data of number of rides and average durations by day of week?
    ls_input-query = | SELECT rental_id, bike_id, bike_model, start_station_name, end_station_name,| && lc_newline &&
                     | FROM bigquery-public-data.london_bicycles.cycle_hire | && lc_newline &&
                     | WHERE start_date > TIMESTAMP '2023-01-01 00:00:00' LIMIT 80000; |.

    "Call API method: bigquery.jobs.query
    CALL METHOD lo_bq->query_jobs
      EXPORTING
        iv_p_projects_id = lv_project_id
        is_input         = ls_input
      IMPORTING
        es_output        = DATA(ls_response)
        ev_ret_code      = DATA(lv_ret_code)
        ev_err_text      = DATA(lv_err_text)
        es_err_resp      = DATA(ls_err_resp).

    IF lo_bq->is_error( lv_ret_code ).
      cl_demo_output=>display( |Error submitting query: { lv_err_text }| ).
      RETURN.
    ENDIF.
    "Check if the job status is complete
    IF ls_response-job_complete = abap_true.
      "Check if results are paginated
      IF ls_response-page_token IS INITIAL.
        ASSIGN COMPONENT 'ROWS' OF STRUCTURE ls_response TO <lt_rows_from_output>.

        IF <lt_rows_from_output> IS ASSIGNED AND <lt_rows_from_output> IS NOT INITIAL.
          IF <lt_all_rows> IS NOT ASSIGNED.
            CREATE DATA lr_all_rows LIKE <lt_rows_from_output>.
            ASSIGN lr_all_rows->* TO <lt_all_rows>.
          ENDIF.

          INSERT LINES OF <lt_rows_from_output> INTO TABLE <lt_all_rows>.
          lv_row_count = lines( <lt_all_rows> ).
          cl_demo_output=>write( |=> Fetched all results. Total rows: { lv_row_count }| ).
        ENDIF.
        cl_demo_output=>display( ).
        RETURN.
      ELSE.
        lv_job_state = 'DONE'.
        lv_job_id = ls_response-query_id.
      ENDIF.
    ELSE.

      lv_job_id = ls_query_response-job_reference-job_id.
      cl_demo_output=>write( |=> Query job submitted successfully. Job ID: { lv_job_id }| ).

      cl_demo_output=>write( '|=>Polling for job completion...' ).

      DO lv_max_wait_times TIMES.
        WAIT UP TO 5 SECONDS.
        CALL METHOD lo_bq->get_jobs
          EXPORTING
            iv_p_projects_id = lv_project_id
            iv_p_jobs_id     = lv_job_id
          IMPORTING
            es_output        = DATA(ls_job_get_response)
            ev_ret_code      = lv_ret_code
            ev_err_text      = lv_err_text.

        IF lo_bq->is_error( lv_ret_code ).
          cl_demo_output=>display( |Error polling job status: { lv_err_text }| ).
          RETURN.
        ENDIF.

        " Get the job status from the correctly typed response structure
        lv_job_state = ls_job_get_response-status-state.
        IF lv_job_state = 'DONE'.
          cl_demo_output=>write( '=> Job completed.' ).
          EXIT.  "Exit the loop
        ELSE.
          cl_demo_output=>write( | Current state: { lv_job_state }| ).
        ENDIF.
      ENDDO.
    ENDIF.

    IF lv_job_state NE 'DONE'.
      cl_demo_output=>write( |=>Query is taking longer to execute, check later| ).
      RETURN.
    ENDIF.

    "Fetching all paginated results...
    DO.
      CALL METHOD lo_bq->get_query_results_jobs
        EXPORTING
          iv_p_project_id = lv_project_id
          iv_p_job_id     = lv_job_id
          iv_q_pagetoken  = lv_page_token
        IMPORTING
          es_output       = ls_get_results_output
          ev_ret_code     = lv_ret_code
          ev_err_text     = lv_err_text.

      IF lo_bq->is_error( lv_ret_code ).
        cl_demo_output=>display( |Error fetching results: { lv_err_text }| ).
        RETURN.
      ENDIF.

      lv_page_token = ls_get_results_output-page_token.

      ASSIGN COMPONENT 'ROWS' OF STRUCTURE ls_get_results_output TO <lt_rows_from_output>.
      IF <lt_rows_from_output> IS ASSIGNED AND <lt_rows_from_output> IS NOT INITIAL.
        IF <lt_all_rows> IS NOT ASSIGNED.
          CREATE DATA lr_all_rows LIKE <lt_rows_from_output>.
          ASSIGN lr_all_rows->* TO <lt_all_rows>.
        ENDIF.

        INSERT LINES OF <lt_rows_from_output> INTO TABLE <lt_all_rows>.
        lv_row_count = lines( <lt_all_rows> ).
        cl_demo_output=>write( |=> Fetched page { sy-index }. Total rows: { lv_row_count }| ).
      ENDIF.

      IF lv_page_token IS INITIAL.
        EXIT.
      ENDIF.
    ENDDO.
    cl_demo_output=>display( |SUCCESS. Total rows retrieved: { lv_row_count }| ).

    "Close HTTP Connection
    lo_bq->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
