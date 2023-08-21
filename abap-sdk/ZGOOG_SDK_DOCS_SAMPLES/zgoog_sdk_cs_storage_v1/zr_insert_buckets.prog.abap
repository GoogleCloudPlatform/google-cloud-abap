*&---------------------------------------------------------------------*
*& Report ZR_INSERT_BUCKETS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zr_insert_buckets.


* Data Declarations
DATA:
  lv_q_project TYPE string,
  ls_input     TYPE /goog/cl_storage_v1=>ty_001.

TRY.
*  Open HTTP Connection
    DATA(lo_client) = NEW /goog/cl_storage_v1( iv_key_name = 'CLIENT_KEY' ).

*   Populate the data that needs to be passed to the api
    "Derive project id from the client object
    lv_q_project  = lo_client->gv_project_id.
    "Name of the bucket
    ls_input-name = 'sample-bucket'.

*   Call API method
    CALL METHOD lo_client->insert_buckets
      EXPORTING
        iv_q_project = lv_q_project
        is_input     = ls_input
      IMPORTING
        es_output    = DATA(ls_output)
        ev_ret_code  = DATA(lv_ret_code)
        ev_err_text  = DATA(lv_err_text)
        es_err_resp  = DATA(ls_err_resp).

    IF lo_client->is_success( lv_ret_code ).
      MESSAGE 'Bucket created successfully!' TYPE 'S'.
    ELSE.
      MESSAGE lv_err_text TYPE 'E'.
    ENDIF.

*   Close HTTP connection
    lo_client->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
