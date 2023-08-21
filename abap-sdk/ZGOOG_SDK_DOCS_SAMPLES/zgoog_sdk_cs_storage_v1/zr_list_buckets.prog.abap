*&---------------------------------------------------------------------*
*& Report ZR_LIST_BUCKETS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zr_list_buckets.


* Data Declarations
DATA:
  lv_q_project    TYPE string,
  lv_q_maxresults TYPE string,
  lv_q_pagetoken  TYPE string,
  lt_buckets      TYPE TABLE OF string.

TRY.
*  Open HTTP Connection
    DATA(lo_client) = NEW /goog/cl_storage_v1( iv_key_name = 'CLIENT_KEY' ).

*  Populate the data that needs to be passed to the api
    "Derive project id from the client object
    lv_q_project    = lo_client->gv_project_id.
    "Maximum number of buckets to be retrieved in 1 API calls
    lv_q_maxresults = '50'.

    WHILE sy-index  = 1 OR lv_q_pagetoken IS NOT INITIAL.

*     Call API method
      CALL METHOD lo_client->list_buckets
        EXPORTING
          iv_q_maxresults = lv_q_maxresults
          iv_q_pagetoken  = lv_q_pagetoken
          iv_q_project    = lv_q_project
        IMPORTING
          es_output       = DATA(ls_output)
          ev_ret_code     = DATA(lv_ret_code)
          ev_err_text     = DATA(lv_err_text)
          es_err_resp     = DATA(ls_err_resp).

      IF lo_client->is_success( lv_ret_code ).
        lt_buckets = VALUE #( FOR wa IN ls_output-items ( wa-name ) ).
      ENDIF.
      lv_q_pagetoken = ls_output-next_page_token.
    ENDWHILE.

    IF lo_client->is_success( lv_ret_code ).
      MESSAGE 'Buckets in the project were retrieved!' TYPE 'S'.
    ELSE.
      MESSAGE lv_err_text TYPE 'E'.
    ENDIF.
*   Close HTTP connection
    lo_client->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
