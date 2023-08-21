*&---------------------------------------------------------------------*
*& Report ZR_UPLOAD_FILE_BUCKET
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zr_upload_file_bucket.

* Data Declarations
DATA:
  lv_p_bucket     TYPE string,
  lv_q_name       TYPE string,
  ls_data         TYPE xstring,
  lv_content_type TYPE string.

TRY.
*  Open HTTP Connection
    DATA(lo_client) = NEW /goog/cl_storage_v1( iv_key_name = 'CLIENT_KEY' ).

*   Populate the data that needs to be passed to the api
    "Name of the bucket
    lv_p_bucket       = 'sample-bucket'.
    "Name of the object
    lv_q_name         = 'sample-object'.
    "Read object data into ls_data
    ls_data          = '<Object data in XSTRING form>'.
    "Content type of the object
    lv_content_type  = '<Content Type>'.

*   Call API method
    CALL METHOD lo_client->insert_objects
      EXPORTING
        iv_q_name       = lv_q_name
        iv_p_bucket     = lv_p_bucket
        is_data         = ls_data
        iv_content_type = lv_content_type
      IMPORTING
        es_output       = DATA(ls_output)
        ev_ret_code     = DATA(lv_ret_code)
        ev_err_text     = DATA(lv_err_text)
        es_err_resp     = DATA(ls_err_resp).

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
