*&---------------------------------------------------------------------*
*& Report ZR_GET_OBJECTS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zr_get_objects.

* Data Declarations
DATA:
  lv_p_bucket TYPE string,
  lv_p_object TYPE string,
  ls_data     TYPE xstring.

TRY.
*  Open HTTP Connection
    DATA(lo_client) = NEW /goog/cl_storage_v1( iv_key_name = 'CLIENT_KEY' ).

*  Populate the data that needs to be passed to the api
    "Name of the bucket
    lv_p_bucket       = 'sample-bucket'.
    "Name of the object
    lv_p_object         = 'sample-object'.

*   Set the common query parameter alt as 'media' to retrieve object data
    CALL METHOD lo_client->add_common_qparam
      EXPORTING
        iv_name  = 'alt'
        iv_value = 'media'.

*     Call API method
    CALL METHOD lo_client->get_objects
      EXPORTING
        iv_p_bucket = lv_p_bucket
        iv_p_object = lv_p_object
      IMPORTING
        es_output   = DATA(ls_output)
        ev_ret_code = DATA(lv_ret_code)
        ev_err_text = DATA(lv_err_text)
        es_err_resp = DATA(ls_err_resp)
        es_raw      = ls_data.

    IF lo_client->is_success( lv_ret_code ).
      MESSAGE 'Object data downloaded successfully!' TYPE 'S'.
    ELSE.
      MESSAGE lv_err_text TYPE 'E'.
    ENDIF.
*   Close HTTP connection
    lo_client->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
