*&---------------------------------------------------------------------*
*& Report ZR_DELETE_SECRETS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zr_delete_secrets.


* Data declarations
DATA: lv_p_project_id TYPE string.

TRY.
*     Open HTTP Connection
    DATA(lo_sm) = NEW /goog/cl_secretmgr_v1( iv_key_name = 'client_key' ).

*     Populate inputs to be passed to the API
    lv_p_project_id = lo_sm->gv_project_id.

*     Call API method
    CALL METHOD lo_sm->delete_secrets
      EXPORTING
        iv_p_projects_id = lv_p_project_id
        iv_p_secrets_id  = 'Secret_001'
      IMPORTING
*       es_raw           =
        es_output        = DATA(ls_output)
        ev_ret_code      = DATA(lv_ret_code)
        ev_err_text      = DATA(lv_err_text)
        es_err_resp      = DATA(ls_err_resp).

    IF /goog/cl_http_client=>is_success( lv_ret_code ).
      MESSAGE 'Secret deleted successfully' TYPE 'S'.
    ELSE.
      MESSAGE lv_err_text TYPE 'E'.
    ENDIF.

*     Close HTTP Connection
    lo_sm->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
