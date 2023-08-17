*&---------------------------------------------------------------------*
*& Report ZR_CREATE_TOPICS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zr_create_topics.

DATA:
  lv_p_projects_id TYPE string,
  lv_p_topics_id   TYPE string,
  ls_input         TYPE /goog/cl_pubsub_v1=>ty_041.

TRY.

* Open HTTP Connection
    DATA(lo_client) = NEW /goog/cl_pubsub_v1( iv_key_name = 'CLIENT_KEY' ).

* Populate relevant parameters
    " Derive project id from the client object
    lv_p_projects_id = lo_client->gv_project_id.
    " Name of Topic that will get created.
    lv_p_topics_id = 'SAMPLE_TOPIC'.
    " Topic Related Attributes can be provided in LS_INPUT.
    ls_input-message_retention_duration = '86400s'.

* Call API method
    CALL METHOD lo_client->create_topics
      EXPORTING
        iv_p_projects_id = lv_p_projects_id
        iv_p_topics_id   = lv_p_topics_id
        is_input         = ls_input
      IMPORTING
*       ES_RAW           =
        es_output        = DATA(ls_output)
        ev_ret_code      = DATA(lv_ret_code)
        ev_err_text      = DATA(lv_err_text)
        es_err_resp      = DATA(ls_err_resp).

    IF /goog/cl_http_client=>is_success( lv_ret_code ).
      MESSAGE 'Topic created successfully!' TYPE 'S'.
    ELSE.
      MESSAGE lv_err_text TYPE 'E'.
    ENDIF.

* Close HTTP Connection
    lo_client->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
