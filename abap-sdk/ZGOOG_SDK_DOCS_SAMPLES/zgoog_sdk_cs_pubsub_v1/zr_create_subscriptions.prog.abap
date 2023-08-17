*&---------------------------------------------------------------------*
*& Report ZR_CREATE_SUBSCRIPTIONS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zr_create_subscriptions.

DATA:
  lv_p_projects_id      TYPE string,
  lv_p_subscriptions_id TYPE string,
  ls_input              TYPE /goog/cl_pubsub_v1=>ty_038.

TRY.

* Open HTTP Connection
    DATA(lo_client) = NEW /goog/cl_pubsub_v1( iv_key_name = 'CLIENT_KEY' ).

* Populate relevant parameters
    " Derive project id from the client object
    lv_p_projects_id = lo_client->gv_project_id.
    " Name of the subscription to be created
    lv_p_subscriptions_id = 'SAMPLE_SUBSCRIPTION'.
    " LS_INPUT contains the attributes of the subscription.
    " Provide the topic to which the subscription will be attached
    ls_input-topic = 'projects/' && lo_client->gv_project_id && '/topics/SAMPLE_TOPIC'.

* Call API method
    CALL METHOD lo_client->create_subscriptions
      EXPORTING
        iv_p_projects_id      = lv_p_projects_id
        iv_p_subscriptions_id = lv_p_subscriptions_id
        is_input              = ls_input
      IMPORTING
*       ES_RAW                =
        es_output             = DATA(ls_output)
        ev_ret_code           = DATA(lv_ret_code)
        ev_err_text           = DATA(lv_err_text)
        es_err_resp           = DATA(ls_err_resp).

    IF /goog/cl_http_client=>is_success( lv_ret_code ).
      MESSAGE 'Subscription created successfully!' TYPE 'S'.
    ELSE.
      MESSAGE lv_err_text TYPE 'E'.
    ENDIF.

* Close HTTP Connection
    lo_client->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
