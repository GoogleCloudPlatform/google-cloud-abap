*&---------------------------------------------------------------------*
*& Report ZR_LIST_SUBSCRIPTIONS1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zr_list_subscriptions1.

DATA:
  lv_q_pagesize    TYPE string,
  " Token that can be used in the next call to API to get next page of subscriptions.
  lv_q_pagetoken   TYPE string,
  lv_p_projects_id TYPE string,
  lv_p_topics_id   TYPE string,
  lt_subscriptions TYPE /goog/cl_pubsub_v1=>ty_t_string.

TRY.

* Open HTTP Connection
    DATA(lo_client) = NEW /goog/cl_pubsub_v1( iv_key_name = 'CLIENT_KEY' ).

* Populate relevant parameters
    " Derive project id from the client object
    lv_p_projects_id = lo_client->gv_project_id.
    " Name of the Topic whose subscriptions will be listed.
    lv_p_topics_id   = 'SAMPLE_TOPIC'.
    " Max number of subscriptions to be retrieved in 1 API Call.
    lv_q_pagesize    = 50.


    WHILE sy-index = 1 OR lv_q_pagetoken IS NOT INITIAL.
* Call API method
      CALL METHOD lo_client->list_subscriptions1
        EXPORTING
          iv_q_pagesize    = lv_q_pagesize
          iv_q_pagetoken   = lv_q_pagetoken
          iv_p_projects_id = lv_p_projects_id
          iv_p_topics_id   = lv_p_topics_id
        IMPORTING
*         ES_RAW           =
          es_output        = DATA(ls_output)
          ev_ret_code      = DATA(lv_ret_code)
          ev_err_text      = DATA(lv_err_text)
          es_err_resp      = DATA(ls_err_resp).

      IF /goog/cl_http_client=>is_success( lv_ret_code ) = abap_true.
        LOOP AT ls_output-subscriptions ASSIGNING FIELD-SYMBOL(<ls_subscription>).
          APPEND <ls_subscription> TO lt_subscriptions.
        ENDLOOP.
      ENDIF.
      lv_q_pagetoken = ls_output-next_page_token.
    ENDWHILE.

    IF /goog/cl_http_client=>is_success( lv_ret_code ).
      MESSAGE 'Subscriptions attached to Topic were received!' TYPE 'S'.
    ELSE.
      MESSAGE lv_err_text TYPE 'E'.
    ENDIF.


* Close HTTP Connection
    lo_client->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
