*&---------------------------------------------------------------------*
*& Report ZR_QS_PUBLISH_MESSAGES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zr_qs_publish_messages.

* Data Declaration
DATA: lv_p_topics_id   TYPE string,
      lv_p_projects_id TYPE string,
      lv_err_text      TYPE string,
      lv_ret_code      TYPE i,
      ls_input         TYPE /goog/cl_pubsub_v1=>ty_023,
      ls_output        TYPE /goog/cl_pubsub_v1=>ty_024,
      ls_message       TYPE /goog/cl_pubsub_v1=>ty_025,
      ls_err_resp      TYPE /goog/err_resp,
      lo_pubsub        TYPE REF TO /goog/cl_pubsub_v1,
      lo_exception     TYPE REF TO /goog/cx_sdk.

TRY.
* Instantiate the client stub
    CREATE OBJECT lo_pubsub
      EXPORTING
        iv_key_name = 'DEMO_PUBSUB'.

* Pass the relevant input parameters
    lv_p_topics_id = 'SAMPLE_TOPIC_01'.
    lv_p_projects_id = lo_pubsub->gv_project_id.
    ls_message-data = cl_http_utility=>encode_base64( 'Hello World!' ).
    APPEND ls_message TO ls_input-messages.

* Call the API
    CALL METHOD lo_pubsub->publish_topics
      EXPORTING
        iv_p_projects_id = lv_p_projects_id
        iv_p_topics_id   = lv_p_topics_id
        is_input         = ls_input
      IMPORTING
        es_output        = ls_output
        ev_ret_code      = lv_ret_code
        ev_err_text      = lv_err_text
        es_err_resp      = ls_err_resp.

* Handle the output
    IF lo_pubsub->is_success( lv_ret_code ) = abap_true.
      MESSAGE 'Message was published!' TYPE 'S'.
    ELSE.
      MESSAGE 'Message was not published!' TYPE 'E'.
    ENDIF.

* Close the HTTP Connection
    lo_pubsub->close( ).

  CATCH /goog/cx_sdk INTO lo_exception.
* Implement suitable error handling
ENDTRY.
