*&---------------------------------------------------------------------*
*& Report ZR_QS_PROCESS_DOCUMENTS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zr_qs_process_documents.

* data declarations
DATA:
  lv_p_projects_id   TYPE string,
  lv_p_locations_id  TYPE string,
  lv_p_processors_id TYPE string,
  ls_input           TYPE /goog/cl_documentai_v1=>ty_017.

TRY.

* open http connection
    DATA(lo_client) = NEW /goog/cl_documentai_v1( iv_key_name = 'DEMO_DOC_PROCESSING' ).

* populate relevant parameters
    lv_p_projects_id  = 'PROJECT_ID'.
    lv_p_locations_id = 'LOCATION_ID'.
    lv_p_processors_id = 'PROCESSOR_ID'.
    ls_input-input_documents-gcs_prefix-gcs_uri_prefix = 'SOURCE_BUCKET_URI'.
    ls_input-document_output_config-gcs_output_config-gcs_uri = 'TARGET_BUCKET_URI'.

* call api method
    CALL METHOD lo_client->batch_process_processors
      EXPORTING
        iv_p_projects_id   = lv_p_projects_id
        iv_p_locations_id  = lv_p_locations_id
        iv_p_processors_id = lv_p_processors_id
        is_input           = ls_input
      IMPORTING
        es_output          = DATA(ls_output)
        ev_ret_code        = DATA(lv_ret_code)
        ev_err_text        = DATA(lv_err_text)
        es_err_resp        = DATA(ls_err_resp).

    IF lo_client->is_success( lv_ret_code ).
      MESSAGE 'Success' TYPE 'S'.
    ELSE.
      MESSAGE lv_err_text TYPE 'E'.
    ENDIF.

* close http connection
    lo_client->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
