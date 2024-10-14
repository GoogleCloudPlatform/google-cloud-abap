**********************************************************************
*  Copyright 2023 Google LLC                                         *
*                                                                    *
*  Licensed under the Apache License, Version 2.0 (the "License");   *
*  you may not use this file except in compliance with the License.  *
*  You may obtain a copy of the License at                           *
*      https://www.apache.org/licenses/LICENSE-2.0                   *
*  Unless required by applicable law or agreed to in writing,        *
*  software distributed under the License is distributed on an       *
*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,      *
*  either express or implied.                                        *
*  See the License for the specific language governing permissions   *
*  and limitations under the License.                                *
**********************************************************************

REPORT zr_batch_translate_text_locati.

"Data Declarations
DATA:
  lv_p_projects_id  TYPE string,
  lv_p_locations_id TYPE string,
  lv_input_uri      TYPE string,
  lv_output_uri     TYPE string,
  lv_folder         TYPE string,
  ls_input          TYPE /goog/cl_translation_v3=>ty_004,
  ls_op_cnf         TYPE /goog/cl_translation_v3=>ty_043,
  ls_ip_cnf         TYPE /goog/cl_translation_v3=>ty_029,
  lt_ip_cnf         TYPE /goog/cl_translation_v3=>ty_t_029,
  lo_exception      TYPE REF TO /goog/cx_sdk.



TRY.
    "Open HTTP Connection
    "Pass the configured client key - TRANSLATE_DEMO is used an example, replace it with actual value.
    DATA(lo_translate) = NEW /goog/cl_translation_v3( iv_key_name = 'TRANSLATE_DEMO' ).

    "Populate the data that needs to be passed to the api
    "Derive project id
    lv_p_projects_id  = lo_translate->gv_project_id.

    "Pass location id, us-central1 is used as an example, replace it with actual value.
    lv_p_locations_id = 'us-central1'.

    "TODO: Provide storage bucket paths for uri values
    "For source provide the complete file path including storage bucket, for target provide the storage bucket name
    "lv_input_uri =
    "lv_output_uri =

    ls_input = VALUE #( input_configs =
                        VALUE #( ( gcs_source = VALUE #( input_uri = lv_input_uri )
                                   mime_type = 'text/plain' ) )
                         output_config =
                         VALUE #( gcs_destination =  VALUE #( output_uri_prefix = lv_output_uri )  )
                         source_language_code = `en`
                         target_language_codes =
                         VALUE #( ( `es` ) )
                         ).


    "Call the API
    CALL METHOD lo_translate->batch_translate_text_locati
      EXPORTING
        iv_p_projects_id  = lv_p_projects_id
        iv_p_locations_id = lv_p_locations_id
        is_input          = ls_input
      IMPORTING
        es_output         = DATA(ls_output)
        ev_ret_code       = DATA(lv_ret_code)
        ev_err_text       = DATA(lv_err_text)
        es_err_resp       = DATA(ls_err_resp).
    IF /goog/cl_http_client=>is_success( lv_ret_code ).
      "This returns a long running operation id for the job that is scheduled for glossary creation
      "You can use the LRO ID to poll and check the status of the operation
      WRITE: 'LRO ID:', ls_output-name.
    ELSE.
      MESSAGE lv_err_text TYPE 'E'.
    ENDIF.

    "Close HTTP Connection
    lo_translate->close( ).

  CATCH /goog/cx_sdk INTO lo_exception.
    lv_err_text = lo_exception->get_text( ).
ENDTRY.
